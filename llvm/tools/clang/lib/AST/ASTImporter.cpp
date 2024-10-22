//===- ASTImporter.cpp - Importing ASTs from other Contexts ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the ASTImporter class which imports AST nodes from one
//  context into another context.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTImporter.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/ASTStructuralEquivalence.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclAccessPair.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclFriend.h"
#include "clang/AST/DeclGroup.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprObjC.h"
#include "clang/AST/ExternalASTSource.h"
#include "clang/AST/LambdaCapture.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/StmtObjC.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TemplateBase.h"
#include "clang/AST/TemplateName.h"
#include "clang/AST/Type.h"
#include "clang/AST/TypeLoc.h"
#include "clang/AST/TypeVisitor.h"
#include "clang/AST/UnresolvedSet.h"
#include "clang/Basic/ExceptionSpecificationType.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/Specifiers.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MemoryBuffer.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>

namespace clang {

  template <class T>
  SmallVector<Decl*, 2>
  getCanonicalForwardRedeclChain(Redeclarable<T>* D) {
    SmallVector<Decl*, 2> Redecls;
    for (auto *R : D->getFirstDecl()->redecls()) {
      if (R != D->getFirstDecl())
        Redecls.push_back(R);
    }
    Redecls.push_back(D->getFirstDecl());
    std::reverse(Redecls.begin(), Redecls.end());
    return Redecls;
  }

  SmallVector<Decl*, 2> getCanonicalForwardRedeclChain(Decl* D) {
    // Currently only FunctionDecl is supported
    auto FD = cast<FunctionDecl>(D);
    return getCanonicalForwardRedeclChain<FunctionDecl>(FD);
  }

  void updateFlags(const Decl *From, Decl *To) {
    // Check if some flags or attrs are new in 'From' and copy into 'To'.
    // FIXME: Other flags or attrs?
    if (From->isUsed(false) && !To->isUsed(false))
      To->setIsUsed();
  }

  class ASTNodeImporter : public TypeVisitor<ASTNodeImporter, QualType>,
                          public DeclVisitor<ASTNodeImporter, Decl *>,
                          public StmtVisitor<ASTNodeImporter, Stmt *> {
    ASTImporter &Importer;

    // Wrapper for an overload set.
    template <typename ToDeclT> struct CallOverloadedCreateFun {
      template <typename... Args>
      auto operator()(Args &&... args)
          -> decltype(ToDeclT::Create(std::forward<Args>(args)...)) {
        return ToDeclT::Create(std::forward<Args>(args)...);
      }
    };

    // Always use these functions to create a Decl during import. There are
    // certain tasks which must be done after the Decl was created, e.g. we
    // must immediately register that as an imported Decl.  The parameter `ToD`
    // will be set to the newly created Decl or if had been imported before
    // then to the already imported Decl.  Returns a bool value set to true if
    // the `FromD` had been imported before.
    template <typename ToDeclT, typename FromDeclT, typename... Args>
    LLVM_NODISCARD bool GetImportedOrCreateDecl(ToDeclT *&ToD, FromDeclT *FromD,
                                                Args &&... args) {
      // There may be several overloads of ToDeclT::Create. We must make sure
      // to call the one which would be chosen by the arguments, thus we use a
      // wrapper for the overload set.
      CallOverloadedCreateFun<ToDeclT> OC;
      return GetImportedOrCreateSpecialDecl(ToD, OC, FromD,
                                            std::forward<Args>(args)...);
    }
    // Use this overload if a special Type is needed to be created.  E.g if we
    // want to create a `TypeAliasDecl` and assign that to a `TypedefNameDecl`
    // then:
    // TypedefNameDecl *ToTypedef;
    // GetImportedOrCreateDecl<TypeAliasDecl>(ToTypedef, FromD, ...);
    template <typename NewDeclT, typename ToDeclT, typename FromDeclT,
              typename... Args>
    LLVM_NODISCARD bool GetImportedOrCreateDecl(ToDeclT *&ToD, FromDeclT *FromD,
                                                Args &&... args) {
      CallOverloadedCreateFun<NewDeclT> OC;
      return GetImportedOrCreateSpecialDecl(ToD, OC, FromD,
                                            std::forward<Args>(args)...);
    }
    // Use this version if a special create function must be
    // used, e.g. CXXRecordDecl::CreateLambda .
    template <typename ToDeclT, typename CreateFunT, typename FromDeclT,
              typename... Args>
    LLVM_NODISCARD bool
    GetImportedOrCreateSpecialDecl(ToDeclT *&ToD, CreateFunT CreateFun,
                                   FromDeclT *FromD, Args &&... args) {
      ToD = cast_or_null<ToDeclT>(Importer.GetAlreadyImportedOrNull(FromD));
      if (ToD)
        return true; // Already imported.
      ToD = CreateFun(std::forward<Args>(args)...);
      InitializeImportedDecl(FromD, ToD);
      return false; // A new Decl is created.
    }

    void InitializeImportedDecl(Decl *FromD, Decl *ToD) {
      Importer.MapImported(FromD, ToD);
      ToD->IdentifierNamespace = FromD->IdentifierNamespace;
      if (FromD->hasAttrs())
        for (const Attr *FromAttr : FromD->getAttrs())
          ToD->addAttr(Importer.Import(FromAttr));
      if (FromD->isUsed())
        ToD->setIsUsed();
      if (FromD->isImplicit())
        ToD->setImplicit();
    }

  public:
    explicit ASTNodeImporter(ASTImporter &Importer) : Importer(Importer) {}

    using TypeVisitor<ASTNodeImporter, QualType>::Visit;
    using DeclVisitor<ASTNodeImporter, Decl *>::Visit;
    using StmtVisitor<ASTNodeImporter, Stmt *>::Visit;

    // Importing types
    QualType VisitType(const Type *T);
    QualType VisitAtomicType(const AtomicType *T);
    QualType VisitBuiltinType(const BuiltinType *T);
    QualType VisitDecayedType(const DecayedType *T);
    QualType VisitComplexType(const ComplexType *T);
    QualType VisitPointerType(const PointerType *T);
    QualType VisitBlockPointerType(const BlockPointerType *T);
    QualType VisitLValueReferenceType(const LValueReferenceType *T);
    QualType VisitRValueReferenceType(const RValueReferenceType *T);
    QualType VisitMemberPointerType(const MemberPointerType *T);
    QualType VisitConstantArrayType(const ConstantArrayType *T);
    QualType VisitIncompleteArrayType(const IncompleteArrayType *T);
    QualType VisitVariableArrayType(const VariableArrayType *T);
    QualType VisitDependentSizedArrayType(const DependentSizedArrayType *T);
    // FIXME: DependentSizedExtVectorType
    QualType VisitVectorType(const VectorType *T);
    QualType VisitExtVectorType(const ExtVectorType *T);
    QualType VisitFunctionNoProtoType(const FunctionNoProtoType *T);
    QualType VisitFunctionProtoType(const FunctionProtoType *T);
    QualType VisitUnresolvedUsingType(const UnresolvedUsingType *T);
    QualType VisitParenType(const ParenType *T);
    QualType VisitTypedefType(const TypedefType *T);
    QualType VisitTypeOfExprType(const TypeOfExprType *T);
    // FIXME: DependentTypeOfExprType
    QualType VisitTypeOfType(const TypeOfType *T);
    QualType VisitDecltypeType(const DecltypeType *T);
    QualType VisitUnaryTransformType(const UnaryTransformType *T);
    QualType VisitAutoType(const AutoType *T);
    QualType VisitInjectedClassNameType(const InjectedClassNameType *T);
    // FIXME: DependentDecltypeType
    QualType VisitRecordType(const RecordType *T);
    QualType VisitEnumType(const EnumType *T);
    QualType VisitAttributedType(const AttributedType *T);
    QualType VisitTemplateTypeParmType(const TemplateTypeParmType *T);
    QualType VisitSubstTemplateTypeParmType(const SubstTemplateTypeParmType *T);
    QualType VisitTemplateSpecializationType(const TemplateSpecializationType *T);
    QualType VisitElaboratedType(const ElaboratedType *T);
    QualType VisitDependentNameType(const DependentNameType *T);
    QualType VisitPackExpansionType(const PackExpansionType *T);
    QualType VisitDependentTemplateSpecializationType(
        const DependentTemplateSpecializationType *T);
    QualType VisitObjCInterfaceType(const ObjCInterfaceType *T);
    QualType VisitObjCObjectType(const ObjCObjectType *T);
    QualType VisitObjCObjectPointerType(const ObjCObjectPointerType *T);

    // Importing declarations
    bool ImportDeclParts(NamedDecl *D, DeclContext *&DC,
                         DeclContext *&LexicalDC, DeclarationName &Name,
                         NamedDecl *&ToD, SourceLocation &Loc);
    void ImportDefinitionIfNeeded(Decl *FromD, Decl *ToD = nullptr);
    void ImportDeclarationNameLoc(const DeclarationNameInfo &From,
                                  DeclarationNameInfo& To);
    void ImportDeclContext(DeclContext *FromDC, bool ForceImport = false);
    void ImportImplicitMethods(const CXXRecordDecl *From, CXXRecordDecl *To);

    bool ImportCastPath(CastExpr *E, CXXCastPath &Path);

    using Designator = DesignatedInitExpr::Designator;

    Designator ImportDesignator(const Designator &D);

    Optional<LambdaCapture> ImportLambdaCapture(const LambdaCapture &From);

    /// What we should import from the definition.
    enum ImportDefinitionKind {
      /// Import the default subset of the definition, which might be
      /// nothing (if minimal import is set) or might be everything (if minimal
      /// import is not set).
      IDK_Default,

      /// Import everything.
      IDK_Everything,

      /// Import only the bare bones needed to establish a valid
      /// DeclContext.
      IDK_Basic
    };

    bool shouldForceImportDeclContext(ImportDefinitionKind IDK) {
      return IDK == IDK_Everything ||
             (IDK == IDK_Default && !Importer.isMinimalImport());
    }

    bool ImportDefinition(RecordDecl *From, RecordDecl *To,
                          ImportDefinitionKind Kind = IDK_Default);
    bool ImportDefinition(VarDecl *From, VarDecl *To,
                          ImportDefinitionKind Kind = IDK_Default);
    bool ImportDefinition(EnumDecl *From, EnumDecl *To,
                          ImportDefinitionKind Kind = IDK_Default);
    bool ImportDefinition(ObjCInterfaceDecl *From, ObjCInterfaceDecl *To,
                          ImportDefinitionKind Kind = IDK_Default);
    bool ImportDefinition(ObjCProtocolDecl *From, ObjCProtocolDecl *To,
                          ImportDefinitionKind Kind = IDK_Default);
    TemplateParameterList *ImportTemplateParameterList(
        TemplateParameterList *Params);
    TemplateArgument ImportTemplateArgument(const TemplateArgument &From);
    Optional<TemplateArgumentLoc> ImportTemplateArgumentLoc(
        const TemplateArgumentLoc &TALoc);
    bool ImportTemplateArguments(const TemplateArgument *FromArgs,
                                 unsigned NumFromArgs,
                                 SmallVectorImpl<TemplateArgument> &ToArgs);

    template <typename InContainerTy>
    bool ImportTemplateArgumentListInfo(const InContainerTy &Container,
                                        TemplateArgumentListInfo &ToTAInfo);

    template<typename InContainerTy>
    bool ImportTemplateArgumentListInfo(SourceLocation FromLAngleLoc,
                                        SourceLocation FromRAngleLoc,
                                        const InContainerTy &Container,
                                        TemplateArgumentListInfo &Result);

    using TemplateArgsTy = SmallVector<TemplateArgument, 8>;
    using OptionalTemplateArgsTy = Optional<TemplateArgsTy>;
    std::tuple<FunctionTemplateDecl *, OptionalTemplateArgsTy>
    ImportFunctionTemplateWithTemplateArgsFromSpecialization(
        FunctionDecl *FromFD);

    bool ImportTemplateInformation(FunctionDecl *FromFD, FunctionDecl *ToFD);

    bool IsStructuralMatch(Decl *From, Decl *To, bool Complain);
    bool IsStructuralMatch(RecordDecl *FromRecord, RecordDecl *ToRecord,
                           bool Complain = true);
    bool IsStructuralMatch(VarDecl *FromVar, VarDecl *ToVar,
                           bool Complain = true);
    bool IsStructuralMatch(EnumDecl *FromEnum, EnumDecl *ToRecord);
    bool IsStructuralMatch(EnumConstantDecl *FromEC, EnumConstantDecl *ToEC);
    bool IsStructuralMatch(FunctionTemplateDecl *From,
                           FunctionTemplateDecl *To);
    bool IsStructuralMatch(FunctionDecl *From, FunctionDecl *To);
    bool IsStructuralMatch(ClassTemplateDecl *From, ClassTemplateDecl *To);
    bool IsStructuralMatch(VarTemplateDecl *From, VarTemplateDecl *To);
    Decl *VisitDecl(Decl *D);
    Decl *VisitEmptyDecl(EmptyDecl *D);
    Decl *VisitAccessSpecDecl(AccessSpecDecl *D);
    Decl *VisitStaticAssertDecl(StaticAssertDecl *D);
    Decl *VisitTranslationUnitDecl(TranslationUnitDecl *D);
    Decl *VisitNamespaceDecl(NamespaceDecl *D);
    Decl *VisitNamespaceAliasDecl(NamespaceAliasDecl *D);
    Decl *VisitTypedefNameDecl(TypedefNameDecl *D, bool IsAlias);
    Decl *VisitTypedefDecl(TypedefDecl *D);
    Decl *VisitTypeAliasDecl(TypeAliasDecl *D);
    Decl *VisitTypeAliasTemplateDecl(TypeAliasTemplateDecl *D);
    Decl *VisitLabelDecl(LabelDecl *D);
    Decl *VisitEnumDecl(EnumDecl *D);
    Decl *VisitRecordDecl(RecordDecl *D);
    Decl *VisitEnumConstantDecl(EnumConstantDecl *D);
    Decl *VisitFunctionDecl(FunctionDecl *D);
    Decl *VisitCXXMethodDecl(CXXMethodDecl *D);
    Decl *VisitCXXConstructorDecl(CXXConstructorDecl *D);
    Decl *VisitCXXDestructorDecl(CXXDestructorDecl *D);
    Decl *VisitCXXConversionDecl(CXXConversionDecl *D);
    Decl *VisitFieldDecl(FieldDecl *D);
    Decl *VisitIndirectFieldDecl(IndirectFieldDecl *D);
    Decl *VisitFriendDecl(FriendDecl *D);
    Decl *VisitObjCIvarDecl(ObjCIvarDecl *D);
    Decl *VisitVarDecl(VarDecl *D);
    Decl *VisitImplicitParamDecl(ImplicitParamDecl *D);
    Decl *VisitParmVarDecl(ParmVarDecl *D);
    Decl *VisitObjCMethodDecl(ObjCMethodDecl *D);
    Decl *VisitObjCTypeParamDecl(ObjCTypeParamDecl *D);
    Decl *VisitObjCCategoryDecl(ObjCCategoryDecl *D);
    Decl *VisitObjCProtocolDecl(ObjCProtocolDecl *D);
    Decl *VisitLinkageSpecDecl(LinkageSpecDecl *D);
    Decl *VisitUsingDecl(UsingDecl *D);
    Decl *VisitUsingShadowDecl(UsingShadowDecl *D);
    Decl *VisitUsingDirectiveDecl(UsingDirectiveDecl *D);
    Decl *VisitUnresolvedUsingValueDecl(UnresolvedUsingValueDecl *D);
    Decl *VisitUnresolvedUsingTypenameDecl(UnresolvedUsingTypenameDecl *D);
//ASUTTON ADDN
    Decl *VisitConstexprDecl(ConstexprDecl *D);
//END

    ObjCTypeParamList *ImportObjCTypeParamList(ObjCTypeParamList *list);
    Decl *VisitObjCInterfaceDecl(ObjCInterfaceDecl *D);
    Decl *VisitObjCCategoryImplDecl(ObjCCategoryImplDecl *D);
    Decl *VisitObjCImplementationDecl(ObjCImplementationDecl *D);
    Decl *VisitObjCPropertyDecl(ObjCPropertyDecl *D);
    Decl *VisitObjCPropertyImplDecl(ObjCPropertyImplDecl *D);
    Decl *VisitTemplateTypeParmDecl(TemplateTypeParmDecl *D);
    Decl *VisitNonTypeTemplateParmDecl(NonTypeTemplateParmDecl *D);
    Decl *VisitTemplateTemplateParmDecl(TemplateTemplateParmDecl *D);
    Decl *VisitClassTemplateDecl(ClassTemplateDecl *D);
    Decl *VisitClassTemplateSpecializationDecl(
                                            ClassTemplateSpecializationDecl *D);
    Decl *VisitVarTemplateDecl(VarTemplateDecl *D);
    Decl *VisitVarTemplateSpecializationDecl(VarTemplateSpecializationDecl *D);
    Decl *VisitFunctionTemplateDecl(FunctionTemplateDecl *D);

    // Importing statements
    DeclGroupRef ImportDeclGroup(DeclGroupRef DG);

    Stmt *VisitStmt(Stmt *S);
    Stmt *VisitGCCAsmStmt(GCCAsmStmt *S);
    Stmt *VisitDeclStmt(DeclStmt *S);
    Stmt *VisitNullStmt(NullStmt *S);
    Stmt *VisitCompoundStmt(CompoundStmt *S);
    Stmt *VisitCaseStmt(CaseStmt *S);
    Stmt *VisitDefaultStmt(DefaultStmt *S);
    Stmt *VisitLabelStmt(LabelStmt *S);
    Stmt *VisitAttributedStmt(AttributedStmt *S);
    Stmt *VisitIfStmt(IfStmt *S);
    Stmt *VisitSwitchStmt(SwitchStmt *S);
    Stmt *VisitWhileStmt(WhileStmt *S);
    Stmt *VisitDoStmt(DoStmt *S);
    Stmt *VisitForStmt(ForStmt *S);
    Stmt *VisitGotoStmt(GotoStmt *S);
    Stmt *VisitIndirectGotoStmt(IndirectGotoStmt *S);
    Stmt *VisitContinueStmt(ContinueStmt *S);
    Stmt *VisitBreakStmt(BreakStmt *S);
    Stmt *VisitReturnStmt(ReturnStmt *S);
    // FIXME: MSAsmStmt
    // FIXME: SEHExceptStmt
    // FIXME: SEHFinallyStmt
    // FIXME: SEHTryStmt
    // FIXME: SEHLeaveStmt
    // FIXME: CapturedStmt
    Stmt *VisitCXXCatchStmt(CXXCatchStmt *S);
    Stmt *VisitCXXTryStmt(CXXTryStmt *S);
    Stmt *VisitCXXForRangeStmt(CXXForRangeStmt *S);
    // FIXME: MSDependentExistsStmt
    Stmt *VisitObjCForCollectionStmt(ObjCForCollectionStmt *S);
    Stmt *VisitObjCAtCatchStmt(ObjCAtCatchStmt *S);
    Stmt *VisitObjCAtFinallyStmt(ObjCAtFinallyStmt *S);
    Stmt *VisitObjCAtTryStmt(ObjCAtTryStmt *S);
    Stmt *VisitObjCAtSynchronizedStmt(ObjCAtSynchronizedStmt *S);
    Stmt *VisitObjCAtThrowStmt(ObjCAtThrowStmt *S);
    Stmt *VisitObjCAutoreleasePoolStmt(ObjCAutoreleasePoolStmt *S);

    // Importing expressions
    Expr *VisitExpr(Expr *E);
    Expr *VisitVAArgExpr(VAArgExpr *E);
    Expr *VisitGNUNullExpr(GNUNullExpr *E);
    Expr *VisitPredefinedExpr(PredefinedExpr *E);
    Expr *VisitDeclRefExpr(DeclRefExpr *E);
    Expr *VisitImplicitValueInitExpr(ImplicitValueInitExpr *ILE);
    Expr *VisitDesignatedInitExpr(DesignatedInitExpr *E);
    Expr *VisitCXXNullPtrLiteralExpr(CXXNullPtrLiteralExpr *E);
    Expr *VisitIntegerLiteral(IntegerLiteral *E);
    Expr *VisitFloatingLiteral(FloatingLiteral *E);
    Expr *VisitCharacterLiteral(CharacterLiteral *E);
    Expr *VisitStringLiteral(StringLiteral *E);
    Expr *VisitCompoundLiteralExpr(CompoundLiteralExpr *E);
    Expr *VisitAtomicExpr(AtomicExpr *E);
    Expr *VisitAddrLabelExpr(AddrLabelExpr *E);
    Expr *VisitParenExpr(ParenExpr *E);
    Expr *VisitParenListExpr(ParenListExpr *E);
    Expr *VisitStmtExpr(StmtExpr *E);
    Expr *VisitUnaryOperator(UnaryOperator *E);
    Expr *VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *E);
    Expr *VisitBinaryOperator(BinaryOperator *E);
    Expr *VisitConditionalOperator(ConditionalOperator *E);
    Expr *VisitBinaryConditionalOperator(BinaryConditionalOperator *E);
    Expr *VisitOpaqueValueExpr(OpaqueValueExpr *E);
    Expr *VisitArrayTypeTraitExpr(ArrayTypeTraitExpr *E);
    Expr *VisitExpressionTraitExpr(ExpressionTraitExpr *E);
    Expr *VisitArraySubscriptExpr(ArraySubscriptExpr *E);
    Expr *VisitCompoundAssignOperator(CompoundAssignOperator *E);
    Expr *VisitImplicitCastExpr(ImplicitCastExpr *E);
    Expr *VisitExplicitCastExpr(ExplicitCastExpr *E);
    Expr *VisitOffsetOfExpr(OffsetOfExpr *OE);
    Expr *VisitCXXThrowExpr(CXXThrowExpr *E);
    Expr *VisitCXXNoexceptExpr(CXXNoexceptExpr *E);
    Expr *VisitCXXDefaultArgExpr(CXXDefaultArgExpr *E);
    Expr *VisitCXXScalarValueInitExpr(CXXScalarValueInitExpr *E);
    Expr *VisitCXXBindTemporaryExpr(CXXBindTemporaryExpr *E);
    Expr *VisitCXXTemporaryObjectExpr(CXXTemporaryObjectExpr *CE);
    Expr *VisitMaterializeTemporaryExpr(MaterializeTemporaryExpr *E);
    Expr *VisitPackExpansionExpr(PackExpansionExpr *E);
    Expr *VisitSizeOfPackExpr(SizeOfPackExpr *E);
    Expr *VisitCXXNewExpr(CXXNewExpr *CE);
    Expr *VisitCXXDeleteExpr(CXXDeleteExpr *E);
    Expr *VisitCXXConstructExpr(CXXConstructExpr *E);
    Expr *VisitCXXMemberCallExpr(CXXMemberCallExpr *E);
    Expr *VisitCXXDependentScopeMemberExpr(CXXDependentScopeMemberExpr *E);
    Expr *VisitDependentScopeDeclRefExpr(DependentScopeDeclRefExpr *E);
    Expr *VisitCXXUnresolvedConstructExpr(CXXUnresolvedConstructExpr *CE);
    Expr *VisitUnresolvedLookupExpr(UnresolvedLookupExpr *E);
    Expr *VisitUnresolvedMemberExpr(UnresolvedMemberExpr *E);
    Expr *VisitExprWithCleanups(ExprWithCleanups *EWC);
    Expr *VisitCXXThisExpr(CXXThisExpr *E);
    Expr *VisitCXXBoolLiteralExpr(CXXBoolLiteralExpr *E);
    Expr *VisitCXXPseudoDestructorExpr(CXXPseudoDestructorExpr *E);
    Expr *VisitMemberExpr(MemberExpr *E);
    Expr *VisitCallExpr(CallExpr *E);
    Expr *VisitLambdaExpr(LambdaExpr *LE);
    Expr *VisitInitListExpr(InitListExpr *E);
    Expr *VisitCXXStdInitializerListExpr(CXXStdInitializerListExpr *E);
    Expr *VisitCXXInheritedCtorInitExpr(CXXInheritedCtorInitExpr *E);
    Expr *VisitArrayInitLoopExpr(ArrayInitLoopExpr *E);
    Expr *VisitArrayInitIndexExpr(ArrayInitIndexExpr *E);
    Expr *VisitCXXDefaultInitExpr(CXXDefaultInitExpr *E);
    Expr *VisitCXXNamedCastExpr(CXXNamedCastExpr *E);
    Expr *VisitSubstNonTypeTemplateParmExpr(SubstNonTypeTemplateParmExpr *E);
    Expr *VisitTypeTraitExpr(TypeTraitExpr *E);
    Expr *VisitCXXTypeidExpr(CXXTypeidExpr *E);
    Expr *VisitCompilerMessageExpr(CompilerMessageExpr *E); //ASUTTON ADDN
    Expr *VisitCompilerDiagnosticExpr(CompilerDiagnosticExpr *E); //DWR ADDN
    // DWR TODO VisitReflectionTraitExpr etc.?  Not sure why ASUTTON had
    // the CompilerError stuff but not reflection stuff in here, don't know
    // what this does...

    template<typename IIter, typename OIter>
    void ImportArray(IIter Ibegin, IIter Iend, OIter Obegin) {
      using ItemT = typename std::remove_reference<decltype(*Obegin)>::type;

      ASTImporter &ImporterRef = Importer;
      std::transform(Ibegin, Iend, Obegin,
                     [&ImporterRef](ItemT From) -> ItemT {
                       return ImporterRef.Import(From);
                     });
    }

    template<typename IIter, typename OIter>
    bool ImportArrayChecked(IIter Ibegin, IIter Iend, OIter Obegin) {
      using ItemT = typename std::remove_reference<decltype(**Obegin)>::type;

      ASTImporter &ImporterRef = Importer;
      bool Failed = false;
      std::transform(Ibegin, Iend, Obegin,
                     [&ImporterRef, &Failed](ItemT *From) -> ItemT * {
                       auto *To = cast_or_null<ItemT>(ImporterRef.Import(From));
                       if (!To && From)
                         Failed = true;
                       return To;
                     });
      return Failed;
    }

    template<typename InContainerTy, typename OutContainerTy>
    bool ImportContainerChecked(const InContainerTy &InContainer,
                                OutContainerTy &OutContainer) {
      return ImportArrayChecked(InContainer.begin(), InContainer.end(),
                                OutContainer.begin());
    }

    template<typename InContainerTy, typename OIter>
    bool ImportArrayChecked(const InContainerTy &InContainer, OIter Obegin) {
      return ImportArrayChecked(InContainer.begin(), InContainer.end(), Obegin);
    }

    // Importing overrides.
    void ImportOverrides(CXXMethodDecl *ToMethod, CXXMethodDecl *FromMethod);

    FunctionDecl *FindFunctionTemplateSpecialization(FunctionDecl *FromFD);
  };

template <typename InContainerTy>
bool ASTNodeImporter::ImportTemplateArgumentListInfo(
    SourceLocation FromLAngleLoc, SourceLocation FromRAngleLoc,
    const InContainerTy &Container, TemplateArgumentListInfo &Result) {
  TemplateArgumentListInfo ToTAInfo(Importer.Import(FromLAngleLoc),
                                    Importer.Import(FromRAngleLoc));
  if (ImportTemplateArgumentListInfo(Container, ToTAInfo))
    return true;
  Result = ToTAInfo;
  return false;
}

template <>
bool ASTNodeImporter::ImportTemplateArgumentListInfo<TemplateArgumentListInfo>(
    const TemplateArgumentListInfo &From, TemplateArgumentListInfo &Result) {
  return ImportTemplateArgumentListInfo(
      From.getLAngleLoc(), From.getRAngleLoc(), From.arguments(), Result);
}

template <>
bool ASTNodeImporter::ImportTemplateArgumentListInfo<
    ASTTemplateArgumentListInfo>(const ASTTemplateArgumentListInfo &From,
                                 TemplateArgumentListInfo &Result) {
  return ImportTemplateArgumentListInfo(From.LAngleLoc, From.RAngleLoc,
                                        From.arguments(), Result);
}

std::tuple<FunctionTemplateDecl *, ASTNodeImporter::OptionalTemplateArgsTy>
ASTNodeImporter::ImportFunctionTemplateWithTemplateArgsFromSpecialization(
    FunctionDecl *FromFD) {
  assert(FromFD->getTemplatedKind() ==
         FunctionDecl::TK_FunctionTemplateSpecialization);
  auto *FTSInfo = FromFD->getTemplateSpecializationInfo();
  auto *Template = cast_or_null<FunctionTemplateDecl>(
      Importer.Import(FTSInfo->getTemplate()));

  // Import template arguments.
  auto TemplArgs = FTSInfo->TemplateArguments->asArray();
  TemplateArgsTy ToTemplArgs;
  if (ImportTemplateArguments(TemplArgs.data(), TemplArgs.size(),
                              ToTemplArgs)) // Error during import.
    return std::make_tuple(Template, OptionalTemplateArgsTy());

  return std::make_tuple(Template, ToTemplArgs);
}

} // namespace clang

//----------------------------------------------------------------------------
// Import Types
//----------------------------------------------------------------------------

using namespace clang;

QualType ASTNodeImporter::VisitType(const Type *T) {
  Importer.FromDiag(SourceLocation(), diag::err_unsupported_ast_node)
    << T->getTypeClassName();
  return {};
}

QualType ASTNodeImporter::VisitAtomicType(const AtomicType *T){
  QualType UnderlyingType = Importer.Import(T->getValueType());
  if(UnderlyingType.isNull())
    return {};

  return Importer.getToContext().getAtomicType(UnderlyingType);
}

QualType ASTNodeImporter::VisitBuiltinType(const BuiltinType *T) {
  switch (T->getKind()) {
#define IMAGE_TYPE(ImgType, Id, SingletonId, Access, Suffix) \
  case BuiltinType::Id: \
    return Importer.getToContext().SingletonId;
#include "clang/Basic/OpenCLImageTypes.def"
#define SHARED_SINGLETON_TYPE(Expansion)
#define BUILTIN_TYPE(Id, SingletonId) \
  case BuiltinType::Id: return Importer.getToContext().SingletonId;
#include "clang/AST/BuiltinTypes.def"

  // FIXME: for Char16, Char32, and NullPtr, make sure that the "to"
  // context supports C++.

  // FIXME: for ObjCId, ObjCClass, and ObjCSel, make sure that the "to"
  // context supports ObjC.

  case BuiltinType::Char_U:
    // The context we're importing from has an unsigned 'char'. If we're
    // importing into a context with a signed 'char', translate to
    // 'unsigned char' instead.
    if (Importer.getToContext().getLangOpts().CharIsSigned)
      return Importer.getToContext().UnsignedCharTy;

    return Importer.getToContext().CharTy;

  case BuiltinType::Char_S:
    // The context we're importing from has an unsigned 'char'. If we're
    // importing into a context with a signed 'char', translate to
    // 'unsigned char' instead.
    if (!Importer.getToContext().getLangOpts().CharIsSigned)
      return Importer.getToContext().SignedCharTy;

    return Importer.getToContext().CharTy;

  case BuiltinType::WChar_S:
  case BuiltinType::WChar_U:
    // FIXME: If not in C++, shall we translate to the C equivalent of
    // wchar_t?
    return Importer.getToContext().WCharTy;
  }

  llvm_unreachable("Invalid BuiltinType Kind!");
}

QualType ASTNodeImporter::VisitDecayedType(const DecayedType *T) {
  QualType OrigT = Importer.Import(T->getOriginalType());
  if (OrigT.isNull())
    return {};

  return Importer.getToContext().getDecayedType(OrigT);
}

QualType ASTNodeImporter::VisitComplexType(const ComplexType *T) {
  QualType ToElementType = Importer.Import(T->getElementType());
  if (ToElementType.isNull())
    return {};

  return Importer.getToContext().getComplexType(ToElementType);
}

QualType ASTNodeImporter::VisitPointerType(const PointerType *T) {
  QualType ToPointeeType = Importer.Import(T->getPointeeType());
  if (ToPointeeType.isNull())
    return {};

  return Importer.getToContext().getPointerType(ToPointeeType);
}

QualType ASTNodeImporter::VisitBlockPointerType(const BlockPointerType *T) {
  // FIXME: Check for blocks support in "to" context.
  QualType ToPointeeType = Importer.Import(T->getPointeeType());
  if (ToPointeeType.isNull())
    return {};

  return Importer.getToContext().getBlockPointerType(ToPointeeType);
}

QualType
ASTNodeImporter::VisitLValueReferenceType(const LValueReferenceType *T) {
  // FIXME: Check for C++ support in "to" context.
  QualType ToPointeeType = Importer.Import(T->getPointeeTypeAsWritten());
  if (ToPointeeType.isNull())
    return {};

  return Importer.getToContext().getLValueReferenceType(ToPointeeType);
}

QualType
ASTNodeImporter::VisitRValueReferenceType(const RValueReferenceType *T) {
  // FIXME: Check for C++0x support in "to" context.
  QualType ToPointeeType = Importer.Import(T->getPointeeTypeAsWritten());
  if (ToPointeeType.isNull())
    return {};

  return Importer.getToContext().getRValueReferenceType(ToPointeeType);
}

QualType ASTNodeImporter::VisitMemberPointerType(const MemberPointerType *T) {
  // FIXME: Check for C++ support in "to" context.
  QualType ToPointeeType = Importer.Import(T->getPointeeType());
  if (ToPointeeType.isNull())
    return {};

  QualType ClassType = Importer.Import(QualType(T->getClass(), 0));
  return Importer.getToContext().getMemberPointerType(ToPointeeType,
                                                      ClassType.getTypePtr());
}

QualType ASTNodeImporter::VisitConstantArrayType(const ConstantArrayType *T) {
  QualType ToElementType = Importer.Import(T->getElementType());
  if (ToElementType.isNull())
    return {};

  return Importer.getToContext().getConstantArrayType(ToElementType,
                                                      T->getSize(),
                                                      T->getSizeModifier(),
                                               T->getIndexTypeCVRQualifiers());
}

QualType
ASTNodeImporter::VisitIncompleteArrayType(const IncompleteArrayType *T) {
  QualType ToElementType = Importer.Import(T->getElementType());
  if (ToElementType.isNull())
    return {};

  return Importer.getToContext().getIncompleteArrayType(ToElementType,
                                                        T->getSizeModifier(),
                                                T->getIndexTypeCVRQualifiers());
}

QualType ASTNodeImporter::VisitVariableArrayType(const VariableArrayType *T) {
  QualType ToElementType = Importer.Import(T->getElementType());
  if (ToElementType.isNull())
    return {};

  Expr *Size = Importer.Import(T->getSizeExpr());
  if (!Size)
    return {};

  SourceRange Brackets = Importer.Import(T->getBracketsRange());
  return Importer.getToContext().getVariableArrayType(ToElementType, Size,
                                                      T->getSizeModifier(),
                                                T->getIndexTypeCVRQualifiers(),
                                                      Brackets);
}

QualType ASTNodeImporter::VisitDependentSizedArrayType(
    const DependentSizedArrayType *T) {
  QualType ToElementType = Importer.Import(T->getElementType());
  if (ToElementType.isNull())
    return {};

  // SizeExpr may be null if size is not specified directly.
  // For example, 'int a[]'.
  Expr *Size = Importer.Import(T->getSizeExpr());
  if (!Size && T->getSizeExpr())
    return {};

  SourceRange Brackets = Importer.Import(T->getBracketsRange());
  return Importer.getToContext().getDependentSizedArrayType(
      ToElementType, Size, T->getSizeModifier(), T->getIndexTypeCVRQualifiers(),
      Brackets);
}

QualType ASTNodeImporter::VisitVectorType(const VectorType *T) {
  QualType ToElementType = Importer.Import(T->getElementType());
  if (ToElementType.isNull())
    return {};

  return Importer.getToContext().getVectorType(ToElementType,
                                               T->getNumElements(),
                                               T->getVectorKind());
}

QualType ASTNodeImporter::VisitExtVectorType(const ExtVectorType *T) {
  QualType ToElementType = Importer.Import(T->getElementType());
  if (ToElementType.isNull())
    return {};

  return Importer.getToContext().getExtVectorType(ToElementType,
                                                  T->getNumElements());
}

QualType
ASTNodeImporter::VisitFunctionNoProtoType(const FunctionNoProtoType *T) {
  // FIXME: What happens if we're importing a function without a prototype
  // into C++? Should we make it variadic?
  QualType ToResultType = Importer.Import(T->getReturnType());
  if (ToResultType.isNull())
    return {};

  return Importer.getToContext().getFunctionNoProtoType(ToResultType,
                                                        T->getExtInfo());
}

QualType ASTNodeImporter::VisitFunctionProtoType(const FunctionProtoType *T) {
  QualType ToResultType = Importer.Import(T->getReturnType());
  if (ToResultType.isNull())
    return {};

  // Import argument types
  SmallVector<QualType, 4> ArgTypes;
  for (const auto &A : T->param_types()) {
    QualType ArgType = Importer.Import(A);
    if (ArgType.isNull())
      return {};
    ArgTypes.push_back(ArgType);
  }

  // Import exception types
  SmallVector<QualType, 4> ExceptionTypes;
  for (const auto &E : T->exceptions()) {
    QualType ExceptionType = Importer.Import(E);
    if (ExceptionType.isNull())
      return {};
    ExceptionTypes.push_back(ExceptionType);
  }

  FunctionProtoType::ExtProtoInfo FromEPI = T->getExtProtoInfo();
  FunctionProtoType::ExtProtoInfo ToEPI;

  ToEPI.ExtInfo = FromEPI.ExtInfo;
  ToEPI.Variadic = FromEPI.Variadic;
  ToEPI.HasTrailingReturn = FromEPI.HasTrailingReturn;
  ToEPI.TypeQuals = FromEPI.TypeQuals;
  ToEPI.RefQualifier = FromEPI.RefQualifier;
  ToEPI.ExceptionSpec.Type = FromEPI.ExceptionSpec.Type;
  ToEPI.ExceptionSpec.Exceptions = ExceptionTypes;
  ToEPI.ExceptionSpec.NoexceptExpr =
      Importer.Import(FromEPI.ExceptionSpec.NoexceptExpr);
  ToEPI.ExceptionSpec.SourceDecl = cast_or_null<FunctionDecl>(
      Importer.Import(FromEPI.ExceptionSpec.SourceDecl));
  ToEPI.ExceptionSpec.SourceTemplate = cast_or_null<FunctionDecl>(
      Importer.Import(FromEPI.ExceptionSpec.SourceTemplate));

  return Importer.getToContext().getFunctionType(ToResultType, ArgTypes, ToEPI);
}

QualType ASTNodeImporter::VisitUnresolvedUsingType(
    const UnresolvedUsingType *T) {
  const auto *ToD =
      cast_or_null<UnresolvedUsingTypenameDecl>(Importer.Import(T->getDecl()));
  if (!ToD)
    return {};

  auto *ToPrevD =
      cast_or_null<UnresolvedUsingTypenameDecl>(
        Importer.Import(T->getDecl()->getPreviousDecl()));
  if (!ToPrevD && T->getDecl()->getPreviousDecl())
    return {};

  return Importer.getToContext().getTypeDeclType(ToD, ToPrevD);
}

QualType ASTNodeImporter::VisitParenType(const ParenType *T) {
  QualType ToInnerType = Importer.Import(T->getInnerType());
  if (ToInnerType.isNull())
    return {};

  return Importer.getToContext().getParenType(ToInnerType);
}

QualType ASTNodeImporter::VisitTypedefType(const TypedefType *T) {
  auto *ToDecl =
      dyn_cast_or_null<TypedefNameDecl>(Importer.Import(T->getDecl()));
  if (!ToDecl)
    return {};

  return Importer.getToContext().getTypeDeclType(ToDecl);
}

QualType ASTNodeImporter::VisitTypeOfExprType(const TypeOfExprType *T) {
  Expr *ToExpr = Importer.Import(T->getUnderlyingExpr());
  if (!ToExpr)
    return {};

  return Importer.getToContext().getTypeOfExprType(ToExpr);
}

QualType ASTNodeImporter::VisitTypeOfType(const TypeOfType *T) {
  QualType ToUnderlyingType = Importer.Import(T->getUnderlyingType());
  if (ToUnderlyingType.isNull())
    return {};

  return Importer.getToContext().getTypeOfType(ToUnderlyingType);
}

QualType ASTNodeImporter::VisitDecltypeType(const DecltypeType *T) {
  // FIXME: Make sure that the "to" context supports C++0x!
  Expr *ToExpr = Importer.Import(T->getUnderlyingExpr());
  if (!ToExpr)
    return {};

  QualType UnderlyingType = Importer.Import(T->getUnderlyingType());
  if (UnderlyingType.isNull())
    return {};

  return Importer.getToContext().getDecltypeType(ToExpr, UnderlyingType);
}

QualType ASTNodeImporter::VisitUnaryTransformType(const UnaryTransformType *T) {
  QualType ToBaseType = Importer.Import(T->getBaseType());
  QualType ToUnderlyingType = Importer.Import(T->getUnderlyingType());
  if (ToBaseType.isNull() || ToUnderlyingType.isNull())
    return {};

  return Importer.getToContext().getUnaryTransformType(ToBaseType,
                                                       ToUnderlyingType,
                                                       T->getUTTKind());
}

QualType ASTNodeImporter::VisitAutoType(const AutoType *T) {
  // FIXME: Make sure that the "to" context supports C++11!
  QualType FromDeduced = T->getDeducedType();
  QualType ToDeduced;
  if (!FromDeduced.isNull()) {
    ToDeduced = Importer.Import(FromDeduced);
    if (ToDeduced.isNull())
      return {};
  }

  return Importer.getToContext().getAutoType(ToDeduced, T->getKeyword(),
                                             /*IsDependent*/false);
}

QualType ASTNodeImporter::VisitInjectedClassNameType(
    const InjectedClassNameType *T) {
  auto *D = cast_or_null<CXXRecordDecl>(Importer.Import(T->getDecl()));
  if (!D)
    return {};

  QualType InjType = Importer.Import(T->getInjectedSpecializationType());
  if (InjType.isNull())
    return {};

  // FIXME: ASTContext::getInjectedClassNameType is not suitable for AST reading
  // See comments in InjectedClassNameType definition for details
  // return Importer.getToContext().getInjectedClassNameType(D, InjType);
  enum {
    TypeAlignmentInBits = 4,
    TypeAlignment = 1 << TypeAlignmentInBits
  };

  return QualType(new (Importer.getToContext(), TypeAlignment)
                  InjectedClassNameType(D, InjType), 0);
}

QualType ASTNodeImporter::VisitRecordType(const RecordType *T) {
  auto *ToDecl = dyn_cast_or_null<RecordDecl>(Importer.Import(T->getDecl()));
  if (!ToDecl)
    return {};

  return Importer.getToContext().getTagDeclType(ToDecl);
}

QualType ASTNodeImporter::VisitEnumType(const EnumType *T) {
  auto *ToDecl = dyn_cast_or_null<EnumDecl>(Importer.Import(T->getDecl()));
  if (!ToDecl)
    return {};

  return Importer.getToContext().getTagDeclType(ToDecl);
}

QualType ASTNodeImporter::VisitAttributedType(const AttributedType *T) {
  QualType FromModifiedType = T->getModifiedType();
  QualType FromEquivalentType = T->getEquivalentType();
  QualType ToModifiedType;
  QualType ToEquivalentType;

  if (!FromModifiedType.isNull()) {
    ToModifiedType = Importer.Import(FromModifiedType);
    if (ToModifiedType.isNull())
      return {};
  }
  if (!FromEquivalentType.isNull()) {
    ToEquivalentType = Importer.Import(FromEquivalentType);
    if (ToEquivalentType.isNull())
      return {};
  }

  return Importer.getToContext().getAttributedType(T->getAttrKind(),
    ToModifiedType, ToEquivalentType);
}

QualType ASTNodeImporter::VisitTemplateTypeParmType(
    const TemplateTypeParmType *T) {
  auto *ParmDecl =
      cast_or_null<TemplateTypeParmDecl>(Importer.Import(T->getDecl()));
  if (!ParmDecl && T->getDecl())
    return {};

  return Importer.getToContext().getTemplateTypeParmType(
        T->getDepth(), T->getIndex(), T->isParameterPack(), ParmDecl);
}

QualType ASTNodeImporter::VisitSubstTemplateTypeParmType(
    const SubstTemplateTypeParmType *T) {
  const auto *Replaced =
      cast_or_null<TemplateTypeParmType>(Importer.Import(
        QualType(T->getReplacedParameter(), 0)).getTypePtr());
  if (!Replaced)
    return {};

  QualType Replacement = Importer.Import(T->getReplacementType());
  if (Replacement.isNull())
    return {};
  Replacement = Replacement.getCanonicalType();

  return Importer.getToContext().getSubstTemplateTypeParmType(
        Replaced, Replacement);
}

QualType ASTNodeImporter::VisitTemplateSpecializationType(
                                       const TemplateSpecializationType *T) {
  TemplateName ToTemplate = Importer.Import(T->getTemplateName());
  if (ToTemplate.isNull())
    return {};

  SmallVector<TemplateArgument, 2> ToTemplateArgs;
  if (ImportTemplateArguments(T->getArgs(), T->getNumArgs(), ToTemplateArgs))
    return {};

  QualType ToCanonType;
  if (!QualType(T, 0).isCanonical()) {
    QualType FromCanonType
      = Importer.getFromContext().getCanonicalType(QualType(T, 0));
    ToCanonType =Importer.Import(FromCanonType);
    if (ToCanonType.isNull())
      return {};
  }
  return Importer.getToContext().getTemplateSpecializationType(ToTemplate,
                                                               ToTemplateArgs,
                                                               ToCanonType);
}

QualType ASTNodeImporter::VisitElaboratedType(const ElaboratedType *T) {
  NestedNameSpecifier *ToQualifier = nullptr;
  // Note: the qualifier in an ElaboratedType is optional.
  if (T->getQualifier()) {
    ToQualifier = Importer.Import(T->getQualifier());
    if (!ToQualifier)
      return {};
  }

  QualType ToNamedType = Importer.Import(T->getNamedType());
  if (ToNamedType.isNull())
    return {};

  TagDecl *OwnedTagDecl =
      cast_or_null<TagDecl>(Importer.Import(T->getOwnedTagDecl()));
  if (!OwnedTagDecl && T->getOwnedTagDecl())
    return {};

  return Importer.getToContext().getElaboratedType(T->getKeyword(),
                                                   ToQualifier, ToNamedType,
                                                   OwnedTagDecl);
}

QualType ASTNodeImporter::VisitPackExpansionType(const PackExpansionType *T) {
  QualType Pattern = Importer.Import(T->getPattern());
  if (Pattern.isNull())
    return {};

  return Importer.getToContext().getPackExpansionType(Pattern,
                                                      T->getNumExpansions());
}

QualType ASTNodeImporter::VisitDependentTemplateSpecializationType(
    const DependentTemplateSpecializationType *T) {
  NestedNameSpecifier *Qualifier = Importer.Import(T->getQualifier());
  if (!Qualifier && T->getQualifier())
    return {};

  IdentifierInfo *Name = Importer.Import(T->getIdentifier());
  if (!Name && T->getIdentifier())
    return {};

  SmallVector<TemplateArgument, 2> ToPack;
  ToPack.reserve(T->getNumArgs());
  if (ImportTemplateArguments(T->getArgs(), T->getNumArgs(), ToPack))
    return {};

  return Importer.getToContext().getDependentTemplateSpecializationType(
        T->getKeyword(), Qualifier, Name, ToPack);
}

QualType ASTNodeImporter::VisitDependentNameType(const DependentNameType *T) {
  NestedNameSpecifier *NNS = Importer.Import(T->getQualifier());
  if (!NNS && T->getQualifier())
    return QualType();

  IdentifierInfo *Name = Importer.Import(T->getIdentifier());
  if (!Name && T->getIdentifier())
    return QualType();

  QualType Canon = (T == T->getCanonicalTypeInternal().getTypePtr())
                       ? QualType()
                       : Importer.Import(T->getCanonicalTypeInternal());
  if (!Canon.isNull())
    Canon = Canon.getCanonicalType();

  return Importer.getToContext().getDependentNameType(T->getKeyword(), NNS,
                                                      Name, Canon);
}

QualType ASTNodeImporter::VisitObjCInterfaceType(const ObjCInterfaceType *T) {
  auto *Class =
      dyn_cast_or_null<ObjCInterfaceDecl>(Importer.Import(T->getDecl()));
  if (!Class)
    return {};

  return Importer.getToContext().getObjCInterfaceType(Class);
}

QualType ASTNodeImporter::VisitObjCObjectType(const ObjCObjectType *T) {
  QualType ToBaseType = Importer.Import(T->getBaseType());
  if (ToBaseType.isNull())
    return {};

  SmallVector<QualType, 4> TypeArgs;
  for (auto TypeArg : T->getTypeArgsAsWritten()) {
    QualType ImportedTypeArg = Importer.Import(TypeArg);
    if (ImportedTypeArg.isNull())
      return {};

    TypeArgs.push_back(ImportedTypeArg);
  }

  SmallVector<ObjCProtocolDecl *, 4> Protocols;
  for (auto *P : T->quals()) {
    auto *Protocol = dyn_cast_or_null<ObjCProtocolDecl>(Importer.Import(P));
    if (!Protocol)
      return {};
    Protocols.push_back(Protocol);
  }

  return Importer.getToContext().getObjCObjectType(ToBaseType, TypeArgs,
                                                   Protocols,
                                                   T->isKindOfTypeAsWritten());
}

QualType
ASTNodeImporter::VisitObjCObjectPointerType(const ObjCObjectPointerType *T) {
  QualType ToPointeeType = Importer.Import(T->getPointeeType());
  if (ToPointeeType.isNull())
    return {};

  return Importer.getToContext().getObjCObjectPointerType(ToPointeeType);
}

//----------------------------------------------------------------------------
// Import Declarations
//----------------------------------------------------------------------------
bool ASTNodeImporter::ImportDeclParts(NamedDecl *D, DeclContext *&DC,
                                      DeclContext *&LexicalDC,
                                      DeclarationName &Name,
                                      NamedDecl *&ToD,
                                      SourceLocation &Loc) {
  // Check if RecordDecl is in FunctionDecl parameters to avoid infinite loop.
  // example: int struct_in_proto(struct data_t{int a;int b;} *d);
  DeclContext *OrigDC = D->getDeclContext();
  FunctionDecl *FunDecl;
  if (isa<RecordDecl>(D) && (FunDecl = dyn_cast<FunctionDecl>(OrigDC)) &&
      FunDecl->hasBody()) {
    SourceRange RecR = D->getSourceRange();
    SourceRange BodyR = FunDecl->getBody()->getSourceRange();
    // If RecordDecl is not in Body (it is a param), we bail out.
    if (RecR.isValid() && BodyR.isValid() &&
        (RecR.getBegin() < BodyR.getBegin() ||
         BodyR.getEnd() < RecR.getEnd())) {
      Importer.FromDiag(D->getLocation(), diag::err_unsupported_ast_node)
          << D->getDeclKindName();
      return true;
    }
  }

  // Import the context of this declaration.
  DC = Importer.ImportContext(OrigDC);
  if (!DC)
    return true;

  LexicalDC = DC;
  if (D->getDeclContext() != D->getLexicalDeclContext()) {
    LexicalDC = Importer.ImportContext(D->getLexicalDeclContext());
    if (!LexicalDC)
      return true;
  }

  // Import the name of this declaration.
  Name = Importer.Import(D->getDeclName());
  if (D->getDeclName() && !Name)
    return true;

  // Import the location of this declaration.
  Loc = Importer.Import(D->getLocation());
  ToD = cast_or_null<NamedDecl>(Importer.GetAlreadyImportedOrNull(D));
  return false;
}

void ASTNodeImporter::ImportDefinitionIfNeeded(Decl *FromD, Decl *ToD) {
  if (!FromD)
    return;

  if (!ToD) {
    ToD = Importer.Import(FromD);
    if (!ToD)
      return;
  }

  if (auto *FromRecord = dyn_cast<RecordDecl>(FromD)) {
    if (auto *ToRecord = cast_or_null<RecordDecl>(ToD)) {
      if (FromRecord->getDefinition() && FromRecord->isCompleteDefinition() && !ToRecord->getDefinition()) {
        ImportDefinition(FromRecord, ToRecord);
      }
    }
    return;
  }

  if (auto *FromEnum = dyn_cast<EnumDecl>(FromD)) {
    if (auto *ToEnum = cast_or_null<EnumDecl>(ToD)) {
      if (FromEnum->getDefinition() && !ToEnum->getDefinition()) {
        ImportDefinition(FromEnum, ToEnum);
      }
    }
    return;
  }
}

void
ASTNodeImporter::ImportDeclarationNameLoc(const DeclarationNameInfo &From,
                                          DeclarationNameInfo& To) {
  // NOTE: To.Name and To.Loc are already imported.
  // We only have to import To.LocInfo.
  switch (To.getName().getNameKind()) {
  case DeclarationName::Identifier:
  case DeclarationName::ObjCZeroArgSelector:
  case DeclarationName::ObjCOneArgSelector:
  case DeclarationName::ObjCMultiArgSelector:
  case DeclarationName::CXXUsingDirective:
  case DeclarationName::CXXDeductionGuideName:
    return;

  case DeclarationName::CXXOperatorName: {
    SourceRange Range = From.getCXXOperatorNameRange();
    To.setCXXOperatorNameRange(Importer.Import(Range));
    return;
  }
  case DeclarationName::CXXLiteralOperatorName: {
    SourceLocation Loc = From.getCXXLiteralOperatorNameLoc();
    To.setCXXLiteralOperatorNameLoc(Importer.Import(Loc));
    return;
  }
  case DeclarationName::CXXConstructorName:
  case DeclarationName::CXXDestructorName:
  case DeclarationName::CXXConversionFunctionName: {
    TypeSourceInfo *FromTInfo = From.getNamedTypeInfo();
    To.setNamedTypeInfo(Importer.Import(FromTInfo));
    return;
  }
//ASUTTON ADDN:
  case DeclarationName::CXXIdExprName:
    SourceRange Range = From.getCXXOperatorNameRange();
    To.setCXXIdExprNameRange(Importer.Import(Range));
//END
  }
  llvm_unreachable("Unknown name kind.");
}

void ASTNodeImporter::ImportDeclContext(DeclContext *FromDC, bool ForceImport) {
  if (Importer.isMinimalImport() && !ForceImport) {
    Importer.ImportContext(FromDC);
    return;
  }

  for (auto *From : FromDC->decls())
    Importer.Import(From);
}

void ASTNodeImporter::ImportImplicitMethods(
    const CXXRecordDecl *From, CXXRecordDecl *To) {
  assert(From->isCompleteDefinition() && To->getDefinition() == To &&
      "Import implicit methods to or from non-definition");

  for (CXXMethodDecl *FromM : From->methods())
    if (FromM->isImplicit())
      Importer.Import(FromM);
}

static void setTypedefNameForAnonDecl(TagDecl *From, TagDecl *To,
                               ASTImporter &Importer) {
  if (TypedefNameDecl *FromTypedef = From->getTypedefNameForAnonDecl()) {
    auto *ToTypedef =
      cast_or_null<TypedefNameDecl>(Importer.Import(FromTypedef));
    assert (ToTypedef && "Failed to import typedef of an anonymous structure");

    To->setTypedefNameForAnonDecl(ToTypedef);
  }
}

bool ASTNodeImporter::ImportDefinition(RecordDecl *From, RecordDecl *To,
                                       ImportDefinitionKind Kind) {
  if (To->getDefinition() || To->isBeingDefined()) {
    if (Kind == IDK_Everything)
      ImportDeclContext(From, /*ForceImport=*/true);

    return false;
  }

  To->startDefinition();

  setTypedefNameForAnonDecl(From, To, Importer);

  // Add base classes.
  if (auto *ToCXX = dyn_cast<CXXRecordDecl>(To)) {
    auto *FromCXX = cast<CXXRecordDecl>(From);

    struct CXXRecordDecl::DefinitionData &ToData = ToCXX->data();
    struct CXXRecordDecl::DefinitionData &FromData = FromCXX->data();
    ToData.UserDeclaredConstructor = FromData.UserDeclaredConstructor;
    ToData.UserDeclaredSpecialMembers = FromData.UserDeclaredSpecialMembers;
    ToData.Aggregate = FromData.Aggregate;
    ToData.PlainOldData = FromData.PlainOldData;
    ToData.Empty = FromData.Empty;
    ToData.Polymorphic = FromData.Polymorphic;
    ToData.Abstract = FromData.Abstract;
    ToData.IsStandardLayout = FromData.IsStandardLayout;
    ToData.IsCXX11StandardLayout = FromData.IsCXX11StandardLayout;
    ToData.HasBasesWithFields = FromData.HasBasesWithFields;
    ToData.HasBasesWithNonStaticDataMembers =
        FromData.HasBasesWithNonStaticDataMembers;
    ToData.HasPrivateFields = FromData.HasPrivateFields;
    ToData.HasProtectedFields = FromData.HasProtectedFields;
    ToData.HasPublicFields = FromData.HasPublicFields;
    ToData.HasMutableFields = FromData.HasMutableFields;
    ToData.HasVariantMembers = FromData.HasVariantMembers;
    ToData.HasOnlyCMembers = FromData.HasOnlyCMembers;
    ToData.HasInClassInitializer = FromData.HasInClassInitializer;
    ToData.HasUninitializedReferenceMember
      = FromData.HasUninitializedReferenceMember;
    ToData.HasUninitializedFields = FromData.HasUninitializedFields;
    ToData.HasInheritedConstructor = FromData.HasInheritedConstructor;
    ToData.HasInheritedAssignment = FromData.HasInheritedAssignment;
    ToData.NeedOverloadResolutionForCopyConstructor
      = FromData.NeedOverloadResolutionForCopyConstructor;
    ToData.NeedOverloadResolutionForMoveConstructor
      = FromData.NeedOverloadResolutionForMoveConstructor;
    ToData.NeedOverloadResolutionForMoveAssignment
      = FromData.NeedOverloadResolutionForMoveAssignment;
    ToData.NeedOverloadResolutionForDestructor
      = FromData.NeedOverloadResolutionForDestructor;
    ToData.DefaultedCopyConstructorIsDeleted
      = FromData.DefaultedCopyConstructorIsDeleted;
    ToData.DefaultedMoveConstructorIsDeleted
      = FromData.DefaultedMoveConstructorIsDeleted;
    ToData.DefaultedMoveAssignmentIsDeleted
      = FromData.DefaultedMoveAssignmentIsDeleted;
    ToData.DefaultedDestructorIsDeleted = FromData.DefaultedDestructorIsDeleted;
    ToData.HasTrivialSpecialMembers = FromData.HasTrivialSpecialMembers;
    ToData.HasIrrelevantDestructor = FromData.HasIrrelevantDestructor;
    ToData.HasConstexprNonCopyMoveConstructor
      = FromData.HasConstexprNonCopyMoveConstructor;
    ToData.HasDefaultedDefaultConstructor
      = FromData.HasDefaultedDefaultConstructor;
    ToData.DefaultedDefaultConstructorIsConstexpr
      = FromData.DefaultedDefaultConstructorIsConstexpr;
    ToData.HasConstexprDefaultConstructor
      = FromData.HasConstexprDefaultConstructor;
    ToData.HasNonLiteralTypeFieldsOrBases
      = FromData.HasNonLiteralTypeFieldsOrBases;
    // ComputedVisibleConversions not imported.
    ToData.UserProvidedDefaultConstructor
      = FromData.UserProvidedDefaultConstructor;
    ToData.DeclaredSpecialMembers = FromData.DeclaredSpecialMembers;
    ToData.ImplicitCopyConstructorCanHaveConstParamForVBase
      = FromData.ImplicitCopyConstructorCanHaveConstParamForVBase;
    ToData.ImplicitCopyConstructorCanHaveConstParamForNonVBase
      = FromData.ImplicitCopyConstructorCanHaveConstParamForNonVBase;
    ToData.ImplicitCopyAssignmentHasConstParam
      = FromData.ImplicitCopyAssignmentHasConstParam;
    ToData.HasDeclaredCopyConstructorWithConstParam
      = FromData.HasDeclaredCopyConstructorWithConstParam;
    ToData.HasDeclaredCopyAssignmentWithConstParam
      = FromData.HasDeclaredCopyAssignmentWithConstParam;

    SmallVector<CXXBaseSpecifier *, 4> Bases;
    for (const auto &Base1 : FromCXX->bases()) {
      QualType T = Importer.Import(Base1.getType());
      if (T.isNull())
        return true;

      SourceLocation EllipsisLoc;
      if (Base1.isPackExpansion())
        EllipsisLoc = Importer.Import(Base1.getEllipsisLoc());

      // Ensure that we have a definition for the base.
      ImportDefinitionIfNeeded(Base1.getType()->getAsCXXRecordDecl());

      Bases.push_back(
                    new (Importer.getToContext())
                      CXXBaseSpecifier(Importer.Import(Base1.getSourceRange()),
                                       Base1.isVirtual(),
                                       Base1.isBaseOfClass(),
                                       Base1.getAccessSpecifierAsWritten(),
                                   Importer.Import(Base1.getTypeSourceInfo()),
                                       EllipsisLoc));
    }
    if (!Bases.empty())
      ToCXX->setBases(Bases.data(), Bases.size());
  }

  if (shouldForceImportDeclContext(Kind))
    ImportDeclContext(From, /*ForceImport=*/true);

  To->completeDefinition();
  return false;
}

bool ASTNodeImporter::ImportDefinition(VarDecl *From, VarDecl *To,
                                       ImportDefinitionKind Kind) {
  if (To->getAnyInitializer())
    return false;

  // FIXME: Can we really import any initializer? Alternatively, we could force
  // ourselves to import every declaration of a variable and then only use
  // getInit() here.
  To->setInit(Importer.Import(const_cast<Expr *>(From->getAnyInitializer())));

  // FIXME: Other bits to merge?

  return false;
}

bool ASTNodeImporter::ImportDefinition(EnumDecl *From, EnumDecl *To,
                                       ImportDefinitionKind Kind) {
  if (To->getDefinition() || To->isBeingDefined()) {
    if (Kind == IDK_Everything)
      ImportDeclContext(From, /*ForceImport=*/true);
    return false;
  }

  To->startDefinition();

  setTypedefNameForAnonDecl(From, To, Importer);

  QualType T = Importer.Import(Importer.getFromContext().getTypeDeclType(From));
  if (T.isNull())
    return true;

  QualType ToPromotionType = Importer.Import(From->getPromotionType());
  if (ToPromotionType.isNull())
    return true;

  if (shouldForceImportDeclContext(Kind))
    ImportDeclContext(From, /*ForceImport=*/true);

  // FIXME: we might need to merge the number of positive or negative bits
  // if the enumerator lists don't match.
  To->completeDefinition(T, ToPromotionType,
                         From->getNumPositiveBits(),
                         From->getNumNegativeBits());
  return false;
}

TemplateParameterList *ASTNodeImporter::ImportTemplateParameterList(
                                                TemplateParameterList *Params) {
  SmallVector<NamedDecl *, 4> ToParams(Params->size());
  if (ImportContainerChecked(*Params, ToParams))
    return nullptr;

  Expr *ToRequiresClause;
  if (Expr *const R = Params->getRequiresClause()) {
    ToRequiresClause = Importer.Import(R);
    if (!ToRequiresClause)
      return nullptr;
  } else {
    ToRequiresClause = nullptr;
  }

  return TemplateParameterList::Create(Importer.getToContext(),
                                       Importer.Import(Params->getTemplateLoc()),
                                       Importer.Import(Params->getLAngleLoc()),
                                       ToParams,
                                       Importer.Import(Params->getRAngleLoc()),
                                       ToRequiresClause);
}

TemplateArgument
ASTNodeImporter::ImportTemplateArgument(const TemplateArgument &From) {
  switch (From.getKind()) {
  case TemplateArgument::Null:
    return TemplateArgument();

  case TemplateArgument::Type: {
    QualType ToType = Importer.Import(From.getAsType());
    if (ToType.isNull())
      return {};
    return TemplateArgument(ToType);
  }

  case TemplateArgument::Integral: {
    QualType ToType = Importer.Import(From.getIntegralType());
    if (ToType.isNull())
      return {};
    return TemplateArgument(From, ToType);
  }

  case TemplateArgument::Declaration: {
    auto *To = cast_or_null<ValueDecl>(Importer.Import(From.getAsDecl()));
    QualType ToType = Importer.Import(From.getParamTypeForDecl());
    if (!To || ToType.isNull())
      return {};
    return TemplateArgument(To, ToType);
  }

  case TemplateArgument::NullPtr: {
    QualType ToType = Importer.Import(From.getNullPtrType());
    if (ToType.isNull())
      return {};
    return TemplateArgument(ToType, /*isNullPtr*/true);
  }

  case TemplateArgument::Template: {
    TemplateName ToTemplate = Importer.Import(From.getAsTemplate());
    if (ToTemplate.isNull())
      return {};

    return TemplateArgument(ToTemplate);
  }

  case TemplateArgument::TemplateExpansion: {
    TemplateName ToTemplate
      = Importer.Import(From.getAsTemplateOrTemplatePattern());
    if (ToTemplate.isNull())
      return {};

    return TemplateArgument(ToTemplate, From.getNumTemplateExpansions());
  }

  case TemplateArgument::Expression:
    if (Expr *ToExpr = Importer.Import(From.getAsExpr()))
      return TemplateArgument(ToExpr);
    return TemplateArgument();

  case TemplateArgument::Pack: {
    SmallVector<TemplateArgument, 2> ToPack;
    ToPack.reserve(From.pack_size());
    if (ImportTemplateArguments(From.pack_begin(), From.pack_size(), ToPack))
      return {};

    return TemplateArgument(
        llvm::makeArrayRef(ToPack).copy(Importer.getToContext()));
  }
  }

  llvm_unreachable("Invalid template argument kind");
}

Optional<TemplateArgumentLoc>
ASTNodeImporter::ImportTemplateArgumentLoc(const TemplateArgumentLoc &TALoc) {
  TemplateArgument Arg = ImportTemplateArgument(TALoc.getArgument());
  TemplateArgumentLocInfo FromInfo = TALoc.getLocInfo();
  TemplateArgumentLocInfo ToInfo;
  if (Arg.getKind() == TemplateArgument::Expression) {
    Expr *E = Importer.Import(FromInfo.getAsExpr());
    ToInfo = TemplateArgumentLocInfo(E);
    if (!E)
      return None;
  } else if (Arg.getKind() == TemplateArgument::Type) {
    if (TypeSourceInfo *TSI = Importer.Import(FromInfo.getAsTypeSourceInfo()))
      ToInfo = TemplateArgumentLocInfo(TSI);
    else
      return None;
  } else {
    ToInfo = TemplateArgumentLocInfo(
          Importer.Import(FromInfo.getTemplateQualifierLoc()),
          Importer.Import(FromInfo.getTemplateNameLoc()),
          Importer.Import(FromInfo.getTemplateEllipsisLoc()));
  }
  return TemplateArgumentLoc(Arg, ToInfo);
}

bool ASTNodeImporter::ImportTemplateArguments(const TemplateArgument *FromArgs,
                                              unsigned NumFromArgs,
                              SmallVectorImpl<TemplateArgument> &ToArgs) {
  for (unsigned I = 0; I != NumFromArgs; ++I) {
    TemplateArgument To = ImportTemplateArgument(FromArgs[I]);
    if (To.isNull() && !FromArgs[I].isNull())
      return true;

    ToArgs.push_back(To);
  }

  return false;
}

// We cannot use Optional<> pattern here and below because
// TemplateArgumentListInfo's operator new is declared as deleted so it cannot
// be stored in Optional.
template <typename InContainerTy>
bool ASTNodeImporter::ImportTemplateArgumentListInfo(
    const InContainerTy &Container, TemplateArgumentListInfo &ToTAInfo) {
  for (const auto &FromLoc : Container) {
    if (auto ToLoc = ImportTemplateArgumentLoc(FromLoc))
      ToTAInfo.addArgument(*ToLoc);
    else
      return true;
  }
  return false;
}

static StructuralEquivalenceKind
getStructuralEquivalenceKind(const ASTImporter &Importer) {
  return Importer.isMinimalImport() ? StructuralEquivalenceKind::Minimal
                                    : StructuralEquivalenceKind::Default;
}

bool ASTNodeImporter::IsStructuralMatch(Decl *From, Decl *To, bool Complain) {
  StructuralEquivalenceContext Ctx(
      Importer.getFromContext(), Importer.getToContext(),
      Importer.getNonEquivalentDecls(), getStructuralEquivalenceKind(Importer),
      false, Complain);
  return Ctx.IsEquivalent(From, To);
}

bool ASTNodeImporter::IsStructuralMatch(RecordDecl *FromRecord,
                                        RecordDecl *ToRecord, bool Complain) {
  // Eliminate a potential failure point where we attempt to re-import
  // something we're trying to import while completing ToRecord.
  Decl *ToOrigin = Importer.GetOriginalDecl(ToRecord);
  if (ToOrigin) {
    auto *ToOriginRecord = dyn_cast<RecordDecl>(ToOrigin);
    if (ToOriginRecord)
      ToRecord = ToOriginRecord;
  }

  StructuralEquivalenceContext Ctx(Importer.getFromContext(),
                                   ToRecord->getASTContext(),
                                   Importer.getNonEquivalentDecls(),
                                   getStructuralEquivalenceKind(Importer),
                                   false, Complain);
  return Ctx.IsEquivalent(FromRecord, ToRecord);
}

bool ASTNodeImporter::IsStructuralMatch(VarDecl *FromVar, VarDecl *ToVar,
                                        bool Complain) {
  StructuralEquivalenceContext Ctx(
      Importer.getFromContext(), Importer.getToContext(),
      Importer.getNonEquivalentDecls(), getStructuralEquivalenceKind(Importer),
      false, Complain);
  return Ctx.IsEquivalent(FromVar, ToVar);
}

bool ASTNodeImporter::IsStructuralMatch(EnumDecl *FromEnum, EnumDecl *ToEnum) {
  StructuralEquivalenceContext Ctx(
      Importer.getFromContext(), Importer.getToContext(),
      Importer.getNonEquivalentDecls(), getStructuralEquivalenceKind(Importer));
  return Ctx.IsEquivalent(FromEnum, ToEnum);
}

bool ASTNodeImporter::IsStructuralMatch(FunctionTemplateDecl *From,
                                        FunctionTemplateDecl *To) {
  StructuralEquivalenceContext Ctx(
      Importer.getFromContext(), Importer.getToContext(),
      Importer.getNonEquivalentDecls(), getStructuralEquivalenceKind(Importer),
      false, false);
  return Ctx.IsEquivalent(From, To);
}

bool ASTNodeImporter::IsStructuralMatch(FunctionDecl *From, FunctionDecl *To) {
  StructuralEquivalenceContext Ctx(
      Importer.getFromContext(), Importer.getToContext(),
      Importer.getNonEquivalentDecls(), getStructuralEquivalenceKind(Importer),
      false, false);
  return Ctx.IsEquivalent(From, To);
}

bool ASTNodeImporter::IsStructuralMatch(EnumConstantDecl *FromEC,
                                        EnumConstantDecl *ToEC) {
  const llvm::APSInt &FromVal = FromEC->getInitVal();
  const llvm::APSInt &ToVal = ToEC->getInitVal();

  return FromVal.isSigned() == ToVal.isSigned() &&
         FromVal.getBitWidth() == ToVal.getBitWidth() &&
         FromVal == ToVal;
}

bool ASTNodeImporter::IsStructuralMatch(ClassTemplateDecl *From,
                                        ClassTemplateDecl *To) {
  StructuralEquivalenceContext Ctx(Importer.getFromContext(),
                                   Importer.getToContext(),
                                   Importer.getNonEquivalentDecls(),
                                   getStructuralEquivalenceKind(Importer));
  return Ctx.IsEquivalent(From, To);
}

bool ASTNodeImporter::IsStructuralMatch(VarTemplateDecl *From,
                                        VarTemplateDecl *To) {
  StructuralEquivalenceContext Ctx(Importer.getFromContext(),
                                   Importer.getToContext(),
                                   Importer.getNonEquivalentDecls(),
                                   getStructuralEquivalenceKind(Importer));
  return Ctx.IsEquivalent(From, To);
}

Decl *ASTNodeImporter::VisitDecl(Decl *D) {
  Importer.FromDiag(D->getLocation(), diag::err_unsupported_ast_node)
    << D->getDeclKindName();
  return nullptr;
}

Decl *ASTNodeImporter::VisitEmptyDecl(EmptyDecl *D) {
  // Import the context of this declaration.
  DeclContext *DC = Importer.ImportContext(D->getDeclContext());
  if (!DC)
    return nullptr;

  DeclContext *LexicalDC = DC;
  if (D->getDeclContext() != D->getLexicalDeclContext()) {
    LexicalDC = Importer.ImportContext(D->getLexicalDeclContext());
    if (!LexicalDC)
      return nullptr;
  }

  // Import the location of this declaration.
  SourceLocation Loc = Importer.Import(D->getLocation());

  EmptyDecl *ToD;
  if (GetImportedOrCreateDecl(ToD, D, Importer.getToContext(), DC, Loc))
    return ToD;

  ToD->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToD);
  return ToD;
}

Decl *ASTNodeImporter::VisitTranslationUnitDecl(TranslationUnitDecl *D) {
  TranslationUnitDecl *ToD =
    Importer.getToContext().getTranslationUnitDecl();

  Importer.MapImported(D, ToD);

  return ToD;
}

Decl *ASTNodeImporter::VisitAccessSpecDecl(AccessSpecDecl *D) {
  SourceLocation Loc = Importer.Import(D->getLocation());
  SourceLocation ColonLoc = Importer.Import(D->getColonLoc());

  // Import the context of this declaration.
  DeclContext *DC = Importer.ImportContext(D->getDeclContext());
  if (!DC)
    return nullptr;

  AccessSpecDecl *ToD;
  if (GetImportedOrCreateDecl(ToD, D, Importer.getToContext(), D->getAccess(),
                              DC, Loc, ColonLoc))
    return ToD;

  // Lexical DeclContext and Semantic DeclContext
  // is always the same for the accessSpec.
  ToD->setLexicalDeclContext(DC);
  DC->addDeclInternal(ToD);

  return ToD;
}

Decl *ASTNodeImporter::VisitStaticAssertDecl(StaticAssertDecl *D) {
  DeclContext *DC = Importer.ImportContext(D->getDeclContext());
  if (!DC)
    return nullptr;

  DeclContext *LexicalDC = DC;

  // Import the location of this declaration.
  SourceLocation Loc = Importer.Import(D->getLocation());

  Expr *AssertExpr = Importer.Import(D->getAssertExpr());
  if (!AssertExpr)
    return nullptr;

  StringLiteral *FromMsg = D->getMessage();
  auto *ToMsg = cast_or_null<StringLiteral>(Importer.Import(FromMsg));
  if (!ToMsg && FromMsg)
    return nullptr;

  StaticAssertDecl *ToD;
  if (GetImportedOrCreateDecl(
          ToD, D, Importer.getToContext(), DC, Loc, AssertExpr, ToMsg,
          Importer.Import(D->getRParenLoc()), D->isFailed()))
    return ToD;

  ToD->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToD);
  return ToD;
}

Decl *ASTNodeImporter::VisitNamespaceDecl(NamespaceDecl *D) {
  // Import the major distinguishing characteristics of this namespace.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  NamespaceDecl *MergeWithNamespace = nullptr;
  if (!Name) {
    // This is an anonymous namespace. Adopt an existing anonymous
    // namespace if we can.
    // FIXME: Not testable.
    if (auto *TU = dyn_cast<TranslationUnitDecl>(DC))
      MergeWithNamespace = TU->getAnonymousNamespace();
    else
      MergeWithNamespace = cast<NamespaceDecl>(DC)->getAnonymousNamespace();
  } else {
    SmallVector<NamedDecl *, 4> ConflictingDecls;
    SmallVector<NamedDecl *, 2> FoundDecls;
    DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
    for (auto *FoundDecl : FoundDecls) {
      if (!FoundDecl->isInIdentifierNamespace(Decl::IDNS_Namespace))
        continue;

      if (auto *FoundNS = dyn_cast<NamespaceDecl>(FoundDecl)) {
        MergeWithNamespace = FoundNS;
        ConflictingDecls.clear();
        break;
      }

      ConflictingDecls.push_back(FoundDecl);
    }

    if (!ConflictingDecls.empty()) {
      Name = Importer.HandleNameConflict(Name, DC, Decl::IDNS_Namespace,
                                         ConflictingDecls.data(),
                                         ConflictingDecls.size());
    }
  }

  // Create the "to" namespace, if needed.
  NamespaceDecl *ToNamespace = MergeWithNamespace;
  if (!ToNamespace) {
    if (GetImportedOrCreateDecl(
            ToNamespace, D, Importer.getToContext(), DC, D->isInline(),
            Importer.Import(D->getLocStart()), Loc, Name.getAsIdentifierInfo(),
            /*PrevDecl=*/nullptr))
      return ToNamespace;
    ToNamespace->setLexicalDeclContext(LexicalDC);
    LexicalDC->addDeclInternal(ToNamespace);

    // If this is an anonymous namespace, register it as the anonymous
    // namespace within its context.
    if (!Name) {
      if (auto *TU = dyn_cast<TranslationUnitDecl>(DC))
        TU->setAnonymousNamespace(ToNamespace);
      else
        cast<NamespaceDecl>(DC)->setAnonymousNamespace(ToNamespace);
    }
  }
  Importer.MapImported(D, ToNamespace);

  ImportDeclContext(D);

  return ToNamespace;
}

Decl *ASTNodeImporter::VisitNamespaceAliasDecl(NamespaceAliasDecl *D) {
  // Import the major distinguishing characteristics of this namespace.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *LookupD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, LookupD, Loc))
    return nullptr;
  if (LookupD)
    return LookupD;

  // NOTE: No conflict resolution is done for namespace aliases now.

  auto *TargetDecl = cast_or_null<NamespaceDecl>(
      Importer.Import(D->getNamespace()));
  if (!TargetDecl)
    return nullptr;

  IdentifierInfo *ToII = Importer.Import(D->getIdentifier());
  if (!ToII)
    return nullptr;

  NestedNameSpecifierLoc ToQLoc = Importer.Import(D->getQualifierLoc());
  if (D->getQualifierLoc() && !ToQLoc)
    return nullptr;

  NamespaceAliasDecl *ToD;
  if (GetImportedOrCreateDecl(ToD, D, Importer.getToContext(), DC,
                              Importer.Import(D->getNamespaceLoc()),
                              Importer.Import(D->getAliasLoc()), ToII, ToQLoc,
                              Importer.Import(D->getTargetNameLoc()),
                              TargetDecl))
    return ToD;

  ToD->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToD);

  return ToD;
}

Decl *ASTNodeImporter::VisitTypedefNameDecl(TypedefNameDecl *D, bool IsAlias) {
  // Import the major distinguishing characteristics of this typedef.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  // If this typedef is not in block scope, determine whether we've
  // seen a typedef with the same name (that we can merge with) or any
  // other entity by that name (which name lookup could conflict with).
  if (!DC->isFunctionOrMethod()) {
    SmallVector<NamedDecl *, 4> ConflictingDecls;
    unsigned IDNS = Decl::IDNS_Ordinary;
    SmallVector<NamedDecl *, 2> FoundDecls;
    DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
    for (auto *FoundDecl : FoundDecls) {
      if (!FoundDecl->isInIdentifierNamespace(IDNS))
        continue;
      if (auto *FoundTypedef = dyn_cast<TypedefNameDecl>(FoundDecl)) {
        if (Importer.IsStructurallyEquivalent(D->getUnderlyingType(),
                                            FoundTypedef->getUnderlyingType()))
          return Importer.MapImported(D, FoundTypedef);
      }

      ConflictingDecls.push_back(FoundDecl);
    }

    if (!ConflictingDecls.empty()) {
      Name = Importer.HandleNameConflict(Name, DC, IDNS,
                                         ConflictingDecls.data(),
                                         ConflictingDecls.size());
      if (!Name)
        return nullptr;
    }
  }

  // Import the underlying type of this typedef;
  QualType T = Importer.Import(D->getUnderlyingType());
  if (T.isNull())
    return nullptr;

  // Create the new typedef node.
  TypeSourceInfo *TInfo = Importer.Import(D->getTypeSourceInfo());
  SourceLocation StartL = Importer.Import(D->getLocStart());

  TypedefNameDecl *ToTypedef;
  if (IsAlias) {
    if (GetImportedOrCreateDecl<TypeAliasDecl>(
            ToTypedef, D, Importer.getToContext(), DC, StartL, Loc,
            Name.getAsIdentifierInfo(), TInfo))
      return ToTypedef;
  } else if (GetImportedOrCreateDecl<TypedefDecl>(
                 ToTypedef, D, Importer.getToContext(), DC, StartL, Loc,
                 Name.getAsIdentifierInfo(), TInfo))
    return ToTypedef;

  ToTypedef->setAccess(D->getAccess());
  ToTypedef->setLexicalDeclContext(LexicalDC);

  // Templated declarations should not appear in DeclContext.
  TypeAliasDecl *FromAlias = IsAlias ? cast<TypeAliasDecl>(D) : nullptr;
  if (!FromAlias || !FromAlias->getDescribedAliasTemplate())
    LexicalDC->addDeclInternal(ToTypedef);

  return ToTypedef;
}

Decl *ASTNodeImporter::VisitTypedefDecl(TypedefDecl *D) {
  return VisitTypedefNameDecl(D, /*IsAlias=*/false);
}

Decl *ASTNodeImporter::VisitTypeAliasDecl(TypeAliasDecl *D) {
  return VisitTypedefNameDecl(D, /*IsAlias=*/true);
}

Decl *ASTNodeImporter::VisitTypeAliasTemplateDecl(TypeAliasTemplateDecl *D) {
  // Import the major distinguishing characteristics of this typedef.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *FoundD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, FoundD, Loc))
    return nullptr;
  if (FoundD)
    return FoundD;

  // If this typedef is not in block scope, determine whether we've
  // seen a typedef with the same name (that we can merge with) or any
  // other entity by that name (which name lookup could conflict with).
  if (!DC->isFunctionOrMethod()) {
    SmallVector<NamedDecl *, 4> ConflictingDecls;
    unsigned IDNS = Decl::IDNS_Ordinary;
    SmallVector<NamedDecl *, 2> FoundDecls;
    DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
    for (auto *FoundDecl : FoundDecls) {
      if (!FoundDecl->isInIdentifierNamespace(IDNS))
        continue;
      if (auto *FoundAlias = dyn_cast<TypeAliasTemplateDecl>(FoundDecl))
        return Importer.MapImported(D, FoundAlias);
      ConflictingDecls.push_back(FoundDecl);
    }

    if (!ConflictingDecls.empty()) {
      Name = Importer.HandleNameConflict(Name, DC, IDNS,
                                         ConflictingDecls.data(),
                                         ConflictingDecls.size());
      if (!Name)
        return nullptr;
    }
  }

  TemplateParameterList *Params = ImportTemplateParameterList(
        D->getTemplateParameters());
  if (!Params)
    return nullptr;

  auto *TemplDecl = cast_or_null<TypeAliasDecl>(
        Importer.Import(D->getTemplatedDecl()));
  if (!TemplDecl)
    return nullptr;

  TypeAliasTemplateDecl *ToAlias;
  if (GetImportedOrCreateDecl(ToAlias, D, Importer.getToContext(), DC, Loc,
                              Name, Params, TemplDecl))
    return ToAlias;

  TemplDecl->setDescribedAliasTemplate(ToAlias);

  ToAlias->setAccess(D->getAccess());
  ToAlias->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToAlias);
  return ToAlias;
}

Decl *ASTNodeImporter::VisitLabelDecl(LabelDecl *D) {
  // Import the major distinguishing characteristics of this label.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  assert(LexicalDC->isFunctionOrMethod());

  LabelDecl *ToLabel;
  if (D->isGnuLocal()
          ? GetImportedOrCreateDecl(ToLabel, D, Importer.getToContext(), DC,
                                    Importer.Import(D->getLocation()),
                                    Name.getAsIdentifierInfo(),
                                    Importer.Import(D->getLocStart()))
          : GetImportedOrCreateDecl(ToLabel, D, Importer.getToContext(), DC,
                                    Importer.Import(D->getLocation()),
                                    Name.getAsIdentifierInfo()))
    return ToLabel;

  auto *Label = cast_or_null<LabelStmt>(Importer.Import(D->getStmt()));
  if (!Label)
    return nullptr;

  ToLabel->setStmt(Label);
  ToLabel->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToLabel);
  return ToLabel;
}

Decl *ASTNodeImporter::VisitEnumDecl(EnumDecl *D) {
  // Import the major distinguishing characteristics of this enum.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  // Figure out what enum name we're looking for.
  unsigned IDNS = Decl::IDNS_Tag;
  DeclarationName SearchName = Name;
  if (!SearchName && D->getTypedefNameForAnonDecl()) {
    SearchName = Importer.Import(D->getTypedefNameForAnonDecl()->getDeclName());
    IDNS = Decl::IDNS_Ordinary;
  } else if (Importer.getToContext().getLangOpts().CPlusPlus)
    IDNS |= Decl::IDNS_Ordinary;

  // We may already have an enum of the same name; try to find and match it.
  if (!DC->isFunctionOrMethod() && SearchName) {
    SmallVector<NamedDecl *, 4> ConflictingDecls;
    SmallVector<NamedDecl *, 2> FoundDecls;
    DC->getRedeclContext()->localUncachedLookup(SearchName, FoundDecls);
    for (auto *FoundDecl : FoundDecls) {
      if (!FoundDecl->isInIdentifierNamespace(IDNS))
        continue;

      Decl *Found = FoundDecl;
      if (auto *Typedef = dyn_cast<TypedefNameDecl>(Found)) {
        if (const auto *Tag = Typedef->getUnderlyingType()->getAs<TagType>())
          Found = Tag->getDecl();
      }

      if (auto *FoundEnum = dyn_cast<EnumDecl>(Found)) {
        if (IsStructuralMatch(D, FoundEnum))
          return Importer.MapImported(D, FoundEnum);
      }

      ConflictingDecls.push_back(FoundDecl);
    }

    if (!ConflictingDecls.empty()) {
      Name = Importer.HandleNameConflict(Name, DC, IDNS,
                                         ConflictingDecls.data(),
                                         ConflictingDecls.size());
    }
  }

  // Create the enum declaration.
  EnumDecl *D2;
  if (GetImportedOrCreateDecl(
          D2, D, Importer.getToContext(), DC, Importer.Import(D->getLocStart()),
          Loc, Name.getAsIdentifierInfo(), nullptr, D->isScoped(),
          D->isScopedUsingClassTag(), D->isFixed()))
    return D2;

  // Import the qualifier, if any.
  D2->setQualifierInfo(Importer.Import(D->getQualifierLoc()));
  D2->setAccess(D->getAccess());
  D2->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(D2);

  // Import the integer type.
  QualType ToIntegerType = Importer.Import(D->getIntegerType());
  if (ToIntegerType.isNull())
    return nullptr;
  D2->setIntegerType(ToIntegerType);

  // Import the definition
  if (D->isCompleteDefinition() && ImportDefinition(D, D2))
    return nullptr;

  return D2;
}

Decl *ASTNodeImporter::VisitRecordDecl(RecordDecl *D) {
  // If this record has a definition in the translation unit we're coming from,
  // but this particular declaration is not that definition, import the
  // definition and map to that.
  TagDecl *Definition = D->getDefinition();
  if (Definition && Definition != D &&
      // In contrast to a normal CXXRecordDecl, the implicit
      // CXXRecordDecl of ClassTemplateSpecializationDecl is its redeclaration.
      // The definition of the implicit CXXRecordDecl in this case is the
      // ClassTemplateSpecializationDecl itself. Thus, we start with an extra
      // condition in order to be able to import the implict Decl.
      !D->isImplicit()) {
    Decl *ImportedDef = Importer.Import(Definition);
    if (!ImportedDef)
      return nullptr;

    return Importer.MapImported(D, ImportedDef);
  }

  // Import the major distinguishing characteristics of this record.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  // Figure out what structure name we're looking for.
  unsigned IDNS = Decl::IDNS_Tag;
  DeclarationName SearchName = Name;
  if (!SearchName && D->getTypedefNameForAnonDecl()) {
    SearchName = Importer.Import(D->getTypedefNameForAnonDecl()->getDeclName());
    IDNS = Decl::IDNS_Ordinary;
  } else if (Importer.getToContext().getLangOpts().CPlusPlus)
    IDNS |= Decl::IDNS_Ordinary;

  // We may already have a record of the same name; try to find and match it.
  RecordDecl *AdoptDecl = nullptr;
  RecordDecl *PrevDecl = nullptr;
  if (!DC->isFunctionOrMethod()) {
    SmallVector<NamedDecl *, 4> ConflictingDecls;
    SmallVector<NamedDecl *, 2> FoundDecls;
    DC->getRedeclContext()->localUncachedLookup(SearchName, FoundDecls);

    if (!FoundDecls.empty()) {
      // We're going to have to compare D against potentially conflicting Decls, so complete it.
      if (D->hasExternalLexicalStorage() && !D->isCompleteDefinition())
        D->getASTContext().getExternalSource()->CompleteType(D);
    }

    for (auto *FoundDecl : FoundDecls) {
      if (!FoundDecl->isInIdentifierNamespace(IDNS))
        continue;

      Decl *Found = FoundDecl;
      if (auto *Typedef = dyn_cast<TypedefNameDecl>(Found)) {
        if (const auto *Tag = Typedef->getUnderlyingType()->getAs<TagType>())
          Found = Tag->getDecl();
      }

      if (D->getDescribedTemplate()) {
        if (auto *Template = dyn_cast<ClassTemplateDecl>(Found))
          Found = Template->getTemplatedDecl();
        else
          continue;
      }

      if (auto *FoundRecord = dyn_cast<RecordDecl>(Found)) {
        if (!SearchName) {
          if (!IsStructuralMatch(D, FoundRecord, false))
            continue;
        }

        PrevDecl = FoundRecord;

        if (RecordDecl *FoundDef = FoundRecord->getDefinition()) {
          if ((SearchName && !D->isCompleteDefinition())
              || (D->isCompleteDefinition() &&
                  D->isAnonymousStructOrUnion()
                    == FoundDef->isAnonymousStructOrUnion() &&
                  IsStructuralMatch(D, FoundDef))) {
            // The record types structurally match, or the "from" translation
            // unit only had a forward declaration anyway; call it the same
            // function.
            // FIXME: Structural equivalence check should check for same
            // user-defined methods.
            Importer.MapImported(D, FoundDef);
            if (const auto *DCXX = dyn_cast<CXXRecordDecl>(D)) {
              auto *FoundCXX = dyn_cast<CXXRecordDecl>(FoundDef);
              assert(FoundCXX && "Record type mismatch");

              if (D->isCompleteDefinition() && !Importer.isMinimalImport())
                // FoundDef may not have every implicit method that D has
                // because implicit methods are created only if they are used.
                ImportImplicitMethods(DCXX, FoundCXX);
            }
            return FoundDef;
          }
        } else if (!D->isCompleteDefinition()) {
          // We have a forward declaration of this type, so adopt that forward
          // declaration rather than building a new one.

          // If one or both can be completed from external storage then try one
          // last time to complete and compare them before doing this.

          if (FoundRecord->hasExternalLexicalStorage() &&
              !FoundRecord->isCompleteDefinition())
            FoundRecord->getASTContext().getExternalSource()->CompleteType(FoundRecord);
          if (D->hasExternalLexicalStorage())
            D->getASTContext().getExternalSource()->CompleteType(D);

          if (FoundRecord->isCompleteDefinition() &&
              D->isCompleteDefinition() &&
              !IsStructuralMatch(D, FoundRecord))
            continue;

          AdoptDecl = FoundRecord;
          continue;
        } else if (!SearchName) {
          continue;
        }
      }

      ConflictingDecls.push_back(FoundDecl);
    }

    if (!ConflictingDecls.empty() && SearchName) {
      Name = Importer.HandleNameConflict(Name, DC, IDNS,
                                         ConflictingDecls.data(),
                                         ConflictingDecls.size());
    }
  }

  // Create the record declaration.
  RecordDecl *D2 = AdoptDecl;
  SourceLocation StartLoc = Importer.Import(D->getLocStart());
  if (!D2) {
    CXXRecordDecl *D2CXX = nullptr;
    if (auto *DCXX = dyn_cast<CXXRecordDecl>(D)) {
      if (DCXX->isLambda()) {
        TypeSourceInfo *TInfo = Importer.Import(DCXX->getLambdaTypeInfo());
        if (GetImportedOrCreateSpecialDecl(
                D2CXX, CXXRecordDecl::CreateLambda, D, Importer.getToContext(),
                DC, TInfo, Loc, DCXX->isDependentLambda(),
                DCXX->isGenericLambda(), DCXX->getLambdaCaptureDefault()))
          return D2CXX;
        Decl *CDecl = Importer.Import(DCXX->getLambdaContextDecl());
        if (DCXX->getLambdaContextDecl() && !CDecl)
          return nullptr;
        D2CXX->setLambdaMangling(DCXX->getLambdaManglingNumber(), CDecl);
      } else if (DCXX->isInjectedClassName()) {
        // We have to be careful to do a similar dance to the one in
        // Sema::ActOnStartCXXMemberDeclarations
        CXXRecordDecl *const PrevDecl = nullptr;
        const bool DelayTypeCreation = true;
        if (GetImportedOrCreateDecl(D2CXX, D, Importer.getToContext(),
                                    D->getTagKind(), DC, StartLoc, Loc,
                                    Name.getAsIdentifierInfo(), PrevDecl,
                                    DelayTypeCreation))
          return D2CXX;
        Importer.getToContext().getTypeDeclType(
            D2CXX, dyn_cast<CXXRecordDecl>(DC));
      } else {
        if (GetImportedOrCreateDecl(D2CXX, D, Importer.getToContext(),
                                    D->getTagKind(), DC, StartLoc, Loc,
                                    Name.getAsIdentifierInfo(),
                                    cast_or_null<CXXRecordDecl>(PrevDecl)))
          return D2CXX;
      }

////ASUTTON ADDN:
//      Expr *FromGenerator = DCXX->getGenerator();
//      Expr *ToGenerator = Importer.Import(FromGenerator);
//      if (!ToGenerator && FromGenerator)
//        return nullptr;
//      D2CXX->setGenerator(ToGenerator);
////END

      D2 = D2CXX;
      D2->setAccess(D->getAccess());
      D2->setLexicalDeclContext(LexicalDC);
      if (!DCXX->getDescribedClassTemplate() || DCXX->isImplicit())
        LexicalDC->addDeclInternal(D2);

      if (ClassTemplateDecl *FromDescribed =
          DCXX->getDescribedClassTemplate()) {
        auto *ToDescribed = cast_or_null<ClassTemplateDecl>(
            Importer.Import(FromDescribed));
        if (!ToDescribed)
          return nullptr;
        D2CXX->setDescribedClassTemplate(ToDescribed);
        if (!DCXX->isInjectedClassName()) {
          // In a record describing a template the type should be an
          // InjectedClassNameType (see Sema::CheckClassTemplate). Update the
          // previously set type to the correct value here (ToDescribed is not
          // available at record create).
          // FIXME: The previous type is cleared but not removed from
          // ASTContext's internal storage.
          CXXRecordDecl *Injected = nullptr;
          for (NamedDecl *Found : D2CXX->noload_lookup(Name)) {
            auto *Record = dyn_cast<CXXRecordDecl>(Found);
            if (Record && Record->isInjectedClassName()) {
              Injected = Record;
              break;
            }
          }
          D2CXX->setTypeForDecl(nullptr);
          Importer.getToContext().getInjectedClassNameType(D2CXX,
              ToDescribed->getInjectedClassNameSpecialization());
          if (Injected) {
            Injected->setTypeForDecl(nullptr);
            Importer.getToContext().getTypeDeclType(Injected, D2CXX);
          }
        }
      } else if (MemberSpecializationInfo *MemberInfo =
                   DCXX->getMemberSpecializationInfo()) {
        TemplateSpecializationKind SK =
            MemberInfo->getTemplateSpecializationKind();
        CXXRecordDecl *FromInst = DCXX->getInstantiatedFromMemberClass();
        auto *ToInst =
            cast_or_null<CXXRecordDecl>(Importer.Import(FromInst));
        if (FromInst && !ToInst)
          return nullptr;
        D2CXX->setInstantiationOfMemberClass(ToInst, SK);
        D2CXX->getMemberSpecializationInfo()->setPointOfInstantiation(
              Importer.Import(MemberInfo->getPointOfInstantiation()));
      }
    } else {
      if (GetImportedOrCreateDecl(D2, D, Importer.getToContext(),
                                  D->getTagKind(), DC, StartLoc, Loc,
                                  Name.getAsIdentifierInfo(), PrevDecl))
        return D2;
      D2->setLexicalDeclContext(LexicalDC);
      LexicalDC->addDeclInternal(D2);
    }

    D2->setQualifierInfo(Importer.Import(D->getQualifierLoc()));
    if (D->isAnonymousStructOrUnion())
      D2->setAnonymousStructOrUnion(true);
  }

  Importer.MapImported(D, D2);

  if (D->isCompleteDefinition() && ImportDefinition(D, D2, IDK_Default))
    return nullptr;

  return D2;
}

Decl *ASTNodeImporter::VisitEnumConstantDecl(EnumConstantDecl *D) {
  // Import the major distinguishing characteristics of this enumerator.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  QualType T = Importer.Import(D->getType());
  if (T.isNull())
    return nullptr;

  // Determine whether there are any other declarations with the same name and
  // in the same context.
  if (!LexicalDC->isFunctionOrMethod()) {
    SmallVector<NamedDecl *, 4> ConflictingDecls;
    unsigned IDNS = Decl::IDNS_Ordinary;
    SmallVector<NamedDecl *, 2> FoundDecls;
    DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
    for (auto *FoundDecl : FoundDecls) {
      if (!FoundDecl->isInIdentifierNamespace(IDNS))
        continue;

      if (auto *FoundEnumConstant = dyn_cast<EnumConstantDecl>(FoundDecl)) {
        if (IsStructuralMatch(D, FoundEnumConstant))
          return Importer.MapImported(D, FoundEnumConstant);
      }

      ConflictingDecls.push_back(FoundDecl);
    }

    if (!ConflictingDecls.empty()) {
      Name = Importer.HandleNameConflict(Name, DC, IDNS,
                                         ConflictingDecls.data(),
                                         ConflictingDecls.size());
      if (!Name)
        return nullptr;
    }
  }

  Expr *Init = Importer.Import(D->getInitExpr());
  if (D->getInitExpr() && !Init)
    return nullptr;

  EnumConstantDecl *ToEnumerator;
  if (GetImportedOrCreateDecl(
          ToEnumerator, D, Importer.getToContext(), cast<EnumDecl>(DC), Loc,
          Name.getAsIdentifierInfo(), T, Init, D->getInitVal()))
    return ToEnumerator;

  ToEnumerator->setAccess(D->getAccess());
  ToEnumerator->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToEnumerator);
  return ToEnumerator;
}

bool ASTNodeImporter::ImportTemplateInformation(FunctionDecl *FromFD,
                                                FunctionDecl *ToFD) {
  switch (FromFD->getTemplatedKind()) {
  case FunctionDecl::TK_NonTemplate:
  case FunctionDecl::TK_FunctionTemplate:
    return false;

  case FunctionDecl::TK_MemberSpecialization: {
    auto *InstFD = cast_or_null<FunctionDecl>(
          Importer.Import(FromFD->getInstantiatedFromMemberFunction()));
    if (!InstFD)
      return true;

    TemplateSpecializationKind TSK = FromFD->getTemplateSpecializationKind();
    SourceLocation POI = Importer.Import(
          FromFD->getMemberSpecializationInfo()->getPointOfInstantiation());
    ToFD->setInstantiationOfMemberFunction(InstFD, TSK);
    ToFD->getMemberSpecializationInfo()->setPointOfInstantiation(POI);
    return false;
  }

  case FunctionDecl::TK_FunctionTemplateSpecialization: {
    FunctionTemplateDecl* Template;
    OptionalTemplateArgsTy ToTemplArgs;
    std::tie(Template, ToTemplArgs) =
        ImportFunctionTemplateWithTemplateArgsFromSpecialization(FromFD);
    if (!Template || !ToTemplArgs)
      return true;

    TemplateArgumentList *ToTAList = TemplateArgumentList::CreateCopy(
          Importer.getToContext(), *ToTemplArgs);

    auto *FTSInfo = FromFD->getTemplateSpecializationInfo();
    TemplateArgumentListInfo ToTAInfo;
    const auto *FromTAArgsAsWritten = FTSInfo->TemplateArgumentsAsWritten;
    if (FromTAArgsAsWritten)
      if (ImportTemplateArgumentListInfo(*FromTAArgsAsWritten, ToTAInfo))
        return true;

    SourceLocation POI = Importer.Import(FTSInfo->getPointOfInstantiation());

    TemplateSpecializationKind TSK = FTSInfo->getTemplateSpecializationKind();
    ToFD->setFunctionTemplateSpecialization(
        Template, ToTAList, /* InsertPos= */ nullptr,
        TSK, FromTAArgsAsWritten ? &ToTAInfo : nullptr, POI);
    return false;
  }

  case FunctionDecl::TK_DependentFunctionTemplateSpecialization: {
    auto *FromInfo = FromFD->getDependentSpecializationInfo();
    UnresolvedSet<8> TemplDecls;
    unsigned NumTemplates = FromInfo->getNumTemplates();
    for (unsigned I = 0; I < NumTemplates; I++) {
      if (auto *ToFTD = cast_or_null<FunctionTemplateDecl>(
              Importer.Import(FromInfo->getTemplate(I))))
        TemplDecls.addDecl(ToFTD);
      else
        return true;
    }

    // Import TemplateArgumentListInfo.
    TemplateArgumentListInfo ToTAInfo;
    if (ImportTemplateArgumentListInfo(
            FromInfo->getLAngleLoc(), FromInfo->getRAngleLoc(),
            llvm::makeArrayRef(FromInfo->getTemplateArgs(),
                               FromInfo->getNumTemplateArgs()),
            ToTAInfo))
      return true;

    ToFD->setDependentTemplateSpecialization(Importer.getToContext(),
                                             TemplDecls, ToTAInfo);
    return false;
  }
  }
  llvm_unreachable("All cases should be covered!");
}

FunctionDecl *
ASTNodeImporter::FindFunctionTemplateSpecialization(FunctionDecl *FromFD) {
  FunctionTemplateDecl* Template;
  OptionalTemplateArgsTy ToTemplArgs;
  std::tie(Template, ToTemplArgs) =
      ImportFunctionTemplateWithTemplateArgsFromSpecialization(FromFD);
  if (!Template || !ToTemplArgs)
    return nullptr;

  void *InsertPos = nullptr;
  auto *FoundSpec = Template->findSpecialization(*ToTemplArgs, InsertPos);
  return FoundSpec;
}

Decl *ASTNodeImporter::VisitFunctionDecl(FunctionDecl *D) {

  SmallVector<Decl*, 2> Redecls = getCanonicalForwardRedeclChain(D);
  auto RedeclIt = Redecls.begin();
  // Import the first part of the decl chain. I.e. import all previous
  // declarations starting from the canonical decl.
  for (; RedeclIt != Redecls.end() && *RedeclIt != D; ++RedeclIt)
    if (!Importer.Import(*RedeclIt))
      return nullptr;
  assert(*RedeclIt == D);

  // Import the major distinguishing characteristics of this function.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  const FunctionDecl *FoundByLookup = nullptr;
  FunctionTemplateDecl *FromFT = D->getDescribedFunctionTemplate();

  // If this is a function template specialization, then try to find the same
  // existing specialization in the "to" context. The localUncachedLookup
  // below will not find any specialization, but would find the primary
  // template; thus, we have to skip normal lookup in case of specializations.
  // FIXME handle member function templates (TK_MemberSpecialization) similarly?
  if (D->getTemplatedKind() ==
      FunctionDecl::TK_FunctionTemplateSpecialization) {
    if (FunctionDecl *FoundFunction = FindFunctionTemplateSpecialization(D)) {
      if (D->doesThisDeclarationHaveABody() &&
          FoundFunction->hasBody())
        return Importer.Imported(D, FoundFunction);
      FoundByLookup = FoundFunction;
    }
  }
  // Try to find a function in our own ("to") context with the same name, same
  // type, and in the same context as the function we're importing.
  else if (!LexicalDC->isFunctionOrMethod()) {
    SmallVector<NamedDecl *, 4> ConflictingDecls;
    unsigned IDNS = Decl::IDNS_Ordinary | Decl::IDNS_OrdinaryFriend;
    SmallVector<NamedDecl *, 2> FoundDecls;
    DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
    for (auto *FoundDecl : FoundDecls) {
      if (!FoundDecl->isInIdentifierNamespace(IDNS))
        continue;

      // If template was found, look at the templated function.
      if (FromFT) {
        if (auto *Template = dyn_cast<FunctionTemplateDecl>(FoundDecl))
          FoundDecl = Template->getTemplatedDecl();
        else
          continue;
      }

      if (auto *FoundFunction = dyn_cast<FunctionDecl>(FoundDecl)) {
        if (FoundFunction->hasExternalFormalLinkage() &&
            D->hasExternalFormalLinkage()) {
          if (IsStructuralMatch(D, FoundFunction)) {
            const FunctionDecl *Definition = nullptr;
            if (D->doesThisDeclarationHaveABody() &&
                FoundFunction->hasBody(Definition)) {
              return Importer.MapImported(
                  D, const_cast<FunctionDecl *>(Definition));
            }
            FoundByLookup = FoundFunction;
            break;
          }

          // FIXME: Check for overloading more carefully, e.g., by boosting
          // Sema::IsOverload out to the AST library.

          // Function overloading is okay in C++.
          if (Importer.getToContext().getLangOpts().CPlusPlus)
            continue;

          // Complain about inconsistent function types.
          Importer.ToDiag(Loc, diag::err_odr_function_type_inconsistent)
            << Name << D->getType() << FoundFunction->getType();
          Importer.ToDiag(FoundFunction->getLocation(),
                          diag::note_odr_value_here)
            << FoundFunction->getType();
        }
      }

      ConflictingDecls.push_back(FoundDecl);
    }

    if (!ConflictingDecls.empty()) {
      Name = Importer.HandleNameConflict(Name, DC, IDNS,
                                         ConflictingDecls.data(),
                                         ConflictingDecls.size());
      if (!Name)
        return nullptr;
    }
  }

  DeclarationNameInfo NameInfo(Name, Loc);
  // Import additional name location/type info.
  ImportDeclarationNameLoc(D->getNameInfo(), NameInfo);

  QualType FromTy = D->getType();
  bool usedDifferentExceptionSpec = false;

  if (const auto *FromFPT = D->getType()->getAs<FunctionProtoType>()) {
    FunctionProtoType::ExtProtoInfo FromEPI = FromFPT->getExtProtoInfo();
    // FunctionProtoType::ExtProtoInfo's ExceptionSpecDecl can point to the
    // FunctionDecl that we are importing the FunctionProtoType for.
    // To avoid an infinite recursion when importing, create the FunctionDecl
    // with a simplified function type and update it afterwards.
    if (FromEPI.ExceptionSpec.SourceDecl ||
        FromEPI.ExceptionSpec.SourceTemplate ||
        FromEPI.ExceptionSpec.NoexceptExpr) {
      FunctionProtoType::ExtProtoInfo DefaultEPI;
      FromTy = Importer.getFromContext().getFunctionType(
          FromFPT->getReturnType(), FromFPT->getParamTypes(), DefaultEPI);
      usedDifferentExceptionSpec = true;
    }
  }

  // Import the type.
  QualType T = Importer.Import(FromTy);
  if (T.isNull())
    return nullptr;

  // Import the function parameters.
  SmallVector<ParmVarDecl *, 8> Parameters;
  for (auto P : D->parameters()) {
    auto *ToP = cast_or_null<ParmVarDecl>(Importer.Import(P));
    if (!ToP)
      return nullptr;

    Parameters.push_back(ToP);
  }

  TypeSourceInfo *TInfo = Importer.Import(D->getTypeSourceInfo());
  if (D->getTypeSourceInfo() && !TInfo)
    return nullptr;

  // Create the imported function.
  FunctionDecl *ToFunction = nullptr;
  SourceLocation InnerLocStart = Importer.Import(D->getInnerLocStart());
  if (auto *FromConstructor = dyn_cast<CXXConstructorDecl>(D)) {
    if (GetImportedOrCreateDecl<CXXConstructorDecl>(
            ToFunction, D, Importer.getToContext(), cast<CXXRecordDecl>(DC),
            InnerLocStart, NameInfo, T, TInfo, FromConstructor->isExplicit(),
            D->isInlineSpecified(), D->isImplicit(), D->isConstexpr()))
      return ToFunction;
    if (unsigned NumInitializers = FromConstructor->getNumCtorInitializers()) {
      SmallVector<CXXCtorInitializer *, 4> CtorInitializers;
      for (auto *I : FromConstructor->inits()) {
        auto *ToI = cast_or_null<CXXCtorInitializer>(Importer.Import(I));
        if (!ToI && I)
          return nullptr;
        CtorInitializers.push_back(ToI);
      }
      auto **Memory =
          new (Importer.getToContext()) CXXCtorInitializer *[NumInitializers];
      std::copy(CtorInitializers.begin(), CtorInitializers.end(), Memory);
      auto *ToCtor = cast<CXXConstructorDecl>(ToFunction);
      ToCtor->setCtorInitializers(Memory);
      ToCtor->setNumCtorInitializers(NumInitializers);
    }
  } else if (isa<CXXDestructorDecl>(D)) {
    if (GetImportedOrCreateDecl<CXXDestructorDecl>(
            ToFunction, D, Importer.getToContext(), cast<CXXRecordDecl>(DC),
            InnerLocStart, NameInfo, T, TInfo, D->isInlineSpecified(),
            D->isImplicit()))
      return ToFunction;
  } else if (CXXConversionDecl *FromConversion =
                 dyn_cast<CXXConversionDecl>(D)) {
    if (GetImportedOrCreateDecl<CXXConversionDecl>(
            ToFunction, D, Importer.getToContext(), cast<CXXRecordDecl>(DC),
            InnerLocStart, NameInfo, T, TInfo, D->isInlineSpecified(),
            FromConversion->isExplicit(), D->isConstexpr(), SourceLocation()))
      return ToFunction;
  } else if (auto *Method = dyn_cast<CXXMethodDecl>(D)) {
    if (GetImportedOrCreateDecl<CXXMethodDecl>(
            ToFunction, D, Importer.getToContext(), cast<CXXRecordDecl>(DC),
            InnerLocStart, NameInfo, T, TInfo, Method->getStorageClass(),
            Method->isInlineSpecified(), D->isConstexpr(), SourceLocation()))
      return ToFunction;
  } else {
    if (GetImportedOrCreateDecl(ToFunction, D, Importer.getToContext(), DC,
                                InnerLocStart, NameInfo, T, TInfo,
                                D->getStorageClass(), D->isInlineSpecified(),
                                D->hasWrittenPrototype(), D->isConstexpr()))
      return ToFunction;
  }

  // Import the qualifier, if any.
  ToFunction->setQualifierInfo(Importer.Import(D->getQualifierLoc()));
  ToFunction->setAccess(D->getAccess());
  ToFunction->setLexicalDeclContext(LexicalDC);
  ToFunction->setVirtualAsWritten(D->isVirtualAsWritten());
  ToFunction->setTrivial(D->isTrivial());
  ToFunction->setPure(D->isPure());
  ToFunction->setRangeEnd(Importer.Import(D->getLocEnd()));

  // Set the parameters.
  for (auto *Param : Parameters) {
    Param->setOwningFunction(ToFunction);
    ToFunction->addDeclInternal(Param);
  }
  ToFunction->setParams(Parameters);

  if (FoundByLookup) {
    auto *Recent = const_cast<FunctionDecl *>(
          FoundByLookup->getMostRecentDecl());
    ToFunction->setPreviousDecl(Recent);
  }

  // We need to complete creation of FunctionProtoTypeLoc manually with setting
  // params it refers to.
  if (TInfo) {
    if (auto ProtoLoc =
        TInfo->getTypeLoc().IgnoreParens().getAs<FunctionProtoTypeLoc>()) {
      for (unsigned I = 0, N = Parameters.size(); I != N; ++I)
        ProtoLoc.setParam(I, Parameters[I]);
    }
  }

  if (usedDifferentExceptionSpec) {
    // Update FunctionProtoType::ExtProtoInfo.
    QualType T = Importer.Import(D->getType());
    if (T.isNull())
      return nullptr;
    ToFunction->setType(T);
  }

  // Import the describing template function, if any.
  if (FromFT)
    if (!Importer.Import(FromFT))
      return nullptr;

  if (D->doesThisDeclarationHaveABody()) {
    if (Stmt *FromBody = D->getBody()) {
      if (Stmt *ToBody = Importer.Import(FromBody)) {
        ToFunction->setBody(ToBody);
      }
    }
  }

  // FIXME: Other bits to merge?

  // If it is a template, import all related things.
  if (ImportTemplateInformation(D, ToFunction))
    return nullptr;

  bool IsFriend = D->isInIdentifierNamespace(Decl::IDNS_OrdinaryFriend);

  // TODO Can we generalize this approach to other AST nodes as well?
  if (D->getDeclContext()->containsDeclAndLoad(D))
    DC->addDeclInternal(ToFunction);
  if (DC != LexicalDC && D->getLexicalDeclContext()->containsDeclAndLoad(D))
    LexicalDC->addDeclInternal(ToFunction);

  // Friend declaration's lexical context is the befriending class, but the
  // semantic context is the enclosing scope of the befriending class.
  // We want the friend functions to be found in the semantic context by lookup.
  // FIXME should we handle this generically in VisitFriendDecl?
  // In Other cases when LexicalDC != DC we don't want it to be added,
  // e.g out-of-class definitions like void B::f() {} .
  if (LexicalDC != DC && IsFriend) {
    DC->makeDeclVisibleInContext(ToFunction);
  }

  // Import the rest of the chain. I.e. import all subsequent declarations.
  for (++RedeclIt; RedeclIt != Redecls.end(); ++RedeclIt)
    if (!Importer.Import(*RedeclIt))
      return nullptr;

  if (auto *FromCXXMethod = dyn_cast<CXXMethodDecl>(D))
    ImportOverrides(cast<CXXMethodDecl>(ToFunction), FromCXXMethod);

  return ToFunction;
}

Decl *ASTNodeImporter::VisitCXXMethodDecl(CXXMethodDecl *D) {
  return VisitFunctionDecl(D);
}

Decl *ASTNodeImporter::VisitCXXConstructorDecl(CXXConstructorDecl *D) {
  return VisitCXXMethodDecl(D);
}

Decl *ASTNodeImporter::VisitCXXDestructorDecl(CXXDestructorDecl *D) {
  return VisitCXXMethodDecl(D);
}

Decl *ASTNodeImporter::VisitCXXConversionDecl(CXXConversionDecl *D) {
  return VisitCXXMethodDecl(D);
}

static unsigned getFieldIndex(Decl *F) {
  auto *Owner = dyn_cast<RecordDecl>(F->getDeclContext());
  if (!Owner)
    return 0;

  unsigned Index = 1;
  for (const auto *D : Owner->noload_decls()) {
    if (D == F)
      return Index;

    if (isa<FieldDecl>(*D) || isa<IndirectFieldDecl>(*D))
      ++Index;
  }

  return Index;
}

Decl *ASTNodeImporter::VisitFieldDecl(FieldDecl *D) {
  // Import the major distinguishing characteristics of a variable.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  // Determine whether we've already imported this field.
  SmallVector<NamedDecl *, 2> FoundDecls;
  DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
  for (auto *FoundDecl : FoundDecls) {
    if (auto *FoundField = dyn_cast<FieldDecl>(FoundDecl)) {
      // For anonymous fields, match up by index.
      if (!Name && getFieldIndex(D) != getFieldIndex(FoundField))
        continue;

      if (Importer.IsStructurallyEquivalent(D->getType(),
                                            FoundField->getType())) {
        Importer.MapImported(D, FoundField);
        return FoundField;
      }

      Importer.ToDiag(Loc, diag::err_odr_field_type_inconsistent)
        << Name << D->getType() << FoundField->getType();
      Importer.ToDiag(FoundField->getLocation(), diag::note_odr_value_here)
        << FoundField->getType();
      return nullptr;
    }
  }

  // Import the type.
  QualType T = Importer.Import(D->getType());
  if (T.isNull())
    return nullptr;

  TypeSourceInfo *TInfo = Importer.Import(D->getTypeSourceInfo());
  Expr *BitWidth = Importer.Import(D->getBitWidth());
  if (!BitWidth && D->getBitWidth())
    return nullptr;

  FieldDecl *ToField;
  if (GetImportedOrCreateDecl(ToField, D, Importer.getToContext(), DC,
                              Importer.Import(D->getInnerLocStart()), Loc,
                              Name.getAsIdentifierInfo(), T, TInfo, BitWidth,
                              D->isMutable(), D->getInClassInitStyle()))
    return ToField;

  ToField->setAccess(D->getAccess());
  ToField->setLexicalDeclContext(LexicalDC);
  if (Expr *FromInitializer = D->getInClassInitializer()) {
    Expr *ToInitializer = Importer.Import(FromInitializer);
    if (ToInitializer)
      ToField->setInClassInitializer(ToInitializer);
    else
      return nullptr;
  }
  ToField->setImplicit(D->isImplicit());
  LexicalDC->addDeclInternal(ToField);
  return ToField;
}

Decl *ASTNodeImporter::VisitIndirectFieldDecl(IndirectFieldDecl *D) {
  // Import the major distinguishing characteristics of a variable.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  // Determine whether we've already imported this field.
  SmallVector<NamedDecl *, 2> FoundDecls;
  DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
  for (unsigned I = 0, N = FoundDecls.size(); I != N; ++I) {
    if (auto *FoundField = dyn_cast<IndirectFieldDecl>(FoundDecls[I])) {
      // For anonymous indirect fields, match up by index.
      if (!Name && getFieldIndex(D) != getFieldIndex(FoundField))
        continue;

      if (Importer.IsStructurallyEquivalent(D->getType(),
                                            FoundField->getType(),
                                            !Name.isEmpty())) {
        Importer.MapImported(D, FoundField);
        return FoundField;
      }

      // If there are more anonymous fields to check, continue.
      if (!Name && I < N-1)
        continue;

      Importer.ToDiag(Loc, diag::err_odr_field_type_inconsistent)
        << Name << D->getType() << FoundField->getType();
      Importer.ToDiag(FoundField->getLocation(), diag::note_odr_value_here)
        << FoundField->getType();
      return nullptr;
    }
  }

  // Import the type.
  QualType T = Importer.Import(D->getType());
  if (T.isNull())
    return nullptr;

  auto **NamedChain =
    new (Importer.getToContext()) NamedDecl*[D->getChainingSize()];

  unsigned i = 0;
  for (auto *PI : D->chain()) {
    Decl *D = Importer.Import(PI);
    if (!D)
      return nullptr;
    NamedChain[i++] = cast<NamedDecl>(D);
  }

  llvm::MutableArrayRef<NamedDecl *> CH = {NamedChain, D->getChainingSize()};
  IndirectFieldDecl *ToIndirectField;
  if (GetImportedOrCreateDecl(ToIndirectField, D, Importer.getToContext(), DC,
                              Loc, Name.getAsIdentifierInfo(), T, CH))
    // FIXME here we leak `NamedChain` which is allocated before
    return ToIndirectField;

  for (const auto *A : D->attrs())
    ToIndirectField->addAttr(Importer.Import(A));

  ToIndirectField->setAccess(D->getAccess());
  ToIndirectField->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToIndirectField);
  return ToIndirectField;
}

Decl *ASTNodeImporter::VisitFriendDecl(FriendDecl *D) {
  // Import the major distinguishing characteristics of a declaration.
  DeclContext *DC = Importer.ImportContext(D->getDeclContext());
  DeclContext *LexicalDC = D->getDeclContext() == D->getLexicalDeclContext()
      ? DC : Importer.ImportContext(D->getLexicalDeclContext());
  if (!DC || !LexicalDC)
    return nullptr;

  // Determine whether we've already imported this decl.
  // FriendDecl is not a NamedDecl so we cannot use localUncachedLookup.
  auto *RD = cast<CXXRecordDecl>(DC);
  FriendDecl *ImportedFriend = RD->getFirstFriend();

  while (ImportedFriend) {
    if (D->getFriendDecl() && ImportedFriend->getFriendDecl()) {
      if (IsStructuralMatch(D->getFriendDecl(), ImportedFriend->getFriendDecl(),
                            /*Complain=*/false))
        return Importer.MapImported(D, ImportedFriend);

    } else if (D->getFriendType() && ImportedFriend->getFriendType()) {
      if (Importer.IsStructurallyEquivalent(
            D->getFriendType()->getType(),
            ImportedFriend->getFriendType()->getType(), true))
        return Importer.MapImported(D, ImportedFriend);
    }
    ImportedFriend = ImportedFriend->getNextFriend();
  }

  // Not found. Create it.
  FriendDecl::FriendUnion ToFU;
  if (NamedDecl *FriendD = D->getFriendDecl()) {
    auto *ToFriendD = cast_or_null<NamedDecl>(Importer.Import(FriendD));
    if (ToFriendD && FriendD->getFriendObjectKind() != Decl::FOK_None &&
        !(FriendD->isInIdentifierNamespace(Decl::IDNS_NonMemberOperator)))
      ToFriendD->setObjectOfFriendDecl(false);

    ToFU = ToFriendD;
  }  else // The friend is a type, not a decl.
    ToFU = Importer.Import(D->getFriendType());
  if (!ToFU)
    return nullptr;

  SmallVector<TemplateParameterList *, 1> ToTPLists(D->NumTPLists);
  auto **FromTPLists = D->getTrailingObjects<TemplateParameterList *>();
  for (unsigned I = 0; I < D->NumTPLists; I++) {
    TemplateParameterList *List = ImportTemplateParameterList(FromTPLists[I]);
    if (!List)
      return nullptr;
    ToTPLists[I] = List;
  }

  FriendDecl *FrD;
  if (GetImportedOrCreateDecl(FrD, D, Importer.getToContext(), DC,
                              Importer.Import(D->getLocation()), ToFU,
                              Importer.Import(D->getFriendLoc()), ToTPLists))
    return FrD;

  FrD->setAccess(D->getAccess());
  FrD->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(FrD);
  return FrD;
}

Decl *ASTNodeImporter::VisitObjCIvarDecl(ObjCIvarDecl *D) {
  // Import the major distinguishing characteristics of an ivar.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  // Determine whether we've already imported this ivar
  SmallVector<NamedDecl *, 2> FoundDecls;
  DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
  for (auto *FoundDecl : FoundDecls) {
    if (auto *FoundIvar = dyn_cast<ObjCIvarDecl>(FoundDecl)) {
      if (Importer.IsStructurallyEquivalent(D->getType(),
                                            FoundIvar->getType())) {
        Importer.MapImported(D, FoundIvar);
        return FoundIvar;
      }

      Importer.ToDiag(Loc, diag::err_odr_ivar_type_inconsistent)
        << Name << D->getType() << FoundIvar->getType();
      Importer.ToDiag(FoundIvar->getLocation(), diag::note_odr_value_here)
        << FoundIvar->getType();
      return nullptr;
    }
  }

  // Import the type.
  QualType T = Importer.Import(D->getType());
  if (T.isNull())
    return nullptr;

  TypeSourceInfo *TInfo = Importer.Import(D->getTypeSourceInfo());
  Expr *BitWidth = Importer.Import(D->getBitWidth());
  if (!BitWidth && D->getBitWidth())
    return nullptr;

  ObjCIvarDecl *ToIvar;
  if (GetImportedOrCreateDecl(
          ToIvar, D, Importer.getToContext(), cast<ObjCContainerDecl>(DC),
          Importer.Import(D->getInnerLocStart()), Loc,
          Name.getAsIdentifierInfo(), T, TInfo, D->getAccessControl(), BitWidth,
          D->getSynthesize()))
    return ToIvar;

  ToIvar->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToIvar);
  return ToIvar;
}

Decl *ASTNodeImporter::VisitVarDecl(VarDecl *D) {
  // Import the major distinguishing characteristics of a variable.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  // Try to find a variable in our own ("to") context with the same name and
  // in the same context as the variable we're importing.
  if (D->isFileVarDecl()) {
    VarDecl *MergeWithVar = nullptr;
    SmallVector<NamedDecl *, 4> ConflictingDecls;
    unsigned IDNS = Decl::IDNS_Ordinary;
    SmallVector<NamedDecl *, 2> FoundDecls;
    DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
    for (auto *FoundDecl : FoundDecls) {
      if (!FoundDecl->isInIdentifierNamespace(IDNS))
        continue;

      if (auto *FoundVar = dyn_cast<VarDecl>(FoundDecl)) {
        // We have found a variable that we may need to merge with. Check it.
        if (FoundVar->hasExternalFormalLinkage() &&
            D->hasExternalFormalLinkage()) {
          if (Importer.IsStructurallyEquivalent(D->getType(),
                                                FoundVar->getType())) {
            MergeWithVar = FoundVar;
            break;
          }

          const ArrayType *FoundArray
            = Importer.getToContext().getAsArrayType(FoundVar->getType());
          const ArrayType *TArray
            = Importer.getToContext().getAsArrayType(D->getType());
          if (FoundArray && TArray) {
            if (isa<IncompleteArrayType>(FoundArray) &&
                isa<ConstantArrayType>(TArray)) {
              // Import the type.
              QualType T = Importer.Import(D->getType());
              if (T.isNull())
                return nullptr;

              FoundVar->setType(T);
              MergeWithVar = FoundVar;
              break;
            } else if (isa<IncompleteArrayType>(TArray) &&
                       isa<ConstantArrayType>(FoundArray)) {
              MergeWithVar = FoundVar;
              break;
            }
          }

          Importer.ToDiag(Loc, diag::err_odr_variable_type_inconsistent)
            << Name << D->getType() << FoundVar->getType();
          Importer.ToDiag(FoundVar->getLocation(), diag::note_odr_value_here)
            << FoundVar->getType();
        }
      }

      ConflictingDecls.push_back(FoundDecl);
    }

    if (MergeWithVar) {
      // An equivalent variable with external linkage has been found. Link
      // the two declarations, then merge them.
      Importer.MapImported(D, MergeWithVar);
      updateFlags(D, MergeWithVar);

      if (VarDecl *DDef = D->getDefinition()) {
        if (VarDecl *ExistingDef = MergeWithVar->getDefinition()) {
          Importer.ToDiag(ExistingDef->getLocation(),
                          diag::err_odr_variable_multiple_def)
            << Name;
          Importer.FromDiag(DDef->getLocation(), diag::note_odr_defined_here);
        } else {
          Expr *Init = Importer.Import(DDef->getInit());
          MergeWithVar->setInit(Init);
          if (DDef->isInitKnownICE()) {
            EvaluatedStmt *Eval = MergeWithVar->ensureEvaluatedStmt();
            Eval->CheckedICE = true;
            Eval->IsICE = DDef->isInitICE();
          }
        }
      }

      return MergeWithVar;
    }

    if (!ConflictingDecls.empty()) {
      Name = Importer.HandleNameConflict(Name, DC, IDNS,
                                         ConflictingDecls.data(),
                                         ConflictingDecls.size());
      if (!Name)
        return nullptr;
    }
  }

  // Import the type.
  QualType T = Importer.Import(D->getType());
  if (T.isNull())
    return nullptr;

  // Create the imported variable.
  TypeSourceInfo *TInfo = Importer.Import(D->getTypeSourceInfo());
  VarDecl *ToVar;
  if (GetImportedOrCreateDecl(ToVar, D, Importer.getToContext(), DC,
                              Importer.Import(D->getInnerLocStart()), Loc,
                              Name.getAsIdentifierInfo(), T, TInfo,
                              D->getStorageClass()))
    return ToVar;

  ToVar->setQualifierInfo(Importer.Import(D->getQualifierLoc()));
  ToVar->setAccess(D->getAccess());
  ToVar->setLexicalDeclContext(LexicalDC);

  // Templated declarations should never appear in the enclosing DeclContext.
  if (!D->getDescribedVarTemplate())
    LexicalDC->addDeclInternal(ToVar);

  // Merge the initializer.
  if (ImportDefinition(D, ToVar))
    return nullptr;

  if (D->isConstexpr())
    ToVar->setConstexpr(true);

  return ToVar;
}

Decl *ASTNodeImporter::VisitImplicitParamDecl(ImplicitParamDecl *D) {
  // Parameters are created in the translation unit's context, then moved
  // into the function declaration's context afterward.
  DeclContext *DC = Importer.getToContext().getTranslationUnitDecl();

  // Import the name of this declaration.
  DeclarationName Name = Importer.Import(D->getDeclName());
  if (D->getDeclName() && !Name)
    return nullptr;

  // Import the location of this declaration.
  SourceLocation Loc = Importer.Import(D->getLocation());

  // Import the parameter's type.
  QualType T = Importer.Import(D->getType());
  if (T.isNull())
    return nullptr;

  // Create the imported parameter.
  ImplicitParamDecl *ToParm = nullptr;
  if (GetImportedOrCreateDecl(ToParm, D, Importer.getToContext(), DC, Loc,
                              Name.getAsIdentifierInfo(), T,
                              D->getParameterKind()))
    return ToParm;
  return ToParm;
}

Decl *ASTNodeImporter::VisitParmVarDecl(ParmVarDecl *D) {
  // Parameters are created in the translation unit's context, then moved
  // into the function declaration's context afterward.
  DeclContext *DC = Importer.getToContext().getTranslationUnitDecl();

  // Import the name of this declaration.
  DeclarationName Name = Importer.Import(D->getDeclName());
  if (D->getDeclName() && !Name)
    return nullptr;

  // Import the location of this declaration.
  SourceLocation Loc = Importer.Import(D->getLocation());

  // Import the parameter's type.
  QualType T = Importer.Import(D->getType());
  if (T.isNull())
    return nullptr;

  // Create the imported parameter.
  TypeSourceInfo *TInfo = Importer.Import(D->getTypeSourceInfo());
  ParmVarDecl *ToParm;
  if (GetImportedOrCreateDecl(ToParm, D, Importer.getToContext(), DC,
                              Importer.Import(D->getInnerLocStart()), Loc,
                              Name.getAsIdentifierInfo(), T, TInfo,
                              D->getStorageClass(),
                              /*DefaultArg*/ nullptr))
    return ToParm;

  // Set the default argument.
  ToParm->setHasInheritedDefaultArg(D->hasInheritedDefaultArg());
  ToParm->setKNRPromoted(D->isKNRPromoted());

  Expr *ToDefArg = nullptr;
  Expr *FromDefArg = nullptr;
  if (D->hasUninstantiatedDefaultArg()) {
    FromDefArg = D->getUninstantiatedDefaultArg();
    ToDefArg = Importer.Import(FromDefArg);
    ToParm->setUninstantiatedDefaultArg(ToDefArg);
  } else if (D->hasUnparsedDefaultArg()) {
    ToParm->setUnparsedDefaultArg();
  } else if (D->hasDefaultArg()) {
    FromDefArg = D->getDefaultArg();
    ToDefArg = Importer.Import(FromDefArg);
    ToParm->setDefaultArg(ToDefArg);
  }
  if (FromDefArg && !ToDefArg)
    return nullptr;

  if (D->isObjCMethodParameter()) {
    ToParm->setObjCMethodScopeInfo(D->getFunctionScopeIndex());
    ToParm->setObjCDeclQualifier(D->getObjCDeclQualifier());
  } else {
    ToParm->setScopeInfo(D->getFunctionScopeDepth(),
                         D->getFunctionScopeIndex());
  }

  return ToParm;
}

Decl *ASTNodeImporter::VisitObjCMethodDecl(ObjCMethodDecl *D) {
  // Import the major distinguishing characteristics of a method.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  SmallVector<NamedDecl *, 2> FoundDecls;
  DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
  for (auto *FoundDecl : FoundDecls) {
    if (auto *FoundMethod = dyn_cast<ObjCMethodDecl>(FoundDecl)) {
      if (FoundMethod->isInstanceMethod() != D->isInstanceMethod())
        continue;

      // Check return types.
      if (!Importer.IsStructurallyEquivalent(D->getReturnType(),
                                             FoundMethod->getReturnType())) {
        Importer.ToDiag(Loc, diag::err_odr_objc_method_result_type_inconsistent)
            << D->isInstanceMethod() << Name << D->getReturnType()
            << FoundMethod->getReturnType();
        Importer.ToDiag(FoundMethod->getLocation(),
                        diag::note_odr_objc_method_here)
          << D->isInstanceMethod() << Name;
        return nullptr;
      }

      // Check the number of parameters.
      if (D->param_size() != FoundMethod->param_size()) {
        Importer.ToDiag(Loc, diag::err_odr_objc_method_num_params_inconsistent)
          << D->isInstanceMethod() << Name
          << D->param_size() << FoundMethod->param_size();
        Importer.ToDiag(FoundMethod->getLocation(),
                        diag::note_odr_objc_method_here)
          << D->isInstanceMethod() << Name;
        return nullptr;
      }

      // Check parameter types.
      for (ObjCMethodDecl::param_iterator P = D->param_begin(),
             PEnd = D->param_end(), FoundP = FoundMethod->param_begin();
           P != PEnd; ++P, ++FoundP) {
        if (!Importer.IsStructurallyEquivalent((*P)->getType(),
                                               (*FoundP)->getType())) {
          Importer.FromDiag((*P)->getLocation(),
                            diag::err_odr_objc_method_param_type_inconsistent)
            << D->isInstanceMethod() << Name
            << (*P)->getType() << (*FoundP)->getType();
          Importer.ToDiag((*FoundP)->getLocation(), diag::note_odr_value_here)
            << (*FoundP)->getType();
          return nullptr;
        }
      }

      // Check variadic/non-variadic.
      // Check the number of parameters.
      if (D->isVariadic() != FoundMethod->isVariadic()) {
        Importer.ToDiag(Loc, diag::err_odr_objc_method_variadic_inconsistent)
          << D->isInstanceMethod() << Name;
        Importer.ToDiag(FoundMethod->getLocation(),
                        diag::note_odr_objc_method_here)
          << D->isInstanceMethod() << Name;
        return nullptr;
      }

      // FIXME: Any other bits we need to merge?
      return Importer.MapImported(D, FoundMethod);
    }
  }

  // Import the result type.
  QualType ResultTy = Importer.Import(D->getReturnType());
  if (ResultTy.isNull())
    return nullptr;

  TypeSourceInfo *ReturnTInfo = Importer.Import(D->getReturnTypeSourceInfo());

  ObjCMethodDecl *ToMethod;
  if (GetImportedOrCreateDecl(
          ToMethod, D, Importer.getToContext(), Loc,
          Importer.Import(D->getLocEnd()), Name.getObjCSelector(), ResultTy,
          ReturnTInfo, DC, D->isInstanceMethod(), D->isVariadic(),
          D->isPropertyAccessor(), D->isImplicit(), D->isDefined(),
          D->getImplementationControl(), D->hasRelatedResultType()))
    return ToMethod;

  // FIXME: When we decide to merge method definitions, we'll need to
  // deal with implicit parameters.

  // Import the parameters
  SmallVector<ParmVarDecl *, 5> ToParams;
  for (auto *FromP : D->parameters()) {
    auto *ToP = cast_or_null<ParmVarDecl>(Importer.Import(FromP));
    if (!ToP)
      return nullptr;

    ToParams.push_back(ToP);
  }

  // Set the parameters.
  for (auto *ToParam : ToParams) {
    ToParam->setOwningFunction(ToMethod);
    ToMethod->addDeclInternal(ToParam);
  }

  SmallVector<SourceLocation, 12> SelLocs;
  D->getSelectorLocs(SelLocs);
  for (auto &Loc : SelLocs)
    Loc = Importer.Import(Loc);

  ToMethod->setMethodParams(Importer.getToContext(), ToParams, SelLocs);

  ToMethod->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToMethod);
  return ToMethod;
}

Decl *ASTNodeImporter::VisitObjCTypeParamDecl(ObjCTypeParamDecl *D) {
  // Import the major distinguishing characteristics of a category.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  TypeSourceInfo *BoundInfo = Importer.Import(D->getTypeSourceInfo());
  if (!BoundInfo)
    return nullptr;

  ObjCTypeParamDecl *Result;
  if (GetImportedOrCreateDecl(
          Result, D, Importer.getToContext(), DC, D->getVariance(),
          Importer.Import(D->getVarianceLoc()), D->getIndex(),
          Importer.Import(D->getLocation()), Name.getAsIdentifierInfo(),
          Importer.Import(D->getColonLoc()), BoundInfo))
    return Result;

  Result->setLexicalDeclContext(LexicalDC);
  return Result;
}

Decl *ASTNodeImporter::VisitObjCCategoryDecl(ObjCCategoryDecl *D) {
  // Import the major distinguishing characteristics of a category.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  auto *ToInterface =
      cast_or_null<ObjCInterfaceDecl>(Importer.Import(D->getClassInterface()));
  if (!ToInterface)
    return nullptr;

  // Determine if we've already encountered this category.
  ObjCCategoryDecl *MergeWithCategory
    = ToInterface->FindCategoryDeclaration(Name.getAsIdentifierInfo());
  ObjCCategoryDecl *ToCategory = MergeWithCategory;
  if (!ToCategory) {

    if (GetImportedOrCreateDecl(ToCategory, D, Importer.getToContext(), DC,
                                Importer.Import(D->getAtStartLoc()), Loc,
                                Importer.Import(D->getCategoryNameLoc()),
                                Name.getAsIdentifierInfo(), ToInterface,
                                /*TypeParamList=*/nullptr,
                                Importer.Import(D->getIvarLBraceLoc()),
                                Importer.Import(D->getIvarRBraceLoc())))
      return ToCategory;

    ToCategory->setLexicalDeclContext(LexicalDC);
    LexicalDC->addDeclInternal(ToCategory);
    // Import the type parameter list after calling Imported, to avoid
    // loops when bringing in their DeclContext.
    ToCategory->setTypeParamList(ImportObjCTypeParamList(
                                   D->getTypeParamList()));

    // Import protocols
    SmallVector<ObjCProtocolDecl *, 4> Protocols;
    SmallVector<SourceLocation, 4> ProtocolLocs;
    ObjCCategoryDecl::protocol_loc_iterator FromProtoLoc
      = D->protocol_loc_begin();
    for (ObjCCategoryDecl::protocol_iterator FromProto = D->protocol_begin(),
                                          FromProtoEnd = D->protocol_end();
         FromProto != FromProtoEnd;
         ++FromProto, ++FromProtoLoc) {
      auto *ToProto =
          cast_or_null<ObjCProtocolDecl>(Importer.Import(*FromProto));
      if (!ToProto)
        return nullptr;
      Protocols.push_back(ToProto);
      ProtocolLocs.push_back(Importer.Import(*FromProtoLoc));
    }

    // FIXME: If we're merging, make sure that the protocol list is the same.
    ToCategory->setProtocolList(Protocols.data(), Protocols.size(),
                                ProtocolLocs.data(), Importer.getToContext());
  } else {
    Importer.MapImported(D, ToCategory);
  }

  // Import all of the members of this category.
  ImportDeclContext(D);

  // If we have an implementation, import it as well.
  if (D->getImplementation()) {
    auto *Impl =
        cast_or_null<ObjCCategoryImplDecl>(
                                       Importer.Import(D->getImplementation()));
    if (!Impl)
      return nullptr;

    ToCategory->setImplementation(Impl);
  }

  return ToCategory;
}

bool ASTNodeImporter::ImportDefinition(ObjCProtocolDecl *From,
                                       ObjCProtocolDecl *To,
                                       ImportDefinitionKind Kind) {
  if (To->getDefinition()) {
    if (shouldForceImportDeclContext(Kind))
      ImportDeclContext(From);
    return false;
  }

  // Start the protocol definition
  To->startDefinition();

  // Import protocols
  SmallVector<ObjCProtocolDecl *, 4> Protocols;
  SmallVector<SourceLocation, 4> ProtocolLocs;
  ObjCProtocolDecl::protocol_loc_iterator
  FromProtoLoc = From->protocol_loc_begin();
  for (ObjCProtocolDecl::protocol_iterator FromProto = From->protocol_begin(),
                                        FromProtoEnd = From->protocol_end();
       FromProto != FromProtoEnd;
       ++FromProto, ++FromProtoLoc) {
    auto *ToProto = cast_or_null<ObjCProtocolDecl>(Importer.Import(*FromProto));
    if (!ToProto)
      return true;
    Protocols.push_back(ToProto);
    ProtocolLocs.push_back(Importer.Import(*FromProtoLoc));
  }

  // FIXME: If we're merging, make sure that the protocol list is the same.
  To->setProtocolList(Protocols.data(), Protocols.size(),
                      ProtocolLocs.data(), Importer.getToContext());

  if (shouldForceImportDeclContext(Kind)) {
    // Import all of the members of this protocol.
    ImportDeclContext(From, /*ForceImport=*/true);
  }
  return false;
}

Decl *ASTNodeImporter::VisitObjCProtocolDecl(ObjCProtocolDecl *D) {
  // If this protocol has a definition in the translation unit we're coming
  // from, but this particular declaration is not that definition, import the
  // definition and map to that.
  ObjCProtocolDecl *Definition = D->getDefinition();
  if (Definition && Definition != D) {
    Decl *ImportedDef = Importer.Import(Definition);
    if (!ImportedDef)
      return nullptr;

    return Importer.MapImported(D, ImportedDef);
  }

  // Import the major distinguishing characteristics of a protocol.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  ObjCProtocolDecl *MergeWithProtocol = nullptr;
  SmallVector<NamedDecl *, 2> FoundDecls;
  DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
  for (auto *FoundDecl : FoundDecls) {
    if (!FoundDecl->isInIdentifierNamespace(Decl::IDNS_ObjCProtocol))
      continue;

    if ((MergeWithProtocol = dyn_cast<ObjCProtocolDecl>(FoundDecl)))
      break;
  }

  ObjCProtocolDecl *ToProto = MergeWithProtocol;
  if (!ToProto) {
    if (GetImportedOrCreateDecl(ToProto, D, Importer.getToContext(), DC,
                                Name.getAsIdentifierInfo(), Loc,
                                Importer.Import(D->getAtStartLoc()),
                                /*PrevDecl=*/nullptr))
      return ToProto;
    ToProto->setLexicalDeclContext(LexicalDC);
    LexicalDC->addDeclInternal(ToProto);
  }

  Importer.MapImported(D, ToProto);

  if (D->isThisDeclarationADefinition() && ImportDefinition(D, ToProto))
    return nullptr;

  return ToProto;
}

Decl *ASTNodeImporter::VisitLinkageSpecDecl(LinkageSpecDecl *D) {
  DeclContext *DC = Importer.ImportContext(D->getDeclContext());
  DeclContext *LexicalDC = Importer.ImportContext(D->getLexicalDeclContext());

  SourceLocation ExternLoc = Importer.Import(D->getExternLoc());
  SourceLocation LangLoc = Importer.Import(D->getLocation());

  bool HasBraces = D->hasBraces();

  LinkageSpecDecl *ToLinkageSpec;
  if (GetImportedOrCreateDecl(ToLinkageSpec, D, Importer.getToContext(), DC,
                              ExternLoc, LangLoc, D->getLanguage(), HasBraces))
    return ToLinkageSpec;

  if (HasBraces) {
    SourceLocation RBraceLoc = Importer.Import(D->getRBraceLoc());
    ToLinkageSpec->setRBraceLoc(RBraceLoc);
  }

  ToLinkageSpec->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToLinkageSpec);

  return ToLinkageSpec;
}

Decl *ASTNodeImporter::VisitUsingDecl(UsingDecl *D) {
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD = nullptr;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  DeclarationNameInfo NameInfo(Name,
                               Importer.Import(D->getNameInfo().getLoc()));
  ImportDeclarationNameLoc(D->getNameInfo(), NameInfo);

  UsingDecl *ToUsing;
  if (GetImportedOrCreateDecl(ToUsing, D, Importer.getToContext(), DC,
                              Importer.Import(D->getUsingLoc()),
                              Importer.Import(D->getQualifierLoc()), NameInfo,
                              D->hasTypename()))
    return ToUsing;

  ToUsing->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToUsing);

  if (NamedDecl *FromPattern =
      Importer.getFromContext().getInstantiatedFromUsingDecl(D)) {
    if (auto *ToPattern =
            dyn_cast_or_null<NamedDecl>(Importer.Import(FromPattern)))
      Importer.getToContext().setInstantiatedFromUsingDecl(ToUsing, ToPattern);
    else
      return nullptr;
  }

  for (auto *FromShadow : D->shadows()) {
    if (auto *ToShadow =
            dyn_cast_or_null<UsingShadowDecl>(Importer.Import(FromShadow)))
      ToUsing->addShadowDecl(ToShadow);
    else
      // FIXME: We return a nullptr here but the definition is already created
      // and available with lookups. How to fix this?..
      return nullptr;
  }
  return ToUsing;
}

Decl *ASTNodeImporter::VisitUsingShadowDecl(UsingShadowDecl *D) {
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD = nullptr;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  auto *ToUsing = dyn_cast_or_null<UsingDecl>(
      Importer.Import(D->getUsingDecl()));
  if (!ToUsing)
    return nullptr;

  auto *ToTarget = dyn_cast_or_null<NamedDecl>(
      Importer.Import(D->getTargetDecl()));
  if (!ToTarget)
    return nullptr;

  UsingShadowDecl *ToShadow;
  if (GetImportedOrCreateDecl(ToShadow, D, Importer.getToContext(), DC, Loc,
                              ToUsing, ToTarget))
    return ToShadow;

  ToShadow->setLexicalDeclContext(LexicalDC);
  ToShadow->setAccess(D->getAccess());

  if (UsingShadowDecl *FromPattern =
      Importer.getFromContext().getInstantiatedFromUsingShadowDecl(D)) {
    if (auto *ToPattern =
            dyn_cast_or_null<UsingShadowDecl>(Importer.Import(FromPattern)))
      Importer.getToContext().setInstantiatedFromUsingShadowDecl(ToShadow,
                                                                 ToPattern);
    else
      // FIXME: We return a nullptr here but the definition is already created
      // and available with lookups. How to fix this?..
      return nullptr;
  }

  LexicalDC->addDeclInternal(ToShadow);

  return ToShadow;
}

Decl *ASTNodeImporter::VisitUsingDirectiveDecl(UsingDirectiveDecl *D) {
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD = nullptr;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  DeclContext *ToComAncestor = Importer.ImportContext(D->getCommonAncestor());
  if (!ToComAncestor)
    return nullptr;

  auto *ToNominated = cast_or_null<NamespaceDecl>(
      Importer.Import(D->getNominatedNamespace()));
  if (!ToNominated)
    return nullptr;

  UsingDirectiveDecl *ToUsingDir;
  if (GetImportedOrCreateDecl(ToUsingDir, D, Importer.getToContext(), DC,
                              Importer.Import(D->getUsingLoc()),
                              Importer.Import(D->getNamespaceKeyLocation()),
                              Importer.Import(D->getQualifierLoc()),
                              Importer.Import(D->getIdentLocation()),
                              ToNominated, ToComAncestor))
    return ToUsingDir;

  ToUsingDir->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToUsingDir);

  return ToUsingDir;
}

Decl *ASTNodeImporter::VisitUnresolvedUsingValueDecl(
    UnresolvedUsingValueDecl *D) {
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD = nullptr;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  DeclarationNameInfo NameInfo(Name, Importer.Import(D->getNameInfo().getLoc()));
  ImportDeclarationNameLoc(D->getNameInfo(), NameInfo);

  UnresolvedUsingValueDecl *ToUsingValue;
  if (GetImportedOrCreateDecl(ToUsingValue, D, Importer.getToContext(), DC,
                              Importer.Import(D->getUsingLoc()),
                              Importer.Import(D->getQualifierLoc()), NameInfo,
                              Importer.Import(D->getEllipsisLoc())))
    return ToUsingValue;

  ToUsingValue->setAccess(D->getAccess());
  ToUsingValue->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToUsingValue);

  return ToUsingValue;
}

Decl *ASTNodeImporter::VisitUnresolvedUsingTypenameDecl(
    UnresolvedUsingTypenameDecl *D) {
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD = nullptr;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  UnresolvedUsingTypenameDecl *ToUsing;
  if (GetImportedOrCreateDecl(ToUsing, D, Importer.getToContext(), DC,
                              Importer.Import(D->getUsingLoc()),
                              Importer.Import(D->getTypenameLoc()),
                              Importer.Import(D->getQualifierLoc()), Loc, Name,
                              Importer.Import(D->getEllipsisLoc())))
    return ToUsing;

  ToUsing->setAccess(D->getAccess());
  ToUsing->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToUsing);

  return ToUsing;
}

//ASUTTON ADDN:
Decl *ASTNodeImporter::VisitConstexprDecl(ConstexprDecl *D) {
  // Import the context of this declaration.
  DeclContext *DC = Importer.ImportContext(D->getDeclContext());
  if (!DC)
    return nullptr;

  DeclContext *LexicalDC = DC;

  // Import the location of this declaration.
  SourceLocation ConstexprLoc = Importer.Import(D->getLocation());

  ConstexprDecl *ToD;

  // Import the underlying representation.
  if (D->hasFunctionRepresentation()) {
    FunctionDecl *ToFunctionRepresentation =
        cast_or_null<FunctionDecl>(Importer.Import(D->getFunctionDecl()));
    if (!ToFunctionRepresentation)
      return nullptr;
    ToD = ConstexprDecl::Create(Importer.getToContext(), DC, ConstexprLoc,
                                D->getcdcv(), //DWR ADDN
                                ToFunctionRepresentation);
  } else {
    CXXRecordDecl *ToLambdaRepresentation =
        cast_or_null<CXXRecordDecl>(Importer.Import(D->getClosureDecl()));
    if (!ToLambdaRepresentation)
      return nullptr;
    ToD = ConstexprDecl::Create(Importer.getToContext(), DC, ConstexprLoc,
                                D->getcdcv(), //DWR ADDN
                                ToLambdaRepresentation);
  }

  // Import the call expression that evaluates this declaration, if set.
  CallExpr *FromCall = D->getCallExpr();
  CallExpr *ToCall = cast_or_null<CallExpr>(Importer.Import(FromCall));
  if (!ToCall && FromCall)
    return nullptr;
  ToD->setCallExpr(ToCall);

  ToD->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToD);

  Importer.Imported(D, ToD);

  return ToD;
}
//END

bool ASTNodeImporter::ImportDefinition(ObjCInterfaceDecl *From,
                                       ObjCInterfaceDecl *To,
                                       ImportDefinitionKind Kind) {
  if (To->getDefinition()) {
    // Check consistency of superclass.
    ObjCInterfaceDecl *FromSuper = From->getSuperClass();
    if (FromSuper) {
      FromSuper = cast_or_null<ObjCInterfaceDecl>(Importer.Import(FromSuper));
      if (!FromSuper)
        return true;
    }

    ObjCInterfaceDecl *ToSuper = To->getSuperClass();
    if ((bool)FromSuper != (bool)ToSuper ||
        (FromSuper && !declaresSameEntity(FromSuper, ToSuper))) {
      Importer.ToDiag(To->getLocation(),
                      diag::err_odr_objc_superclass_inconsistent)
        << To->getDeclName();
      if (ToSuper)
        Importer.ToDiag(To->getSuperClassLoc(), diag::note_odr_objc_superclass)
          << To->getSuperClass()->getDeclName();
      else
        Importer.ToDiag(To->getLocation(),
                        diag::note_odr_objc_missing_superclass);
      if (From->getSuperClass())
        Importer.FromDiag(From->getSuperClassLoc(),
                          diag::note_odr_objc_superclass)
        << From->getSuperClass()->getDeclName();
      else
        Importer.FromDiag(From->getLocation(),
                          diag::note_odr_objc_missing_superclass);
    }

    if (shouldForceImportDeclContext(Kind))
      ImportDeclContext(From);
    return false;
  }

  // Start the definition.
  To->startDefinition();

  // If this class has a superclass, import it.
  if (From->getSuperClass()) {
    TypeSourceInfo *SuperTInfo = Importer.Import(From->getSuperClassTInfo());
    if (!SuperTInfo)
      return true;

    To->setSuperClass(SuperTInfo);
  }

  // Import protocols
  SmallVector<ObjCProtocolDecl *, 4> Protocols;
  SmallVector<SourceLocation, 4> ProtocolLocs;
  ObjCInterfaceDecl::protocol_loc_iterator
  FromProtoLoc = From->protocol_loc_begin();

  for (ObjCInterfaceDecl::protocol_iterator FromProto = From->protocol_begin(),
                                         FromProtoEnd = From->protocol_end();
       FromProto != FromProtoEnd;
       ++FromProto, ++FromProtoLoc) {
    auto *ToProto = cast_or_null<ObjCProtocolDecl>(Importer.Import(*FromProto));
    if (!ToProto)
      return true;
    Protocols.push_back(ToProto);
    ProtocolLocs.push_back(Importer.Import(*FromProtoLoc));
  }

  // FIXME: If we're merging, make sure that the protocol list is the same.
  To->setProtocolList(Protocols.data(), Protocols.size(),
                      ProtocolLocs.data(), Importer.getToContext());

  // Import categories. When the categories themselves are imported, they'll
  // hook themselves into this interface.
  for (auto *Cat : From->known_categories())
    Importer.Import(Cat);

  // If we have an @implementation, import it as well.
  if (From->getImplementation()) {
    auto *Impl = cast_or_null<ObjCImplementationDecl>(
        Importer.Import(From->getImplementation()));
    if (!Impl)
      return true;

    To->setImplementation(Impl);
  }

  if (shouldForceImportDeclContext(Kind)) {
    // Import all of the members of this class.
    ImportDeclContext(From, /*ForceImport=*/true);
  }
  return false;
}

ObjCTypeParamList *
ASTNodeImporter::ImportObjCTypeParamList(ObjCTypeParamList *list) {
  if (!list)
    return nullptr;

  SmallVector<ObjCTypeParamDecl *, 4> toTypeParams;
  for (auto fromTypeParam : *list) {
    auto *toTypeParam = cast_or_null<ObjCTypeParamDecl>(
        Importer.Import(fromTypeParam));
    if (!toTypeParam)
      return nullptr;

    toTypeParams.push_back(toTypeParam);
  }

  return ObjCTypeParamList::create(Importer.getToContext(),
                                   Importer.Import(list->getLAngleLoc()),
                                   toTypeParams,
                                   Importer.Import(list->getRAngleLoc()));
}

Decl *ASTNodeImporter::VisitObjCInterfaceDecl(ObjCInterfaceDecl *D) {
  // If this class has a definition in the translation unit we're coming from,
  // but this particular declaration is not that definition, import the
  // definition and map to that.
  ObjCInterfaceDecl *Definition = D->getDefinition();
  if (Definition && Definition != D) {
    Decl *ImportedDef = Importer.Import(Definition);
    if (!ImportedDef)
      return nullptr;

    return Importer.MapImported(D, ImportedDef);
  }

  // Import the major distinguishing characteristics of an @interface.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  // Look for an existing interface with the same name.
  ObjCInterfaceDecl *MergeWithIface = nullptr;
  SmallVector<NamedDecl *, 2> FoundDecls;
  DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
  for (auto *FoundDecl : FoundDecls) {
    if (!FoundDecl->isInIdentifierNamespace(Decl::IDNS_Ordinary))
      continue;

    if ((MergeWithIface = dyn_cast<ObjCInterfaceDecl>(FoundDecl)))
      break;
  }

  // Create an interface declaration, if one does not already exist.
  ObjCInterfaceDecl *ToIface = MergeWithIface;
  if (!ToIface) {
    if (GetImportedOrCreateDecl(
            ToIface, D, Importer.getToContext(), DC,
            Importer.Import(D->getAtStartLoc()), Name.getAsIdentifierInfo(),
            /*TypeParamList=*/nullptr,
            /*PrevDecl=*/nullptr, Loc, D->isImplicitInterfaceDecl()))
      return ToIface;
    ToIface->setLexicalDeclContext(LexicalDC);
    LexicalDC->addDeclInternal(ToIface);
  }
  Importer.MapImported(D, ToIface);
  // Import the type parameter list after calling Imported, to avoid
  // loops when bringing in their DeclContext.
  ToIface->setTypeParamList(ImportObjCTypeParamList(
                              D->getTypeParamListAsWritten()));

  if (D->isThisDeclarationADefinition() && ImportDefinition(D, ToIface))
    return nullptr;

  return ToIface;
}

Decl *ASTNodeImporter::VisitObjCCategoryImplDecl(ObjCCategoryImplDecl *D) {
  auto *Category = cast_or_null<ObjCCategoryDecl>(
      Importer.Import(D->getCategoryDecl()));
  if (!Category)
    return nullptr;

  ObjCCategoryImplDecl *ToImpl = Category->getImplementation();
  if (!ToImpl) {
    DeclContext *DC = Importer.ImportContext(D->getDeclContext());
    if (!DC)
      return nullptr;

    SourceLocation CategoryNameLoc = Importer.Import(D->getCategoryNameLoc());
    if (GetImportedOrCreateDecl(
            ToImpl, D, Importer.getToContext(), DC,
            Importer.Import(D->getIdentifier()), Category->getClassInterface(),
            Importer.Import(D->getLocation()),
            Importer.Import(D->getAtStartLoc()), CategoryNameLoc))
      return ToImpl;

    DeclContext *LexicalDC = DC;
    if (D->getDeclContext() != D->getLexicalDeclContext()) {
      LexicalDC = Importer.ImportContext(D->getLexicalDeclContext());
      if (!LexicalDC)
        return nullptr;

      ToImpl->setLexicalDeclContext(LexicalDC);
    }

    LexicalDC->addDeclInternal(ToImpl);
    Category->setImplementation(ToImpl);
  }

  Importer.MapImported(D, ToImpl);
  ImportDeclContext(D);
  return ToImpl;
}

Decl *ASTNodeImporter::VisitObjCImplementationDecl(ObjCImplementationDecl *D) {
  // Find the corresponding interface.
  auto *Iface = cast_or_null<ObjCInterfaceDecl>(
      Importer.Import(D->getClassInterface()));
  if (!Iface)
    return nullptr;

  // Import the superclass, if any.
  ObjCInterfaceDecl *Super = nullptr;
  if (D->getSuperClass()) {
    Super = cast_or_null<ObjCInterfaceDecl>(
                                          Importer.Import(D->getSuperClass()));
    if (!Super)
      return nullptr;
  }

  ObjCImplementationDecl *Impl = Iface->getImplementation();
  if (!Impl) {
    // We haven't imported an implementation yet. Create a new @implementation
    // now.
    if (GetImportedOrCreateDecl(Impl, D, Importer.getToContext(),
                                Importer.ImportContext(D->getDeclContext()),
                                Iface, Super, Importer.Import(D->getLocation()),
                                Importer.Import(D->getAtStartLoc()),
                                Importer.Import(D->getSuperClassLoc()),
                                Importer.Import(D->getIvarLBraceLoc()),
                                Importer.Import(D->getIvarRBraceLoc())))
      return Impl;

    if (D->getDeclContext() != D->getLexicalDeclContext()) {
      DeclContext *LexicalDC
        = Importer.ImportContext(D->getLexicalDeclContext());
      if (!LexicalDC)
        return nullptr;
      Impl->setLexicalDeclContext(LexicalDC);
    }

    // Associate the implementation with the class it implements.
    Iface->setImplementation(Impl);
    Importer.MapImported(D, Iface->getImplementation());
  } else {
    Importer.MapImported(D, Iface->getImplementation());

    // Verify that the existing @implementation has the same superclass.
    if ((Super && !Impl->getSuperClass()) ||
        (!Super && Impl->getSuperClass()) ||
        (Super && Impl->getSuperClass() &&
         !declaresSameEntity(Super->getCanonicalDecl(),
                             Impl->getSuperClass()))) {
      Importer.ToDiag(Impl->getLocation(),
                      diag::err_odr_objc_superclass_inconsistent)
        << Iface->getDeclName();
      // FIXME: It would be nice to have the location of the superclass
      // below.
      if (Impl->getSuperClass())
        Importer.ToDiag(Impl->getLocation(),
                        diag::note_odr_objc_superclass)
        << Impl->getSuperClass()->getDeclName();
      else
        Importer.ToDiag(Impl->getLocation(),
                        diag::note_odr_objc_missing_superclass);
      if (D->getSuperClass())
        Importer.FromDiag(D->getLocation(),
                          diag::note_odr_objc_superclass)
        << D->getSuperClass()->getDeclName();
      else
        Importer.FromDiag(D->getLocation(),
                          diag::note_odr_objc_missing_superclass);
      return nullptr;
    }
  }

  // Import all of the members of this @implementation.
  ImportDeclContext(D);

  return Impl;
}

Decl *ASTNodeImporter::VisitObjCPropertyDecl(ObjCPropertyDecl *D) {
  // Import the major distinguishing characteristics of an @property.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  // Check whether we have already imported this property.
  SmallVector<NamedDecl *, 2> FoundDecls;
  DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
  for (auto *FoundDecl : FoundDecls) {
    if (auto *FoundProp = dyn_cast<ObjCPropertyDecl>(FoundDecl)) {
      // Check property types.
      if (!Importer.IsStructurallyEquivalent(D->getType(),
                                             FoundProp->getType())) {
        Importer.ToDiag(Loc, diag::err_odr_objc_property_type_inconsistent)
          << Name << D->getType() << FoundProp->getType();
        Importer.ToDiag(FoundProp->getLocation(), diag::note_odr_value_here)
          << FoundProp->getType();
        return nullptr;
      }

      // FIXME: Check property attributes, getters, setters, etc.?

      // Consider these properties to be equivalent.
      Importer.MapImported(D, FoundProp);
      return FoundProp;
    }
  }

  // Import the type.
  TypeSourceInfo *TSI = Importer.Import(D->getTypeSourceInfo());
  if (!TSI)
    return nullptr;

  // Create the new property.
  ObjCPropertyDecl *ToProperty;
  if (GetImportedOrCreateDecl(
          ToProperty, D, Importer.getToContext(), DC, Loc,
          Name.getAsIdentifierInfo(), Importer.Import(D->getAtLoc()),
          Importer.Import(D->getLParenLoc()), Importer.Import(D->getType()),
          TSI, D->getPropertyImplementation()))
    return ToProperty;

  ToProperty->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToProperty);

  ToProperty->setPropertyAttributes(D->getPropertyAttributes());
  ToProperty->setPropertyAttributesAsWritten(
                                      D->getPropertyAttributesAsWritten());
  ToProperty->setGetterName(Importer.Import(D->getGetterName()),
                            Importer.Import(D->getGetterNameLoc()));
  ToProperty->setSetterName(Importer.Import(D->getSetterName()),
                            Importer.Import(D->getSetterNameLoc()));
  ToProperty->setGetterMethodDecl(
     cast_or_null<ObjCMethodDecl>(Importer.Import(D->getGetterMethodDecl())));
  ToProperty->setSetterMethodDecl(
     cast_or_null<ObjCMethodDecl>(Importer.Import(D->getSetterMethodDecl())));
  ToProperty->setPropertyIvarDecl(
       cast_or_null<ObjCIvarDecl>(Importer.Import(D->getPropertyIvarDecl())));
  return ToProperty;
}

Decl *ASTNodeImporter::VisitObjCPropertyImplDecl(ObjCPropertyImplDecl *D) {
  auto *Property = cast_or_null<ObjCPropertyDecl>(
      Importer.Import(D->getPropertyDecl()));
  if (!Property)
    return nullptr;

  DeclContext *DC = Importer.ImportContext(D->getDeclContext());
  if (!DC)
    return nullptr;

  // Import the lexical declaration context.
  DeclContext *LexicalDC = DC;
  if (D->getDeclContext() != D->getLexicalDeclContext()) {
    LexicalDC = Importer.ImportContext(D->getLexicalDeclContext());
    if (!LexicalDC)
      return nullptr;
  }

  auto *InImpl = dyn_cast<ObjCImplDecl>(LexicalDC);
  if (!InImpl)
    return nullptr;

  // Import the ivar (for an @synthesize).
  ObjCIvarDecl *Ivar = nullptr;
  if (D->getPropertyIvarDecl()) {
    Ivar = cast_or_null<ObjCIvarDecl>(
                                    Importer.Import(D->getPropertyIvarDecl()));
    if (!Ivar)
      return nullptr;
  }

  ObjCPropertyImplDecl *ToImpl
    = InImpl->FindPropertyImplDecl(Property->getIdentifier(),
                                   Property->getQueryKind());
  if (!ToImpl) {
    if (GetImportedOrCreateDecl(ToImpl, D, Importer.getToContext(), DC,
                                Importer.Import(D->getLocStart()),
                                Importer.Import(D->getLocation()), Property,
                                D->getPropertyImplementation(), Ivar,
                                Importer.Import(D->getPropertyIvarDeclLoc())))
      return ToImpl;

    ToImpl->setLexicalDeclContext(LexicalDC);
    LexicalDC->addDeclInternal(ToImpl);
  } else {
    // Check that we have the same kind of property implementation (@synthesize
    // vs. @dynamic).
    if (D->getPropertyImplementation() != ToImpl->getPropertyImplementation()) {
      Importer.ToDiag(ToImpl->getLocation(),
                      diag::err_odr_objc_property_impl_kind_inconsistent)
        << Property->getDeclName()
        << (ToImpl->getPropertyImplementation()
                                              == ObjCPropertyImplDecl::Dynamic);
      Importer.FromDiag(D->getLocation(),
                        diag::note_odr_objc_property_impl_kind)
        << D->getPropertyDecl()->getDeclName()
        << (D->getPropertyImplementation() == ObjCPropertyImplDecl::Dynamic);
      return nullptr;
    }

    // For @synthesize, check that we have the same
    if (D->getPropertyImplementation() == ObjCPropertyImplDecl::Synthesize &&
        Ivar != ToImpl->getPropertyIvarDecl()) {
      Importer.ToDiag(ToImpl->getPropertyIvarDeclLoc(),
                      diag::err_odr_objc_synthesize_ivar_inconsistent)
        << Property->getDeclName()
        << ToImpl->getPropertyIvarDecl()->getDeclName()
        << Ivar->getDeclName();
      Importer.FromDiag(D->getPropertyIvarDeclLoc(),
                        diag::note_odr_objc_synthesize_ivar_here)
        << D->getPropertyIvarDecl()->getDeclName();
      return nullptr;
    }

    // Merge the existing implementation with the new implementation.
    Importer.MapImported(D, ToImpl);
  }

  return ToImpl;
}

Decl *ASTNodeImporter::VisitTemplateTypeParmDecl(TemplateTypeParmDecl *D) {
  // For template arguments, we adopt the translation unit as our declaration
  // context. This context will be fixed when the actual template declaration
  // is created.

  // FIXME: Import default argument.
  TemplateTypeParmDecl *ToD = nullptr;
  (void)GetImportedOrCreateDecl(
      ToD, D, Importer.getToContext(),
      Importer.getToContext().getTranslationUnitDecl(),
      Importer.Import(D->getLocStart()), Importer.Import(D->getLocation()),
      D->getDepth(), D->getIndex(), Importer.Import(D->getIdentifier()),
      D->wasDeclaredWithTypename(), D->isParameterPack());
  return ToD;
}

Decl *
ASTNodeImporter::VisitNonTypeTemplateParmDecl(NonTypeTemplateParmDecl *D) {
  // Import the name of this declaration.
  DeclarationName Name = Importer.Import(D->getDeclName());
  if (D->getDeclName() && !Name)
    return nullptr;

  // Import the location of this declaration.
  SourceLocation Loc = Importer.Import(D->getLocation());

  // Import the type of this declaration.
  QualType T = Importer.Import(D->getType());
  if (T.isNull())
    return nullptr;

  // Import type-source information.
  TypeSourceInfo *TInfo = Importer.Import(D->getTypeSourceInfo());
  if (D->getTypeSourceInfo() && !TInfo)
    return nullptr;

  // FIXME: Import default argument.

  NonTypeTemplateParmDecl *ToD = nullptr;
  (void)GetImportedOrCreateDecl(
      ToD, D, Importer.getToContext(),
      Importer.getToContext().getTranslationUnitDecl(),
      Importer.Import(D->getInnerLocStart()), Loc, D->getDepth(),
      D->getPosition(), Name.getAsIdentifierInfo(), T, D->isParameterPack(),
      TInfo);
  return ToD;
}

Decl *
ASTNodeImporter::VisitTemplateTemplateParmDecl(TemplateTemplateParmDecl *D) {
  // Import the name of this declaration.
  DeclarationName Name = Importer.Import(D->getDeclName());
  if (D->getDeclName() && !Name)
    return nullptr;

  // Import the location of this declaration.
  SourceLocation Loc = Importer.Import(D->getLocation());

  // Import template parameters.
  TemplateParameterList *TemplateParams
    = ImportTemplateParameterList(D->getTemplateParameters());
  if (!TemplateParams)
    return nullptr;

  // FIXME: Import default argument.

  TemplateTemplateParmDecl *ToD = nullptr;
  (void)GetImportedOrCreateDecl(
      ToD, D, Importer.getToContext(),
      Importer.getToContext().getTranslationUnitDecl(), Loc, D->getDepth(),
      D->getPosition(), D->isParameterPack(), Name.getAsIdentifierInfo(),
      TemplateParams);
  return ToD;
}

// Returns the definition for a (forward) declaration of a ClassTemplateDecl, if
// it has any definition in the redecl chain.
static ClassTemplateDecl *getDefinition(ClassTemplateDecl *D) {
  CXXRecordDecl *ToTemplatedDef = D->getTemplatedDecl()->getDefinition();
  if (!ToTemplatedDef)
    return nullptr;
  ClassTemplateDecl *TemplateWithDef =
      ToTemplatedDef->getDescribedClassTemplate();
  return TemplateWithDef;
}

Decl *ASTNodeImporter::VisitClassTemplateDecl(ClassTemplateDecl *D) {
  // If this record has a definition in the translation unit we're coming from,
  // but this particular declaration is not that definition, import the
  // definition and map to that.
  auto *Definition =
      cast_or_null<CXXRecordDecl>(D->getTemplatedDecl()->getDefinition());
  if (Definition && Definition != D->getTemplatedDecl()) {
    Decl *ImportedDef
      = Importer.Import(Definition->getDescribedClassTemplate());
    if (!ImportedDef)
      return nullptr;

    return Importer.MapImported(D, ImportedDef);
  }

  // Import the major distinguishing characteristics of this class template.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  // We may already have a template of the same name; try to find and match it.
  if (!DC->isFunctionOrMethod()) {
    SmallVector<NamedDecl *, 4> ConflictingDecls;
    SmallVector<NamedDecl *, 2> FoundDecls;
    DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
    for (auto *FoundDecl : FoundDecls) {
      if (!FoundDecl->isInIdentifierNamespace(Decl::IDNS_Ordinary))
        continue;

      Decl *Found = FoundDecl;
      if (auto *FoundTemplate = dyn_cast<ClassTemplateDecl>(Found)) {

        // The class to be imported is a definition.
        if (D->isThisDeclarationADefinition()) {
          // Lookup will find the fwd decl only if that is more recent than the
          // definition. So, try to get the definition if that is available in
          // the redecl chain.
          ClassTemplateDecl *TemplateWithDef = getDefinition(FoundTemplate);
          if (!TemplateWithDef)
            continue;
          FoundTemplate = TemplateWithDef; // Continue with the definition.
        }

        if (IsStructuralMatch(D, FoundTemplate)) {
          // The class templates structurally match; call it the same template.

          Importer.MapImported(D->getTemplatedDecl(),
                               FoundTemplate->getTemplatedDecl());
          return Importer.MapImported(D, FoundTemplate);
        }
      }

      ConflictingDecls.push_back(FoundDecl);
    }

    if (!ConflictingDecls.empty()) {
      Name = Importer.HandleNameConflict(Name, DC, Decl::IDNS_Ordinary,
                                         ConflictingDecls.data(),
                                         ConflictingDecls.size());
    }

    if (!Name)
      return nullptr;
  }

  CXXRecordDecl *FromTemplated = D->getTemplatedDecl();

  // Create the declaration that is being templated.
  auto *ToTemplated = cast_or_null<CXXRecordDecl>(
      Importer.Import(FromTemplated));
  if (!ToTemplated)
    return nullptr;

  // Create the class template declaration itself.
  TemplateParameterList *TemplateParams =
      ImportTemplateParameterList(D->getTemplateParameters());
  if (!TemplateParams)
    return nullptr;

  ClassTemplateDecl *D2;
  if (GetImportedOrCreateDecl(D2, D, Importer.getToContext(), DC, Loc, Name,
                              TemplateParams, ToTemplated))
    return D2;

  ToTemplated->setDescribedClassTemplate(D2);

  D2->setAccess(D->getAccess());
  D2->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(D2);

  if (FromTemplated->isCompleteDefinition() &&
      !ToTemplated->isCompleteDefinition()) {
    // FIXME: Import definition!
  }

  return D2;
}

Decl *ASTNodeImporter::VisitClassTemplateSpecializationDecl(
                                          ClassTemplateSpecializationDecl *D) {
  // If this record has a definition in the translation unit we're coming from,
  // but this particular declaration is not that definition, import the
  // definition and map to that.
  TagDecl *Definition = D->getDefinition();
  if (Definition && Definition != D) {
    Decl *ImportedDef = Importer.Import(Definition);
    if (!ImportedDef)
      return nullptr;

    return Importer.MapImported(D, ImportedDef);
  }

  auto *ClassTemplate =
      cast_or_null<ClassTemplateDecl>(Importer.Import(
                                                 D->getSpecializedTemplate()));
  if (!ClassTemplate)
    return nullptr;

  // Import the context of this declaration.
  DeclContext *DC = ClassTemplate->getDeclContext();
  if (!DC)
    return nullptr;

  DeclContext *LexicalDC = DC;
  if (D->getDeclContext() != D->getLexicalDeclContext()) {
    LexicalDC = Importer.ImportContext(D->getLexicalDeclContext());
    if (!LexicalDC)
      return nullptr;
  }

  // Import the location of this declaration.
  SourceLocation StartLoc = Importer.Import(D->getLocStart());
  SourceLocation IdLoc = Importer.Import(D->getLocation());

  // Import template arguments.
  SmallVector<TemplateArgument, 2> TemplateArgs;
  if (ImportTemplateArguments(D->getTemplateArgs().data(),
                              D->getTemplateArgs().size(),
                              TemplateArgs))
    return nullptr;

  // Try to find an existing specialization with these template arguments.
  void *InsertPos = nullptr;
  ClassTemplateSpecializationDecl *D2
    = ClassTemplate->findSpecialization(TemplateArgs, InsertPos);
  if (D2) {
    // We already have a class template specialization with these template
    // arguments.

    // FIXME: Check for specialization vs. instantiation errors.

    if (RecordDecl *FoundDef = D2->getDefinition()) {
      if (!D->isCompleteDefinition() || IsStructuralMatch(D, FoundDef)) {
        // The record types structurally match, or the "from" translation
        // unit only had a forward declaration anyway; call it the same
        // function.
        return Importer.MapImported(D, FoundDef);
      }
    }
  } else {
    // Create a new specialization.
    if (auto *PartialSpec =
            dyn_cast<ClassTemplatePartialSpecializationDecl>(D)) {
      // Import TemplateArgumentListInfo
      TemplateArgumentListInfo ToTAInfo;
      const auto &ASTTemplateArgs = *PartialSpec->getTemplateArgsAsWritten();
      if (ImportTemplateArgumentListInfo(ASTTemplateArgs, ToTAInfo))
        return nullptr;

      QualType CanonInjType = Importer.Import(
            PartialSpec->getInjectedSpecializationType());
      if (CanonInjType.isNull())
        return nullptr;
      CanonInjType = CanonInjType.getCanonicalType();

      TemplateParameterList *ToTPList = ImportTemplateParameterList(
            PartialSpec->getTemplateParameters());
      if (!ToTPList && PartialSpec->getTemplateParameters())
        return nullptr;

      if (GetImportedOrCreateDecl<ClassTemplatePartialSpecializationDecl>(
              D2, D, Importer.getToContext(), D->getTagKind(), DC, StartLoc,
              IdLoc, ToTPList, ClassTemplate,
              llvm::makeArrayRef(TemplateArgs.data(), TemplateArgs.size()),
              ToTAInfo, CanonInjType, nullptr))
        return D2;

    } else {
      if (GetImportedOrCreateDecl(
              D2, D, Importer.getToContext(), D->getTagKind(), DC, StartLoc,
              IdLoc, ClassTemplate, TemplateArgs, /*PrevDecl=*/nullptr))
        return D2;
    }

    D2->setSpecializationKind(D->getSpecializationKind());

    // Add this specialization to the class template.
    ClassTemplate->AddSpecialization(D2, InsertPos);

    // Import the qualifier, if any.
    D2->setQualifierInfo(Importer.Import(D->getQualifierLoc()));

    if (auto *TSI = D->getTypeAsWritten()) {
      TypeSourceInfo *TInfo = Importer.Import(TSI);
      if (!TInfo)
        return nullptr;
      D2->setTypeAsWritten(TInfo);
      D2->setTemplateKeywordLoc(Importer.Import(D->getTemplateKeywordLoc()));
      D2->setExternLoc(Importer.Import(D->getExternLoc()));
    }

    SourceLocation POI = Importer.Import(D->getPointOfInstantiation());
    if (POI.isValid())
      D2->setPointOfInstantiation(POI);
    else if (D->getPointOfInstantiation().isValid())
      return nullptr;

    D2->setTemplateSpecializationKind(D->getTemplateSpecializationKind());

    // Set the context of this specialization/instantiation.
    D2->setLexicalDeclContext(LexicalDC);

    // Add to the DC only if it was an explicit specialization/instantiation.
    if (D2->isExplicitInstantiationOrSpecialization()) {
      LexicalDC->addDeclInternal(D2);
    }
  }
  if (D->isCompleteDefinition() && ImportDefinition(D, D2))
    return nullptr;

  return D2;
}

Decl *ASTNodeImporter::VisitVarTemplateDecl(VarTemplateDecl *D) {
  // If this variable has a definition in the translation unit we're coming
  // from,
  // but this particular declaration is not that definition, import the
  // definition and map to that.
  auto *Definition =
      cast_or_null<VarDecl>(D->getTemplatedDecl()->getDefinition());
  if (Definition && Definition != D->getTemplatedDecl()) {
    Decl *ImportedDef = Importer.Import(Definition->getDescribedVarTemplate());
    if (!ImportedDef)
      return nullptr;

    return Importer.MapImported(D, ImportedDef);
  }

  // Import the major distinguishing characteristics of this variable template.
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;
  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;
  if (ToD)
    return ToD;

  // We may already have a template of the same name; try to find and match it.
  assert(!DC->isFunctionOrMethod() &&
         "Variable templates cannot be declared at function scope");
  SmallVector<NamedDecl *, 4> ConflictingDecls;
  SmallVector<NamedDecl *, 2> FoundDecls;
  DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
  for (auto *FoundDecl : FoundDecls) {
    if (!FoundDecl->isInIdentifierNamespace(Decl::IDNS_Ordinary))
      continue;

    Decl *Found = FoundDecl;
    if (auto *FoundTemplate = dyn_cast<VarTemplateDecl>(Found)) {
      if (IsStructuralMatch(D, FoundTemplate)) {
        // The variable templates structurally match; call it the same template.
        Importer.MapImported(D->getTemplatedDecl(),
                             FoundTemplate->getTemplatedDecl());
        return Importer.MapImported(D, FoundTemplate);
      }
    }

    ConflictingDecls.push_back(FoundDecl);
  }

  if (!ConflictingDecls.empty()) {
    Name = Importer.HandleNameConflict(Name, DC, Decl::IDNS_Ordinary,
                                       ConflictingDecls.data(),
                                       ConflictingDecls.size());
  }

  if (!Name)
    return nullptr;

  VarDecl *DTemplated = D->getTemplatedDecl();

  // Import the type.
  QualType T = Importer.Import(DTemplated->getType());
  if (T.isNull())
    return nullptr;

  // Create the declaration that is being templated.
  auto *ToTemplated = dyn_cast_or_null<VarDecl>(Importer.Import(DTemplated));
  if (!ToTemplated)
    return nullptr;

  // Create the variable template declaration itself.
  TemplateParameterList *TemplateParams =
      ImportTemplateParameterList(D->getTemplateParameters());
  if (!TemplateParams)
    return nullptr;

  VarTemplateDecl *ToVarTD;
  if (GetImportedOrCreateDecl(ToVarTD, D, Importer.getToContext(), DC, Loc,
                              Name, TemplateParams, ToTemplated))
    return ToVarTD;

  ToTemplated->setDescribedVarTemplate(ToVarTD);

  ToVarTD->setAccess(D->getAccess());
  ToVarTD->setLexicalDeclContext(LexicalDC);
  LexicalDC->addDeclInternal(ToVarTD);

  if (DTemplated->isThisDeclarationADefinition() &&
      !ToTemplated->isThisDeclarationADefinition()) {
    // FIXME: Import definition!
  }

  return ToVarTD;
}

Decl *ASTNodeImporter::VisitVarTemplateSpecializationDecl(
    VarTemplateSpecializationDecl *D) {
  // If this record has a definition in the translation unit we're coming from,
  // but this particular declaration is not that definition, import the
  // definition and map to that.
  VarDecl *Definition = D->getDefinition();
  if (Definition && Definition != D) {
    Decl *ImportedDef = Importer.Import(Definition);
    if (!ImportedDef)
      return nullptr;

    return Importer.MapImported(D, ImportedDef);
  }

  auto *VarTemplate = cast_or_null<VarTemplateDecl>(
      Importer.Import(D->getSpecializedTemplate()));
  if (!VarTemplate)
    return nullptr;

  // Import the context of this declaration.
  DeclContext *DC = VarTemplate->getDeclContext();
  if (!DC)
    return nullptr;

  DeclContext *LexicalDC = DC;
  if (D->getDeclContext() != D->getLexicalDeclContext()) {
    LexicalDC = Importer.ImportContext(D->getLexicalDeclContext());
    if (!LexicalDC)
      return nullptr;
  }

  // Import the location of this declaration.
  SourceLocation StartLoc = Importer.Import(D->getLocStart());
  SourceLocation IdLoc = Importer.Import(D->getLocation());

  // Import template arguments.
  SmallVector<TemplateArgument, 2> TemplateArgs;
  if (ImportTemplateArguments(D->getTemplateArgs().data(),
                              D->getTemplateArgs().size(), TemplateArgs))
    return nullptr;

  // Try to find an existing specialization with these template arguments.
  void *InsertPos = nullptr;
  VarTemplateSpecializationDecl *D2 = VarTemplate->findSpecialization(
      TemplateArgs, InsertPos);
  if (D2) {
    // We already have a variable template specialization with these template
    // arguments.

    // FIXME: Check for specialization vs. instantiation errors.

    if (VarDecl *FoundDef = D2->getDefinition()) {
      if (!D->isThisDeclarationADefinition() ||
          IsStructuralMatch(D, FoundDef)) {
        // The record types structurally match, or the "from" translation
        // unit only had a forward declaration anyway; call it the same
        // variable.
        return Importer.MapImported(D, FoundDef);
      }
    }
  } else {
    // Import the type.
    QualType T = Importer.Import(D->getType());
    if (T.isNull())
      return nullptr;

    TypeSourceInfo *TInfo = Importer.Import(D->getTypeSourceInfo());
    if (D->getTypeSourceInfo() && !TInfo)
      return nullptr;

    TemplateArgumentListInfo ToTAInfo;
    if (ImportTemplateArgumentListInfo(D->getTemplateArgsInfo(), ToTAInfo))
      return nullptr;

    using PartVarSpecDecl = VarTemplatePartialSpecializationDecl;
    // Create a new specialization.
    if (auto *FromPartial = dyn_cast<PartVarSpecDecl>(D)) {
      // Import TemplateArgumentListInfo
      TemplateArgumentListInfo ArgInfos;
      const auto *FromTAArgsAsWritten = FromPartial->getTemplateArgsAsWritten();
      // NOTE: FromTAArgsAsWritten and template parameter list are non-null.
      if (ImportTemplateArgumentListInfo(*FromTAArgsAsWritten, ArgInfos))
        return nullptr;

      TemplateParameterList *ToTPList = ImportTemplateParameterList(
            FromPartial->getTemplateParameters());
      if (!ToTPList)
        return nullptr;

      PartVarSpecDecl *ToPartial;
      if (GetImportedOrCreateDecl(ToPartial, D, Importer.getToContext(), DC,
                                  StartLoc, IdLoc, ToTPList, VarTemplate, T,
                                  TInfo, D->getStorageClass(), TemplateArgs,
                                  ArgInfos))
        return ToPartial;

      auto *FromInst = FromPartial->getInstantiatedFromMember();
      auto *ToInst = cast_or_null<PartVarSpecDecl>(Importer.Import(FromInst));
      if (FromInst && !ToInst)
        return nullptr;

      ToPartial->setInstantiatedFromMember(ToInst);
      if (FromPartial->isMemberSpecialization())
        ToPartial->setMemberSpecialization();

      D2 = ToPartial;
    } else { // Full specialization
      if (GetImportedOrCreateDecl(D2, D, Importer.getToContext(), DC, StartLoc,
                                  IdLoc, VarTemplate, T, TInfo,
                                  D->getStorageClass(), TemplateArgs))
        return D2;
    }

    SourceLocation POI = D->getPointOfInstantiation();
    if (POI.isValid())
      D2->setPointOfInstantiation(Importer.Import(POI));

    D2->setSpecializationKind(D->getSpecializationKind());
    D2->setTemplateArgsInfo(ToTAInfo);

    // Add this specialization to the class template.
    VarTemplate->AddSpecialization(D2, InsertPos);

    // Import the qualifier, if any.
    D2->setQualifierInfo(Importer.Import(D->getQualifierLoc()));

    if (D->isConstexpr())
      D2->setConstexpr(true);

    // Add the specialization to this context.
    D2->setLexicalDeclContext(LexicalDC);
    LexicalDC->addDeclInternal(D2);

    D2->setAccess(D->getAccess());
  }

  // NOTE: isThisDeclarationADefinition() can return DeclarationOnly even if
  // declaration has initializer. Should this be fixed in the AST?.. Anyway,
  // we have to check the declaration for initializer - otherwise, it won't be
  // imported.
  if ((D->isThisDeclarationADefinition() || D->hasInit()) &&
      ImportDefinition(D, D2))
    return nullptr;

  return D2;
}

Decl *ASTNodeImporter::VisitFunctionTemplateDecl(FunctionTemplateDecl *D) {
  DeclContext *DC, *LexicalDC;
  DeclarationName Name;
  SourceLocation Loc;
  NamedDecl *ToD;

  if (ImportDeclParts(D, DC, LexicalDC, Name, ToD, Loc))
    return nullptr;

  if (ToD)
    return ToD;

  // Try to find a function in our own ("to") context with the same name, same
  // type, and in the same context as the function we're importing.
  if (!LexicalDC->isFunctionOrMethod()) {
    unsigned IDNS = Decl::IDNS_Ordinary;
    SmallVector<NamedDecl *, 2> FoundDecls;
    DC->getRedeclContext()->localUncachedLookup(Name, FoundDecls);
    for (auto *FoundDecl : FoundDecls) {
      if (!FoundDecl->isInIdentifierNamespace(IDNS))
        continue;

      if (auto *FoundFunction = dyn_cast<FunctionTemplateDecl>(FoundDecl)) {
        if (FoundFunction->hasExternalFormalLinkage() &&
            D->hasExternalFormalLinkage()) {
          if (IsStructuralMatch(D, FoundFunction)) {
            Importer.MapImported(D, FoundFunction);
            // FIXME: Actually try to merge the body and other attributes.
            return FoundFunction;
          }
        }
      }
    }
  }

  TemplateParameterList *Params =
      ImportTemplateParameterList(D->getTemplateParameters());
  if (!Params)
    return nullptr;

  auto *TemplatedFD =
      cast_or_null<FunctionDecl>(Importer.Import(D->getTemplatedDecl()));
  if (!TemplatedFD)
    return nullptr;

  FunctionTemplateDecl *ToFunc;
  if (GetImportedOrCreateDecl(ToFunc, D, Importer.getToContext(), DC, Loc, Name,
                              Params, TemplatedFD))
    return ToFunc;

  TemplatedFD->setDescribedFunctionTemplate(ToFunc);
  ToFunc->setAccess(D->getAccess());
  ToFunc->setLexicalDeclContext(LexicalDC);

  LexicalDC->addDeclInternal(ToFunc);
  return ToFunc;
}

//----------------------------------------------------------------------------
// Import Statements
//----------------------------------------------------------------------------

DeclGroupRef ASTNodeImporter::ImportDeclGroup(DeclGroupRef DG) {
  if (DG.isNull())
    return DeclGroupRef::Create(Importer.getToContext(), nullptr, 0);
  size_t NumDecls = DG.end() - DG.begin();
  SmallVector<Decl *, 1> ToDecls(NumDecls);
  auto &_Importer = this->Importer;
  std::transform(DG.begin(), DG.end(), ToDecls.begin(),
    [&_Importer](Decl *D) -> Decl * {
      return _Importer.Import(D);
    });
  return DeclGroupRef::Create(Importer.getToContext(),
                              ToDecls.begin(),
                              NumDecls);
}

Stmt *ASTNodeImporter::VisitStmt(Stmt *S) {
  Importer.FromDiag(S->getLocStart(), diag::err_unsupported_ast_node)
    << S->getStmtClassName();
  return nullptr;
}

Stmt *ASTNodeImporter::VisitGCCAsmStmt(GCCAsmStmt *S) {
  SmallVector<IdentifierInfo *, 4> Names;
  for (unsigned I = 0, E = S->getNumOutputs(); I != E; I++) {
    IdentifierInfo *ToII = Importer.Import(S->getOutputIdentifier(I));
    // ToII is nullptr when no symbolic name is given for output operand
    // see ParseStmtAsm::ParseAsmOperandsOpt
    if (!ToII && S->getOutputIdentifier(I))
      return nullptr;
    Names.push_back(ToII);
  }
  for (unsigned I = 0, E = S->getNumInputs(); I != E; I++) {
    IdentifierInfo *ToII = Importer.Import(S->getInputIdentifier(I));
    // ToII is nullptr when no symbolic name is given for input operand
    // see ParseStmtAsm::ParseAsmOperandsOpt
    if (!ToII && S->getInputIdentifier(I))
      return nullptr;
    Names.push_back(ToII);
  }

  SmallVector<StringLiteral *, 4> Clobbers;
  for (unsigned I = 0, E = S->getNumClobbers(); I != E; I++) {
    auto *Clobber = cast_or_null<StringLiteral>(
        Importer.Import(S->getClobberStringLiteral(I)));
    if (!Clobber)
      return nullptr;
    Clobbers.push_back(Clobber);
  }

  SmallVector<StringLiteral *, 4> Constraints;
  for (unsigned I = 0, E = S->getNumOutputs(); I != E; I++) {
    auto *Output = cast_or_null<StringLiteral>(
        Importer.Import(S->getOutputConstraintLiteral(I)));
    if (!Output)
      return nullptr;
    Constraints.push_back(Output);
  }

  for (unsigned I = 0, E = S->getNumInputs(); I != E; I++) {
    auto *Input = cast_or_null<StringLiteral>(
        Importer.Import(S->getInputConstraintLiteral(I)));
    if (!Input)
      return nullptr;
    Constraints.push_back(Input);
  }

  SmallVector<Expr *, 4> Exprs(S->getNumOutputs() + S->getNumInputs());
  if (ImportContainerChecked(S->outputs(), Exprs))
    return nullptr;

  if (ImportArrayChecked(S->inputs(), Exprs.begin() + S->getNumOutputs()))
    return nullptr;

  auto *AsmStr = cast_or_null<StringLiteral>(
      Importer.Import(S->getAsmString()));
  if (!AsmStr)
    return nullptr;

  return new (Importer.getToContext()) GCCAsmStmt(
        Importer.getToContext(),
        Importer.Import(S->getAsmLoc()),
        S->isSimple(),
        S->isVolatile(),
        S->getNumOutputs(),
        S->getNumInputs(),
        Names.data(),
        Constraints.data(),
        Exprs.data(),
        AsmStr,
        S->getNumClobbers(),
        Clobbers.data(),
        Importer.Import(S->getRParenLoc()));
}

Stmt *ASTNodeImporter::VisitDeclStmt(DeclStmt *S) {
  DeclGroupRef ToDG = ImportDeclGroup(S->getDeclGroup());
  for (auto *ToD : ToDG) {
    if (!ToD)
      return nullptr;
  }
  SourceLocation ToStartLoc = Importer.Import(S->getStartLoc());
  SourceLocation ToEndLoc = Importer.Import(S->getEndLoc());
  return new (Importer.getToContext()) DeclStmt(ToDG, ToStartLoc, ToEndLoc);
}

Stmt *ASTNodeImporter::VisitNullStmt(NullStmt *S) {
  SourceLocation ToSemiLoc = Importer.Import(S->getSemiLoc());
  return new (Importer.getToContext()) NullStmt(ToSemiLoc,
                                                S->hasLeadingEmptyMacro());
}

Stmt *ASTNodeImporter::VisitCompoundStmt(CompoundStmt *S) {
  SmallVector<Stmt *, 8> ToStmts(S->size());

  if (ImportContainerChecked(S->body(), ToStmts))
    return nullptr;

  SourceLocation ToLBraceLoc = Importer.Import(S->getLBracLoc());
  SourceLocation ToRBraceLoc = Importer.Import(S->getRBracLoc());
  return CompoundStmt::Create(Importer.getToContext(), ToStmts, ToLBraceLoc,
                              ToRBraceLoc);
}

Stmt *ASTNodeImporter::VisitCaseStmt(CaseStmt *S) {
  Expr *ToLHS = Importer.Import(S->getLHS());
  if (!ToLHS)
    return nullptr;
  Expr *ToRHS = Importer.Import(S->getRHS());
  if (!ToRHS && S->getRHS())
    return nullptr;
  Stmt *ToSubStmt = Importer.Import(S->getSubStmt());
  if (!ToSubStmt && S->getSubStmt())
    return nullptr;
  SourceLocation ToCaseLoc = Importer.Import(S->getCaseLoc());
  SourceLocation ToEllipsisLoc = Importer.Import(S->getEllipsisLoc());
  SourceLocation ToColonLoc = Importer.Import(S->getColonLoc());
  auto *ToStmt = new (Importer.getToContext())
      CaseStmt(ToLHS, ToRHS, ToCaseLoc, ToEllipsisLoc, ToColonLoc);
  ToStmt->setSubStmt(ToSubStmt);
  return ToStmt;
}

Stmt *ASTNodeImporter::VisitDefaultStmt(DefaultStmt *S) {
  SourceLocation ToDefaultLoc = Importer.Import(S->getDefaultLoc());
  SourceLocation ToColonLoc = Importer.Import(S->getColonLoc());
  Stmt *ToSubStmt = Importer.Import(S->getSubStmt());
  if (!ToSubStmt && S->getSubStmt())
    return nullptr;
  return new (Importer.getToContext()) DefaultStmt(ToDefaultLoc, ToColonLoc,
                                                   ToSubStmt);
}

Stmt *ASTNodeImporter::VisitLabelStmt(LabelStmt *S) {
  SourceLocation ToIdentLoc = Importer.Import(S->getIdentLoc());
  auto *ToLabelDecl = cast_or_null<LabelDecl>(Importer.Import(S->getDecl()));
  if (!ToLabelDecl && S->getDecl())
    return nullptr;
  Stmt *ToSubStmt = Importer.Import(S->getSubStmt());
  if (!ToSubStmt && S->getSubStmt())
    return nullptr;
  return new (Importer.getToContext()) LabelStmt(ToIdentLoc, ToLabelDecl,
                                                 ToSubStmt);
}

Stmt *ASTNodeImporter::VisitAttributedStmt(AttributedStmt *S) {
  SourceLocation ToAttrLoc = Importer.Import(S->getAttrLoc());
  ArrayRef<const Attr*> FromAttrs(S->getAttrs());
  SmallVector<const Attr *, 1> ToAttrs(FromAttrs.size());
  if (ImportContainerChecked(FromAttrs, ToAttrs))
    return nullptr;
  Stmt *ToSubStmt = Importer.Import(S->getSubStmt());
  if (!ToSubStmt && S->getSubStmt())
    return nullptr;
  return AttributedStmt::Create(Importer.getToContext(), ToAttrLoc,
                                ToAttrs, ToSubStmt);
}

Stmt *ASTNodeImporter::VisitIfStmt(IfStmt *S) {
  SourceLocation ToIfLoc = Importer.Import(S->getIfLoc());
  Stmt *ToInit = Importer.Import(S->getInit());
  if (!ToInit && S->getInit())
    return nullptr;
  VarDecl *ToConditionVariable = nullptr;
  if (VarDecl *FromConditionVariable = S->getConditionVariable()) {
    ToConditionVariable =
      dyn_cast_or_null<VarDecl>(Importer.Import(FromConditionVariable));
    if (!ToConditionVariable)
      return nullptr;
  }
  Expr *ToCondition = Importer.Import(S->getCond());
  if (!ToCondition && S->getCond())
    return nullptr;
  Stmt *ToThenStmt = Importer.Import(S->getThen());
  if (!ToThenStmt && S->getThen())
    return nullptr;
  SourceLocation ToElseLoc = Importer.Import(S->getElseLoc());
  Stmt *ToElseStmt = Importer.Import(S->getElse());
  if (!ToElseStmt && S->getElse())
    return nullptr;
  return new (Importer.getToContext()) IfStmt(Importer.getToContext(),
                                              ToIfLoc, S->isConstexpr(),
                                              ToInit,
                                              ToConditionVariable,
                                              ToCondition, ToThenStmt,
                                              ToElseLoc, ToElseStmt);
}

Stmt *ASTNodeImporter::VisitSwitchStmt(SwitchStmt *S) {
  Stmt *ToInit = Importer.Import(S->getInit());
  if (!ToInit && S->getInit())
    return nullptr;
  VarDecl *ToConditionVariable = nullptr;
  if (VarDecl *FromConditionVariable = S->getConditionVariable()) {
    ToConditionVariable =
      dyn_cast_or_null<VarDecl>(Importer.Import(FromConditionVariable));
    if (!ToConditionVariable)
      return nullptr;
  }
  Expr *ToCondition = Importer.Import(S->getCond());
  if (!ToCondition && S->getCond())
    return nullptr;
  auto *ToStmt = new (Importer.getToContext()) SwitchStmt(
                         Importer.getToContext(), ToInit,
                         ToConditionVariable, ToCondition);
  Stmt *ToBody = Importer.Import(S->getBody());
  if (!ToBody && S->getBody())
    return nullptr;
  ToStmt->setBody(ToBody);
  ToStmt->setSwitchLoc(Importer.Import(S->getSwitchLoc()));
  // Now we have to re-chain the cases.
  SwitchCase *LastChainedSwitchCase = nullptr;
  for (SwitchCase *SC = S->getSwitchCaseList(); SC != nullptr;
       SC = SC->getNextSwitchCase()) {
    auto *ToSC = dyn_cast_or_null<SwitchCase>(Importer.Import(SC));
    if (!ToSC)
      return nullptr;
    if (LastChainedSwitchCase)
      LastChainedSwitchCase->setNextSwitchCase(ToSC);
    else
      ToStmt->setSwitchCaseList(ToSC);
    LastChainedSwitchCase = ToSC;
  }
  return ToStmt;
}

Stmt *ASTNodeImporter::VisitWhileStmt(WhileStmt *S) {
  VarDecl *ToConditionVariable = nullptr;
  if (VarDecl *FromConditionVariable = S->getConditionVariable()) {
    ToConditionVariable =
      dyn_cast_or_null<VarDecl>(Importer.Import(FromConditionVariable));
    if (!ToConditionVariable)
      return nullptr;
  }
  Expr *ToCondition = Importer.Import(S->getCond());
  if (!ToCondition && S->getCond())
    return nullptr;
  Stmt *ToBody = Importer.Import(S->getBody());
  if (!ToBody && S->getBody())
    return nullptr;
  SourceLocation ToWhileLoc = Importer.Import(S->getWhileLoc());
  return new (Importer.getToContext()) WhileStmt(Importer.getToContext(),
                                                 ToConditionVariable,
                                                 ToCondition, ToBody,
                                                 ToWhileLoc);
}

Stmt *ASTNodeImporter::VisitDoStmt(DoStmt *S) {
  Stmt *ToBody = Importer.Import(S->getBody());
  if (!ToBody && S->getBody())
    return nullptr;
  Expr *ToCondition = Importer.Import(S->getCond());
  if (!ToCondition && S->getCond())
    return nullptr;
  SourceLocation ToDoLoc = Importer.Import(S->getDoLoc());
  SourceLocation ToWhileLoc = Importer.Import(S->getWhileLoc());
  SourceLocation ToRParenLoc = Importer.Import(S->getRParenLoc());
  return new (Importer.getToContext()) DoStmt(ToBody, ToCondition,
                                              ToDoLoc, ToWhileLoc,
                                              ToRParenLoc);
}

Stmt *ASTNodeImporter::VisitForStmt(ForStmt *S) {
  Stmt *ToInit = Importer.Import(S->getInit());
  if (!ToInit && S->getInit())
    return nullptr;
  Expr *ToCondition = Importer.Import(S->getCond());
  if (!ToCondition && S->getCond())
    return nullptr;
  VarDecl *ToConditionVariable = nullptr;
  if (VarDecl *FromConditionVariable = S->getConditionVariable()) {
    ToConditionVariable =
      dyn_cast_or_null<VarDecl>(Importer.Import(FromConditionVariable));
    if (!ToConditionVariable)
      return nullptr;
  }
  Expr *ToInc = Importer.Import(S->getInc());
  if (!ToInc && S->getInc())
    return nullptr;
  Stmt *ToBody = Importer.Import(S->getBody());
  if (!ToBody && S->getBody())
    return nullptr;
  SourceLocation ToForLoc = Importer.Import(S->getForLoc());
  SourceLocation ToLParenLoc = Importer.Import(S->getLParenLoc());
  SourceLocation ToRParenLoc = Importer.Import(S->getRParenLoc());
  return new (Importer.getToContext()) ForStmt(Importer.getToContext(),
                                               ToInit, ToCondition,
                                               ToConditionVariable,
                                               ToInc, ToBody,
                                               ToForLoc, ToLParenLoc,
                                               ToRParenLoc);
}

Stmt *ASTNodeImporter::VisitGotoStmt(GotoStmt *S) {
  LabelDecl *ToLabel = nullptr;
  if (LabelDecl *FromLabel = S->getLabel()) {
    ToLabel = dyn_cast_or_null<LabelDecl>(Importer.Import(FromLabel));
    if (!ToLabel)
      return nullptr;
  }
  SourceLocation ToGotoLoc = Importer.Import(S->getGotoLoc());
  SourceLocation ToLabelLoc = Importer.Import(S->getLabelLoc());
  return new (Importer.getToContext()) GotoStmt(ToLabel,
                                                ToGotoLoc, ToLabelLoc);
}

Stmt *ASTNodeImporter::VisitIndirectGotoStmt(IndirectGotoStmt *S) {
  SourceLocation ToGotoLoc = Importer.Import(S->getGotoLoc());
  SourceLocation ToStarLoc = Importer.Import(S->getStarLoc());
  Expr *ToTarget = Importer.Import(S->getTarget());
  if (!ToTarget && S->getTarget())
    return nullptr;
  return new (Importer.getToContext()) IndirectGotoStmt(ToGotoLoc, ToStarLoc,
                                                        ToTarget);
}

Stmt *ASTNodeImporter::VisitContinueStmt(ContinueStmt *S) {
  SourceLocation ToContinueLoc = Importer.Import(S->getContinueLoc());
  return new (Importer.getToContext()) ContinueStmt(ToContinueLoc);
}

Stmt *ASTNodeImporter::VisitBreakStmt(BreakStmt *S) {
  SourceLocation ToBreakLoc = Importer.Import(S->getBreakLoc());
  return new (Importer.getToContext()) BreakStmt(ToBreakLoc);
}

Stmt *ASTNodeImporter::VisitReturnStmt(ReturnStmt *S) {
  SourceLocation ToRetLoc = Importer.Import(S->getReturnLoc());
  Expr *ToRetExpr = Importer.Import(S->getRetValue());
  if (!ToRetExpr && S->getRetValue())
    return nullptr;
  auto *NRVOCandidate = const_cast<VarDecl *>(S->getNRVOCandidate());
  auto *ToNRVOCandidate = cast_or_null<VarDecl>(Importer.Import(NRVOCandidate));
  if (!ToNRVOCandidate && NRVOCandidate)
    return nullptr;
  return new (Importer.getToContext()) ReturnStmt(ToRetLoc, ToRetExpr,
                                                  ToNRVOCandidate);
}

Stmt *ASTNodeImporter::VisitCXXCatchStmt(CXXCatchStmt *S) {
  SourceLocation ToCatchLoc = Importer.Import(S->getCatchLoc());
  VarDecl *ToExceptionDecl = nullptr;
  if (VarDecl *FromExceptionDecl = S->getExceptionDecl()) {
    ToExceptionDecl =
      dyn_cast_or_null<VarDecl>(Importer.Import(FromExceptionDecl));
    if (!ToExceptionDecl)
      return nullptr;
  }
  Stmt *ToHandlerBlock = Importer.Import(S->getHandlerBlock());
  if (!ToHandlerBlock && S->getHandlerBlock())
    return nullptr;
  return new (Importer.getToContext()) CXXCatchStmt(ToCatchLoc,
                                                    ToExceptionDecl,
                                                    ToHandlerBlock);
}

Stmt *ASTNodeImporter::VisitCXXTryStmt(CXXTryStmt *S) {
  SourceLocation ToTryLoc = Importer.Import(S->getTryLoc());
  Stmt *ToTryBlock = Importer.Import(S->getTryBlock());
  if (!ToTryBlock && S->getTryBlock())
    return nullptr;
  SmallVector<Stmt *, 1> ToHandlers(S->getNumHandlers());
  for (unsigned HI = 0, HE = S->getNumHandlers(); HI != HE; ++HI) {
    CXXCatchStmt *FromHandler = S->getHandler(HI);
    if (Stmt *ToHandler = Importer.Import(FromHandler))
      ToHandlers[HI] = ToHandler;
    else
      return nullptr;
  }
  return CXXTryStmt::Create(Importer.getToContext(), ToTryLoc, ToTryBlock,
                            ToHandlers);
}

Stmt *ASTNodeImporter::VisitCXXForRangeStmt(CXXForRangeStmt *S) {
  auto *ToRange =
    dyn_cast_or_null<DeclStmt>(Importer.Import(S->getRangeStmt()));
  if (!ToRange && S->getRangeStmt())
    return nullptr;
  auto *ToBegin =
    dyn_cast_or_null<DeclStmt>(Importer.Import(S->getBeginStmt()));
  if (!ToBegin && S->getBeginStmt())
    return nullptr;
  auto *ToEnd =
    dyn_cast_or_null<DeclStmt>(Importer.Import(S->getEndStmt()));
  if (!ToEnd && S->getEndStmt())
    return nullptr;
  Expr *ToCond = Importer.Import(S->getCond());
  if (!ToCond && S->getCond())
    return nullptr;
  Expr *ToInc = Importer.Import(S->getInc());
  if (!ToInc && S->getInc())
    return nullptr;
  auto *ToLoopVar =
    dyn_cast_or_null<DeclStmt>(Importer.Import(S->getLoopVarStmt()));
  if (!ToLoopVar && S->getLoopVarStmt())
    return nullptr;
  Stmt *ToBody = Importer.Import(S->getBody());
  if (!ToBody && S->getBody())
    return nullptr;
  SourceLocation ToForLoc = Importer.Import(S->getForLoc());
  SourceLocation ToCoawaitLoc = Importer.Import(S->getCoawaitLoc());
  SourceLocation ToColonLoc = Importer.Import(S->getColonLoc());
  SourceLocation ToRParenLoc = Importer.Import(S->getRParenLoc());
  return new (Importer.getToContext()) CXXForRangeStmt(ToRange, ToBegin, ToEnd,
                                                       ToCond, ToInc,
                                                       ToLoopVar, ToBody,
                                                       ToForLoc, ToCoawaitLoc,
                                                       ToColonLoc, ToRParenLoc);
}

Stmt *ASTNodeImporter::VisitObjCForCollectionStmt(ObjCForCollectionStmt *S) {
  Stmt *ToElem = Importer.Import(S->getElement());
  if (!ToElem && S->getElement())
    return nullptr;
  Expr *ToCollect = Importer.Import(S->getCollection());
  if (!ToCollect && S->getCollection())
    return nullptr;
  Stmt *ToBody = Importer.Import(S->getBody());
  if (!ToBody && S->getBody())
    return nullptr;
  SourceLocation ToForLoc = Importer.Import(S->getForLoc());
  SourceLocation ToRParenLoc = Importer.Import(S->getRParenLoc());
  return new (Importer.getToContext()) ObjCForCollectionStmt(ToElem,
                                                             ToCollect,
                                                             ToBody, ToForLoc,
                                                             ToRParenLoc);
}

Stmt *ASTNodeImporter::VisitObjCAtCatchStmt(ObjCAtCatchStmt *S) {
  SourceLocation ToAtCatchLoc = Importer.Import(S->getAtCatchLoc());
  SourceLocation ToRParenLoc = Importer.Import(S->getRParenLoc());
  VarDecl *ToExceptionDecl = nullptr;
  if (VarDecl *FromExceptionDecl = S->getCatchParamDecl()) {
    ToExceptionDecl =
      dyn_cast_or_null<VarDecl>(Importer.Import(FromExceptionDecl));
    if (!ToExceptionDecl)
      return nullptr;
  }
  Stmt *ToBody = Importer.Import(S->getCatchBody());
  if (!ToBody && S->getCatchBody())
    return nullptr;
  return new (Importer.getToContext()) ObjCAtCatchStmt(ToAtCatchLoc,
                                                       ToRParenLoc,
                                                       ToExceptionDecl,
                                                       ToBody);
}

Stmt *ASTNodeImporter::VisitObjCAtFinallyStmt(ObjCAtFinallyStmt *S) {
  SourceLocation ToAtFinallyLoc = Importer.Import(S->getAtFinallyLoc());
  Stmt *ToAtFinallyStmt = Importer.Import(S->getFinallyBody());
  if (!ToAtFinallyStmt && S->getFinallyBody())
    return nullptr;
  return new (Importer.getToContext()) ObjCAtFinallyStmt(ToAtFinallyLoc,
                                                         ToAtFinallyStmt);
}

Stmt *ASTNodeImporter::VisitObjCAtTryStmt(ObjCAtTryStmt *S) {
  SourceLocation ToAtTryLoc = Importer.Import(S->getAtTryLoc());
  Stmt *ToAtTryStmt = Importer.Import(S->getTryBody());
  if (!ToAtTryStmt && S->getTryBody())
    return nullptr;
  SmallVector<Stmt *, 1> ToCatchStmts(S->getNumCatchStmts());
  for (unsigned CI = 0, CE = S->getNumCatchStmts(); CI != CE; ++CI) {
    ObjCAtCatchStmt *FromCatchStmt = S->getCatchStmt(CI);
    if (Stmt *ToCatchStmt = Importer.Import(FromCatchStmt))
      ToCatchStmts[CI] = ToCatchStmt;
    else
      return nullptr;
  }
  Stmt *ToAtFinallyStmt = Importer.Import(S->getFinallyStmt());
  if (!ToAtFinallyStmt && S->getFinallyStmt())
    return nullptr;
  return ObjCAtTryStmt::Create(Importer.getToContext(),
                               ToAtTryLoc, ToAtTryStmt,
                               ToCatchStmts.begin(), ToCatchStmts.size(),
                               ToAtFinallyStmt);
}

Stmt *ASTNodeImporter::VisitObjCAtSynchronizedStmt
  (ObjCAtSynchronizedStmt *S) {
  SourceLocation ToAtSynchronizedLoc =
    Importer.Import(S->getAtSynchronizedLoc());
  Expr *ToSynchExpr = Importer.Import(S->getSynchExpr());
  if (!ToSynchExpr && S->getSynchExpr())
    return nullptr;
  Stmt *ToSynchBody = Importer.Import(S->getSynchBody());
  if (!ToSynchBody && S->getSynchBody())
    return nullptr;
  return new (Importer.getToContext()) ObjCAtSynchronizedStmt(
    ToAtSynchronizedLoc, ToSynchExpr, ToSynchBody);
}

Stmt *ASTNodeImporter::VisitObjCAtThrowStmt(ObjCAtThrowStmt *S) {
  SourceLocation ToAtThrowLoc = Importer.Import(S->getThrowLoc());
  Expr *ToThrow = Importer.Import(S->getThrowExpr());
  if (!ToThrow && S->getThrowExpr())
    return nullptr;
  return new (Importer.getToContext()) ObjCAtThrowStmt(ToAtThrowLoc, ToThrow);
}

Stmt *ASTNodeImporter::VisitObjCAutoreleasePoolStmt
  (ObjCAutoreleasePoolStmt *S) {
  SourceLocation ToAtLoc = Importer.Import(S->getAtLoc());
  Stmt *ToSubStmt = Importer.Import(S->getSubStmt());
  if (!ToSubStmt && S->getSubStmt())
    return nullptr;
  return new (Importer.getToContext()) ObjCAutoreleasePoolStmt(ToAtLoc,
                                                               ToSubStmt);
}

//----------------------------------------------------------------------------
// Import Expressions
//----------------------------------------------------------------------------
Expr *ASTNodeImporter::VisitExpr(Expr *E) {
  Importer.FromDiag(E->getLocStart(), diag::err_unsupported_ast_node)
    << E->getStmtClassName();
  return nullptr;
}

Expr *ASTNodeImporter::VisitVAArgExpr(VAArgExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *SubExpr = Importer.Import(E->getSubExpr());
  if (!SubExpr && E->getSubExpr())
    return nullptr;

  TypeSourceInfo *TInfo = Importer.Import(E->getWrittenTypeInfo());
  if (!TInfo)
    return nullptr;

  return new (Importer.getToContext()) VAArgExpr(
        Importer.Import(E->getBuiltinLoc()), SubExpr, TInfo,
        Importer.Import(E->getRParenLoc()), T, E->isMicrosoftABI());
}

Expr *ASTNodeImporter::VisitGNUNullExpr(GNUNullExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  return new (Importer.getToContext()) GNUNullExpr(
        T, Importer.Import(E->getLocStart()));
}

Expr *ASTNodeImporter::VisitPredefinedExpr(PredefinedExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  auto *SL = cast_or_null<StringLiteral>(Importer.Import(E->getFunctionName()));
  if (!SL && E->getFunctionName())
    return nullptr;

  return new (Importer.getToContext()) PredefinedExpr(
        Importer.Import(E->getLocStart()), T, E->getIdentType(), SL);
}

Expr *ASTNodeImporter::VisitDeclRefExpr(DeclRefExpr *E) {
  auto *ToD = cast_or_null<ValueDecl>(Importer.Import(E->getDecl()));
  if (!ToD)
    return nullptr;

  NamedDecl *FoundD = nullptr;
  if (E->getDecl() != E->getFoundDecl()) {
    FoundD = cast_or_null<NamedDecl>(Importer.Import(E->getFoundDecl()));
    if (!FoundD)
      return nullptr;
  }

  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  TemplateArgumentListInfo ToTAInfo;
  TemplateArgumentListInfo *ResInfo = nullptr;
  if (E->hasExplicitTemplateArgs()) {
    if (ImportTemplateArgumentListInfo(E->template_arguments(), ToTAInfo))
      return nullptr;
    ResInfo = &ToTAInfo;
  }

  DeclRefExpr *DRE = DeclRefExpr::Create(Importer.getToContext(),
                                         Importer.Import(E->getQualifierLoc()),
                                   Importer.Import(E->getTemplateKeywordLoc()),
                                         ToD,
                                        E->refersToEnclosingVariableOrCapture(),
                                         Importer.Import(E->getLocation()),
                                         T, E->getValueKind(),
                                         FoundD, ResInfo);
  if (E->hadMultipleCandidates())
    DRE->setHadMultipleCandidates(true);
  return DRE;
}

Expr *ASTNodeImporter::VisitImplicitValueInitExpr(ImplicitValueInitExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  return new (Importer.getToContext()) ImplicitValueInitExpr(T);
}

ASTNodeImporter::Designator
ASTNodeImporter::ImportDesignator(const Designator &D) {
  if (D.isFieldDesignator()) {
    IdentifierInfo *ToFieldName = Importer.Import(D.getFieldName());
    // Caller checks for import error
    return Designator(ToFieldName, Importer.Import(D.getDotLoc()),
                      Importer.Import(D.getFieldLoc()));
  }
  if (D.isArrayDesignator())
    return Designator(D.getFirstExprIndex(),
                      Importer.Import(D.getLBracketLoc()),
                      Importer.Import(D.getRBracketLoc()));

  assert(D.isArrayRangeDesignator());
  return Designator(D.getFirstExprIndex(),
                    Importer.Import(D.getLBracketLoc()),
                    Importer.Import(D.getEllipsisLoc()),
                    Importer.Import(D.getRBracketLoc()));
}


Expr *ASTNodeImporter::VisitDesignatedInitExpr(DesignatedInitExpr *DIE) {
  auto *Init = cast_or_null<Expr>(Importer.Import(DIE->getInit()));
  if (!Init)
    return nullptr;

  SmallVector<Expr *, 4> IndexExprs(DIE->getNumSubExprs() - 1);
  // List elements from the second, the first is Init itself
  for (unsigned I = 1, E = DIE->getNumSubExprs(); I < E; I++) {
    if (auto *Arg = cast_or_null<Expr>(Importer.Import(DIE->getSubExpr(I))))
      IndexExprs[I - 1] = Arg;
    else
      return nullptr;
  }

  SmallVector<Designator, 4> Designators(DIE->size());
  llvm::transform(DIE->designators(), Designators.begin(),
                  [this](const Designator &D) -> Designator {
                    return ImportDesignator(D);
                  });

  for (const auto &D : DIE->designators())
    if (D.isFieldDesignator() && !D.getFieldName())
      return nullptr;

  return DesignatedInitExpr::Create(
        Importer.getToContext(), Designators,
        IndexExprs, Importer.Import(DIE->getEqualOrColonLoc()),
        DIE->usesGNUSyntax(), Init);
}

Expr *ASTNodeImporter::VisitCXXNullPtrLiteralExpr(CXXNullPtrLiteralExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  return new (Importer.getToContext())
      CXXNullPtrLiteralExpr(T, Importer.Import(E->getLocation()));
}

Expr *ASTNodeImporter::VisitIntegerLiteral(IntegerLiteral *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  return IntegerLiteral::Create(Importer.getToContext(),
                                E->getValue(), T,
                                Importer.Import(E->getLocation()));
}

Expr *ASTNodeImporter::VisitFloatingLiteral(FloatingLiteral *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  return FloatingLiteral::Create(Importer.getToContext(),
                                E->getValue(), E->isExact(), T,
                                Importer.Import(E->getLocation()));
}

Expr *ASTNodeImporter::VisitCharacterLiteral(CharacterLiteral *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  return new (Importer.getToContext()) CharacterLiteral(E->getValue(),
                                                        E->getKind(), T,
                                          Importer.Import(E->getLocation()));
}

Expr *ASTNodeImporter::VisitStringLiteral(StringLiteral *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  SmallVector<SourceLocation, 4> Locations(E->getNumConcatenated());
  ImportArray(E->tokloc_begin(), E->tokloc_end(), Locations.begin());

  return StringLiteral::Create(Importer.getToContext(), E->getBytes(),
                               E->getKind(), E->isPascal(), T,
                               Locations.data(), Locations.size());
}

Expr *ASTNodeImporter::VisitCompoundLiteralExpr(CompoundLiteralExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  TypeSourceInfo *TInfo = Importer.Import(E->getTypeSourceInfo());
  if (!TInfo)
    return nullptr;

  Expr *Init = Importer.Import(E->getInitializer());
  if (!Init)
    return nullptr;

  return new (Importer.getToContext()) CompoundLiteralExpr(
        Importer.Import(E->getLParenLoc()), TInfo, T, E->getValueKind(),
        Init, E->isFileScope());
}

Expr *ASTNodeImporter::VisitAtomicExpr(AtomicExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  SmallVector<Expr *, 6> Exprs(E->getNumSubExprs());
  if (ImportArrayChecked(
        E->getSubExprs(), E->getSubExprs() + E->getNumSubExprs(),
        Exprs.begin()))
    return nullptr;

  return new (Importer.getToContext()) AtomicExpr(
        Importer.Import(E->getBuiltinLoc()), Exprs, T, E->getOp(),
        Importer.Import(E->getRParenLoc()));
}

Expr *ASTNodeImporter::VisitAddrLabelExpr(AddrLabelExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  auto *ToLabel = cast_or_null<LabelDecl>(Importer.Import(E->getLabel()));
  if (!ToLabel)
    return nullptr;

  return new (Importer.getToContext()) AddrLabelExpr(
        Importer.Import(E->getAmpAmpLoc()), Importer.Import(E->getLabelLoc()),
        ToLabel, T);
}

Expr *ASTNodeImporter::VisitParenExpr(ParenExpr *E) {
  Expr *SubExpr = Importer.Import(E->getSubExpr());
  if (!SubExpr)
    return nullptr;

  return new (Importer.getToContext())
                                  ParenExpr(Importer.Import(E->getLParen()),
                                            Importer.Import(E->getRParen()),
                                            SubExpr);
}

Expr *ASTNodeImporter::VisitParenListExpr(ParenListExpr *E) {
  SmallVector<Expr *, 4> Exprs(E->getNumExprs());
  if (ImportContainerChecked(E->exprs(), Exprs))
    return nullptr;

  return new (Importer.getToContext()) ParenListExpr(
        Importer.getToContext(), Importer.Import(E->getLParenLoc()),
        Exprs, Importer.Import(E->getLParenLoc()));
}

Expr *ASTNodeImporter::VisitStmtExpr(StmtExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  auto *ToSubStmt = cast_or_null<CompoundStmt>(
      Importer.Import(E->getSubStmt()));
  if (!ToSubStmt && E->getSubStmt())
    return nullptr;

  return new (Importer.getToContext()) StmtExpr(ToSubStmt, T,
        Importer.Import(E->getLParenLoc()), Importer.Import(E->getRParenLoc()));
}

Expr *ASTNodeImporter::VisitUnaryOperator(UnaryOperator *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *SubExpr = Importer.Import(E->getSubExpr());
  if (!SubExpr)
    return nullptr;

  return new (Importer.getToContext()) UnaryOperator(
      SubExpr, E->getOpcode(), T, E->getValueKind(), E->getObjectKind(),
      Importer.Import(E->getOperatorLoc()), E->canOverflow());
}

Expr *
ASTNodeImporter::VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *E) {
  QualType ResultType = Importer.Import(E->getType());

  if (E->isArgumentType()) {
    TypeSourceInfo *TInfo = Importer.Import(E->getArgumentTypeInfo());
    if (!TInfo)
      return nullptr;

    return new (Importer.getToContext()) UnaryExprOrTypeTraitExpr(E->getKind(),
                                           TInfo, ResultType,
                                           Importer.Import(E->getOperatorLoc()),
                                           Importer.Import(E->getRParenLoc()));
  }

  Expr *SubExpr = Importer.Import(E->getArgumentExpr());
  if (!SubExpr)
    return nullptr;

  return new (Importer.getToContext()) UnaryExprOrTypeTraitExpr(E->getKind(),
                                          SubExpr, ResultType,
                                          Importer.Import(E->getOperatorLoc()),
                                          Importer.Import(E->getRParenLoc()));
}

Expr *ASTNodeImporter::VisitBinaryOperator(BinaryOperator *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *LHS = Importer.Import(E->getLHS());
  if (!LHS)
    return nullptr;

  Expr *RHS = Importer.Import(E->getRHS());
  if (!RHS)
    return nullptr;

  return new (Importer.getToContext()) BinaryOperator(LHS, RHS, E->getOpcode(),
                                                      T, E->getValueKind(),
                                                      E->getObjectKind(),
                                           Importer.Import(E->getOperatorLoc()),
                                                      E->getFPFeatures());
}

Expr *ASTNodeImporter::VisitConditionalOperator(ConditionalOperator *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *ToLHS = Importer.Import(E->getLHS());
  if (!ToLHS)
    return nullptr;

  Expr *ToRHS = Importer.Import(E->getRHS());
  if (!ToRHS)
    return nullptr;

  Expr *ToCond = Importer.Import(E->getCond());
  if (!ToCond)
    return nullptr;

  return new (Importer.getToContext()) ConditionalOperator(
        ToCond, Importer.Import(E->getQuestionLoc()),
        ToLHS, Importer.Import(E->getColonLoc()),
        ToRHS, T, E->getValueKind(), E->getObjectKind());
}

Expr *ASTNodeImporter::VisitBinaryConditionalOperator(
    BinaryConditionalOperator *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *Common = Importer.Import(E->getCommon());
  if (!Common)
    return nullptr;

  Expr *Cond = Importer.Import(E->getCond());
  if (!Cond)
    return nullptr;

  auto *OpaqueValue = cast_or_null<OpaqueValueExpr>(
      Importer.Import(E->getOpaqueValue()));
  if (!OpaqueValue)
    return nullptr;

  Expr *TrueExpr = Importer.Import(E->getTrueExpr());
  if (!TrueExpr)
    return nullptr;

  Expr *FalseExpr = Importer.Import(E->getFalseExpr());
  if (!FalseExpr)
    return nullptr;

  return new (Importer.getToContext()) BinaryConditionalOperator(
        Common, OpaqueValue, Cond, TrueExpr, FalseExpr,
        Importer.Import(E->getQuestionLoc()), Importer.Import(E->getColonLoc()),
        T, E->getValueKind(), E->getObjectKind());
}

Expr *ASTNodeImporter::VisitArrayTypeTraitExpr(ArrayTypeTraitExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  TypeSourceInfo *ToQueried = Importer.Import(E->getQueriedTypeSourceInfo());
  if (!ToQueried)
    return nullptr;

  Expr *Dim = Importer.Import(E->getDimensionExpression());
  if (!Dim && E->getDimensionExpression())
    return nullptr;

  return new (Importer.getToContext()) ArrayTypeTraitExpr(
        Importer.Import(E->getLocStart()), E->getTrait(), ToQueried,
        E->getValue(), Dim, Importer.Import(E->getLocEnd()), T);
}

Expr *ASTNodeImporter::VisitExpressionTraitExpr(ExpressionTraitExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *ToQueried = Importer.Import(E->getQueriedExpression());
  if (!ToQueried)
    return nullptr;

  return new (Importer.getToContext()) ExpressionTraitExpr(
        Importer.Import(E->getLocStart()), E->getTrait(), ToQueried,
        E->getValue(), Importer.Import(E->getLocEnd()), T);
}

Expr *ASTNodeImporter::VisitOpaqueValueExpr(OpaqueValueExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *SourceExpr = Importer.Import(E->getSourceExpr());
  if (!SourceExpr && E->getSourceExpr())
    return nullptr;

  return new (Importer.getToContext()) OpaqueValueExpr(
        Importer.Import(E->getLocation()), T, E->getValueKind(),
        E->getObjectKind(), SourceExpr);
}

Expr *ASTNodeImporter::VisitArraySubscriptExpr(ArraySubscriptExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *ToLHS = Importer.Import(E->getLHS());
  if (!ToLHS)
    return nullptr;

  Expr *ToRHS = Importer.Import(E->getRHS());
  if (!ToRHS)
    return nullptr;

  return new (Importer.getToContext()) ArraySubscriptExpr(
        ToLHS, ToRHS, T, E->getValueKind(), E->getObjectKind(),
        Importer.Import(E->getRBracketLoc()));
}

Expr *ASTNodeImporter::VisitCompoundAssignOperator(CompoundAssignOperator *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  QualType CompLHSType = Importer.Import(E->getComputationLHSType());
  if (CompLHSType.isNull())
    return nullptr;

  QualType CompResultType = Importer.Import(E->getComputationResultType());
  if (CompResultType.isNull())
    return nullptr;

  Expr *LHS = Importer.Import(E->getLHS());
  if (!LHS)
    return nullptr;

  Expr *RHS = Importer.Import(E->getRHS());
  if (!RHS)
    return nullptr;

  return new (Importer.getToContext())
                        CompoundAssignOperator(LHS, RHS, E->getOpcode(),
                                               T, E->getValueKind(),
                                               E->getObjectKind(),
                                               CompLHSType, CompResultType,
                                           Importer.Import(E->getOperatorLoc()),
                                               E->getFPFeatures());
}

bool ASTNodeImporter::ImportCastPath(CastExpr *CE, CXXCastPath &Path) {
  for (auto I = CE->path_begin(), E = CE->path_end(); I != E; ++I) {
    if (CXXBaseSpecifier *Spec = Importer.Import(*I))
      Path.push_back(Spec);
    else
      return true;
  }
  return false;
}

Expr *ASTNodeImporter::VisitImplicitCastExpr(ImplicitCastExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *SubExpr = Importer.Import(E->getSubExpr());
  if (!SubExpr)
    return nullptr;

  CXXCastPath BasePath;
  if (ImportCastPath(E, BasePath))
    return nullptr;

  return ImplicitCastExpr::Create(Importer.getToContext(), T, E->getCastKind(),
                                  SubExpr, &BasePath, E->getValueKind());
}

Expr *ASTNodeImporter::VisitExplicitCastExpr(ExplicitCastExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *SubExpr = Importer.Import(E->getSubExpr());
  if (!SubExpr)
    return nullptr;

  TypeSourceInfo *TInfo = Importer.Import(E->getTypeInfoAsWritten());
  if (!TInfo && E->getTypeInfoAsWritten())
    return nullptr;

  CXXCastPath BasePath;
  if (ImportCastPath(E, BasePath))
    return nullptr;

  switch (E->getStmtClass()) {
  case Stmt::CStyleCastExprClass: {
    auto *CCE = cast<CStyleCastExpr>(E);
    return CStyleCastExpr::Create(Importer.getToContext(), T,
                                  E->getValueKind(), E->getCastKind(),
                                  SubExpr, &BasePath, TInfo,
                                  Importer.Import(CCE->getLParenLoc()),
                                  Importer.Import(CCE->getRParenLoc()));
  }

  case Stmt::CXXFunctionalCastExprClass: {
    auto *FCE = cast<CXXFunctionalCastExpr>(E);
    return CXXFunctionalCastExpr::Create(Importer.getToContext(), T,
                                         E->getValueKind(), TInfo,
                                         E->getCastKind(), SubExpr, &BasePath,
                                         Importer.Import(FCE->getLParenLoc()),
                                         Importer.Import(FCE->getRParenLoc()));
  }

  case Stmt::ObjCBridgedCastExprClass: {
      auto *OCE = cast<ObjCBridgedCastExpr>(E);
      return new (Importer.getToContext()) ObjCBridgedCastExpr(
            Importer.Import(OCE->getLParenLoc()), OCE->getBridgeKind(),
            E->getCastKind(), Importer.Import(OCE->getBridgeKeywordLoc()),
            TInfo, SubExpr);
  }
  default:
    break; // just fall through
  }

  auto *Named = cast<CXXNamedCastExpr>(E);
  SourceLocation ExprLoc = Importer.Import(Named->getOperatorLoc()),
      RParenLoc = Importer.Import(Named->getRParenLoc());
  SourceRange Brackets = Importer.Import(Named->getAngleBrackets());

  switch (E->getStmtClass()) {
  case Stmt::CXXStaticCastExprClass:
    return CXXStaticCastExpr::Create(Importer.getToContext(), T,
                                     E->getValueKind(), E->getCastKind(),
                                     SubExpr, &BasePath, TInfo,
                                     ExprLoc, RParenLoc, Brackets);

  case Stmt::CXXDynamicCastExprClass:
    return CXXDynamicCastExpr::Create(Importer.getToContext(), T,
                                      E->getValueKind(), E->getCastKind(),
                                      SubExpr, &BasePath, TInfo,
                                      ExprLoc, RParenLoc, Brackets);

  case Stmt::CXXReinterpretCastExprClass:
    return CXXReinterpretCastExpr::Create(Importer.getToContext(), T,
                                          E->getValueKind(), E->getCastKind(),
                                          SubExpr, &BasePath, TInfo,
                                          ExprLoc, RParenLoc, Brackets);

  case Stmt::CXXConstCastExprClass:
    return CXXConstCastExpr::Create(Importer.getToContext(), T,
                                    E->getValueKind(), SubExpr, TInfo, ExprLoc,
                                    RParenLoc, Brackets);
  default:
    llvm_unreachable("Cast expression of unsupported type!");
    return nullptr;
  }
}

Expr *ASTNodeImporter::VisitOffsetOfExpr(OffsetOfExpr *OE) {
  QualType T = Importer.Import(OE->getType());
  if (T.isNull())
    return nullptr;

  SmallVector<OffsetOfNode, 4> Nodes;
  for (int I = 0, E = OE->getNumComponents(); I < E; ++I) {
    const OffsetOfNode &Node = OE->getComponent(I);

    switch (Node.getKind()) {
    case OffsetOfNode::Array:
      Nodes.push_back(OffsetOfNode(Importer.Import(Node.getLocStart()),
                                   Node.getArrayExprIndex(),
                                   Importer.Import(Node.getLocEnd())));
      break;

    case OffsetOfNode::Base: {
      CXXBaseSpecifier *BS = Importer.Import(Node.getBase());
      if (!BS && Node.getBase())
        return nullptr;
      Nodes.push_back(OffsetOfNode(BS));
      break;
    }
    case OffsetOfNode::Field: {
      auto *FD = cast_or_null<FieldDecl>(Importer.Import(Node.getField()));
      if (!FD)
        return nullptr;
      Nodes.push_back(OffsetOfNode(Importer.Import(Node.getLocStart()), FD,
                                   Importer.Import(Node.getLocEnd())));
      break;
    }
    case OffsetOfNode::Identifier: {
      IdentifierInfo *ToII = Importer.Import(Node.getFieldName());
      if (!ToII)
        return nullptr;
      Nodes.push_back(OffsetOfNode(Importer.Import(Node.getLocStart()), ToII,
                                   Importer.Import(Node.getLocEnd())));
      break;
    }
    }
  }

  SmallVector<Expr *, 4> Exprs(OE->getNumExpressions());
  for (int I = 0, E = OE->getNumExpressions(); I < E; ++I) {
    Expr *ToIndexExpr = Importer.Import(OE->getIndexExpr(I));
    if (!ToIndexExpr)
      return nullptr;
    Exprs[I] = ToIndexExpr;
  }

  TypeSourceInfo *TInfo = Importer.Import(OE->getTypeSourceInfo());
  if (!TInfo && OE->getTypeSourceInfo())
    return nullptr;

  return OffsetOfExpr::Create(Importer.getToContext(), T,
                              Importer.Import(OE->getOperatorLoc()),
                              TInfo, Nodes, Exprs,
                              Importer.Import(OE->getRParenLoc()));
}

Expr *ASTNodeImporter::VisitCXXNoexceptExpr(CXXNoexceptExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *Operand = Importer.Import(E->getOperand());
  if (!Operand)
    return nullptr;

  CanThrowResult CanThrow;
  if (E->isValueDependent())
    CanThrow = CT_Dependent;
  else
    CanThrow = E->getValue() ? CT_Can : CT_Cannot;

  return new (Importer.getToContext()) CXXNoexceptExpr(
        T, Operand, CanThrow,
        Importer.Import(E->getLocStart()), Importer.Import(E->getLocEnd()));
}

Expr *ASTNodeImporter::VisitCXXThrowExpr(CXXThrowExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *SubExpr = Importer.Import(E->getSubExpr());
  if (!SubExpr && E->getSubExpr())
    return nullptr;

  return new (Importer.getToContext()) CXXThrowExpr(
        SubExpr, T, Importer.Import(E->getThrowLoc()),
        E->isThrownVariableInScope());
}

Expr *ASTNodeImporter::VisitCXXDefaultArgExpr(CXXDefaultArgExpr *E) {
  auto *Param = cast_or_null<ParmVarDecl>(Importer.Import(E->getParam()));
  if (!Param)
    return nullptr;

  return CXXDefaultArgExpr::Create(
        Importer.getToContext(), Importer.Import(E->getUsedLocation()), Param);
}

Expr *ASTNodeImporter::VisitCXXScalarValueInitExpr(CXXScalarValueInitExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  TypeSourceInfo *TypeInfo = Importer.Import(E->getTypeSourceInfo());
  if (!TypeInfo)
    return nullptr;

  return new (Importer.getToContext()) CXXScalarValueInitExpr(
        T, TypeInfo, Importer.Import(E->getRParenLoc()));
}

Expr *ASTNodeImporter::VisitCXXBindTemporaryExpr(CXXBindTemporaryExpr *E) {
  Expr *SubExpr = Importer.Import(E->getSubExpr());
  if (!SubExpr)
    return nullptr;

  auto *Dtor = cast_or_null<CXXDestructorDecl>(
        Importer.Import(const_cast<CXXDestructorDecl *>(
                          E->getTemporary()->getDestructor())));
  if (!Dtor)
    return nullptr;

  ASTContext &ToCtx = Importer.getToContext();
  CXXTemporary *Temp = CXXTemporary::Create(ToCtx, Dtor);
  return CXXBindTemporaryExpr::Create(ToCtx, Temp, SubExpr);
}

Expr *ASTNodeImporter::VisitCXXTemporaryObjectExpr(CXXTemporaryObjectExpr *CE) {
  QualType T = Importer.Import(CE->getType());
  if (T.isNull())
    return nullptr;

  TypeSourceInfo *TInfo = Importer.Import(CE->getTypeSourceInfo());
  if (!TInfo)
    return nullptr;

  SmallVector<Expr *, 8> Args(CE->getNumArgs());
  if (ImportContainerChecked(CE->arguments(), Args))
    return nullptr;

  auto *Ctor = cast_or_null<CXXConstructorDecl>(
        Importer.Import(CE->getConstructor()));
  if (!Ctor)
    return nullptr;

  return new (Importer.getToContext()) CXXTemporaryObjectExpr(
      Importer.getToContext(), Ctor, T, TInfo, Args,
      Importer.Import(CE->getParenOrBraceRange()), CE->hadMultipleCandidates(),
      CE->isListInitialization(), CE->isStdInitListInitialization(),
      CE->requiresZeroInitialization());
}

Expr *
ASTNodeImporter::VisitMaterializeTemporaryExpr(MaterializeTemporaryExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *TempE = Importer.Import(E->GetTemporaryExpr());
  if (!TempE)
    return nullptr;

  auto *ExtendedBy = cast_or_null<ValueDecl>(
        Importer.Import(const_cast<ValueDecl *>(E->getExtendingDecl())));
  if (!ExtendedBy && E->getExtendingDecl())
    return nullptr;

  auto *ToMTE =  new (Importer.getToContext()) MaterializeTemporaryExpr(
        T, TempE, E->isBoundToLvalueReference());

  // FIXME: Should ManglingNumber get numbers associated with 'to' context?
  ToMTE->setExtendingDecl(ExtendedBy, E->getManglingNumber());
  return ToMTE;
}

Expr *ASTNodeImporter::VisitPackExpansionExpr(PackExpansionExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *Pattern = Importer.Import(E->getPattern());
  if (!Pattern)
    return nullptr;

  return new (Importer.getToContext()) PackExpansionExpr(
        T, Pattern, Importer.Import(E->getEllipsisLoc()),
        E->getNumExpansions());
}

Expr *ASTNodeImporter::VisitSizeOfPackExpr(SizeOfPackExpr *E) {
  auto *Pack = cast_or_null<NamedDecl>(Importer.Import(E->getPack()));
  if (!Pack)
    return nullptr;

  Optional<unsigned> Length;

  if (!E->isValueDependent())
    Length = E->getPackLength();

  SmallVector<TemplateArgument, 8> PartialArguments;
  if (E->isPartiallySubstituted()) {
    if (ImportTemplateArguments(E->getPartialArguments().data(),
                                E->getPartialArguments().size(),
                                PartialArguments))
      return nullptr;
  }

  return SizeOfPackExpr::Create(
      Importer.getToContext(), Importer.Import(E->getOperatorLoc()), Pack,
      Importer.Import(E->getPackLoc()), Importer.Import(E->getRParenLoc()),
      Length, PartialArguments);
}

Expr *ASTNodeImporter::VisitCXXNewExpr(CXXNewExpr *CE) {
  QualType T = Importer.Import(CE->getType());
  if (T.isNull())
    return nullptr;

  SmallVector<Expr *, 4> PlacementArgs(CE->getNumPlacementArgs());
  if (ImportContainerChecked(CE->placement_arguments(), PlacementArgs))
    return nullptr;

  auto *OperatorNewDecl = cast_or_null<FunctionDecl>(
        Importer.Import(CE->getOperatorNew()));
  if (!OperatorNewDecl && CE->getOperatorNew())
    return nullptr;

  auto *OperatorDeleteDecl = cast_or_null<FunctionDecl>(
        Importer.Import(CE->getOperatorDelete()));
  if (!OperatorDeleteDecl && CE->getOperatorDelete())
    return nullptr;

  Expr *ToInit = Importer.Import(CE->getInitializer());
  if (!ToInit && CE->getInitializer())
    return nullptr;

  TypeSourceInfo *TInfo = Importer.Import(CE->getAllocatedTypeSourceInfo());
  if (!TInfo)
    return nullptr;

  Expr *ToArrSize = Importer.Import(CE->getArraySize());
  if (!ToArrSize && CE->getArraySize())
    return nullptr;

  return new (Importer.getToContext()) CXXNewExpr(
        Importer.getToContext(),
        CE->isGlobalNew(),
        OperatorNewDecl, OperatorDeleteDecl,
        CE->passAlignment(),
        CE->doesUsualArrayDeleteWantSize(),
        PlacementArgs,
        Importer.Import(CE->getTypeIdParens()),
        ToArrSize, CE->getInitializationStyle(), ToInit, T, TInfo,
        Importer.Import(CE->getSourceRange()),
        Importer.Import(CE->getDirectInitRange()));
}

Expr *ASTNodeImporter::VisitCXXDeleteExpr(CXXDeleteExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  auto *OperatorDeleteDecl = cast_or_null<FunctionDecl>(
        Importer.Import(E->getOperatorDelete()));
  if (!OperatorDeleteDecl && E->getOperatorDelete())
    return nullptr;

  Expr *ToArg = Importer.Import(E->getArgument());
  if (!ToArg && E->getArgument())
    return nullptr;

  return new (Importer.getToContext()) CXXDeleteExpr(
        T, E->isGlobalDelete(),
        E->isArrayForm(),
        E->isArrayFormAsWritten(),
        E->doesUsualArrayDeleteWantSize(),
        OperatorDeleteDecl,
        ToArg,
        Importer.Import(E->getLocStart()));
}

Expr *ASTNodeImporter::VisitCXXConstructExpr(CXXConstructExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  auto *ToCCD =
    dyn_cast_or_null<CXXConstructorDecl>(Importer.Import(E->getConstructor()));
  if (!ToCCD)
    return nullptr;

  SmallVector<Expr *, 6> ToArgs(E->getNumArgs());
  if (ImportContainerChecked(E->arguments(), ToArgs))
    return nullptr;

  return CXXConstructExpr::Create(Importer.getToContext(), T,
                                  Importer.Import(E->getLocation()),
                                  ToCCD, E->isElidable(),
                                  ToArgs, E->hadMultipleCandidates(),
                                  E->isListInitialization(),
                                  E->isStdInitListInitialization(),
                                  E->requiresZeroInitialization(),
                                  E->getConstructionKind(),
                                  Importer.Import(E->getParenOrBraceRange()));
}

Expr *ASTNodeImporter::VisitExprWithCleanups(ExprWithCleanups *EWC) {
  Expr *SubExpr = Importer.Import(EWC->getSubExpr());
  if (!SubExpr && EWC->getSubExpr())
    return nullptr;

  SmallVector<ExprWithCleanups::CleanupObject, 8> Objs(EWC->getNumObjects());
  for (unsigned I = 0, E = EWC->getNumObjects(); I < E; I++)
    if (ExprWithCleanups::CleanupObject Obj =
        cast_or_null<BlockDecl>(Importer.Import(EWC->getObject(I))))
      Objs[I] = Obj;
    else
      return nullptr;

  return ExprWithCleanups::Create(Importer.getToContext(),
                                  SubExpr, EWC->cleanupsHaveSideEffects(),
                                  Objs);
}

Expr *ASTNodeImporter::VisitCXXMemberCallExpr(CXXMemberCallExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *ToFn = Importer.Import(E->getCallee());
  if (!ToFn)
    return nullptr;

  SmallVector<Expr *, 4> ToArgs(E->getNumArgs());
  if (ImportContainerChecked(E->arguments(), ToArgs))
    return nullptr;

  return new (Importer.getToContext()) CXXMemberCallExpr(
        Importer.getToContext(), ToFn, ToArgs, T, E->getValueKind(),
        Importer.Import(E->getRParenLoc()));
}

Expr *ASTNodeImporter::VisitCXXThisExpr(CXXThisExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  return new (Importer.getToContext())
  CXXThisExpr(Importer.Import(E->getLocation()), T, E->isImplicit());
}

Expr *ASTNodeImporter::VisitCXXBoolLiteralExpr(CXXBoolLiteralExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  return new (Importer.getToContext())
  CXXBoolLiteralExpr(E->getValue(), T, Importer.Import(E->getLocation()));
}


Expr *ASTNodeImporter::VisitMemberExpr(MemberExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *ToBase = Importer.Import(E->getBase());
  if (!ToBase && E->getBase())
    return nullptr;

  auto *ToMember = dyn_cast<ValueDecl>(Importer.Import(E->getMemberDecl()));
  if (!ToMember && E->getMemberDecl())
    return nullptr;

  auto *ToDecl =
      dyn_cast_or_null<NamedDecl>(Importer.Import(E->getFoundDecl().getDecl()));
  if (!ToDecl && E->getFoundDecl().getDecl())
    return nullptr;

  DeclAccessPair ToFoundDecl =
      DeclAccessPair::make(ToDecl, E->getFoundDecl().getAccess());

  DeclarationNameInfo ToMemberNameInfo(
    Importer.Import(E->getMemberNameInfo().getName()),
    Importer.Import(E->getMemberNameInfo().getLoc()));

  if (E->hasExplicitTemplateArgs()) {
    return nullptr; // FIXME: handle template arguments
  }

  return MemberExpr::Create(Importer.getToContext(), ToBase,
                            E->isArrow(),
                            Importer.Import(E->getOperatorLoc()),
                            Importer.Import(E->getQualifierLoc()),
                            Importer.Import(E->getTemplateKeywordLoc()),
                            ToMember, ToFoundDecl, ToMemberNameInfo,
                            nullptr, T, E->getValueKind(),
                            E->getObjectKind());
}

Expr *ASTNodeImporter::VisitCXXPseudoDestructorExpr(
    CXXPseudoDestructorExpr *E) {
  Expr *BaseE = Importer.Import(E->getBase());
  if (!BaseE)
    return nullptr;

  TypeSourceInfo *ScopeInfo = Importer.Import(E->getScopeTypeInfo());
  if (!ScopeInfo && E->getScopeTypeInfo())
    return nullptr;

  PseudoDestructorTypeStorage Storage;
  if (IdentifierInfo *FromII = E->getDestroyedTypeIdentifier()) {
    IdentifierInfo *ToII = Importer.Import(FromII);
    if (!ToII)
      return nullptr;
    Storage = PseudoDestructorTypeStorage(
          ToII, Importer.Import(E->getDestroyedTypeLoc()));
  } else {
    TypeSourceInfo *TI = Importer.Import(E->getDestroyedTypeInfo());
    if (!TI)
      return nullptr;
    Storage = PseudoDestructorTypeStorage(TI);
  }

  return new (Importer.getToContext()) CXXPseudoDestructorExpr(
        Importer.getToContext(), BaseE, E->isArrow(),
        Importer.Import(E->getOperatorLoc()),
        Importer.Import(E->getQualifierLoc()),
        ScopeInfo, Importer.Import(E->getColonColonLoc()),
        Importer.Import(E->getTildeLoc()), Storage);
}

Expr *ASTNodeImporter::VisitCXXDependentScopeMemberExpr(
    CXXDependentScopeMemberExpr *E) {
  Expr *Base = nullptr;
  if (!E->isImplicitAccess()) {
    Base = Importer.Import(E->getBase());
    if (!Base)
      return nullptr;
  }

  QualType BaseType = Importer.Import(E->getBaseType());
  if (BaseType.isNull())
    return nullptr;

  TemplateArgumentListInfo ToTAInfo, *ResInfo = nullptr;
  if (E->hasExplicitTemplateArgs()) {
    if (ImportTemplateArgumentListInfo(E->getLAngleLoc(), E->getRAngleLoc(),
                                       E->template_arguments(), ToTAInfo))
      return nullptr;
    ResInfo = &ToTAInfo;
  }

  DeclarationName Name = Importer.Import(E->getMember());
  if (!E->getMember().isEmpty() && Name.isEmpty())
    return nullptr;

  DeclarationNameInfo MemberNameInfo(Name, Importer.Import(E->getMemberLoc()));
  // Import additional name location/type info.
  ImportDeclarationNameLoc(E->getMemberNameInfo(), MemberNameInfo);
  auto ToFQ = Importer.Import(E->getFirstQualifierFoundInScope());
  if (!ToFQ && E->getFirstQualifierFoundInScope())
    return nullptr;

  return CXXDependentScopeMemberExpr::Create(
      Importer.getToContext(), Base, BaseType, E->isArrow(),
      Importer.Import(E->getOperatorLoc()),
      Importer.Import(E->getQualifierLoc()),
      Importer.Import(E->getTemplateKeywordLoc()),
      cast_or_null<NamedDecl>(ToFQ), MemberNameInfo, ResInfo);
}

Expr *
ASTNodeImporter::VisitDependentScopeDeclRefExpr(DependentScopeDeclRefExpr *E) {
  DeclarationName Name = Importer.Import(E->getDeclName());
  if (!E->getDeclName().isEmpty() && Name.isEmpty())
    return nullptr;

  DeclarationNameInfo NameInfo(Name, Importer.Import(E->getExprLoc()));
  ImportDeclarationNameLoc(E->getNameInfo(), NameInfo);

  TemplateArgumentListInfo ToTAInfo(Importer.Import(E->getLAngleLoc()),
                                    Importer.Import(E->getRAngleLoc()));
  TemplateArgumentListInfo *ResInfo = nullptr;
  if (E->hasExplicitTemplateArgs()) {
    if (ImportTemplateArgumentListInfo(E->template_arguments(), ToTAInfo))
      return nullptr;
    ResInfo = &ToTAInfo;
  }

  return DependentScopeDeclRefExpr::Create(
      Importer.getToContext(), Importer.Import(E->getQualifierLoc()),
      Importer.Import(E->getTemplateKeywordLoc()), NameInfo, ResInfo);
}

Expr *ASTNodeImporter::VisitCXXUnresolvedConstructExpr(
    CXXUnresolvedConstructExpr *CE) {
  unsigned NumArgs = CE->arg_size();

  SmallVector<Expr *, 8> ToArgs(NumArgs);
  if (ImportArrayChecked(CE->arg_begin(), CE->arg_end(), ToArgs.begin()))
    return nullptr;

  return CXXUnresolvedConstructExpr::Create(
      Importer.getToContext(), Importer.Import(CE->getTypeSourceInfo()),
      Importer.Import(CE->getLParenLoc()), llvm::makeArrayRef(ToArgs),
      Importer.Import(CE->getRParenLoc()));
}

Expr *ASTNodeImporter::VisitUnresolvedLookupExpr(UnresolvedLookupExpr *E) {
  auto *NamingClass =
      cast_or_null<CXXRecordDecl>(Importer.Import(E->getNamingClass()));
  if (E->getNamingClass() && !NamingClass)
    return nullptr;

  DeclarationName Name = Importer.Import(E->getName());
  if (E->getName() && !Name)
    return nullptr;

  DeclarationNameInfo NameInfo(Name, Importer.Import(E->getNameLoc()));
  // Import additional name location/type info.
  ImportDeclarationNameLoc(E->getNameInfo(), NameInfo);

  UnresolvedSet<8> ToDecls;
  for (auto *D : E->decls()) {
    if (auto *To = cast_or_null<NamedDecl>(Importer.Import(D)))
      ToDecls.addDecl(To);
    else
      return nullptr;
  }

  TemplateArgumentListInfo ToTAInfo, *ResInfo = nullptr;
  if (E->hasExplicitTemplateArgs()) {
    if (ImportTemplateArgumentListInfo(E->getLAngleLoc(), E->getRAngleLoc(),
                                       E->template_arguments(), ToTAInfo))
      return nullptr;
    ResInfo = &ToTAInfo;
  }

  if (ResInfo || E->getTemplateKeywordLoc().isValid())
    return UnresolvedLookupExpr::Create(
        Importer.getToContext(), NamingClass,
        Importer.Import(E->getQualifierLoc()),
        Importer.Import(E->getTemplateKeywordLoc()), NameInfo, E->requiresADL(),
        ResInfo, ToDecls.begin(), ToDecls.end());

  return UnresolvedLookupExpr::Create(
      Importer.getToContext(), NamingClass,
      Importer.Import(E->getQualifierLoc()), NameInfo, E->requiresADL(),
      E->isOverloaded(), ToDecls.begin(), ToDecls.end());
}

Expr *ASTNodeImporter::VisitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
  DeclarationName Name = Importer.Import(E->getName());
  if (!E->getName().isEmpty() && Name.isEmpty())
    return nullptr;
  DeclarationNameInfo NameInfo(Name, Importer.Import(E->getNameLoc()));
  // Import additional name location/type info.
  ImportDeclarationNameLoc(E->getNameInfo(), NameInfo);

  QualType BaseType = Importer.Import(E->getType());
  if (!E->getType().isNull() && BaseType.isNull())
    return nullptr;

  UnresolvedSet<8> ToDecls;
  for (Decl *D : E->decls()) {
    if (NamedDecl *To = cast_or_null<NamedDecl>(Importer.Import(D)))
      ToDecls.addDecl(To);
    else
      return nullptr;
  }

  TemplateArgumentListInfo ToTAInfo;
  TemplateArgumentListInfo *ResInfo = nullptr;
  if (E->hasExplicitTemplateArgs()) {
    if (ImportTemplateArgumentListInfo(E->template_arguments(), ToTAInfo))
      return nullptr;
    ResInfo = &ToTAInfo;
  }

  Expr *BaseE = E->isImplicitAccess() ? nullptr : Importer.Import(E->getBase());
  if (!BaseE && !E->isImplicitAccess() && E->getBase()) {
    return nullptr;
  }

  return UnresolvedMemberExpr::Create(
      Importer.getToContext(), E->hasUnresolvedUsing(), BaseE, BaseType,
      E->isArrow(), Importer.Import(E->getOperatorLoc()),
      Importer.Import(E->getQualifierLoc()),
      Importer.Import(E->getTemplateKeywordLoc()), NameInfo, ResInfo,
      ToDecls.begin(), ToDecls.end());
}

Expr *ASTNodeImporter::VisitCallExpr(CallExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *ToCallee = Importer.Import(E->getCallee());
  if (!ToCallee && E->getCallee())
    return nullptr;

  unsigned NumArgs = E->getNumArgs();
  SmallVector<Expr *, 2> ToArgs(NumArgs);
  if (ImportContainerChecked(E->arguments(), ToArgs))
     return nullptr;

  auto **ToArgs_Copied = new (Importer.getToContext()) Expr*[NumArgs];

  for (unsigned ai = 0, ae = NumArgs; ai != ae; ++ai)
    ToArgs_Copied[ai] = ToArgs[ai];

  if (const auto *OCE = dyn_cast<CXXOperatorCallExpr>(E)) {
    return new (Importer.getToContext()) CXXOperatorCallExpr(
          Importer.getToContext(), OCE->getOperator(), ToCallee, ToArgs, T,
          OCE->getValueKind(), Importer.Import(OCE->getRParenLoc()),
          OCE->getFPFeatures());
  }

  return new (Importer.getToContext())
    CallExpr(Importer.getToContext(), ToCallee,
             llvm::makeArrayRef(ToArgs_Copied, NumArgs), T, E->getValueKind(),
             Importer.Import(E->getRParenLoc()));
}

Optional<LambdaCapture>
ASTNodeImporter::ImportLambdaCapture(const LambdaCapture &From) {
  VarDecl *Var = nullptr;
  if (From.capturesVariable()) {
    Var = cast_or_null<VarDecl>(Importer.Import(From.getCapturedVar()));
    if (!Var)
      return None;
  }

  return LambdaCapture(Importer.Import(From.getLocation()), From.isImplicit(),
                       From.getCaptureKind(), Var,
                       From.isPackExpansion()
                         ? Importer.Import(From.getEllipsisLoc())
                         : SourceLocation());
}

Expr *ASTNodeImporter::VisitLambdaExpr(LambdaExpr *LE) {
  CXXRecordDecl *FromClass = LE->getLambdaClass();
  auto *ToClass = dyn_cast_or_null<CXXRecordDecl>(Importer.Import(FromClass));
  if (!ToClass)
    return nullptr;

  // NOTE: lambda classes are created with BeingDefined flag set up.
  // It means that ImportDefinition doesn't work for them and we should fill it
  // manually.
  if (ToClass->isBeingDefined()) {
    for (auto FromField : FromClass->fields()) {
      auto *ToField = cast_or_null<FieldDecl>(Importer.Import(FromField));
      if (!ToField)
        return nullptr;
    }
  }

  auto *ToCallOp = dyn_cast_or_null<CXXMethodDecl>(
        Importer.Import(LE->getCallOperator()));
  if (!ToCallOp)
    return nullptr;

  ToClass->completeDefinition();

  unsigned NumCaptures = LE->capture_size();
  SmallVector<LambdaCapture, 8> Captures;
  Captures.reserve(NumCaptures);
  for (const auto &FromCapture : LE->captures()) {
    if (auto ToCapture = ImportLambdaCapture(FromCapture))
      Captures.push_back(*ToCapture);
    else
      return nullptr;
  }

  SmallVector<Expr *, 8> InitCaptures(NumCaptures);
  if (ImportContainerChecked(LE->capture_inits(), InitCaptures))
    return nullptr;

  return LambdaExpr::Create(Importer.getToContext(), ToClass,
                            Importer.Import(LE->getIntroducerRange()),
                            LE->getCaptureDefault(),
                            Importer.Import(LE->getCaptureDefaultLoc()),
                            Captures,
                            LE->hasExplicitParameters(),
                            LE->hasExplicitResultType(),
                            InitCaptures,
                            Importer.Import(LE->getLocEnd()),
                            LE->containsUnexpandedParameterPack());
}

Expr *ASTNodeImporter::VisitInitListExpr(InitListExpr *ILE) {
  QualType T = Importer.Import(ILE->getType());
  if (T.isNull())
    return nullptr;

  SmallVector<Expr *, 4> Exprs(ILE->getNumInits());
  if (ImportContainerChecked(ILE->inits(), Exprs))
    return nullptr;

  ASTContext &ToCtx = Importer.getToContext();
  InitListExpr *To = new (ToCtx) InitListExpr(
        ToCtx, Importer.Import(ILE->getLBraceLoc()),
        Exprs, Importer.Import(ILE->getLBraceLoc()));
  To->setType(T);

  if (ILE->hasArrayFiller()) {
    Expr *Filler = Importer.Import(ILE->getArrayFiller());
    if (!Filler)
      return nullptr;
    To->setArrayFiller(Filler);
  }

  if (FieldDecl *FromFD = ILE->getInitializedFieldInUnion()) {
    auto *ToFD = cast_or_null<FieldDecl>(Importer.Import(FromFD));
    if (!ToFD)
      return nullptr;
    To->setInitializedFieldInUnion(ToFD);
  }

  if (InitListExpr *SyntForm = ILE->getSyntacticForm()) {
    auto *ToSyntForm = cast_or_null<InitListExpr>(Importer.Import(SyntForm));
    if (!ToSyntForm)
      return nullptr;
    To->setSyntacticForm(ToSyntForm);
  }

  To->sawArrayRangeDesignator(ILE->hadArrayRangeDesignator());
  To->setValueDependent(ILE->isValueDependent());
  To->setInstantiationDependent(ILE->isInstantiationDependent());

  return To;
}

Expr *ASTNodeImporter::VisitCXXStdInitializerListExpr(
    CXXStdInitializerListExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  Expr *SE = Importer.Import(E->getSubExpr());
  if (!SE)
    return nullptr;

  return new (Importer.getToContext()) CXXStdInitializerListExpr(T, SE);
}

Expr *ASTNodeImporter::VisitCXXInheritedCtorInitExpr(
    CXXInheritedCtorInitExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  auto *Ctor = cast_or_null<CXXConstructorDecl>(Importer.Import(
      E->getConstructor()));
  if (!Ctor)
    return nullptr;

  return new (Importer.getToContext()) CXXInheritedCtorInitExpr(
      Importer.Import(E->getLocation()), T, Ctor,
      E->constructsVBase(), E->inheritedFromVBase());
}

Expr *ASTNodeImporter::VisitArrayInitLoopExpr(ArrayInitLoopExpr *E) {
  QualType ToType = Importer.Import(E->getType());
  if (ToType.isNull())
    return nullptr;

  Expr *ToCommon = Importer.Import(E->getCommonExpr());
  if (!ToCommon && E->getCommonExpr())
    return nullptr;

  Expr *ToSubExpr = Importer.Import(E->getSubExpr());
  if (!ToSubExpr && E->getSubExpr())
    return nullptr;

  return new (Importer.getToContext())
      ArrayInitLoopExpr(ToType, ToCommon, ToSubExpr);
}

Expr *ASTNodeImporter::VisitArrayInitIndexExpr(ArrayInitIndexExpr *E) {
  QualType ToType = Importer.Import(E->getType());
  if (ToType.isNull())
    return nullptr;
  return new (Importer.getToContext()) ArrayInitIndexExpr(ToType);
}

Expr *ASTNodeImporter::VisitCXXDefaultInitExpr(CXXDefaultInitExpr *DIE) {
  auto *ToField = dyn_cast_or_null<FieldDecl>(Importer.Import(DIE->getField()));
  if (!ToField && DIE->getField())
    return nullptr;

  return CXXDefaultInitExpr::Create(
      Importer.getToContext(), Importer.Import(DIE->getLocStart()), ToField);
}

Expr *ASTNodeImporter::VisitCXXNamedCastExpr(CXXNamedCastExpr *E) {
  QualType ToType = Importer.Import(E->getType());
  if (ToType.isNull() && !E->getType().isNull())
    return nullptr;
  ExprValueKind VK = E->getValueKind();
  CastKind CK = E->getCastKind();
  Expr *ToOp = Importer.Import(E->getSubExpr());
  if (!ToOp && E->getSubExpr())
    return nullptr;
  CXXCastPath BasePath;
  if (ImportCastPath(E, BasePath))
    return nullptr;
  TypeSourceInfo *ToWritten = Importer.Import(E->getTypeInfoAsWritten());
  SourceLocation ToOperatorLoc = Importer.Import(E->getOperatorLoc());
  SourceLocation ToRParenLoc = Importer.Import(E->getRParenLoc());
  SourceRange ToAngleBrackets = Importer.Import(E->getAngleBrackets());

  if (isa<CXXStaticCastExpr>(E)) {
    return CXXStaticCastExpr::Create(
        Importer.getToContext(), ToType, VK, CK, ToOp, &BasePath,
        ToWritten, ToOperatorLoc, ToRParenLoc, ToAngleBrackets);
  } else if (isa<CXXDynamicCastExpr>(E)) {
    return CXXDynamicCastExpr::Create(
        Importer.getToContext(), ToType, VK, CK, ToOp, &BasePath,
        ToWritten, ToOperatorLoc, ToRParenLoc, ToAngleBrackets);
  } else if (isa<CXXReinterpretCastExpr>(E)) {
    return CXXReinterpretCastExpr::Create(
        Importer.getToContext(), ToType, VK, CK, ToOp, &BasePath,
        ToWritten, ToOperatorLoc, ToRParenLoc, ToAngleBrackets);
  } else {
    return nullptr;
  }
}

Expr *ASTNodeImporter::VisitSubstNonTypeTemplateParmExpr(
    SubstNonTypeTemplateParmExpr *E) {
  QualType T = Importer.Import(E->getType());
  if (T.isNull())
    return nullptr;

  auto *Param = cast_or_null<NonTypeTemplateParmDecl>(
        Importer.Import(E->getParameter()));
  if (!Param)
    return nullptr;

  Expr *Replacement = Importer.Import(E->getReplacement());
  if (!Replacement)
    return nullptr;

  return new (Importer.getToContext()) SubstNonTypeTemplateParmExpr(
        T, E->getValueKind(), Importer.Import(E->getExprLoc()), Param,
        Replacement);
}

Expr *ASTNodeImporter::VisitTypeTraitExpr(TypeTraitExpr *E) {
  QualType ToType = Importer.Import(E->getType());
  if (ToType.isNull())
    return nullptr;

  SmallVector<TypeSourceInfo *, 4> ToArgs(E->getNumArgs());
  if (ImportContainerChecked(E->getArgs(), ToArgs))
    return nullptr;

  // According to Sema::BuildTypeTrait(), if E is value-dependent,
  // Value is always false.
  bool ToValue = false;
  if (!E->isValueDependent())
    ToValue = E->getValue();

  return TypeTraitExpr::Create(
      Importer.getToContext(), ToType, Importer.Import(E->getLocStart()),
      E->getTrait(), ToArgs, Importer.Import(E->getLocEnd()), ToValue);
}

Expr *ASTNodeImporter::VisitCXXTypeidExpr(CXXTypeidExpr *E) {
  QualType ToType = Importer.Import(E->getType());
  if (ToType.isNull())
    return nullptr;

  if (E->isTypeOperand()) {
    TypeSourceInfo *TSI = Importer.Import(E->getTypeOperandSourceInfo());
    if (!TSI)
      return nullptr;

    return new (Importer.getToContext())
        CXXTypeidExpr(ToType, TSI, Importer.Import(E->getSourceRange()));
  }

  Expr *Op = Importer.Import(E->getExprOperand());
  if (!Op)
    return nullptr;

  return new (Importer.getToContext())
      CXXTypeidExpr(ToType, Op, Importer.Import(E->getSourceRange()));
}

void ASTNodeImporter::ImportOverrides(CXXMethodDecl *ToMethod,
                                      CXXMethodDecl *FromMethod) {
  for (auto *FromOverriddenMethod : FromMethod->overridden_methods())
    ToMethod->addOverriddenMethod(
      cast<CXXMethodDecl>(Importer.Import(const_cast<CXXMethodDecl*>(
                                            FromOverriddenMethod))));
}

//ASUTTON ADDN:
Expr *ASTNodeImporter::VisitCompilerMessageExpr(CompilerMessageExpr *E) {
  Expr *FromMsg = E->getMessage();
  Expr *ToMsg = Importer.Import(FromMsg);
  if (!ToMsg && FromMsg)
    return nullptr;

  return CompilerMessageExpr::Create(Importer.getToContext(),
                                     ToMsg,
                                     Importer.Import(E->getKWLoc()),
                                     Importer.Import(E->getRParenLoc()));
}
//END
//DWR ADDN:
Expr *ASTNodeImporter::VisitCompilerDiagnosticExpr(CompilerDiagnosticExpr *E) {
    //Adapted from VisitAttributedStmt, TESTME
    ArrayRef<Expr*> FromExprs(E->getArgs());
    SmallVector<Expr *, 4> ToExprs(FromExprs.size());
    if (ImportContainerChecked(FromExprs, ToExprs))
      return nullptr;

  return CompilerDiagnosticExpr::Create(Importer.getToContext(),
                                        Importer.Import(E->getKWLoc()),
                                        makeArrayRef(ToExprs),
                                        Importer.Import(E->getRParenLoc()));
}
//END

ASTImporter::ASTImporter(ASTContext &ToContext, FileManager &ToFileManager,
                         ASTContext &FromContext, FileManager &FromFileManager,
                         bool MinimalImport)
    : ToContext(ToContext), FromContext(FromContext),
      ToFileManager(ToFileManager), FromFileManager(FromFileManager),
      Minimal(MinimalImport) {
  ImportedDecls[FromContext.getTranslationUnitDecl()]
    = ToContext.getTranslationUnitDecl();
}

ASTImporter::~ASTImporter() = default;

QualType ASTImporter::Import(QualType FromT) {
  if (FromT.isNull())
    return {};

  const Type *fromTy = FromT.getTypePtr();

  // Check whether we've already imported this type.
  llvm::DenseMap<const Type *, const Type *>::iterator Pos
    = ImportedTypes.find(fromTy);
  if (Pos != ImportedTypes.end())
    return ToContext.getQualifiedType(Pos->second, FromT.getLocalQualifiers());

  // Import the type
  ASTNodeImporter Importer(*this);
  QualType ToT = Importer.Visit(fromTy);
  if (ToT.isNull())
    return ToT;

  // Record the imported type.
  ImportedTypes[fromTy] = ToT.getTypePtr();

  return ToContext.getQualifiedType(ToT, FromT.getLocalQualifiers());
}

TypeSourceInfo *ASTImporter::Import(TypeSourceInfo *FromTSI) {
  if (!FromTSI)
    return FromTSI;

  // FIXME: For now we just create a "trivial" type source info based
  // on the type and a single location. Implement a real version of this.
  QualType T = Import(FromTSI->getType());
  if (T.isNull())
    return nullptr;

  return ToContext.getTrivialTypeSourceInfo(T,
           Import(FromTSI->getTypeLoc().getLocStart()));
}

Attr *ASTImporter::Import(const Attr *FromAttr) {
  Attr *ToAttr = FromAttr->clone(ToContext);
  ToAttr->setRange(Import(FromAttr->getRange()));
  return ToAttr;
}

Decl *ASTImporter::GetAlreadyImportedOrNull(Decl *FromD) {
  llvm::DenseMap<Decl *, Decl *>::iterator Pos = ImportedDecls.find(FromD);
  if (Pos != ImportedDecls.end()) {
    Decl *ToD = Pos->second;
    // FIXME: move this call to ImportDeclParts().
    ASTNodeImporter(*this).ImportDefinitionIfNeeded(FromD, ToD);
    return ToD;
  } else {
    return nullptr;
  }
}

Decl *ASTImporter::Import(Decl *FromD) {
  if (!FromD)
    return nullptr;

  ASTNodeImporter Importer(*this);

  // Check whether we've already imported this declaration.
  Decl *ToD = GetAlreadyImportedOrNull(FromD);
  if (ToD) {
    // If FromD has some updated flags after last import, apply it
    updateFlags(FromD, ToD);
    return ToD;
  }

  // Import the type.
  ToD = Importer.Visit(FromD);
  if (!ToD)
    return nullptr;

  // Notify subclasses.
  Imported(FromD, ToD);

  return ToD;
}

DeclContext *ASTImporter::ImportContext(DeclContext *FromDC) {
  if (!FromDC)
    return FromDC;

  auto *ToDC = cast_or_null<DeclContext>(Import(cast<Decl>(FromDC)));
  if (!ToDC)
    return nullptr;

  // When we're using a record/enum/Objective-C class/protocol as a context, we
  // need it to have a definition.
  if (auto *ToRecord = dyn_cast<RecordDecl>(ToDC)) {
    auto *FromRecord = cast<RecordDecl>(FromDC);
    if (ToRecord->isCompleteDefinition()) {
      // Do nothing.
    } else if (FromRecord->isCompleteDefinition()) {
      ASTNodeImporter(*this).ImportDefinition(FromRecord, ToRecord,
                                              ASTNodeImporter::IDK_Basic);
    } else {
      CompleteDecl(ToRecord);
    }
  } else if (auto *ToEnum = dyn_cast<EnumDecl>(ToDC)) {
    auto *FromEnum = cast<EnumDecl>(FromDC);
    if (ToEnum->isCompleteDefinition()) {
      // Do nothing.
    } else if (FromEnum->isCompleteDefinition()) {
      ASTNodeImporter(*this).ImportDefinition(FromEnum, ToEnum,
                                              ASTNodeImporter::IDK_Basic);
    } else {
      CompleteDecl(ToEnum);
    }
  } else if (auto *ToClass = dyn_cast<ObjCInterfaceDecl>(ToDC)) {
    auto *FromClass = cast<ObjCInterfaceDecl>(FromDC);
    if (ToClass->getDefinition()) {
      // Do nothing.
    } else if (ObjCInterfaceDecl *FromDef = FromClass->getDefinition()) {
      ASTNodeImporter(*this).ImportDefinition(FromDef, ToClass,
                                              ASTNodeImporter::IDK_Basic);
    } else {
      CompleteDecl(ToClass);
    }
  } else if (auto *ToProto = dyn_cast<ObjCProtocolDecl>(ToDC)) {
    auto *FromProto = cast<ObjCProtocolDecl>(FromDC);
    if (ToProto->getDefinition()) {
      // Do nothing.
    } else if (ObjCProtocolDecl *FromDef = FromProto->getDefinition()) {
      ASTNodeImporter(*this).ImportDefinition(FromDef, ToProto,
                                              ASTNodeImporter::IDK_Basic);
    } else {
      CompleteDecl(ToProto);
    }
  }

  return ToDC;
}

Expr *ASTImporter::Import(Expr *FromE) {
  if (!FromE)
    return nullptr;

  return cast_or_null<Expr>(Import(cast<Stmt>(FromE)));
}

Stmt *ASTImporter::Import(Stmt *FromS) {
  if (!FromS)
    return nullptr;

  // Check whether we've already imported this declaration.
  llvm::DenseMap<Stmt *, Stmt *>::iterator Pos = ImportedStmts.find(FromS);
  if (Pos != ImportedStmts.end())
    return Pos->second;

  // Import the type
  ASTNodeImporter Importer(*this);
  Stmt *ToS = Importer.Visit(FromS);
  if (!ToS)
    return nullptr;

  // Record the imported declaration.
  ImportedStmts[FromS] = ToS;
  return ToS;
}

NestedNameSpecifier *ASTImporter::Import(NestedNameSpecifier *FromNNS) {
  if (!FromNNS)
    return nullptr;

  NestedNameSpecifier *prefix = Import(FromNNS->getPrefix());

  switch (FromNNS->getKind()) {
  case NestedNameSpecifier::Identifier:
    if (IdentifierInfo *II = Import(FromNNS->getAsIdentifier())) {
      return NestedNameSpecifier::Create(ToContext, prefix, II);
    }
    return nullptr;

  case NestedNameSpecifier::Namespace:
    if (auto *NS =
            cast_or_null<NamespaceDecl>(Import(FromNNS->getAsNamespace()))) {
      return NestedNameSpecifier::Create(ToContext, prefix, NS);
    }
    return nullptr;

  case NestedNameSpecifier::NamespaceAlias:
    if (auto *NSAD =
          cast_or_null<NamespaceAliasDecl>(Import(FromNNS->getAsNamespaceAlias()))) {
      return NestedNameSpecifier::Create(ToContext, prefix, NSAD);
    }
    return nullptr;

  case NestedNameSpecifier::Global:
    return NestedNameSpecifier::GlobalSpecifier(ToContext);

  case NestedNameSpecifier::Super:
    if (auto *RD =
            cast_or_null<CXXRecordDecl>(Import(FromNNS->getAsRecordDecl()))) {
      return NestedNameSpecifier::SuperSpecifier(ToContext, RD);
    }
    return nullptr;

  case NestedNameSpecifier::TypeSpec:
  case NestedNameSpecifier::TypeSpecWithTemplate: {
      QualType T = Import(QualType(FromNNS->getAsType(), 0u));
      if (!T.isNull()) {
        bool bTemplate = FromNNS->getKind() ==
                         NestedNameSpecifier::TypeSpecWithTemplate;
        return NestedNameSpecifier::Create(ToContext, prefix,
                                           bTemplate, T.getTypePtr());
      }
    }
      return nullptr;
  }

  llvm_unreachable("Invalid nested name specifier kind");
}

NestedNameSpecifierLoc ASTImporter::Import(NestedNameSpecifierLoc FromNNS) {
  // Copied from NestedNameSpecifier mostly.
  SmallVector<NestedNameSpecifierLoc , 8> NestedNames;
  NestedNameSpecifierLoc NNS = FromNNS;

  // Push each of the nested-name-specifiers's onto a stack for
  // serialization in reverse order.
  while (NNS) {
    NestedNames.push_back(NNS);
    NNS = NNS.getPrefix();
  }

  NestedNameSpecifierLocBuilder Builder;

  while (!NestedNames.empty()) {
    NNS = NestedNames.pop_back_val();
    NestedNameSpecifier *Spec = Import(NNS.getNestedNameSpecifier());
    if (!Spec)
      return NestedNameSpecifierLoc();

    NestedNameSpecifier::SpecifierKind Kind = Spec->getKind();
    switch (Kind) {
    case NestedNameSpecifier::Identifier:
      Builder.Extend(getToContext(),
                     Spec->getAsIdentifier(),
                     Import(NNS.getLocalBeginLoc()),
                     Import(NNS.getLocalEndLoc()));
      break;

    case NestedNameSpecifier::Namespace:
      Builder.Extend(getToContext(),
                     Spec->getAsNamespace(),
                     Import(NNS.getLocalBeginLoc()),
                     Import(NNS.getLocalEndLoc()));
      break;

    case NestedNameSpecifier::NamespaceAlias:
      Builder.Extend(getToContext(),
                     Spec->getAsNamespaceAlias(),
                     Import(NNS.getLocalBeginLoc()),
                     Import(NNS.getLocalEndLoc()));
      break;

    case NestedNameSpecifier::TypeSpec:
    case NestedNameSpecifier::TypeSpecWithTemplate: {
      TypeSourceInfo *TSI = getToContext().getTrivialTypeSourceInfo(
            QualType(Spec->getAsType(), 0));
      Builder.Extend(getToContext(),
                     Import(NNS.getLocalBeginLoc()),
                     TSI->getTypeLoc(),
                     Import(NNS.getLocalEndLoc()));
      break;
    }

    case NestedNameSpecifier::Global:
      Builder.MakeGlobal(getToContext(), Import(NNS.getLocalBeginLoc()));
      break;

    case NestedNameSpecifier::Super: {
      SourceRange ToRange = Import(NNS.getSourceRange());
      Builder.MakeSuper(getToContext(),
                        Spec->getAsRecordDecl(),
                        ToRange.getBegin(),
                        ToRange.getEnd());
    }
  }
  }

  return Builder.getWithLocInContext(getToContext());
}

TemplateName ASTImporter::Import(TemplateName From) {
  switch (From.getKind()) {
  case TemplateName::Template:
    if (auto *ToTemplate =
            cast_or_null<TemplateDecl>(Import(From.getAsTemplateDecl())))
      return TemplateName(ToTemplate);

    return {};

  case TemplateName::OverloadedTemplate: {
    OverloadedTemplateStorage *FromStorage = From.getAsOverloadedTemplate();
    UnresolvedSet<2> ToTemplates;
    for (auto *I : *FromStorage) {
      if (auto *To = cast_or_null<NamedDecl>(Import(I)))
        ToTemplates.addDecl(To);
      else
        return {};
    }
    return ToContext.getOverloadedTemplateName(ToTemplates.begin(),
                                               ToTemplates.end());
  }

  case TemplateName::QualifiedTemplate: {
    QualifiedTemplateName *QTN = From.getAsQualifiedTemplateName();
    NestedNameSpecifier *Qualifier = Import(QTN->getQualifier());
    if (!Qualifier)
      return {};

    if (auto *ToTemplate =
            cast_or_null<TemplateDecl>(Import(From.getAsTemplateDecl())))
      return ToContext.getQualifiedTemplateName(Qualifier,
                                                QTN->hasTemplateKeyword(),
                                                ToTemplate);

    return {};
  }

  case TemplateName::DependentTemplate: {
    DependentTemplateName *DTN = From.getAsDependentTemplateName();
    NestedNameSpecifier *Qualifier = Import(DTN->getQualifier());
    if (!Qualifier)
      return {};

    if (DTN->isIdentifier()) {
      return ToContext.getDependentTemplateName(Qualifier,
                                                Import(DTN->getIdentifier()));
    }

    return ToContext.getDependentTemplateName(Qualifier, DTN->getOperator());
  }

  case TemplateName::SubstTemplateTemplateParm: {
    SubstTemplateTemplateParmStorage *subst
      = From.getAsSubstTemplateTemplateParm();
    auto *param =
        cast_or_null<TemplateTemplateParmDecl>(Import(subst->getParameter()));
    if (!param)
      return {};

    TemplateName replacement = Import(subst->getReplacement());
    if (replacement.isNull())
      return {};

    return ToContext.getSubstTemplateTemplateParm(param, replacement);
  }

  case TemplateName::SubstTemplateTemplateParmPack: {
    SubstTemplateTemplateParmPackStorage *SubstPack
      = From.getAsSubstTemplateTemplateParmPack();
    auto *Param =
        cast_or_null<TemplateTemplateParmDecl>(
            Import(SubstPack->getParameterPack()));
    if (!Param)
      return {};

    ASTNodeImporter Importer(*this);
    TemplateArgument ArgPack
      = Importer.ImportTemplateArgument(SubstPack->getArgumentPack());
    if (ArgPack.isNull())
      return {};

    return ToContext.getSubstTemplateTemplateParmPack(Param, ArgPack);
  }
  }

  llvm_unreachable("Invalid template name kind");
}

SourceLocation ASTImporter::Import(SourceLocation FromLoc) {
  if (FromLoc.isInvalid())
    return {};

  SourceManager &FromSM = FromContext.getSourceManager();

  std::pair<FileID, unsigned> Decomposed = FromSM.getDecomposedLoc(FromLoc);
  FileID ToFileID = Import(Decomposed.first);
  if (ToFileID.isInvalid())
    return {};
  SourceManager &ToSM = ToContext.getSourceManager();
  return ToSM.getComposedLoc(ToFileID, Decomposed.second);
}

SourceRange ASTImporter::Import(SourceRange FromRange) {
  return SourceRange(Import(FromRange.getBegin()), Import(FromRange.getEnd()));
}

FileID ASTImporter::Import(FileID FromID) {
  llvm::DenseMap<FileID, FileID>::iterator Pos = ImportedFileIDs.find(FromID);
  if (Pos != ImportedFileIDs.end())
    return Pos->second;

  SourceManager &FromSM = FromContext.getSourceManager();
  SourceManager &ToSM = ToContext.getSourceManager();
  const SrcMgr::SLocEntry &FromSLoc = FromSM.getSLocEntry(FromID);

  // Map the FromID to the "to" source manager.
  FileID ToID;
  if (FromSLoc.isExpansion()) {
    const SrcMgr::ExpansionInfo &FromEx = FromSLoc.getExpansion();
    SourceLocation ToSpLoc = Import(FromEx.getSpellingLoc());
    SourceLocation ToExLocS = Import(FromEx.getExpansionLocStart());
    unsigned TokenLen = FromSM.getFileIDSize(FromID);
    SourceLocation MLoc;
    if (FromEx.isMacroArgExpansion()) {
      MLoc = ToSM.createMacroArgExpansionLoc(ToSpLoc, ToExLocS, TokenLen);
    } else {
      SourceLocation ToExLocE = Import(FromEx.getExpansionLocEnd());
      MLoc = ToSM.createExpansionLoc(ToSpLoc, ToExLocS, ToExLocE, TokenLen,
                                     FromEx.isExpansionTokenRange());
    }
    ToID = ToSM.getFileID(MLoc);
  } else {
    // Include location of this file.
    SourceLocation ToIncludeLoc = Import(FromSLoc.getFile().getIncludeLoc());

    const SrcMgr::ContentCache *Cache = FromSLoc.getFile().getContentCache();
    if (Cache->OrigEntry && Cache->OrigEntry->getDir()) {
      // FIXME: We probably want to use getVirtualFile(), so we don't hit the
      // disk again
      // FIXME: We definitely want to re-use the existing MemoryBuffer, rather
      // than mmap the files several times.
      const FileEntry *Entry =
          ToFileManager.getFile(Cache->OrigEntry->getName());
      if (!Entry)
        return {};
      ToID = ToSM.createFileID(Entry, ToIncludeLoc,
                               FromSLoc.getFile().getFileCharacteristic());
    } else {
      // FIXME: We want to re-use the existing MemoryBuffer!
      const llvm::MemoryBuffer *FromBuf =
          Cache->getBuffer(FromContext.getDiagnostics(), FromSM);
      std::unique_ptr<llvm::MemoryBuffer> ToBuf =
          llvm::MemoryBuffer::getMemBufferCopy(FromBuf->getBuffer(),
                                               FromBuf->getBufferIdentifier());
      ToID = ToSM.createFileID(std::move(ToBuf),
                               FromSLoc.getFile().getFileCharacteristic());
    }
  }

  ImportedFileIDs[FromID] = ToID;
  return ToID;
}

CXXCtorInitializer *ASTImporter::Import(CXXCtorInitializer *From) {
  Expr *ToExpr = Import(From->getInit());
  if (!ToExpr && From->getInit())
    return nullptr;

  if (From->isBaseInitializer()) {
    TypeSourceInfo *ToTInfo = Import(From->getTypeSourceInfo());
    if (!ToTInfo && From->getTypeSourceInfo())
      return nullptr;

    return new (ToContext) CXXCtorInitializer(
        ToContext, ToTInfo, From->isBaseVirtual(), Import(From->getLParenLoc()),
        ToExpr, Import(From->getRParenLoc()),
        From->isPackExpansion() ? Import(From->getEllipsisLoc())
                                : SourceLocation());
  } else if (From->isMemberInitializer()) {
    auto *ToField = cast_or_null<FieldDecl>(Import(From->getMember()));
    if (!ToField && From->getMember())
      return nullptr;

    return new (ToContext) CXXCtorInitializer(
        ToContext, ToField, Import(From->getMemberLocation()),
        Import(From->getLParenLoc()), ToExpr, Import(From->getRParenLoc()));
  } else if (From->isIndirectMemberInitializer()) {
    auto *ToIField = cast_or_null<IndirectFieldDecl>(
        Import(From->getIndirectMember()));
    if (!ToIField && From->getIndirectMember())
      return nullptr;

    return new (ToContext) CXXCtorInitializer(
        ToContext, ToIField, Import(From->getMemberLocation()),
        Import(From->getLParenLoc()), ToExpr, Import(From->getRParenLoc()));
  } else if (From->isDelegatingInitializer()) {
    TypeSourceInfo *ToTInfo = Import(From->getTypeSourceInfo());
    if (!ToTInfo && From->getTypeSourceInfo())
      return nullptr;

    return new (ToContext)
        CXXCtorInitializer(ToContext, ToTInfo, Import(From->getLParenLoc()),
                           ToExpr, Import(From->getRParenLoc()));
  } else {
    return nullptr;
  }
}

CXXBaseSpecifier *ASTImporter::Import(const CXXBaseSpecifier *BaseSpec) {
  auto Pos = ImportedCXXBaseSpecifiers.find(BaseSpec);
  if (Pos != ImportedCXXBaseSpecifiers.end())
    return Pos->second;

  CXXBaseSpecifier *Imported = new (ToContext) CXXBaseSpecifier(
        Import(BaseSpec->getSourceRange()),
        BaseSpec->isVirtual(), BaseSpec->isBaseOfClass(),
        BaseSpec->getAccessSpecifierAsWritten(),
        Import(BaseSpec->getTypeSourceInfo()),
        Import(BaseSpec->getEllipsisLoc()));
  ImportedCXXBaseSpecifiers[BaseSpec] = Imported;
  return Imported;
}

void ASTImporter::ImportDefinition(Decl *From) {
  Decl *To = Import(From);
  if (!To)
    return;

  if (auto *FromDC = cast<DeclContext>(From)) {
    ASTNodeImporter Importer(*this);

    if (auto *ToRecord = dyn_cast<RecordDecl>(To)) {
      if (!ToRecord->getDefinition()) {
        Importer.ImportDefinition(cast<RecordDecl>(FromDC), ToRecord,
                                  ASTNodeImporter::IDK_Everything);
        return;
      }
    }

    if (auto *ToEnum = dyn_cast<EnumDecl>(To)) {
      if (!ToEnum->getDefinition()) {
        Importer.ImportDefinition(cast<EnumDecl>(FromDC), ToEnum,
                                  ASTNodeImporter::IDK_Everything);
        return;
      }
    }

    if (auto *ToIFace = dyn_cast<ObjCInterfaceDecl>(To)) {
      if (!ToIFace->getDefinition()) {
        Importer.ImportDefinition(cast<ObjCInterfaceDecl>(FromDC), ToIFace,
                                  ASTNodeImporter::IDK_Everything);
        return;
      }
    }

    if (auto *ToProto = dyn_cast<ObjCProtocolDecl>(To)) {
      if (!ToProto->getDefinition()) {
        Importer.ImportDefinition(cast<ObjCProtocolDecl>(FromDC), ToProto,
                                  ASTNodeImporter::IDK_Everything);
        return;
      }
    }

    Importer.ImportDeclContext(FromDC, true);
  }
}

DeclarationName ASTImporter::Import(DeclarationName FromName) {
  if (!FromName)
    return {};

  switch (FromName.getNameKind()) {
  case DeclarationName::Identifier:
    return Import(FromName.getAsIdentifierInfo());

  case DeclarationName::ObjCZeroArgSelector:
  case DeclarationName::ObjCOneArgSelector:
  case DeclarationName::ObjCMultiArgSelector:
    return Import(FromName.getObjCSelector());

  case DeclarationName::CXXConstructorName: {
    QualType T = Import(FromName.getCXXNameType());
    if (T.isNull())
      return {};

    return ToContext.DeclarationNames.getCXXConstructorName(
                                               ToContext.getCanonicalType(T));
  }

  case DeclarationName::CXXDestructorName: {
    QualType T = Import(FromName.getCXXNameType());
    if (T.isNull())
      return {};

    return ToContext.DeclarationNames.getCXXDestructorName(
                                               ToContext.getCanonicalType(T));
  }

  case DeclarationName::CXXDeductionGuideName: {
    auto *Template = cast_or_null<TemplateDecl>(
        Import(FromName.getCXXDeductionGuideTemplate()));
    if (!Template)
      return {};
    return ToContext.DeclarationNames.getCXXDeductionGuideName(Template);
  }

  case DeclarationName::CXXConversionFunctionName: {
    QualType T = Import(FromName.getCXXNameType());
    if (T.isNull())
      return {};

    return ToContext.DeclarationNames.getCXXConversionFunctionName(
                                               ToContext.getCanonicalType(T));
  }

  case DeclarationName::CXXOperatorName:
    return ToContext.DeclarationNames.getCXXOperatorName(
                                          FromName.getCXXOverloadedOperator());

  case DeclarationName::CXXLiteralOperatorName:
    return ToContext.DeclarationNames.getCXXLiteralOperatorName(
                                   Import(FromName.getCXXLiteralIdentifier()));

//ASUTTON ADDN:
  case DeclarationName::CXXIdExprName:
    llvm_unreachable("unimplemented");
//END

  case DeclarationName::CXXUsingDirective:
    // FIXME: STATICS!
    return DeclarationName::getUsingDirectiveName();
  }

  llvm_unreachable("Invalid DeclarationName Kind!");
}

IdentifierInfo *ASTImporter::Import(const IdentifierInfo *FromId) {
  if (!FromId)
    return nullptr;

  IdentifierInfo *ToId = &ToContext.Idents.get(FromId->getName());

  if (!ToId->getBuiltinID() && FromId->getBuiltinID())
    ToId->setBuiltinID(FromId->getBuiltinID());

  return ToId;
}

Selector ASTImporter::Import(Selector FromSel) {
  if (FromSel.isNull())
    return {};

  SmallVector<IdentifierInfo *, 4> Idents;
  Idents.push_back(Import(FromSel.getIdentifierInfoForSlot(0)));
  for (unsigned I = 1, N = FromSel.getNumArgs(); I < N; ++I)
    Idents.push_back(Import(FromSel.getIdentifierInfoForSlot(I)));
  return ToContext.Selectors.getSelector(FromSel.getNumArgs(), Idents.data());
}

DeclarationName ASTImporter::HandleNameConflict(DeclarationName Name,
                                                DeclContext *DC,
                                                unsigned IDNS,
                                                NamedDecl **Decls,
                                                unsigned NumDecls) {
  return Name;
}

DiagnosticBuilder ASTImporter::ToDiag(SourceLocation Loc, unsigned DiagID) {
  if (LastDiagFromFrom)
    ToContext.getDiagnostics().notePriorDiagnosticFrom(
      FromContext.getDiagnostics());
  LastDiagFromFrom = false;
  return ToContext.getDiagnostics().Report(Loc, DiagID);
}

DiagnosticBuilder ASTImporter::FromDiag(SourceLocation Loc, unsigned DiagID) {
  if (!LastDiagFromFrom)
    FromContext.getDiagnostics().notePriorDiagnosticFrom(
      ToContext.getDiagnostics());
  LastDiagFromFrom = true;
  return FromContext.getDiagnostics().Report(Loc, DiagID);
}

void ASTImporter::CompleteDecl (Decl *D) {
  if (auto *ID = dyn_cast<ObjCInterfaceDecl>(D)) {
    if (!ID->getDefinition())
      ID->startDefinition();
  }
  else if (auto *PD = dyn_cast<ObjCProtocolDecl>(D)) {
    if (!PD->getDefinition())
      PD->startDefinition();
  }
  else if (auto *TD = dyn_cast<TagDecl>(D)) {
    if (!TD->getDefinition() && !TD->isBeingDefined()) {
      TD->startDefinition();
      TD->setCompleteDefinition(true);
    }
  }
  else {
    assert(0 && "CompleteDecl called on a Decl that can't be completed");
  }
}

Decl *ASTImporter::MapImported(Decl *From, Decl *To) {
  llvm::DenseMap<Decl *, Decl *>::iterator Pos = ImportedDecls.find(From);
  assert((Pos == ImportedDecls.end() || Pos->second == To) &&
      "Try to import an already imported Decl");
  if (Pos != ImportedDecls.end())
    return Pos->second;
  ImportedDecls[From] = To;
  return To;
}

bool ASTImporter::IsStructurallyEquivalent(QualType From, QualType To,
                                           bool Complain) {
  llvm::DenseMap<const Type *, const Type *>::iterator Pos
   = ImportedTypes.find(From.getTypePtr());
  if (Pos != ImportedTypes.end() && ToContext.hasSameType(Import(From), To))
    return true;

  StructuralEquivalenceContext Ctx(FromContext, ToContext, NonEquivalentDecls,
                                   getStructuralEquivalenceKind(*this), false,
                                   Complain);
  return Ctx.IsEquivalent(From, To);
}
