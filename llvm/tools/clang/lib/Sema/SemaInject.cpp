//ASUTTON ADDN (+ DWR ADDN: APValueCharIter, ApplyExpand, plus modified ApplyEffects)
//===--- SemaInject.cpp - Semantic Analysis for Injection -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic rules for the injection of declarations into
//  various declarative contexts.
//
//===----------------------------------------------------------------------===//

#include <clang/Parse/CDContextVars.h>
#include "TreeTransform.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/ExprCXX.h"
#include "clang/Lex/MetaparseDebug.h" //DWR ADDN
#include "clang/Parse/ParseDiagnostic.h" //DWR ADDN
#include "clang/Parse/CDContextVars.h" //DWR ADDN
#include "clang/Parse/Parser.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Template.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/Lex/APValueCharIter.h"

//#include "ReflectedTypeEtc.h" //DWR ADDN


//#include "clang/Lex/HeaderSearch.h" //DWR ADDN (perhaps needed to build new Preprocessor, not actually used)
//#include "clang/Lex/HeaderSearchOptions.h" //DWR ADDN ""
//#include "clang/Sema/CXXFieldCollector.h" //DWR ADDN

using namespace clang;
using namespace sema;

//DWR ADDN:
void Sema::TheParserEnterScope(unsigned int ScopeFlags) {
  TheParser->EnterScope(ScopeFlags);
}
void Sema::TheParserExitScope() {
  TheParser->ExitScope();
}
//END

////ASUTTON ADDN:
//namespace clang {
//
///// \brief A compile-time value along with its type.
//struct TypedValue
//{
//  TypedValue(QualType T, const APValue& V)
//    : Type(T), Value(V)
//  { }
//
//  QualType Type;
//  APValue Value;
//};
//
///// Records information about a definition inside a fragment that must be
///// processed later. These are typically fields and methods.
//struct InjectedDef
//{
//  InjectedDef(Decl *F, Decl *I) : Fragment(F), Injected(I) { }
//
//  /// The declaration within the fragment.
//  Decl *Fragment;
//
//  /// The injected declaration.
//  Decl *Injected;
//};
//
//
//enum AccessModifier { NoAccess, Public, Private, Protected, Default };
//enum StorageModifier { NoStorage, Static, Automatic, ThreadLocal };
//
///// Modifiers used for cloning definitions. When the values of modifiers are
///// non-zero they indicate that the property should be specified. Modifiers
///// can never remove specifiers.
//struct DeclModifiers
//{
//  DeclModifiers()
//    : Access(), Storage(), Constexpr(), Virtual(), Pure(), Name()
//  { }
//
//  // This is the traits layout in the library:
//  //
//  //    linkage_kind new_linkage : 2;
//  //    access_kind new_access : 2;
//  //    storage_kind new_storage : 2;
//  //    bool make_constexpr : 1;
//  //    bool make_virtual : 1;
//  //    bool make_pure : 1;
//  //
//  // Note that we don't currently use the linkage spec.
//  DeclModifiers(const APValue& Traits)
//    : Access((AccessModifier)Traits.getStructField(1).getInt().getExtValue()),
//      Storage((StorageModifier)Traits.getStructField(2).getInt().getExtValue()),
//      Constexpr(Traits.getStructField(3).getInt().getExtValue()),
//      Virtual(Traits.getStructField(4).getInt().getExtValue()),
//      Pure(Traits.getStructField(5).getInt().getExtValue())
//  {
//    assert(Storage != Automatic && "Can't make declarations automatic");
//    assert(Storage != ThreadLocal && "Thread local storage not implemented");
//  }
//
//  // Returns true if access is modified.
//  bool modifyAccess() const { return Access != NoAccess; }
//
//  // Returns the modified access specifier.
//  AccessSpecifier getAccess() {
//    switch (Access) {
//    case NoAccess:
//      llvm_unreachable("No access specifier");
//    case Public:
//      return AS_public;
//    case Private:
//      return AS_private;
//    case Protected:
//      return AS_protected;
//    default:
//      // FIXME: What does Default mean?
//      llvm_unreachable("Invalid access specifier");
//    }
//  }
//
//  // Returns true if access is modified.
//  bool modifyStorage() const { return Storage != NoStorage; }
//
//  // Returns the modified storage specifier.
//  StorageClass getStorage() {
//    switch (Storage) {
//    case NoStorage:
//      llvm_unreachable("No storage specifier");
//    case Static:
//      return SC_Static;
//    default:
//      // FIXME: We're obviously missing some thing.
//      llvm_unreachable("Invalid storage specifier");
//    }
//  }
//
//  // If true, add the constexpr specifier.
//  bool addConstexpr() const { return Constexpr; }
//
//  // If true, add the virtual specifier.
//  bool addVirtual() const { return Virtual; }
//
//  // If true, add the pure virtual specifier.
//  bool addPureVirtual() const { return Pure; }
//
//  /// True if a rename is requested.
//  bool hasRename() const {
//    return !Name.isUninit();
//  }
//
//  /// Returns the string that will serve as the new name.
//  std::string getNewName() const {
//    assert(Name.isLValue());
//    APValue::LValueBase Base = Name.getLValueBase();
//    const Expr *E = Base.get<const Expr *>();
//    assert(isa<StringLiteral>(E));
//    const StringLiteral *StrLit = cast<StringLiteral>(E);
//    return StrLit->getString();
//  }
//
//  void dump() const;
//
//  AccessModifier Access;
//  StorageModifier Storage;
//  bool Constexpr;
//  bool Virtual;
//  bool Pure;
//  APValue Name;
//};
//
//void
//DeclModifiers::dump() const
//{
//  llvm::errs() << "Access: ";
//  switch (Access) {
//  case NoAccess:
//    llvm::errs() << "none\n";
//    break;
//  case Public:
//    llvm::errs() << "public\n";
//    break;
//  case Private:
//    llvm::errs() << "private\n";
//    break;
//  case Protected:
//    llvm::errs() << "protected\n";
//    break;
//  case Default:
//    llvm::errs() << "default\n";
//    break;
//  }
//  llvm::errs() << "Storage: ";
//  switch (Storage) {
//    case NoStorage:
//    llvm::errs() << "none\n";
//    break;
//  case Static:
//    llvm::errs() << "static\n";
//    break;
//  case Automatic:
//    llvm::errs() << "auto\n";
//    break;
//  case ThreadLocal:
//    llvm::errs() << "thread\n";
//    break;
//  }
//
//  llvm::errs() << "Constexpr: " << (Constexpr ? "yes" : "no") << '\n';
//  llvm::errs() << "Virtual: " << (Virtual ? "yes" : "no") << '\n';
//  llvm::errs() << "Pure: " << (Pure ? "yes" : "no") << '\n';
//
//  // FIXME: Actually fetch the name from the value.
//  llvm::errs() << "Renamed? " << (Name.isUninit() ? "no" : "yes");
//}
//
//class InjectionContext;
///// \brief An injection context. This is declared to establish a set of
///// substitutions during an injection.
//class InjectionContext : public TreeTransform<InjectionContext>
//{
//  using Base = TreeTransform<InjectionContext>;
//public:
//////ASUTTON ADDN (note this one has the Frag parameter)
////  /// Initialize the context for injecting a fragment.
////  InjectionContext(Sema &SemaRef,
////                   CXXFragmentDecl *Frag,
////                   DeclContext *Injectee,
////                   Decl *Injection)
////      : Base(SemaRef), Prev(SemaRef.CurrentInjectionContext),
//////        Fragment(Frag), //ASUTTON ADDN
////        Injectee(Injectee), Injection(Injection), Modifiers() {
////    getSema().CurrentInjectionContext = this;
////  }
//////END
//
//  /// Initialize the context for relocating a declaration.
//  InjectionContext(Sema &SemaRef,
//                   DeclContext *Injectee,
//                   Decl *Injection)
//    : Base(SemaRef), Prev(SemaRef.CurrentInjectionContext),
////      Fragment(), //ASUTTON ADDN
//      Injectee(Injectee), Injection(Injection), Modifiers() {
//   getSema().CurrentInjectionContext = this;
//  }
//
//  ~InjectionContext() {
//    if (Prev != (InjectionContext *)0x1)
//      getSema().CurrentInjectionContext = Prev;
//  }
//
//  ASTContext &getContext() { return getSema().Context; }
//
//  /// Injection always builds new trees.
//  bool AlwaysRebuild() const { return true; }
//
//  /// We are injecting code.
//  bool InjectingCode() const { return true; }
//
//  /// Detach the context from the semantics object. Returns this object for
//  /// convenience.
//  InjectionContext *Detach() {
//    getSema().CurrentInjectionContext = Prev;
//    Prev = (InjectionContext *)0x1;
//
//    // Reset the declaration modifiers. They're already been applied and
//    // must not apply to nested declarations in a definition.
//    Modifiers = DeclModifiers();
//
//    return this;
//  }
//
//  /// Re-attach the context to the context stack.
//  void Attach() {
//    assert((Prev == (InjectionContext *)0x1) && "Context not detached");
//    Prev = getSema().CurrentInjectionContext;
//    getSema().CurrentInjectionContext = this;
//  }
//
//  /// \brief Adds a substitution from one declaration to another.
//  void AddDeclSubstitution(Decl *Old, Decl *New) {
//    assert(TransformedLocalDecls.count(Old) == 0 && "Overwriting substitution");
//    transformedLocalDecl(Old, New);
//  }
//
//  /// \brief Adds a substitution from a fragment placeholder to its
//  /// (type) constant value.
//  void AddPlaceholderSubstitution(Decl *Orig, QualType T, const APValue &V) {
//    assert(isa<VarDecl>(Orig) && "Expected a variable declaration");
//    assert(PlaceholderSubsts.count(Orig) == 0 && "Overwriting substitution");
//    PlaceholderSubsts.try_emplace(Orig, T, V);
//  }
//
//////ASUTTON ADDN:
////  /// \brief Adds substitutions for each placeholder in the fragment.
////  /// The types and values are sourced from the fields of the reflection
////  /// class and the captured values.
////  void AddPlaceholderSubstitutions(DeclContext *Fragment,
////                                   CXXRecordDecl *Reflection,
////                                   ArrayRef<APValue> Captures) {
////    assert(isa<CXXFragmentDecl>(Fragment) && "Context is not a fragment");
////    auto FieldIter = Reflection->field_begin();
////    auto PlaceIter = Fragment->decls_begin();
////    for (std::size_t I = 0; I < Captures.size(); ++I) {
////      Decl *Var = *PlaceIter++;
////      QualType Ty = (*FieldIter++)->getType();
////      const APValue &Val = Captures[I];
////      AddPlaceholderSubstitution(Var, Ty, Val);
////    }
////  }
//////END
//
//  /// Returns a replacement for D if a substitution has been registered or
//  /// nullptr if no such replacement exists.
//  Decl *GetDeclReplacement(Decl *D) {
//    auto Iter = TransformedLocalDecls.find(D);
//    if (Iter != TransformedLocalDecls.end())
//      return Iter->second;
//    else
//      return nullptr;
//  }
//
//  /// Returns a replacement expression if E refers to a placeholder.
//  Expr *GetPlaceholderReplacement(DeclRefExpr *E) {
//    auto Iter = PlaceholderSubsts.find(E->getDecl());
//    if (Iter != PlaceholderSubsts.end()) {
//      // Build a new constant expression as the replacement. The source
//      // expression is opaque since the actual declaration isn't part of
//      // the output AST (but we might want it as context later -- makes
//      // pretty printing more elegant).
//      const TypedValue &TV = Iter->second;
//      Expr *Opaque = new (getContext()) OpaqueValueExpr(
//          E->getLocation(), TV.Type, VK_RValue, OK_Ordinary, E);
//      return new (getContext()) CXXConstantExpr(Opaque, TV.Value);
//    } else {
//      return nullptr;
//    }
//  }
//
//  /// Returns the declaration for the injectee.
//  Decl *GetInjecteeDecl() const { return Decl::castFromDeclContext(Injectee); }
//
//  /// Returns true if D is within an injected fragment or cloned declaration.
//  bool IsInInjection(Decl *D);
//
//  /// Sets the declaration modifiers.
//  void setModifiers(const APValue& Traits) { Modifiers = DeclModifiers(Traits); }
//
//  /// Sets the modified name of the declaration. If Name represents the null
//  /// pointer, then this has no effect.
//  void setName(const APValue& Name) {
//  if (!Name.isNullPointer())
//    Modifiers.Name = Name;
//  }
//
//  /// True if a rename is requested.
//  bool hasRename() const { return Modifiers.hasRename(); }
//
//  DeclarationName applyRename() {
//    std::string Str = Modifiers.getNewName();
//    IdentifierInfo *II = &SemaRef.Context.Idents.get(Str);
//
//    // Reset the rename, so that it applies once, at the top level
//    // of the injection (hopefully).
//    //
//    // FIXME: This is a sign of some fragility. We'd like the rename to
//    // associate only with the fragment/decl we're replaying. This is
//    // true of other modifiers also.
//    Modifiers.Name = APValue();
//
//    return DeclarationName(II);
//  }
//
//  /// Suppresses modifiers for nested declarations.
//  struct SuppressModifiersRAII
//  {
//    SuppressModifiersRAII(InjectionContext &Cxt)
//      : Context(Cxt), Mods(Cxt.Modifiers)
//    {
//      Context.Modifiers = DeclModifiers();
//    }
//    ~SuppressModifiersRAII()
//    {
//      Context.Modifiers = Mods;
//    }
//    InjectionContext &Context;
//    DeclModifiers Mods;
//  };
//
//  // TreeTransform Overloads
//
//  DeclarationNameInfo TransformDeclarationName(NamedDecl *ND) {
//    DeclarationNameInfo DNI(ND->getDeclName(), ND->getLocation());
//    return TransformDeclarationNameInfo(DNI);
//  }
//
//  DeclarationNameInfo TransformDeclarationNameInfo(DeclarationNameInfo DNI) {
//    if (hasRename())
//      DNI = DeclarationNameInfo(applyRename(), DNI.getLoc());
//
//    return Base::TransformDeclarationNameInfo(DNI);
//  }
//
//  Decl *TransformDecl(SourceLocation Loc, Decl *D);
//  Decl *TransformDefinition(SourceLocation Loc, Decl *D);
//
//  ValueDecl *LookupDecl(NestedNameSpecifierLoc NNS, DeclarationNameInfo DNI);
//  ValueDecl *LookupMember(NestedNameSpecifierLoc NNS, DeclarationNameInfo DNI);
//
//  ExprResult TransformDeclRefExpr(DeclRefExpr *E);
//
//  // Declaration injection
//
//  bool InjectDeclarator(DeclaratorDecl *D,
//                        DeclarationNameInfo &DNI,
//                        TypeSourceInfo *&TSI);
//  bool InjectMemberDeclarator(DeclaratorDecl *D,
//                              DeclarationNameInfo &DNI,
//                              TypeSourceInfo *&TSI,
//                              CXXRecordDecl *&Owner);
//
//  TemplateParameterList *InjectTemplateParms(TemplateParameterList *Old);
//
//  void UpdateFunctionParms(FunctionDecl *Old, FunctionDecl *New);
//
//  Decl *InjectDeclImpl(Decl* D);
//  Decl *InjectDecl(Decl *D);
//  Decl *InjectNamespaceDecl(NamespaceDecl* D);
//  Decl *InjectEnumDecl(EnumDecl *D);
//  Decl *InjectEnumConstantDecl(EnumConstantDecl *D);
//  Decl *InjectTypedefNameDecl(TypedefNameDecl *D);
//  Decl *InjectFunctionDecl(FunctionDecl *D);
//  Decl *InjectVarDecl(VarDecl *D);
//  Decl *InjectParmVarDecl(ParmVarDecl *D);
//  Decl *InjectCXXRecordDecl(CXXRecordDecl *D);
//  Decl *InjectStaticDataMemberDecl(FieldDecl *D);
//  Decl *InjectFieldDecl(FieldDecl *D);
//  Decl *InjectCXXMethodDecl(CXXMethodDecl *D);
//  Decl *InjectAccessSpecDecl(AccessSpecDecl *D);
//  Decl *InjectConstexprDecl(ConstexprDecl *D);
//  Decl *InjectFunctionTemplateDecl(FunctionTemplateDecl *D);
//  Decl *InjectTemplateTypeParmDecl(TemplateTypeParmDecl *D);
//
//  // Members
//
//  /// \brief The previous injection context.
//  InjectionContext *Prev;
//
//////ASUTTON ADDN:
////  /// \brief The fragment being injected.
////  CXXFragmentDecl *Fragment;
//////END
//
//  /// \brief The context into which the fragment is injected
//  DeclContext *Injectee;
//
//  /// \brief The declaration being Injected.
//  Decl *Injection;
//
//  /// \brief Attached to an injection context to modify specifiers
//  /// when cloning a definition.
//  DeclModifiers Modifiers;
//
//  /// \brief A mapping of fragment placeholders to their typed compile-time
//  /// values. This is used by TreeTransformer to replace references with
//  /// constant expressions.
//  llvm::DenseMap<Decl *, TypedValue> PlaceholderSubsts;
//
//  /// \brief A mapping of injected parameters to their corresponding
//  /// expansions.
//  llvm::DenseMap<ParmVarDecl *, SmallVector<ParmVarDecl *, 4>> InjectedParms;
//
//  SmallVectorImpl<ParmVarDecl *> *GetInjectedParameterPack(ParmVarDecl *P) {
//    return &InjectedParms[P];
//  }
//
//  SmallVectorImpl<ParmVarDecl *> *FindInjectedParameterPack(ParmVarDecl *P) {
//    auto Iter = InjectedParms.find(P);
//    if (Iter == InjectedParms.end())
//      return nullptr;
//    return &Iter->second;
//  }
//
//  /// \brief A list of declarations whose definitions have not yet been
//  /// injected. These are processed when a class receiving injections is
//  /// completed.
//  llvm::SmallVector<InjectedDef, 8> InjectedDefinitions;
//};
////END
//////ASUTTON ADDN:
////SmallVectorImpl<ParmVarDecl *> *
////Sema::GetInjectedParameterPack(ParmVarDecl *P)
////{
////  assert(CurrentInjectionContext);
////  return CurrentInjectionContext->GetInjectedParameterPack(P);
////}
////
////SmallVectorImpl<ParmVarDecl *> *
////Sema::FindInjectedParameterPack(ParmVarDecl *P)
////{
////  assert(CurrentInjectionContext);
////  return CurrentInjectionContext->FindInjectedParameterPack(P);
////}
////
//////END
////ASUTTON ADDN:
//bool InjectionContext::IsInInjection(Decl *D) {
//////ASUTTON ADDN:
////  // If this is actually a fragment, then we can check in the usual way.
////  if (Fragment)
////    return D->isInFragment();
//////END
//
//  // Otherwise, we're cloning a declaration, (not a fragment) but we need
//  // to ensure that any any declarations within that are injected.
//
//  // If D is injection source, then it must be injected.
//  if (D == Injection)
//    return true;
//
//  // If the injection is not a DC, then D cannot be in the injection because
//  // it could not have been declared within (e.g., if the injection is a
//  // variable).
//  DeclContext *Outer = dyn_cast<DeclContext>(Injection);
//  if (!Outer)
//    return false;
//
//  // Note that Injection->getDeclContext() == InjecteeDC. We don't want to
//  // use that as the outermost context since it includes declarations that
//  // should not be injected. That is, copying a member function does not
//  // mean that we are copying member variables of the same class.
//
//  // Otherwise, work outwards to see if D is in the Outermost context
//  // of the injection. If we reach the outermost scope, we're not inside
//  // an injection.
//  DeclContext *DC = D->getDeclContext();
//  while (DC) {
//    if (DC == Outer)
//      return true;
//    if (DC == Injectee)
//      return false;
//    DC = DC->getParent();
//  }
//  return false;
//}
//
//Decl* InjectionContext::TransformDecl(SourceLocation Loc, Decl *D) {
//  if (!D)
//    return nullptr;
//
//  // If we've EVER seen a replacement, then return that.
//  if (Decl *Repl = GetDeclReplacement(D))
//    return Repl;
//
//  // If D is part of the injection, then we must have seen a previous
//  // declaration. Otherwise, return nullptr and force a lookup or error.
//  //
//  // FIXME: This may not be valid for gotos and labels.
//  if (IsInInjection(D))
//    return nullptr;
//
//  // When copying existing declarations, if D is a member of the of the
//  // injection's declaration context, then we want to re-map that so that the
//  // result is a member of the injection. For example:
//  //
//  //    struct S {
//  //      int x;
//  //      int f() {
//  //        return x; // Not dependent, bound
//  //      }
//  //    };
//  //
//  //    struct T { constexpr { __generate $S::f; } };
//  //
//  // At the point that we inject S::f, the reference to x is not dependent,
//  // and therefore not subject to two-phase lookup. However, we would expect
//  // the reference to be to the T::x during injection.
//  //
//  // Note that this isn't necessary for fragments. We expect names to be
//  // written dependently there and subject to the usual name resolution
//  // rules.
//  //
//  // Defer the resolution to the caller so that the result can be interpreted
//  // within the context of the expression, not here.
//  if (
////          !Fragment && //ASUTTON ADDN
//          D->getDeclContext() == Injection->getDeclContext())
//    return nullptr;
//
//  return D;
//}
//
//Decl* InjectionContext::TransformDefinition(SourceLocation Loc, Decl *D) {
//  // Rebuild the by injecting it. This will apply substitutions to the type
//  // and initializer of the declaration.
//  return InjectDecl(D);
//}
//
//
//ExprResult InjectionContext::TransformDeclRefExpr(DeclRefExpr *E) {
//  if (Expr *R = GetPlaceholderReplacement(E))
//    return R;
//  else
//    return Base::TransformDeclRefExpr(E);
//}
//
//ValueDecl *InjectionContext::LookupDecl(NestedNameSpecifierLoc NNS,
//                                        DeclarationNameInfo DNI) {
//  return nullptr;
//}
//
//
//ValueDecl *InjectionContext::LookupMember(NestedNameSpecifierLoc NNS,
//                                          DeclarationNameInfo DNI) {
//  CXXScopeSpec SS;
//  SS.Adopt(NNS);
//  assert(SS.isEmpty() && "Qualified lookup of member not implemented");
//
//  // FIXME: What if we find an overload set or an ambiguous result?
//  // This may need to call ActOnMemberAccessExpr to completely rebuild
//  // the original expression. Same with LookupDecl.
//  LookupResult R(getSema(), DNI, Sema::LookupMemberName);
//  if (!getSema().LookupName(R, getSema().getCurScope())) {
//    getSema().DiagnoseEmptyLookup(getSema().getCurScope(), SS, R, nullptr);
//    return nullptr;
//  }
//  return R.getAsSingle<ValueDecl>();
//}
//
//static bool InjectEnumDefinition(InjectionContext &Cxt,
//                                 EnumDecl *OldEnum,
//                                 EnumDecl *NewEnum) {
//  SmallVector<Decl*, 4> Enumerators;
//
//  EnumConstantDecl *LastConst = nullptr;
//  for (auto *OldConst : OldEnum->enumerators()) {
//    // The specified value for the enumerator.
//    ExprResult Value;
//    if (Expr *OldValue = OldConst->getInitExpr()) {
//      // The enumerator's value expression is a constant expression.
//      EnterExpressionEvaluationContext Unevaluated(
//          Cxt.getSema(), Sema::ExpressionEvaluationContext::ConstantEvaluated);
//      Value = Cxt.TransformExpr(OldValue);
//    }
//
//    // Drop the initial value and continue.
//    bool Invalid = false;
//    if (Value.isInvalid()) {
//      Value = nullptr;
//      Invalid = true;
//    }
//
//    // Create the new enum.
//    EnumConstantDecl *Const = Cxt.getSema().CheckEnumConstant(
//        NewEnum, LastConst, OldConst->getLocation(),
//        OldConst->getIdentifier(), Value.get());
//    if (!Const) {
//      NewEnum->setInvalidDecl();
//      continue;
//    }
//    Cxt.AddDeclSubstitution(OldConst, Const);
//
//    if (Invalid) {
//      Const->setInvalidDecl();
//      NewEnum->setInvalidDecl();
//    }
//
//    Const->setAccess(OldConst->getAccess());
//    NewEnum->addDecl(Const);
//
//    Enumerators.push_back(Const);
//    LastConst = Const;
//  }
//
//  Cxt.getSema().ActOnEnumBody(
//      NewEnum->getLocation(), NewEnum->getBraceRange(), NewEnum,
//      Enumerators, /*Scope=*/nullptr, /*AttributeList=*/ParsedAttributesView()); //DWR NOTE: AttributeList=nullptr_t in ASutton's
//
//  return !NewEnum->isInvalidDecl();
//}
//
//Decl* InjectionContext::InjectNamespaceDecl(NamespaceDecl *D)
//{
//  DeclContext *Owner = getSema().CurContext;
//
//  // Build the namespace.
//  //
//  // FIXME: Search for a previous declaration of the namespace so that they
//  // can be stitched together (i.e., redo lookup).
//  NamespaceDecl *Ns = NamespaceDecl::Create(
//      getContext(), Owner, D->isInline(), D->getLocation(), D->getLocation(),
//      D->getIdentifier(), /*PrevDecl=*/nullptr);
//  AddDeclSubstitution(D, Ns);
//
//  Owner->addDecl(Ns);
//
//  // Inject the namespace members.
//  Sema::ContextRAII NsCxt(getSema(), Ns);
//  for (Decl *OldMember : D->decls()) {
//    Decl *NewMember = InjectDecl(OldMember);
//    if (!NewMember || NewMember->isInvalidDecl())
//      Ns->setInvalidDecl(true);
//  }
//
//  return Ns;
//}
//
//Decl* InjectionContext::InjectEnumDecl(EnumDecl *D) {
//  DeclContext *Owner = getSema().CurContext;
//
//  // FIXME: Transform the name and nested name specifier.
//
//  // FIXME: If there's a previous decl, be sure to link that with this
//  // enum.
//
//  // Start by creating the new enumeration.
//  EnumDecl *Enum = EnumDecl::Create(
//      getContext(), Owner, D->getLocStart(), D->getLocation(),
//      D->getIdentifier(), /*PrevDecl*/nullptr, D->isScoped(),
//      D->isScopedUsingClassTag(), D->isFixed());
//  AddDeclSubstitution(D, Enum);
//
//  if (D->isFixed()) {
//    if (TypeSourceInfo *TSI = D->getIntegerTypeSourceInfo()) {
//      // If we have type source information for the underlying type, it means it
//      // has been explicitly set by the user. Perform substitution on it before
//      // moving on.
//      TSI = TransformType(TSI);
//      if (!TSI || getSema().CheckEnumUnderlyingType(TSI))
//        Enum->setIntegerType(getSema().Context.IntTy);
//      else
//        Enum->setIntegerTypeSourceInfo(TSI);
//    } else {
//      assert(!D->getIntegerType()->isDependentType()
//             && "Dependent type without type source info");
//      Enum->setIntegerType(D->getIntegerType());
//    }
//  }
//
//  // Update the access specifier. Note that if the enum's access is modified,
//  // then so is that of its members.
//  if (Modifiers.modifyAccess())
//    Enum->setAccess(Modifiers.getAccess());
//  else
//    Enum->setAccess(D->getAccess());
//
//  // Forward the mangling number from the template to the instantiated decl.
//  getContext().setManglingNumber(Enum, getContext().getManglingNumber(D));
//
//  // See if the old tag was defined along with a declarator.
//  // If it did, mark the new tag as being associated with that declarator.
//  if (DeclaratorDecl *DD = getContext().getDeclaratorForUnnamedTagDecl(D))
//    getContext().addDeclaratorForUnnamedTagDecl(Enum, DD);
//
//  // See if the old tag was defined along with a typedef.
//  // If it did, mark the new tag as being associated with that typedef.
//  if (TypedefNameDecl *TD = getContext().getTypedefNameForUnnamedTagDecl(D))
//    getContext().addTypedefNameForUnnamedTagDecl(Enum, TD);
//
//  Owner->addDecl(Enum);
//
//  // If the enum is defined, inject it.
//  EnumDecl *Def = D->getDefinition();
//  if (Def == D)
//    InjectEnumDefinition(*this, D, Enum);
//
//  return D;
//}
//
//Decl* InjectionContext::InjectEnumConstantDecl(EnumConstantDecl *D) {
//  // NOTE: Enumerators are processed by InjectEnumDefinition.
//  llvm_unreachable("Should not get here");
//}
//
//Decl* InjectionContext::InjectTypedefNameDecl(TypedefNameDecl *D) {
//  bool Invalid = false;
//
//  DeclContext *Owner = getSema().CurContext;
//
//  // Transform the type. If this fails, just retain the original, but
//  // invalidate the declaration later.
//  TypeSourceInfo *TSI = TransformType(D->getTypeSourceInfo());
//  if (!TSI) {
//    TSI = D->getTypeSourceInfo();
//    Invalid = true;
//  }
//
//  // Create the new typedef
//  TypedefNameDecl *Typedef;
//  if (isa<TypeAliasDecl>(D))
//    Typedef = TypeAliasDecl::Create(
//        getContext(), Owner, D->getLocStart(), D->getLocation(),
//        D->getIdentifier(), TSI);
//  else
//    Typedef = TypedefDecl::Create(
//        getContext(), Owner, D->getLocStart(), D->getLocation(),
//        D->getIdentifier(), TSI);
//  AddDeclSubstitution(D, Typedef);
//
//  if (Modifiers.modifyAccess())
//    Typedef->setAccess(Modifiers.getAccess());
//  else
//    Typedef->setAccess(D->getAccess());
//  Typedef->setInvalidDecl(Invalid);
//  Owner->addDecl(Typedef);
//
//  return Typedef;
//}
//
//// Inject the name and the type of a declarator declaration. Sets the
//// declaration name info, type, and owner. Returns true if the declarator
//// is invalid.
////
//// FIXME: If the declarator has a nested names specifier, rebuild that
//// also. That potentially modifies the owner of the declaration
//bool InjectionContext::InjectDeclarator(DeclaratorDecl *D,
//                                        DeclarationNameInfo &DNI,
//                                        TypeSourceInfo *&TSI) {
//  bool Invalid = false;
//
//  // Rebuild the name.
//  DNI = TransformDeclarationName(D);
//  if (D->getDeclName().isEmpty() != DNI.getName().isEmpty()) {
//    DNI = DeclarationNameInfo(D->getDeclName(), D->getLocation());
//    Invalid = true;
//  }
//
//  // Rebuild the type.
//  TSI = TransformType(D->getTypeSourceInfo());
//  if (!TSI) {
//    TSI = D->getTypeSourceInfo();
//    Invalid = true;
//  }
//
//  return Invalid;
//}
//
//// Inject the name and the type of a declarator declaration. Sets the
//// declaration name info, type, and owner. Returns true if the declarator
//// is invalid.
//bool InjectionContext::InjectMemberDeclarator(DeclaratorDecl *D,
//                                              DeclarationNameInfo &DNI,
//                                              TypeSourceInfo *&TSI,
//                                              CXXRecordDecl *&Owner) {
//  bool Invalid = InjectDeclarator(D, DNI, TSI);
//  Owner = cast<CXXRecordDecl>(getSema().CurContext);
//  return Invalid;
//}
//
//static bool InjectVariableInitializer(InjectionContext &Cxt,
//                                      VarDecl *Old,
//                                      VarDecl *New) {
//  if (Old->getInit()) {
//    if (New->isStaticDataMember() && !Old->isOutOfLine())
//      Cxt.getSema().PushExpressionEvaluationContext(
//          Sema::ExpressionEvaluationContext::ConstantEvaluated, Old);
//    else
//      Cxt.getSema().PushExpressionEvaluationContext(
//          Sema::ExpressionEvaluationContext::PotentiallyEvaluated, Old);
//
//    // Instantiate the initializer.
//    ExprResult Init;
//    {
//      Sema::ContextRAII SwitchContext(Cxt.getSema(), New->getDeclContext());
//      bool DirectInit = (Old->getInitStyle() == VarDecl::CallInit);
//      Init = Cxt.TransformInitializer(Old->getInit(), DirectInit);
//    }
//
//    if (!Init.isInvalid()) {
//      Expr *InitExpr = Init.get();
//      if (New->hasAttr<DLLImportAttr>() &&
//          (!InitExpr ||
//           !InitExpr->isConstantInitializer(Cxt.getContext(), false))) {
//        // Do not dynamically initialize dllimport variables.
//      } else if (InitExpr) {
//        Cxt.getSema().AddInitializerToDecl(New, InitExpr, Old->isDirectInit());
//      } else {
//        Cxt.getSema().ActOnUninitializedDecl(New);
//      }
//    } else {
//      New->setInvalidDecl();
//    }
//
//    Cxt.getSema().PopExpressionEvaluationContext();
//  } else {
//    if (New->isStaticDataMember()) {
//      if (!New->isOutOfLine())
//        return New;
//
//      // If the declaration inside the class had an initializer, don't add
//      // another one to the out-of-line definition.
//      if (Old->getFirstDecl()->hasInit())
//        return New;
//    }
//
//    // We'll add an initializer to a for-range declaration later.
//    if (New->isCXXForRangeDecl())
//      return New;
//
//    Cxt.getSema().ActOnUninitializedDecl(New);
//  }
//
//  return New;
//}
//
//void InjectionContext::UpdateFunctionParms(FunctionDecl* Old,
//                                           FunctionDecl* New) {
//  // Make sure the parameters are actually bound to the function.
//  TypeSourceInfo *TSI = New->getTypeSourceInfo();
//  FunctionProtoTypeLoc TL = TSI->getTypeLoc().castAs<FunctionProtoTypeLoc>();
//  New->setParams(TL.getParams());
//
//  // Update the parameters their owning functions and register substitutions
//  // as needed. Note that we automatically register substitutions for injected
//  // parameters.
//  unsigned OldIndex = 0;
//  unsigned NewIndex = 0;
//  auto OldParms = Old->parameters();
//  auto NewParms = New->parameters();
//  if (OldParms.size() > 0) {
//    do {
//      ParmVarDecl *OldParm = OldParms[OldIndex++];
//      if (auto *Injected = FindInjectedParameterPack(OldParm)) {
//        for (unsigned I = 0; I < Injected->size(); ++I) {
//          ParmVarDecl *NewParm = NewParms[NewIndex++];
//          NewParm->setOwningFunction(New);
//        }
//      } else {
//        ParmVarDecl *NewParm = NewParms[NewIndex++];
//        NewParm->setOwningFunction(New);
//        AddDeclSubstitution(OldParm, NewParm);
//      }
//    } while (OldIndex < OldParms.size() && NewIndex < NewParms.size());
//  } else {
//    assert(NewParms.size() == 0);
//  }
//  assert(OldIndex == OldParms.size() && NewIndex == NewParms.size());
//}
//
//
//Decl *InjectionContext::InjectFunctionDecl(FunctionDecl *D) {
//  DeclContext *Owner = getSema().CurContext;
//
//  DeclarationNameInfo DNI;
//  TypeSourceInfo* TSI;
//  bool Invalid = InjectDeclarator(D, DNI, TSI);
//
//  // FIXME: Check for redeclaration.
//
//
//  FunctionDecl* Fn = FunctionDecl::Create(
//      getContext(), Owner, D->getLocation(), DNI, TSI->getType(), TSI,
//      D->getStorageClass(), D->hasWrittenPrototype(), D->isConstexpr());
//  AddDeclSubstitution(D, Fn);
//
//  UpdateFunctionParms(D, Fn);
//
//  // Set properties.
//  Fn->setInlineSpecified(D->isInlineSpecified());
//  Fn->setInvalidDecl(Invalid);
//
//  Owner->addDecl(Fn);
//
//  // If the function has a body, inject that also. Note that namespace-scope
//  // function definitions are never deferred. Also, function decls never
//  // appear in class scope (we hope), so we shouldn't be doing this too
//  // early.
//  if (Stmt *OldBody = D->getBody()) {
//    Sema::ContextRAII FnCxt (getSema(), Fn);
//    StmtResult NewBody = TransformStmt(OldBody);
//    if (NewBody.isInvalid())
//      Fn->setInvalidDecl();
//    else
//      Fn->setBody(NewBody.get());
//  }
//
//  return Fn;
//}
//
//Decl *InjectionContext::InjectVarDecl(VarDecl *D) {
//  DeclContext *Owner = getSema().CurContext;
//
//  DeclarationNameInfo DNI;
//  TypeSourceInfo *TSI;
//  bool Invalid = InjectDeclarator(D, DNI, TSI);
//
//  // FIXME: Check for re-declaration.
//
//  VarDecl *Var = VarDecl::Create(
//      getContext(), Owner, D->getInnerLocStart(), DNI, TSI->getType(),
//      TSI, D->getStorageClass());
//  AddDeclSubstitution(D, Var);
//
//  if (D->isNRVOVariable()) {
//    QualType ReturnType = cast<FunctionDecl>(Owner)->getReturnType();
//    if (getSema().isCopyElisionCandidate(ReturnType, Var, Sema::CES_Strict)) //DWR CHANGE / POSSIBLE FIXME : last used
//                                      // to be "false"; CES_Strict is enum value zero so I used that; change if need be
//      Var->setNRVOVariable(true);
//  }
//
//  Var->setImplicit(D->isImplicit());
//  Var->setInvalidDecl(Invalid);
//  Owner->addDecl(Var);
//
//  // If we are instantiating a local extern declaration, the
//  // instantiation belongs lexically to the containing function.
//  // If we are instantiating a static data member defined
//  // out-of-line, the instantiation will have the same lexical
//  // context (which will be a namespace scope) as the template.
//  if (D->isLocalExternDecl()) {
//    Var->setLocalExternDecl();
//    Var->setLexicalDeclContext(Owner);
//  } else if (D->isOutOfLine()) {
//    Var->setLexicalDeclContext(D->getLexicalDeclContext());
//  }
//  Var->setTSCSpec(D->getTSCSpec());
//  Var->setInitStyle(D->getInitStyle());
//  Var->setCXXForRangeDecl(D->isCXXForRangeDecl());
//  Var->setConstexpr(D->isConstexpr());
//  Var->setInitCapture(D->isInitCapture());
//  Var->setPreviousDeclInSameBlockScope(D->isPreviousDeclInSameBlockScope());
//
//  if (Modifiers.modifyAccess())
//    Var->setAccess(Modifiers.getAccess());
//  else
//    Var->setAccess(D->getAccess());
//
//  if (!D->isStaticDataMember()) {
//    if (D->isUsed(false))
//      Var->setIsUsed();
//    Var->setReferenced(D->isReferenced());
//  }
//
//  // FIXME: Instantiate attributes.
//
//  // Forward the mangling number from the template to the instantiated decl.
//  getContext().setManglingNumber(
//      Var, getContext().getManglingNumber(D));
//  getContext().setStaticLocalNumber(
//      Var, getContext().getStaticLocalNumber(D));
//
//  if (D->isInlineSpecified())
//    Var->setInlineSpecified();
//  else if (D->isInline())
//    Var->setImplicitlyInline();
//
//  InjectVariableInitializer(*this, D, Var);
//
//  // FIXME: Diagnose unused declarations here?
//
//  return Var;
//}
//
//Decl *InjectionContext::InjectParmVarDecl(ParmVarDecl *D) {
//  // Parameters are created during type transformation. We add mappings
//  // for them when creating the function.
//  llvm_unreachable("Should not get here");
//}
//
///// Injects the base specifier Base into Class.
//static bool InjectBaseSpecifiers(InjectionContext &Cxt,
//                                 CXXRecordDecl *OldClass,
//                                 CXXRecordDecl *NewClass) {
//  bool Invalid = false;
//  SmallVector<CXXBaseSpecifier*, 4> Bases;
//  for (const CXXBaseSpecifier &OldBase : OldClass->bases()) {
//    TypeSourceInfo *TSI = Cxt.TransformType(OldBase.getTypeSourceInfo());
//    if (!TSI) {
//      Invalid = true;
//      continue;
//    }
//
//    CXXBaseSpecifier *NewBase = Cxt.getSema().CheckBaseSpecifier(
//        NewClass, OldBase.getSourceRange(), OldBase.isVirtual(),
//        OldBase.getAccessSpecifierAsWritten(), TSI, OldBase.getEllipsisLoc());
//    if (!NewBase) {
//      Invalid = true;
//      continue;
//    }
//
//    Bases.push_back(NewBase);
//  }
//
//  if (!Invalid && Cxt.getSema().AttachBaseSpecifiers(NewClass, Bases))
//    Invalid = true;
//
//  // Invalidate the class if necessary.
//  NewClass->setInvalidDecl(Invalid);
//
//  return Invalid;
//}
//
//static bool InjectClassMembers(InjectionContext &Cxt,
//                               CXXRecordDecl *OldClass,
//                               CXXRecordDecl *NewClass) {
//  for (Decl *OldMember : OldClass->decls()) {
//    // Don't transform invalid declarations.
//    if (OldMember->isInvalidDecl())
//      continue;
//
//    // Don't transform non-members appearing in a class.
//    //
//    // FIXME: What does it mean to inject friends?
//    if (OldMember->getDeclContext() != OldClass)
//      continue;
//
//    Decl *NewMember = Cxt.InjectDecl(OldMember);
//    if (!NewMember)
//      NewClass->setInvalidDecl();
//  }
//  return NewClass->isInvalidDecl();
//}
//
//static bool InjectClassDefinition(InjectionContext &Cxt,
//                                  CXXRecordDecl *OldClass,
//                                  CXXRecordDecl *NewClass) {
//  Sema::ContextRAII SwitchContext(Cxt.getSema(), NewClass);
//  InjectionContext::SuppressModifiersRAII SwitchModifiers(Cxt);
//  Cxt.getSema().StartDefinition(NewClass);
//  InjectBaseSpecifiers(Cxt, OldClass, NewClass);
//  InjectClassMembers(Cxt, OldClass, NewClass);
//  Cxt.getSema().CompleteDefinition(NewClass);
//  return NewClass->isInvalidDecl();
//}
//
//Decl *InjectionContext::InjectCXXRecordDecl(CXXRecordDecl *D) {
//  bool Invalid = false;
//  DeclContext *Owner = getSema().CurContext;
//
//  // FIXME: Do a lookup for previous declarations.
//
//  CXXRecordDecl *Class;
//  if (D->isInjectedClassName()) {
//    DeclarationName DN = cast<CXXRecordDecl>(Owner)->getDeclName();
//    Class = CXXRecordDecl::Create(
//        getContext(), D->getTagKind(), Owner, D->getLocStart(),
//        D->getLocation(), DN.getAsIdentifierInfo(), /*PrevDecl=*/nullptr);
//  } else {
//    DeclarationNameInfo DNI = TransformDeclarationName(D);
//    if (!DNI.getName())
//      Invalid = true;
//    Class = CXXRecordDecl::Create(
//        getContext(), D->getTagKind(), Owner, D->getLocStart(),
//        D->getLocation(), DNI.getName().getAsIdentifierInfo(),
//        /*PrevDecl=*/nullptr);
//  }
//  AddDeclSubstitution(D, Class);
//
//  // FIXME: Inject attributes.
//
//  // FIXME: Propagate other properties?
//  if (Modifiers.modifyAccess())
//    Class->setAccess(Modifiers.getAccess());
//  else
//    Class->setAccess(D->getAccess());
//  Class->setImplicit(D->isImplicit());
//  Class->setInvalidDecl(Invalid);
//  Owner->addDecl(Class);
//
//  if (D->hasDefinition())
//    InjectClassDefinition(*this, D, Class);
//
//  return Class;
//}
//
//// FIXME: This needs a LOT of work.
//Decl* InjectionContext::InjectStaticDataMemberDecl(FieldDecl *D) {
//  DeclarationNameInfo DNI;
//  TypeSourceInfo *TSI;
//  CXXRecordDecl *Owner;
//  bool Invalid = InjectMemberDeclarator(D, DNI, TSI, Owner);
//
//  VarDecl *Var = VarDecl::Create(
//      getContext(), Owner, D->getLocation(), DNI, TSI->getType(),
//      TSI, SC_Static);
//  AddDeclSubstitution(D, Var);
//
//  Var->setAccess(D->getAccess());
//  Var->setInvalidDecl(Invalid);
//  Owner->addDecl(Var);
//
//  // FIXME: This is almost certainly going to break when it runs.
//  // if (D->hasInClassInitializer())
//  //   InjectedDefinitions.push_back(InjectedDef(D, Var));
//
//  if (D->hasInClassInitializer())
//    llvm_unreachable("Initialization of static members not implemented");
//
//  return Var;
//}
//
//Decl *InjectionContext::InjectFieldDecl(FieldDecl *D) {
//  if (Modifiers.modifyStorage() && Modifiers.getStorage() == SC_Static)
//    return InjectStaticDataMemberDecl(D);
//
//  DeclarationNameInfo DNI;
//  TypeSourceInfo *TSI;
//  CXXRecordDecl *Owner;
//  bool Invalid = InjectMemberDeclarator(D, DNI, TSI, Owner);
//
//  // FIXME: Substitute through the bit width.
//  Expr *BitWidth = nullptr;
//
//  // Build and check the field.
//  FieldDecl *Field = getSema().CheckFieldDecl(
//      DNI.getName(), TSI->getType(), TSI, Owner, D->getLocation(),
//      D->isMutable(), BitWidth, D->getInClassInitStyle(), D->getInnerLocStart(),
//      D->getAccess(), nullptr);
//  AddDeclSubstitution(D, Field);
//
//  // FIXME: Propagate attributes?
//
//  // FIXME: In general, see VisitFieldDecl in the template instantiatior.
//  // There are some interesting cases we probably need to handle.
//
//  // Can't make
//  if (Modifiers.addConstexpr()) {
//    SemaRef.Diag(D->getLocation(), diag::err_modify_constexpr_field);
//    Field->setInvalidDecl(true);
//  }
//
//  // Propagate semantic properties.
//  Field->setImplicit(D->isImplicit());
//  if (Modifiers.modifyAccess())
//    Field->setAccess(Modifiers.getAccess());
//  else
//    Field->setAccess(D->getAccess());
//
//  if (!Field->isInvalidDecl())
//    Field->setInvalidDecl(Invalid);
//
//  Owner->addDecl(Field);
//
//  // If the field has an initializer, add it to the Fragment so that we
//  // can process it later.
//  if (D->hasInClassInitializer())
//    InjectedDefinitions.push_back(InjectedDef(D, Field));
//
//  return Field;
//}
//
//Decl *InjectionContext::InjectCXXMethodDecl(CXXMethodDecl *D) {
//  ASTContext &AST = getContext();
//  DeclarationNameInfo DNI;
//  TypeSourceInfo *TSI;
//  CXXRecordDecl *Owner;
//  bool Invalid = InjectMemberDeclarator(D, DNI, TSI, Owner);
//
//  // Build the underlying method.
//  //
//  // FIXME: Should we propagate implicit operators?
//  CXXMethodDecl *Method;
//  if (CXXConstructorDecl *Ctor = dyn_cast<CXXConstructorDecl>(D)) {
//    Method = CXXConstructorDecl::Create(AST, Owner, D->getLocStart(), DNI,
//                                        TSI->getType(), TSI,
//                                        Ctor->isExplicit(),
//                                        Ctor->isInlineSpecified(),
//                                        Ctor->isImplicit(),
//                                        Ctor->isConstexpr());
//    Method->setRangeEnd(D->getLocEnd());
//  } else if (CXXDestructorDecl *Dtor = dyn_cast<CXXDestructorDecl>(D)) {
//    Method = CXXDestructorDecl::Create(AST, Owner, D->getLocStart(), DNI,
//                                       TSI->getType(), TSI,
//                                       Dtor->isInlineSpecified(),
//                                       Dtor->isImplicit());
//    Method->setRangeEnd(D->getLocEnd());
//  } else if (CXXConversionDecl *Conv = dyn_cast<CXXConversionDecl>(D)) {
//    Method = CXXConversionDecl::Create(AST, Owner, D->getLocStart(), DNI,
//                                       TSI->getType(), TSI,
//                                       Conv->isInlineSpecified(),
//                                       Conv->isExplicit(), Conv->isConstexpr(),
//                                       Conv->getLocEnd());
//  } else {
//    Method = CXXMethodDecl::Create(AST, Owner, D->getLocStart(), DNI,
//                                   TSI->getType(), TSI,
//                                   D->isStatic() ? SC_Static : SC_None,
//                                   D->isInlineSpecified(), D->isConstexpr(),
//                                   D->getLocEnd());
//  }
//
//  AddDeclSubstitution(D, Method);
//  UpdateFunctionParms(D, Method);
//
//  // Propagate Template Attributes
//  MemberSpecializationInfo *MemberSpecInfo = D->getMemberSpecializationInfo();
//  if (MemberSpecInfo) {
//    FunctionDecl *TemplateFD =
//      static_cast<FunctionDecl *>(MemberSpecInfo->getInstantiatedFrom());
//    TemplateSpecializationKind TemplateSK =
//      MemberSpecInfo->getTemplateSpecializationKind();
//    Method->setInstantiationOfMemberFunction(TemplateFD, TemplateSK);
//  }
//
//  // FIXME: Propagate attributes
//
//  // FIXME: Check for redeclarations
//
//  // Propagate semantic properties.
//  //
//  // FIXME: Inherit access as a semantic attribute or trace it through the
//  // injection as if parsing?
//  Method->setImplicit(D->isImplicit());
//
//  // Update the access specifier.
//  if (Modifiers.modifyAccess())
//    Method->setAccess(Modifiers.getAccess());
//  else
//    Method->setAccess(D->getAccess());
//
//  // Update the constexpr specifier.
//  if (Modifiers.addConstexpr()) {
//    if (isa<CXXDestructorDecl>(Method)) {
//      SemaRef.Diag(D->getLocation(), diag::err_constexpr_dtor);
//      Method->setInvalidDecl(true);
//    }
//    Method->setConstexpr(true);
//  }
//
//  // Propagate virtual flags.
//  Method->setVirtualAsWritten(D->isVirtualAsWritten());
//  if (D->isPure())
//    SemaRef.CheckPureMethod(Method, Method->getSourceRange());
//
//  // Request to make function virtual. Note that the original may have
//  // a definition. When the original is defined, we'll ignore the definition.
//  if (Modifiers.addVirtual() || Modifiers.addPureVirtual()) {
//    // FIXME: Actually generate a diagnostic here.
//    if (isa<CXXConstructorDecl>(Method)) {
//      SemaRef.Diag(D->getLocation(), diag::err_modify_virtual_constructor);
//      Method->setInvalidDecl(true);
//    } else {
//      Method->setVirtualAsWritten(true);
//      if (Modifiers.addPureVirtual())
//        SemaRef.CheckPureMethod(Method, Method->getSourceRange());
//    }
//  }
//
//  // FIXME: Should be modifiable?
//  Method->setDeletedAsWritten(D->isDeletedAsWritten());
//
//  // FIXME: Should be modifiable?
//  Method->setDefaulted(D->isDefaulted());
//
//  if (!Method->isInvalidDecl())
//    Method->setInvalidDecl(Invalid);
//
//  // Don't register the declaration if we're injecting the declaration of
//  // a template-declaration. We'll add the template later.
//  if (!D->getDescribedFunctionTemplate())
//    Owner->addDecl(Method);
//
//  // If the method is has a body, add it to the context so that we can
//  // process it later. Note that deleted/defaulted definitions are just
//  // flags processed above. Ignore the definition if we've marked this
//  // as pure virtual.
//  if (D->hasBody() && !Method->isPure())
//    InjectedDefinitions.push_back(InjectedDef(D, Method));
//
//  return Method;
//}
//
//Decl *InjectionContext::InjectAccessSpecDecl(AccessSpecDecl *D) {
//  CXXRecordDecl *Owner = cast<CXXRecordDecl>(getSema().CurContext);
//  return AccessSpecDecl::Create(
//      getContext(), D->getAccess(), Owner, D->getLocation(), D->getColonLoc());
//}
//
//Decl *InjectionContext::InjectConstexprDecl(ConstexprDecl *D) {
//  // We can use the ActOn* members since the initial parsing for these
//  // declarations is trivial (i.e., don't have to translate declarators).
//  unsigned ScopeFlags; // Unused
//  Decl *New = getSema().ActOnConstexprDecl(
//    /*Scope=*/nullptr, D->getLocation(), ScopeFlags, D->getcdcv());
//
//  getSema().ActOnStartConstexprDecl(/*Scope=*/nullptr, New);
//  StmtResult S = TransformStmt(D->getBody());
//  if (!S.isInvalid())
//    getSema().ActOnFinishConstexprDecl(/*Scope=*/nullptr, New, S.get());
//  else
//    getSema().ActOnConstexprDeclError(/*Scope=*/nullptr, New);
//
//  return New;
//}
//
//TemplateParameterList *
//InjectionContext::InjectTemplateParms(TemplateParameterList *OldParms) {
//  bool Invalid = false;
//  SmallVector<NamedDecl *, 8> NewParms;
//  NewParms.reserve(OldParms->size());
//  for (auto &P : *OldParms) {
//    NamedDecl *D = cast_or_null<NamedDecl>(InjectDecl(P));
//    NewParms.push_back(D);
//    if (!D || D->isInvalidDecl())
//      Invalid = true;
//  }
//
//  // Clean up if we had an error.
//  if (Invalid)
//    return nullptr;
//
//  ExprResult Reqs = TransformExpr(OldParms->getRequiresClause());
//  if (Reqs.isInvalid())
//    return nullptr;
//
//  return TemplateParameterList::Create(
//      getSema().Context, OldParms->getTemplateLoc(), OldParms->getLAngleLoc(),
//      NewParms, OldParms->getRAngleLoc(), Reqs.get());
//}
//
//Decl* InjectionContext::InjectFunctionTemplateDecl(FunctionTemplateDecl *D) {
//  DeclContext *Owner = getSema().CurContext;
//
//  TemplateParameterList *Parms = InjectTemplateParms(D->getTemplateParameters());
//  if (!Parms)
//    return nullptr;
//
//  // Build the underlying pattern.
//  Decl *Pattern = InjectDecl(D->getTemplatedDecl());
//  if (!Pattern)
//    return nullptr;
//  FunctionDecl *Fn = cast<FunctionDecl>(Pattern);
//
//  // Build the enclosing template.
//  FunctionTemplateDecl *Template = FunctionTemplateDecl::Create(
//      getSema().Context, getSema().CurContext, Fn->getLocation(),
//      Fn->getDeclName(), Parms, Fn);
//  AddDeclSubstitution(D, Template);
//
//  // FIXME: Other attributes to process?
//  Fn->setDescribedFunctionTemplate(Template);
//  Template->setAccess(D->getAccess());
//
//  // Add the declaration.
//  Owner->addDecl(Template);
//
//  return Template;
//}
//
//Decl* InjectionContext::InjectTemplateTypeParmDecl(TemplateTypeParmDecl *D) {
//  TemplateTypeParmDecl *Parm = TemplateTypeParmDecl::Create(
//      getSema().Context, getSema().CurContext, D->getLocStart(), D->getLocation(),
//      D->getDepth(), D->getIndex(), D->getIdentifier(),
//      D->wasDeclaredWithTypename(), D->isParameterPack());
//  AddDeclSubstitution(D, Parm);
//
//  Parm->setAccess(AS_public);
//
//  // Process the default argument.
//  if (D->hasDefaultArgument() && !D->defaultArgumentWasInherited()) {
//    TypeSourceInfo *Default = TransformType(D->getDefaultArgumentInfo());
//    if (Default)
//      Parm->setDefaultArgument(Default);
//    // FIXME: What if this fails.
//  }
//
//  return Parm;
//}
//
//Decl *InjectionContext::InjectDeclImpl(Decl *D) {
//  // Inject the declaration.
//  switch (D->getKind()) {
//  case Decl::Namespace:
//    return InjectNamespaceDecl(cast<NamespaceDecl>(D));
//  case Decl::Enum:
//    return InjectEnumDecl(cast<EnumDecl>(D));
//  case Decl::EnumConstant:
//    return InjectEnumConstantDecl(cast<EnumConstantDecl>(D));
//  case Decl::Typedef:
//  case Decl::TypeAlias:
//    return InjectTypedefNameDecl(cast<TypedefNameDecl>(D));
//  case Decl::Function:
//    return InjectFunctionDecl(cast<FunctionDecl>(D));
//  case Decl::Var:
//    return InjectVarDecl(cast<VarDecl>(D));
//  case Decl::ParmVar:
//    return InjectParmVarDecl(cast<ParmVarDecl>(D));
//  case Decl::CXXRecord:
//    return InjectCXXRecordDecl(cast<CXXRecordDecl>(D));
//  case Decl::Field:
//    return InjectFieldDecl(cast<FieldDecl>(D));
//  case Decl::CXXMethod:
//  case Decl::CXXConstructor:
//  case Decl::CXXDestructor:
//  case Decl::CXXConversion:
//    return InjectCXXMethodDecl(cast<CXXMethodDecl>(D));
//  case Decl::AccessSpec:
//    return InjectAccessSpecDecl(cast<AccessSpecDecl>(D));
//  case Decl::Constexpr:
//    return InjectConstexprDecl(cast<ConstexprDecl>(D));
//  case Decl::FunctionTemplate:
//    return InjectFunctionTemplateDecl(cast<FunctionTemplateDecl>(D));
//  case Decl::TemplateTypeParm:
//    return InjectTemplateTypeParmDecl(cast<TemplateTypeParmDecl>(D));
//  default:
//    break;
//  }
//  D->dump();
//  llvm_unreachable("unknown declaration");
//}
//
///// \brief Injects a new version of the declaration. Do not use this to
///// resolve references to declarations; use ResolveDecl instead.
//Decl *InjectionContext::InjectDecl(Decl *D) {
//  assert(!GetDeclReplacement(D) && "Declaration already injected");
//
//  // If the declaration does not appear in the context, then it need
//  // not be resolved.
//  if (!IsInInjection(D))
//    return D;
//
//  Decl* R = InjectDeclImpl(D);
//  if (!R)
//    return nullptr;
//
//  // If we injected a top-level declaration, notify the AST consumer,
//  // so that it can be processed for code generation.
//  if (isa<TranslationUnitDecl>(R->getDeclContext()))
//    getSema().Consumer.HandleTopLevelDecl(DeclGroupRef(R));
//
//  return R;
//}
//
//} // namespace clang



// -------------------------------------------------------------------------- //
// Semantic analysis



////ASUTTON ADDN:
//// Find variables to capture in the given scope.
//static void FindCapturesInScope(Sema &SemaRef, Scope *S,
//                                SmallVectorImpl<VarDecl *> &Vars) {
//  for (Decl *D : S->decls()) {
//    if (VarDecl *Var = dyn_cast<VarDecl>(D)) {
//      // Only capture locals with initializers.
//      //
//      // FIXME: If the fragment is in the initializer of a variable, this
//      // will also capture that variable. For example:
//      //
//      //    auto f = __fragment class { ... };
//      //
//      // The capture list for the fragment will include f. This seems insane,
//      // but lambda capture seems to also do this (with some caveats about
//      // usage).
//      //
//      // We can actually detect this case in this implementation because
//      // the type must be deduced and we won't have associated the
//      // initializer with the variable yet.
//      if (!isa<ParmVarDecl>(Var) &&
//          !Var->hasInit() &&
//          Var->getType()->isUndeducedType())
//        continue;
//
//      Vars.push_back(Var);
//    }
//  }
//}
//
//// Search the scope list for captured variables. When S is null, we're
//// applying applying a transformation.
//static void FindCaptures(Sema &SemaRef, Scope *S, FunctionDecl *Fn,
//                         SmallVectorImpl<VarDecl *> &Vars) {
//  assert(S && "Expected non-null scope");
//  while (S && S->getEntity() != Fn) {
//    FindCapturesInScope(SemaRef, S, Vars);
//    S = S->getParent();
//  }
//  if (S)
//    FindCapturesInScope(SemaRef, S, Vars);
//}
//
///// Construct a reference to each captured value and force an r-value
///// conversion so that we get rvalues during evaluation.
//static void ReferenceCaptures(Sema &SemaRef,
//                              SmallVectorImpl<VarDecl *> &Vars,
//                              SmallVectorImpl<Expr *> &Refs) {
//  Refs.resize(Vars.size());
//  std::transform(Vars.begin(), Vars.end(), Refs.begin(), [&](VarDecl *D) {
//    Expr *Ref = new (SemaRef.Context) DeclRefExpr(D, false, D->getType(),
//                                                  VK_LValue, D->getLocation());
//    return ImplicitCastExpr::Create(SemaRef.Context, D->getType(),
//                                    CK_LValueToRValue, Ref, nullptr, VK_RValue);
//  });
//}
//
///// Returns the variable from a captured declaration.
//static VarDecl *GetVariableFromCapture(Expr *E)
//{
//  Expr *Ref = cast<ImplicitCastExpr>(E)->getSubExpr();
//  return cast<VarDecl>(cast<DeclRefExpr>(Ref)->getDecl());
//}
//
//// Create a placeholder for each captured expression in the scope of the
//// fragment. For some captured variable 'v', these have the form:
////
////    constexpr auto v = <opaque>;
////
//// These are replaced by their values during injection.
//static void CreatePlaceholder(Sema &SemaRef, CXXFragmentDecl *Frag, Expr *E) {
//  ValueDecl *Var = GetVariableFromCapture(E);
//  SourceLocation IdLoc = Var->getLocation();
//  IdentifierInfo *Id = Var->getIdentifier();
//  QualType T = SemaRef.Context.DependentTy;
//  TypeSourceInfo *TSI = SemaRef.Context.getTrivialTypeSourceInfo(T);
//  VarDecl *Placeholder = VarDecl::Create(SemaRef.Context, Frag, IdLoc, IdLoc,
//                                         Id, T, TSI, SC_Static);
//  Placeholder->setConstexpr(true);
//  Placeholder->setImplicit(true);
//  Placeholder->setInitStyle(VarDecl::CInit);
//  Placeholder->setInit(
//      new (SemaRef.Context) OpaqueValueExpr(IdLoc, T, VK_RValue));
//  Placeholder->setReferenced(true);
//  Placeholder->markUsed(SemaRef.Context);
//  Frag->addDecl(Placeholder);
//}
//
//static void CreatePlaceholders(Sema &SemaRef, CXXFragmentDecl *Frag,
//                               SmallVectorImpl<Expr *> &Captures) {
//  std::for_each(Captures.begin(), Captures.end(), [&](Expr *E) {
//    CreatePlaceholder(SemaRef, Frag, E);
//  });
//}
//
///// Called at the start of a source code fragment to establish the list of
///// automatic variables captured. This is only called by the parser and searches
///// the list of local variables in scope.
//void Sema::ActOnCXXFragmentCapture(SmallVectorImpl<Expr *> &Captures) {
//  assert(Captures.empty() && "Captures already specified");
//
//  // Only collect captures within a function.
//  //
//  // FIXME: It might be better to use the scope, but the flags don't appear
//  // to be set right within constexpr declarations, etc.
//  if (isa<FunctionDecl>(CurContext)) {
//    SmallVector<VarDecl *, 8> Vars;
//    FindCaptures(*this, CurScope, getCurFunctionDecl(), Vars);
//    ReferenceCaptures(*this, Vars, Captures);
//  }
//}
//
///// Called at the start of a source code fragment to establish the fragment
///// declaration and placeholders.
//Decl *Sema::ActOnStartCXXFragment(Scope* S, SourceLocation Loc,
//                                  SmallVectorImpl<Expr *> &Captures) {
//  CXXFragmentDecl *Fragment = CXXFragmentDecl::Create(Context, CurContext, Loc);
//  CreatePlaceholders(*this, Fragment, Captures);
//  if (S)
//    PushDeclContext(S, Fragment);
//  return Fragment;
//}
//
///// Binds the content the fragment declaration. Returns the updated fragment.
///// The Fragment is nullptr if an error occurred during parsing. However,
///// we still need to pop the declaration context.
//Decl *Sema::ActOnFinishCXXFragment(Scope *S, Decl *Fragment, Decl *Content) {
//  CXXFragmentDecl *FD = nullptr;
//  if (Fragment) {
//    FD = cast<CXXFragmentDecl>(Fragment);
//    FD->setContent(Content);
//  }
//
//  if (S)
//    PopDeclContext();
//
//  return FD;
//}
//
///// Builds a new fragment expression.
//ExprResult Sema::ActOnCXXFragmentExpr(SourceLocation Loc,
//                                      SmallVectorImpl<Expr *> &Captures,
//                                      Decl *Fragment) {
//  return BuildCXXFragmentExpr(Loc, Captures, Fragment);
//}
//
///// \brief Builds a new fragment expression.
///// Consider the following:
/////
/////   constexpr {
/////     int n = 0;
/////     auto x = __fragment class { int a, b, c };
/////   }
/////
///// The type of the expression is a new meta:: class defined, approximately,
///// like this:
/////
/////   using __base_type = typename($<fragment>); // for exposition
/////
/////   struct __fragment_type : base_type
/////     // inherit constructors.
/////     using base_type::base_type;
/////
/////     // storage for capture values.
/////     int n;
/////   };
/////
///// TODO: It seems like the base class subobject can be statically initialized
///// as part of a default constructor instead of providing an inherited
///// constructor and deferring all initialization until evaluation time.
//ExprResult Sema::BuildCXXFragmentExpr(SourceLocation Loc,
//                                      SmallVectorImpl<Expr *> &Captures,
//                                      Decl *Fragment) {
//  CXXFragmentDecl *FD = cast<CXXFragmentDecl>(Fragment);
//
//  // If the fragment appears in a context that depends on template parameters,
//  // then the expression is dependent.
//  //
//  // FIXME: This is just an approximation of the right answer. In truth, the
//  // expression is dependent if the fragment depends on any template parameter
//  // in this or any enclosing context.
//  if (CurContext->isDependentContext()) {
//    return new (Context) CXXFragmentExpr(Context, Loc, Context.DependentTy,
//                                         Captures, FD, nullptr);
//  }
//
//  // Build the expression used to the reflection of fragment.
//  //
//  // TODO: We should be able to compute the type without generating an
//  // expression. We're not actually using the expression.
//  ExprResult Reflection = BuildDeclReflection(Loc, FD->getContent());
//  if (Reflection.isInvalid())
//    return ExprError();
//
//  // Generate a fragment expression type.
//  //
//  // TODO: We currently use the declaration-global Fragment bit to indicate
//  // that the type of the expression is (indeed) a reflection of some kind.
//  // We might want create the class in the meta:: namespace and rely on only
//  // that information.
//  CXXRecordDecl *Class = CXXRecordDecl::Create(
//      Context, TTK_Class, CurContext, Loc, Loc, nullptr, nullptr);
//  Class->setImplicit(true);
//  Class->setFragment(true);
//  StartDefinition(Class);
//  QualType ClassTy = Context.getRecordType(Class);
//  TypeSourceInfo *ClassTSI = Context.getTrivialTypeSourceInfo(ClassTy);
//
//  // Build the base class for the fragment type; this is the type of the
//  // reflected entity.s
//  QualType BaseTy = Reflection.get()->getType();
//  TypeSourceInfo *BaseTSI = Context.getTrivialTypeSourceInfo(BaseTy);
//  CXXBaseSpecifier* Base = new (Context) CXXBaseSpecifier(
//    SourceRange(Loc, Loc), false, true, AS_public, BaseTSI, SourceLocation());
//  Class->setBases(&Base, 1);
//
//  // Create a field for each capture.
//  SmallVector<FieldDecl *, 4> Fields;
//  for (Expr *E : Captures) {
//    VarDecl *Var = GetVariableFromCapture(E);
//    std::string Name = "__captured_" + Var->getIdentifier()->getName().str();
//    IdentifierInfo *Id = &Context.Idents.get(Name);
//    TypeSourceInfo *TypeInfo = Context.getTrivialTypeSourceInfo(Var->getType());
//    FieldDecl *Field = FieldDecl::Create(
//        Context, Class, Loc, Loc, Id, Var->getType(), TypeInfo, nullptr, false,
//        ICIS_NoInit);
//    Field->setAccess(AS_public);
//    Field->setImplicit(true);
//    Fields.push_back(Field);
//    Class->addDecl(Field);
//  }
//
//  // Build a constructor that accepts the generated members.
//  DeclarationName Name = Context.DeclarationNames.getCXXConstructorName(
//      Context.getCanonicalType(ClassTy));
//  DeclarationNameInfo NameInfo(Name, Loc);
//  CXXConstructorDecl *Ctor = CXXConstructorDecl::Create(
//      Context, Class, Loc, NameInfo, /*Type*/QualType(), /*TInfo=*/nullptr,
//      /*isExplicit=*/true, /*isInline=*/true, /*isImplicitlyDeclared=*/false,
//      /*isConstexpr=*/true);
//  Ctor->setAccess(AS_public);
//
//  // Build the function type for said constructor.
//  FunctionProtoType::ExtProtoInfo EPI;
//  EPI.ExceptionSpec.Type = EST_Unevaluated;
//  EPI.ExceptionSpec.SourceDecl = Ctor;
//  EPI.ExtInfo = EPI.ExtInfo.withCallingConv(
//      Context.getDefaultCallingConvention(/*IsVariadic=*/false,
//                                          /*IsCXXMethod=*/true));
//  SmallVector<QualType, 4> ArgTypes;
//  for (Expr *E : Captures)
//    ArgTypes.push_back(E->getType());
//  QualType CtorTy = Context.getFunctionType(Context.VoidTy, ArgTypes, EPI);
//  Ctor->setType(CtorTy);
//
//  SmallVector<ParmVarDecl *, 4> Parms;
//  for (std::size_t I = 0; I < Captures.size(); ++I) {
//    Expr *E = Captures[I];
//    VarDecl *Var = GetVariableFromCapture(E);
//    std::string Name = "__parm_" + Var->getIdentifier()->getName().str();
//    IdentifierInfo* Id = &Context.Idents.get(Name);
//    QualType ParmTy = E->getType();
//    TypeSourceInfo *TypeInfo = Context.getTrivialTypeSourceInfo(ParmTy);
//    ParmVarDecl *Parm = ParmVarDecl::Create(
//        Context, Ctor, Loc, Loc, Id, ParmTy, TypeInfo, SC_None, nullptr);
//    Parm->setScopeInfo(0, I);
//    Parm->setImplicit(true);
//    Parms.push_back(Parm);
//  }
//  Ctor->setParams(Parms);
//
//  // Build constructor initializers.
//  std::size_t NumInits = Fields.size() + 1;
//  CXXCtorInitializer **Inits = new (Context) CXXCtorInitializer *[NumInits];
//  // Build the base initializer.
//  {
//    SourceLocation EL; // Empty ellipsis.
//    Expr *Arg = new (Context) ParenListExpr(Context, Loc, None, Loc);
//    Inits[0] = BuildBaseInitializer(BaseTy, BaseTSI, Arg, Class, EL).get();
//  }
//  // Build member initializers.
//  for (std::size_t I = 0; I < Parms.size(); ++I) {
//    ParmVarDecl *Parm = Parms[I];
//    FieldDecl *Field = Fields[I];
//    DeclRefExpr *Ref = new (Context) DeclRefExpr(
//        Parm, false, Parm->getType(), VK_LValue, Loc);
//    Expr *Arg = new (Context) ParenListExpr(Context, Loc, Ref, Loc);
//    Inits[I + 1] = BuildMemberInitializer(Field, Arg, Loc).get();
//  }
//  Ctor->setNumCtorInitializers(NumInits);
//  Ctor->setCtorInitializers(Inits);
//
//  // Build the definition.
//  Stmt *Def = new (Context) CompoundStmt(Context, None, Loc, Loc);
//  Ctor->setBody(Def);
//
//  Class->addDecl(Ctor);
//
//  CompleteDefinition(Class);
//
//  // Build an expression that that initializes the fragment object.
//  Expr *Init;
//  if (Captures.size() == 1) {
//    CXXConstructExpr *Cast = CXXConstructExpr::Create(
//        Context, ClassTy, Loc, Ctor, true, Captures,
//        /*HadMultipleCandidates=*/false, /*ListInitialization=*/false,
//        /*StdInitListInitialization=*/false, /*ZeroInitialization=*/false,
//        CXXConstructExpr::CK_Complete, SourceRange(Loc, Loc));
//    Init = CXXFunctionalCastExpr::Create(
//        Context, ClassTy, VK_RValue, ClassTSI, CK_NoOp, Cast,
//        /*Path=*/nullptr, Loc, Loc);
//  } else {
//    Init = new (Context) CXXTemporaryObjectExpr(
//        Context, Ctor, ClassTy, ClassTSI, Captures, SourceRange(Loc, Loc),
//        /*HadMultipleCandidates=*/false, /*ListInitialization=*/false,
//        /*StdInitListInitialization=*/false, /*ZeroInitialization=*/false);
//  }
//
//  // Finally, build the fragment expression.
//  return new (Context) CXXFragmentExpr(Context, Loc, ClassTy, Captures, FD, Init);
//}
////END
////ASUTTON ADDN:
///// Returns an injection statement.
//StmtResult Sema::ActOnCXXInjectionStmt(SourceLocation Loc, Expr *Reflection) {
//  return BuildCXXInjectionStmt(Loc, Reflection);
//}

///// Returns an injection statement.
//StmtResult Sema::BuildCXXInjectionStmt(SourceLocation Loc, Expr *Reflection) {
//  // The operand must be a reflection (if non-dependent).
//  if (!Reflection->isTypeDependent() && !Reflection->isValueDependent()) {
//    if (!isReflectionType(*this, Reflection->getType())) {
//      Diag(Reflection->getExprLoc(), diag::err_not_a_reflection);
//      return StmtError();
//    }
//  }
//  // Perform an lvalue-to-value conversion so that we get an rvalue in
//  // evaluation.
//  if (Reflection->isGLValue())
//    Reflection = ImplicitCastExpr::Create(Context, Reflection->getType(),
//                                          CK_LValueToRValue, Reflection,
//                                          nullptr, VK_RValue);
//
//  return new(Context) CXXInjectionStmt(Loc, Reflection);
//}
////END
////ASUTTON ADDN:
///// An injection declaration injects its fragment members at this point
///// in the program.
//StmtResult Sema::ActOnCXXExtensionStmt(SourceLocation Loc,
//                                       Expr *Target,
//                                       Expr *Reflection) {
//  llvm_unreachable("extension not supported");
//  // return BuildCXXExtensionStmt(Loc, Target, Reflection);
//}
//
//StmtResult Sema::BuildCXXExtensionStmt(SourceLocation Loc, Expr *Target,
//                                       Expr *Reflection) {
//  llvm_unreachable("extension not supported");
//#if 0
//  // Check the glvalue.
//  if (!Target->isTypeDependent()) {
//    // FIXEM: This isn't strictly *required* since even prvalues are just
//    // pointers to a mutable data structure. This is disabled, because the
//    // reflection operator returns prvalues, which complicates certain
//    // use patterns. For example:
//    //
//    //    struct C {
//    //      constexpr { fill($C); } // Would be an error.
//    //    };
//    //
//    // So, disable this for now.
//
//    // if (!Target->isGLValue()) {
//    //   Diag(Target->getExprLoc(), diag::err_extending_rvalue);
//    //   return StmtError();
//    // }
//
//    QualType TargetTy = Context.getCanonicalType(Target->getType());
//    if (CXXRecordDecl* Class = TargetTy->getAsCXXRecordDecl()) {
//      // FIXME: This isn't the right test. We need to determine during
//      // application if the target satisfies the requirements for extensions.
//      // if (!Class->isFragment() || !Class->isBeingDefined()) {
//      //   Diag(Target->getExprLoc(), diag::err_extending_declaration);
//      //   return StmtError();
//      // }
//    } else {
//      Diag(Target->getExprLoc(), diag::err_extending_non_reflection);
//      return StmtError();
//    }
//  }
//
//  // FIXME: If the reflection is non-dependent, verify that we actually
//  // have a reflection.
//
//  // Force an lvalue-to-rvalue conversion.
//  if (Target->isGLValue())
//    Target = ImplicitCastExpr::Create(Context, Target->getType(),
//                                      CK_LValueToRValue, Target,
//                                      nullptr, VK_RValue);
//  if (Reflection->isGLValue())
//    Reflection = ImplicitCastExpr::Create(Context, Reflection->getType(),
//                                          CK_LValueToRValue, Reflection,
//                                          nullptr, VK_RValue);
//
//  // Build an extension statement that can be evaluated when executed.
//  return new (Context) CXXExtensionStmt(Loc, Target, Reflection);
//#endif
//}
////END
////ASUTTON ADDN / DWR MOD:
//static Decl *
//GetDeclFromReflection(Sema &SemaRef, unsigned ReflPtr, SourceLocation Loc) //DWR REPLACED QualType Ty with Expr *E
//{
//  ReflectedConstruct Construct = ReflectedConstruct(ReflPtr); //replaces: EvaluateReflection(SemaRef, Ty, Loc);
//  Decl *Injection = nullptr;
//  if (Type *T = Construct.getAsType()) {
//    if (CXXRecordDecl *Class = T->getAsCXXRecordDecl())
//      Injection = Class;
//  } else
//    Injection = Construct.getAsDeclaration();
//  if (!Injection) {
//    SemaRef.Diag(Loc, diag::err_reflection_not_a_decl);
//    return nullptr;
//  }
//  return Injection;
//}
//
////END
////ASUTTON ADDN:
//// Not used.
//#if 0
//static Decl *
//GetDeclFromReflection(Sema &SemaRef, Expr *Reflection)
//{
//  return GetDeclFromReflection(SemaRef,
//                               Reflection->getType(),
//                               Reflection->getExprLoc());
//}
//#endif
///// An injection declaration injects its fragment members at this point
///// in the program.
//Sema::DeclGroupPtrTy Sema::ActOnCXXInjectionDecl(SourceLocation Loc,
//                                                 Expr *Reflection) {
//  llvm_unreachable("injection declarations not supported");
//  #if 0
//  if (Reflection->isTypeDependent() || Reflection->isValueDependent()) {
//    Decl *D = CXXInjectionDecl::Create(Context, CurContext, Loc, Reflection);
//    // FIXME: Actually use the current access specifier. For now, simply
//    // assume that public was meant.
//    if (isa<CXXRecordDecl>(CurContext))
//      D->setAccess(AS_public);
//    CurContext->addDecl(D);
//    return DeclGroupPtrTy::make(DeclGroupRef(D));
//  }
//
//  // Force an lvalue-to-rvalue conversion.
//  if (Reflection->isGLValue())
//    Reflection = ImplicitCastExpr::Create(Context, Reflection->getType(),
//                                          CK_LValueToRValue, Reflection,
//                                          nullptr, VK_RValue);
//
//  // Get the declaration or fragment to be injected.
//  Decl *Injection = GetDeclFromReflection(*this, Reflection);
//  if (!Injection)
//    return DeclGroupPtrTy();
//
//  // The Injectee is the current context.
//  Decl *Injectee = Decl::castFromDeclContext(CurContext);
//
//  // Evaluate the reflection.
//  SmallVector<PartialDiagnosticAt, 8> Notes;
//  Expr::EvalResult Result;
//  Result.Diag = &Notes;
//  if (!Reflection->EvaluateAsRValue(Result, Context)) {
//    // FIXME: This is not the right error.
//    Diag(Reflection->getExprLoc(), diag::err_not_a_reflection);
//    if (!Notes.empty()) {
//      for (const PartialDiagnosticAt &Note : Notes)
//        Diag(Note.first, Note.second);
//    }
//    return DeclGroupPtrTy();
//  }
//
//  // FIXME: If this is a fragment without a name, that should probably
//  // be an error, right?
//
//  // Always copy the injected declaration.
//  QualType Ty = Reflection->getType();
//  SmallVector<Decl *, 8> Decls;
//  if (!CopyDeclaration(*this, Loc, Ty, Result.Val, Injectee, Injection, Decls))
//    return DeclGroupPtrTy();
//
//  if (Decls.empty()) {
//    return DeclGroupPtrTy();
//  } else if (Decls.size() == 1) {
//    return DeclGroupPtrTy::make(DeclGroupRef(Decls.front()));
//  } else {
//    DeclGroup *DG = DeclGroup::Create(Context, Decls.data(), Decls.size());
//    return DeclGroupPtrTy::make(DeclGroupRef(DG));
//  }
//  #endif
//}
//
//
//static ClassTemplateSpecializationDecl *ReferencedReflectionClass(Sema &SemaRef,
//                                                                  Expr *E) {
//  QualType ExprTy = SemaRef.Context.getCanonicalType(E->getType());
//  if (!ExprTy->isRecordType())
//    return nullptr;
//  CXXRecordDecl* Class = ExprTy->getAsCXXRecordDecl();
//  if (!isa<ClassTemplateSpecializationDecl>(Class))
//    return nullptr;
//  ClassTemplateSpecializationDecl *Spec
//      = cast<ClassTemplateSpecializationDecl>(Class);
//
//  // Make sure that this is actually defined in meta.
//  DeclContext* Owner = Class->getDeclContext();
//  if (Owner->isInlineNamespace())
//    Owner = Owner->getParent();
//  if (!Owner->Equals(RequireCppxMetaNamespace(SemaRef, E->getExprLoc())))
//    return nullptr;
//  return Spec;
//}
//
//// Returns true if ExprTy refers to either a reflected function or the
//// parameters of a function. If true, Ref is set to the type containing the
//// function's encoded value.
//static bool ReferencesFunction(Sema &SemaRef, Expr *E, QualType &RefTy)
//{
//  auto *Spec = ReferencedReflectionClass(SemaRef, E);
//  if (!Spec)
//    return false;
//  StringRef Name = Spec->getIdentifier()->getName();
//  if (Name == "function") {
//    RefTy = SemaRef.Context.getTagDeclType(Spec);
//    return true;
//  }
//  if (Name == "reflected_tuple") {
//    // Dig out the class containing the info type. It should be:
//    //    reflected_tupe<function<X>::parm_info>.
//    TemplateArgument First = Spec->getTemplateArgs()[0];
//    if (First.getKind() != TemplateArgument::Type)
//      return false;
//    QualType T = First.getAsType();
//    if (!T->isRecordType())
//      return false;
//    CXXRecordDecl *Class = T->getAsCXXRecordDecl();
//    if (Class->getIdentifier()->getName() != "parm_info")
//        return false;
//    if (!Class->getDeclContext()->isRecord())
//      return false;
//    Class = cast<CXXRecordDecl>(Class->getDeclContext());
//    if (Class->getIdentifier()->getName() != "function" &&
//        Class->getIdentifier()->getName() != "method")
//      return false;
//    RefTy = SemaRef.Context.getTagDeclType(Class);
//    return true;
//  }
//
//  return false;
//}
//
//// Returns true if E refers to a reflected parameter. If true, then Ref is
//// set to the type containing the parameter's encoded value.
//static bool ReferencesParameter(Sema &SemaRef, Expr *E, QualType &RefTy) {
//  auto *Spec = ReferencedReflectionClass(SemaRef, E);
//  if (!Spec)
//    return false;
//  StringRef Name = Spec->getIdentifier()->getName();
//  if (Name == "parameter") {
//    RefTy = SemaRef.Context.getTagDeclType(Spec);
//    return true;
//  }
//  return false;
//}
//
//bool Sema::ActOnCXXInjectedParameter(SourceLocation Loc, Expr *Reflection,
//                                     IdentifierInfo *II,
//                           SmallVectorImpl<DeclaratorChunk::ParamInfo> &Parms) {
//  if (Reflection->isTypeDependent() || Reflection->isValueDependent()) {
//    // The type is an injected parameter type.
//    QualType T = Context.getInjectedParmType(Reflection);
//    TypeSourceInfo *TSI = Context.getTrivialTypeSourceInfo(T);
//
//    // FIXME: Make the constructor accept the type.
//    ParmVarDecl *New = ParmVarDecl::Create(Context,
//                                           Context.getTranslationUnitDecl(),
//                                           Loc, Loc, II, T, TSI, SC_None,
//                                           nullptr);
//      New->setScopeInfo(CurScope->getFunctionPrototypeDepth(),
//                        CurScope->getNextFunctionPrototypeIndex());
//    Parms.push_back(DeclaratorChunk::ParamInfo(nullptr, Loc, New));
//    return true;
//  }
//
//  // If T is meta::function<X> or reflected_tuple<meta::function<X>::parm_info>
//  // Then EllipsisLoc must be valid, and we inject all parameters.
//  QualType RefTy;
//  if (ReferencesFunction(*this, Reflection, RefTy)) {
//    ReflectedConstruct C = EvaluateReflection(*this, RefTy, Reflection->getExprLoc());
//    FunctionDecl *Fn = cast<FunctionDecl>(C.getAsDeclaration());
//
//    // Clone each parameter, inserting a chunk for the declaration.
//    for (ParmVarDecl *Orig : Fn->parameters()) {
//      TypeSourceInfo *TSI = Context.getTrivialTypeSourceInfo(Orig->getType());
//      ParmVarDecl *New = ParmVarDecl::Create(Context,
//                                             Context.getTranslationUnitDecl(),
//                                             Orig->getLocStart(),
//                                             Orig->getLocation(),
//                                             Orig->getIdentifier(),
//                                             Orig->getType(), TSI, SC_None,
//                                             nullptr);
//      New->setScopeInfo(CurScope->getFunctionPrototypeDepth(),
//                        CurScope->getNextFunctionPrototypeIndex());
//      New->setInjected(true);
//      Parms.push_back(DeclaratorChunk::ParamInfo(New->getIdentifier(),
//                                                 New->getLocation(), New));
//    }
//    return true;
//  }
//
//  // If T is meta::parameter<X>, then we inject that one parameter.
//  if (ReferencesParameter(*this, Reflection, RefTy)) {
//    // Clone the referenced parameter.
//    ReflectedConstruct C = EvaluateReflection(*this, RefTy, Reflection->getExprLoc());
//    ParmVarDecl *Orig = cast<ParmVarDecl>(C.getAsDeclaration());
//    TypeSourceInfo *TSI = Context.getTrivialTypeSourceInfo(Orig->getType());
//    ParmVarDecl *New = ParmVarDecl::Create(Context,
//                                           Context.getTranslationUnitDecl(),
//                                           Orig->getLocStart(),
//                                           Orig->getLocation(),
//                                           Orig->getIdentifier(),
//                                           Orig->getType(), TSI, SC_None,
//                                           nullptr);
//    New->setScopeInfo(CurScope->getFunctionPrototypeDepth(),
//                      CurScope->getNextFunctionPrototypeIndex());
//    New->setInjected(true);
//    Parms.push_back(DeclaratorChunk::ParamInfo(New->getIdentifier(),
//                                               New->getLocation(), New));
//    return true;
//  }
//
//  // FIXME: Improve diagnostics.
//  Diag(Reflection->getExprLoc(), diag::err_compiler_error) << "invalid parameter";
//  return false;
//}
//
//QualType Sema::BuildInjectedParmType(SourceLocation Loc, Expr *E) {
//  if (E->isTypeDependent())
//    return Context.getInjectedParmType(E);
//
//  MarkDeclarationsReferencedInExpr(E);
//
//  // If T is meta::function<X> or reflected_tuple<meta::function<X>::parm_info>
//  // Then EllipsisLoc must be valid, and we inject all parameters.
//  QualType RefTy;
//  if (ReferencesFunction(*this, E, RefTy)) {
//    ReflectedConstruct C = EvaluateReflection(*this, RefTy, E->getExprLoc());
//    FunctionDecl *Fn = cast<FunctionDecl>(C.getAsDeclaration());
//    return Context.getInjectedParmType(E, Fn->parameters());
//  }
//
//  // If T is meta::parameter<X>, then we inject that one parameter.
//  if (ReferencesParameter(*this, E, RefTy)) {
//    llvm_unreachable("not implemented");
//  }
//
//  // FIXME: Improve diagnostics.
//  Diag(E->getExprLoc(), diag::err_compiler_error) << "invalid parameter";
//  return QualType();
//}
//
//
////END
////ASUTTON ADDN:
//// Returns an integer value describing the target context of the injection.
//// This correlates to the second %select in err_invalid_injection.
//static int DescribeInjectionTarget(DeclContext *DC) {
//  if (DC->isFunctionOrMethod())
//    return 0;
//  else if (DC->isRecord())
//    return 1;
//  else if (DC->isNamespace())
//    return 2;
//  else if (DC->isTranslationUnit())
//    return 3;
//  else
//    llvm_unreachable("Invalid injection context");
//}
//
//struct TypedValue
//{
//  QualType Type;
//  APValue Value;
//};
//
//// Generate an error injecting a declaration of kind SK into the given
//// declaration context. Returns false. Note that SK correlates to the first
//// %select in err_invalid_injection.
//static bool InvalidInjection(Sema& S, SourceLocation POI, int SK,
//                             DeclContext *DC) {
//  S.Diag(POI, diag::err_invalid_injection) << SK << DescribeInjectionTarget(DC);
//  return false;
//}
//
//// FIXME: This is not particularly good. It would be nice if we didn't have
//// to search for this field.
//static const APValue& GetModifications(const APValue &V, QualType T,
//                                       DeclarationName N)
//{
//  CXXRecordDecl *Class = T->getAsCXXRecordDecl();
//  assert(Class && "Expected a class");
//
//  auto Lookup = Class->lookup(N);
//  assert(Lookup.size() <= 1 && "Ambiguous reference to traits");
//  if (Lookup.empty()) {
//    // If we can't find the field, work up recursively.
//    if (Class->getNumBases()) {
//      CXXBaseSpecifier &B = *Class->bases().begin();
//      return GetModifications(V.getStructBase(0), B.getType(), N);
//    }
//  }
//  FieldDecl *F = cast<FieldDecl>(Lookup.front());
//  return V.getStructField(F->getFieldIndex());
//}
//
//static bool CheckInjectionContexts(Sema &SemaRef, SourceLocation POI,
//                                   DeclContext *Injection,
//                                   DeclContext *Injectee) {
//  if (Injection->isRecord() && !Injectee->isRecord()) {
//    InvalidInjection(SemaRef, POI, 1, Injectee);
//    return false;
//  } else if (Injection->isFileContext() && !Injectee->isFileContext()) {
//    InvalidInjection(SemaRef, POI, 0, Injectee);
//    return false;
//  }
//  return true;
//}
//
//static bool CheckInjectionKind(Sema &SemaRef, SourceLocation POI,
//                               Decl *Injection, DeclContext *Injectee) {
//  // Make sure that injection is marginally sane.
//  if (VarDecl *Var = dyn_cast<VarDecl>(Injection)) {
//    if (Var->hasLocalStorage() && !Injectee->isFunctionOrMethod()) {
//      SemaRef.Diag(POI, diag::err_injecting_local_into_invalid_scope)
//        << Injectee->isRecord();
//      return false;
//    }
//  }
//  return true;
//}
////END
////ASUTTON ADDN:
///// Inject a fragment into the current context.
//bool Sema::InjectFragment(SourceLocation POI,
//                          QualType ReflectionTy,
//                          const APValue &ReflectionVal,
//                          Decl *Injectee,
//                          Decl *Injection) {
//  assert(isa<CXXRecordDecl>(Injection) || isa<NamespaceDecl>(Injection));
//  DeclContext *InjecteeDC = Decl::castToDeclContext(Injectee);
//  DeclContext *InjectionDC = Decl::castToDeclContext(Injection);
//
//  if (!CheckInjectionContexts(*this, POI, InjectionDC, InjecteeDC))
//    return false;
//
//  // Extract the captured values for replacement.
//  unsigned NumCaptures = ReflectionVal.getStructNumFields();
//  ArrayRef<APValue> Captures(None);
//  if (NumCaptures) {
//    const APValue *First = &ReflectionVal.getStructField(0);
//    Captures = ArrayRef<APValue>(First, NumCaptures);
//  }
//
//  CXXRecordDecl *Class = ReflectionTy->getAsCXXRecordDecl();
//  CXXFragmentDecl *Fragment = cast<CXXFragmentDecl>(Injection->getDeclContext());
//
//  ContextRAII Switch(*this, InjecteeDC, isa<CXXRecordDecl>(Injectee));
//
//  // Establish the injection context and register the substitutions.
//  InjectionContext *Cxt = new InjectionContext(*this, Fragment, InjecteeDC, Injection);
//  Cxt->AddDeclSubstitution(Injection, Injectee);
//  Cxt->AddPlaceholderSubstitutions(Fragment, Class, Captures);
//
//  // Inject each declaration in the fragment.
//  for (Decl *D : InjectionDC->decls()) {
//    // Never inject injected class names.
//    if (CXXRecordDecl *Class = dyn_cast<CXXRecordDecl>(D))
//      if (Class->isInjectedClassName())
//        continue;
//
//    // llvm::outs() << "BEFORE INJECT\n";
//    // D->dump();
//
//    Decl *R = Cxt->InjectDecl(D);
//    if (!R || R->isInvalidDecl()) {
//      // if (R && R->isInvalidDecl()) {
//      //   llvm::outs() << "INVALID INJECT\n";
//      //   R->dump();
//      // }
//      Injectee->setInvalidDecl(true);
//      continue;
//    }
//
//    // llvm::outs() << "AFTER INJECT\n";
//    // R->dump();
//  }
//
//  // If we're injecting into a class and have pending definitions, attach
//  // those to the class for subsequent analysis.
//  if (CXXRecordDecl *ClassInjectee = dyn_cast<CXXRecordDecl>(Injectee)) {
//    if (!Injectee->isInvalidDecl() && !Cxt->InjectedDefinitions.empty()) {
//      PendingClassMemberInjections.push_back(Cxt->Detach());
//      return true;
//    }
//  }
//
//  delete Cxt;
//  return !Injectee->isInvalidDecl();
//}
//
////END
////ASUTTON ADDN:
///// Clone a declaration into the current context.
//bool Sema::CopyDeclaration(SourceLocation POI,
//                           QualType ReflectionTy,
//                           const APValue &ReflectionVal,
//                           Decl *Injectee,
//                           Decl *Injection) {
//  DeclContext *InjectionDC = Injection->getDeclContext();
//  Decl *InjectionOwner = Decl::castFromDeclContext(InjectionDC);
//  DeclContext *InjecteeDC = Decl::castToDeclContext(Injectee);
//
//  // Don't copy injected class names.
//  if (CXXRecordDecl *Class = dyn_cast<CXXRecordDecl>(Injection))
//    if (Class->isInjectedClassName())
//      return true;
//
//  if (!CheckInjectionContexts(*this, POI, InjectionDC, InjecteeDC))
//    return false;
//
//  if (!CheckInjectionKind(*this, POI, Injection, InjecteeDC))
//    return false;
//
//  // Set up the injection context for the declaration. Note that we're
//  // going to replace references to the inectee with the current owner.
//  InjectionContext *Cxt = new InjectionContext(*this, InjecteeDC, Injection);
//  Cxt->AddDeclSubstitution(InjectionOwner, Injectee);
//
//  // Establish injectee as the current context.
//  ContextRAII Switch(*this, InjecteeDC, isa<CXXRecordDecl>(Injectee));
//
//  // Unpack the modification traits so we can apply them after generating
//  // the declaration.
//  DeclarationName Name(&Context.Idents.get("mods"));
//  const APValue &Traits = GetModifications(ReflectionVal, ReflectionTy, Name);
//  Cxt->setModifiers(Traits);
//
//  // Do the same for the rename member.
//  DeclarationName Rename(&Context.Idents.get("rename"));
//  const APValue &NameVal = GetModifications(ReflectionVal, ReflectionTy, Rename);
//  Cxt->setName(NameVal);
//
//  // llvm::outs() << "BEFORE CLONE\n";
//  // Injection->dump();
//
//  Decl* Result = Cxt->InjectDecl(Injection);
//  if (!Result || Result->isInvalidDecl()) {
//    Injectee->setInvalidDecl(true);
//    return false;
//  }
//
//  // llvm::outs() << "AFTER CLONING\n";
//  // Result->dump();
//
//  // If we're injecting into a class and have pending definitions, attach
//  // those to the class for subsequent analysis.
//  if (CXXRecordDecl *ClassInjectee = dyn_cast<CXXRecordDecl>(Injectee)) {
//    if (!Injectee->isInvalidDecl() && !Cxt->InjectedDefinitions.empty()) {
//      PendingClassMemberInjections.push_back(Cxt->Detach());
//      return true;
//    }
//  }
//  return !Injectee->isInvalidDecl();
//}

////DWR COMMENTED OUT:
//bool Sema::ApplyInjection(SourceLocation POI, InjectionInfo &II) {
//  // Get the injection declaration.
//  assert(II.ReflectionValue.isStruct() && "DWR: I guess I need to rethink this");
////  Decl *Injection = GetDeclFromReflection(*this, II.ReflectionType, POI); //DWR COMMENTED OUT
//  Decl *Injection = GetDeclFromReflection(*this, II.ReflectionValue.getStructField(0).getInt().getExtValue(), POI); //DWR REPLACEMENT
//
//
//  assert(II.InjecteeType.isNull() && "DWR: Need to fix code here if using nonnull InjecteeTypes");
////DWR COMMENTED OUT because GetDeclFromReflection has changed:
////  /// Get the injectee declaration. This is either the one specified or
////  /// the current context.
////  Decl *Injectee = nullptr;
////  if (!II.InjecteeType.isNull())
//////    Injectee = GetDeclFromReflection(*this, II.InjecteeType, POI);
////  else
////      Injectee = Decl::castFromDeclContext(CurContext);
////END
//  Decl *Injectee = Decl::castFromDeclContext(CurContext); //DWR REPLACEMENT
//
//  if (!Injectee)
//    return false;
//
//  // FIXME: Make sure that we can actually apply the injection to the
//  // target context. For example, we should only be able to extend fragments
//  // or classes currently being defined. We'll need to incorporate the kind
//  // of extension operator into the InjectionInfo.
//
//  // Apply the injection operation.
//  QualType Ty = II.ReflectionType;
//  const APValue &Val = II.ReflectionValue;
//////ASUTTON ADDN:
////  CXXRecordDecl *Class = Ty->getAsCXXRecordDecl();
////  if (Class->isFragment())
////    return InjectFragment(POI, Ty, Val, Injectee, Injection);
////  else
//////END
//    return CopyDeclaration(POI, Ty, Val, Injectee, Injection);
//}
////END

////DWR FIXME, this works but is a bit sloppy
//bool Sema::ApplyExpand(SourceLocation POI, ExpandInfo &II) {
//  auto Ty = II.NewSrcCodeType;
////  const APValue* Val = &II.NewSrcCodeValue; //OLD
//  const StringLiteral *StrLitVal = II.NewSrcCodeValue;


//////DWR OLD:
////  // Note that you cannot in fact pass struct names to an __queue_metaparse statement, notwithstanding the below branch.
////  // However whenever you pass struct MEMBER names, e.g. A::value, for some reason the requested APValue field
////  // result gets wrapped in an APValue struct with exactly one field: the one you requested.
////  // So in such a case we have to "unwrap" the APValue field.
////  if (Val->isStruct()) {
////    const RecordDecl *RD = Ty->getAs<RecordType>()->getDecl();
////    assert(!RD->field_empty() && "Expecting exactly one field; this has none!");
////    assert(++(RD->field_begin()) == RD->field_end() && "Expecting exactly one field; this has more than one!");
////    Val = &Val->getStructField(RD->field_begin()->getFieldIndex());
////  }
////  //NB no 'else'; the below is a new if statement (the above one just did any necessary 'unwrapping' of Val).
////  if (Val->isArray()) {
////    if (!Val->getArrayInitializedElt(0).isInt()) {
////      //DWR FIXME: turn this into a diag::err message, then return false: something like
////      // "invalid expand argument"
////      METAPARSE_DEBUG("ERROR: Array must be of ints!")
////      return false;
////    }
////    auto begin = APValueCharIter(Val);
////    auto end = begin + Val->getArraySize();

////    ///Iterate over APValues, casting each via dereference operator that calls getInt().getZExtValue(), to build string:
////    //DWR FIXME this is no good, lots of malloc traffic; at the very least need a cache or something.
////    std::string* str = new std::string();
////    for (auto cur = begin; cur != end; ++cur) {
////      str->push_back(*cur);
////    }
////    assert(str->back() == '\0' && "Expecting null-terminated string!");
////    //llvm::outs() << "Pushing string onto MetaSrcStrVec: " << *str << "\n";
////    PP.MetaSrcStrVec.push_back(str);
////    return true;
////  }
////  else if (Val->isLValue()) {
////    auto strLitVal = dyn_cast<StringLiteral>(Val->getLValueBase().get<const Expr*>());
////    assert(strLitVal && "Expected a const Expr* LValue -- perhaps need to change code to address alternate possibilities?");
////    auto strLit = strLitVal->getString();
////    std::string* str = new std::string(strLit.begin(), strLit.end()-strLit.begin());
////    assert(str->empty() || str->back() != '\0' && "Expecting non-null-terminated string.");
////    str->push_back('\0'); //Null-terminate
////    //llvm::outs() << "Pushing string onto MetaSrcStrVec: " << *str << "\n";
////    PP.MetaSrcStrVec.push_back(str);
////    return true;
////  } else {
////    //DWR FIXME clean this up
////    llvm::outs() << "Value to re-parse is neither an LValue nor an array!\n";
////    llvm::outs() << "--Kind = " << Val->getKind() << "\n";
////    llvm::outs() << "--Val string: \n--" << Val->getAsString(Context, Ty) << "\n\n\n";\
////    return false;
////  }
//}
////END DWR ADDN

//static void
//PrintDecl(Sema &SemaRef, Decl *D) {
//  PrintingPolicy PP = SemaRef.Context.getPrintingPolicy();
//  PP.TerseOutput = false;
//  D->print(llvm::errs(), PP);
//  llvm::errs() << '\n';
//}
//
//static void
//PrintType(Sema &SemaRef, Type *T) {
//  if (TagDecl *TD = T->getAsTagDecl())
//    return PrintDecl(SemaRef, TD);
//  PrintingPolicy PP = SemaRef.Context.getPrintingPolicy();
//  QualType QT(T, 0);
//  QT.print(llvm::errs(), PP);
//  llvm::errs() << '\n';
//}

//static bool
//ApplyDiagnostic(Sema &SemaRef, SourceLocation Loc, const APValue &Arg) {
//  ReflectedConstruct RC(Arg.getInt().getExtValue());
//  //DWR MOD: replaced if/else with switch...
//  switch (RC.getKind()) {
//    case RK_Decl:
//      PrintDecl(SemaRef, RC.getAsDeclaration());
//      break;
//    case RK_Type: {
//      PrintType(SemaRef, RC.getAsType());
//      break;
//    }
//    default:
//      llvm_unreachable("Attempting to print unhandled reflection kind");
//  }
//  return true;
//}



// DWR TODO: move all this stuff to SemaMetaparse.cpp








///// Check if there are any pending definitions of member functions for
///// this class or any of its nested class definitions. We can simply look
///// at the most recent injection; if it's D or declared inside D, then
///// the answer is yes. Otherwise the answer is no.
/////
///// We need to check for this whenever a class is completed during an
///// injection. We don't want to prematurely inject definitions.
/////
///// FIXME: It's likely that this wouldn't be necessarily if we integrated
///// injection contexts into the template instantiation context; they are
///// somewhat similar.
//bool Sema::HasPendingInjections(DeclContext *D) {
//  if (PendingClassMemberInjections.empty())
//    return false;
//  InjectionContext *Cxt = PendingClassMemberInjections.back();
//  assert(!Cxt->InjectedDefinitions.empty() && "bad injection queue");
//  InjectedDef& Def = Cxt->InjectedDefinitions.front();
//  DeclContext *DC = Def.Injected->getDeclContext();
//  while (!DC->isFileContext()) {
//    if (DC == D)
//      return true;
//    DC = DC->getParent();
//  }
//  return false;
//}
//
//void Sema::InjectPendingDefinitions() {
//  while (!PendingClassMemberInjections.empty()) {
//    InjectionContext *Cxt = PendingClassMemberInjections.back();
//    PendingClassMemberInjections.pop_back();
//    InjectPendingDefinitions(Cxt);
//  }
//}
//
//void Sema::InjectPendingDefinitions(InjectionContext *Cxt) {
//  Cxt->Attach();
//  for (InjectedDef& Def : Cxt->InjectedDefinitions)
//    InjectPendingDefinition(Cxt,
//            Def.Fragment,
//            Def.Injected);
//  delete Cxt;
//}
//
//void Sema::InjectPendingDefinition(InjectionContext *Cxt,
//                                   Decl *Frag,
//                                   Decl *New) {
//  // Switch to the class enclosing the newly injected declaration.
//  ContextRAII ClassCxt (*this, New->getDeclContext());
//
//  if (FieldDecl *OldField = dyn_cast<FieldDecl>(Frag)) {
//    FieldDecl *NewField = cast<FieldDecl>(New);
//    ExprResult Init = Cxt->TransformExpr(OldField->getInClassInitializer());
//    if (Init.isInvalid())
//      NewField->setInvalidDecl();
//    else
//      NewField->setInClassInitializer(Init.get());
//  }
//  else if (CXXMethodDecl *OldMethod = dyn_cast<CXXMethodDecl>(Frag)) {
//    CXXMethodDecl *NewMethod = cast<CXXMethodDecl>(New);
//    ContextRAII MethodCxt (*this, NewMethod);
//    StmtResult Body = Cxt->TransformStmt(OldMethod->getBody());
//    if (Body.isInvalid())
//      NewMethod->setInvalidDecl();
//    else
//      NewMethod->setBody(Body.get());
//  }
//}
////END
////ASUTTON ADDN:
//Sema::DeclGroupPtrTy Sema::ActOnCXXGeneratedTypeDecl(SourceLocation UsingLoc,
//                                                     bool IsClass,
//                                                     SourceLocation IdLoc,
//                                                     IdentifierInfo *Id,
//                                                     Expr *Generator,
//                                                     Expr *Reflection) {
//  // Create the generated type.
//  TagTypeKind TTK = IsClass ? TTK_Class : TTK_Struct;
//  CXXRecordDecl *Class = CXXRecordDecl::Create(Context, TTK, CurContext,
//                                               IdLoc, IdLoc, Id);
//  Class->setImplicit(true);
//
//  // FIXME: Actually use the current access specifier.
//  if (isa<CXXRecordDecl>(CurContext))
//    Class->setAccess(AS_public);
//
//  CurContext->addDecl(Class);
//  StartDefinition(Class);
//
//  // PushDeclContext(CurScope, Class);
//  ContextRAII ClassContext(*this, Class);
//
//  // FIXME: If the reflection (ref) is a fragment DO NOT insert the
//  // prototype. A fragment is NOT a type.
//
//  // Insert 'using prototype = typename(ref)'.
//  IdentifierInfo *ProtoId = &Context.Idents.get("prototype");
//  QualType ProtoTy = BuildReflectedType(*this, IdLoc, Reflection);
//  TypeSourceInfo *ProtoTSI = Context.getTrivialTypeSourceInfo(ProtoTy);
//  Decl *Alias = TypeAliasDecl::Create(Context, Class, IdLoc, IdLoc,
//                                      ProtoId, ProtoTSI);
//  Alias->setImplicit(true);
//  Alias->setAccess(AS_public);
//  Class->addDecl(Alias);
//
//  // Insert 'constexpr { <gen>(<ref>); }'.
//  unsigned ScopeFlags;
//  Decl *CD = ActOnConstexprDecl(nullptr, UsingLoc, ScopeFlags);
//  CD->setImplicit(true);
//  CD->setAccess(AS_public);
//
//  ActOnStartConstexprDecl(nullptr, CD);
//
//  // Build the call to <gen>(<ref>)
//  Expr *Args[] {Reflection};
//  ExprResult Call = ActOnCallExpr(nullptr, Generator, IdLoc, Args, IdLoc);
//  if (Call.isInvalid()) {
//    ActOnConstexprDeclError(nullptr, CD);
//    Class->setInvalidDecl(true);
//    CompleteDefinition(Class);
//    PopDeclContext();
//  }
//
//  Stmt* Body = new (Context) CompoundStmt(Context, Call.get(), IdLoc, IdLoc);
//  ActOnFinishConstexprDecl(nullptr, CD, Body);
//
//  CompleteDefinition(Class);
//  PopDeclContext();
//
//  return DeclGroupPtrTy::make(DeclGroupRef(Class));
//}
////END ASUTTON ADDN
