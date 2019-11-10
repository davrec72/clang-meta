//
// Created by David Rector on 2019-04-24.
//

#ifndef LLVM_REFLECTEDTYPEETC_H
#define LLVM_REFLECTEDTYPEETC_H

#include "clang/Sema/SemaInternal.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/Expr.h" //for EvalResult
#include "clang/Lex/Preprocessor.h" //for PP.getIdentifierTable()

namespace clang {

/// \brief Returns the cppx namespace if a suitable header has been included.
/// If not, a diagnostic is emitted, and \c nullptr is returned.
static NamespaceDecl *RequireCppxNamespace(Sema &S, SourceLocation Loc) {
  IdentifierInfo *CppxII = &S.PP.getIdentifierTable().get("cppx");
  LookupResult R(S, CppxII, Loc, S.LookupNamespaceName);
  S.LookupQualifiedName(R, S.Context.getTranslationUnitDecl());
  if (!R.isSingleResult()) {
    S.Diag(Loc, diag::err_need_header_before_dollar);
    return nullptr;
  }
  NamespaceDecl *Cppx = R.getAsSingle<NamespaceDecl>();
  assert(Cppx && "cppx is not a namespace");
  return Cppx;
}

/// \brief Same as RequireCppxNamespace, but requires cppx::meta.
static NamespaceDecl *RequireCppxMetaNamespace(Sema &S, SourceLocation Loc) {
  NamespaceDecl *Cppx = RequireCppxNamespace(S, Loc);
  if (!Cppx)
    return nullptr;

  // Get the cppx::meta namespace.
  IdentifierInfo *MetaII = &S.PP.getIdentifierTable().get("meta");
  LookupResult R(S, MetaII, Loc, S.LookupNamespaceName);
  S.LookupQualifiedName(R, Cppx);
  if (!R.isSingleResult()) {
    S.Diag(Loc, diag::err_need_header_before_dollar);
    return nullptr;
  }
  NamespaceDecl *Meta = R.getAsSingle<NamespaceDecl>();
  assert(Meta && "cppx::meta is not a namespace");
  return Meta;
}

//DWR ADDN
/// \brief Same as RequireCppxMetaNamespace, but requires cppx::meta::refldetail.
static NamespaceDecl *RequireReflNS(Sema &S, SourceLocation Loc) {
  NamespaceDecl *CppxMeta = RequireCppxMetaNamespace(S, Loc);
  if (!CppxMeta)
    return nullptr;

  // Get the cppx::meta::refldetail namespace.
  IdentifierInfo *MetaII = &S.PP.getIdentifierTable().get("refldetail");
  LookupResult R(S, MetaII, Loc, S.LookupNamespaceName);
  S.LookupQualifiedName(R, CppxMeta);
  if (!R.isSingleResult()) {
    S.Diag(Loc, diag::err_need_header_before_dollar);
    return nullptr;
  }
  NamespaceDecl *Meta = R.getAsSingle<NamespaceDecl>();
  assert(Meta && "cppx::meta::refldetail is not a namespace");
  return Meta;
}
//END

/// \brief The kind of source code construct reflected by
/// This value is packed into the low-order bits of each reflected pointer.
///
enum InitialReflectionObjKind {
    IRK_Decl = 1,
    IRK_Type = 2,
//    RK_Base = 3,
//    RK_QualType = 4, //DWR ADDN
//    RK_Expr = 5, //DWR ADDN
//    RK_Stmt = 6, //DWR ADDN
};

/// Stores a reflection by stuffing bits into pointer.
using InitialReflectionValue = llvm::PointerSumType<
  InitialReflectionObjKind
, llvm::PointerSumTypeMember<IRK_Decl, Decl *>
, llvm::PointerSumTypeMember<IRK_Type, Type *>
//, llvm::PointerSumTypeMember<RK_Base, CXXBaseSpecifier *>
//, llvm::PointerSumTypeMember<RK_QualType, detail::rbk_value_dummy *> //DWR ADDN
//, llvm::PointerSumTypeMember<RK_Expr, Expr *>,//DWR ADDN
//, llvm::PointerSumTypeMember<RK_Stmt, Stmt *> //DWR ADDN
>;


//static InitialReflectionValue createReflectionValue(CXXBaseSpecifier *B) {
//  return InitialReflectionValue::create<IRK_Base>(B);
//}
//
//static InitialReflectionValue createReflectionValue(QualType QT) {
//  static_assert(sizeof(QualType) <= sizeof(Type *),
//                "I assumed QualType was the size of a pointer, as the manual stated.");
//
//  // DWR: DANGEROUS cast below -- but needed to pass QualType off as a pointer.
//  // If this is an issue, need to also fix getAsQualType() etc. in ReflectedConstruct.
//  // Probably best to come up with a union type that can mix
//  // pointers and non-pointers, instead of using PointerSumTypeMember.
//  // Note too that detail::rbk_value_dummy * is a dummy type,
//  // used only because void * /int * didn't work.
//  detail::rbk_value_dummy * QTdangerouscast2ptr =
//          *(detail::rbk_value_dummy **)(&QT);
//
//  return ReflectionValue::create<RK_QualType>(QTdangerouscast2ptr);
//}
//static ReflectionValue createReflectionValue(Expr *E) {
//  return ReflectionValue::create<RK_Expr>(E);
//}
//static ReflectionValue createReflectionValue(Stmt *S) {
//  return ReflectionValue::create<RK_Stmt>(S);
//}



using InitialReflectionPair = std::pair<InitialReflectionObjKind, void *>;

/// A wrapper around an exploded reflection pair.
struct InitialReflectedConstruct {
    bool Valid;
  InitialReflectionPair P;

  InitialReflectedConstruct()
            : Valid(false), P() { }

  InitialReflectedConstruct(std::uintptr_t V)
            : Valid(true), P(Explode(V)) { }

    /// Returns true if the reflection is valid.
    bool isValid() const { return Valid; }

    /// Returns true if the reflection is invalid.
    bool isInvalid() const { return !Valid; }

    /// Converts to true when the reflection is valid.
    explicit operator bool() const { return Valid; }

    /// \brief The kind of construct reflected.
    InitialReflectionObjKind getKind() const { return P.first; }

    /// Returns true if this is a declaration.
    bool isDeclaration() const { return P.first == IRK_Decl; }

    /// Returns true if this is a type reflection.
    bool isType() const { return P.first == IRK_Type; }

//  /// \brief True if a reflection of a base class specifier.
//  bool isBaseSpecifier() const { return P.first == IRK_Base; }
//
////DWR ADDN:
//    /// Returns true if this is a qualified/pointer type reflection.
//    bool isQualType() const { return P.first == RK_QualType; }
//
//    /// Returns true if this is an expression reflection.
//    bool isExpr() const { return P.first == RK_Expr; }
//
//    /// Returns true if this is a statement reflection (expressions not included).
//    bool isStmt() const { return P.first == RK_Stmt; }
////END

    /// Returns the reflected declaration or nullptr if not a declaration.
    Decl *getAsDeclaration() {
      return isDeclaration() ? (Decl *)P.second : nullptr;
    }

    /// Returns the reflected type or nullptr if not a type.
    Type *getAsType() {
      return isType() ? (Type *)P.second : nullptr;
    }

//  /// \brief The reflected base class specifier or nullptr.
//  CXXBaseSpecifier *getAsBaseSpecifier() {
//    return isBaseSpecifier() ? (CXXBaseSpecifier *)P.second : nullptr;
//  }
//
////DWR ADDN:
//    /// Returns the reflected qualified type or nullptr if not a qualtype.
//    QualType getAsQualType() {
//      assert(QualType().isNull()
//             && "I assumed dflt-constructed QualTypes were null");
//      static_assert(sizeof(QualType) <= sizeof(P.second),
//              "QualType won't fit!");
//      //DWR POSSIBLE FIXME: possibly super-dangerous cast below;
//      // but so long as static_assert above works I assume it will work.
//      //DWR TEST
//      return isQualType() ? *(QualType*)(&P.second) : QualType();
//    }
//
//    /// Returns the reflected expression or nullptr if not an Expr.
//    Expr *getAsExpr() {
//      return isExpr() ? (Expr *)P.second : nullptr;
//    }
//
//    /// Returns the reflected statment or nullptr if not an Stmt (Exprs not included).
//    Stmt *getAsStmt() {
//      return isStmt() ? (Stmt *)P.second : nullptr;
//    }
////END

    static InitialReflectionPair Explode(std::uintptr_t N) {
      // Look away. I'm totally breaking abstraction.
      using Helper = llvm::detail::PointerSumTypeHelper<
        InitialReflectionObjKind
      , llvm::PointerSumTypeMember<IRK_Decl, Decl *>
      , llvm::PointerSumTypeMember<IRK_Type, Type *>
//      , llvm::PointerSumTypeMember<ORK_Base, CXXBaseSpecifier *>
//      , llvm::PointerSumTypeMember<RK_QualType, QualType *> //DWR ADDN
//      , llvm::PointerSumTypeMember<RK_Expr, Expr *> //DWR ADDN
//      , llvm::PointerSumTypeMember<RK_Stmt, Stmt *> //DWR ADDN
      >;

      InitialReflectionObjKind K = (InitialReflectionObjKind)(N & Helper::TagMask);
      void *P = (void *)(N & Helper::PointerMask);
      return {K, P};
    }
};



///// Information supporting reflection operations.
/////
//struct Reflector {
//  Sema &S;
//  SourceLocation KWLoc;
//  SourceLocation RParenLoc;
//  ArrayRef<Expr *> Args;
//  ArrayRef<llvm::APSInt> Vals;
//  unsigned firstpmidx; //DWR ADDN
//
//  //Sema::ActOnReflectionTrait interface function:
//  template<typename T>
//  ExprResult Reflect(
////          ReflectionTrait RT,
//          T t) {
////    switch (RT) {
////      case URT_ReflectPrint:
////        return ReflectPrint(t);
////      case BRT_ReflectProp:
//        return ReflectProp(Vals[1].getZExtValue(), t);
////    }
////    llvm_unreachable("The only supported reflection traits are "
////                     "__reflect_print(X) and __reflect_prop(X,N).");
//  }
//
////ReflectPrint:
//  ExprResult ReflectPrint(Decl *D);
//  ExprResult ReflectPrint(CXXBaseSpecifier *B);
//  ExprResult ReflectPrint(QualType Q);
//  ExprResult ReflectPrint(Type *T);
//
////  Note that we must include ReflectProp overloads for leaf AND ABSTRACT nodes:
//
////DWR TEMP dummy implems...
//  ExprResult ReflectProp(unsigned MemNum, Decl *D) { return ExprResult(); }
//  ExprResult ReflectProp(unsigned MemNum, Type *T) { return ExprResult(); }
//  ExprResult ReflectProp(unsigned MemNum, Stmt *M) { return ExprResult(); }
//  ExprResult ReflectProp(unsigned MemNum, CXXBaseSpecifier *B) { return ExprResult(); }
//  ExprResult ReflectProp(unsigned MemNum, QualType QT) { return ExprResult(); }
//
//  //Note: the root Expr * case will be created in the EXPR macro
//
////  ExprResult ReflectProp(unsigned MemNum, DeclContext *D);
////#define DECL(DERIVED, BASE)\
////  ExprResult ReflectProp(unsigned MemNum, DERIVED##Decl *D);
////#include "clang/AST/DeclNodes.inc"
////
////#define TYPE(DERIVED, BASE)\
////  ExprResult ReflectProp(unsigned MemNum, DERIVED##Type *T);
////#include "clang/AST/TypeNodes.def"
////
////#define EXPR(DERIVED, BASE)\
////  ExprResult ReflectProp(unsigned MemNum, DERIVED *E);
////#define STMT(DERIVED, BASE)\
////  ExprResult ReflectProp(unsigned MemNum, DERIVED *M);
////#include "clang/AST/StmtNodes.inc"
//
//};
//
//// DWR EXTRACTION (def in SemaReflect.cpp)
//ExprResult RouteToProperReflectCall(ReflectedConstruct C, Reflector &R
////        , ReflectionTrait Kind
//);
////END


///*-----------------------------------------------------------------*/
/////  Require...
//
///// \brief Returns the cppx namespace if a suitable header has been included.
///// If not, a diagnostic is emitted, and \c nullptr is returned.
/////
//// TODO: We should probably cache this the same way that we do
//// with typeid (see CXXTypeInfoDecl in Sema.h).
//static NamespaceDecl *RequireCppxNamespace(Sema &S, SourceLocation Loc) {
//  IdentifierInfo *CppxII = &S.PP.getIdentifierTable().get("cppx");
//  LookupResult R(S, CppxII, Loc, S.LookupNamespaceName);
//  S.LookupQualifiedName(R, S.Context.getTranslationUnitDecl());
//  if (!R.isSingleResult()) {
//    S.Diag(Loc, diag::err_need_header_before_dollar);
//    return nullptr;
//  }
//  NamespaceDecl *Cppx = R.getAsSingle<NamespaceDecl>();
//  assert(Cppx && "cppx is not a namespace");
//  return Cppx;
//}
//
///// \brief Same as RequireCppxNamespace, but requires cppx::meta.
//static NamespaceDecl *RequireCppxMetaNamespace(Sema &S, SourceLocation Loc) {
//  NamespaceDecl *Cppx = RequireCppxNamespace(S, Loc);
//  if (!Cppx)
//    return nullptr;
//
//  // Get the cppx::meta namespace.
//  IdentifierInfo *MetaII = &S.PP.getIdentifierTable().get("meta");
//  LookupResult R(S, MetaII, Loc, S.LookupNamespaceName);
//  S.LookupQualifiedName(R, Cppx);
//  if (!R.isSingleResult()) {
//    S.Diag(Loc, diag::err_need_header_before_dollar);
//    return nullptr;
//  }
//  NamespaceDecl *Meta = R.getAsSingle<NamespaceDecl>();
//  assert(Meta && "cppx::meta is not a namespace");
//  return Meta;
//}
//
///// \brief Returns the class with the given name in the
///// std::[experimental::]meta namespace.
/////
///// If no such class can be found, a diagnostic is emitted, and \c nullptr
///// returned.
/////
//// TODO: Cache these types so we don't keep doing lookup. In on the first
//// lookup, cache the names of ALL meta types so that we can easily check
//// for appropriate arguments in the reflection traits.
//static ClassTemplateDecl *RequireReflectionType(Sema &S, SourceLocation Loc,
//                                            char const *Name) {
//  NamespaceDecl *Meta = RequireCppxMetaNamespace(S, Loc);
//  if (!Meta)
//    return nullptr;
//
//  // Get the corresponding reflection class.
//  IdentifierInfo *TypeII = &S.PP.getIdentifierTable().get(Name);
//  LookupResult R(S, TypeII, SourceLocation(), S.LookupAnyName);
//  S.LookupQualifiedName(R, Meta);
//    ClassTemplateDecl *Decl = R.getAsSingle<ClassTemplateDecl>();
//  if (!Decl) {
//    S.Diag(Loc, diag::err_need_header_before_dollar);
//    return nullptr;
//  }
//  return Decl;
//}


/*-----------------------------------------------------------------*/
//
// EvaluateReflection etc.
//
//static ExprResult ValueReflectionError(Sema &SemaRef, SourceLocation Loc) {
//  SemaRef.Diag(Loc, diag::err_reflection_not_a_value);
//  return ExprResult(true);
//}

//static ReflectedConstruct EvaluateReflection(Sema &S, QualType T, SourceLocation Loc) {
//  T = S.Context.getCanonicalType(T);
//  CXXRecordDecl *Class = T->getAsCXXRecordDecl();
//  if (!Class) {
//    ValueReflectionError(S, Loc);
//    return ReflectedConstruct();
//  }
//
////  //ASUTTON ADDN
////  if (Class->isFragment()) {
////    // If the class is a fragment, then uses its base type for the
////    // evaluation.
////    assert(Class->getNumBases() == 1 && "invalid injection");
////    CXXBaseSpecifier &Base = *Class->bases_begin();
////    Class = Base.getType()->getAsCXXRecordDecl();
////  }
////  //END
//
//  //DWR FIXME now that we're not using templates for reflections this needs to change.
//
////  Class->findFirstNamedDataMember();
//
//  // If the type is a reflection...
//  if (!isa<ClassTemplateSpecializationDecl>(Class)) {
//    S.Diag(Loc, diag::err_not_a_reflection);
//    return ReflectedConstruct();
//  }
//  ClassTemplateSpecializationDecl *Spec =
//    cast<ClassTemplateSpecializationDecl>(Class);
//
//  // Make sure that this is actually a metaclass.
//  DeclContext* Owner = Spec->getDeclContext();
//  if (Owner->isInlineNamespace())
//    Owner = Owner->getParent();
//  if (!Owner->Equals(RequireCppxMetaNamespace(S, Loc))) {
//    S.Diag(Loc, diag::err_not_a_reflection);
//    return ReflectedConstruct();
//  }
//
//  const TemplateArgumentList& Args = Spec->getTemplateArgs();
//  if (Args.size() == 0) {
//    ValueReflectionError(S, Loc);
//    return ReflectedConstruct();
//  }
//
//  const TemplateArgument &Arg = Args.get(0);
//  if (Arg.getKind() != TemplateArgument::Integral) {
//    ValueReflectionError(S, Loc);
//    return ReflectedConstruct();
//  }
//
//  // Decode the specialization argument.
//  llvm::APSInt Data = Arg.getAsIntegral();
//  return ReflectedConstruct(Data.getExtValue());
//}

//static InitialReflectedConstruct EvaluateReflection(Sema &S, Expr *E) {
//  if (E->getType().getCanonicalType().getTypePtr() == S.Context.getIntPtrType().getTypePtr()) {
//    // If this looks like an encoded integer, then evaluate it as such.
//    //
//    // FIXME: This is *really* unsafe.
//    Expr::EvalResult Result;
//    if (!E->EvaluateAsRValue(Result, S.Context)) {
//      E->dump();
//      S.Diag(E->getExprLoc(), diag::err_expr_not_ice) << 1;
//      return InitialReflectedConstruct();
//    }
//    return InitialReflectedConstruct(Result.Val.getInt().getExtValue());
//  }
////DWR ADDN:
//  E->dump();
//  S.Diag(E->getLocStart(), diag::err_expected) << "a uintptr_t";
//  return InitialReflectedConstruct();
////END
////  return EvaluateReflection(S, E->getType(), E->getExprLoc()); //DWR COMMENTED OUT
//}



/// FIXME: Move this to ASTContext.
static bool isReflectionType(Sema &S, QualType T) {
  if (CXXRecordDecl *Class = T->getAsCXXRecordDecl()) {

////ASUTTON ADDN
//    // Fragments are reflections.
//    if (Class->isFragment())
//      return true;
////END

//DWR COMMENTED OUT:
//    // Reflections are specializations.
//    if (!isa<ClassTemplateSpecializationDecl>(Class))
//      return false;
//END

    // Reflections are defined in the meta namespace.
    DeclContext* Owner = Class->getDeclContext();
    if (Owner->isInlineNamespace())
      Owner = Owner->getParent();

    // FIXME: Don't emit errors if this fails, just return false.
    if (!Owner->Equals(RequireReflNS(S, SourceLocation()))) //DWR changed from RequireCppxMetaNamespace
      return false;

    // FIXME: We should actually test the name of the class. This is mostly a
    // heuristic right now.
    return true;
  }

  return false;
}


/// Diagnose a type reflection error and return a type error.
static QualType TypeReflectionError(Sema &SemaRef, Expr *E) {
  SemaRef.Diag(E->getLocStart(), diag::err_reflection_not_a_type)
          << E->getSourceRange();
  return QualType();
}


// Compute the type of a reflection.
//
// FIXME: A lot of this code seems to be duplicated from elsewhere.
static QualType GetNonDependentReflectedType(Sema &SemaRef, Expr *E)
{
  QualType Ty = E->getType();
  if (!isReflectionType(SemaRef, Ty)) {
    SemaRef.Diag(E->getLocStart(), diag::err_expected) << "reflection";
    return QualType();
  }

//  // Perform an lvalue-to-value conversion so that we get an rvalue in
//  // evaluation.
//  if (E->isGLValue()) {
//    SemaRef.UpdateMarkingForLValueToRValue(E);
//    //^ DWR: added this to try to solve assert(MaybeODRUseExpr.empty()) issue that arises when using __queue_metaparse in constexpr functions;
//    // I think it's working but not 100% sure.
//    E = ImplicitCastExpr::Create(SemaRef.Context, Ty,
//                                          CK_LValueToRValue, E,
//                                          nullptr, VK_RValue);
//  }

//DWR REPLACEMENT:
  Expr::EvalResult Result;
  if (!E->EvaluateAsRValue(Result, SemaRef.Context)) {
    SemaRef.Diag(E->getExprLoc(), diag::err_expected) << "reflection constant expression";
    return QualType();
  }

  assert(Result.Val.isStruct()
  && "DWR I assumed the reflection type would be a struct since isReflectionType passed..."
     "need to improve the isReflectionType test, or turn this assert into a test (or"
     "account for LValues or whatever)");

  llvm::APSInt Data = Result.Val.getStructField(0).getInt();
//END

////DWR COMMENTED OUT:
//  // Get the type of the reflection.
//  QualType T = E->getType();
//  if (AutoType *D = T->getContainedAutoType()) {
//    T = D->getDeducedType();
//    if (!T.getTypePtr())
//      llvm_unreachable("Undeduced type reflection");
//  }
//
//  // Unpack information from the expression.
//  CXXRecordDecl *Class = T->getAsCXXRecordDecl();
//  if (!Class)
//    return TypeReflectionError(SemaRef, E);
//  if (!isa<ClassTemplateSpecializationDecl>(Class)) {
//    // This could be a fragment; search the base list for the type.
//    if (Class->getNumBases() == 0)
//      return TypeReflectionError(SemaRef, E);
//    Class = (*Class->bases_begin()).getType()->getAsCXXRecordDecl();
//  }
//  if (!Class)
//    return TypeReflectionError(SemaRef, E);
//
//  ClassTemplateSpecializationDecl *Spec =
//      cast<ClassTemplateSpecializationDecl>(Class);
//
//  // FIXME: We should verify that this Class actually a meta class object so
//  // that we aren't arbitrarily converting integers into addresses.
//
//  const TemplateArgumentList &Args = Spec->getTemplateArgs();
//  if (Args.size() == 0)
//    return TypeReflectionError(SemaRef, E);
//  const TemplateArgument &Arg = Args.get(0);
//  if (Arg.getKind() != TemplateArgument::Integral)
//    return TypeReflectionError(SemaRef, E);
//
//  // Decode the specialization argument as a type.
//  llvm::APSInt Data = Arg.getAsIntegral();
////END



  InitialReflectedConstruct C(Data.getExtValue());

  if (Type *T = C.getAsType()) {
    // Returns the referenced type. For example:
    //
    //    typename($int) x; // int x
    QualType QT(T, 0);
    return SemaRef.Context.getReflectedType(E, QT);
  }
//DWR COMMENTED OUT: this seems to just duplicate decltype(x) functionality, do we really need it?
//  else if (Decl *D = C.getAsDeclaration()) {
//    // Returns the type of the referenced declaration. For example:
//    //    char x;
//    //    typename($x) y; // char y
//    if (!isa<ValueDecl>(D)) {
//      SemaRef.Diag(E->getLocStart(), diag::err_reflection_not_a_typed_decl)
//                   << E->getSourceRange();
//      return QualType();
//    }
//    return SemaRef.Context.getReflectedType(E, cast<ValueDecl>(D)->getType());
//  }
//END
  else
    return TypeReflectionError(SemaRef, E);
//    llvm_unreachable("expression type not implemented");
}

/// Evaluates the given expression and yields the computed type.
LLVM_ATTRIBUTE_UNUSED static QualType BuildReflectedType(Sema &S, SourceLocation TypenameLoc, Expr *E) {
  QualType T;
  if (E->isTypeDependent())
    return S.Context.getReflectedType(E, S.Context.DependentTy);
  else
    return GetNonDependentReflectedType(S, E);
}

} //namespace clang

#endif //LLVM_REFLECTEDTYPEETC_H
