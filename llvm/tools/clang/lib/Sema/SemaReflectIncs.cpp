//DWR ADDN
//===--- SemaReflectB.cpp - Semantic Analysis for Reflection --------------===//
//
//  This file implements semantic analysis for C++ reflection alongside
//  SemaReflect.cpp; however because the two use some very large .inc files
//  (particularly this file), we split them up to try to reduce the recompile
//  time for small changes.
//
//===----------------------------------------------------------------------===//

//DWR TODO: rename this SemaReflectB.cpp for clarity.

#include "TypeLocBuilder.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/ReflectionIncludes.hpp"
//#include "clang/AST/PrettyPrinter.h" //DWR ADDN for setPrintingPolicy
#include "clang/Basic/ContainerGetNth.h" //DWR ADDN
//#include "clang/Lex/MetaparseDebug.h" //DWR ADDN
//#include "clang/Lex/Preprocessor.h"
//#include "clang/Parse/CDContextVars.h" //DWR ADDN
//#include "clang/Parse/Parser.h" //DWR ADDN
//#include "clang/Parse/RAIIObjectsForParser.h" //DWR ADDN
#include "clang/Sema/Initialization.h"
#include "clang/Sema/SemaInternal.h"
//#include "clang/Sema/ScopeInfo.h" //DWR addition, needed for LambdaScopeInfo def

#include "reflection_incs/ReflectionHeaderID.h" //DWR ADDN

#include "ReflectedTypeEtc.h" //RequireReflNs

using namespace clang;

/// Assigns unique MemNums to each method/field name
// DWR TODO rename all MemNums instances to TraitNum or something
namespace refl {
# include "reflection_incs/ReflInfoNamespaces.inc"
}

/// \brief Identifies the AST class reflected.
enum ReflectionObjKind {
# include "reflection_incs/ReflectionObjKindList.inc"
  MAX_REFLOBJKIND
};

static unsigned getTotalMemNums(ReflectionObjKind RK) {
  switch (RK) {

#   include "reflection_incs/ReflKindGetTotalMemNumCases.inc"

    default:
      llvm_unreachable("Unhandled ReflectionObjKind");
  }
}

static std::string to_string(ReflectionObjKind RK) {
  switch (RK) {

#   include "reflection_incs/ReflKindToStringCases.inc"

  default:
    llvm_unreachable("Unhandled ReflectionObjKind");
  }
}

LLVM_ATTRIBUTE_UNUSED static std::string to_string(ReflectionTraitKind TraitKind) {
  switch (TraitKind) {
    case RTK_prop:        return "__reflect_prop";
    case RTK_range_nth:   return "__reflect_range_nth";
    case RTK_range_size:  return "__reflect_range_size";
    case RTK_cast:        return "__reflect_cast";
  }
}






/*-----------------------------------------------------------------*/
///  BuildReflection

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// First, Sema methods and helper functions for looking up the
// templates etc. defined in the client's reflection header...
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



DeclContext *Sema::GetClientReflDC(SourceLocation Loc) {
  if (!ClientReflDC) {
    ClientReflDC = RequireReflNS(*this, Loc);
    if (ClientReflDC) {
      // Check if the REFLECTION_HEADER_ID matches your CMAKE_GEN_REFLHEADERID; if you don't or cannot
      // verify a match, return nullptr.
      IdentifierInfo *TypeII = &PP.getIdentifierTable().get("__reflheaderid__");
      LookupResult R(*this, TypeII, SourceLocation(), LookupAnyName);
      LookupQualifiedName(R, ClientReflDC);
      VarDecl *vd = R.getAsSingle<VarDecl>();
      if (!vd) {
//        llvm::outs() << "\n[DWR DEBUG] Did not find client __reflheaderid__\n";
        Diag(Loc, diag::err_need_header_before_dollar);
        return nullptr;
      }
      Expr *InitExpr = vd->getInit();
      Expr::EvalResult Result;
      if (!InitExpr->EvaluateAsRValue(Result, Context)) {
        Diag(InitExpr->getExprLoc(), diag::err_expr_not_ice) << 1;
        return nullptr;
      }
      if (Result.Val.getInt().getZExtValue() != CMAKE_GEN_REFLHEADERID) {
        Diag(InitExpr->getExprLoc(), diag::err_wrong_refl_header)
                << std::to_string(Result.Val.getInt().getZExtValue())
                << std::to_string(CMAKE_GEN_REFLHEADERID);
        return nullptr;
      }
    }
  }
  return ClientReflDC;
}

ClassTemplateDecl *Sema::getClientReflPtrwrpCTD(SourceLocation Loc) {
  if (!ClientReflPtrwrpCTD) {
    if (auto ReflNs = GetClientReflDC(Loc)) {
      // Get the corresponding reflection class.
      IdentifierInfo *TypeII = &PP.getIdentifierTable().get("ptrwrp");
      LookupResult R(*this, TypeII, SourceLocation(), LookupAnyName);
      LookupQualifiedName(R, ReflNs);
      ClientReflPtrwrpCTD = R.getAsSingle<ClassTemplateDecl>();
      if (!ClientReflPtrwrpCTD) {
//        llvm::outs() << "\n[DWR DEBUG] Did not find ptrwrp\n";
        Diag(Loc, diag::err_invalid_refl_header_missing) << "the ptrwrp<T> template";
        return nullptr;
      }
    }
  }
  return ClientReflPtrwrpCTD;
}

LLVM_ATTRIBUTE_UNUSED
static DeclContext *LookupDC(
        Sema &S, SourceLocation Loc, DeclContext *enclctx, const char *Name) {
  if (!enclctx)
    return nullptr;

  IdentifierInfo *TypeII = &S.PP.getIdentifierTable().get(Name);
  LookupResult R(S, TypeII, SourceLocation(), S.LookupAnyName);
  S.LookupQualifiedName(R, enclctx);
  DeclContext *DC = R.getAsSingle<DeclContext>();
  if (!DC) {
    S.Diag(Loc, diag::err_invalid_refl_header_missing) << Name << "(a DeclContext, presumably a class)";
    return nullptr;
  }
  return DC;
}

LLVM_ATTRIBUTE_UNUSED
static ClassTemplateDecl *LookupCTD(
        Sema &S, SourceLocation Loc, DeclContext *enclctx, const char *Name) {
  if (!enclctx)
    return nullptr;

  IdentifierInfo *TypeII = &S.PP.getIdentifierTable().get(Name);
  LookupResult R(S, TypeII, SourceLocation(), S.LookupAnyName);
  S.LookupQualifiedName(R, enclctx);
  ClassTemplateDecl *CTD = R.getAsSingle<ClassTemplateDecl>();
  if (!CTD) {
    auto enclctxND = cast<NamedDecl>(Decl::castFromDeclContext(enclctx));
    assert(enclctxND);
    S.Diag(Loc, diag::err_invalid_refl_header_missing)
            << enclctxND->getName() << "::" << Name << "<...>";
    return nullptr;
  }
  return CTD;
}

LLVM_ATTRIBUTE_UNUSED
static ClassTemplateDecl *LookupReflectionCTD(
        Sema &S, SourceLocation Loc,
        ReflectionObjKind RK, class DeclContext *cppxmeta) {
  auto ReflNs = S.GetClientReflDC(Loc);
  if (!ReflNs)
    return nullptr;
  switch (RK) {
#   include "reflection_incs/ReflectionCTDLookupCases.inc"
    default:
      llvm::errs() << "Unhandled ReflectionObjKind: " << to_string(RK) << "\n";
      llvm_unreachable("Unhandled ReflectionObjKind");
  }

}

ClassTemplateDecl *Sema::getClientReflCTD(SourceLocation Loc, unsigned/*ReflectionObjKind*/ RK) {
  ClassTemplateDecl *&res = reflection_cached_lookups[RK];
  if (!res) {
    assert(RK < MAX_REFLOBJKIND && "Invalid ReflectionObjKind!");
    res = LookupReflectionCTD(*this, Loc, static_cast<ReflectionObjKind>(RK), GetClientReflDC(Loc));
    assert(!res || res == reflection_cached_lookups[RK] && "Result wasn't cached!");
  }
  return res;
}




// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Next, some helper functions for instantiating the client's
// reflection templates...
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/// For class-returning ReflectionTraitExprs, i.e. RTE's for which we calculate
/// the type via the CB_getTypeFromArgs, the CB_getResultFromArgs will be defined
/// as a simple call to this function (i.e. this will be called with a captured SemaRef
/// during Expr evaluation).
static ExprResult defaultConstructType(Sema &S, SourceLocation KWLoc, QualType TempType) {
  // Whenever the GetType callback encounters an error, it should emit an S.Diag
  // explanation then return QualType().
  // So, we'll check if the type is null and if so just emit an ExprError here,
  // no further Diag needed.
  if (TempType.isNull())
    return ExprError();

  // Produce a value-initialized temporary of the template specialization type.
  // (ASUTTON below note from old BuildDeclReflection:
  //    FIXME: We should actually return a reflection expression and attach
  //    the initializer for use in evaluation. )
  SmallVector<Expr *, 1> Args;
  InitializedEntity Entity = InitializedEntity::InitializeTemporary(TempType);
  InitializationKind Kind = InitializationKind::CreateValue(KWLoc, KWLoc, KWLoc);
  InitializationSequence InitSeq(S, Entity, Kind, Args);
  return InitSeq.Perform(S, Entity, Kind, Args);
}


//static void addTypenameTemplateArg(
//        Sema &S, SourceLocation KWLoc, QualType TheType,
//        TemplateArgumentListInfo &TempArgs) {
//  TemplateArgument Arg(TheType);
//  TemplateArgumentLocInfo ArgLocInfo(
//          S.Context.getTrivialTypeSourceInfo(TheType, KWLoc));
//  //^ POSSIBLE FIXME?
//  TemplateArgumentLoc ArgLoc(Arg, ArgLocInfo);
//  TempArgs.addArgument(ArgLoc);
//}

template<typename INT_T>
static void addUIntTemplateArg(
        Sema &S, SourceLocation KWLoc, INT_T InputVal,
        TemplateArgumentListInfo &TempArgs, QualType IntType) {
  llvm::APSInt Val = S.Context.MakeIntValue(/*!*/InputVal, IntType);
  Expr *Literal = new (S.Context) IntegerLiteral(S.Context, Val, IntType, KWLoc);
  TemplateArgument Arg(Literal);

  TemplateArgumentLocInfo ArgLocInfo(Literal);
  TemplateArgumentLoc ArgLoc(Arg, ArgLocInfo);
  TempArgs.addArgument(ArgLoc);
}

/// BuildReflectionTypeFromRK -- Reflect-by-pointer overload
static QualType BuildReflectionTypeFromRK(
        Sema &S, SourceLocation KWLoc, ReflectionObjKind RK, bool IsPtr,
        intptr_t Val) {
  assert(RK);
  ClassTemplateDecl *CTD = S.getClientReflCTD(KWLoc, RK);
  if (!CTD)
    return QualType(); //Error: template decl not found

  TemplateArgumentListInfo TempArgs(KWLoc, KWLoc);
  QualType UIntPtrTy = S.Context.getUIntPtrType();

  // TEMPLATE ARGUMENTS:
  ////DWR TODO try adding this param in, here and to client header, once things are working:
//  //[-1] chunk_t
//  addTypenameTemplateArg(S, KWLoc, UIntPtrTy, TempArgs);
  //[0] IsPtr
  addUIntTemplateArg(S, KWLoc, IsPtr, TempArgs, UIntPtrTy); //S.Context.getBOOLType()?
  //[1] Data pointer
  addUIntTemplateArg(S, KWLoc, Val, TempArgs, UIntPtrTy);

  // Instantiate the CTD template with the above TempArgs, complete it,
  // and return the completed type:
  // DWR TODO: it would be great if we could forego instantiation until
  // the defaultConstructType function -- i.e. somehow get a TempType
  // here that is not instantiated.
  // Because we instantiate here, we force a chain reaction of instantiations
  // that seems sure to cause serious build time performance issues when performing even
  // small insignificant reflections in large projects (basically, to get one reflection,
  // you are forced to calculate ALL the reflections, instantiate reflections for the whole
  // AST, I think -- no good).
  TemplateName TempName(CTD);
  QualType TempType = S.CheckTemplateIdType(TempName, KWLoc, TempArgs);

  if (S.RequireCompleteType(KWLoc, TempType, diag::err_incomplete_type))
    return QualType(); //null QualType: error

  return TempType;
}

// BuildReflectionTypeFromRK -- Reflect-by-value/"data chunks" overload:
LLVM_ATTRIBUTE_UNUSED
static QualType BuildReflectionTypeFromRK(
        Sema &S, SourceLocation KWLoc, ReflectionObjKind RK, bool IsPtr,
        const intptr_t *ValChunkArr, size_t NumChunks) {
  ClassTemplateDecl *CTD = S.getClientReflCTD(KWLoc, RK);
  if (!CTD)
    return QualType(); //Error: template decl not found

  TemplateArgumentListInfo TempArgs(KWLoc, KWLoc);
  QualType UIntPtrTy = S.Context.getUIntPtrType();

  // TEMPLATE ARGUMENTS:
  //[0] IsPtr (needed to interpret Data chunks args)
  addUIntTemplateArg(S, KWLoc, IsPtr, TempArgs, UIntPtrTy);
  //[1...n] Data chunks:
  for (unsigned i = 0; i != NumChunks; ++i)
    addUIntTemplateArg(S, KWLoc, ValChunkArr[i], TempArgs, UIntPtrTy);

  // Instantiate the CTD template with the above TempArgs, complete it,
  // and return the completed type:
  TemplateName TempName(CTD);
  QualType TempType = S.CheckTemplateIdType(TempName, KWLoc, TempArgs);

////DWR TEMP TEST
//  RecordDecl *Rec = cast<RecordType>(TempType.getCanonicalType())->getDecl();
//  assert(Rec);
//  Rec->setCompleteDefinition(false);
////END

////DWR MOVED THIS TO DEFAULT CONSTRUCT TYPE:
//  if (S.RequireCompleteType(KWLoc, TempType, diag::err_incomplete_type))
//    return QualType(); //null QualType: error
////END

  return TempType;
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Next, the BuildReflectionType helper functions, which
// help us reflect a type with the proper syntax as the original
// type.  E.g. if an original reflection trait return type
// was an Expr **, we want to wrap that twice in ptrwrp templates,
// which provide the operator-> syntax.
// If a return type is a reference, e.g. ASTContext&, we don't
// wrap in any templates, but we instantiate with the pointer, i.e.
// i.e. the address, as the reflection data.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/// \brief Type trait that gives the ReflectionObjKind
/// for a given class.
///
template<typename T>
struct GetReflectionObjKind;
//        {
//  static constexpr ReflectionObjKind value = {}; //= RK_none
//};

/// \brief Type trait that gives the class
/// for a given ReflectionObjKind.
///
template<ReflectionObjKind RK>
struct GetTypeFromRK;
//        {
//  using type = void;
//};

#include "reflection_incs/ReflectionGetRKSpecs.inc"


LLVM_ATTRIBUTE_UNUSED
static constexpr size_t ceceil(float num) {
  return (static_cast<float>(static_cast<size_t>(num)) == num)
         ? static_cast<size_t>(num)
         : static_cast<size_t>(num) + ((num > 0) ? 1 : 0);
}

template<typename T, typename U>
static constexpr size_t getNumReqChunks() {
  // References are reflected by pointer, so reason about
  // the size of a pointer here:
  return ( std::is_reference<T>::value
           ? ceceil((float)sizeof(intptr_t) / sizeof(U))
           : ceceil((float)sizeof(T) / sizeof(U)) );
}


template<typename... Ts>
struct NumReqUIntChunks;

template<>
struct NumReqUIntChunks<> {
  static constexpr size_t value = 0;
};

template<typename T, typename... Us>
struct NumReqUIntChunks<T, Us...> {
  static constexpr size_t value =
      getNumReqChunks<T, intptr_t>() + NumReqUIntChunks<Us...>::value;
};


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Finally, the BuildReflection overloads.
// We have a type trait determine whether a type is,
// at root, a class, which means it needs to be reflected
// using the above functions, or is primtive, in which case we
// can simply return the appropriate literal.
//
// DWR TODO fix documentation here, explain structure and macros...
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


/// We need this dummy type to route reference vals
/// to reference overloads of BuildReflectionX, rather than
/// value overloads, to avoid ambiguity.
template<bool REF1VAL0> struct RefOvld; //(never defined)

/// BuildReflectionType -- Value overload
template<typename T>
static QualType BuildReflectionType(
        Sema &S, SourceLocation KWLoc, T val, RefOvld<0>* = nullptr) {
//    static_assert(GetReflectionObjKind<T>::value != RK_none,
//                "GetReflectionObjKind specialization not defined for this type!  "
//                "Most likely you have not #included headers with all the DeclNode definitions"
//                "in the clang/AST/ReflectionIncludes.h file used to generate the reflection source..."); //DWR TEMP COMMENTED OUT, FIXME
  return BuildReflectionTypeFromRK(
          S, KWLoc, GetReflectionObjKind<T>::value, /*WasPtr=*/false,
          reinterpret_cast<const intptr_t *>(&val), NumReqUIntChunks<T>::value );
}

/// BuildReflectionType -- Reference overload
template<typename T>
static QualType BuildReflectionType(
        Sema &S, SourceLocation KWLoc, const T &val, RefOvld<1>*) {

  static_assert(GetReflectionObjKind<T>::value != RK_none,
                "GetReflectionObjKind specialization not defined for this type!  "
                "Most likely you have not #included headers with all the DeclNode definitions"
                "in the clang/AST/ReflectionIncludes.h file used to generate the reflection source...");

  return BuildReflectionTypeFromRK(
          S, KWLoc, GetReflectionObjKind<T>::value, true, //DWR: replaced WasPtr with true, since references need to be handled in the PointerCases
          reinterpret_cast<intptr_t>(&val) );
}

/// BuildReflectionType -- Pointer non-reference overload.
/// Creates ptrwrp<T> instantiation around inner instantiation.
template<typename T>
static QualType BuildReflectionType(
        Sema &S, SourceLocation KWLoc, T *val, RefOvld<0>* = nullptr) {
  QualType PointeeClientType = BuildReflectionType( S, KWLoc, *val, (RefOvld<1>*)nullptr );
  ClassTemplateDecl *ptrwrpCTD = S.getClientReflPtrwrpCTD(KWLoc);
  if (!ptrwrpCTD || PointeeClientType.isNull())
    return QualType();

  // TEMPLATE ARGUMENTS:
  // [0] Pointee Type
  TemplateArgument Arg(PointeeClientType);
  TemplateArgumentLocInfo ArgLocInfo(
          S.Context.getTrivialTypeSourceInfo(PointeeClientType, KWLoc));
  //^ POSSIBLE FIXME?
  TemplateArgumentLoc ArgLoc(Arg, ArgLocInfo);
  TemplateArgumentListInfo TempArgs(KWLoc, KWLoc);
  TempArgs.addArgument(ArgLoc);

  // Instantiate the ptrwrp<T> template, complete it, and return it
  TemplateName TempName(ptrwrpCTD);
  QualType TempType = S.CheckTemplateIdType(TempName, KWLoc, TempArgs);
  if (S.RequireCompleteType(KWLoc, TempType, diag::err_incomplete_type))
    return QualType(); //i.e. error if above returns true
  return TempType;
}


/// BuildReflectionType Pointer reference overload.
/// I.e. a reference to a pointer.
template<typename T>
static QualType BuildReflectionType(
        Sema &S, SourceLocation KWLoc, T *val, RefOvld<1>*) {
  // Just send the inner pointer to the non-reference pointer
  // overload; i.e. ignore the reference part.
  return BuildReflectionType(S, KWLoc, val);
}





/// Helper type trait for defining distinct primitive and class overloads
template<typename T>
struct remove_all_pointers {
  using type = typename std::remove_cv<T>::type;
};
template<typename T>
struct remove_all_pointers<T *> {
  using type = typename remove_all_pointers<T>::type;
};
template<typename T>
struct remove_all_pointers<const T *> {
  using type = typename remove_all_pointers<T>::type;
};


//template<typename T>
//struct remove_all_pointers {
//  using type = typename std::conditional<
//      std::is_pointer<T>::value,
//      typename remove_all_pointers<typename std::remove_pointer<T>::type>::type,
//      typename std::remove_cv<T>::type //also remove const from innermost type
//    >::type;
//};

template<typename T>
using rmvptrs_t = typename remove_all_pointers<T>::type;





///// BuildReflection -- overload for ref whose de-ref'd, de-pointered type is a class
///// (e.g. ASTContext &, SomeOtherClass * const Expr *&)
//template<typename T,
//        typename std::enable_if<std::is_class<rmvptrs_t<T>>::value, int>::type = 0 >
//static ExprResult BuildReflection(Sema &S, SourceLocation KWLoc, const T &val, RefOvld<1>*) {
////  llvm::outs() << "[DWR DEBUG] BuildReflection: reference, non-prim overload\n";
//  return defaultConstructType(S, KWLoc, BuildReflectionType(S, KWLoc, val, (RefOvld<1>*)0));
//}



////(forward decl for non-ref, class-based BuildReflection overload)
//template< typename T,
//        typename std::enable_if<std::is_class<rmvptrs_t<T>>::value, int>::type = 0 >
//static ExprResult BuildReflection(Sema &S, SourceLocation KWLoc, T val, RefOvld<0>* = 0);

///// BuildReflection -- overload for non-ref whose de-pointered type is a class
///// Note that the RefOvld param is defaulted (in fwd decl), so that for BuildReflection
///// calls with only three parameters, we default to this overload, for convenience
///// (E.g. ActOnReflExpr calls this without that param since it knows the
///// T will be a Decl * or Type *, I think...)
//template< typename T,
//        typename std::enable_if<std::is_class<rmvptrs_t<T>>::value, int>::type>
//static ExprResult BuildReflection(Sema &S, SourceLocation KWLoc, T val, RefOvld<0>*) {
////  llvm::outs() << "[DWR DEBUG] BuildReflection: non-reference, non-prim overload\n";
//  static_assert(std::is_class<T>::value || std::is_pointer<T>::value,
//                "Expected only a class or pointer  in this overload");
//  return defaultConstructType(S, KWLoc, BuildReflectionType(S, KWLoc, val));
//}


// NOTE that BuildReflectionResult will take the QualType as the first param; remaining params same as BuildReflectionType...


/// GetPrimType -- overload for a pointer to a non-class type,
/// or a pointer to such a pointer, et al. -- excepting const char *
template< typename T,
          typename std::enable_if<std::is_pointer<T>::value &&
                                  !std::is_same<typename std::remove_cv<T>::type,
                                                const char *>::value, int>::type = 0 >
static QualType GetPrimType(ASTContext &C) {
  static_assert(!std::is_class<rmvptrs_t<T>>::value,
                "Expected a pointer to a primitive type, but this is to a class");
  return C.getUIntPtrType();
}
/// BuildReflectionResult -- overload for a pointer to a non-class type, or a pointer to such a pointer, et al.
template< typename T>
static ExprResult BuildReflectionResult(QualType QT, ASTContext &C, SourceLocation Loc, const T *Val, RefOvld<0>* = nullptr) {
  static_assert(!std::is_class<rmvptrs_t<T>>::value,
                "Expected a pointer to a primitive type, but this is to a class");
  static_assert(!std::is_same<T, char>::value, "Should have used BuildReflectStr for this!");
//  llvm::outs() << "[DWR DEBUG] BuildReflectionResult: pointer to primitive type overload\n";
  llvm::APSInt N = C.MakeIntValue(reinterpret_cast<intptr_t>(Val), QT);
  return IntegerLiteral::Create(C, N, QT, Loc);
}


/// GetPrimType/BuildReflectionResult -- reference overload.
/// De-ref'd, de-pointered type must be a primitive, as for all
/// BuildReflectionResult cases right now.
/// (e.g. unsigned &, const int * const char **&).
/// NOTE: we will return such types as a pointer, and de-reference them in the
/// client reflection header.  (I don't believe any clang AST return types fall into
/// this category, but if they did, we could handle them.)
/// The QT first param should be the fed by the type of the
/// ReflectionTraitExpr.
template<typename T,
        typename std::enable_if<std::is_reference<T>::value, int>::type = 0>
static QualType GetPrimType(ASTContext &C) {
  static_assert(!std::is_class<typename std::remove_reference<T>::type>::value,
                "Expected a reference to a primitive type, but this is to a class");
  return GetPrimType<typename std::add_pointer<typename std::remove_reference<T>::type>::type>(C);
}
template<typename T>
static ExprResult BuildReflectionResult(QualType QT, ASTContext &C, SourceLocation Loc, const T &val, RefOvld<1>*) {
  static_assert(!std::is_class<rmvptrs_t<T>>::value, "Expected a reference to a primitive, not a class");
//  llvm::outs() << "[DWR DEBUG] BuildReflectionResult: reference, prim overload\n";
  return BuildReflectionResult(QT, C, Loc, &val); //NON-ref version, called with address of val
}


/// GetPrimType -- void overload
template<typename T,
        typename std::enable_if<std::is_same<T, void>::value, int>::type = 0>
static QualType GetPrimType(ASTContext &C) {
  return C.VoidTy;
}

/// GetPrimType -- Integral overload
template<typename T,
        typename std::enable_if<std::is_integral<T>::value, int>::type = 0>
static QualType GetPrimType(ASTContext &C) {
//  llvm::outs() << "[DWR DEBUG] GetPrimTYpe: integer type\n";
  if /*constexpr*/ (std::is_same<T, bool>::value)
    return C.BoolTy;
  else if (std::is_same<T, unsigned>::value)
    return C.UnsignedIntTy;
  else if (std::is_same<T, int>::value)
    return C.IntTy;
  else if (std::is_same<T, unsigned char>::value)
    return C.UnsignedCharTy;
  else if (std::is_same<T, char>::value)
    return C.CharTy;
  else if (std::is_same<T, unsigned long>::value)
    return C.UnsignedLongTy;
  else if (std::is_same<T, long>::value)
    return C.LongTy;
  else if (std::is_same<T, unsigned short>::value)
    return C.UnsignedShortTy;
  else if (std::is_same<T, short>::value)
    return C.ShortTy;
  else if (std::is_same<T, unsigned long long>::value)
    return C.UnsignedLongLongTy;
  else
    return C.LongLongTy;
}
/// BuildReflectionResult -- Integral overload
template<typename T,
         typename std::enable_if<std::is_integral<T>::value, int>::type = 0>
static ExprResult BuildReflectionResult(QualType QT, ASTContext &C, SourceLocation Loc, T IntVal, RefOvld<0>* = nullptr) {
//  llvm::outs() << "[DWR DEBUG] BuildReflectionResult: integer overload\n";
  llvm::APSInt N = C.MakeIntValue(IntVal, QT);
  return IntegerLiteral::Create(C, N, QT, Loc);
}



/// GetPrimType -- Float overload
template<typename T,
        typename std::enable_if<std::is_floating_point<T>::value, int>::type = 0>
static QualType GetPrimType(ASTContext &C) {
//  llvm::outs() << "[DWR DEBUG] GetPrimType: float overload\n";
  if /*constexpr*/ (std::is_same<T, float>::value)
    return C.FloatTy;
  else if (std::is_same<T, double>::value)
    return C.DoubleTy;
  else
    return C.LongDoubleTy;
}
/// BuildReflectionResult -- float overload
template<typename T,
         typename std::enable_if<std::is_floating_point<T>::value, int>::type = 0>
static ExprResult BuildReflectionResult(QualType QT, ASTContext &C, SourceLocation Loc, T FloatVal, RefOvld<0>* = nullptr) {
//  llvm::outs() << "[DWR DEBUG] BuildReflectionResult: float overload\n";
  return FloatingLiteral::Create(C, llvm::APFloat(FloatVal), /*isexact=*/true, QT, Loc);
}


/// GetPrimType -- Enum overload
template<typename T,
        typename std::enable_if<std::is_enum<T>::value, int>::type = 0>
static QualType GetPrimType(ASTContext &C) {
  return GetPrimType<typename std::underlying_type<T>::type>(C);
}
/// BuildReflectionResult -- Enum overload
template<typename T,
        typename std::enable_if<std::is_enum<T>::value, int>::type = 0>
static ExprResult BuildReflectionResult(QualType QT, ASTContext &C,  SourceLocation KWLoc, T val, RefOvld<0>* = nullptr) {
//  llvm::outs() << "[DWR DEBUG] BuildReflectionResult: enum overload\n";
  return BuildReflectionResult(QT, C, KWLoc, static_cast<
                            typename std::underlying_type<T>::type>(val) );
}


/// GetPrimType for any T that is convertible to StringRef
/// (i.e. any representation of a string in clang)
/// (std::string, StringRef, const char *).
template< typename T,
          typename std::enable_if<std::is_convertible<T, StringRef>::value, int>::type = 0 >
static QualType GetPrimType(ASTContext &C) {
  // The type of the expression will be 'const char*'.
  return C.getPointerType(C.getConstType(C.CharTy));

//  //DWR TEST: changing type of string literals to an array type with a dummy size...
//  static unsigned DummySize = 100;
//  return C.getConstantArrayType(C.CharTy.withConst(),
//                                llvm::APInt(32, DummySize + 1),
//                                ArrayType::Normal, 0);

//  //DWR TEST B:
//  return C.getFunctionNoProtoType(C.getPointerType(C.getConstType(C.CharTy)));
}


/// BuildReflectionStr -- StringRef overload
template<bool ISREF = 0>
LLVM_ATTRIBUTE_UNUSED
static ExprResult BuildReflectionStr(ASTContext &C, SourceLocation Loc, StringRef Str, RefOvld<ISREF>* = nullptr) {
    QualType StrTy = C.getConstantArrayType(C.CharTy.withConst(),
                                            llvm::APInt(32, Str.size() + 1), //DWR TEMP
                                            ArrayType::Normal, 0);

    return StringLiteral::Create(C, Str, StringLiteral::Ascii, false, StrTy, Loc);
}

/// BuildReflectionStr -- std::string overload
template<bool ISREF = 0>
LLVM_ATTRIBUTE_UNUSED
static ExprResult BuildReflectionStr(ASTContext &C, SourceLocation Loc, std::string Str, RefOvld<ISREF>* = nullptr) {
//  llvm::outs() << "[DWR DEBUG] BuildReflectionResult: std::string overload\n";
  return BuildReflectionStr(C, Loc, StringRef(Str));
}

/// BuildReflectionStr -- const char * overload
template<bool ISREF = 0>
LLVM_ATTRIBUTE_UNUSED
static ExprResult BuildReflectionStr(ASTContext &C, SourceLocation Loc, const char * CCP, RefOvld<ISREF>* = nullptr) {
//  llvm::outs() << "[DWR DEBUG] BuildReflectionResult: const char * overload\n";
  return BuildReflectionStr(C, Loc, StringRef(CCP));
}





/*-----------------------------------------------------------------*/
///  Sema::Evaluate.../ActOn.../etc. reflection implems


static ExprResult BuildReflectionWFullyDerivedDecl(Sema &SemaRef, SourceLocation KWLoc, Decl *D) {
  switch(D->getKind()) {
#define DECL(DERIVED, BASE) case Decl::DERIVED: {\
    /*llvm::outs() << "[DWR DEBUG] Reflecting " << #DERIVED << "Decl\n";*/\
    return defaultConstructType(SemaRef, KWLoc,\
        BuildReflectionType(SemaRef, KWLoc, cast<DERIVED ## Decl>(D)) );\
  }
#define ABSTRACT_DECL(DECL)
#include "clang/AST/DeclNodes.inc"
  }
}

//DWR COMMENTED OUT: UNUSED
///// Returns a reflected declaration or nullptr if E does not reflect a
///// declaration. If E reflects a user-defined type, then this returns the
///// declaration of that type.
//static Decl *GetReflectedDeclaration(Sema &S, Expr *E)
//{
//  ReflectedConstruct R = EvaluateReflection(S, E);
//  Decl *D = R.getAsDeclaration();
//  if (!D)
//    if (Type *T = R.getAsType())
//      D = T->getAsCXXRecordDecl();
//  if (!D)
//    S.Diag(E->getLocStart(), diag::err_invalid_reflection_trait);
//  return D;
//}

/// The expression \c $x returns an object describing the reflected entity \c x.
/// The type of that object depends on the type of the thing reflected.
///
/// The \p E argument is not null.
ExprResult Sema::ActOnCXXReflectExpr(SourceLocation OpLoc, Expr *E) {
//  llvm::outs() << "[DWR DEBUG] in ActOnCXXReflectExpr, Expr *E overload\n";
  // Try to correct typos.
  if (isa<TypoExpr>(E)) {
    ExprResult Fixed = CorrectDelayedTyposInExpr(E);
    if (!Fixed.isUsable())
      return ExprError();
    E = Fixed.get();
  }

  // If the node is dependent, then preserve the expression until instantiation.
  if (E->isTypeDependent() || E->isValueDependent())
    return new (Context) ReflectionExpr(OpLoc, E, Context.DependentTy);

  if (OverloadExpr *Ovl = dyn_cast<OverloadExpr>(E)) {
    // FIXME: This should be okay. We should be able to provide a limited
    // interface to overloaded functions.
    return ExprError(Diag(OpLoc, diag::err_reflected_overload)
                     << Ovl->getSourceRange());
  }

  if (!isa<DeclRefExpr>(E)) {
    // We can get here when substituting through placeholders in a fragment.
    // For example:
    //
    //    constexpr {
    //      int n = 0;
    //      __generate ... {
    //        __inject $n;
    //      };
    //    }
    //
    // The fragment of __declare is a reflection whose operand refers to
    // a placeholder. When we inject the fragment the name 'n' is replaced
    // by a constant expression with n's value, not a reference to n. That's
    // an error -- you couldn't declare a value anyway.
    Diag(E->getExprLoc(), diag::err_not_a_reflection);
    return ExprError();
  }

  // Build the reflection expression with its full dynamic type:
  return BuildReflectionWFullyDerivedDecl(
          *this, OpLoc, cast<DeclRefExpr>(E)->getDecl());

  //  return BuildReflection(*this, OpLoc, cast<DeclRefExpr>(E)->getDecl());
}

/// \brief Build a reflection for the type wrapped by \p TSI.
ExprResult Sema::ActOnCXXReflectExpr(SourceLocation OpLoc,
                                     TypeSourceInfo *TSI) {
//  llvm::outs() << "[DWR DEBUG] in ActOnCXXReflectExpr, TypeSourceInfo overload\n";

  QualType QT = TSI->getType();

  // If the type is dependent, preserve the expression until instantiation.
  if (QT->isDependentType())
    return new (Context) ReflectionExpr(OpLoc, TSI, Context.DependentTy);


  Type *T = const_cast<Type *>(QT.getTypePtr());



  switch(T->getTypeClass()) {
#define TYPE(DERIVED, BASE) case Type::DERIVED: {\
    /*llvm::outs() << "[DWR DEBUG] Reflecting " << #DERIVED << "Type\n";*/\
    return defaultConstructType(*this, OpLoc,\
      BuildReflectionType(*this, OpLoc, cast<DERIVED ## Type>(T)) );\
  }
#define ABSTRACT_TYPE(DERIVED, BASE)
#include "clang/AST/TypeNodes.def"
  }

//  return BuildReflection(*this, OpLoc, T); //DWR POSSIBLE FIXME
}

/// \brief Build a reflection for the type-id stored in \p D.
ExprResult Sema::ActOnCXXReflectExpr(SourceLocation OpLoc, Declarator &D) {
//  llvm::outs() << "[DWR DEBUG] in ActOnCXXReflectExpr, Declarator overload\n";
  return ActOnCXXReflectExpr(OpLoc, GetTypeForDeclarator(D, CurScope));
}

/// Try to construct a reflection for the declaration named by \p II.
///
/// This will reflect:
///
///   - id-expressions whose unqualified-id is an identifier
///   - type-names that are identifiers, and
///   - namespace-names
///
// TODO: Handle ambiguous and overloaded lookups.
ExprResult Sema::ActOnCXXReflectExpr(SourceLocation OpLoc, CXXScopeSpec &SS,
                                     IdentifierInfo *II, SourceLocation IdLoc) {
//  llvm::outs() << "[DWR DEBUG] in ActOnCXXReflectExpr, IdentifierInfo overload\n";

  // Perform any declaration having the given name.
  LookupResult R(*this, II, OpLoc, LookupAnyName);
  LookupParsedName(R, CurScope, &SS);
  if (!R.isSingleResult())
    // FIXME: This doesn't give the best errors in case of e.g., finding
    // an overload (although we should handle that).
    return ExprError();

  Decl *D = R.getAsSingle<Decl>();
  if (!D)
    return ExprError();

  // If the declaration is a template parameter, defer until instantiation.
  //
  // FIXME: This needs to be adapted for non-type and template template
  // parameters also. Most likely, we need to allow ReflectExprs to contain
  // declarations in addition to expressions and types.
  if (TypeDecl *TD = dyn_cast<TypeDecl>(D)) {
    QualType T = Context.getTypeDeclType(TD); // The reflected type.
    QualType R = T; // The result type of the expression.

//// DWR COMMENTED OUT because I think this only applies to $classes...
//    // If this refers to a metaclass specifier, then pretend that it's
//    // dependent so we don't substitute too early.
//    //
//    // FIXME: This is a hack. It would be better if we always returned an
//    // ReflectionExpr and then packed that will all of the evaluation
//    // information needed for later.
//    //
//    // FIXME: This even hackier since we're adjusting the qualified type to
//    // avoid a more principled check for metaclassiness later on.
//    if (CXXRecordDecl *Class = dyn_cast<CXXRecordDecl>(TD)) {
//      if (Class->isInjectedClassName()) {
//        Class = dyn_cast<CXXRecordDecl>(Class->getDeclContext());
//        T = Context.getRecordType(Class); // Point at the actual type.
//      }
//////ASUTTON ADDN:
////      if (dyn_cast<MetaclassDecl>(Class->getDeclContext()))
////        R = Context.DependentTy;
//////END
//    }
//// END DWR COMMENTED OUT

    // If the return type is dependent, don't evaluate too early.
    if (R->isDependentType()) {
      TypeSourceInfo *TSI = Context.getTrivialTypeSourceInfo(T);
      return new (Context) ReflectionExpr(OpLoc, TSI, Context.DependentTy);
    }
  }

  // If we have a value declaration of dependent type, then make a dependent
  // decl-ref expression to be resolved later.
  if (ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
    QualType T = VD->getType();
    if (T->isDependentType()) {
      Expr *E = new (Context)
          DeclRefExpr(VD, false, Context.DependentTy, VK_LValue, OpLoc);
      return new (Context) ReflectionExpr(OpLoc, E, Context.DependentTy);
    }
  }

  // Build the reflection expression with its full dynamic type:
  return BuildReflectionWFullyDerivedDecl(*this, OpLoc, D);
//  return BuildReflection(*this, OpLoc, D);
}







// HELPER MACROS

///// The last base overload attempt will just return this:
//#define BASE_OVERLOAD(MP_base) \
//  ReflectProp( MemNum, static_cast<typename std::conditional<std::is_copy_constructible<MP_base>::value, MP_base, const MP_base &>::type>(X) )

///// Note that void method calls will return ExprEmpty, which will
///// register as Unset here -- meaning that even if a void call was
///// made you'll still end up doing a (very cheap) MemNum search
///// on any base overloads (will just go to each base, see that
///// the number is too high for each, and return)
///// which will still end with returning an ExprEmpty().
///// That's fine -- void calls will only be things like dump(),
///// which are debug only, and in any case this will be super
///// cheap so no need to worry about the slight inefficiency. [DWR]
//#define TRY_BASE_OVERLOAD(MP_base) \
//  { \
//    ExprResult res = BASE_OVERLOAD(MP_base); \
//    if (!res.isUnset()) \
//      return res; \
//  }

// DWR NB: We distinguish between PRIM and CLASS reflections
// below for conceputal clarity; for now, they are implemented
// exactly the same.

//// DWR TEMP DEBUG:
//#ifndef DWR_PP_STRINGIZE
////   Helpers:
//#   define DWR_PP_EMPTY()
//#   define DWR_PP_DEFER(id) id DWR_PP_EMPTY()
//#   define DWR_PP_EXPAND(...) __VA_ARGS__
//#   define DWR_PP_STRINGIZE_SIMPLE(x) #x
///// This expands any macros in x and stringizes the result.
///// That is important because right now we're having some difficulty
///// parsing certain macros in __queue_metaparse statements --
///// this lets us get rid of them.
//#  define DWR_PP_STRINGIZE(x) DWR_PP_EXPAND(DWR_PP_DEFER(DWR_PP_STRINGIZE_SIMPLE)(x))
//#endif
////END TEMP DEBUG

/// Methods that return void (e.g. dump()) will use this in their
/// CB_GetResult callback.
#define VOID_REFLECTION(.../*void method call*/)\
{\
  __VA_ARGS__;\
  return {};/*return null Expr *; never used with Void-typed reflections, just a dummy*/\
}
/**/




///// Primitive-typed (e.g. unsigned, int*, void *, const char *&)
///// fields and -return-typed methods will use this in their CB_GetResult
///// callback.
//#define PRIM_REFLECTION_CALL(MP_val) {\
//  /*llvm::outs() << "[DWR TEMP DEBUG] Prim refl, getting result of "\
//                 << DWR_PP_STRINGIZE(MP_val) << "\n";\
//    decltype(MP_val) res = MP_val;\
//    llvm::outs() << "[DWR TEMP DEBUG] got res without issue; "\
//                    "now calling BuildReflectionResult on res...\n";*/\
//    return BuildReflection(\
//        S, KWLoc, MP_val,\
//        (RefOvld<std::is_reference<decltype(MP_val)>::value>*)0 );\
//  }
///**/






// Now some helper macros to help us build the X_CALL macro from the memname
// and args etc.  (Not needed for VOID_REFLECTION, note.)

//#define X_ACCESSOR_V X.
//#define X_ACCESSOR_P X->
//#define X_ACCESSOR_S CLASSNAME::
//#define XREF_V  X
//#define XREF_P  *X

#define DV(...) std::declval<__VA_ARGS__>()

//#define BUILD_REFLECTION(MP_macrosfx, MP_ResultOrType, MP_vps, MP_memname, .../*args*/) \
//  BUILD_REFLECTION_CALL_##MP_macrosfx(MP_ResultOrType, X_ACCESSOR_##MP_vps MP_memname(__VA_ARGS__) )
///**/
//#define BUILD_REFLECTION_0(MP_macrosfx, MP_ResultOrType, MP_vps, MP_memname)\
//  BUILD_REFLECTION_CALL_##MP_macrosfx(MP_ResultOrType, X_ACCESSOR_##MP_vps MP_memname())
///**/
///// This one is useful for accessing RANGE_NTH and RANGE_SIZE
///// on classes that have begin()/end() functions.
//#define BUILD_SELF_REFLECTION(MP_macrosfx, MP_ResultOrType, MP_vps, MP_memname)\
//  BUILD_REFLECTION_CALL_##MP_macrosfx(MP_ResultOrType, XREF_##MP_vps)
///**/


#define INITARGS_BuildReflectionResult   QT, SemaRef.Context //Note QT is the name of an arg to the GetResult callback
#define INITARGS_BuildReflectionStr      SemaRef.Context
#define INITARGS_BuildReflectionType     SemaRef


/// Primitive-typed (e.g. unsigned, int*, void *, const char *&)
/// fields and -return-typed methods will use this, with
/// MP_ResultOrType = Res, in their CB_GetResult
/// callback.
/// Non-primitive fields (e.g. ASTContext &, SourceLocation, etc.)
/// and non-primitive-returning methods will use this, with
/// MP_ResultOrType = Type, in their CB_GetType callback.
/// (Their CB_GetResult callback will simply be a matter of default constructing
/// this type.)
// DWR NOTE re the !is_copy_constructible conditionbelow : this is needed,
// for CLASS reflections only, when we have a value field,
// e.g. DeclarationNameTable in ASTContext, that is not copy constructible:
// the best we can do is reflect those by pointer.  HOWEVER, this only works
// because ASTContext is always itself returned by reference, i.e. it is persistent,
// never a temporary.  If an object were returned by value, i.e. was a temporary
// (e.g. SourceLocation, QualType etc.), and had a non-copy-constructible value field,
// building a pointer reflection for that value field would result in a bad
// pointer access once its owner went out of scope -- so DWR TODO to be safe,
// check such conditions in teh load_inclusions_for code in GenReflectionSrc.cpp
// and omit any that would be dangerous.
#define BUILD_REFLECTION_CALL_NORMAL(MP_ResultOrTypeOrStr, MP_val) {\
  /*llvm::outs() << "[DWR TEMP DEBUG] Class refl, getting result of "\
                 << DWR_PP_STRINGIZE(MP_val) << "\n";\
    decltype(MP_val) res = MP_val;\
    llvm::outs() << "[DWR TEMP DEBUG] got res without issue; "\
                    "now calling BuildReflectionType on res...\n";*/\
    return BuildReflection##MP_ResultOrTypeOrStr(\
        INITARGS_BuildReflection##MP_ResultOrTypeOrStr,\
        KWLoc, MP_val,\
        (RefOvld< std::is_reference<decltype(MP_val)>::value\
                  || !std::is_copy_constructible<decltype(MP_val)>::value/*^See note*/\
                >*)0 );\
  }

//All the complexity here is to keep it from being negative,
//which might occur if there are errors with push/insert reflections
// but not with pop/erase reflections that follow; in such cases
// we see weird errors and unconstrained iteration, so best to just
// set the range size to zero in such cases to prevent any iterations:
// DWR FIXME: this might be O(n) for each iter!!
// Need to solve the root issue here: the tuple expansion system right now
// is just not efficient.
#define RANGE_SIZE(MP_val)  static_cast<std::size_t>( std::max(\
  std::distance((MP_val).begin(), (MP_val).end()),\
  static_cast<decltype(std::distance((MP_val).begin(), (MP_val).end()))>(0) ) )
/**/

//#define MAKE_DIAG(diagID)  diaghandler = PartialDiagnostic(diagID, Context.getDiagAllocator())
#define MAKE_DIAG(diagID)  SemaRef.Diag(KWLoc, diagID)

#define ERROR_Result ExprError()
#define ERROR_Type   QualType()
#define ERROR_Str    ExprError()

/// Note we don't pass N as a param. we just trust that an N param
/// has been defined prior to any instance of this.
#define BUILD_REFLECTION_CALL_RANGE_NTH(MP_ResultOrTypeOrStr, MP_val)\
  assert (TraitKind == RTK_range_nth);\
  if (static_cast<std::size_t>(N) >= RANGE_SIZE(MP_val)) {\
    MAKE_DIAG(diag::err_reflrange_n_out_of_bounds)\
      << std::to_string(N) << std::to_string(RANGE_SIZE(MP_val));\
    return ERROR_##MP_ResultOrTypeOrStr;\
  }\
  BUILD_REFLECTION_CALL_NORMAL( MP_ResultOrTypeOrStr, reflcontainers::ContainerGetNth((MP_val), N)/**std::next((MP_val).begin(), N)*/ )
/**/
#define BUILD_REFLECTION_CALL_RANGE_SIZE(MP_val)\
  /*assert (TraitKind == RTK_range_size);*/\
  BUILD_REFLECTION_CALL_NORMAL( Result, RANGE_SIZE(MP_val) )
/**/

#define ASSERT_TRAIT_IS_REFLPROP(MP_name)\
  assert(TraitKind == RTK_prop\
         && "Only __reflect_prop is supported on this reflection member");\

//template<typename T>
//void setReflTypeInfoFromType(QualType &type,
//                             function_impl<QualType(ArrayRef<intptr_t>)> &CB_getTypeFromArgs);

//void setReflTypeInfoFromRK(ReflectionObjKind RK,
//                           QualType &type,
//                           function_impl<QualType(ArrayRef<intptr_t>)> &CB_getTypeFromArgs);



#define SET_CALLBACK(MP_ResultOrTypeOrStr) SET_CALLBACK_Get##MP_ResultOrTypeOrStr

/// The block that follows this macro will define how to get the result.
/// We use this for primitive return types.  We must set the type of the
/// expression to be non-dependent.
#define SET_CALLBACK_GetResult \
  CB_getResultFromArgs = [CB_CAPTURES](ArrayRef<intptr_t> RemArgs, QualType QT) -> ExprResult
/**/

/// We use this one when the type is DependentTy for now, becuase the return type is a class
/// (i.e. non-primitive), which means we'll have to instantiate a client reflection template.
/// This template instance will contain all the info for the reflection; once we know its type
/// (i.e. the template params), all we have to do is default construct that type when evaluating.
/// So, we first set the getResult callback to just default-construct whatever (template) type we
/// come up with, then we set up the signature for the CB_getTypeFromArgs def, so that the block
/// that follows this macro defines how to get the QT once we have sufficient args.
#define SET_CALLBACK_GetType \
  type = Context.DependentTy;\
  CB_getResultFromArgs = [CB_CAPTURES](ArrayRef<intptr_t> RemArgs, QualType QT)\
                         -> ExprResult {\
    return defaultConstructType(SemaRef, KWLoc, QT);\
  };\
  CB_getTypeFromArgs = [CB_CAPTURES](ArrayRef<intptr_t> RemArgs) -> QualType
/**/

///// We use this one when we have a string literal return type, which means we
///// have to set both callbacks -- we need the string size to determine the const
///// char[N] type, but we can't just default construct to get the res as we could
///// above.  So, we'll just define the CB_getTypeFromArgs as using the CB_getResultFromArgs,
///// then set things up so the user can define the CB_getResultFromArgs after this.
///// I.e. after this macro everything behaves just like SET_CALLBACK_GetResult above.
//#define SET_CALLBACK_GetStr \
//  type = Context.DependentTy;\
//  assert(!CB_getResultFromArgs); /*sanity check*/\
//  CB_getTypeFromArgs = [CB_CAPTURES](ArrayRef<intptr_t> RemArgs) -> QualType {\
//    assert(CB_getResultFromArgs && "DWR I guess this wasn't passed by ref?");\
//    ExprResult res = CB_getResultFromArgs(QualType()/*unused*/, RemArgs);\
//    if (res.isInvalid())\
//      return QualType();\
//    assert(res.get());\
//    assert(!res.get()->getType().isNull() && "Expected a valid type");\
//    return res.get()->getType();\
//  };\
//  CB_getResultFromArgs = [CB_CAPTURES](ArrayRef<intptr_t> RemArgs, QualType QT) -> ExprResult
///**/

// Disregard above, we can treat strings as ordinary prim types:
#define SET_CALLBACK_GetStr  SET_CALLBACK_GetResult

/// We use this macro when the type is not DependentTy, because it is primitive;
/// this will get the proper QualType from the actual type.
///
#define SET_PRIM_TYPE(...)\
  { __attribute__((__unused__)) const PP_REMOVE_PARENS(CLASSNAME) *X;\
      /*^dummy, used in decltype statements given in __VA_ARGS__*/\
    type = GetPrimType<__VA_ARGS__>(Context);\
  }
/**/

/// reftoptr -- since we convert all references to simple pointers, and
/// pass those pointers back whenever a reference is needed in a client
/// reflection function, we use this to convert any parameter type
/// needed in LOAD_VAL to a pointer
template<typename T> struct reftoptr            { using type = T; };
template<typename T> struct reftoptr<T &>       { using type = T *; };
template<typename T> struct reftoptr<const T &> { using type = const T *; };

/// derefptr_if_wasref -- changes the pointer back to a reference
/// wherever reftoptr made a reference type into a pointer.
template<typename T,
         typename std::enable_if<!std::is_reference<T>::value, int>::type = 0 >
T derefptr_if_wasref(T t) {
  return t;
}
template<typename T,
         typename std::enable_if<std::is_reference<T>::value, int>::type = 0 >
T derefptr_if_wasref(typename std::remove_reference<T>::type *t) {
  return *t;
}

#define PP_EXPAND_0(...) __VA_ARGS__
#define PP_EXPAND_1(...) __VA_ARGS__
#define PP_REMOVE_PARENS(x) PP_EXPAND_1(PP_EXPAND_0 x) //TODO maybe make this only conditionally remove parens, for convenience

// DWR TODO: for value parameters, i.e. non pointers,
// if 0 is provided, that should be a signal to default
// construct, perhaps, instead of reinterpret_casting.
// Or, I suppose always casting is faster/easier, but just
// be sure value types have an isValid() function that
// requires some field to be nonzero to return true; then
// zero-casting shouldn't be an issue.
#define LOAD_VAL_NODECL(T, .../*name*/)\
  assert(!RemArgs.empty() && "Insufficient args supplied to reflection trait -- empty RemArgs!");\
  assert(RemArgs.size() >= NumReqUIntChunks<PP_REMOVE_PARENS(T)>::value\
         && "Insufficient args supplied to reflection trait -- "\
            "non-empty RemArgs, but not enough for requested type!");\
  __VA_ARGS__ = derefptr_if_wasref<PP_REMOVE_PARENS(T)>(\
    reinterpret_cast<typename reftoptr<PP_REMOVE_PARENS(T)>::type &>(\
      const_cast<intptr_t &>(RemArgs.front()) ) );\
  RemArgs = RemArgs.drop_front(NumReqUIntChunks<PP_REMOVE_PARENS(T)>::value);\
/**/
#define LOAD_VAL(T, name)  LOAD_VAL_NODECL(T, PP_REMOVE_PARENS(T) name)


// LOAD_VAL_X helpers:

#define CHECK_NULLPTR_X(MP_ResultOrTypeOrStr)\
  if (!X) {\
    MAKE_DIAG(diag::err_null_reflected_ptr_access);\
    return ERROR_##MP_ResultOrTypeOrStr;\
  }
/**/

#define LOADX_ALWAYSPTR(CLASSNAME, MP_ResultOrTypeOrStr)\
  LOAD_VAL((PP_REMOVE_PARENS(CLASSNAME) *), X)\
  CHECK_NULLPTR_X(MP_ResultOrTypeOrStr)
/**/



// DWR TODO: might want to just load Xdata as a reference,
// to avoid any copy construction issues.  The allocated
// data will stay put so should be no issues there.
// OR better yet use different policies based on the sizeof(CLASSNAME)
// and whether CLASSNAME has a copy ctor.
// (In fact, just do this more generally for all types in LOAD_VAL_NODECL.)
#define LOADX_NEVERPTR(CLASSNAME, MP_ResultOrTypeOrStr)\
  LOAD_VAL(CLASSNAME, Xdata)\
  PP_REMOVE_PARENS(CLASSNAME) *X = &Xdata;
/**/

//#define XdataInit_1(CLASSNAME)  CLASSNAME()
//#define XdataInit_0(CLASSNAME)  llvm::Optional<CLASSNAME>()

//#define XdataGetPtr_1  &Xdata
//#define XdataGetPtr_0  Xdata.getPointer()

//#define LOADX_MAYBEPTR(HASDFLTCTOR, CLASSNAME, MP_ResultOrTypeOrStr)\
//  assert(TraitKind != RTK_cast &&\
//         "DWR: I implemented so that RTK_cast assumes the IsPtr is unused and "\
//         "thus can instead refer to the dyn arg, but now we need the IsPtr "\
//         "status from a RTK_cast trait -- so need to fix implem I guess...");\
//  CLASSNAME *X = nullptr;\
//  auto Xdata = XdataInit_##HASDFLTCTOR(CLASSNAME);\
//  if (IsPtr) {\
//    LOAD_VAL_NODECL(CLASSNAME *, X)\
//    CHECK_NULLPTR_X(MP_ResultOrTypeOrStr)\
//  } else {\
//    LOAD_VAL_NODECL(CLASSNAME, Xdata)\
//    X = XdataGetPtr_##HASDFLTCTOR;\
//  }\
//  assert(X);
///**/


// Been having a lot of issues with this; the best thing would probably
// be to reimplement GenReflectionSrc for cases when sometimes its
// a pointer, sometimes not, to just use two different branches -- i.e.
// enclose the main content of the callback in an
//    if (IsPtr) {LOADX_PTR ...body...}
//    else       {LOADX_VAL ...body...}
// body
#define LOADX_MAYBEPTR(HASDFLTCTOR, CLASSNAME, MP_ResultOrTypeOrStr)\
  assert(IsPtr <= 1 && "IsPtr is a nonsense value -- probably didn't capture it by value!");\
  assert(!RemArgs.empty()\
         && "Insufficient args supplied to reflection trait -- empty RemArgs!");\
  unsigned NumReqChunks = ( IsPtr ? NumReqUIntChunks<PP_REMOVE_PARENS(CLASSNAME)*>::value\
                                  : NumReqUIntChunks<PP_REMOVE_PARENS(CLASSNAME)>::value );\
  assert((RemArgs.size() >= NumReqChunks)\
         && "Insufficient args supplied to reflection trait -- "\
            "non-empty RemArgs, but not enough for requested type!");\
  intptr_t &RemArgsFront = const_cast<intptr_t &>(RemArgs.front());\
  if (IsPtr && !RemArgsFront) {\
    MAKE_DIAG(diag::err_null_reflected_ptr_access);\
    return ERROR_##MP_ResultOrTypeOrStr;\
  }\
  /* Define X either as a pointer to RemArgsFront or RemArgsFront itself\
   * reinterpreted as a pointer, depending on the IsPtr param:*/\
  PP_REMOVE_PARENS(CLASSNAME) *X = ( IsPtr ? reinterpret_cast<PP_REMOVE_PARENS(CLASSNAME) *&>(RemArgsFront)\
                                           : &reinterpret_cast<PP_REMOVE_PARENS(CLASSNAME) &>(RemArgsFront) );\
  /*llvm::errs() << "DWR TEMP DEBUG In LOADX_MAYBEPTR, IsPtr: " << IsPtr << "; X: " << X << "; NumReqChunks: " << NumReqChunks \
               << "; RemArgs.size() before drop: " << RemArgs.size() << "; RemArgs vals before drop, with trailing comma: \n";\
    for (auto e : RemArgs) { llvm::errs() << e << ", "; }*/\
  RemArgs = RemArgs.drop_front(NumReqChunks);\
  /*llvm::errs() << ". RemArgs.size() after drop: " << RemArgs.size() << "\n\n";*/\
  assert(X);\
/**/


#define IF_NO_MORE  if (RemArgs.empty())

#define ASSERT_NO_MORE \
  assert(RemArgs.empty()\
         && "Too many args supplied to reflection trait");
/**/



/// Helpers for case_REFLPROP_0ARGS
#define SET_TYPE_IF_RESULT_Result(...)  SET_PRIM_TYPE(__VA_ARGS__)
#define SET_TYPE_IF_RESULT_Type(...)    /*blank*/
//#define SET_TYPE_IF_RESULT_Str(...)     /*blank*/
#define SET_TYPE_IF_RESULT_Str  SET_TYPE_IF_RESULT_Result
/**/






/// For the simple, common case of a __reflect_prop trait for a nullary method or field:
#define case_REFLPROP_0ARGS(MP_idstr, MP_ResultOrTypeOrStr, MP_val)\
    case currefl::MP_idstr: {\
      ASSERT_TRAIT_IS_REFLPROP(MP_idstr)\
      SET_TYPE_IF_RESULT_##MP_ResultOrTypeOrStr(decltype(MP_val))\
      SET_CALLBACK(MP_ResultOrTypeOrStr) {\
        LOAD_VAL_X(MP_ResultOrTypeOrStr)\
        ASSERT_NO_MORE\
        BUILD_REFLECTION_CALL_NORMAL(MP_ResultOrTypeOrStr, MP_val)\
      }; break;\
    }
/**/



//#define reflprop_unreachable()\
//    llvm_unreachable("Method number routing must be flawed")\
///**/



/*

 TODO:
  --Figure out how to incorporate IsPtr into the callbacks where necessary, so you don't need separate pointer cases for everything.
  --Go through the GenReflectionSrc code, adapting stuff to use the macros and the Nodes instead of individual .incs

  -----------------

  Okay lemme think this through here.
  The main thing that we have to tweak now are the ReflTrait_SetTypeAndCBs_Cases_inc.
  They will follow the asic form of the nullary case above, except with addition LOAD_VAL calls etc.

  The REFLECT_CAST_CASEs below are JUST for __reflect_cast, which we need to address separately in GenReflectionSrc.

  So, just need to move along to fixing up GenReflectionSrc here.



 */






//// BuildCastReflectionType: called within a CB_GetType for a __reflect_cast trait.
/// (i.e. the cast/dyn_cast call goes where the method call went in __reflect_prop traits) //////

#define REFLECT_CAST_KIND_CASE(MP_TargRK)\
  case MP_TargRK: {\
    using targ_t = typename GetTypeFromRK<MP_TargRK>::type;\
    if (dyn) {\
      BUILD_REFLECTION_CALL_NORMAL(Type, dyn_cast<targ_t>(X))\
    } else if (isa<targ_t>(X)) {\
      BUILD_REFLECTION_CALL_NORMAL(Type, cast<targ_t>(X))\
    } else {\
      MAKE_DIAG(diag::err_reflcast_bad_isa_failed);\
      return QualType();\
    }\
  }
/**/

/// Decl * overload:
LLVM_ATTRIBUTE_UNUSED
QualType BuildCastReflectionType(SourceLocation KWLoc, Sema &SemaRef,
                                 const Decl *X, ReflectionObjKind ToKind, const bool dyn) {
  switch (ToKind) {
#   define DECL(RK) REFLECT_CAST_KIND_CASE(RK)
#   include "reflection_incs/ReflectionObjKindNodes.inc"
    default:
      MAKE_DIAG(diag::err_reflcast_incompatible_targ_kind)
          << "RK_clang__Decl" << to_string(ToKind);
      return QualType();
  }
}

/// Type * overload:
LLVM_ATTRIBUTE_UNUSED
static QualType BuildCastReflectionType(SourceLocation KWLoc, Sema &SemaRef,
                                 const Type *X, ReflectionObjKind ToKind, const bool dyn) {
  switch (ToKind) {
#   define TYPE(RK) REFLECT_CAST_KIND_CASE(RK)
#   include "reflection_incs/ReflectionObjKindNodes.inc"
    default:
      MAKE_DIAG(diag::err_reflcast_incompatible_targ_kind)
          << "RK_clang__Type" << to_string(ToKind);
      return QualType();
  }
}

/// Stmt * overload:
LLVM_ATTRIBUTE_UNUSED
static QualType BuildCastReflectionType(SourceLocation KWLoc, Sema &SemaRef,
                                 const Stmt *X, ReflectionObjKind ToKind, const bool dyn) {
  switch (ToKind) {
#   define STMT(RK) REFLECT_CAST_KIND_CASE(RK)
#   include "reflection_incs/ReflectionObjKindNodes.inc"
    default:
      MAKE_DIAG(diag::err_reflcast_incompatible_targ_kind)
          << "RK_clang__Stmt" << to_string(ToKind);
      return QualType();
  }
}

#undef REFLECT_CAST_KIND_CASE


// This is the macro used in the .inc file to handle RTK_cast traits:
#define HANDLE_RTK_CAST_TRAIT(X)\
  if (TraitKind == RTK_cast) {\
    SET_CALLBACK(Type) {\
      LOAD_VAL_X(Type)\
      ASSERT_NO_MORE\
      /*NB interpreting N as TargKind, IsPtr as dyn:*/\
      return BuildCastReflectionType(KWLoc, SemaRef,\
          X, static_cast<ReflectionObjKind>(MemNum), IsPtr/*dyn*/);\
    };\
    break;\
  }
/**/



//ExprResult Reflector::ReflectCast(ReflectionObjKind ToKind, Type *X, bool dyn) {
//  switch (ToKind) {
//#define TYPE(DERIVED, BASE) \
//      case GetReflectionObjKind<DERIVED##Type>::value:\
//        return BuildReflection(S, KWLoc,\
//          (dyn ? dyn_cast<DERIVED##Type>(X) : cast<DERIVED##Type>(X)) );
//#include "clang/AST/TypeNodes.def"
//    case GetReflectionObjKind<Type>::value:
//      return BuildReflection(S, KWLoc, (dyn ? dyn_cast<Type>(X) : cast<Type>(X)) );
//    default:
//      S.Diag(KWLoc, diag::err_reflcast_incompatible_targ_kind) << "Type" << std::to_string(ToKind);
//      return ExprError();
//  }
//}
//ExprResult Reflector::ReflectCast(ReflectionObjKind ToKind, Decl *X, bool dyn) {
////  llvm::outs() << "[DWR DEBUG] in ReflectCast for Decl*, dyn = " << dyn << "\n";
//  switch (ToKind) {
//    //Special case: DeclContext
//    case GetReflectionObjKind<DeclContext>::value:
//      return BuildReflection(S, KWLoc,
//        (dyn ? dyn_cast<DeclContext>(X) : cast<DeclContext>(X)) );
//#define DECL(DERIVED, BASE) \
//    case GetReflectionObjKind<DERIVED##Decl>::value:\
//      return BuildReflection(S, KWLoc, \
//        (dyn ? dyn_cast<DERIVED##Decl>(X) : cast<DERIVED##Decl>(X)) );
//#include "clang/AST/DeclNodes.inc"
//    case GetReflectionObjKind<Decl>::value:
//      return BuildReflection(S, KWLoc, (dyn ? dyn_cast<Decl>(X) : cast<Decl>(X)) );
//    default:
//      S.Diag(KWLoc, diag::err_reflcast_incompatible_targ_kind) << "Decl" << std::to_string(ToKind);
//      return ExprError();
//  }
//}






/// Note that this version only hanldes integer types,
/// not string literals like EvaluateReflectionArg in ExprConstant.cpp version.
/// We use it to load the first three args of a reflection trait (and
/// a few other args for reflection-related expressions).
static llvm::Optional<intptr_t> EvaluateIntReflectionArg(Sema &S, Expr *E) {
  assert(!E->isValueDependent());
  assert(!E->isTypeDependent());
//  if (E->getType()->isIntegerType()) {
    llvm::APSInt theint;
    // FIXME: Emit the right diagnostics.
    if (!E->EvaluateAsInt(theint, S.Context)) {
      S.Diag(E->getBeginLoc(), diag::err_refltrait_arg_eval_failed) << "integer constant expression";
      return {};
    }
    return theint.getZExtValue();
//  }
//  S.Diag(E->dia)
//  return {};
}

/// Called by ParseReflectionTrait, and by RebuildReflectionTraitExpr
/// whenever the first three args were not fully determined during
/// the initial call to this function.
///
/// When each the first three args ARE determined, this processes all
/// of them, makes sure their values make sense, creates proper callback
/// functions for the corresponding reflected methods/properties, and calls
/// ActOnReflectionTraitExpr with all these calculated values/callbacks
/// to do the remaining work.
///
/// This very intensive function is intended to be called once during the
/// initial processing of the generated client_reflection_impl.h; the client
/// then uses only the functions/classes in that header, rather than
/// any direct calls to __reflect_..., and thus only makes use of
/// ReflectionTraitExprs, which SHOULD be more efficient due to all
/// the below stuff having been pre-calculated and checked.
/// (DWR TODO need to test whether reality agrees with this ideal...)
///
/// Note that if the client does try to use __reflect_... traits directly,
/// there will be very little diagnostic support; we make use of asserts
/// and unreachables rather than Diags, for efficiency.  So there IS a danger
/// that a client could use a Release version of clang, make direct use of the
/// reflection traits with invalid values, and thereby create havoc due to
/// bad casts and the like.  Can't see any reason why a client would ever
/// need to direclty use the traits though (except perhaps via fully
/// parameter packed expressions in template functions, which should be safe),
/// so we'll live with this risk.
///
ExprResult Sema::ActOnReflectionTrait(SourceLocation KWLoc,
                                      ReflectionTraitKind TraitKind,
                                      ArrayRef<Expr *> Args,
                                      SourceLocation RParenLoc
//                                      , bool HasNonIntArgs
                                      ) {
  // If any of the first three args are dependent, build another ReflectionTraitExpr.
  // I.e. don't both evaluating/interpreting anything until you have at least those three.
  for (unsigned I = 0; I != 3; ++I) {
    Expr *TheArg = Args[I];
    if (TheArg->isTypeDependent() || TheArg->isValueDependent()) {
      return new (Context) ReflectionTraitExpr(Context, KWLoc, TraitKind,
//                                               Context.DependentTy,
                                               Args, RParenLoc
//                                               , HasNonIntArgs
                                               );
    }
  }

//  llvm::outs() << "DWR TEMP DEBUG no args are dependent, so creating a ReflectionTraitExpr with callbacks set...\n";

  // The first three are non-dependent: check their values, use it to
  // determine the client return type/CTD, then ActOnReflectionTraitTyped
  // with all that data:

  unsigned idx;
  llvm::Optional<intptr_t> intgetres;

  ////////////////////////////
  // Arg[0]: ObjKind
  idx = 0;
  intgetres = EvaluateIntReflectionArg(*this, Args[idx]);
  unsigned ObjKind;
  if (intgetres)
    ObjKind = *intgetres;
  else {
    Diag(KWLoc, diag::err_expected) << "An integer value (the ObjKind), but evaluation to int failed";
    return ExprError();
  }

  if (!ObjKind || ObjKind > MAX_REFLOBJKIND) {
    Diag(Args[idx]->getLocStart(), diag::err_reflection_trait_param_out_of_bounds)
            << (std::to_string(idx) + " (the ReflectionObjKind arg)")
            << ("an unsigned integer less than " + std::to_string(MAX_REFLOBJKIND))
            << ObjKind;
    return ExprError();
  } //if ObjKind out of range


  ReflectionObjKind ObjKindEnum = static_cast<ReflectionObjKind>(ObjKind);


  ////////////////////////////
  // Arg[1]: MemNum (or, for __reflect_cast, the target ObjKind)
  ++idx;
  intgetres = EvaluateIntReflectionArg(*this, Args[idx]);
  unsigned MemNum;
  if (intgetres) {
    MemNum = *intgetres;
    if (TraitKind == RTK_cast) {
      //Special case: if this is a cast trait this arg isn't really a memnum, it is another ObjKind...
      if (MemNum/*=targ ObjKind*/ > MAX_REFLOBJKIND) {
        Diag(Args[idx]->getLocStart(), diag::err_reflection_trait_param_out_of_bounds)
                << (std::to_string(idx) + " (the target ObjKind arg for a __reflect_cast trait)")
                << ("an unsigned integer less than " + std::to_string(MAX_REFLOBJKIND))
                << MemNum;
        return ExprError();
      }
    } else {
      if (MemNum >= getTotalMemNums(ObjKindEnum)) {
        Diag(Args[idx]->getLocStart(), diag::err_reflection_trait_param_out_of_bounds)
                << (std::to_string(idx) + " (the MemNum arg for ReflectionObjKind = "
                                        + to_string(ObjKindEnum) + ")")
                << ("an unsigned integer less than " + std::to_string(getTotalMemNums(ObjKindEnum)))
                << MemNum;
        return ExprError();
      } //if MemNum out of range
    }

  } else {
    Diag(KWLoc, diag::err_expected) << "An integer value (the MemNum), but evaluation to int failed";
    return ExprError();
  }


  ////////////////////////////
  // Arg[2]: IsPtr (or, for __reflect_cast, dynamic=1/static=0)
  ++idx;
  intgetres = EvaluateIntReflectionArg(*this, Args[idx]);
  unsigned IsPtr;
  if (intgetres) {
    IsPtr = *intgetres;
    if (IsPtr > 1) {
      Diag(Args[idx]->getLocStart(), diag::err_reflection_trait_param_out_of_bounds)
              << (std::to_string(idx) + " (the IsPtr arg)")
              << "0 or 1"
              << IsPtr;
      return ExprError();
    }
  } else {
    Diag(KWLoc, diag::err_expected) << "An integer value (the IsPtr value), but evaluation to int failed";
    return ExprError();
  }


//  ////////////////////////////
//  // Set RetClientReflPrimType or RetClientReflNonprimCTD
//  QualType RetClientReflPrimType;
//  ClassTemplateDecl *RetClientReflNonprimCTD;

//  QualTypeOrCTD QTorCTD = getClientReflQTorCTD(Context, ObjKindEnum, MemNum, IsPtr);

//  if (QTorCTD.isQT) {
//    RetClientReflPrimType = QTorCTD.QT;
//    RetClientReflNonprimCTD = nullptr;
//  } else {
//    RetClientReflPrimType = Context.DependentTy;
//    RetClientReflNonprimCTD = QTorCTD.CTD;
//  }



  ////////////////////////////
  // Get the Type or CB_getTypeFromArgs callback,
  // and the CB_getResultFromArgs.
  QualType type;
  function_impl<QualType(ArrayRef<intptr_t>)>  CB_getTypeFromArgs;
  function_impl<ExprResult(ArrayRef<intptr_t>, QualType)> CB_getResultFromArgs;

  LLVM_ATTRIBUTE_UNUSED Sema &SemaRef = *this;

  // Will be used in defining the callbacks.
  // Captures by copy by default, except SemaRef is by reference.
  // VERY important to capture e.g. IsPtr by value, otherwise
  // during the callback you'll get a nonsense value!
  // DWR TODO I think only ALWAYSPTR or MAYBEPTR X's actually use the SemaRef,
  // otherwise it doesn't need to be captured, so perhaps add that condition
  // somehow.  Or turn off hte warning about it begin unused
# define CB_CAPTURES  =, &SemaRef

// DWR TEMP COMMENTED OUT:
  switch (ObjKindEnum) {
#   include "reflection_incs/ReflTrait_SetTypeAndCBs_Cases.inc" //BIG file
    default:
      llvm_unreachable("Unhandled/invalid ObjKind");
  }


  ArrayRef<Expr *> RemArgs = Args.drop_front(3); //NB: omit the first three args, since already processed

  assert(RemArgs.size() == Args.size() - 3); //sanity check
  assert(!RemArgs.empty() && "Expected at least one arg (the X val)!");

  assert(!type.isNull() && "Did not set type!");

  if (type->isDependentType()) {
      return ActOnReflectionTrait(KWLoc,
                                  TraitKind,
                                  ObjKind,
                                  MemNum,
                                  IsPtr,
                                  CB_getTypeFromArgs,
                                  CB_getResultFromArgs,
                                  RemArgs,
                                  RParenLoc
//                                  , HasNonIntArgs
                                  );
  }
  assert(!CB_getTypeFromArgs && "Expected this to remain unset if type is not dependent");
  return ActOnReflectionTrait(KWLoc,
                              TraitKind,
                              ObjKind,
                              MemNum,
                              IsPtr,
                              type,
                              CB_getResultFromArgs,
                              RemArgs,
                              RParenLoc
//                              , HasNonIntArgs
                              );

}

/*
 DWR TODO:
 To avoid all these HasNonIntArgs loops for every single reflection trait,
 maybe put the bool field back in, and have TransformReflectionTraitExpr
 recalc it when it needs to form new args, and pass along either the field
 val or the recalcd val.
 */

//static bool HasNonIntArgs(ArrayRef<Expr *> Args) {
//  for (auto E : Args) {
//    if (!E->getType()->isIntegerType()
//        && !E->containsUnexpandedParameterPack() //don't count param packs
//        )
//      return true;
//  }
//  return false;
//}

/// Non-dependent type overload
ExprResult Sema::ActOnReflectionTrait(SourceLocation KWLoc,
                                      ReflectionTraitKind TraitKind,
                                      unsigned ObjKind,
                                      unsigned MemNum,
                                      bool IsPtr,
                                      QualType NonDepType,
                                      function_impl<ExprResult(ArrayRef<intptr_t>, QualType)>
                                         CB_getResultFromArgs,
                                      ArrayRef<Expr *> RemArgs,
                                      SourceLocation RParenLoc
//                                      , bool HasNonIntArgs
                                      ) {

  //DWR commented this first branch out because I did all the
  //DefaultFunctionArray... conversions in TreeTransform.h -- much better place
  // for that sort of thing.

  // If all the Args are integer typed (the usual case), nothing fancy needs to be done --
  // just build a new ReflectionTraitExpr with the same RemArgs.
  // But if there is at least one string literal arg (or perhaps other kinds, yet to be
  // processed), then we need to do some conversions so that at evaluation time we
  // can Evaluate them without ending up with a ValueDecl instead of an Expr as the
  // LValueBase:

  //DWR TODO see if maybe you can get away with just doing this once or something.

  //WAIT -- actually, just do all this nonsense in TreeTransform.h.

//  if (HasNonIntArgs) {
//    // Uncommon to get here -- usually only when we have a reflection container
//    // that takes a string literal.
//    SmallVector<Expr *, 4> NewArgs;
//#ifndef NDEBUG
//    bool foundnonintarg = false;
//#endif
//    for (auto E : RemArgs) {
//      if (E->getType()->isIntegerType())
//        NewArgs.push_back(E);
//      else {
//#ifndef NDEBUG
//        foundnonintarg = true;
//#endif
//        // Used this conversion procedure because __compiler_debug also uses it
//        // on string literal inputs; don't know quite what it does:
//        ExprResult Converted = DefaultFunctionArrayLvalueConversion(E);
//        if (Converted.isInvalid())
//          return ExprError();
//        NewArgs.push_back(Converted.get());
//      }
//    }
//    assert(foundnonintarg
//           && "HasNonIntArgs non calcd correctly, didn't "
//              "find any here. Doing needless work.");
//    assert(NewArgs.size() == RemArgs.size());
//    return new (Context) ReflectionTraitExpr(Context, KWLoc, TraitKind,
//                                             ObjKind, MemNum, IsPtr,
//                                             NonDepType, CB_getResultFromArgs,
//                                             NewArgs/*!*/, RParenLoc
//                                             , HasNonIntArgs
//                                             );
//  }


//  //DWR TEST:
//  SmallVector<Expr *, 4> NewArgs;
//  for (unsigned I = 0; I != RemArgs.size(); ++I) {
//    // Convert it so we can get an Expr * when we evaluate it later,
//    // rather than a ValueDecl*:
//    ExprResult Converted = DefaultFunctionArrayLvalueConversion(RemArgs[I]);
//    if (Converted.isInvalid())
//      return ExprError();
//    NewArgs.push_back(Converted.get());
//  }
//  //END


  // Usual case: all integer args; no need to set up a new vector
  // of them, just use the existing ArrayRef.
  return new (Context) ReflectionTraitExpr(Context, KWLoc, TraitKind,
                                           ObjKind, MemNum, IsPtr,
                                           NonDepType, CB_getResultFromArgs,
                                           RemArgs, RParenLoc
//                                           , HasNonIntArgs
                                           );
}


/// This version differs from the EvaluateReflectionArgs in ExprConstant.cpp
/// only in that only integer arguments are allowed.
/// We use this on class-returning reflection traits in evaluating
/// the type.
/// The reason we cannot accept string literal arguments: to process
/// such args, we really need an EvalInfo input to take stock of the
/// context, and help us perform substitution; but we're not in
/// any such evaluation context here.  Evaluating integer arguments
/// is for some reason mercifully less demanding.
/// It's unfortunate we have to tiptoe around implementation details,
/// but it would take too much major surgery right now to ExprConstant.cpp
/// etc. to get everything to work as it should.
static bool EvaluateIntReflectionArgs(Sema &S,
                                      ArrayRef<Expr *> Args,
                                      intptr_t *IntDataArr) {
  for (unsigned i = 0; i < Args.size(); ++i) {
    Expr *E = Args[i];
    if (auto res = EvaluateIntReflectionArg(S, E)) {
      const_cast<intptr_t &>(IntDataArr[i]) = res.getValue();
    } else {
      //Note diagnostics already provided in EvaluateReflectionArg,
      // so can just pass along the false:
      return false;
    }
    assert(!i || &IntDataArr[i] == &IntDataArr[i-1] + 1
                 && "Expected sequential storage!");
  }
  return true;
}


/// Dependent type overload (applies when the type of the expression is a class, i.e. non primitive --
/// since a new client reflection template instance will be needed for the result, we have
///
ExprResult Sema::ActOnReflectionTrait(SourceLocation KWLoc,
                                      ReflectionTraitKind TraitKind,
                                      unsigned ObjKind,
                                      unsigned MemNum,
                                      bool IsPtr,
                                      function_impl<QualType(ArrayRef<intptr_t>)> CB_getTypeFromArgs,
                                      function_impl<ExprResult(ArrayRef<intptr_t>, QualType)>
                                         CB_getResultFromArgs,
                                      ArrayRef<Expr *> RemArgs,
                                      SourceLocation RParenLoc
//                                      , bool HasNonIntArgs
                                      ) {

#ifndef NDEBUG
  assert(CB_getTypeFromArgs);
  for (auto E : RemArgs) {
    if (!E->getType()->isIntegerType() && !E->isTypeDependent()) {
      Diag(E->getBeginLoc(), diag::err_expected) << "a class-returning reflection trait to only take integral-convertible "
                       "arguments, i.e. no string literals; but this arg is not an integer.  Please adjust the GenReflectionSrc.cpp "
                       "code to exclude this trait.";
      return ExprError();
    }
  }
#endif


//  //DWR TEST:
//  SmallVector<Expr *, 4> NewArgs;
//  for (unsigned I = 0; I != RemArgs.size(); ++I) {
//    // Convert it so we can get an Expr * when we evaluate it later,
//    // rather than a ValueDecl*:
//    ExprResult Converted = DefaultFunctionArrayLvalueConversion(RemArgs[I]);
//    if (Converted.isInvalid())
//      return ExprError();
//    NewArgs.push_back(Converted.get());
//  }
//  //END


  if (AnyDependentExprs(RemArgs)) {
    //Still dependent: call the constructor that takes the CB_getTypeFromArgs callback function.
    return new (Context) ReflectionTraitExpr(Context, KWLoc, TraitKind,
                                             ObjKind, MemNum, IsPtr,
                                             CB_getTypeFromArgs/*!*/, CB_getResultFromArgs,
                                             RemArgs, RParenLoc
//                                             , HasNonIntArgs
                                             );
  }

  // All the Args are known/non-dependent; so we can (and indeed must, I think) now specify
  // the type before building the expression.
  QualType newtype;

  // The below code parallels that in EvaluateReflectionArgs in ExprConstant.cpp,
  // but we need a separate version here because that version necessarily takes
  // an EvalInfo (for doing substitutions properly for string literal args, and for
  // diagnostics), and EvalInfo is private to ExprConstant.cpp.
  auto NumArgs = RemArgs.size();

  // We split this up into cases because we MUST ensure sequential
  // temporary storage of the integer-evaluated args (so we can reinterpret_cast
  // to objects larger than integers at any point in the array), and yet would
  // like to stack allocate for reasonable numbers of args.
  // Bottom line a solution like SmallVector will NOT work here, since it might
  // split up the data chunks, causing possible havoc with our reinterpret_casts.
# define GET_FROM_INTARGS_LEQ(N) \
  if (NumArgs <= N) { \
    intptr_t IntDataRawArr[N];/*stack allocate*/\
    if (!EvaluateIntReflectionArgs(*this, RemArgs, IntDataRawArr)) \
      return ExprError();\
    /*llvm::errs() << "DWR TEMP DEBUG evaluating from arg exprs, TraitKind/ObjKind/MemNum/IsPtr="\
                 << TraitKind << "/" << ObjKind << "/" << MemNum << "/" << IsPtr << "...";*/\
    assert(CB_getTypeFromArgs);\
    newtype = CB_getTypeFromArgs(ArrayRef<intptr_t>(IntDataRawArr, NumArgs));\
    /*llvm::errs() << "done\n";*/\
  }
/**/

  GET_FROM_INTARGS_LEQ(2)
  else GET_FROM_INTARGS_LEQ(4)
  else GET_FROM_INTARGS_LEQ(16)
  else GET_FROM_INTARGS_LEQ(64)
  else llvm_unreachable("Didn't expect this many args");

# undef GET_FROM_INTARGS_LEQ


  // A null type returned from our CB_getType indicates an error
  // was encountered.  But we did not do any diagnostics within EvaluateTypeFromArgExprs
  // so need to supply one now.
  if (newtype.isNull()) {
    Diag(KWLoc, diag::err_refltrait_eval_failed_args_fine);
    //^ Other diagnostics should come before this; mainly included since we use
    // it in the ExprConstant.cpp version
    return ExprError();
  }

  // Make sure the type is complete.
  // (DWR I don't believe this does any instantiation, just makes sure the class has
  // been defined.  We don't want to instantiate at this point -- only instantiate
  // when we absolutely need to, in the callback, via defaultConstructType.)
  if (RequireCompleteType(KWLoc, newtype, diag::err_incomplete_type))
    return ExprError();

  // Call the constructor that takes a non-dependent QualType; we no longer need the
  // CB_getTypeFromArgs callback.
  // Note that we provide an EMPTY ArrayRef to RemArgs; this will signal to
  // the ExprEvaluator/VoidExprEvaluator that we don't need to evaluate
  // any args to return the result.
  return new (Context) ReflectionTraitExpr(Context, KWLoc, TraitKind,
                                           ObjKind, MemNum, IsPtr,
                                           newtype/*!*/, CB_getResultFromArgs,
                                           ArrayRef<Expr *>()/*EMPTY RemArgs*/, RParenLoc
//                                           , HasNonIntArgs
                                           );

}








ExprResult Sema::ActOnReflectNew(SourceLocation KWLoc,
                                 unsigned/*ReflectionObjKind*/ ObjKind,
                                 ArrayRef<Expr *> Args,
                                 SourceLocation RParenLoc) {

  bool isDependent = false;
  for (unsigned i = 0; i < Args.size(); ++i) {
    if (Args[i]->isTypeDependent() || Args[i]->isValueDependent()) {
      isDependent = true;
      break;
    }
  }
  llvm::Optional<intptr_t> res = {};
  if (!ObjKind && !Args.front()->isTypeDependent() && !Args.front()->isValueDependent()) {
    //ObjKind has not yet been set, but is (now) non-dependent: set it.
    res = EvaluateIntReflectionArg(*this, Args.front());
    if (!res) {
      Diag(KWLoc, diag::err_expected) << "A ReflObjKind enum value, but this is not an integer!";
      return ExprError();
    }
    ObjKind = *res;
    if (ObjKind >= ReflectionObjKind::MAX_REFLOBJKIND) {
      Diag(KWLoc, diag::err_expected) << "A ReflObjKind enum value, but this integer is too large";
      return ExprError();
    }
  }

  if (isDependent) {
    return new (Context) ReflectNewExpr(Context,
                                        ObjKind,
                                        Args, KWLoc, RParenLoc);
  }
  //Is non-dependent: time to interpret the rest of the args.
  assert(ObjKind && "Expected Args[0] to have already been set if everything is non-dependent");

  if (Args.size() > 1) {
    llvm_unreachable("Haven't implemented passing arguments to __reflect_new other than the type.");
  }

  //TODO parse this as an argument
  // For now, we're going to always a (Context) use autodelete
  // Determines whether we allocate with "new" or "new (Context)"; if the latter,
  // the deallocation will occur automatically at the end of the program; if the former,
  // we had better do __reflect_delete later or we'll have a memory leak.
  // For now, we'll just set autodelete = true and not bother worrying about
  // __reflect_delete; we might not ever really need autodelete=false / __reflect_delete
  // anyway, unless clients start requiring some really big temporary constexpr containers
  LLVM_ATTRIBUTE_UNUSED bool autodelete = true;
  LLVM_ATTRIBUTE_UNUSED Sema &SemaRef = *this;

  switch (static_cast<ReflectionObjKind>(ObjKind)) {
#   include "reflection_incs/BuildReflectNewCases.inc"
    default:
      Diag(KWLoc, diag::err_expected) << "a ReflectionObjKind that is constructible via __reflect_new. "
                                         " (For now this means it must have a default constructor; this RK did not, it seems.)";
      return ExprError();
  }

}

/// We need this to wrap the delete call, so we can build a VoidReflectionExpr out of it
/// and thereby do the delete at exactly the right time (perhaps not important, I dunno).
template<typename T>
static void deleterfunc(T *t) {
  delete t;
}







/// Largely a copy of ActOnReflectNew, except that you also parse a pointer arg AND
/// build a Void reflection so you the delete is performed exactly when expected.
ExprResult Sema::ActOnReflectDelete(SourceLocation KWLoc,
                                    unsigned/*ReflectionObjKind*/ ObjKind,
                                    ArrayRef<Expr *> Args,
                                    SourceLocation RParenLoc) {

  //DWR TEMP:
  llvm_unreachable("Implementation disabled for now, since ReflectNew is currently "
                   "implemented with autodelete = true and allowing the user to do "
                   "deletions is unnecessary and could be a source of problems. "
                   "This could however very easily be enabled if/when necessary.");
  //END TEMP

  bool isDependent = false;
  for (unsigned i = 0; i < Args.size(); ++i) {
    if (Args[i]->isTypeDependent() || Args[i]->isValueDependent()) {
      isDependent = true;
      break;
    }
  }
  llvm::Optional<intptr_t> res = {};
  if (!ObjKind && !Args[0]->isTypeDependent() && !Args[0]->isValueDependent()) {
    //ObjKind has not yet been set, but is (now) non-dependent: set it.
    res = EvaluateIntReflectionArg(*this, Args[0]);
    if (!res) {
      Diag(KWLoc, diag::err_expected) << "A ReflObjKind enum value, but this is not an integer!";
      return ExprError();
    }
    ObjKind = *res;
    if (ObjKind >= ReflectionObjKind::MAX_REFLOBJKIND) {
      Diag(KWLoc, diag::err_expected) << "A ReflObjKind enum value, but this integer is too large";
      return ExprError();
    }
  }

  if (isDependent) {
    return new (Context) ReflectDeleteExpr(Context,
                                           ObjKind,
                                           Args, KWLoc, RParenLoc);
  }
  //Is non-dependent: time to interpret the rest of the args.
  assert(ObjKind && "Expected Args[0] to have already been set if everything is non-dependent");

  if (Args.size() > 2) {
    llvm_unreachable("Expected only a ReflectionObjKind and a pointer value arg to a ReflectDeleteExpr");
  }

  res = EvaluateIntReflectionArg(*this, Args[1]);
  if (!res) {
    Diag(KWLoc, diag::err_expected) << "A pointer value, but this is not a uintptr!";
    return ExprError();
  }
  LLVM_ATTRIBUTE_UNUSED intptr_t X = *res;

  switch (static_cast<ReflectionObjKind>(ObjKind)) {
//#   include "reflection_incs/ReflectDeleteCases.inc" //DWR COMMENTED OUT since unused
    default:
      Diag(KWLoc, diag::err_expected) << "a ReflectionObjKind that is delete-able via __reflect_delete. "
                                         " (For now this means it must have a default constructor; this RK did not, it seems.)";
      return ExprError();
  }
}





