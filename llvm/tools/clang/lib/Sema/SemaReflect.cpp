//ASUTTON ADDN, DWR HEAVILY MODDED:

//===--- SemaReflect.cpp - Semantic Analysis for Reflection ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic analysis for C++ reflection.
//
//===----------------------------------------------------------------------===//



#include "TypeLocBuilder.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/ASTLambda.h" //DWR ADDN (for asserting that isLambdaCallOperator
#include "clang/AST/DeclFriend.h"
#include "clang/AST/DeclOpenMP.h" //for reflecting its props
#include "clang/AST/ExprCXX.h"
#include "clang/AST/PrettyPrinter.h" //DWR ADDN for setPrintingPolicy
#include "clang/AST/ReflectionIncludes.hpp"
#include "clang/Lex/MetaparseDebug.h" //DWR ADDN
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/CDContextVars.h" //DWR ADDN
#include "clang/Parse/Parser.h" //DWR ADDN
#include "clang/Parse/RAIIObjectsForParser.h" //DWR ADDN
#include "clang/Sema/Initialization.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/Sema/ScopeInfo.h" //DWR addition, needed for LambdaScopeInfo def

#include "reflection_incs/ReflectionHeaderID.h" //DWR ADDN

#include "ReflectedTypeEtc.h" //RequireReflNs



using namespace clang;
using namespace sema;


///// Assigns unique MemNums to each method/field name
//// DWR TODO rename all MemNums instances to TraitNum or something
//namespace refl {
//# include "reflection_incs/ReflInfoNamespaces.inc"
//}

///// \brief Identifies the AST class reflected.
//enum ReflectionObjKind {
//# include "reflection_incs/ReflectionObjKindList.inc"
//  MAX_REFLOBJKIND
//};





//DWR TODO: Move all this ConstexprDecl stuff into a separate cpp -- basically,
// everything that doesn't rely on BuildReflection/ReflectedConstruct etc.
// needs to get out of this file.

//ASUTTON ADDN:
/// Returns true if a constexpr-declaration in declaration context DC
/// would be represented using a function (vs. a lambda).
static inline bool NeedsFunctionRepresentation(DeclContext *DC
                                               , bool Nested //DWR ADDN
                                               ) {
  return
//      !Nested && //DWR ADDN
      (DC->isFileContext() || DC->isRecord());
  //I.e. if its nested, we always go lambda (DWR TEST)
}

/// Create a constexpr-declaration that will hold the body of the
/// constexpr-declaration.
///
/// \p ScopeFlags is set to the value that should be used to create the scope
/// containing the constexpr-declaration body.
Decl *Sema::ActOnConstexprDecl(Scope *S, SourceLocation ConstexprLoc,
                               unsigned &ScopeFlags
                               , CDContextVars &cdcv //DWR ADDN
                               , bool Nested //DWR ADDN
                               ) {

  ConstexprDecl *CD;

////DWR TEMP DEBUG
//  llvm::outs() << "DWR TEMP DEBUG; about to test whether NeedsFunctionRepresentation"
//                  "(i.e. whether CurContext is a class or file context; i.e. not a "
//                  "func or method); here's the Nested/CurContext "
//                  "name/kind/NeedsFunctionRepresentation: ";
//  auto ND = dyn_cast<NamedDecl>(CurContext);
//  llvm::outs() << Nested << "/"
//               << (ND ? ND->getQualifiedNameAsString() : "[unnamed]")
//               << "/" << CurContext->getDeclKindName()
//               << "/" << NeedsFunctionRepresentation(CurContext, Nested)
//               << "\n";
////END

  if (NeedsFunctionRepresentation(CurContext
                                  , Nested //DWR ADDN
                                  )) {
    ScopeFlags = Scope::FnScope | Scope::DeclScope;

    PushFunctionScope(); //DWR TEMP COMMENTED OUT
//    LambdaScopeInfo *LSI = PushLambdaScope();// DWR TEMP TEST FIXME!!!
//    LSI->


    // Build the function
    //
    //  constexpr void __constexpr_decl() compound-statement
    //
    // where compound-statement is the as-of-yet parsed body of the
    // constexpr-declaration.
    IdentifierInfo *II = &PP.getIdentifierTable().get("__constexpr_decl");
    DeclarationName Name(II);
    DeclarationNameInfo NameInfo(Name, ConstexprLoc);

    FunctionProtoType::ExtProtoInfo EPI(
        Context.getDefaultCallingConvention(/*IsVariadic=*/false,
                                            /*IsCXXMethod=*/false));
    QualType FunctionTy = Context.getFunctionType(Context.VoidTy, None, EPI);
    TypeSourceInfo *FunctionTyInfo =
        Context.getTrivialTypeSourceInfo(FunctionTy);

    // FIXME: Why is the owner the current context? We should probably adjust
    // this to the constexpr-decl later on. Maybe the owner should be the
    // nearest file context, since this is essentially a non-member function.
    FunctionDecl *Function =
        FunctionDecl::Create(Context, CurContext, ConstexprLoc, NameInfo,
                             FunctionTy, FunctionTyInfo, SC_None,
                             /*isInlineSpecified=*/false,
                             /*hasWrittenPrototype=*/true,
                             /*isConstexprSpecified=*/true);
    Function->setImplicit();
    Function->setMetaprogram();

    // Build the constexpr declaration around the function.
    CD = ConstexprDecl::Create(Context, CurContext, ConstexprLoc, cdcv, Function);

  } else if (CurContext->isFunctionOrMethod()
             || Nested //DWR ADDN
             ) {
    ScopeFlags = Scope::BlockScope | Scope::FnScope | Scope::DeclScope;

    LambdaScopeInfo *LSI = PushLambdaScope();

    // Build the expression
    //
    //    []() -> void compound-statement
    //
    // where compound-statement is the as-of-yet parsed body of the
    // constexpr-declaration. Note that the return type is not deduced (it
    // doesn't need to be).
    //
    // TODO: It would be great if we could only capture constexpr declarations,
    // but C++ doesn't have a constexpr default.
    const bool KnownDependent = S->getTemplateParamParent();

    FunctionProtoType::ExtProtoInfo EPI(
        Context.getDefaultCallingConvention(/*IsVariadic=*/false,
                                            /*IsCXXMethod=*/true));
    EPI.HasTrailingReturn = true;
    EPI.TypeQuals |= DeclSpec::TQ_const;
    QualType MethodTy = Context.getFunctionType(Context.VoidTy, None, EPI);
    TypeSourceInfo *MethodTyInfo = Context.getTrivialTypeSourceInfo(MethodTy);

    LambdaIntroducer Intro;
    Intro.Range = SourceRange(ConstexprLoc);
    Intro.Default = LCD_ByRef; //DWR MOD; ASUTTON used LCD_None

    CXXRecordDecl *Closure = createLambdaClosureType(
        Intro.Range, MethodTyInfo, KnownDependent, Intro.Default);

    CXXMethodDecl *Method =
        startLambdaDefinition(Closure, Intro.Range, MethodTyInfo, ConstexprLoc,
                              None, /*IsConstexprSpecified=*/true);
    buildLambdaScope(LSI, Method, Intro.Range, Intro.Default, Intro.DefaultLoc,
                     /*ExplicitParams=*/false,
                     /*ExplicitResultType=*/true,
                     /*Mutable=*/false);
    Method->setMetaprogram();

    // NOTE: The call operator is not yet attached to the closure type. That
    // happens in ActOnFinishConstexprDecl(). The operator is, however,
    // available in the LSI.
    CD = ConstexprDecl::Create(Context, CurContext, ConstexprLoc, cdcv, Closure);
  } else {
//    Decl::castFromDeclContext(CurContext)->dump();
    llvm_unreachable("constexpr declaration in unsupported context");
  }

  // Add the declaration to the current context. This will be removed from the
  // AST after evaluation.
  CurContext->addDecl(CD);

  return CD;
}








/*-----------------------------------------------------------------*/
///  Sema ConstexprDecl implems (ActOn, Evaluate, etc.)

// DWR TODO move all of this into a separate file, since
// constexpr decls are common to metaparsing and reflection.
// Or alternatively, move the SemaMetaparse stuff here too and name
// this file more generally.

/// Called just prior to parsing the body of a constexpr-declaration.
///
/// This ensures that the declaration context is pushed with the appropriate
/// scope.
void Sema::ActOnStartConstexprDecl(Scope *S, Decl *D) {


  ConstexprDecl *CD = cast<ConstexprDecl>(D);

  if (CD->hasFunctionRepresentation()) {
    if (S)
      PushDeclContext(S, CD->getFunctionDecl());
    else
      CurContext = CD->getFunctionDecl();
  } else {
    LambdaScopeInfo *LSI = cast<LambdaScopeInfo>(FunctionScopes.back());
    if (S)
      PushDeclContext(S, LSI->CallOperator);
    else
      CurContext = LSI->CallOperator;

    PushExpressionEvaluationContext(
        ExpressionEvaluationContext::PotentiallyEvaluated);

  }



}

/// Called immediately after parsing the body of a constexpr-declaration.
///
/// The statements within the body are evaluated here.
void Sema::ActOnFinishConstexprDecl(Scope *S, Decl *D, Stmt *Body) {
  METAPARSE_DEBUG_HDR("In ActOnFinishConstexprDecl...")
  ConstexprDecl *CD = cast<ConstexprDecl>(D);

//  PopFunctionScopeInfo(); //DWR ADDN -- you pushed above, shouldn't you pop here?  I dunno.

  if (CD->hasFunctionRepresentation()) {
    FunctionDecl *Fn = CD->getFunctionDecl();

    ActOnFinishFunctionBody(Fn, Body);
    if (!CurContext->isDependentContext()) {

      METAPARSE_DEBUG("Is non-lambda, non-dependent: will save parse state, then EvaluateConstexprDecl...")

      //DWR TEMP DEBUG
#ifndef NDEBUG
      if (CurContext->isFunctionOrMethod()) {
          Decl::castFromDeclContext(CurContext)->dump();
          llvm_unreachable("This ConstexprDecl should have function representation! FIX");
      }
#endif

      assert(!CD->isDependent());
      ParserBrickWallRAII SavedParserState(*TheParser);

      EvaluateConstexprDecl(CD, Fn);

      //DWR POSSIBLE FIXME: it's arguable that the below could be DEBUG only,
      //since the ParserBrickWallRAII only uses the Valid bool to control
      //whether it does rigorous asserts or not, at least for now...
      if (auto Record = dyn_cast<CXXRecordDecl>(CurContext)) {
        if (Record->isInvalidDecl()) {
          METAPARSE_DEBUG("ERROR: (Non-template) class had errors parsed in __queue_metaparse statements")
          SavedParserState.setInvalid();
        }
      }
    } else {
      CD->setDependentTrue();
      Decl::castFromDeclContext(CurContext)->setInstantiationsWillNeedParsingTrue();
      METAPARSE_DEBUG("Is non-lambda, DEPENDENT context: will delay evaluation until instantiation.")
    }

  } else {

    //    PopExpressionEvaluationContext(); //DWR ADDN why isn't this in here, to match the Push above?  See if you need it...

    ExprResult Lambda = ActOnLambdaExpr(CD->getLocation(), Body, S);
    if (!CurContext->isDependentContext()) {
      METAPARSE_DEBUG("Is LAMBDA, non-dependent context: will save parse state, then about to EvaluateConstexprDecl...")
      assert(!CD->isDependent());

      //DWR TODO: you should only set up the brick wall when you have PARSE statements in the constexpr decl.
      // Or set up a param to each constexpr decl that determines whether metaparsing is allowed.
      // THen only set up the brick wall when you might/will need to do parsing.  (Be sure to change the above case too.)
      // OR WAIT -- just set up the brick wall in DoQueuedMetaparsing!  No need for it before then.  DWR FIXME!!! do this.
      ParserBrickWallRAII SavedParserState(*TheParser);
      EvaluateConstexprDecl(CD, Lambda.get());
    } else {
      CD->setDependentTrue();
      Decl::castFromDeclContext(CurContext)->setInstantiationsWillNeedParsingTrue();
      CD->setLambdaExpr(Lambda.get());
      METAPARSE_DEBUG("Is LAMBDA, DEPENDENT context: will delay evaluation until instantiation.")
    }
  }

  // If we didn't have a scope when building this, we need to restore the
  // current context.
  if (!S)
    CurContext = CD->getDeclContext();
}

/// Called when an error occurs while parsing the constexpr-declaration body.
void Sema::ActOnConstexprDeclError(Scope *S, Decl *D) {
  ConstexprDecl *CD = cast<ConstexprDecl>(D);
  CD->setInvalidDecl();
  if (CD->hasFunctionRepresentation()) {
    ActOnFinishFunctionBody(CD->getFunctionDecl(), nullptr);
  } else
//DWR ADDN
    DiscardCleanupsInEvaluationContext();
    PopExpressionEvaluationContext();
//END

    ActOnLambdaError(CD->getLocation(), S);

  // Remove the declaration; we don't want to see it in the source tree.
  //
  // FIXME: Do we really want to do this?  (DWR TODO good question, consider this)
  CD->getDeclContext()->removeDecl(CD);
}

/// Process a constexpr-declaration.
///
/// This builds an unnamed constexpr void function whose body is that of
/// the constexpr-delaration, and evaluates a call to that function.
bool Sema::EvaluateConstexprDecl(ConstexprDecl *CD,
                                 FunctionDecl *D) {
  QualType FunctionTy = D->getType();
  DeclRefExpr *Ref =
      new (Context) DeclRefExpr(D, /*RefersToEnclosingVariableOrCapture=*/false,
                                FunctionTy, VK_LValue, SourceLocation());
  QualType PtrTy = Context.getPointerType(FunctionTy);
  ImplicitCastExpr *Cast =
      ImplicitCastExpr::Create(Context, PtrTy, CK_FunctionToPointerDecay, Ref,
                               /*BasePath=*/nullptr, VK_RValue);

  CallExpr *Call =
      new (Context) CallExpr(Context, Cast, ArrayRef<Expr *>(), Context.VoidTy,
                             VK_RValue, SourceLocation());

  return EvaluateConstexprDeclCall(CD, Call);
}

/// Process a constexpr-declaration.
///
/// This builds an unnamed \c constexpr \c void function whose body is that of
/// the constexpr-delaration, and evaluates a call to that function.
bool Sema::EvaluateConstexprDecl(ConstexprDecl *CD,
                                 Expr *E) {
  LambdaExpr *Lambda = cast<LambdaExpr>(E);
  CXXMethodDecl *Method = Lambda->getCallOperator();
  QualType MethodTy = Method->getType();
  DeclRefExpr *Ref = new (Context)
      DeclRefExpr(Method, /*RefersToEnclosingVariableOrCapture=*/false,
                  MethodTy, VK_LValue, SourceLocation());
  QualType PtrTy = Context.getPointerType(MethodTy);
  ImplicitCastExpr *Cast =
      ImplicitCastExpr::Create(Context, PtrTy, CK_FunctionToPointerDecay, Ref,
                               /*BasePath=*/nullptr, VK_RValue);
  CallExpr *Call = new (Context) CXXOperatorCallExpr(Context, OO_Call,
                                                     Cast, {Lambda},
                                                     Context.VoidTy,
                                                     VK_RValue,
                                                     SourceLocation(),
                                                     FPOptions());
  return EvaluateConstexprDeclCall(CD, Call);
}

/// Evaluate the expression.
///
/// \returns  \c true if the expression \p E can be evaluated, \c false
///           otherwise.
///
// FIXME: We probably want to trap declarative effects so that we can apply
// them as declarations after execution. That would require a modification to
// EvalResult (e.g., an injection set?).
bool Sema::EvaluateConstexprDeclCall(ConstexprDecl *CD,
                                     CallExpr *Call) {
  // Associate the call expression with the declaration.
  CD->setCallExpr(Call);

////ASUTTON ADDN:
//  // Don't evaluate the call if this declaration appears within a metaclass.
//  if (CXXRecordDecl *RD = dyn_cast_or_null<CXXRecordDecl>(CurContext)) {
//    if (RD->isMetaclassDefinition())
//      return true;
//  }
////END

  // assert(InjectedStmts.empty() && "Residual injected statements");

  SmallVector<PartialDiagnosticAt, 8> Notes;

  Expr::EvalResult Result;
  Result.Diag = &Notes;

////ASUTTON ORIG
//// [DWR NOTE: in ASutton's version when you add more than 16 effects I think there
//// may have been be memory allocation issues if I recall, probably having to do with
//// the "delete" calls in ~EvalEffect().  Moot point for now, all commented out.]
  //  SmallVector<EvalEffect, 16> Effects;
  //  Result.Effects = &Effects;
////END
//DWR REPLACEMENT:
    SmallVector<const StringLiteral *, 16> MetaCodeChunks; //DWR ADDN
    Result.MetaCodeChunks = &MetaCodeChunks; //DWR ADDN
//END

//  llvm::errs() << "\nDWR TEMP DEBUG: About to evaluate constexpr decl...\n";
  assert(Call->getType()->isVoidType());
  bool Folded = Call->EvaluateAsVoid(Result, Context); //DWR MOD: used to be EvaluateAsRValue
//  llvm::errs() << "DWR TEMP DEBUG: ... DONE evaluating constexpr decl.  Folded? " << Folded << "\n\n";

//  llvm::outs() << "DWR TEMP DEBUG after EvaluateAsRValue, Result.MetaCodeChunks->size(): " << Result.MetaCodeChunks->size() << "\n";

  if (!Folded) {

    // Sometimes we may have a fold failure without other errors,
    // due to e.g. the condition on a non-constexpr if encountering
    // an error.  We want an error to stop the program in such cases.
    // TODO if we could check if other errors had already been
    // raised, we should avoid this error in such cases, as it's
    // redundant.
    if (Notes.empty())
      Diag(CD->getLocEnd(), diag::err_constexpr_decl_fold_failure);
    else {
      // If we got a compiler error, then just emit that.
      if (Notes[0].second.getDiagID() == diag::err_user_defined_error)
        Diag(CD->getLocStart(), Notes[0].second);
      else /*if (Notes[0].second.getDiagID() != diag::note_constexpr_uninitialized)*/ {
        // FIXME: These source locations are wrong. (DWR not sure what this means.)
        Diag(CD->getLocEnd(), diag::err_constexpr_decl_fold_failure);
        for (const PartialDiagnosticAt &Note : Notes)
          Diag(Note.first, Note.second);
      }
    }

    //DWR FIXME!!! now that we're using EvaluateAsVoid, I think we need to give some
    //error or warning that it wasn't folded.
    // (Also, futz with the evaluation mode you use in EvaluateAsVoid -- that's where
    // I think maybe you can get different behavior, hopefully.)
  }

  SourceLocation POI = CD->getSourceRange().getEnd();

  // Apply any modifications, and if successful, remove the declaration from
  // the class; it shouldn't be visible in the output code.

//  ApplyEffects(POI, Effects, CD); //ASUTTON ORIG
//DWR REPLACEMENT
  if (!MetaCodeChunks.empty())
    DoQueuedMetaparsing(POI, MetaCodeChunks, CD);
//END

  // FIXME: Do we really want to remove the metaprogram after evaluation? Or
  // should we just mark it completed.
  CD->getDeclContext()->removeDecl(CD);

  return !Notes.empty();
}
//END







/*-----------------------------------------------------------------*/
///  Sema ConcatenateExpr implems

ExprResult Sema::ActOnCXXConcatenateExpr(SmallVectorImpl<Expr *>& Parts,
                                         SourceLocation KWLoc,
                                         SourceLocation LParenLoc,
                                         SourceLocation RParenLoc) {
  return BuildCXXConcatenateExpr(Parts, KWLoc);
}

//static bool IsConstCharPtr(Sema &SemaRef, QualType T) {
//  T = T.getCanonicalType();
//  if (auto Ptr = dyn_cast<PointerType>(T))
//    T = Ptr->getPointeeType();
//  return T.isConstQualified() && T->isCharType();
//}

static bool CheckConcatenateOperand(QualType T) {
  T = T.getCanonicalType();
  if (isConstCharPtrType(T))
    return true;
  if (T->isIntegerType())
    return true;
  return false;
}

static bool CheckConcatenateOperand(Sema &SemaRef, Expr *E) {
  QualType T = E->getType();
  if (CheckConcatenateOperand (T))
    return true;

  // FIXME: This is the wrong diagnostic.
  SemaRef.Diag(E->getLocStart(), diag::err_concat_invalid_operand_type) << T;
  return false;
}

ExprResult Sema::BuildCXXConcatenateExpr(SmallVectorImpl<Expr *>& Parts,
                                         SourceLocation KWLoc) {
  // The type of the expression is 'const char*'.
  QualType T = Context.getPointerType(Context.getConstType(Context.CharTy));

  // Look for dependent arguments.
  for (Expr *E : Parts) {
    if (E->isTypeDependent() || E->isValueDependent())
      return new (Context) CXXConcatenateExpr(Context, T, KWLoc, Parts);
    //DWR TEMP DEBUG:
    assert(!E->getType()->containsUnexpandedParameterPack() &&
           "DWR TEMP DEBUG assumed this would be marked dependent");
    //END
  }

  // Convert operands to rvalues.
  SmallVector<Expr*, 4> Converted;
  for (Expr *E : Parts) {
    // Decay arrays first.
    ExprResult R = DefaultFunctionArrayLvalueConversion(E);
    if (R.isInvalid())
      return ExprError();
    E = R.get();

    // Check that the operand (type) is acceptable.
    if (!CheckConcatenateOperand(*this, E))
      return ExprError();

    // TODO: Perform a tentative evaluation to see if this
    // is a constant expression or no?

    Converted.push_back(E);
  }

  return new (Context) CXXConcatenateExpr(Context, T, KWLoc, Converted);
}






/*-----------------------------------------------------------------*/
///  IDEXPR AND ITS HELPER FUNCTIONS

//ASUTTON ADDN:
static bool AppendStringValue(Sema& S, llvm::raw_ostream& OS,
                              const APValue& Val) {

  // Extracting the string valkue from the LValue.
  //
  // FIXME: We probably want something like EvaluateAsString in the Expr class.
  APValue::LValueBase Base = Val.getLValueBase();
  if (Base.is<const Expr *>()) {
    const Expr *BaseExpr = Base.get<const Expr *>();
    assert(isa<StringLiteral>(BaseExpr) && "Not a string literal");
    const StringLiteral *Str = cast<StringLiteral>(BaseExpr);
    OS << Str->getString();
  } else {
    llvm_unreachable("Use of string variable not implemented");
    // const ValueDecl *D = Base.get<const ValueDecl *>();
    // return Error(E->getMessage());
  }
  return true;
}
static bool AppendCharacterArray(Sema& S, llvm::raw_ostream &OS, Expr *E,
                                 QualType T) {
  assert(T->isArrayType() && "Not an array type");
  const ArrayType *ArrayTy = cast<ArrayType>(T.getTypePtr());

  // Check that the type is 'const char[N]' or 'char[N]'.
  QualType ElemTy = ArrayTy->getElementType();
  if (!ElemTy->isCharType()) {
    S.Diag(E->getLocStart(), diag::err_concat_invalid_operand_type) << T;
    return false;
  }

  // Evaluate the expression.
  Expr::EvalResult Result;
  if (!E->EvaluateAsLValue(Result, S.Context)) {
    // FIXME: Include notes in the diagnostics.
    S.Diag(E->getLocStart(), diag::err_expr_not_ice) << 1;
    return false;
  }

  return AppendStringValue(S, OS, Result.Val);
}

static bool AppendCharacterPointer(Sema& S, llvm::raw_ostream &OS, Expr *E,
                                   QualType T) {
  assert(T->isPointerType() && "Not a pointer type");
  const PointerType* PtrTy = cast<PointerType>(T.getTypePtr());

  // Check for 'const char*'.
  QualType ElemTy = PtrTy->getPointeeType();
  if (!ElemTy->isCharType() || !ElemTy.isConstQualified()) {
    S.Diag(E->getLocStart(), diag::err_concat_invalid_operand_type) << T;
    return false;
  }

  // Try evaluating the expression as an rvalue and then extract the result.
  Expr::EvalResult Result;
  if (!E->EvaluateAsRValue(Result, S.Context)) {
    // FIXME: This is not the right error.
    S.Diag(E->getLocStart(), diag::err_expr_not_ice) << 1;
    return false;
  }

  return AppendStringValue(S, OS, Result.Val);
}

static bool AppendInteger(Sema& S, llvm::raw_ostream &OS, Expr *E, QualType T) {
  llvm::APSInt N;
  if (!E->EvaluateAsInt(N, S.Context)) {
    S.Diag(E->getLocStart(), diag::err_expr_not_ice) << 1;
    return false;
  }
  OS << N;
  return true;
}

////DWR FIXME are we still using this? It's only used in BuildIdExpr, which also maybe we can get rid of.  Bottom line
//// don't worry about this too much I think.
//static bool
//AppendReflection(Sema& S, llvm::raw_ostream &OS, Expr *E, QualType T) {
//  InitialReflectedConstruct RC = EvaluateReflection(S, E);
//
//  // DWR MODIFICATION: replaced if statements with switch
//  // (since some reflection kinds we might add to this
//  // are values, e.g. QualTypes, their getAs... implems
//  // might not return a "false"-convertible value,
//  // so it's safer to use RC.getKind() to distinguish)
//  switch(RC.getKind()) {
//    case IRK_Decl: {
//      Decl *D = RC.getAsDeclaration();
//      // If this is a named declaration, append its identifier.
//      if (!isa<NamedDecl>(D)) {
//        // FIXME: Improve diagnostics.
//        S.Diag(E->getLocStart(), diag::err_reflection_not_supported)
//                << "a non-named decl";
//        return false;
//      }
//      NamedDecl *ND = cast<NamedDecl>(D);
//
//      // FIXME: What if D has a special name? For example operator==?
//      // What would we append in that case?
//      DeclarationName Name = ND->getDeclName();
//      if (!Name.isIdentifier()) {
//        S.Diag(E->getLocStart(), diag::err_idexpr_not_an_identifer) << Name;
//        return false;
//      }
//
//      OS << ND->getName();
//      break;
//    }
//    case IRK_Type: {
//      Type *T = RC.getAsType();
//      // If this is a class type, append its identifier.
//      if (auto *RC = T->getAsCXXRecordDecl())
//        OS << RC->getName();
//      else {
//        S.Diag(E->getLocStart(), diag::err_idexpr_not_an_identifer)
//                << QualType(T, 0);
//        return false;
//      }
//      break;
//    }
//    default:
//      llvm_unreachable("Unhandled InitialReflectionObjKind");
//  }
//  return true;
//}

static bool HasDependentParts(SmallVectorImpl<Expr *>& Parts) {
 return std::any_of(Parts.begin(), Parts.end(), [](const Expr *E) {
    return E->isTypeDependent();
  });
}

/// Constructs a new identifier from the expressions in Parts. Returns nullptr
/// on error.
DeclarationNameInfo Sema::BuildIdExprName(SourceLocation OpLoc,
                                          SmallVectorImpl<Expr *>& Parts,
                                          SourceLocation EndLoc) {

  // If any components are dependent, we can't compute the name.
  if (HasDependentParts(Parts)) {
    DeclarationName Name
      = Context.DeclarationNames.getCXXIdExprName(Parts.size(), &Parts[0]);
    DeclarationNameInfo NameInfo(Name, OpLoc);
    NameInfo.setCXXIdExprNameRange({OpLoc, EndLoc});
    return NameInfo;
  }

  SmallString<256> Buf;
  llvm::raw_svector_ostream OS(Buf);
  for (std::size_t I = 0; I < Parts.size(); ++I) {
    Expr *E = Parts[I];

    assert(!E->isTypeDependent() && !E->isValueDependent()
        && "Dependent name component");

    // Get the type of the reflection.
    QualType T = E->getType();
    if (AutoType *D = T->getContainedAutoType()) {
      T = D->getDeducedType();
      if (!T.getTypePtr())
        llvm_unreachable("Undeduced value reflection");
    }
    T = Context.getCanonicalType(T);

    SourceLocation ExprLoc = E->getLocStart();

    // Evaluate the sub-expression (depending on type) in order to compute
    // a string part that will constitute a declaration name.
    if (T->isConstantArrayType()) {
      if (!AppendCharacterArray(*this, OS, E, T))
        return DeclarationNameInfo();
    }
    else if (T->isPointerType()) {
      if (!AppendCharacterPointer(*this, OS, E, T))
        return DeclarationNameInfo();
    }
    else if (T->isIntegerType()) {
      if (I == 0) {
        // An identifier cannot start with an integer value.
        Diag(ExprLoc, diag::err_idexpr_with_integer_prefix);
        return DeclarationNameInfo();
      }
      if (!AppendInteger(*this, OS, E, T))
        return DeclarationNameInfo();
    }
////DWR COMMENTED OUT
//    else if (T->isRecordType()) {
//      if (!AppendReflection(*this, OS, E, T))
//        return DeclarationNameInfo();
//    }
////END
    else {
      Diag(ExprLoc, diag::err_concat_invalid_operand_type) << T;
      return DeclarationNameInfo();
    }
  }

  // FIXME: Should we always return a declaration name?
  IdentifierInfo *Id = &PP.getIdentifierTable().get(Buf);
  DeclarationName Name = Context.DeclarationNames.getIdentifier(Id);
  return DeclarationNameInfo(Name, OpLoc);
}

/// Constructs a new identifier from the expressions in Parts. Returns false
/// if no errors were encountered.
bool Sema::BuildDeclnameId(SmallVectorImpl<Expr *>& Parts,
                           UnqualifiedId& Result,
                           SourceLocation KWLoc,
                           SourceLocation LParenLoc,
                           SourceLocation RParenLoc) {
  DeclarationNameInfo NameInfo = BuildIdExprName(KWLoc, Parts, RParenLoc);
  DeclarationName Name = NameInfo.getName();
  if (!Name)
    return true;
  if (Name.getNameKind() == DeclarationName::CXXIdExprName)
    Result.setIdExprOperator(KWLoc, Name.getCXXIdExprArguments(), RParenLoc);
  else
    Result.setIdentifier(Name.getAsIdentifierInfo(), KWLoc);
  return false;
}
//END
////ASUTTON ADDN:
//ExprResult Sema::ActOnHasNameExpr(SourceLocation KWLoc, Expr *E,
//                                  UnqualifiedId & I, SourceLocation RParenLoc) {
//  ReflectedConstruct RC = EvaluateReflection(S, E);
//  if (!RC)
//    return ExprError();
//
//  DeclarationName N1;
//  if (Decl *D = RC.getAsDeclaration()) {
//    // Get the declration name.
//    if (!isa<NamedDecl>(D)) {
//      Diag(E->getLocStart(), diag::err_reflection_not_named);
//      return ExprError();
//    }
//    NamedDecl *ND = cast<NamedDecl>(D);
//    N1 = ND->getDeclName();
//  } else if (Type *T = RC.getAsType()) {
//    // Only get the declaration name for classes.
//    //
//    // FIXME: Why only classes?
//    if (!T->isRecordType()) {
//      Diag(E->getLocStart(), diag::err_idexpr_not_an_identifer)
//        << QualType(T, 0);
//      return ExprError();
//    }
//    CXXRecordDecl *C = T->getAsCXXRecordDecl();
//    N1 = C->getDeclName();
//  }
//
//  // DeclarationName N2 = GetNameFromUnqualifiedId(I).getName();
//  // if (N1 == N2)
//  //   llvm::outs() << "TRUE\n";
//  // else
//  //   llvm::outs() << "FALSE\n";
//  llvm_unreachable("Not implemented");
//
//  // Build a declaration name form I.
//  return ExprError();
//}
////END









///*-----------------------------------------------------------------*/
////  Reflector

///// A functor, constructed from a __reflect_X instance (in
///// ActOnReflectionTraitExpr, for non-dependent cases), whose
///// call operator:
///// 1) decodes the reflection data chunk(s), guided by the
/////    already-decoded ObjKind/MemNum/IsPtr values;
///// 2) decodes any additional paramater data supplied to the
/////    __reflect_X call, depending on the MemNum;
///// 3) calls the corresponding method on the corresponding reflection
/////    data with the decoded parameters, and
///// 4) calls BuildReflection on the returned object, if the called
/////    method was non-void.
//class Reflector {
//  Sema &S;
//  const SourceLocation KWLoc;
//  const SourceLocation RParenLoc;
//  ReflectionTraitKind TraitKind;
//  ReflectionObjKind ObjKind;
//  unsigned MemNum;
//  bool IsPointer;
//  ArrayRef<const intptr_t> remargdata;

//  /// The current argdata index from which the next parameter is to be loaded.
//  unsigned curdataidx;

//  /// Set on construction and update every time you advance the curdataidx;
//  bool no_more;

//  // Defined below:

//  template<typename T>
//  bool sufficient_argdata_for_next() const;

//  ///NON-REFERENCE version
//  template<typename T, typename std::enable_if<!std::is_reference<T>::value, int>::type = 0>
//  T get_next_as();

//  ///REFERENCE version
//  template<typename T, typename std::enable_if<std::is_reference<T>::value, int>::type = 0>
//  T get_next_as();

//public:
//  Reflector(Sema &S, SourceLocation KWLoc, SourceLocation RParenLoc,
//            ReflectionTraitKind TraitKind, ReflectionObjKind ObjKind, unsigned MemNum, unsigned IsPtr,
//            ArrayRef<intptr_t> remargdata);

//  ExprResult operator()();

//# include "reflection_incs/ReflectPropDecls.inc"

//  ExprResult ReflectCast(ReflectionObjKind ToKind, Stmt *X, bool dyn);
//  ExprResult ReflectCast(ReflectionObjKind ToKind, Type *X, bool dyn);
//  ExprResult ReflectCast(ReflectionObjKind ToKind, Decl *X, bool dyn);

//}; //class Reflector




// STATIC HELPER FUNCTIONS FOR LOADING REFLECTION DATA:












//template<typename T>
//static bool sufficient_argdata_for_next(ArrayRef<intptr_t> Args) {
//  return Args.size() >= getNumReqChunks<T>();
//}

///// NON-reference version (handles pointers and values)
//template< typename T
//        , typename std::enable_if<!std::is_reference<T>::value, int>::type >
//T get_next_as(ArrayRef<intptr_t> &Args) {
////    llvm::outs() << "[DWR DEBUG]; Non-ref version of get_next_as; starting/ending data idx/remargdata.size(): " << curdataidx << "/";
//  assert(sufficient_argdata_for_next<T>());
//  intptr_t &data = const_cast<intptr_t &>(*Args.data());
//  T res = reinterpret_cast<const T&>(data);
//  Args.drop_front(getNumReqChunks<T>());
//}

/////REFERENCE version
//template< typename T
//        , typename std::enable_if<std::is_reference<T>::value, int>::type >
//T get_next_as(ArrayRef<intptr_t> &Args) {
////  llvm::outs() << "[DWR DEBUG] Calling Reference version of get_next_as, which should "
////                  "call non-ref version with pointer then deref the result ...\n";
//  using NoRefT = typename std::remove_reference<T>::type;
//  static_assert(!std::is_pointer<NoRefT>::value,
//                "Have not yet accounted for get_next_as<...> for a reference to a pointer; "
//                "consider whether you need to do anything special.");
//  return *get_next_as<NoRefT *>(Args);
//}



//// REFLECTOR TEMPLATE METHODS IMPLEMS:

//template<typename T>
//bool Reflector::sufficient_argdata_for_next() const {
//  return (remargdata.size() - curdataidx) >= getNumReqChunks<T>();
//}

///// A helper function to make sure the inputs to __reflect_X look
///// as expected -- e.g. that we're not providing a pointer where we
///// expect a bool etc.
//template<typename T, typename U = intptr_t,
//        typename std::enable_if<sizeof(T) % sizeof(U) == 0, int>::type = 0 >
//static bool unusedBitsAreZeroed(U u) {
//  return true;
//}

//template<typename T, typename U = intptr_t,
//        typename std::enable_if<(sizeof(T) % sizeof(U) != 0)
//                                && (sizeof(T) < sizeof(U)), int>::type = 0 >
//static bool unusedBitsAreZeroed(U u) {
//  //DWR HACK: this function isn't working for non-integral types, so FIXME and remove this.  Not a big deal though I guess.
//  if (!std::is_integral<T>::value)
//    return true;
//  //END HACK
//  return !( u >> (8 * sizeof(T)) );
//}

//template<typename T, typename U = intptr_t,
//        typename std::enable_if<(sizeof(T) % sizeof(U) != 0)
//                                && (sizeof(T) > sizeof(U)), int>::type = 0 >
//static bool unusedBitsAreZeroed(U u) {
//  llvm_unreachable("Not implemented, rare case");
//  return true;
//}

///// NON-reference version
//template< typename T
//        , typename std::enable_if<!std::is_reference<T>::value, int>::type >
//T Reflector::get_next_as() {
////    llvm::outs() << "[DWR DEBUG]; Non-ref version of get_next_as; starting/ending data idx/remargdata.size(): " << curdataidx << "/";
//  assert(sufficient_argdata_for_next<T>());
//  intptr_t &data = const_cast<intptr_t &>(remargdata[curdataidx]);
//  T res = reinterpret_cast<const T&>(data);
//  // This will check if you expect a bool / small enum etc. but see
//  // a bunch of non-zeros in the ignored more-significant bits
//  // within the chunk:

//#ifndef NDEBUG
//  if (!unusedBitsAreZeroed<T>(remargdata[curdataidx])) {
//    llvm::outs() << "DWR DEBUG ERROR: remargdata[curdataidx] = " << remargdata[curdataidx]
//                 << ", sizeof(T): " << sizeof(T)
//                 << ", std::is_same<T, bool>::value = " << std::is_same<T, bool>::value
//                 << ", std::is_same<T, intptr_t>::value = " << std::is_same<T, intptr_t>::value
//                 << ", curdataidx = " << curdataidx << "\n";
//    assert(false && "Your __reflect_X parameters are probably out of order, see above");
//  }
//#endif

//  // Adjust the helper variables:
//  curdataidx += getNumReqChunks<T>();
//  assert(curdataidx <= remargdata.size()); //sanity check
////    llvm::outs() << curdataidx << "/" << remargdata.size() << "\n";

//  no_more = (curdataidx == remargdata.size());

//  return res;
//}


/////REFERENCE version
//template< typename T
//        , typename std::enable_if<std::is_reference<T>::value, int>::type >
//T Reflector::get_next_as() {
////  llvm::outs() << "[DWR DEBUG] Calling Reference version of get_next_as, which should "
////                  "call non-ref version with pointer then deref the result ...\n";
//  using NoRefT = typename std::remove_reference<T>::type;
//  return *get_next_as<NoRefT *>();
//}



//ExprResult Reflector::operator()() {

////  llvm::outs() << "About to reflect; remargdata.size() before getting first: " << remargdata.size() << "\n";

//  if (!ObjKind)
//    llvm_unreachable("Invalid ReflectionObjKind");

//  if (IsPointer) {
//    intptr_t Val = get_next_as<intptr_t>();

//    if (!Val) {
//      S.Diag(KWLoc, diag::err_null_reflected_ptr_access) << 1;
//      return ExprError();
//    }
//    switch (ObjKind) {
//#       include "reflection_incs/ReflectorCallOperator_PointerCases.inc"

//      default:
//        llvm::outs() << "Unexpected Reflection pointer case: Reflecting from pointer; remargdata.size() = "
//                 << remargdata.size() << "\n"
//                 << "`--TraitKind = " << TraitKind
//                 << ", ObjKind = " << ObjKind
//                 << ", MemNum = " << MemNum << "\n";
//        llvm_unreachable("Unexpected pointer case");
//    }
//  } else {
//    switch (ObjKind) {
//#       include "reflection_incs/ReflectorCallOperator_NonPointerCases.inc"

//      default:
//        llvm_unreachable("Unexpected non-pointer case");
//    }
//  }
//}

//Reflector::Reflector(
//        Sema &S, SourceLocation KWLoc, SourceLocation RParenLoc,
//        ReflectionTraitKind TraitKind,
//        ReflectionObjKind ObjKind, unsigned MemNum, unsigned IsPtr,
//        ArrayRef<intptr_t> remargdata )
//        : S(S), KWLoc(KWLoc), RParenLoc(RParenLoc),
//          TraitKind(TraitKind),
//          ObjKind(ObjKind), MemNum(MemNum), IsPointer(IsPtr),
//          remargdata(remargdata.data(), remargdata.size()), //to make it const array
//          curdataidx(0), no_more(false) {
//  assert(ObjKind && "Expected this to have been set");
//}










/*-----------------------------------------------------------------*/
///  Sema::ActOnReflectionTrait, ActOnCompilerMessageExpr


//static std::pair<unsigned, unsigned> getMemNumRange(ReflectionObjKind RK) {
//  switch (RK) {
//#   include "reflection_incs/ReflKindGetMemNumRangeCases.inc"
//
//    default:
//      llvm_unreachable("Unhandled ReflectionObjKind");
//      return {0, 0}; //needed to avoid error when .inc is empty I guess
//  }
//}





//struct QualTypeOrCTD {
//  union {
//    QualType QT;
//    ClassTemplateDecl *CTD;
//  };
//  bool isQT;
//  QualTypeOrCTD(QualType QT) : QT(QT), isQT(true) {}
//  QualTypeOrCTD(ClassTemplateDecl *CTD) : CTD(CTD), isQT(false) {}
//};


//// TODO set up specializations of this that parallel those in BuildReflection.
//template<typename T>
//QualTypeOrCTD toClientReflType(ASTContext &C) {
//  return QualTypeOrCTD(C.DependentTy); //TEMP, TODO set up specializations for different T's
//}


//QualTypeOrCTD getClientReflQTorCTD(ASTContext &C,
//                                   ReflectionObjKind ObjKind,
//                                   unsigned MemNum,
//                                   bool IsPtr/*needed for reflection of '_this_'*/) {
//  switch (ObjKind) {

//#   include "reflection_incs/ReflKindAndMemNumToQTorCTD.inc"

//  default:
//    llvm_unreachable("Unhandled ReflectionObjKind");
//  }
//}















//ExprResult Sema::ActOnReflectionTraitTyped(SourceLocation KWLoc,
//                                           ReflectionTraitKind TraitKind,
//                                           unsigned ObjKind,
//                                           unsigned MemNum,
//                                           bool IsPtr,
//                                           QualType RetClientReflPrimType,
//                                           ClassTemplateDecl *RetClientReflNonprimCTD,
//                                           ArrayRef<Expr *> RemArgs,
//                                           SourceLocation RParenLoc) {

//  // If any of the remaining args are dependent, build another ReflectionTraitTypedExpr.
//  // Note that we're not just being lazy here; consider:
//  // When we instantiate a client reflection template, the instantiator will
//  // go through each of the member function implems, transforming the __reflect_prop(...) etc expressions
//  // and calling ActOn..., and even though we COULD bind the reflection object data at that
//  // point, we'd be doing it for a bunch of functions that we never intend to call.
//  // Bottom line, if anything is still dependent at this point, it very likely will never be used,
//  // so just get through this loop as efficiently as possible.
//  // (BUT, nullary functions will pass this test, meaning you'll needlessly instantiate many of those...
//  // oh well, we won't fuss too much about this right now.)
//  for (Expr *TheArg : RemArgs) {
//    if (TheArg->isTypeDependent() || TheArg->isValueDependent()) {
//      return new (Context) ReflectionTraitTypedExpr(Context, KWLoc, TraitKind,
//                                                    ObjKind, MemNum, IsPtr,
//                                                    RetClientReflPrimType, RetClientReflNonprimCTD,
//                                                    RemArgs, RParenLoc);
//    }
//  }

//  // If we've reached here, ALL the arguments are now non-dependent, meaning we could evaluate the expression
//  // right now.  However, we won't -- instead, we'll tranform this expression into a VoidReflectionExpr or
//  // a NonvoidReflectionExpr and let the ExprEvaluator/VoidExprEvaluator's Visit... implems handle evaluation
//  // (see ExprConstant.cpp).

//  // We're acting on a NON-dependent reflection trait -- i.e. we're no longer
//  // parsing templates, we've finally been asked to actually reflect something
//  // back to the client!
//  // So, evaluate the remaining integer values from the trait arguments,
//  // and construct and call the Reflector functor to handle the rest.


//  // We split this up into cases because we MUST ensure sequential
//  // temporary storage of the integer-evaluated args (so we can reinterpret_cast
//  // to objects larger than integers at any point in the array), and yet would
//  // like to stack allocate for reasonable numbers of args.
//  // Bottom line a solution like SmallVector will NOT work here, since it might
//  // split up the data chunks, causing possible havoc with our reinterpret_casts.
//# define LOAD_INTDATA_AND_REFLECT(Vals) \
//  if (!LoadIntDataFromArgExprs(*this, KWLoc, RemArgs, Vals)) \
//    return ExprError();\
//  return Reflector(*this, KWLoc, RParenLoc, TraitKind,\
//                   static_cast<ReflectionObjKind>(ObjKind),\
//                   MemNum, IsPtr, Vals)()/*call operator*/;
///**/
//# define REFLECT_REMARGS_LEQ(N) \
//  if (RemArgs.size() <= N) { \
//    intptr_t ValsArr[N];/*stack allocate*/\
//    auto Vals = ArrayRef<intptr_t>(ValsArr, RemArgs.size());/*set up arrayref with exact size*/\
//    LOAD_INTDATA_AND_REFLECT(Vals)\
//  }
///**/
//  REFLECT_REMARGS_LEQ(2)
//  REFLECT_REMARGS_LEQ(4)
//  REFLECT_REMARGS_LEQ(16)
//  REFLECT_REMARGS_LEQ(64)

//# undef REFLECT_REMARGS_LEQ

//  // In the unlikely event you get here, you're passing a whole mess of args,
//  // so just heap allocate with a std::vector.
//  // (Probably a code smell that you require so much data for your reflection
//  // call -- means something very big was reflected by value, or you are
//  // taking big params by value, or just have way more params than expected,
//  // so perhaps DWR TODO add some kind of warning here)
//  std::vector<intptr_t> Vals; //heap allocate
//  Vals.reserve(RemArgs.size());
//  LOAD_INTDATA_AND_REFLECT(Vals)

//# undef LOAD_INTDATA_AND_REFLECT

//}

/*
 WAIT.
 Okay.  Consider:
 You test whether you have a QualType or a CTD.  If you have a CTD, you (presumably) need to get the exact type determined
 before creating a NonvoidReflectionExpr...

 Wait.  Consider this: the ReflectionTraitTypedExpr is constructed with either a QualType OR a function_impl that returns the QualType, given the intptr_t & args beginning.  GOOD.
 Then, there's an additional field: another std::function, this one returning an Expr * and again taking a const intptr_t &, that retrieves the actual result (or in the case of void functions, returns nullptr -- or perhaps just have a union, one returns Expr*, one returns void, and you select the right one based on the getType() == VoidTy).
 Then, we CAN do the Reflector and BuildReflection in ActOnReflectionTrait, but all it will do is set those two functions appropriately and construct a ReflectionTraitTypedExpr.

 Then, all ActOnReflectionTraitTypedExpr does is rebuild itself -- or actually it checks if its still a DependentTy, and if so, it checks if the first arg is non-dependent, and if so, it calls the getTypeFromIntArgs_callback, or whatever we want to call that function, to set the type for the new construction.

 Then, over in ExprEvaluator/VoidExprEvaluator, we have Visit implems, in which we load all int data into an array, then call the callbacks on it.  I.e. no more Reflector.

 Then all we can get rid of the new GenReflectionSrc method with the nested switches, and instead put all the content in what is currently generate_ReflectProp_overload, except that now THAT will contain nested switches, and all the TRY_LOAD_VAL etc. stuff will now go in a lambda def, and we'll be setting the callback function to that lambda.

 Okay, GOOD. Lot to think about here, but I think it's doable.

 TODO
 1. Adjust ReflectionTraitTypedExpr so it has the additional callback functions as fields, plus delete VoidReflectionExpr etc.
 2. Write the code for setting those callbacks at the end of ActOnReflectionTrait.  Should be a switch over the ObjKindEnum, followed by an #include of ReflectionTraitBindings.inc, which will take the place of ReflectPropDefs/ReflectPropDecls.
 3. Write generate_ReflectionTraitBindings.inc, and delete the unneeded stuff (ReflectPropDefs/Decls, plus the new thing.)
 4. Write ActOnReflectionTraitTypedExpr as just checking if its a DependentType and if so checking if the first arg is no longer dependent, and if so changing the type by calling the appropriate callback.
 5. Write the VisitReflectionTraitTypedExpr, in both ExprEvaluator and VoidExprEvaluator, as a) loading the int data from the Args, b) calling the appropriate callback function.  If its nonvoid, you should first create an APValue and then call Evaluate with that and the callback result.

 GOOD, THIS SHOULD WORK WELL and be super efficient.

 */



  //TODO: Until the RemArgs are all non-dependent, just keep rebuilding this.
  // Once they are all non-dependent, set up the Reflector and call its call operator.

  //Then, just need to adjust the current BuildReflection implems so they mimic VOID_REFLECTION: they set up a NonvoidReflectionExpr with the appropriate params.
  //BUT, I think they need to take an additional RET type param, and then the doit() function gets an Expr * via a call to DispatchReflection on the returned value; DispatchReflection is an overloaded function that is basically the remaining portions of BuildReflection.  GOOD.








////DWR OLD:
///// Note: Generally, when we first parse the __reflect_prop statements, we will go through each
///// of the below branches; but then during instantiation, only the IsPtr of these might need
///// to be evaluated and checked (and even that only when we have a reflection
///// class that might possibly be reflected as both a pointer and a value -- fairly rare).
///// Bottom line, the point of all this code here is to do all the basic kind/methodnum/etc
///// argument evalutation/checking done in the initial parse, but avoid evaluating the
///// single-use instance parameters until ALL dependencies are resolved and we're ready to
///// actually reflect something to the user.
//ExprResult Sema::ActOnReflectionTrait(SourceLocation KWLoc,
//                                      ReflectionTraitKind TraitKind, //DWR ADDN
//                                      unsigned/*ReflectionObjKind*/ ObjKind, unsigned MemNum, unsigned IsPtr, //DWR ADDN
//                                      ArrayRef<Expr *> Args,
//                                      SourceLocation RParenLoc) {

////  llvm::outs() << "[DWR DEBUG] In ActOnReflectionTrait, initial Args.size() == " << Args.size() << "\n";

//  assert(ObjKind < MAX_REFLOBJKIND && "Invalid ReflectionObjKind!");
//  // Figure out which true index Args[0] refers to, by determining how many of the first 3
//  // arguments have PREVIOUSLY been evaluated (in a template pattern from which this was
//  // instantiated), given the supplied ObjKind, MemNum, and IsPtr inputs.
//  // Note that Args[0] will refer to the first UNEVALUATED arg; that is why
//  // we have to calculate this right away -- so we know what Args[i] refers to.
//  unsigned Args0TrueIdx = 3;
//  if (IsPtr == ReflectionTraitExpr::IsPtr_Unset) {
//    --Args0TrueIdx;
//    if (MemNum == ReflectionTraitExpr::MemNum_Unset) {
//      --Args0TrueIdx;
//      if (!ObjKind)
//        --Args0TrueIdx;
//    } else
//      assert(ObjKind
//             && "Did not expect MemNum to be evald but not ObjKind too");
//  } else {
////    llvm::outs() << "[DWR DEBUG] IsPtr: " << IsPtr << ", MemNum: " << MemNum << ", ReflKind: " << ObjKind << "\n";
//    assert(ObjKind && MemNum != ReflectionTraitExpr::MemNum_Unset
//           && "Did not expect IsPtr to be evald but not ObjKind and MemNum too");
//  }

//  assert(Args0TrueIdx >= 0 && Args0TrueIdx <= 3);

//  // Try to evaluate [0] ObjKind, [1] MemNum, and [2] IsPtr, if not already evaluated.
//  // As you evaluate each, check its validity.
//  // We'll take them in reverse order.
//  bool isDependent = false;
//  unsigned numArgsEvald = 0;
//  if (IsPtr == ReflectionTraitExpr::IsPtr_Unset) {
//    unsigned idx = 2 - Args0TrueIdx;
//    assert(idx < 3); //sanity check

//    llvm::Optional<intptr_t> res = {};
//    assert(!res); //sanity check

//    if (Args.size() <= idx //so that we can include the isptr status in the param pack
//        || Args[idx]->isTypeDependent() || Args[idx]->isValueDependent())
//      isDependent = true;
//    else {
//      // [2] IsPtr is non-dependent; set it.
//      ++numArgsEvald;
//      res = EvaluateIntReflectionArg(*this, Args[idx]);
//      if (res) {
//        IsPtr = (unsigned) *res;
//        assert(IsPtr < ReflectionTraitExpr::IsPtr_Unset);

//        if (IsPtr > 1) {
//          Diag(Args[idx]->getLocStart(), diag::err_reflection_trait_param_out_of_bounds)
//                  << "2 (the IsPtr arg)" << "0 or 1" << IsPtr;
//          return ExprError();
//        }
//      } else
//        return ExprError();


//      if (MemNum == ReflectionTraitExpr::MemNum_Unset) {
//        --idx;
//        bool MemNumDependent = false;
//        if (Args.size() <= idx //""
//            || Args[idx]->isTypeDependent() || Args[idx]->isValueDependent()) {
//          isDependent = true;
//          MemNumDependent = true;
//        } else {
//          // [1] MemNum is non-dependent; set it.
//          ++numArgsEvald;
//          res = EvaluateIntReflectionArg(*this, Args[idx]);
//          if (res) {
//            MemNum = (unsigned) *res;
//            assert(MemNum < ReflectionTraitExpr::MemNum_Unset);
//            //Can't check its bounds yet, we will once we
//            // are sure we've evaluated the ObjKind below...
//          } else
//            return ExprError();
//        }

//        if (!ObjKind) {
//          --idx;
//          if (Args.size() <= idx //""
//              || Args[idx]->isTypeDependent() || Args[idx]->isValueDependent()) {
//            isDependent = true;
//          } else {
//            // [0] ObjKind is non-dependent; set it.
//            ++numArgsEvald;
//            res = EvaluateIntReflectionArg(*this, Args[idx]);
//            if (res)
//              ObjKind = static_cast<ReflectionObjKind>(*res);
//            else
//              return ExprError();

//            if (!ObjKind || ObjKind > MAX_REFLOBJKIND) {
//              Diag(Args[idx]->getLocStart(), diag::err_reflection_trait_param_out_of_bounds)
//                      << "0 (the ReflectionObjKind arg)"
//                      << ("an unsigned integer less than " + std::to_string(MAX_REFLOBJKIND))
//                      << ObjKind;
//            } //if ObjKind out of range
//          }
//        } //if ObjKind was not already set

//        if (ObjKind/*i.e. if ObjKind is not dependent*/) {
//          // Now that we have the kind we can check the
//          // MemNum if we set that too (we probably did).
//          // NOTE: in the case of RTK_cast, we will interpret
//          // the memnum as the ReflectionObjKind we want to cast to,
//          // so two different interpretations to consider here.
//          if (!MemNumDependent) {
//            if (TraitKind == RTK_cast) {
//              if (MemNum > ReflectionObjKind::MAX_REFLOBJKIND) {
//                Diag(Args[idx]->getLocStart(), diag::err_reflection_trait_param_out_of_bounds)
//                        << ("1 (the Target ReflectionObjKind for the __reflect_cast expression")
//                        << ("an unsigned integer less than " +
//                            std::to_string(ReflectionObjKind::MAX_REFLOBJKIND))
//                        << MemNum;
//              }
//            } else {
//              unsigned totalmemnums = getTotalMemNums((ReflectionObjKind)ObjKind);
//              if (MemNum >= totalmemnums) {
//                Diag(Args[idx]->getLocStart(), diag::err_reflection_trait_param_out_of_bounds)
//                        << ("1 (the MemNum arg for Reflection ObjKind = " + std::to_string((unsigned)ObjKind) + ")") //DWR FIXME set up a to_string for the RK, call that here.
//                        << ("an unsigned integer less than " +
//                            std::to_string(totalmemnums))
//                        << MemNum;
//                return ExprError();
//              } //if MemNum out of range
//            } //if this is a __reflect_prop call rather than __reflect_cast
//          } //if MemNum is not dependent
//        } //if ObjKind is not dependent
//      } //if MemNum was not already set
//    } //if IsPtr is not dependent
//  } //if IsPtr was not already set

//  // Now, remove as many arguments from the front as we have
//  // evaluated here.  (Since there could not have been gaps,
//  // and there were all in the front, this works.)
//  assert(Args.size() > numArgsEvald // should be at least one more arg for the actual reflected data
//         && "Did not properly verify the size during ParseReflectionTrait!");

//  //DWR POSSIBLE FIXME: do we need to do some kind of Lvalue conversion here?  I dunno how that works.
//  //Look into it.
//  auto UnevaldArgs = ArrayRef<Expr *>(Args.data() + numArgsEvald, Args.size() - numArgsEvald);

////  llvm::outs() << "[DWR DEBUG] In ActOnReflectionTrait, after evals; UnevaldArgs.size() == " << UnevaldArgs.size() << ", numArgsEvald = " << numArgsEvald << "\n";

//  // Now, finish checking if ANY of the args are dependent.
//  if (!isDependent) {
//    for (unsigned i = 0; i < UnevaldArgs.size(); ++i) {
//      if (UnevaldArgs[i]->isTypeDependent() || UnevaldArgs[i]->isValueDependent()) {
//        isDependent = true;
//        break;
//      }
//    }
//  }

//  // If any of the args were dependent, the whole trait is dependent; we
//  // won't be returning anything to the user at this time. (E.g.
//  //  after the initial parse of the client reflection header templates.)
//  // [DWR] Note that we won't bother trying to evaluate any non-dependent
//  // ones other than the first three in this case -- we'll only do that
//  // when all the dependencies have been filled in, since the remaining
//  // params don't really generalize, they only apply to single-use
//  // instantiations.
//  // (Note that this applies even when we implement allowing class-based
//  // parameters [i.e. reflection class params] -- in such cases, we'll
//  // only pass the Xs... of the parameter, relying on overload resolution
//  // to ensure the ObjKind and IsPtr status is valid, and then will interpret
//  // the Xs... params in the Reflector just like primitive parameters, if
//  // that makes sense.)
//  if (isDependent) {
//    return new (Context) ReflectionTraitExpr(Context, TraitKind,
//            (unsigned)ObjKind, MemNum, IsPtr, Context.DependentTy,
//            UnevaldArgs,
//            APValue(), KWLoc, RParenLoc
////          , /*SemaPtr=*/this //DWR TEMP
//    );
//  }

//  assert(ObjKind && "Should not be zero if non-dependent");

//  // We're acting on a NON-dependent reflection trait -- i.e. we're no longer
//  // parsing templates, we've finally been asked to actually reflect something
//  // back to the client!
//  // So, evaluate the remaining integer values from the trait arguments,
//  // and construct and call the Reflector functor to handle the rest.


//  // We split this up into cases because we MUST ensure sequential
//  // temporary storage of the integer-evaluated args (so we can reinterpret_cast
//  // to objects larger than integers at any point in the array), and yet would
//  // like to stack allocate for reasonable numbers of args.
//  // Bottom line a solution like SmallVector will NOT work here, since it might
//  // split up the data chunks, causing possible havoc with our reinterpret_casts.
//# define LOAD_INTDATA_AND_REFLECT(Vals) \
//  if (!LoadIntDataFromArgExprs(*this, KWLoc, UnevaldArgs, Vals)) \
//    return ExprError();\
//  return Reflector(*this, KWLoc, RParenLoc, TraitKind,\
//                   static_cast<ReflectionObjKind>(ObjKind),\
//                   MemNum, IsPtr, Vals)()/*call operator*/;
///**/
//# define REFLECT_UARGS_LEQ(N) \
//  if (UnevaldArgs.size() <= N) { \
//    intptr_t ValsArr[N];/*stack allocate*/\
//    auto Vals = ArrayRef<intptr_t>(ValsArr, UnevaldArgs.size());/*set up arrayref with exact size*/\
//    LOAD_INTDATA_AND_REFLECT(Vals)\
//  }
///**/
//  REFLECT_UARGS_LEQ(2)
//  REFLECT_UARGS_LEQ(4)
//  REFLECT_UARGS_LEQ(16)
//  REFLECT_UARGS_LEQ(64)

//# undef REFLECT_UARGS_LEQ

//  // In the unlikely event you get here, you're passing a whole mess of args,
//  // so just heap allocate with a std::vector.
//  // (Probably a code smell that you require so much data for your reflection
//  // call -- means something very big was reflected by value, or you are
//  // taking big params by value, or just have way more params than expected,
//  // so perhaps TODO add an llvm_unreachable here, or at least some kind of
//  //  compile-time warning)
//  std::vector<intptr_t> Vals; //heap allocate
//  Vals.reserve(UnevaldArgs.size());
//  LOAD_INTDATA_AND_REFLECT(Vals)

//# undef LOAD_INTDATA_AND_REFLECT
//}









//ASUTTON ADDN, DWR MOD
/// Handle a call to \c __compiler_debug.
ExprResult Sema::ActOnCompilerMessageExpr(SourceLocation KWLoc,
                                          Expr *MessageExpr,
                                          SourceLocation RParenLoc) {
  if (DiagnoseUnexpandedParameterPack(MessageExpr))
    return ExprError();

  return BuildCompilerMessageExpr(KWLoc, MessageExpr, RParenLoc);
}

/// Build a \c __compiler_debug expression.
ExprResult Sema::BuildCompilerMessageExpr(SourceLocation KWLoc,
                                          Expr *MessageExpr,
                                          SourceLocation RParenLoc) {
  assert(MessageExpr != nullptr);

  ExprResult Converted = DefaultFunctionArrayLvalueConversion(MessageExpr);
  if (Converted.isInvalid())
    return ExprError();

  MessageExpr = Converted.get();

  // FIXME: Make sure that the type is const char* or a compile-time
  // string literal (which doesn't exist).

  return CompilerMessageExpr::Create(Context,
                                     MessageExpr,
                                     KWLoc,
                                     RParenLoc);
}
//END
//DWR ADDN
/// Handle a call to \c __compiler_diag.
ExprResult Sema::ActOnCompilerDiagnosticExpr(SourceLocation KWLoc,
                                             ArrayRef<Expr *> Args,
                                             SourceLocation RParenLoc) {
  //DWR POSSIBLE FIXME: do we need to do a DefaultLvalueConversion or whatever?
  return BuildCompilerDiagnosticExpr(KWLoc, Args, RParenLoc);
}

/// Build a \c __compiler_diag expression.
ExprResult Sema::BuildCompilerDiagnosticExpr(SourceLocation KWLoc,
                                             ArrayRef<Expr *> Args,
                                             SourceLocation RParenLoc) {
  return CompilerDiagnosticExpr::Create(Context,
                                        KWLoc,
                                        Args,
                                        RParenLoc);
}
//END







/// Update the reflection expression with by indicating that it is in fact
/// a reflexpr expression.
ExprResult Sema::ActOnCXXReflexprExpr(Expr *E,
                                      SourceLocation LParenLoc,
                                      SourceLocation RParenLoc) {
  assert(isa<ReflectionExpr>(E));
  ReflectionExpr *RE = cast<ReflectionExpr>(E);
  RE->setParenLocs(LParenLoc, RParenLoc);
  return E;
}



/// Evaluates the given expression and yields the computed type.
TypeResult Sema::ActOnTypeReflectionSpecifier(SourceLocation TypenameLoc,
                                              Expr *E) {
  QualType T = BuildReflectedType(*this, TypenameLoc, E);
  if (T.isNull())
    return TypeResult(true);

  // FIXME: Add parens?
  TypeLocBuilder TLB;
  ReflectedTypeLoc TL = TLB.push<ReflectedTypeLoc>(T);
  TL.setNameLoc(TypenameLoc);
  TypeSourceInfo *TSI = TLB.getTypeSourceInfo(Context, T);
  return CreateParsedType(T, TSI);
}



/*-----------------------------------------------------------------*/
///  Sema Metaclass implems (DWR commented out)

///// Buid a new metaclass definition.
//Decl *Sema::ActOnMetaclass(Scope *S, SourceLocation DLoc, SourceLocation IdLoc,
//                           IdentifierInfo *II) {
//  assert(II);
//
//  bool IsInvalid = false;
//
//  // Make sure that this definition doesn't conflict with existing tag
//  // definitions.
//  LookupResult Previous(*this, II, IdLoc, LookupOrdinaryName, ForRedeclaration);
//  LookupName(Previous, S);
//  if (!Previous.empty()) {
//    NamedDecl *PrevDecl = Previous.getRepresentativeDecl();
//    MetaclassDecl *PrevMD = dyn_cast<MetaclassDecl>(PrevDecl);
//    if (PrevMD) {
//      Diag(IdLoc, diag::err_redefinition) << II;
//      Diag(PrevMD->getLocation(), diag::note_previous_definition);
//    } else {
//      Diag(IdLoc, diag::err_redefinition_different_kind) << II;
//      Diag(PrevDecl->getLocation(), diag::note_previous_definition);
//    }
//    IsInvalid = true;
//  }
//
//  MetaclassDecl *Metaclass =
//      MetaclassDecl::Create(Context, CurContext, DLoc, IdLoc, II);
//
//  if (IsInvalid)
//    Metaclass->setInvalidDecl();
//
//  PushOnScopeChains(Metaclass, S);
//  return Metaclass;
//}
//
//void Sema::ActOnMetaclassStartDefinition(Scope *S, Decl *MD,
//                                         ParsedAttributes &Attrs,
//                                         CXXRecordDecl *&Definition,
//                                         unsigned Depth) {
//  MetaclassDecl *Metaclass = cast<MetaclassDecl>(MD);
//
//  PushDeclContext(S, Metaclass);
//  ActOnDocumentableDecl(Metaclass);
//
//  TagTypeKind Kind = TypeWithKeyword::getTagTypeKindForTypeSpec(TST_metaclass);
//
//  // Create a nested class to store the metaclass member declarations.
//  Definition = CXXRecordDecl::Create(
//      Context, Kind, CurContext, Metaclass->getLocStart(),
//      Metaclass->getLocation(), Metaclass->getIdentifier());
//  Definition->setImplicit(true);
//  Definition->setFragment(true);
//  CurContext->addHiddenDecl(Definition);
//
//  StartDefinition(Definition);
//
//  assert(Definition->isMetaclassDefinition() && "Broken metaclass definition");
//
//  // Build an implicit template parameter 'prototype'. This is essentially
//  // a reserved identifier within the scope of the metaclass.
//  auto *Proto = TemplateTypeParmDecl::Create(Context, Definition,
//                                             SourceLocation(), SourceLocation(),
//                                             Depth, 0,
//                                             &Context.Idents.get("prototype"),
//                                             /*Typename=*/true,
//                                             /*ParameterPack=*/false);
//  Proto->setImplicit(true);
//  Definition->addDecl(Proto);
//  PushOnScopeChains(Proto, S, false);
//
//  // Apply attributes to the definition.
//  if (AttributeList *List = Attrs.getList())
//    ProcessDeclAttributeList(CurScope, Definition, List, true);
//
//  Metaclass->setDefinition(Definition);
//}
//
//void Sema::ActOnMetaclassFinishDefinition(Scope *S, Decl *MD,
//                                          SourceRange BraceRange) {
//  MetaclassDecl *Metaclass = cast<MetaclassDecl>(MD);
//  Metaclass->setBraceRange(BraceRange);
//
//  PopDeclContext();
//}
//
//void Sema::ActOnMetaclassDefinitionError(Scope *S, Decl *MD) {
//  MetaclassDecl *Metaclass = cast<MetaclassDecl>(MD);
//  Metaclass->setInvalidDecl();
//
//  PopDeclContext();
//}
//
//namespace {
//
//class MetaclassNameValidatorCCC : public CorrectionCandidateCallback {
//public:
//  explicit MetaclassNameValidatorCCC(bool AllowInvalid)
//      : AllowInvalidDecl(AllowInvalid) {
//    WantExpressionKeywords = false;
//    WantCXXNamedCasts = false;
//    WantRemainingKeywords = false;
//  }
//
//  bool ValidateCandidate(const TypoCorrection &candidate) override {
//    if (NamedDecl *ND = candidate.getCorrectionDecl()) {
//      bool IsMetaclass = isa<MetaclassDecl>(ND);
//      return IsMetaclass && (AllowInvalidDecl || !ND->isInvalidDecl());
//    }
//    return false;
//  }
//
//private:
//  bool AllowInvalidDecl;
//};
//
//} // end anonymous namespace
//
///// Determine whether the given identifier is the name of a C++ metaclass.
/////
///// \param S                  The scope from which unqualified metaclass name
/////                           lookup will begin.
///// \param SS                 If non-null, the C++ scope specifier that
/////                           qualifies the name \p Name.
///// \param Name               The identifier.
///// \param NameLoc            The source location of the identifier \p Name.
///// \param [in,out] Metaclass If non-null and this function returns \c true,
/////                           will contain the metaclass declaration found by
/////                           lookup.
///// \returns                  \c true if a metaclass declaration with the
/////                           specified name is found, \c false otherwise.
//bool Sema::isMetaclassName(Scope *S, CXXScopeSpec *SS,
//                           const IdentifierInfo &Name, SourceLocation NameLoc,
//                           Decl **Metaclass) {
//  // FIXME: What kind of lookup should be performed for metaclass names?
//  LookupResult R(*this, &Name, NameLoc, LookupOrdinaryName);
//  // TODO: Check for metaclass template specializations.
//  LookupParsedName(R, S, SS);
//
//  if (R.isAmbiguous()) {
//    // FIXME: Diagnose an ambiguity if we find at least one declaration.
//    R.suppressDiagnostics();
//    return false;
//  }
//
//  MetaclassDecl *MD = R.getAsSingle<MetaclassDecl>();
//
//  if (!MD)
//    return false;
//  if (Metaclass)
//    *Metaclass = MD;
//  return true;
//}
//
///// \brief If the identifier refers to a metaclass name within this scope,
///// return the declaration of that type.
/////
///// This routine performs ordinary name lookup of the identifier \p II
///// within the given scope, with optional C++ scope specifier \p SS, to
///// determine whether the name refers to a metaclass. If so, returns an
///// opaque pointer (actually a QualType) corresponding to that
///// type. Otherwise, returns \c NULL.
/////
///// \note This function extracts the type from a MetaclassDecl's underlying
/////       CXXRecordDecl representation and is used to provide a ParsedType
/////       object to Parser::ParseMetaclassBaseSpecifier() when parsing metaclass
/////       base specifiers. Should MetaclassDecl ever become a subclass of
/////       RecordDecl or CXXRecordDecl, this function will hopefully no longer be
/////       necessary.
/////
///// \see  Sema::getTypeName()
//ParsedType Sema::getMetaclassName(const IdentifierInfo &II,
//                                  SourceLocation NameLoc, Scope *S,
//                                  CXXScopeSpec *SS,
//                                  bool WantNontrivialTypeSourceInfo,
//                                  IdentifierInfo **CorrectedII) {
//  // Determine where we will perform name lookup.
//  DeclContext *LookupCtx = nullptr;
//  if (SS && SS->isNotEmpty()) {
//    LookupCtx = computeDeclContext(*SS, false);
//
//    if (!LookupCtx) {
//      if (isDependentScopeSpecifier(*SS)) {
//        // FIXME: Update this section if metaclasses are ever allowed to be
//        // members of a dependent context.
//
//        // C++ [temp.res]p3:
//        //   A qualified-id that refers to a type and in which the
//        //   nested-name-specifier depends on a template-parameter (14.6.2)
//        //   shall be prefixed by the keyword typename to indicate that the
//        //   qualified-id denotes a type, forming an
//        //   elaborated-type-specifier (7.1.5.3).
//        //
//        // We therefore do not perform any name lookup if the result would
//        // refer to a member of an unknown specialization.
//
//        // We know from the grammar that this name refers to a type,
//        // so build a dependent node to describe the type.
//        if (WantNontrivialTypeSourceInfo)
//          return ActOnTypenameType(S, SourceLocation(), *SS, II, NameLoc).get();
//
//        NestedNameSpecifierLoc QualifierLoc = SS->getWithLocInContext(Context);
//        QualType T = CheckTypenameType(ETK_None, SourceLocation(), QualifierLoc,
//                                       II, NameLoc);
//        return ParsedType::make(T);
//      }
//
//      return nullptr;
//    }
//
//    if (!LookupCtx->isDependentContext() &&
//        RequireCompleteDeclContext(*SS, LookupCtx))
//      return nullptr;
//  }
//
//  LookupResult Result(*this, &II, NameLoc, LookupOrdinaryName);
//  if (LookupCtx) {
//    // Perform "qualified" name lookup into the declaration context we
//    // computed, which is either the type of the base of a member access
//    // expression or the declaration context associated with a prior
//    // nested-name-specifier.
//    LookupQualifiedName(Result, LookupCtx);
//  } else {
//    // Perform unqualified name lookup.
//    LookupName(Result, S);
//  }
//
//  NamedDecl *IIDecl = nullptr;
//  switch (Result.getResultKind()) {
//  case LookupResult::NotFound:
//  case LookupResult::NotFoundInCurrentInstantiation:
//    if (CorrectedII) {
//      TypoCorrection Correction =
//          CorrectTypo(Result.getLookupNameInfo(), LookupOrdinaryName, S, SS,
//                      llvm::make_unique<MetaclassNameValidatorCCC>(true),
//                      CTK_ErrorRecovery);
//      IdentifierInfo *NewII = Correction.getCorrectionAsIdentifierInfo();
//      NestedNameSpecifier *NNS = Correction.getCorrectionSpecifier();
//      CXXScopeSpec NewSS, *NewSSPtr = SS;
//      if (SS && NNS) {
//        NewSS.MakeTrivial(Context, NNS, SourceRange(NameLoc));
//        NewSSPtr = &NewSS;
//      }
//      if (Correction && (NNS || NewII != &II)) {
//        ParsedType Ty = getMetaclassName(*NewII, NameLoc, S, NewSSPtr,
//                                         WantNontrivialTypeSourceInfo);
//        if (Ty) {
//          diagnoseTypo(Correction,
//                       PDiag(diag::err_unknown_type_or_class_name_suggest)
//                           << Result.getLookupName() << /*class*/ 1);
//          if (SS && NNS)
//            SS->MakeTrivial(Context, NNS, SourceRange(NameLoc));
//          *CorrectedII = NewII;
//          return Ty;
//        }
//      }
//    }
//  // If typo correction failed or was not performed, fall through
//  case LookupResult::FoundOverloaded:
//  case LookupResult::FoundUnresolvedValue:
//    Result.suppressDiagnostics();
//    return nullptr;
//
//  case LookupResult::Ambiguous:
//    // Recover from metaclass-hiding ambiguities by hiding the metaclass.  We'll
//    // do the lookup again when looking for an object, and we can
//    // diagnose the error then.  If we don't do this, then the error
//    // about hiding the metaclass will be immediately followed by an error
//    // that only makes sense if the identifier was treated like a metaclass.
//    // FIXME: Should we really suppress diagnostics here?
//    if (Result.getAmbiguityKind() == LookupResult::AmbiguousTagHiding) {
//      Result.suppressDiagnostics();
//      return nullptr;
//    }
//
//    // Look to see if we have a metaclass anywhere in the list of results.
//    for (LookupResult::iterator Res = Result.begin(), ResEnd = Result.end();
//         Res != ResEnd; ++Res) {
//      if (isa<MetaclassDecl>(*Res)) {
//        if (!IIDecl ||
//            (*Res)->getLocation().getRawEncoding() <
//                IIDecl->getLocation().getRawEncoding())
//          IIDecl = *Res;
//      }
//    }
//
//    if (!IIDecl) {
//      // None of the entities we found is a metaclass, so there is no way
//      // to even assume that the result is a metaclass. In this case, don't
//      // complain about the ambiguity. The parser will either try to
//      // perform this lookup again (e.g., as an object name), which
//      // will produce the ambiguity, or will complain that it expected
//      // a metaclass name.
//      Result.suppressDiagnostics();
//      return nullptr;
//    }
//
//    // We found a metaclass within the ambiguous lookup; diagnose the
//    // ambiguity and then return that metaclass. This might be the right
//    // answer, or it might not be, but it suppresses any attempt to
//    // perform the name lookup again.
//    break;
//
//  case LookupResult::Found:
//    IIDecl = Result.getFoundDecl();
//    break;
//  }
//
//  assert(IIDecl && "Didn't find decl");
//
//  QualType T;
//  if (MetaclassDecl *MD = dyn_cast<MetaclassDecl>(IIDecl)) {
//    // Get the underlying class that contains the metaclass' definition.
//    TypeDecl *TD = MD->getDefinition();
//
//    DiagnoseUseOfDecl(IIDecl, NameLoc);
//
//    T = Context.getTypeDeclType(TD);
//    // MarkAnyDeclReferenced(TD->getLocation(), TD, /*OdrUse=*/false);
//  }
//
//  if (T.isNull()) {
//    // If it's not plausibly a type, suppress diagnostics.
//    Result.suppressDiagnostics();
//    return nullptr;
//  }
//
//  // FIXME: Is this necessary?
//  if (SS && SS->isNotEmpty()) {
//    if (WantNontrivialTypeSourceInfo) {
//      // Construct a type with type-source information.
//      TypeLocBuilder Builder;
//      Builder.pushTypeSpec(T).setNameLoc(NameLoc);
//
//      T = getElaboratedType(ETK_None, *SS, T);
//      ElaboratedTypeLoc ElabTL = Builder.push<ElaboratedTypeLoc>(T);
//      ElabTL.setElaboratedKeywordLoc(SourceLocation());
//      ElabTL.setQualifierLoc(SS->getWithLocInContext(Context));
//      return CreateParsedType(T, Builder.getTypeSourceInfo(Context, T));
//    } else {
//      T = getElaboratedType(ETK_None, *SS, T);
//    }
//  }
//
//  return ParsedType::make(T);
//}
////END







