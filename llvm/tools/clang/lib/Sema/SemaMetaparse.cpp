//DWR ADDN
//===--- SemaExpand.cpp - Semantic Analysis for Expand statements -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic rules for the __queue_metaparse statements
//
//===----------------------------------------------------------------------===//

//DWR FIXME not sure which are really needed
#include "TreeTransform.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/ExprCXX.h"
#include "clang/Lex/MetaparseDebug.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/CDContextVars.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Template.h"
#include "clang/Sema/SemaInternal.h"

using namespace clang;
using namespace sema;




/// Returns an queue_metaparse statement.
StmtResult Sema::ActOnCXXQueueMetaparseStmt(SourceLocation Loc, Expr *NewSrcCode) {
  return BuildCXXQueueMetaparseStmt(Loc, NewSrcCode);
}


/// TODO DESCRIPTION
StmtResult Sema::BuildCXXQueueMetaparseStmt(SourceLocation Loc, Expr *NewSrcCode) {
  QualType StrTy = NewSrcCode->getType();

  // When possible (i.e. when not dependent on template params), check that the operand must be a string.
  //(This isn't doing anything meaningful right now...)
  if (!NewSrcCode->isTypeDependent() && !NewSrcCode->isValueDependent()) {
    // If here, it means we're not inside a template, and thus can do type checking.
    if (!isStrLitType(NewSrcCode->getType())) {
      Diag(NewSrcCode->getExprLoc(), diag::err_expected) << "a string literal";
      IF_METAPARSE_DEBUG( NewSrcCode->dump(); )
      return StmtError();
    }
  }

  // [DWR TODO: try commenting the below out / adding an assert about it not being a GLValue, and commenting in
  // the DefaultFunctionArray conversion code in Transform...]
  // Perform an lvalue-to-value conversion so that we get an rvalue in
  // evaluation.
  if (NewSrcCode->isGLValue()) {
    UpdateMarkingForLValueToRValue(NewSrcCode);
    //^ DWR: added this to try to solve assert(MaybeODRUseExpr.empty()) issue that arises when using __queue_metaparse in constexpr functions;
    // I think it's working but not 100% sure.
    NewSrcCode = ImplicitCastExpr::Create(Context, StrTy,
                                          CK_LValueToRValue, NewSrcCode,
                                          nullptr, VK_RValue);
  }

  return new(Context) CXXQueueMetaparseStmt(Loc, NewSrcCode);
}

/*
 DWR TODO: get rid of the QualType T param in all this stuff, it's never used.
 OR, actually, if there was some way to infer the type right at the beginning on the first
 parse, that would be good, but I don't think that's possible.
 MAYBE you could pass an Expr * that will HOLD the type, and you'd transform that as you go,
 and then hopefully it would eventually have a good type,
 */

/// Returns an in-place metaparse expression.
ExprResult Sema::ActOnCXXMetaparseExpr(SourceLocation Loc, QualType T, Expr *NewSrcCode, Parser &P) {
  return BuildCXXMetaparseExpr(Loc, T, NewSrcCode, P);
}

ExprResult Sema::BuildCXXMetaparseExpr(SourceLocation Loc, QualType T, Expr *NewSrcCode, Parser &P) {
  QualType StrTy = NewSrcCode->getType();

  if (!NewSrcCode->isTypeDependent() && !NewSrcCode->isValueDependent()) {
    assert(!NewSrcCode->isInstantiationDependent() && "DWR guess you need to check this too");
    assert(!NewSrcCode->containsUnexpandedParameterPack() && "DWR guess you need to check this too");
    // If here, it means we're not inside a template, and thus can do type checking.
    if (!isStrLitType(NewSrcCode->getType())) {
      Diag(NewSrcCode->getExprLoc(), diag::err_expected) << "a string literal";
      IF_METAPARSE_DEBUG( NewSrcCode->dump(); )
      return ExprError();
    }



//    const StringLiteral *StrLitResult;

//    //DWR NOTE using Expr::EvaluateAsString, not as good as the version that takes an EvalInfo &Info,
//    // substitutions may not be performed.  But that's all we have available here.
//    if (!NewSrcCode->EvaluateAsString(StrLitResult, Context)) {
//      Diag(NewSrcCode->getExprLoc(), diag::err_metaparse_expr_eval_failed);
//      return ExprError();
//    }
//    assert(StrLitResult);

//    StringRef exprstr = StrLitResult->getString();

//    assert(*exprstr.end() == '\0' &&
//           "Expected every string literal to be null-terminated");


////    // We need to add a semicolon.  Best to just copy the string and adjust it,
////    // it should be pretty small:
////    std::string withsemi = exprstr;
//////    withsemi.pop_back();//pop the null terminator;
//////    assert(exprstr.back() != '\0');
//////    withsemi.push_back(';');
//////    withsemi.push_back('\0');

////    exprstr = withsemi;
////    assert(exprstr.back() == '\0');


//    { //scope for METAPARSE_DEBUG
//      METAPARSE_DEBUG_HDR("Metaparsing expression in place");
//      METAPARSE_DEBUG_VAR(exprstr)
//      assert(StrLitResult->getBeginLoc().isValid() &&
//             "Didn't expect loc to be invalid");
//      assert(TheParser && "Should have called setParser(...) on the Sema object after constructing the parser; "
//                          "we need it for processing generated source strings.");
//      ParserBrickWallRAII SavedParserState(*TheParser);
//      /*
//       DWR NOTE: at this point, IF the string literal is successfully evaluated,
//       you still are going to need to enter whatever instantiation scope is needed
//       for proper lookup (if this is in an instantiation), I think, but we'll cross
//       that bridge if we get to it.
//       */

//      PP.InsertGeneratedSrcStr(exprstr, StrLitResult->getBeginLoc());

//      TheParser->ConsumeToken(); //load up the first token of the inserted string

//      ExprResult res = TheParser->ParseExpression(); //parse it

//      if (!PP.NoMoreMetaparseStrsAvailable()) {
//        Diag(NewSrcCode->getExprLoc(), diag::err_metaparse_str_not_single_expr);
//        return ExprError();
//      }
//      //TODO perhaps check if res.isInvalid(), and if so, add a note that it was metaparsed?  I dunno.
//      return res;
//    }


  } //if non-dependent src code expression

  // If here, NewSrcCode is still dependent...

  // [DWR TODO: try commenting the below out / adding an assert about it not being a GLValue, and commenting in
  // the DefaultFunctionArray conversion code in Transform...]
  // Perform an lvalue-to-value conversion so that we get an rvalue in
  // evaluation.
  if (NewSrcCode->isGLValue()) {
    UpdateMarkingForLValueToRValue(NewSrcCode);
    //^ DWR: added this to try to solve assert(MaybeODRUseExpr.empty()) issue that arises when using __queue_metaparse in constexpr functions;
    // I think it's working but not 100% sure.
    NewSrcCode = ImplicitCastExpr::Create(Context, StrTy,
                                          CK_LValueToRValue, NewSrcCode,
                                          nullptr, VK_RValue);
  }

  return new(Context) CXXMetaparseExpr(Loc, T, NewSrcCode, P);
}





void Sema::TheParserEnterScope(unsigned int ScopeFlags) {
  TheParser->EnterScope(ScopeFlags);
}
void Sema::TheParserExitScope() {
  TheParser->ExitScope();
}


//Helper fcn, since these are different enums for some reason (TODO merge them)
template<typename T>
DeclSpec::TST convertTTKtoTST(T t) {
  DeclSpec::TST TagType;
  switch(t) {
    case TTK_Class:
      TagType = TST_class;
      break;
    case TTK_Enum:
      TagType = TST_enum;
      break;
    case TTK_Struct:
      TagType = TST_struct;
      break;
    case TTK_Interface:
      TagType = TST_interface;
      break;
    case TTK_Union:
      TagType = TST_union;
      break;
  }
  return TagType;
}
//END

/// Parse a queue of source code strings accumulated during
/// eevaluation of a ConstexprDecl.
///
/// \returns  true if no errors are encountered, false otherwise.
bool Sema::DoQueuedMetaparsing(SourceLocation POI,
//                               SmallVectorImpl<EvalEffect> &Effects, //ASUTTON ORIG (for DoInjections or whatever it was)
                               SmallVectorImpl<const StringLiteral *> &MetaCodeChunks, //DWR REPLACEMENT
                               ConstexprDecl *CD) {

  assert (!MetaCodeChunks.empty() &&
          "Should have checked if there were any MetaCodeChunks before calling DoQueuedMetaparsing");

  assert(TheParser && "Should have called setParser(...) on the Sema object after constructing the parser; "
                      "we need it for processing generated source strings.");

  assert(CD);
  CDContextVars& cdcv = CD->getcdcv();

  METAPARSE_DEBUG_HDR("In DoQueuedMetaparsing...")
  bool Ok = true;

  //First, construct std::strings and their locs for each CXXQueueMetaparseStmt in the constexpr block,
  //pushing them onto MetaSrcStrVec.

//  SmallVector<SourceLocation, 16> expandsrclocvec;
//  for (EvalEffect &Effect : Effects) {
////    if (Effect.Kind == EvalEffect::ExpandEffect) {//DWR ADDN
//      expandsrclocvec.push_back(POI);
//      Ok &= ApplyExpand(POI, Effect.SrcCodeToExpand); //DWR ADDN
////    }
////    else if (Effect.Kind == EvalEffect::InjectionEffect)
////      Ok &= ApplyInjection(POI, *Effect.Injection);
////    else
////      Ok &= ApplyDiagnostic(*this, POI, *Effect.DiagnosticArg);
//  }
//  assert(PP.MetaSrcStrVec.size() >= expandsrclocvec.size());

//  if (!Ok || expandsrclocvec.empty()) {
//    METAPARSE_DEBUG("Returning without parsing anything: "
//              << (expandsrclocvec.empty() ? "Empty constexpr block"
//                                          : "Error while generating source code"))
//    return Ok;
//  }



  // We push the evaluated StringLiterals onto the lexer stack in REVERSE order, since it will process
  // last in, first out.
  // But first (i.e. last), we'll push an r_brace token -- this will signal to the parser when it is
  // finished parsing the new source code.

  assert(MetaCodeChunks.size() >= 1);

  // DWR TODO: move this dummy rbrace StringLiteral creation (& null termination) to Sema constructor, so you have a fixed dummy rbrace and don't have to allocate each time.
  static const std::string finaldummystr = "}";
  QualType StrTy = Context.getConstantArrayType(Context.CharTy.withConst(),
                                                llvm::APInt(32, finaldummystr.size() + 1),
                                                ArrayType::Normal, 0);
  SourceLocation rbraceloc = CD->getLocEnd();
  assert(rbraceloc.isValid() && "The loc assigned to the rbrace "
                                "has to be valid, since we test for invalid "
                                "locs to identify terminators ");
  MetaCodeChunks.emplace_back(StringLiteral::Create(Context, finaldummystr, StringLiteral::Ascii, false, StrTy, rbraceloc));

  // For preprocessor diagnostics (e.g. unterminated metaparse expr):
  PP.setCurMetaparseLoc(rbraceloc);

//  *const_cast<char *>(MetaCodeChunks.back()->getString().end()) = '\0'; // Null terminate (DANGEROUS!)

  { //scope, for METAPARSE_DEBUG_HDR
    METAPARSE_DEBUG_HDR("Strings pushed for lexing (in REVERSE order -- first in, last out):")
    for (auto strlitit = MetaCodeChunks.rbegin(); strlitit != MetaCodeChunks.rend(); ++strlitit) {
      const StringLiteral *strlit = *strlitit;
      assert(strlit);
//      StringRef Str = strlit->getString();

//      // Annoyingly, some of the string literals we want to lex are not null terminated.
//      // The Lexer requires a null terminator on any buffer its lexing.
//      // The only reliable solution (I've tried MANY possibilities) is to copy them to a std::string
//      // which is properly null terminated.  (For some reason only std::string works
//      // without creating errors regarding the buffer location,
//      // so gotta go with that.)
//      // I'm farily certain these need to persist throughout the program -- you get weird errors
//      // occasionally otherwise; hence the heap allocation via the ASTContext allocator (which
//      // handles deallocation on its own, conveniently).
//      // Would be so much more efficient if all the strings were properly terminated in the
//      // first place, but that is a battle for later.  [DWR]
//      if (*Str.end() != '\0') {
//        METAPARSE_DEBUG("Ugh, string not null terminated...")
//        Str = *new (Context) std::string(Str);
//        assert(*Str.end() == '\0' && "STILL not null terminated?!");
//      }

//      //DWR TEST: still getting occasional weird crashes, so commented above out and instead will ALWAYS reallocate string.
//      // The idea is that perhaps the errors are because string literals are not persistent, whereas
//      // parsed text needs to be.  If that is the case, then DWR FIXME figure out the most efficient way
//      // to implement this -- most likely, allocate big blocks of memory and memcpy in the bytes AND the null
//      // terminators, to avoid all these "new" statements.
//      // If you STILL get weird errors, get rid of the MetaSrcStrLexer cache stuff you introduced as well.
//      // (I think I tried just doing that and was still getting weird errors, so I hope the below solves the issue on its own.)

      StringRef Str = strlit->getString();

//      //DWR HACK V1:
//      StringRef Str = *new (Context) std::string(strlit->getString());
//        //^ Works okay, maybe fewer errors (maybe not though, hard to say).

//      //DWR HACK V2:
//      PP.MetaSrcStrVec.push_back(new std::string(strlit->getString()));
//      assert(*PP.MetaSrcStrVec.back()->end() == '\0' &&
//             "Expected std::string to be null terminated");
//      StringRef Str = *PP.MetaSrcStrVec.back();


//      //DWR TEMP DEBUG
//      if (!strlit->getBeginLoc().isValid()) {
//        llvm::outs() << "DWR TEMP DEBUG Invalid strlit loc, dump: ";
//        strlit->dump();
//      }
//      //END
      //DWR TEMP HACK COMMENTED OUT:
//      assert(strlit->getBeginLoc().isValid() && "Didn't expect loc to be invalid");

      assert(Str.data());
      assert(*Str.end() == '\0' && "Expected each string literal to be null terminated");
      assert(strlit->getBeginLoc().isValid() || Str == "}"); //DWR HACK replacement (DWR do we still need this?)

      METAPARSE_DEBUG_VAR(Str)
      PP.InsertGeneratedSrcStr(Str, strlit->getBeginLoc());
    }
  }

//  { //scope
//    auto srclocit = expandsrclocvec.rbegin();
//    auto strit = PP.MetaSrcStrVec.rbegin();

//    //Add extra r-brace to last one; we'll use that brace to determine when we're done.
//    // This is pretty hacky but is perhaps the best way to be sure we're adding the rbrace to the very last one.
//    // (Perhaps better to just set up a new string?)
//    //  --Actually would better to somehow EnterToken(tok::r_brace) or whatever.
//    //  But let's delay solving this until we figure a more efficient way to deal with the strings
//    assert((*strit)->back() == '\0');
//    auto nonconstfinalstr = const_cast<std::string *>(*strit);
//    nonconstfinalstr->pop_back();
//    nonconstfinalstr->push_back('}');
//    nonconstfinalstr->push_back('\0');

//    METAPARSE_DEBUG_HDR("Strings pushed for lexing (in reverse order -- first in, last out; last has extra }):")
//    for (; srclocit != expandsrclocvec.rend(); ++srclocit, ++strit) {
//      METAPARSE_DEBUG((**strit).c_str())
//      PP.InsertGeneratedSrcStr(**strit, *srclocit);
//    }

//  }
  assert(TheParser);
  assert(TheParser->Tok.getLocation().isInvalid()
         && "Expected an invalid dummy token before we begin parsing "
            "(to signal the ParserBrickWallRAII has been properly created)");
  //^ DWR: Note this does NOT refer to the dummy RBrace tok we entered into the Preprocessor
  // above; the Parser maintains a current Tok distinct from the Preprocessor, and this should
  // also be invalid at this point.

  // To make sure context gets restored at end,
  // since we might have to adjust it below to handle nested constexpr decls
  // (so we don't get errors)
  Sema::ContextRAII(*this, CurContext);
  assert(CurContext);

  if (cdcv.kind == CDContextVars::CDCV_fcn) {
    assert(CurContext->isFunctionOrMethod());

    METAPARSE_DEBUG_HDR("Parsing __queue_metaparsed function statements/declarations; initial toks:")
    //ParseCompoundStatementBody expects the current tok to be an l_brace, so we make it so:
    Token initlbracetok;
    initlbracetok.startToken();
    initlbracetok.setKind(tok::l_brace);
//    METAPARSE_DEBUG("Token we're replacing (better be meaningless:) ")
//    METAPARSE_DEBUG_VAR(TheParser->Tok.getKind());
    TheParser->Tok = initlbracetok;

    // ParseCompoundStatementBody should consume all the contents of the constexpr decl, unless it encountered an
    // extraneous r_brace, in which case we'll get an error on clearAnyDeadLexers.
    // Note that ParseCompoundStatementBody takes a "bool isStmtExpr = false" param, but after looking at
    // the code that seems to only be true when processing a statement in parentheses, which we needn't worry
    // about for the constexpr decl, so we can safely leave it to the default false (rather than passing it via
    // CDContextVars).
    StmtResult sr(TheParser->ParseCompoundStatementBody());

    if (sr.isInvalid()) {
      METAPARSE_DEBUG("ERROR: Result of ParseCompoundStatementBody was invalid")
      Ok = false;
    }
    CD->setStmtResult(sr); //This will allow it to be processed in ActOnDeclStmt

  }
  else { //kind is cls or extdecl:

    assert(!CurContext->isFunctionOrMethod() && "Either CDContextVars wasn't constructed correctly, or this is a nested case and you perhaps need to temporarily set CurContext to teh parent context here");


    //For external declarations and class member declarations, we'll be handling the loop ourselves, so we consume
    //the initial token to get straight to the declarations (i.e. no need to set up an l_brace tok like above).
    TheParser->ConsumeToken();

    Parser::DeclGroupPtrTy ADecl;
    bool isclass = cdcv.kind == CDContextVars::CDCV_cls;
    assert(isclass || cdcv.kind == CDContextVars::CDCV_extdecl);
    assert(isclass == CurContext->isRecord()
           && "CDContextVars seems to have been incorrectly constructed");
    CXXRecordDecl *InstantiationOrClass = isclass ? cast<CXXRecordDecl>(CurContext) : nullptr;

    METAPARSE_DEBUG_HDR("Parsing " << (isclass ? "Class members" : "ext. decls") << "; initial toks for each:")

    DeclSpec::TST TagType;
    if (InstantiationOrClass)
      TagType = convertTTKtoTST(InstantiationOrClass->getTagKind());

    while (TheParser->Tok.isNot(tok::r_brace)) {
      METAPARSE_DEBUG("First tok of current decl: " << tokIDstr(TheParser->getCurToken()).c_str())
      if (PP.NoMoreMetaparseStrsAvailable() && Ok) {
        TheParser->Diag(CD->getEndLoc(),
                        diag::err_too_many_lbraces_expanded_in_cedecl); //DWR FIXME: error message should be similar to err_extraneous_closing_brace
        PP.setNoMoreMetaparseStrsAvailable(false);
        if (InstantiationOrClass)
          InstantiationOrClass->setInvalidDecl();
        return false;
      }
      assert(TheParser->getCurToken().getLocation().isValid());
      if (InstantiationOrClass) {
        ADecl = TheParser->ParseCXXClassMemberDeclarationWithPragmas(cdcv.cls.AS, cdcv.cls.AccessAttrs, TagType,
                                                                     InstantiationOrClass);
          //^ DWR POSSIBLE FIXME: WithPragmas? or just ParseCXXClassMemberDeclaration?
      } else { //ext decl:
        //DWR FIXME: remove attrs from CDContextVars, since you're just locally constructing it here
        ParsedAttributesWithRange attrs(TheParser->AttrFactory);
        TheParser->MaybeParseCXX11Attributes(attrs);
        ADecl = TheParser->ParseExternalDeclaration(attrs);

        //DWR FIXME: if the below isn't needed, delete it; if it is needed, uncomment
//        if (ADecl && !getASTConsumer().HandleTopLevelDecl(ADecl.get())) {
//          //DWR TODO: need to test
//          METAPARSE_DEBUG("ERROR: Top level decl was non null, but HandleTopLevelDecl returned 0!")
//          return false;
//        }
      }
      if (ADecl) {
        assert(ADecl.get().isSingleDecl()
               && "Didn't expect ADecl to not be a SingleDecl -- adjust code");
        Decl *SingleDecl = ADecl.get().getSingleDecl();
        assert(SingleDecl);
        if (SingleDecl->isInvalidDecl()) {
          Ok = false;
          if (InstantiationOrClass)
            InstantiationOrClass->setInvalidDecl();
        }
      } else {
        METAPARSE_DEBUG("ADecl was nullptr")
      }

      // When you have multiple statements/decls in a single __queue_metaparse statement,
      // e.g. __queue_metaparse "int i; float f;", then you won't have an invalid token after
      // parsing the first statement/decl.  But after the last one in the __queue_metaparse
      // statement you WILL have an invalid token.  BUT that's not all -- if you use
      // a call to a constexpr function that calls an expand statement, that seems to
      // add an ADDITIONAL invalid token -- hence the need for a while loop here (AND
      // in the ParseCompoundStmtBody version).
      while (TheParser->getCurToken().getLocation().isInvalid()
             && !TheParser->getCurToken().is(tok::r_brace) //DWR HACK: some of the dummy rbraces have invalid locs,
                                                           //don't skip over them.
             ) {
        METAPARSE_DEBUG_VAR(TheParser->getCurToken().getKind())
        METAPARSE_DEBUG("Invalid token, consuming to load up next one")
        TheParser->ConsumeToken();

      }
      //else_METAPARSE_DEBUG("(Valid tok, not consuming...)")
        //^ you encounter this when you have multiple statements in one gen src string.

    } //end while

    //Consume the dummy r_brace we entered above, and make sure that ends the constexpr decl
    assert(TheParser->Tok.is(tok::r_brace));
    METAPARSE_DEBUG("About to ConsumeBrace to skip over r_brace")
    auto RBraceTok = TheParser->Tok;
    TheParser->ConsumeBrace();
    if (!PP.NoMoreMetaparseStrsAvailable() && Ok) {
      TheParser->Diag(RBraceTok, diag::err_extraneous_closing_brace);
      Ok = false;
    }
    PP.setNoMoreMetaparseStrsAvailable(false);

  } //end else

  METAPARSE_DEBUG("PP.getTotalNumLexers() just before returning = " << PP.getTotalNumLexers())

  return Ok;
}



