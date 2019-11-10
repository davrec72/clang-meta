//DWR ADDN:
//
// Created by David Rector on 2019-02-26.
//

//Note: this file is included in the lib/Sema/CMakeLists.txt, but not the lib/Parse/CMakeLists.txt,
// because it is exclusively used by Sema methods.  You probably could add it to the Parse one though without issue.

#include "clang/Lex/MetaparseDebug.h"
#include "clang/Parse/RAIIObjectsForParser.h"

using namespace clang;

ParserBrickWallRAII::JustTheParserVariables::JustTheParserVariables(Parser &P)
  : TheParser(&P), WasInCachingLexMode(P.PP.InCachingLexMode())
#ifndef NMETAPARSEDEBUG
  , DEBUGOldTok(P.Tok)
  , ClassStack_Size(P.ClassStack.size())
#endif
{
  METAPARSE_DEBUG_HDR("Constructing ParserBrickWallRAII::JustTheParserVariables")
  METAPARSE_DEBUG("Old P.Tok: " << tokIDstr(P.Tok))

  //NB: The PreprocessorBrickWallRAII will NOT have been constructed yet,
  // so TheParser->PP is still in its old state.

  //We unconsume the current token, first making sure it is not an eof, to help us
  //restore the state during destruction via a ConsumeToken call.
  //Note that this may push an extra lexer temporarily onto the IncludeMacroStack
  // than we had originally -- but since the PreprocessorBrickWallRAII will be constructed
  // after this, it will register the proper num lexers as the TotalNumLexersAtRAIIConstruction
  // it uses to identify exactly when it is finished processing the current batch of
  // generated source strings.  I.e. this code shouldn't affect the behavior of the Preprocessor.

  // DWR commented this out because when expanding a statement with a macro inside it this would fail,
  // and I don't see any problems with it commented out as of yet.
//  assert(P.Tok.getLocation().isValid()
//         || P.Tok.is(tok::eof)
//         && "Did not expect an invalid Tok here"); //DWR TEMP TEST COMMENTED OUT FIXME!!!

  IF_METAPARSE_DEBUG( P.PP.debugDispCachedTokens("initial"); )

  P.PP.CleanUpCache();

  IF_METAPARSE_DEBUG( P.PP.debugDispCachedTokens("after CleanUpCache()"); )


//  if (P.PP.InCachingLexMode() && P.PP.isBacktrackEnabled()) {
//    METAPARSE_DEBUG("Backtrack enabled, calling RevertCachedTokens(1) instead of UnconsumeToken")
//    P.PP.RevertCachedTokens(1);
//    P.Tok = P.EmptyToken;
//  } else {
    METAPARSE_DEBUG("Calling UnconsumeToken")
    P.UnconsumeToken(P.EmptyToken);
//  }

  assert(P.PP.InCachingLexMode()
         && "Expected to be in caching lex mode at this point"); //sanity check

  IF_METAPARSE_DEBUG( P.PP.debugDispCachedTokens("after unconsuming"); )

  P.PP.ExitCachingLexMode();

//  P.ParsingFromMetaSrcStrBool = true;
  //Note that P.PP.ParsingFromMetaSrcStrs() should not be asserted at this point, because
  // the PreprocesLocationRAII has not yet been constructed.
}

ParserBrickWallRAII::JustTheParserVariables::~JustTheParserVariables() {
  METAPARSE_DEBUG_HDR("...Destroying ParserBrickWallRAII::JustTheParserVariables")
  //NB: The PreprocessorBrickWallRAII will have been destroyed BEFORE this,
  // so TheParser->PP should be back to its original state
  // (and all the asserts about the numlexers being the same before
  // and after will have passed.)
//  TheParser->ParsingFromMetaSrcStrBool = TheParser->PP.ParsingFromMetaSrcStr();

  TheParser->PP.EnterCachingLexMode(); //Since the UnconsumeToken cached the orig token

//  if (Valid )
//    TheParser->ConsumeToken();
//  else
    TheParser->ConsumeAnyToken();

  IF_METAPARSE_DEBUG(TheParser->PP.debugDispCachedTokens("after ConsumeToken()"));

  if (!WasInCachingLexMode) { //i.e. if before the Unconsume you weren't already in Caching mode
    METAPARSE_DEBUG("Was not in caching lex mode originally, therefore will ExitCachingLexMode()...")
    TheParser->PP.ExitCachingLexMode();
  }
  else_METAPARSE_DEBUG("WAS in caching lex mode originally...")

  METAPARSE_DEBUG_VAR(tokIDstr(TheParser->Tok))

#ifndef NMETAPARSEDEBUG
  assert(tokIDstr(TheParser->Tok) == tokIDstr(DEBUGOldTok)
    //^ DWR POSSIBLE FIXME better to compare addresses?
         && "Expected the Tok after the final ConsumeToken call to "
            "be restored to its original value!");
  assert(Parser::DEBUGnumParsersConstructed > 0
         && "Parser static variable not initialized correctly -- "
            "possibly initialized twice?  Check if you included the"
            "cpp with its initialization in multiple libraries -- that"
            "might be the issue.");
  assert(Parser::DEBUGnumParsersConstructed != 1
         || DEBUGrecursiondepth //NB this will have been decremented in ~TempParseIntoClassInstantiation() before getting here
         || !TheParser->ParsingFromMetaSrcStr()
            && "If you've only ever constructed one parser, "
               "and DEBUGrecursiondepth == 0, "
               "that should imply you're no longer ParsingFromMetaSrcStrs!");
  assert(TheParser->ClassStack.size() == ClassStack_Size
         && "You somehow ate into the old ClassStack"
            "while in the new parsing state!");

//  METAPARSE_DEBUG_VAR(TheParser->ClassStack.top()->LateParsedDeclarations.size())
#endif
}

void ParserBrickWallRAII::JustTheParserVariables::setInvalid() {
  Valid = false;
};

ParserBrickWallRAII::PreprocessorBrickWallRAII::PreprocessorBrickWallRAII(Preprocessor &ThePP)
  : PP(ThePP)
  , OldTotalNumLexersAtRAIIConstruction(PP.TotalNumLexersAtRAIIConstruction)
  , OldParsingFromMetaSrcStrBool(PP.ParsingFromMetaSrcStrBool)
  , SavedCachedTokens(PP.CachedTokens)
  , SavedCachedLexPos(PP.CachedLexPos)
  , SavedBacktrackPositions(PP.BacktrackPositions)
  , SavedCachedTokenRangeToErase(PP.CachedTokenRangeToErase)
#ifndef NMETAPARSEDEBUG
  , NonNullCurPPLexer(PP.CurPPLexer)
#endif
{
  METAPARSE_DEBUG_HDR("Constructing ParserBrickWallRAII::PreprocessorRAII")
  IF_METAPARSE_DEBUG( PP.debugDispLexerKinds(); )
  PP.TotalNumLexersAtRAIIConstruction = PP.getTotalNumLexers();
  METAPARSE_DEBUG_VAR(OldTotalNumLexersAtRAIIConstruction)
  METAPARSE_DEBUG_VAR(PP.TotalNumLexersAtRAIIConstruction)

  assert(!PP.NoMoreMetaparseStrsAvailableBool
         && "Did not expect to construct a PreprocessorBrickWallRAII while "
            "DoneProcessing... was true; was that value perhaps not reset correctly"
            "after the last constexpr decl processing?");
  PP.ParsingFromMetaSrcStrBool = true;
  METAPARSE_DEBUG_VAR(OldParsingFromMetaSrcStrBool)
  assert(PP.ParsingFromMetaSrcStr()); //sanity check

  assert(PP.isBacktrackEnabled() == !SavedBacktrackPositions.empty()); //just to be sure
  IF_METAPARSE_DEBUG( PP.debugDispCachedTokens("before clearing"); )

  //Clear the cache
  PP.CachedTokens.clear();
  PP.CachedLexPos = 0;
  PP.BacktrackPositions.clear();
  PP.CachedTokenRangeToErase = None;
}

ParserBrickWallRAII::PreprocessorBrickWallRAII::~PreprocessorBrickWallRAII() {
  assert (PP.getTotalNumLexers() >= PP.TotalNumLexersAtRAIIConstruction
          && "Your metaparse statements/expressions ate past the brick wall!");

  METAPARSE_DEBUG_HDR("...Destroying ParserBrickWallRAII::PreprocessorRAII")
  clearAnyDeadLexers();
  IF_METAPARSE_DEBUG(
    PP.debugDispLexerKinds();
    signed int numlexersdiff = PP.getTotalNumLexers() - PP.TotalNumLexersAtRAIIConstruction;
    if (numlexersdiff != 0) {
      assert(numlexersdiff >= 0 && "PP has fewer lexers than when this RAII was constructed!");
      assert(false && "You have more lexers than you started with -- was "
                      "late-parsing performed and you forgot to clearAnyDeadLexers()?");
    }
  )

  PP.TotalNumLexersAtRAIIConstruction = OldTotalNumLexersAtRAIIConstruction;
  PP.ParsingFromMetaSrcStrBool = OldParsingFromMetaSrcStrBool;
  METAPARSE_DEBUG_VAR(PP.TotalNumLexersAtRAIIConstruction)
  METAPARSE_DEBUG_VAR(PP.ParsingFromMetaSrcStrBool)


  assert(PP.CachedTokens.size() == PP.CachedLexPos || PP.ErrorWhileParsingFromMetaSrcStr()
         && "DWR: BUG! Finished processing gen src strs, with no errors reported, "
            "and num lexers was same before"
            "and after (per previous assert), BUT it appears you somehow cached some extra"
            "tokens that you didn't use, indicating you ate into the enclosing source code"
            "after the generated source.  So, somehow the code has a bug AND the previous"
            "assert isn't doing its job! (Well, assuming that the CachedLexPos < "
            "CachedTokens.size(); should never be greater.)");

  PP.setErrorWhileParsingFromMetaSrcStr(false);

  METAPARSE_DEBUG("Restoring CachedTokens, CachedLexPos, BacktrackPositions, and CachedTokenRangeToErase")
  PP.CachedTokens = SavedCachedTokens;
  PP.CachedLexPos = SavedCachedLexPos;
  PP.BacktrackPositions = SavedBacktrackPositions;
  PP.CachedTokenRangeToErase = SavedCachedTokenRangeToErase;

  IF_METAPARSE_DEBUG(
    if (NonNullCurPPLexer != (bool)PP.CurPPLexer) {
      METAPARSE_DEBUG("On construction, CurPPLexer was " << (NonNullCurPPLexer ? "non-" : "")
                << "null, but now it is " << (PP.CurPPLexer ? "non-" : "") << "null")
    } else {
      METAPARSE_DEBUG("On construction, CurPPLexer was " << (NonNullCurPPLexer ? "non-" : "")
                << "null, and now it is also " << (PP.CurPPLexer ? "non-" : "") << "null")
    }
    IF_METAPARSE_DEBUG( PP.debugDispCachedTokens("after destruction"); )
  )
}

/// Used in ~ParserBrickWallRAII()
/// to clear out any dead lexers (should only be TokenLexers I think, left over
/// by late parsing) before proceeding to the ~PreprocessorBrickWallRAII()
/// call, where we will double-check our work by asserting that the numlexers is
/// the same as when it was first constructed.
/// -- Also, if errors are encountered during processing, such that you've called
/// setInvalid on this object, this will clear away the "live lexers" as well.
/// \returns a tok::eof if everything cleared as expected or !Valid (such
/// that it fully expected to encounter valid tokens while clearing), otherwise
/// the first valid tok it encounters.
/// I.e. you can call this manually before destruction to see if the input
/// string was ill-formed or other errors were encountered.
//TODO: put this in PreprocessorBrickWallRAII
void ParserBrickWallRAII::PreprocessorBrickWallRAII::clearAnyDeadLexers() {
  METAPARSE_DEBUG_HDR("In clearAnyDeadLexers()")
  METAPARSE_DEBUG_VAR(PP.getTotalNumLexers())
  METAPARSE_DEBUG_VAR(PP.TotalNumLexersAtRAIIConstruction)
  IF_METAPARSE_DEBUG( PP.debugDispLexerKinds(); ) //TEMP

//  // In case we ran into errors while parsing, add that into the
//  // "valid" variable so we know to expect some non-null toks below maybe
//  Valid = Valid && !PP.ErrorWhileParsingFromMetaSrcStr();

//  Token FirstNonnullTok;
//  llvm::errs() << "DWR TEMP DEBUG in clearAnyDeadLexers; PP.ErrorWhileParsingFromMetaSrcStr() = " << PP.ErrorWhileParsingFromMetaSrcStr() << "\n";
//  FirstNonnullTok.setKind(tok::eof); //default

//  bool untermd_error = false;
  // If we've been lexing from a terminator lexer to kill off an unterminated
  // metaparse expression, we can stop now.  Must do this before any more
  // PP.Lex calls or we'll never make any progress.
  if (PP.CurLexerKind == Preprocessor::CLK_TerminatorPretendLexer) {
      //Since we didn't PushIncludeMacroStack when we first set
      // CLK_TerminatorPretendLexer as the kind, we can just recompute
      // to get back to the original state.
      // First gotta set the ParsingFromMetaSrcStr val; we'll do that
      // again later but we definitely need to do it now before recomputing,
      // becuase the introduction of the terminator lexer wiped out that
      // info:
      PP.ParsingFromMetaSrcStrBool = OldParsingFromMetaSrcStrBool;
      PP.recomputeCurLexerKind();
      assert(PP.CurLexerKind != Preprocessor::CLK_TerminatorPretendLexer);
//      untermd_error = true;
  }

  Token ShouldBeNullTok;
  while (PP.getTotalNumLexers() > PP.TotalNumLexersAtRAIIConstruction) {
    llvm::errs() << "DWR TEMP DEBUG lexing a token...\n";
    PP.Lex(ShouldBeNullTok);
    //    METAPARSE_DEBUG("PP.Lex result (expecting unknown): " << tokIDstr(Result))

    if (!ShouldBeNullTok.getLocation().isInvalid()) {
//      if (FirstNonnullTok.getKind() == tok::eof) {
//        FirstNonnullTok = ShouldBeNullTok;
//        assert(FirstNonnullTok.getKind() != tok::eof
//            && "DWR I didn't think eof toks could have valid loks, I guess you need"
//               "to choose a different default tok kind than tok::eof here");
//      }
      IF_METAPARSE_DEBUG(
        if (Valid) {
          METAPARSE_DEBUG_HDR("Unexpected nonnull token encountered during clearAnyDeadLexers; "
              "probably extraneous characters appended to the input string, or otherwise ill-formed")
          METAPARSE_DEBUG_VAR(ShouldBeNullTok.getKind())
          METAPARSE_DEBUG_VAR(ShouldBeNullTok.getName())
        }
      )
    }

//    assert(Result.getLocation().isInvalid()
//           || !Valid //we disregard this assert if Valid has been set to false
//           && "Expected an invalid token -- are you calling this function before you've finished"
//              "processing the generates sources?");
  }

  PP.setNoMoreMetaparseStrsAvailable(false);

  // I think it is critical you do NOT temporarily Lex a token beyond the while loop,
  // e.g. to assert that it's valid or that it equals the DEBUGOldToken, because
  // we may need to e.g. restore CachedTokens to its old value before we can properly
  // Lex the code beyond the brick wall, and right now that happens after the
  // clearAnyDeadLexers() call in ~PreprocessorBrickWallRAII().


//  assert (!PP.ErrorWhileParsingFromMetaSrcStr() || FirstNonnullTok.getKind() != tok::eof); //DWR sanity check

//  return FirstNonnullTok;
}

///// \returns a tok::eof if no unexpected tokens were encountered,
///// or a valid token if an unexpected token WAS encountered,
///// indicating you likely did not parse enough source code
///// and thus the string provided for metaparsing has extraneous
///// characters or was otherwise ill-formed.
///// NOTE that you should look at the Parser::getCurToken() before
///// calling this; if there is only one leftover token, it will be there,
///// this won't tell you about it.
//Token ParserBrickWallRAII::getFirstLeftoverToken() {
//  return SavedPreprocessorState.clearAnyDeadLexers();
//}

void ParserBrickWallRAII::PreprocessorBrickWallRAII::setInvalid() {
  PP.setNoMoreMetaparseStrsAvailable(false);
    //^ DWR HACK (?) this seems to be left as true sometimes during error recovery
  Valid = false;
};

ParserBrickWallRAII::ParserBrickWallRAII(Parser &P)
        : SavedParseState(P) //Must be constructed BEFORE SavedPreprocessorState and destroyed AFTER it.
        , SavedPreprocessorState(P.PP)
{
  METAPARSE_DEBUG_HDR("Constructing ParserBrickWallRAII")
  IF_METAPARSE_DEBUG(
    ++DEBUGrecursiondepth;
    assert(P.ParsingFromMetaSrcStr() == true && P.PP.ParsingFromMetaSrcStr() == true
           && "Expected the Preprocessor and Parser to both have set ParsingFromMetaSrcStrBool = true!");
    assert(Parser::DEBUGnumParsersConstructed != 0
           && "DEBUGnumParsersConstructed == 0, which cannot be -- an issue with static var initialization?");
    if (Parser::DEBUGnumParsersConstructed > 1) {
      METAPARSE_DEBUG("NB: detected that more than one parser has been constructed, "
                "so any assertions about DEBUGrecursiondepth won't work...\n")
    }
  )
}

ParserBrickWallRAII::~ParserBrickWallRAII() {
  METAPARSE_DEBUG_HDR("...Destroying ParserBrickWallRAII")

  //NB: Anything you add here will be performed BEFORE destruction of data members.
  IF_METAPARSE_DEBUG(
    --DEBUGrecursiondepth;
  )
  // Then, AFTER this block:
  // --SavedPreprocessorState's destructor will be called (will make sure
  //    numlexers is the same as it was before, and will add back in any
  //    forward-cached tokens it temporarily chopped off), THEN
  // --SavedParseState's destructor will be called
  //    (will call ConsumeToken() to load back the orig token)
}

/// There are Valid bools in each of the RAII component classes; right now we only use them
/// to disable asserts when an attempted parse yielded invalid results, so
/// perhaps you could constrain all the validity-checking and setInvalid in DEBUG mode.
/// DWR FIXME ^
void ParserBrickWallRAII::setInvalid() {
  METAPARSE_DEBUG("Setting BrickWallRAII objects as invalid, to put them in error recovery mode...")
  Valid = false;
  SavedPreprocessorState.setInvalid();
  SavedParseState.setInvalid();
};

TempParseIntoDiffScope::TempParseIntoDiffScope(Sema &S, Sema::ParsingIntoInstantiation NewPII, unsigned ScopeFlags)
        : ParserBrickWallRAII(S.getParser())
        , TheSema(S)
        , OldScope(S.CurScope)
        , OldPII(S.PII)
{
  METAPARSE_DEBUG_HDR("Setting scope variables for proper lookup")
  // Enter the scope of TagOrTemplate, using the TUScope as the parent
  assert(S.TUScope && "Assumed we'd have a nonnull Translation Unit Scope here!");
  S.CurScope = S.TUScope;
  //^ DWR POSSIBLE FIXME: are you sure you shouldn't ExitScope, storing the flags in a stack, until you reach TUScope, then re-enter it on destruction using the stored flags?  If you get errors might want to try that.
  S.getParser().EnterScope(ScopeFlags);
  S.CurScope->setEntity(S.CurContext); //This is the key to getting LookupName to work for classes
  S.PII = NewPII;
}

TempParseIntoDiffScope::~TempParseIntoDiffScope() {
  METAPARSE_DEBUG_HDR("...Destroying TempParseIntoDiffScope: returning Scope to OldScope")
  TheSema.getParser().ExitScope();
  assert(TheSema.CurScope == TheSema.TUScope
         && "Expected to be back at TUScope -- was a scope imbalance somehow introduced?");
  TheSema.CurScope = OldScope;
  TheSema.PII = OldPII;
}

TempParseIntoClassInstantiation::TempParseIntoClassInstantiation(
        Sema &S, bool IsInterface)
        : TempParseIntoDiffScope(S, Sema::PII_class, Scope::ClassScope|Scope::DeclScope)
          //^ DWR: I intended these scope flags to be the standard class definition flags
        , Pcd(S.getParser(), Decl::castFromDeclContext(S.CurContext),
                true/*always treat instantiations as non-nested classes*/,
                IsInterface)
{
  METAPARSE_DEBUG_HDR("Constructed Pcd (to hold late-parsed stuff)")
}

TempParseIntoClassInstantiation::~TempParseIntoClassInstantiation() {
  METAPARSE_DEBUG_HDR("...Destroying Pcd")
}

TempParseIntoFuncInstantiation::TempParseIntoFuncInstantiation(Sema &S)
        : TempParseIntoDiffScope(S, Sema::PII_func, Scope::FnScope | Scope::DeclScope |
                                                    Scope::CompoundStmtScope)
          //^ DWR: I intended these scope flags to be the standard function definition flags

{
//  METAPARSE_DEBUG_HDR("")
}

TempParseIntoFuncInstantiation::~TempParseIntoFuncInstantiation() {
//  METAPARSE_DEBUG_HDR("")
}

//END
