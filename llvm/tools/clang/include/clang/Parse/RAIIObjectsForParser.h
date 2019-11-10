//===--- RAIIObjectsForParser.h - RAII helpers for the parser ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines and implements the some simple RAII objects that are used
// by the parser to manage bits in recursion.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_PARSE_RAIIOBJECTSFORPARSER_H
#define LLVM_CLANG_LIB_PARSE_RAIIOBJECTSFORPARSER_H

#include "clang/Lex/Preprocessor.h" //DWR ADDN
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Sema/DelayedDiagnostic.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Sema.h"

namespace clang {
  // TODO: move ParsingClassDefinition here.
  // TODO: move TentativeParsingAction here.

  /// A RAII object used to temporarily suppress access-like
  /// checking.  Access-like checks are those associated with
  /// controlling the use of a declaration, like C++ access control
  /// errors and deprecation warnings.  They are contextually
  /// dependent, in that they can only be resolved with full
  /// information about what's being declared.  They are also
  /// suppressed in certain contexts, like the template arguments of
  /// an explicit instantiation.  However, those suppression contexts
  /// cannot necessarily be fully determined in advance;  for
  /// example, something starting like this:
  ///   template <> class std::vector<A::PrivateType>
  /// might be the entirety of an explicit instantiation:
  ///   template <> class std::vector<A::PrivateType>;
  /// or just an elaborated type specifier:
  ///   template <> class std::vector<A::PrivateType> make_vector<>();
  /// Therefore this class collects all the diagnostics and permits
  /// them to be re-delayed in a new context.
  class SuppressAccessChecks {
    Sema &S;
    sema::DelayedDiagnosticPool DiagnosticPool;
    Sema::ParsingDeclState State;
    bool Active;

  public:
    /// Begin suppressing access-like checks
    SuppressAccessChecks(Parser &P, bool activate = true)
            : S(P.getActions()), DiagnosticPool(nullptr) {
      if (activate) {
        State = S.PushParsingDeclaration(DiagnosticPool);
        Active = true;
      } else {
        Active = false;
      }
    }

    SuppressAccessChecks(SuppressAccessChecks &&Other)
            : S(Other.S), DiagnosticPool(std::move(Other.DiagnosticPool)),
              State(Other.State), Active(Other.Active) {
      Other.Active = false;
    }

    void operator=(SuppressAccessChecks &&Other) = delete;

    void done() {
      assert(Active && "trying to end an inactive suppression");
      S.PopParsingDeclaration(State, nullptr);
      Active = false;
    }

    void redelay() {
      assert(!Active && "redelaying without having ended first");
      if (!DiagnosticPool.pool_empty())
        S.redelayDiagnostics(DiagnosticPool);
      assert(DiagnosticPool.pool_empty());
    }

    ~SuppressAccessChecks() {
      if (Active) done();
    }
  };

  /// RAII object used to inform the actions that we're
  /// currently parsing a declaration.  This is active when parsing a
  /// variable's initializer, but not when parsing the body of a
  /// class or function definition.
  class ParsingDeclRAIIObject {
    Sema &Actions;
    sema::DelayedDiagnosticPool DiagnosticPool;
    Sema::ParsingDeclState State;
    bool Popped;

    ParsingDeclRAIIObject(const ParsingDeclRAIIObject &) = delete;

    void operator=(const ParsingDeclRAIIObject &) = delete;

  public:
    enum NoParent_t {
      NoParent
    };

    ParsingDeclRAIIObject(Parser &P, NoParent_t _)
            : Actions(P.getActions()), DiagnosticPool(nullptr) {
      push();
    }

    /// Creates a RAII object whose pool is optionally parented by another.
    ParsingDeclRAIIObject(Parser &P,
                          const sema::DelayedDiagnosticPool *parentPool)
            : Actions(P.getActions()), DiagnosticPool(parentPool) {
      push();
    }

    /// Creates a RAII object and, optionally, initialize its
    /// diagnostics pool by stealing the diagnostics from another
    /// RAII object (which is assumed to be the current top pool).
    ParsingDeclRAIIObject(Parser &P, ParsingDeclRAIIObject *other)
            : Actions(P.getActions()),
              DiagnosticPool(other ? other->DiagnosticPool.getParent() : nullptr) {
      if (other) {
        DiagnosticPool.steal(other->DiagnosticPool);
        other->abort();
      }
      push();
    }

    ~ParsingDeclRAIIObject() {
      abort();
    }

    sema::DelayedDiagnosticPool &getDelayedDiagnosticPool() {
      return DiagnosticPool;
    }

    const sema::DelayedDiagnosticPool &getDelayedDiagnosticPool() const {
      return DiagnosticPool;
    }

    /// Resets the RAII object for a new declaration.
    void reset() {
      abort();
      push();
    }

    /// Signals that the context was completed without an appropriate
    /// declaration being parsed.
    void abort() {
      pop(nullptr);
    }

    void complete(Decl *D) {
      assert(!Popped && "ParsingDeclaration has already been popped!");
      pop(D);
    }

    /// Unregister this object from Sema, but remember all the
    /// diagnostics that were emitted into it.
    void abortAndRemember() {
      pop(nullptr);
    }

  private:
    void push() {
      State = Actions.PushParsingDeclaration(DiagnosticPool);
      Popped = false;
    }

    void pop(Decl *D) {
      if (!Popped) {
        Actions.PopParsingDeclaration(State, D);
        Popped = true;
      }
    }
  };

  /// A class for parsing a DeclSpec.
  class ParsingDeclSpec : public DeclSpec {
    ParsingDeclRAIIObject ParsingRAII;

  public:
    ParsingDeclSpec(Parser &P)
            : DeclSpec(P.getAttrFactory()),
              ParsingRAII(P, ParsingDeclRAIIObject::NoParent) {}

    ParsingDeclSpec(Parser &P, ParsingDeclRAIIObject *RAII)
            : DeclSpec(P.getAttrFactory()),
              ParsingRAII(P, RAII) {}

    const sema::DelayedDiagnosticPool &getDelayedDiagnosticPool() const {
      return ParsingRAII.getDelayedDiagnosticPool();
    }

    void complete(Decl *D) {
      ParsingRAII.complete(D);
    }

    void abort() {
      ParsingRAII.abort();
    }
  };

  /// A class for parsing a declarator.
  class ParsingDeclarator : public Declarator {
    ParsingDeclRAIIObject ParsingRAII;

  public:
    ParsingDeclarator(Parser &P, const ParsingDeclSpec &DS, DeclaratorContext C)
            : Declarator(DS, C), ParsingRAII(P, &DS.getDelayedDiagnosticPool()) {
    }

    const ParsingDeclSpec &getDeclSpec() const {
      return static_cast<const ParsingDeclSpec &>(Declarator::getDeclSpec());
    }

    ParsingDeclSpec &getMutableDeclSpec() const {
      return const_cast<ParsingDeclSpec &>(getDeclSpec());
    }

    void clear() {
      Declarator::clear();
      ParsingRAII.reset();
    }

    void complete(Decl *D) {
      ParsingRAII.complete(D);
    }
  };

  /// A class for parsing a field declarator.
  class ParsingFieldDeclarator : public FieldDeclarator {
    ParsingDeclRAIIObject ParsingRAII;

  public:
    ParsingFieldDeclarator(Parser &P, const ParsingDeclSpec &DS)
            : FieldDeclarator(DS), ParsingRAII(P, &DS.getDelayedDiagnosticPool()) {
    }

    const ParsingDeclSpec &getDeclSpec() const {
      return static_cast<const ParsingDeclSpec &>(D.getDeclSpec());
    }

    ParsingDeclSpec &getMutableDeclSpec() const {
      return const_cast<ParsingDeclSpec &>(getDeclSpec());
    }

    void complete(Decl *D) {
      ParsingRAII.complete(D);
    }
  };

  /// ExtensionRAIIObject - This saves the state of extension warnings when
  /// constructed and disables them.  When destructed, it restores them back to
  /// the way they used to be.  This is used to handle __extension__ in the
  /// parser.
  class ExtensionRAIIObject {
    ExtensionRAIIObject(const ExtensionRAIIObject &) = delete;

    void operator=(const ExtensionRAIIObject &) = delete;

    DiagnosticsEngine &Diags;
  public:
    ExtensionRAIIObject(DiagnosticsEngine &diags) : Diags(diags) {
      Diags.IncrementAllExtensionsSilenced();
    }

    ~ExtensionRAIIObject() {
      Diags.DecrementAllExtensionsSilenced();
    }
  };

  /// ColonProtectionRAIIObject - This sets the Parser::ColonIsSacred bool and
  /// restores it when destroyed.  This says that "foo:" should not be
  /// considered a possible typo for "foo::" for error recovery purposes.
  class ColonProtectionRAIIObject {
    Parser &P;
    bool OldVal;
  public:
    ColonProtectionRAIIObject(Parser &p, bool Value = true)
            : P(p), OldVal(P.ColonIsSacred) {
      P.ColonIsSacred = Value;
    }

    /// restore - This can be used to restore the state early, before the dtor
    /// is run.
    void restore() {
      P.ColonIsSacred = OldVal;
    }

    ~ColonProtectionRAIIObject() {
      restore();
    }
  };

  /// RAII object that makes '>' behave either as an operator
  /// or as the closing angle bracket for a template argument list.
  class GreaterThanIsOperatorScope {
    bool &GreaterThanIsOperator;
    bool OldGreaterThanIsOperator;
  public:
    GreaterThanIsOperatorScope(bool &GTIO, bool Val)
            : GreaterThanIsOperator(GTIO), OldGreaterThanIsOperator(GTIO) {
      GreaterThanIsOperator = Val;
    }

    ~GreaterThanIsOperatorScope() {
      GreaterThanIsOperator = OldGreaterThanIsOperator;
    }
  };

  class InMessageExpressionRAIIObject {
    bool &InMessageExpression;
    bool OldValue;

  public:
    InMessageExpressionRAIIObject(Parser &P, bool Value)
            : InMessageExpression(P.InMessageExpression),
              OldValue(P.InMessageExpression) {
      InMessageExpression = Value;
    }

    ~InMessageExpressionRAIIObject() {
      InMessageExpression = OldValue;
    }
  };

  /// RAII object that makes sure paren/bracket/brace count is correct
  /// after declaration/statement parsing, even when there's a parsing error.
  class ParenBraceBracketBalancer {
    Parser &P;
    unsigned short ParenCount, BracketCount, BraceCount;
  public:
    ParenBraceBracketBalancer(Parser &p)
            : P(p), ParenCount(p.ParenCount), BracketCount(p.BracketCount),
              BraceCount(p.BraceCount) {}

    ~ParenBraceBracketBalancer() {
      P.AngleBrackets.clear(P);
      P.ParenCount = ParenCount;
      P.BracketCount = BracketCount;
      P.BraceCount = BraceCount;
    }
  };

  class PoisonSEHIdentifiersRAIIObject {
    PoisonIdentifierRAIIObject Ident_AbnormalTermination;
    PoisonIdentifierRAIIObject Ident_GetExceptionCode;
    PoisonIdentifierRAIIObject Ident_GetExceptionInfo;
    PoisonIdentifierRAIIObject Ident__abnormal_termination;
    PoisonIdentifierRAIIObject Ident__exception_code;
    PoisonIdentifierRAIIObject Ident__exception_info;
    PoisonIdentifierRAIIObject Ident___abnormal_termination;
    PoisonIdentifierRAIIObject Ident___exception_code;
    PoisonIdentifierRAIIObject Ident___exception_info;
  public:
    PoisonSEHIdentifiersRAIIObject(Parser &Self, bool NewValue)
            : Ident_AbnormalTermination(Self.Ident_AbnormalTermination, NewValue),
              Ident_GetExceptionCode(Self.Ident_GetExceptionCode, NewValue),
              Ident_GetExceptionInfo(Self.Ident_GetExceptionInfo, NewValue),
              Ident__abnormal_termination(Self.Ident__abnormal_termination, NewValue),
              Ident__exception_code(Self.Ident__exception_code, NewValue),
              Ident__exception_info(Self.Ident__exception_info, NewValue),
              Ident___abnormal_termination(Self.Ident___abnormal_termination, NewValue),
              Ident___exception_code(Self.Ident___exception_code, NewValue),
              Ident___exception_info(Self.Ident___exception_info, NewValue) {
    }
  };

  /// RAII class that helps handle the parsing of an open/close delimiter
  /// pair, such as braces { ... } or parentheses ( ... ).
  class BalancedDelimiterTracker : public GreaterThanIsOperatorScope {
    Parser &P;
    tok::TokenKind Kind, Close, FinalToken;

    SourceLocation (Parser::*Consumer)();

    SourceLocation LOpen, LClose;

    unsigned short &getDepth() {
      switch (Kind) {
        case tok::l_brace:
          return P.BraceCount;
        case tok::l_square:
          return P.BracketCount;
        case tok::l_paren:
          return P.ParenCount;
        default:
          llvm_unreachable("Wrong token kind");
      }
    }

    bool diagnoseOverflow();

    bool diagnoseMissingClose();

  public:
    BalancedDelimiterTracker(Parser &p, tok::TokenKind k,
                             tok::TokenKind FinalToken = tok::semi)
            : GreaterThanIsOperatorScope(p.GreaterThanIsOperator, true),
              P(p), Kind(k), FinalToken(FinalToken) {
      switch (Kind) {
        default:
          llvm_unreachable("Unexpected balanced token");
        case tok::l_brace:
          Close = tok::r_brace;
          Consumer = &Parser::ConsumeBrace;
          break;
        case tok::l_paren:
          Close = tok::r_paren;
          Consumer = &Parser::ConsumeParen;
          break;

        case tok::l_square:
          Close = tok::r_square;
          Consumer = &Parser::ConsumeBracket;
          break;
      }
    }

    SourceLocation getOpenLocation() const { return LOpen; }

    SourceLocation getCloseLocation() const { return LClose; }

    SourceRange getRange() const { return SourceRange(LOpen, LClose); }

    bool consumeOpen() {
      if (!P.Tok.is(Kind))
        return true;

      if (getDepth() < P.getLangOpts().BracketDepth) {
        LOpen = (P.*Consumer)();
        return false;
      }

      return diagnoseOverflow();
    }

    bool expectAndConsume(unsigned DiagID = diag::err_expected,
                          const char *Msg = "",
                          tok::TokenKind SkipToTok = tok::unknown);

    bool consumeClose() {
      if (P.Tok.is(Close)) {
        LClose = (P.*Consumer)();
        return false;
      } else if (P.Tok.is(tok::semi) && P.NextToken().is(Close)) {
        SourceLocation SemiLoc = P.ConsumeToken();
        P.Diag(SemiLoc, diag::err_unexpected_semi)
                << Close << FixItHint::CreateRemoval(SourceRange(SemiLoc, SemiLoc));
        LClose = (P.*Consumer)();
        return false;
      }

      return diagnoseMissingClose();
    }

    void skipToEnd();
  };

  /// RAIIObject to destroy the contents of a SmallVector of
  /// TemplateIdAnnotation pointers and clear the vector.
  class DestroyTemplateIdAnnotationsRAIIObj {
    SmallVectorImpl<TemplateIdAnnotation *> &Container;

  public:
    DestroyTemplateIdAnnotationsRAIIObj(
            SmallVectorImpl<TemplateIdAnnotation *> &Container)
            : Container(Container) {}

    ~DestroyTemplateIdAnnotationsRAIIObj() {
      for (SmallVectorImpl<TemplateIdAnnotation *>::iterator I =
              Container.begin(),
                   E = Container.end();
           I != E; ++I)
        (*I)->Destroy();
      Container.clear();
    }
  };



//DWR ADDN:

  /// ParserBrickWallRAII stores and temporarily sets state variables
  /// to temporarily prevent parsing from proceeding beyond the point
  /// it was constructed, until it is destroyed.
  /// New lexers can then be pushed "in front of" the brick wall
  /// and lexed from with the assurance that you will not accidentally
  /// eat into the content beyond the wall.
  ///
  /// This is used in parsing __queue_metaparse statements; e.g.:
  /// \code
  ///   class MyClass {
  ///     int varA;
  ///     constexpr {
  ///       __queue_metaparse "int varB;";
  ///       __queue_metaparse "float varC;";
  ///     } /*BRICK WALL HERE while processing __queue_metaparse stmts*/
  ///     char varD;
  ///   };
  /// \endcode
  ///
  /// To process the __queue_metaparse statements, we first create a ParserBrickWallRAII
  /// at the end of the constexpr decl, then push new lexers onto the stack to
  /// parse the contents of the __queue_metaparse statements, assured that we won't
  /// accidentally parse, or even be able to peek at, the char varD line following
  /// the constexpr decl.
  ///
  /// The utility isn't evident in this simple example, but it is very clear
  /// when we need to handle template instantiations with __queue_metaparse statements
  /// (which we handle with the TempParseIntoClassInstantiation : ParserBrickWallRAII class
  /// defined below).
  ///
  /// Note that these can be constructed recursively with the expected results
  /// -- which, again, is useful primarily when performing a series of template
  /// instantiations: e.g. when MyTmplWCEDeclsC<int> has __queue_metaparse statements
  /// that, when parsed, require instantiating MyTmplWCEDeclsB<void>,
  /// which has __queue_metaparse statements that, when parsed, requires instantiating
  /// MyTmplWCEDeclsA<long>, etc.
  ///
  /// Usage: For non-template classes, the ParserBrickWallRAII will be created in
  /// Sema::ActOnFinishConstexprDecl(...); for template instantiations it will be
  /// created in Sema::InstantiateClass(...).
  ///
  /// Debugging: if you get strange errors when using __queue_metaparse statements in
  /// unusual code, it may be because not enough state variables are being saved
  /// and restored in this RAII; so simply add debug messages (via METAPARSE_DEBUG(...),
  /// METAPARSE_DEBUG_VAR(...), etc.) in the ctors and dtors below to identify which
  /// variables are changing in a meaningful way, then when you find them
  /// add the relevant data members and save/restore statements in the
  /// relevant ctor/dtor.
  ///
  class ParserBrickWallRAII {

    class JustTheParserVariables {
      friend class ParserBrickWallRAII;
      Parser *TheParser;
      bool WasInCachingLexMode;
      bool Valid = true;
#ifndef NMETAPARSEDEBUG
      Token DEBUGOldTok;
      size_t ClassStack_Size;
#endif

    public:
      JustTheParserVariables(Parser &P);
      ~JustTheParserVariables();
      void setInvalid();
    } SavedParseState;

    /// This saves just the state of the Preprocessor upon construction,
    /// and restores it upon destruction.
    ///
    /// Most critically, we set the Preprocessor's TotalNumLexersAtRAIIConstruction's
    /// variable, which it uses internally in PopIncludeMacroStack() to ensure it does not
    /// lex into the enclosing source code whenver it is ParsingFromMetaSrcStrs().
    ///
    /// This should arguably be a public class in, say, a Lex/RAIIObjectsForPreprocessor.h
    /// file, since it is independent of the Parser (whereas the JustTheParserVariables
    /// RAII should never be constructed without the PreprocessorBrickWallRAII, thus
    /// it should definitely be a private class).  But it is only used here for now,
    /// so we keep it as a private implementation detail of ParserBrickWallRAII.
    ///
    /// Note that there are a number of Preprocessor variables that could theoretically
    /// need to be saved and restored, -- e.g. LastTokenWasAt, ParsingIfOrElifDirective,
    /// etc.; if these variables were to be changed while parsing generated sources during
    /// template instatiation, e.g. by adding an "#endif" in a templated expand statement without
    /// a preceding "#if", and then instantiating within an #if, they would change the
    /// parsing interpretation at the instantiation point, possibly causing
    /// difficult-to-debug errors (e.g. if after the instantiation another #endif were present).
    /// However, any code that would cause such state changes would be very non-standard
    /// as far as I can imagine, so I think it's safe, and perhaps helpful to advanced
    /// users, to not explicitly restrict or prevent such uses, until they are proven
    /// to be pathological.
    class PreprocessorBrickWallRAII {
      friend class ParserBrickWallRAII;
      Preprocessor &PP;
      const size_t OldTotalNumLexersAtRAIIConstruction;
      bool OldParsingFromMetaSrcStrBool;
      decltype(PP.CachedTokens) SavedCachedTokens;
      decltype(PP.CachedLexPos) SavedCachedLexPos;
      decltype(PP.BacktrackPositions) SavedBacktrackPositions;
      decltype(PP.CachedTokenRangeToErase) SavedCachedTokenRangeToErase;
      bool Valid = true;
    public:
      PreprocessorBrickWallRAII(Preprocessor &ThePP);
      ~PreprocessorBrickWallRAII();
      /// Used in ~ParserBrickWallRAII() (NOT in ~JustTheParserVariables())
      /// BEFORE the SavedPreprocessorState destruction
      /// to clear out any dead lexers -- (only TokenLexers I think) left over
      /// by late parsing.
      void setInvalid();

      void clearAnyDeadLexers();
#ifndef NMETAPARSEDEBUG
      const bool NonNullCurPPLexer;
      bool TotalLexers_Equals_TotalLexersAtRAIIConstruction() const {
        return PP.getTotalNumLexers() == PP.TotalNumLexersAtRAIIConstruction;
      }
#endif
    } SavedPreprocessorState;
    bool Valid = true;
  public:
    ParserBrickWallRAII(Parser &P);
    ~ParserBrickWallRAII();
    void setInvalid();

//    /// \returns a tok::eof if no unexpected tokens were encountered,
//    /// or a valid token if an unexpected token WAS encountered,
//    /// indicating you likely did not parse enough source code
//    /// and thus the string provided for metaparsing has extraneous
//    /// characters or was otherwise ill-formed,
//    /// or a tok::unknown if there was an error during preprocessing
//    /// that didn't result in any valid extra nonnull token being found.
//    Token getFirstLeftoverToken();

#ifndef NMETAPARSEDEBUG
    static unsigned DEBUGrecursiondepth;
    bool PPTotalLexers_Equals_TotalLexersAtRAIIConstruction() const {
      return SavedPreprocessorState.TotalLexers_Equals_TotalLexersAtRAIIConstruction();
    }
#endif
  };


  //DWR FIXME: I think you can/should remove the DeclContext *DC/Instantiation params from
  //     the below RAII, and instead always use TheSema.CurContext, because that's how
  //     they are always used.  Then, if you want to also change the CurContext you have
  //     to alter do an RAII for that as well.
  //     Bottom line, this RAII just brings the Parser into sync with where the Sema is at, temporarily.

  /// Sets up the ParserBrickWall RAII, and sets the scope to be that of a different
  /// entity (e.g. an instantiation), for proper lookup; then on destruction returns
  /// the parser and TheSema.CurScope to their original states
  /// TODO add example code for a function template, assuming this works
  class TempParseIntoDiffScope : public ParserBrickWallRAII
  {
    Sema &TheSema;
    Scope *OldScope;
    Sema::ParsingIntoInstantiation OldPII;
  public:
    TempParseIntoDiffScope(Sema &S, Sema::ParsingIntoInstantiation NewPII, unsigned ScopeFlags);
    ~TempParseIntoDiffScope();
  };




  /// \code
  ///   template<typename T>
  ///   struct Tmpl {
  ///     T varA;
  ///     constexpr {
  ///       __queue_metaparse "int varB";
  ///     }
  ///     char varD;
  ///   };
  ///   Tmpl<float> /*TempParseIntoClassInstantiation CREATED HERE,
  ///             * in InstantiateClass*/ myinstantiation;
  /// \endcode
  ///
  /// During instantiation of Tmpl<float> (which occurs just before parsing "myinstantiation"),
  /// we will need to do further parsing.  We again want a brick wall
  /// to be sure our parsing doesn't creep beyond the current parse state (i.e.
  /// into myinstantiation); however we need MORE than that: we will also
  /// to temporarily set up the context and scope of the
  /// instantiation so that we may parse directly into it and do lookup from it, and then
  /// when we're done restore the original parse state, context, scope, etc.
  // DWR FIXME can we give this a better name?
  class TempParseIntoClassInstantiation : public TempParseIntoDiffScope
  {
    Parser::ParsingClassDefinition Pcd;
  public:
    TempParseIntoClassInstantiation(Sema &S, bool IsInterface);
    ~TempParseIntoClassInstantiation();
  };

  /// DWR TODO add comments
  class TempParseIntoFuncInstantiation : public TempParseIntoDiffScope
  {
  public:
    TempParseIntoFuncInstantiation(Sema &S);
    ~TempParseIntoFuncInstantiation();
  };

  class SemaPIIRAII {
    Sema &SemaRef;
    decltype(SemaRef.PII) OldPII;
    bool exited = false;
  public:
    SemaPIIRAII(Sema &SemaRef) : SemaRef(SemaRef), OldPII(SemaRef.PII) {}
    ~SemaPIIRAII() {
      if (!exited)
        SemaRef.PII = OldPII;
    }
    //Exit early
    void Exit() {
      if (!exited) {
        SemaRef.PII = OldPII;
        exited = true;
      }
    }
  };

//END DWR ADDN


} // end namespace clang

#endif
