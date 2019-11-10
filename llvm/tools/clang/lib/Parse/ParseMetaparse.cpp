//DWR ADDN
//===--- ParseExpand.cpp - Reflection Parsing -----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements parsing for C++ __queue_metaparse(...) statements.
//
//===----------------------------------------------------------------------===//

//DWR FIXME: not sure which of these I really need, once stuff is working remove most of them probably.
#include "clang/AST/ASTConsumer.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/AST/PrettyDeclStackTrace.h" //DWR note: ASutton's was Sema/PrettyDeclStackTrace.h

using namespace clang;

//TODO: __queue_metaparse should be followed by a parens, and possibly by a comma-separated list of args.

/// \brief Parse a C++ __queue_metaparse statement.
///
///   queue-metaparse-statement:
///     '__queue_metaparse' '(' string-literal ')' ';'
///
/// Note that the statement parser will collect the trailing semicolon.
StmtResult Parser::ParseCXXQueueMetaparseStmt() {
  assert(Tok.is(tok::kw___queue_metaparse) && "expected __queue_metaparse");

  SourceLocation Loc = ConsumeToken();
  //Loc = SourceLocation::getMacroLoc(Loc.getOffset()); //convert from FileID to MacroID, for proper lexing/diagnostics (I hope...)

  BalancedDelimiterTracker T(*this, tok::l_paren);
  if (T.expectAndConsume(diag::err_expected_lparen_after, "__metaparse_expr"))
    return true;

  /// Compute the constexpr string to the right of the __queue_metaparse keyword
  ExprResult String = ParseConstantExpression();
  if (String.isInvalid()) {
    T.skipToEnd();
    return StmtError();
  }

  if (T.consumeClose())
    return StmtError();

  return Actions.ActOnCXXQueueMetaparseStmt(Loc, String.get());
}
/// \brief Parse a C++ __metaparse expression (for in-place metaparsing).
///
///   metaparse-expression:
///     '__metaparse_expr '(' type ',' constexpr char[N] ');'
///     '__metaparse_expr '(' type ',' string-literal ');'
///
/// Note that the parser will collect the trailing semicolon.
ExprResult Parser::ParseCXXMetaparseExpr() {
  assert(Tok.is(tok::kw___metaparse_expr) && "expected __metaparse_expr");

  SourceLocation Loc = ConsumeToken();
  //Loc = SourceLocation::getMacroLoc(Loc.getOffset()); //convert from FileID to MacroID, for proper lexing/diagnostics (I hope...)

  BalancedDelimiterTracker T(*this, tok::l_paren);
  if (T.expectAndConsume(diag::err_expected_lparen_after, "__metaparse_expr"))
    return true;

  /// [0] Compute the constexpr string:
  ExprResult String = ParseConstantExpression();
  if (String.isInvalid()) {
    T.skipToEnd();
    return ExprError();
  }
  assert(String.get());

  if (ExpectAndConsume(tok::comma))
    return ExprError();

  /// [1] Compute the type
  TypeResult Ty = ParseTypeName();
  if (Ty.isInvalid()) {
    //Skip to end before giving the error:
    T.skipToEnd();
    return ExprError();
  }
  assert(Ty.get());
  assert(!Ty.get().get().isNull() && "Didn't expect null type");

  if (T.consumeClose())
    return ExprError();

  // DWR TODO I guess you should supply the Ty.get() (= ParsedType) to
  // the ActOn..., and only change it to a QualType when you need it,
  // i.e. preserve the extra info (loc info I guess).
  QualType ActualType = Actions.GetTypeFromParser(Ty.get(), nullptr);

//  assert(isa<LocInfoType>(Ty.get().get()) &&
//         "DWR expected this to always be a LocInfoType, i.e. one you could get a TypeSourceInfo from");
//  const LocInfoType *LIT = cast<LocInfoType>(Ty.get().get());

//  TypeSourceInfo *DI = LIT->getTypeSourceInfo();

  return Actions.ActOnCXXMetaparseExpr(Loc,
                                       ActualType,
                                       String.get(), *this);
}
//END
