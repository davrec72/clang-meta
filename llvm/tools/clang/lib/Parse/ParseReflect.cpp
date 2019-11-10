//ASUTTON ADDN
//===--- ParseReflect.cpp - Reflection Parsing ----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements parsing for C++ reflection.
//
//===----------------------------------------------------------------------===//

#include "clang/Lex/MetaparseDebug.h" //DWR ADDN
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/CDContextVars.h" //DWR ADDN
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/AST/PrettyDeclStackTrace.h" //DWR Note: ASutton's was clang/Sema/PrettyDeclStackTrace.h

using namespace clang;

/// FIXME: This function is called from several parses. We need to provide more
/// information in order to appropriately diagnose errors.
ExprResult Parser::ParseReflectOperand(SourceLocation OpLoc)
{
  CXXScopeSpec SS;
  ParseOptionalCXXScopeSpecifier(SS, nullptr, /*EnteringContext=*/false);

  // If the next token is an identifier, try to resolve that. This will likely
  // match most uses of the reflection operator, but there are some cases
  // of id-expressions and type-ids that must be handled separately.
  if (!SS.isInvalid() && Tok.is(tok::identifier)) {
    SourceLocation IdLoc = Tok.getLocation();
    IdentifierInfo *II = Tok.getIdentifierInfo();
    ExprResult Expr = Actions.ActOnCXXReflectExpr(OpLoc, SS, II, IdLoc);
    if (!Expr.isInvalid()) {
      ConsumeToken();
      return Expr;
    }
  }

  // Determine if the operand is actually a type-id.
  if (isCXXTypeId(TypeIdAsTemplateArgument)) {
    DeclSpec DS(AttrFactory);
    ParseSpecifierQualifierList(DS);
    Declarator D(DS, DeclaratorContext::TypeNameContext); //DWR Note: ASutton's was Declarator::TypeNameContext
    ParseDeclarator(D);
    return Actions.ActOnCXXReflectExpr(OpLoc, D);
  }

//ASUTTON ADDN:
  // If not that, then this could be an id-expression. Try parsing this.
  Token Replacement;
  ExprResult Id = tryParseCXXIdExpression(SS, true, Replacement);
  if (!Id.isInvalid())
    return Actions.ActOnCXXReflectExpr(OpLoc, Id.get());
//END

  return ExprError();
}

/// \brief Parse a reflect expression.
///
/// \verbatim
///   primary-expression:
///     '$' id-expression
///     '$' type-id
///     '$' nested-name-specifier[opt] namespace-name
/// \endverbatim
// TODO: Consider adding specifiers? $static? $private?
ExprResult Parser::ParseReflectExpression() {
  assert(Tok.is(tok::dollar));
  SourceLocation OpLoc = ConsumeToken();
  return ParseReflectOperand(OpLoc);
}

/// \brief Parse a \c reflexpr expression.
///
/// \verbatim
///   primary-expression:
///      'reflexpr' '(' reflection-id ')'
///
///   reflection-id:
///      id-expression
///      type-id
///      nested-name-specifier[opt] namespace-name
ExprResult Parser::ParseReflexprExpression() {
  assert(Tok.is(tok::kw_reflexpr));
  SourceLocation KeyLoc = ConsumeToken();

  BalancedDelimiterTracker T(*this, tok::l_paren);
  if (T.expectAndConsume(diag::err_expected_lparen_after, "reflexpr"))
    return ExprError();
  ExprResult Result = ParseReflectOperand(KeyLoc);
  T.consumeClose();
  if (!Result.isInvalid())
    Result = Actions.ActOnCXXReflexprExpr(Result.get(), T.getOpenLocation(),
                                          T.getCloseLocation());
  return Result;
}

//ASUTTON ADDN:
/// \brief Parse a idexpr-id
///
///   unqualified-id:
///      'idexpr' '(' constant-argument-list ')'
///
/// Each argument in the constant-argument-list must be a constant expression.
///
/// Returns true if parsing or semantic analysis fail.
bool Parser::ParseCXXExprId(UnqualifiedId& Result) {
  assert(Tok.is(tok::kw_idexpr));
  SourceLocation KeyLoc = ConsumeToken();

  BalancedDelimiterTracker T(*this, tok::l_paren);
  if (T.expectAndConsume(diag::err_expected_lparen_after, "idexpr"))
    return true;
  SmallVector<Expr *, 4> Parts;
  while (true) {
    ExprResult Result = ParseConstantExpression();
    if (Result.isInvalid())
      return true;
    Parts.push_back(Result.get());
    if (Tok.is(tok::r_paren))
      break;
    if (ExpectAndConsume(tok::comma))
      return true;
  }
  if (T.consumeClose())
    return true;

  return Actions.BuildDeclnameId(Parts, Result, KeyLoc,
                                 T.getOpenLocation(), T.getCloseLocation());
}
//END
////ASUTTON ADDN:
///// Parse a has-name expression:
/////
/////   hasname-expression:
/////     'hasname' '(' reflection-expression ',' unqualified-id ')'
/////
///// \todo Support qualified-ids in that space?
//ExprResult Parser::ParseHasNameExpression() {
//  assert(Tok.is(tok::kw_hasname) && "Expected hasname token");
//  SourceLocation KeyLoc = ConsumeToken();
//
//  BalancedDelimiterTracker T(*this, tok::l_paren);
//  if (T.expectAndConsume(diag::err_expected_lparen_after, "idexpr"))
//    return ExprError();
//
//  ExprResult E = ParseConstantExpression();
//  if (E.isInvalid())
//    return ExprError();
//  ExpectAndConsume(tok::comma);
//  CXXScopeSpec SS;
//  ParsedType PT;
//  SourceLocation TempLoc;
//  UnqualifiedId I;
//  if (ParseUnqualifiedId(SS, false, true, true, false, PT, TempLoc, I))
//    return ExprError();
//  if (T.consumeClose())
//    return ExprError();
//
//  return Actions.ActOnHasNameExpr(KeyLoc, E.get(), I, T.getCloseLocation());
//}
////END

/// Parse a reflection type specifier.
///
/// \verbatim
///   reflection-type-specifier
///     'typename' '(' constant-expression ')'
/// \endverbatim
///
/// The constant expression must be a reflection of a type.
TypeResult Parser::ParseTypeReflectionSpecifier(SourceLocation TypenameLoc,
                                                SourceLocation &EndLoc) {
  BalancedDelimiterTracker T(*this, tok::l_paren);
  if (T.expectAndConsume(diag::err_expected_lparen_after, "reflexpr"))
    return TypeResult(true);
  ExprResult Result = ParseConstantExpression();
  if (!T.consumeClose()) {
    EndLoc = T.getCloseLocation();
    if (!Result.isInvalid())
      return Actions.ActOnTypeReflectionSpecifier(TypenameLoc, Result.get());
  }
  return TypeResult(true);
}


//static ReflectionTrait ReflectionTraitKind(tok::TokenKind kind) {
//  switch (kind) {
//  default:
//    llvm_unreachable("Not a known type trait");
//#define REFLECTION_TRAIT_1(Spelling, K)                                        \
//  case tok::kw_##Spelling:                                                     \
//    return URT_##K;
//#define REFLECTION_TRAIT_2(Spelling, K)                                        \
//  case tok::kw_##Spelling:                                                     \
//    return BRT_##K;
//#define REFLECTION_TRAIT_3(Spelling, K)                                        \
//  case tok::kw_##Spelling:                                                     \
//    return TRT_##K;
//#include "clang/Basic/TokenKinds.def"
//  }
//}
//
//static unsigned ReflectionTraitArity(tok::TokenKind kind) {
//  switch (kind) {
//  default:
//    llvm_unreachable("Not a known type trait");
//#define REFLECTION_TRAIT(N, Spelling, K)                                       \
//  case tok::kw_##Spelling:                                                     \
//    return N;
//#include "clang/Basic/TokenKinds.def"
//  }
//}

////ASUTTON DOC for ParseReflectionTrait (DWR commented out)
/// \brief Parse a reflection trait.
///
/// \verbatim
///   primary-expression:
///     unary-reflection-trait '(' expression ')'
///     binary-reflection-trait '(' expression ',' expression ')'
///
///   unary-reflection-trait:
///     '__reflect_unqualified_name'
///     '__reflect_qualified_name'
///     '__reflect_type'
///     '__reflect_traits'
///     '__reflect_specifiers'
///     '__reflect_pointer'
///     '__reflect_value'
///     '__reflect_value_expr'
///     '__reflect_num_parameters'
///     '__reflect_declaration_context'
///     '__reflect_lexical_context'
///     '__reflect_num_members'
///
///   binary-reflection-trait:
///     '__reflect_parameter'
///     '__reflect_member'
///     '__modify_access'
///     '__modify_virtual'
///     '__modify_constexpr'
/// \endverbatim
////END ASUTTON DOC


//DWR DOC:
/// \brief Parse a reflection trait.
///
/// Simply parses the argument expressions and passes them along, along
/// with the ReflectionTraitKind, to ActOnReflectionTrait.
///
/// The structure of the args:
///   Args[0]:    ReflectionValue as integer
///   Args[1]:    ReflectionObjKind as integer
///   Args[2]:    MemNum as integer
///   Args[3...]: any arguments accepted by the method denoted
///               by MemNum in the class denoted by ReflectionObjKind.
///
/// \verbatim
///   primary-expression:
///     __reflect_prop '(' constant-expression ',' constant-expression ',' constant-expression
//                         ',' constant-expression-list[opt] ')'
//    //TODO other __reflect_X stuff
/// \endverbatim
///
///
ExprResult Parser::ParseReflectionTrait(ReflectionTraitKind rtk) {
//  tok::TokenKind Kind = Tok.getKind();
  SourceLocation Loc = ConsumeToken();

  // Parse any number of arguments in parens.
  BalancedDelimiterTracker Parens(*this, tok::l_paren);
  if (Parens.expectAndConsume())
    return ExprError();
  SmallVector<Expr *, 8> Args;
//  bool HasNonIntArgs = false;
  do {
    ExprResult Expr = ParseConstantExpression();
    //DWR ADDN:
    if (Tok.is(tok::ellipsis))
      Expr = Actions.ActOnPackExpansion(Expr.get(), ConsumeToken());
      //^ DWR: I believe/hope/expect this sets things up so that
      // TreeTransform automatically handles unpacking parameter packs
      // during each successive transformation/template instantiation
//    else //for NON-param pack args, see if its an integer type
//      HasNonIntArgs = HasNonIntArgs || !Expr.get()->getType()->isIntegerType();
    //END
    if (Expr.isInvalid()) {
      Parens.skipToEnd();
      return ExprError();
    }
    Args.push_back(Expr.get());

  } while (TryConsumeToken(tok::comma));
  if (Parens.consumeClose())
    return ExprError();
  SourceLocation EndLoc = Parens.getCloseLocation();

  return Actions.ActOnReflectionTrait(Loc,
                                      /*ReflTraitKind=*/rtk,
                                      Args,
                                      EndLoc
//                                      , HasNonIntArgs
                                      );
}

//DWR ADDN:
/// DWR TODO DOC
ExprResult Parser::ParseReflectNew() {
  SourceLocation Loc = ConsumeToken();

  BalancedDelimiterTracker Parens(*this, tok::l_paren);
  if (Parens.expectAndConsume())
    return ExprError();
  SmallVector<Expr *, 2> Args;
  do {
    ExprResult Expr = ParseConstantExpression();
    if (Tok.is(tok::ellipsis))
      Expr = Actions.ActOnPackExpansion(Expr.get(), ConsumeToken());
    if (Expr.isInvalid()) {
      Parens.skipToEnd();
      return ExprError();
    }
    Args.push_back(Expr.get());
  } while (TryConsumeToken(tok::comma));
  if (Parens.consumeClose())
    return ExprError();

  SourceLocation EndLoc = Parens.getCloseLocation();

  return Actions.ActOnReflectNew(Loc,
                                 /*ObjKind=RK_none=*/0,
                                 Args, EndLoc);
}

/// DWR TODO DOC
// Note this is exact clone of ParseReflectNew, for now
ExprResult Parser::ParseReflectDelete() {
  SourceLocation Loc = ConsumeToken();

  BalancedDelimiterTracker Parens(*this, tok::l_paren);
  if (Parens.expectAndConsume())
    return ExprError();
  SmallVector<Expr *, 4> Args;
  do {
    ExprResult Expr = ParseConstantExpression();
    if (Tok.is(tok::ellipsis))
      Expr = Actions.ActOnPackExpansion(Expr.get(), ConsumeToken());
    if (Expr.isInvalid()) {
      Parens.skipToEnd();
      return ExprError();
    }
    Args.push_back(Expr.get());
  } while (TryConsumeToken(tok::comma));
  if (Parens.consumeClose())
    return ExprError();

  SourceLocation EndLoc = Parens.getCloseLocation();

  return Actions.ActOnReflectDelete(Loc,
                                    /*ObjKind=RK_none=*/0,
                                    Args, EndLoc);
}
//END

//ASUTTON ADDN / DWR MOD (added MessageKind param, added SourceLocation args etc.)
ExprResult Parser::ParseCompilerMessageExpression() {
  assert(Tok.is(tok::kw___compiler_debug) && "Not '__compiler_debug'");
  SourceLocation KWLoc = ConsumeToken();
  BalancedDelimiterTracker T(*this, tok::l_paren);

  if (T.expectAndConsume(diag::err_expected_lparen_after, "__compiler_error/warning/debug"))
    return ExprError();

  ExprResult MessageExpr = ParseConstantExpression();

  if (MessageExpr.isInvalid()) {
    SkipUntil(tok::r_paren, StopAtSemi);
    return ExprError();
  }

  if (T.consumeClose())
    return ExprError();

  return Actions.ActOnCompilerMessageExpr(KWLoc, MessageExpr.get(), T.getCloseLocation());
}

ExprResult Parser::ParseCompilerDiagnosticExpression() {
  assert(Tok.is(tok::kw___compiler_diag) && "Not '__compiler_diag'");
  SourceLocation KWLoc = ConsumeToken();
  BalancedDelimiterTracker Parens(*this, tok::l_paren);

  if (Parens.expectAndConsume(diag::err_expected_lparen_after, "__compiler_diag"))
    return ExprError();

  SmallVector<Expr *, 4> Args;
  do {
    ExprResult Expr = ParseConstantExpression();
    if (Tok.is(tok::ellipsis))
      Expr = Actions.ActOnPackExpansion(Expr.get(), ConsumeToken());
    if (Expr.isInvalid()) {
      Parens.skipToEnd();
      return ExprError();
    }
    Args.push_back(Expr.get());
  } while (TryConsumeToken(tok::comma));
  if (Parens.consumeClose())
    return ExprError();
  SourceLocation EndLoc = Parens.getCloseLocation();

  return Actions.ActOnCompilerDiagnosticExpr(KWLoc, Args, EndLoc);
}
//END

////ASUTTON ADDN:
///// \brief Replace the current identifier token (and possibly the C++ scope
///// specifier that precedes it) with a C++ metaclass-name annotation token.
/////
///// \param SS         If non-null, the C++ scope specifier that qualifies the
/////                   metaclass-name and was extracted from the preceding scope
/////                   annotation token.
///// \param Metaclass  The C++ metaclass declaration that corresponds to the
/////                   metaclass-name.
//void Parser::AnnotateMetaclassName(CXXScopeSpec *SS, Decl *Metaclass) {
//  assert(getLangOpts().Reflection &&
//         "Can only annotate metaclass-names when C++ reflection is enabled");
//  assert(Tok.is(tok::identifier));
//
//  // Replace the current token with an annotation token.
//  Tok.setKind(tok::annot_metaclass);
//  Tok.setAnnotationValue(Metaclass);
//  Tok.setAnnotationEndLoc(Tok.getLocation());
//  if (SS && SS->isNotEmpty()) // C++ qualified metaclass-name.
//    Tok.setLocation(SS->getBeginLoc());
//
//  // Update any cached tokens.
//  PP.AnnotateCachedTokens(Tok);
//}
//
///// Parse a C++ metaclass definition.
/////
///// \verbatim
/////   metaclass-name:
/////     identifier
/////
/////   metaclass-definition:
/////     metaclass-head '{' member-specification[opt] '}'
/////
/////   metaclass-head:
/////     '$class' metaclass-name metaclass-base-clause[opt]
///// \endverbatim
//Parser::DeclGroupPtrTy Parser::ParseMetaclassDefinition() {
//  assert(Tok.is(tok::dollar));
//  SourceLocation DLoc = ConsumeToken();
//
//  // For now, pretend we are defining a class that was declared with the
//  // 'struct' class-key.
//  DeclSpec::TST TagType = DeclSpec::TST_struct;
//  assert(Tok.is(tok::kw_class)); // TODO: Support for '$struct' and '$union'?
//  ConsumeToken();
//
//  // Allow GNU and C++11 attributes after '$class'.
//  ParsedAttributesWithRange Attrs(AttrFactory);
//  SourceLocation AttrFixItLoc = Tok.getLocation();
//  MaybeParseGNUAttributes(Attrs);
//  MaybeParseCXX11Attributes(Attrs);
//
//  // Parse the metaclass name.
//  assert(Tok.is(tok::identifier));
//  IdentifierInfo *II = Tok.getIdentifierInfo();
//  SourceLocation IdLoc = ConsumeToken();
//
//  if (Tok.isNot(tok::colon) && Tok.isNot(tok::l_brace)) {
//    Diag(Tok, diag::err_expected_either) << tok::colon << tok::l_brace;
//    return nullptr;
//  }
//
//  Decl *Metaclass = Actions.ActOnMetaclass(getCurScope(), DLoc, IdLoc, II);
//  CXXRecordDecl *MetaclassDef = nullptr;
//
//  // Enter a scope for the metaclass.
//  ParseScope MetaclassScope(this, Scope::DeclScope);
//
//  // Increase the tempalte
//  TemplateParameterDepthRAII TemplateDepthTracker(TemplateParameterDepth);
//  ++TemplateDepthTracker;
//
//  Actions.ActOnMetaclassStartDefinition(getCurScope(), Metaclass, Attrs,
//                                        MetaclassDef,
//                                        TemplateDepthTracker.getDepth());
//
//  PrettyDeclStackTraceEntry CrashInfo(Actions, Metaclass, DLoc,
//                                      "parsing metaclass body");
//
//  // Parse the body of the metaclass.
//  {
//    Decl *MetaclassDefTmp = static_cast<Decl *>(MetaclassDef);
//    ParseCXXMemberSpecification(DLoc, AttrFixItLoc, Attrs, TagType,
//                                MetaclassDefTmp);
//    MetaclassDef = cast<CXXRecordDecl>(MetaclassDefTmp);
//  }
//
//  if (MetaclassDef->isInvalidDecl()) {
//    Actions.ActOnMetaclassDefinitionError(getCurScope(), Metaclass);
//    return nullptr;
//  }
//
//  Actions.ActOnMetaclassFinishDefinition(getCurScope(), Metaclass,
//                                         MetaclassDef->getBraceRange());
//
//  return Actions.ConvertDeclToDeclGroup(Metaclass);
//}
//
///// Parse a C++ metaclass-base-specifier.
/////
///// Note that we only check that the result names a type; semantic analysis will
///// need to verify that the type names a class. The result is either a type or
///// null, depending on whether a type name was found.
/////
///// \verbatim
/////   metaclass-base-clause:
/////     ':' metaclass-base-specifier-list
/////
/////   metaclass-base-specifier-list:
/////     metaclass-base-specifier
/////     metaclass-base-specifier ',' metaclass-base-specifier
/////
/////   metaclass-base-specifier:
/////     nested-name-specifier[opt] metaclass-name
///// \endverbatim
//TypeResult Parser::ParseMetaclassBaseSpecifier(SourceLocation &BaseLoc,
//                                               SourceLocation &EndLocation) {
//  // Parse optional nested-name-specifier.
//  CXXScopeSpec SS;
//  ParseOptionalCXXScopeSpecifier(SS, nullptr, /*EnteringContext=*/false);
//
//  BaseLoc = Tok.getLocation();
//
//  if (Tok.isNot(tok::identifier)) {
//    Diag(Tok, diag::err_expected_class_name);
//    return true;
//  }
//
//  IdentifierInfo *Id = Tok.getIdentifierInfo();
//  SourceLocation IdLoc = ConsumeToken();
//
//  // We have an identifier; check whether it is actually is a metaclass.
//  IdentifierInfo *CorrectedII = nullptr;
//  ParsedType Type =
//      Actions.getMetaclassName(*Id, IdLoc, getCurScope(), &SS,
//                               /*NonTrivialTypeSourceInfo=*/true, &CorrectedII);
//
//  if (!Type) {
//    Diag(IdLoc, diag::err_expected_class_name);
//    return true;
//  }
//
//  // Consume the identifier.
//  EndLocation = IdLoc;
//
//  // Fake up a Declarator to use with ActOnTypeName.
//  DeclSpec DS(AttrFactory);
//  DS.SetRangeStart(IdLoc);
//  DS.SetRangeEnd(EndLocation);
//  DS.getTypeSpecScope() = SS;
//
//  const char *PrevSpec = nullptr;
//  unsigned DiagID;
//  DS.SetTypeSpecType(TST_typename, IdLoc, PrevSpec, DiagID, Type,
//                     Actions.getASTContext().getPrintingPolicy());
//
//  Declarator DeclaratorInfo(DS, DeclaratorContext::TypeNameContext); //DWR changed from Declarator::TypeNameContext
//  return Actions.ActOnTypeName(getCurScope(), DeclaratorInfo);
//}
//
////END
//ASUTTON ADDN:
/// Parse a constexpr-declaration.
///
/// \verbatim
///   constexpr-declaration:
///     'constexpr' compound-statement
/// \endverbatim
Parser::DeclGroupPtrTy Parser::ParseConstexprDeclaration(CDContextVars &cdcv, bool Nested) {
  assert(getLangOpts().CPlusPlus17 &&
         "Can only parse constexpr declarations in C++1z"); //DWR FIXME is this necessary?
  assert(Tok.is(tok::kw_constexpr));

  SourceLocation ConstexprLoc = ConsumeToken();

  if (!Tok.is(tok::l_brace)) {
    Diag(Tok, diag::err_expected) << tok::l_brace;
    return nullptr;
  }

  METAPARSE_DEBUG_HDR("Parsing constexpr decl...")

  unsigned ScopeFlags;
  Decl *D = Actions.ActOnConstexprDecl(getCurScope(), ConstexprLoc, ScopeFlags, cdcv, Nested);

  // Enter a scope for the constexpr declaration body.
  ParseScope BodyScope(this, ScopeFlags);

  Actions.ActOnStartConstexprDecl(getCurScope(), D);

  PrettyDeclStackTraceEntry CrashInfo(Actions.Context, D, ConstexprLoc, //DWR NOTE: added .Context
                                      "parsing constexpr declaration body");

  // Parse the body of the constexpr declaration.
  StmtResult Body(ParseCompoundStatementBody());

  if (!Body.isInvalid())
    Actions.ActOnFinishConstexprDecl(getCurScope(), D, Body.get());
  else
    Actions.ActOnConstexprDeclError(getCurScope(), D);

  return Actions.ConvertDeclToDeclGroup(D);
}
//END
//ASUTTON ADDN, DWR MOD:
/// \brief Parse a concatenation expression.
///
///   primary-expression:
///      '__concatenate' '(' constant-argument-list ')'
///
/// Each argument in the constant-argument-list must be a constant expression.
///
/// Returns true if parsing or semantic analysis fail.
ExprResult Parser::ParseCXXConcatenateExpression() {
  assert(Tok.is(tok::kw___concatenate));
  SourceLocation KeyLoc = ConsumeToken();

  BalancedDelimiterTracker T(*this, tok::l_paren);
  if (T.expectAndConsume(diag::err_expected_lparen_after, "__concatenate"))
    return true;

  SmallVector<Expr *, 4> Parts;
  while (true) {
    ExprResult Result = ParseConstantExpression();
    //DWR ADDN:
    if (Tok.is(tok::ellipsis))
      Result = Actions.ActOnPackExpansion(Result.get(), ConsumeToken());
    //END
    if (Result.isInvalid()) {
      T.skipToEnd(); //DWR ADDN
      return ExprError();
    }
    Parts.push_back(Result.get());
    if (Tok.is(tok::r_paren))
      break;
    if (ExpectAndConsume(tok::comma))
      return ExprError();
  }
  if (T.consumeClose())
    return ExprError();

  return Actions.ActOnCXXConcatenateExpr(Parts, KeyLoc,
                                         T.getOpenLocation(),
                                         T.getCloseLocation());
}
//END

//END ASUTTON ADDN
