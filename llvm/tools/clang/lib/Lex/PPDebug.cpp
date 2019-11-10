//
// Created by David Rector on 2019-03-02.
//

#include "clang/Lex/MetaparseDebug.h"
#include "clang/Lex/Preprocessor.h"

using namespace clang;

#ifndef NMETAPARSEDEBUG
void Preprocessor::debugDispCachedTokens(const char *msg) const {
  METAPARSE_DEBUG_NOLINEBREAK("PP.CachedTokens " << msg << ": [")
  unsigned i = 0;
  unsigned curbp = 0;
  for (auto t : CachedTokens) {
    bool isBacktrackPos = (isBacktrackEnabled() ? (BacktrackPositions[curbp] == i) : false);
    if (isBacktrackPos)
      ++curbp;
    auto tokstr = tokIDstr(t);
    llvm::outs() << (i ? ", " : "") << tokstr.c_str()
                 << (isBacktrackPos ? " [Backtrack Pos]"
                                    : (i == CachedLexPos ? " [CLP]" : ""));
    ++i;
  }
  llvm::outs() << (i ? ", " : "") << "(end)" << (i == CachedLexPos ? " [CLP]" : "") << " ]\n";
  assert(curbp == BacktrackPositions.size() && "Expected to account for all backtrack positions "
                                                    "in above debug messages");
}

void Preprocessor::debugDispLexerKinds(const char *msg) const {
  METAPARSE_DEBUG_VEC(IncludeMacroStack, e.CurLexerKind, msg)
  METAPARSE_DEBUG(" --CurPPLexer = " << CurPPLexer
               << ", CurTokenLexer = " << CurTokenLexer.get()
               << ", InCachingLexMode() = " << InCachingLexMode()
               << ", CurLexerKind = " << CurLexerKind
               << ", getTotalNumLexers() = " << getTotalNumLexers()
               << ", IncludeMacroStack.size() = " << IncludeMacroStack.size())
}

#endif