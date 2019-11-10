# TODO: Add -maltivec when ARCH is PowerPC.

set(LLVM_LINK_COMPONENTS support)

# DWR ADDN: add headers etc. so they are visible when imported into QT Creator
file(GLOB CLANG_LEX_INCLUDES
  "${CLANG_SOURCE_DIR}/include/clang/Lex/*.hpp"
  "${CLANG_SOURCE_DIR}/include/clang/Lex/*.h"
  "${CLANG_SOURCE_DIR}/include/clang/Lex/*.def"
  "${CLANG_SOURCE_DIR}/include/clang/Lex/*.td"
  )
# END

add_clang_library(clangLex
  ${CLANG_LEX_INCLUDES} # DWR ADDN

  HeaderMap.cpp
  HeaderSearch.cpp
  Lexer.cpp
  LiteralSupport.cpp
  MacroArgs.cpp
  MacroInfo.cpp
  MetaparseDebug.cpp #DWR ADDN
  ModuleMap.cpp
  PPCaching.cpp
  PPCallbacks.cpp
  PPConditionalDirectiveRecord.cpp
  PPDirectives.cpp
  PPDebug.cpp #DWR ADDN
  PPExpressions.cpp
  PPLexerChange.cpp
  PPMacroExpansion.cpp
  PTHLexer.cpp
  Pragma.cpp
  PreprocessingRecord.cpp
  Preprocessor.cpp
  PreprocessorLexer.cpp
  ScratchBuffer.cpp
  StaticVarsInit.cpp #DWR ADDN
  TokenConcatenation.cpp
  TokenLexer.cpp


  LINK_LIBS
  clangBasic
  )
