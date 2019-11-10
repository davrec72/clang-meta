set(LLVM_LINK_COMPONENTS
  MC
  MCParser
  Support
  )

# DWR ADDN: add headers etc. so they are visible when imported into QT Creator
file(GLOB CLANG_PARSE_INCLUDES
  "${CLANG_SOURCE_DIR}/include/clang/Parse/*.hpp"
  "${CLANG_SOURCE_DIR}/include/clang/Parse/*.h"
  "${CLANG_SOURCE_DIR}/include/clang/Parse/*.def"
  "${CLANG_SOURCE_DIR}/include/clang/Parse/*.td"
  )

add_clang_library(clangParse
  ${CLANG_PARSE_INCLUDES} #DWR ADDN

  ParseAST.cpp
  ParseCXXInlineMethods.cpp
  ParseDecl.cpp
  ParseDeclCXX.cpp
  ParseMetaparse.cpp #DWR ADDN
  ParseExpr.cpp
  ParseExprCXX.cpp
  ParseInit.cpp
  ParseInject.cpp #ASUTTON ADDN
  ParseObjc.cpp
  ParseOpenMP.cpp
  ParsePragma.cpp
  ParserBrickWallRAII.cpp #DWR ADDN
  ParseReflect.cpp #ASUTTON ADDN
  ParseStmt.cpp
  ParseStmtAsm.cpp
  ParseTemplate.cpp
  ParseTentative.cpp
  Parser.cpp


  LINK_LIBS
  clangAST
  clangBasic
  clangLex
  clangSema
  )
