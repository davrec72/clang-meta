set(LLVM_LINK_COMPONENTS
  BitReader
  Support
  )

# DWR ADDN: add headers etc. so they are visible when imported into QT Creator
file(GLOB CLANG_SERIALIZATION_INCLUDES
  "${CLANG_SOURCE_DIR}/include/clang/Serialization/*.hpp"
  "${CLANG_SOURCE_DIR}/include/clang/Serialization/*.h"
  "${CLANG_SOURCE_DIR}/include/clang/Serialization/*.def"
  "${CLANG_SOURCE_DIR}/include/clang/Serialization/*.td"
  )

add_clang_library(clangSerialization
  ${CLANG_SERIALIZATION_INCLUDES} #DWR ADDN

  ASTCommon.cpp
  ASTReader.cpp
  ASTReaderDecl.cpp
  ASTReaderStmt.cpp
  ASTWriter.cpp
  ASTWriterDecl.cpp
  ASTWriterStmt.cpp
  GeneratePCH.cpp
  GlobalModuleIndex.cpp
  Module.cpp
  ModuleFileExtension.cpp
  ModuleManager.cpp

  ADDITIONAL_HEADERS
  ASTCommon.h
  ASTReaderInternals.h

  LINK_LIBS
  clangAST
  clangBasic
  clangLex
  clangSema
  )
