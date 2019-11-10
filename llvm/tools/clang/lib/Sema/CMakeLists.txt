set(LLVM_LINK_COMPONENTS
        MC #DWR ADDN (since the Parse cpps need them -- see note below)
        MCParser #DWR ADDN ""
        Support
        )

if (MSVC)
  set_source_files_properties(SemaDeclAttr.cpp PROPERTIES COMPILE_FLAGS /bigobj)
  set_source_files_properties(SemaExpr.cpp PROPERTIES COMPILE_FLAGS /bigobj)
  set_source_files_properties(SemaTemplate.cpp PROPERTIES COMPILE_FLAGS /bigobj)
endif()

# DWR ADDN: add headers etc. so they are visible when imported into QT Creator
file(GLOB CLANG_SEMA_INCLUDES
  "${CLANG_SOURCE_DIR}/include/clang/Sema/*.hpp"
  "${CLANG_SOURCE_DIR}/include/clang/Sema/*.h"
  "${CLANG_SOURCE_DIR}/include/clang/Sema/*.def"
  "${CLANG_SOURCE_DIR}/include/clang/Sema/*.td"
  )

add_clang_library(clangSema
  ${CLANG_SEMA_INCLUDES} #DWR ADDN

  #DWR ADDN: All the Parse cpps (EXCEPT ParserStaticVarsInit.cpp),
  # so that ParseTopLevelDecl can be called from Sema::ApplyEffects
  #(We could just link clangParse in LINK_LIBS below, but that would cause a cyclic dependency that
  # only works for static libraries, and
  # for debugging purposes it's nice to be able to use dynamic libs.
  #DWR TODO are there more of these you could leave out?  Or other ways to decrease the Sema->Parser dependency?
  # Exhaust all alternatives...
  ../Parse/ParseAST.cpp
  ../Parse/ParseCXXInlineMethods.cpp
  ../Parse/ParseDecl.cpp
  ../Parse/ParseDeclCXX.cpp
        ../Parse/ParseMetaparse.cpp #DWR ADDN
  ../Parse/ParseExpr.cpp
  ../Parse/ParseExprCXX.cpp
  ../Parse/ParseInit.cpp
  ../Parse/ParseInject.cpp #ASUTTON ADDN
  ../Parse/ParseObjc.cpp
  ../Parse/ParseOpenMP.cpp
  ../Parse/ParsePragma.cpp
  ../Parse/ParserBrickWallRAII.cpp #DWR ADDN
  ../Parse/ParseReflect.cpp #ASUTTON ADDN
  ../Parse/ParseStmt.cpp
  ../Parse/ParseStmtAsm.cpp
  ../Parse/ParseTemplate.cpp
  ../Parse/ParseTentative.cpp
  ../Parse/Parser.cpp

  AnalysisBasedWarnings.cpp
  CodeCompleteConsumer.cpp
  DeclSpec.cpp
  DelayedDiagnostic.cpp
  IdentifierResolver.cpp
  JumpDiagnostics.cpp
  MultiplexExternalSemaSource.cpp
  ParsedAttr.cpp
  Scope.cpp
  ScopeInfo.cpp
  Sema.cpp
  SemaAccess.cpp
  SemaAttr.cpp
  SemaCXXScopeSpec.cpp
  SemaCast.cpp
  SemaChecking.cpp
  SemaCodeComplete.cpp
  SemaConsumer.cpp
  SemaCoroutine.cpp
  SemaCUDA.cpp
  SemaDecl.cpp
  SemaDeclAttr.cpp
  SemaDeclCXX.cpp
  SemaDeclObjC.cpp
  SemaExceptionSpec.cpp
  SemaMetaparse.cpp #DWR ADDN
  SemaExpr.cpp
  SemaExprCXX.cpp
  SemaExprMember.cpp
  SemaExprObjC.cpp
  SemaFixItUtils.cpp
  SemaInit.cpp
  SemaInject.cpp #ASUTTON ADDN
  SemaLambda.cpp
  SemaLookup.cpp
  SemaObjCProperty.cpp
  SemaOpenMP.cpp
  SemaOverload.cpp
  SemaPseudoObject.cpp
  SemaReflect.cpp #ASUTTON ADDN
  SemaReflectIncs.cpp #DWR ADDN
  SemaStmt.cpp
  SemaStmtAsm.cpp
  SemaStmtAttr.cpp
  SemaTemplate.cpp
  SemaTemplateDeduction.cpp
  SemaTemplateInstantiate.cpp
  SemaTemplateInstantiateDecl.cpp
  SemaTemplateVariadic.cpp
  SemaType.cpp
  TypeLocBuilder.cpp

  LINK_LIBS
  clangAST
  clangAnalysis
  clangBasic
  clangEdit
  clangLex
  LLVMCore #DWR ADDN for use of IR/Type.cpp implems in reflection
  )
