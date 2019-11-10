//===- ReflectionIncludes.h - Combines all the headers containing the
//     definitions of classes we wish to reflect--*- C++ -*-====//
//
//                     The LLVM Compiler Infrastructure
//
//===----------------------------------------------------------------------===//
//
//  This file defines the included reflection classes,
//  and the current build's reflection header ID.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_REFLECTIONINCLUDES_H
#define LLVM_CLANG_AST_REFLECTIONINCLUDES_H

#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclFriend.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclOpenMP.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprObjC.h"
#include "clang/AST/ExprOpenMP.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/StmtObjC.h"
#include "clang/AST/StmtOpenMP.h"
#include "clang/AST/Type.h"
//#include "clang/AST/TypeLoc.h"

#include "clang/AST/DeclLookups.h" //DeclContext::all_lookups_iterator
#include "clang/AST/DependentDiagnostic.h" //DeclContext::ddiag_iterator

#include "clang/Basic/ClientReflContainers.h" //all the user-construcible containers in here

#endif //LLVM_CLANG_AST_REFLECTIONINCLUDES_H
