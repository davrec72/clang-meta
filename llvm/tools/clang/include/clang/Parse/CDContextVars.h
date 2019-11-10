//DWR ADDN:
//===- CDContextVars.h ------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Defines CDContextVars, a discriminated union for storing variables needed
// when doing extra parsing while evaluating constexpr decls
//
//===----------------------------------------------------------------------===//


#ifndef LLVM_CLANG_PARSE_CDCONTEXTVARS_H_
#define LLVM_CLANG_PARSE_CDCONTEXTVARS_H_

#include "clang/Sema/ParsedAttr.h"
#include "clang/Basic/Specifiers.h" //for AccessSpecifier
#include "RAIIObjectsForParser.h"

namespace clang {

struct CDContextVars {
  union {
    struct {
      ParsedAttributesWithRange &attrs;
      ParsingDeclSpec *DS;
    } extdecl;
    struct {
      ParsedAttributesWithRange &AccessAttrs;
      AccessSpecifier AS;
    } cls;
    struct {
      //nothing for now
    } fcn;
  };
  enum {
    CDCV_extdecl,
    CDCV_cls,
    CDCV_fcn
  } kind;
  CDContextVars(ParsedAttributesWithRange &attrs, ParsingDeclSpec *DS)
    : extdecl{attrs, DS}, kind(CDCV_extdecl)
  {}
  CDContextVars(ParsedAttributesWithRange &AccessAttrs, AccessSpecifier AS)
    : cls{AccessAttrs, AS}, kind(CDCV_cls)
  {}
  CDContextVars()
    : fcn{}, kind(CDCV_fcn)
  {}
};

} //namespace clang



#endif //LLVM_CLANG_PARSE_CDCONTEXTVARS_H_
//END
