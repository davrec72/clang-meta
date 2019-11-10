//
// Created by David Rector on 2019-02-27.
//

#ifndef LLVM_METAPARSEDEBUG_H
#define LLVM_METAPARSEDEBUG_H

#include <string>
#include <llvm/Support/raw_ostream.h>
#include "Token.h"
#include "clang/Basic/IdentifierTable.h" //for IdentifierInfo

namespace clang {

#define NMETAPARSEDEBUG //comment OUT to turn ON [DWR] debug messages regarding meta

//NDEBUG implies NMETAPARSEDEBUG:
#if defined(NDEBUG) && !defined(NMETAPARSEDEBUG)
#  define NMETAPARSEDEBUG
#endif

#ifndef NMETAPARSEDEBUG
  static unsigned METAPARSE_DEBUG_curindent;

  struct METAPARSE_DEBUG_curindent_RAII {
    static unsigned indent;

    METAPARSE_DEBUG_curindent_RAII() {
      ++indent;
    }

    ~METAPARSE_DEBUG_curindent_RAII() {
      --indent;
    }
  };

  inline void METAPARSE_DEBUG_repeat2llvmouts(const char *strlit, int num) {
    for (auto i = 0; i != num; ++i) {
      llvm::outs() << strlit;
    }
  }
  inline std::string tokIDstr(const clang::Token &t) {
    if (t.is(tok::raw_identifier)) {
      return std::string("RawID:") + t.getRawIdentifier().data();
    } else if (t.is(tok::identifier)) {
      return std::string("ID:") + t.getIdentifierInfo()->getNameStart();
    } else {
      return t.getName();
    }
  }

  //Helper function (FIXME probably would be better to just add an equality comparison operator in Token, then the vector comparison operation would be defined automatically
  template<typename T, typename U>
  bool TokenVecsEqual(T &t, U &u) {
    bool res = (t.size() == u.size());
    for (auto ttokit = t.begin(), utokit = u.begin();
         res && (ttokit != t.end());
         ++ttokit, ++utokit) {
      res = (ttokit->getName() == utokit->getName());
    }
    return res;
  }


#   define METAPARSE_DEBUG_assert(...) assert(__VA_ARGS__)

  /// @example
  ///     METAPARSE_DEBUG(0, "My Message header")
  ///     METAPARSE_DEBUG(1, "Value of var: " << var)
#   define METAPARSE_DEBUG(...) \
        if (!METAPARSE_DEBUG_curindent_RAII::indent) \
          llvm::outs() << " \n"; \
        METAPARSE_DEBUG_FULL("[DWR] ", METAPARSE_DEBUG_curindent_RAII::indent, "\t", 1, __VA_ARGS__)
  /**/
#   define METAPARSE_DEBUG_HDR(...) \
      METAPARSE_DEBUG(__VA_ARGS__) \
      METAPARSE_DEBUG_curindent_RAII METAPARSE_DEBUG_savedindentstate;
  /**/
#   define else_METAPARSE_DEBUG(...) else { METAPARSE_DEBUG(__VA_ARGS__) }
#   define METAPARSE_DEBUG_NOLINEBREAK(...) \
        METAPARSE_DEBUG_FULL("[DWR] ", METAPARSE_DEBUG_curindent_RAII::indent, "\t", 0/*!*/, __VA_ARGS__)
/**/
#   define IF_METAPARSE_DEBUG(...) __VA_ARGS__
#   define METAPARSE_DEBUG_IFUNDERHDR(...) \
      if (METAPARSE_DEBUG_curindent_RAII::indent) { __VA_ARGS__ }
  /**/

#   define METAPARSE_DEBUG_VAR(var) METAPARSE_DEBUG(#var << ": " << var)

#ifndef METAPARSE_DEBUG_VEC_MAXELEMS
  #define METAPARSE_DEBUG_VEC_MAXELEMS 50
#endif

/// METAPARSE_DEBUG_VEC: e2str is an expression in terms of e (the elem) and, if desired, idx
#   define METAPARSE_DEBUG_VEC(vec, e2str, msg) \
  METAPARSE_DEBUG_NOLINEBREAK(msg << " " #vec << ", " << #e2str << ": [")\
  unsigned idx = 0;\
  for (auto& e : vec) {\
    if (idx >= METAPARSE_DEBUG_VEC_MAXELEMS) {\
      llvm::outs() << "...";\
      break;\
    }\
    llvm::outs() << (idx ? ", " : "") << e2str;\
    ++idx;\
  }\
  llvm::outs() << "]\n";
/**/

  //HELPER:
#   define METAPARSE_DEBUG_FULL(prefixstream, indentnum, indentsym, numlinebreaks, msgstream) \
    { \
      llvm::outs() << prefixstream; \
      METAPARSE_DEBUG_repeat2llvmouts(indentsym, indentnum); \
      llvm::outs()  << msgstream; \
      METAPARSE_DEBUG_repeat2llvmouts("\n", numlinebreaks); \
    }
  /**/

  template<typename TV>
  static void tokvec2llvmouts(TV &v) {
    METAPARSE_DEBUG_IFUNDERHDR(METAPARSE_DEBUG_NOLINEBREAK("[ "))
    bool first = true;
    for (auto t : v) {
      llvm::outs() << (first ? "" : ", ") << tokIDstr(t);
      first = false;
    }
    llvm::outs() << " ]\n";
  }


#else //NDEBUG defined (i.e. non-debug mode)
#   define METAPARSE_DEBUG_assert(...)
#   define METAPARSE_DEBUG(...)
#   define METAPARSE_DEBUG_NOLINEBREAK(...)
#   define METAPARSE_DEBUG_HDR(...)
#   define else_METAPARSE_DEBUG(...)
#   define METAPARSE_DEBUG_IFUNDERHDR(...)
#   define IF_METAPARSE_DEBUG(...)
#   define METAPARSE_DEBUG_VAR(...)
#   define METAPARSE_DEBUG_VEC(...)
#   define METAPARSE_DEBUG_FULL(...)
#endif //ndef NDEBUG

} //clang


#endif //LLVM_METAPARSEDEBUG_H
