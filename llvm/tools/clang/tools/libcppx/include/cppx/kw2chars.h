//
// Created by David Rector on 2019-03-17.
//

#ifndef LLVM_KW2CHARS_H
#define LLVM_KW2CHARS_H

#include "traits.h" //for access_kind, storage_kind, and linkage_kind defs

namespace cppx {
namespace meta {
inline namespace v1 {

  //Enums

  constexpr const char * kw2str(access_kind A) {
    if (A==AK_public)
      return "public ";
    else if (A==AK_private)
      return "private ";
    else if (A==AK_protected)
      return "protected ";
    else
      return "";
  }

  constexpr const char * kw2str(storage_kind S) {
    if (S==SK_thread_local)
      return "thread_local ";
//    else if (S==automatic_storage)
//      return "register";
//    else if (S==static_storage)
//      return "";
    else
      return "";
  }

  constexpr const char * kw2str(linkage_kind L) {
    if (L==LK_internal)
      return "static ";
    else if (L==LK_external)
      return "extern ";
    else
      return "";
  }

  namespace bool2str {

    // Non-final keywords

    /*===================================*/
    /// @name PP_BOOL2KWSTR(keyname)
    /// @example
    ///  PP_BOOL2KWSTR(virtual)
    ///  PP_BOOL2KWSTR(inline)
    ///   __queue_metaparse virtual_<true>();  //<==> __queue_metaparse "virtual ";
    ///   __queue_metaparse virtual_<false>(); //<==> __queue_metaparse "";
    /*-----------------------------------*/
#define PP_BOOL2KWSTR(keyname)               \
    constexpr const char * keyname ## _(bool b) {    \
      if (b)                              \
        return #keyname " ";              \
      else                                \
        return "";                        \
    }                                     \
  /*-------------------------------------*/
    PP_BOOL2KWSTR(explicit)
    PP_BOOL2KWSTR(const)
    PP_BOOL2KWSTR(virtual)
    PP_BOOL2KWSTR(final)
    PP_BOOL2KWSTR(override)
    PP_BOOL2KWSTR(noexcept)
    PP_BOOL2KWSTR(inline)
    PP_BOOL2KWSTR(constexpr)
#undef PP_BOOL2KWSTR
/*===================================*/

    //Final keywords

    constexpr const char * deleted_(bool b) {
      if (b)
        return "= delete;";
      else
        return "";
    }

    constexpr const char * defaulted_(bool b) {
      if (b)
        return "= default;";
      else
        return "";
    }

    constexpr const char * pure_virtual_(bool b) {
      if (b)
        return "= 0;";
      else
        return "";
    }

  } //kw2chars

} //c1
} //meta
} //cppx



#endif //LLVM_KW2CHARS_H
