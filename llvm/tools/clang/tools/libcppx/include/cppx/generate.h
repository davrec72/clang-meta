//
// Created by David Rector on 2019-04-07.
//
// This file intends to provide an interface for code-generation
// that encapsulates all conceivable usages of __queue_metaparse,
// such that if __queue_metaparse is changed or becomes deprecated only
// this file need be updated.

#ifndef UNTITLED4_GENERATE_H
#define UNTITLED4_GENERATE_H

#include <cppx/kw2chars.h>
#include <cppx/traits.h>

namespace cppx {
  namespace meta {
    inline namespace v2 {
      namespace gen {

        constexpr void access_spec(access_kind A) {
          __queue_metaparse kw2str(A);
          __queue_metaparse ":";
        }

        constexpr void open_scope() {
          __queue_metaparse "{";
        }
        constexpr void close_scope() {
          __queue_metaparse "}";
        }

        constexpr void namespace_decl(bool is_inline, const char *name) {
          if (is_inline)
            __queue_metaparse "inline namespace";
          else
            __queue_metaparse "namespace";
          __queue_metaparse name;
        }

        namespace detail {
          constexpr void inline_constexpr(bool _inline, bool _constexpr) {
            if (_inline)
              __queue_metaparse "inline";
            if (_constexpr)
              __queue_metaparse "constexpr";
          }
          constexpr void linkage(linkage_kind l) {
            __queue_metaparse kw2str(l);
          }
          constexpr void storage(storage_kind s) {
            __queue_metaparse kw2str(s);
          }
          constexpr void variable_prequals(variable_traits traits) {
            inline_constexpr(traits.is_inline, traits.is_constexpr);
            linkage(traits.linkage);
            storage(traits.storage);
          }
        }


        template<typename T>
        constexpr void list(const T& t) {
          bool first = true;
          for... (auto m : t) {
            if (first)
              first = false;
            else
              __queue_metaparse ",";
            m.generate();
          }
        }

        template<typename TUP>
        constexpr void template_decl(const TUP& t, const char *addlpms = 0) {
          __queue_metaparse "template<";
          list(t);
          if (addlpms) {
            if (t.size())
              __queue_metaparse ",";
            __queue_metaparse addlpms;
          }
          __queue_metaparse ">";
        }


/*
 * Okay hold on a sec.
 * How do we want to handle this.
 * Consider:
 *
 */


      } //gen


    } //v2
  } //meta
} //cppx

#endif //UNTITLED4_GENERATE_H
