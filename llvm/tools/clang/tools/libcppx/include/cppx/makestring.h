//
// Created by David Rector on 2019-04-08.
//

#ifndef UNTITLED4_MAKESTRING_H
#define UNTITLED4_MAKESTRING_H

#include <cppx/kw2chars.h>
#include <cppx/traits.hpp>

namespace cppx {
  namespace meta {
    inline namespace v2 {

      namespace str {

        template<typename T>
        constexpr const char *name_list_str(const T& t) {
          bool first = true;
          const char * res;
          for... (auto m : t) {
            if (first) {
              first = false;
              res = m.name();
            }
            else
              res = __concatenate(res, ",", m.name());
          }
          return res;
        }

        /*---------------------------------------------------*/
#define DEF_ENCL_NAME_LIST_STR_FCN(MP_namepfx,                \
                                 MP_openstr, MP_closestr)     \
        template<typename TUP>                                \
        constexpr const char * MP_namepfx##_name_list_str(    \
                const TUP& t, const char *addlpms = 0) {      \
          if (addlpms) {                                      \
            if (t.size())                                     \
              return __concatenate(                           \
                      MP_openstr,                             \
                      name_list_str(t), "," addlpms,          \
                      MP_closestr);                           \
            return __concatenate(                             \
                    MP_openstr, addlpms, MP_closestr);        \
          }                                                   \
          return __concatenate(                               \
                  MP_openstr,                                 \
                  name_list_str(t), MP_closestr);             \
        }                                                     \
        /*---------------------------------------------------*/
        //specpm_name_list_str(tup, addlpms):
        DEF_ENCL_NAME_LIST_STR_FCN(specpm,"<",">")

//        //funcpm_name_list_str(tup, addlpms):
//        DEF_ENCL_NAME_LIST_STR_FCN(funcpm,"(",")")
//
//        //initpm_name_list_str(tup, addlpms):
//        DEF_ENCL_NAME_LIST_STR_FCN(initpm,"{","}")

#undef  DEF_ENCL_NAME_LIST_STR_FCN

      } //str


    } //v2
  } //meta
} //cppx

#endif //UNTITLED4_MAKESTRING_H
