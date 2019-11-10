//
//

#ifndef LLVM_REFLECTION_BASE_H
#define LLVM_REFLECTION_BASE_H

#include <cstdint>
#include "traits.h" //for decl_traits

//namespace sstr = ak_toolkit::static_str; //FIXME

namespace cppx
{
namespace meta
{
inline namespace v2
{

//Some macros used in common in types.h, decls.h etc.
#define COMMA_VA_ARGS(...) , ##__VA_ARGS__
#define DEF_TRAITS(MP_name) \
  static constexpr MP_name ## _traits traits() {\
    return MP_name ## _traits(__reflect_traits(X));\
  }\
/**/

// The type of a reflected AST node.
using reflection_t = std::intptr_t;

namespace detail {
  template<reflection_t X>
  struct reflection_base {
//    //Not sure if this is useful, but seems like it could be
//    // to allow reflection functions to return a nullptr:
    constexpr operator bool() {
      return X;
    }
  };
} //detail

template<typename T>
struct getreflX;

template<template<reflection_t> class refltmpl, reflection_t X>
struct getreflX<refltmpl<X>> {
  static constexpr reflection_t value = X;
};

/// dyn_cast<T>(u): will return T<0> if the conversion is not possible.
template<template<reflection_t X> class T, typename U>
static constexpr T< std::is_base_of<T<getreflX<U>::value>, U>::value
                    ? getreflX<U>::value
                    : 0 >
dyn_cast(U u) {
  return {};
//  if constexpr (std::is_base_of<T<getreflX<U>::value>, U>::value)
//    return u;
//  else
//    return T<0>{};
}

/// cast<T>(u): will fail at compilation if the
/// T<X> is not a base of/same as/convertible to U.
template<template<reflection_t X> class T, typename U>
static constexpr T<getreflX<U>::value>
cast(U u) {
  static_assert(std::is_base_of<T<getreflX<U>::value>, U>::value,
          "Invalid cast!");
  return {};
}

template<reflection_t X1, reflection_t X2,
        template<reflection_t> class T1, template<reflection_t> class T2,
        typename std::enable_if_t<
                std::is_base_of<detail::reflection_base<X1>, T1<X1>>::value
             && std::is_base_of<detail::reflection_base<X2>, T2<X2>>::value
              , int> = 0>
constexpr bool operator==(T1<X1>, T2<X2>) {
  return X1 == X2;
}

template<reflection_t X1, reflection_t X2,
         template<reflection_t> class T1, template<reflection_t> class T2,
        typename std::enable_if_t<
                std::is_base_of<detail::reflection_base<X1>, T1<X1>>::value
             && std::is_base_of<detail::reflection_base<X2>, T2<X2>>::value
              , int> = 0>
constexpr bool operator!=(T1<X1>, T2<X2>) {
  return X1 != X2;
}

} //v2
} //meta
} //cppx

#endif //LLVM_REFLECTION_BASE_H
