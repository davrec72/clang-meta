// -*- C++ -*-

#ifndef CPPX_FILTERED_TUPLE_HPP
#define CPPX_FILTERED_TUPLE_HPP

#include <cstddef>
#include <tuple>
#include "reflected_tuple.hpp"

namespace cppx
{
namespace meta
{
inline namespace v1
{

// Count if

namespace detail
{
template<template<typename> typename P>
struct count_type_if_fn
{
  int& n;

  // Matching case.
  template<typename T>
  constexpr std::enable_if_t<P<T>::value> operator()(T const& t) {
    ++n;
  }

  // Non-matching case.
  template<typename T>
  constexpr std::enable_if_t<!P<T>::value> operator()(T const& t) { }
};
} // namespace detail

// Returns the number of elements in t whose types satisfy a unary type trait.
template<template<typename> typename P, typename T>
constexpr int
tuple_count_type_if(T const& t) {
  int n = 0;
  detail::count_type_if_fn<P> f{n};
  tuple_for_each(t, f);
  return n;
}

// -------------------------------------------------------------------------- //
// Tuple algorithms

// A filter applied to a tuple.
template<typename OrigTup, template<typename> typename Pred>
struct filtered_tuple
{
  static constexpr std::size_t size() {
    return tuple_count_type_if<Pred>(OrigTup());
  }
  static constexpr bool empty() {
    return size() == 0;
  }
};

namespace detail
{

// The type of nth element of a tuple.
//
// TODO: This should be equivalent to tuple_element. I don't think I need it.
template<int N, typename T>
using constant_element_type_t = decltype(std::get<N>(std::declval<T>()));

// A helper class for accessing the filtered element of a tuple. The first
// parameter determines whether the nested type is defined.
template<bool B, int N, typename T>
struct safe_element_type
{
  using type = constant_element_type_t<N, T>;
};

// FIXME: If the predicate is a type trait, this might fail in bad ways.
template<int N, typename T>
struct safe_element_type<false, N, T>
{
  using type = void;
};

template<int N, typename T>
using safe_element_type_t = typename safe_element_type<N < std::tuple_size<T>::value, N, T>::type;

template<std::size_t N, template<typename> typename P, typename T>
constexpr bool is_satisfied()
{
  return P<safe_element_type_t<N, T>>::value;
}

// A helper class used to implement get<>() for filtered tuples.
struct filtered_get
{
  // Off the end. This returns void, which is almost certainly unusable at
  // the point of use.
  template<std::size_t I, std::size_t J, template<typename> typename P, typename T>
  static constexpr auto get(
      T const& t,
      std::enable_if_t<I == std::tuple_size<T>::value>* = 0)
  {
    return;
  }

  // I < N, J == 0 and get<I>(t) satisfies P. Returns get<I>(t).
  template<std::size_t I, std::size_t J, template<typename> typename P, typename T>
  static constexpr auto get(
      T const& t,
      std::enable_if_t<
          I < std::tuple_size<T>::value && J == 0 && is_satisfied<I, P, T>()
      >* = 0)
  {
    return std::get<I>(t);
  }

  // I < N, J == 0 and std::get<I>(t) does not satisfy P. Recurse.
  template<std::size_t I, std::size_t J, template<typename> typename P, typename T>
  static constexpr auto get(
      T const& t,
      std::enable_if_t<
          I < std::tuple_size<T>::value && J == 0 && !is_satisfied<I, P, T>()
      >* = 0)
  {
    return get<I + 1, 0, P>(t);
  }

  // I < N, J > 0 and std::get<I>(t) satisfies P. Recurse, decrementing J.
  template<std::size_t I, std::size_t J, template<typename> typename P, typename T>
  static constexpr auto get(
      T const& t,
      std::enable_if_t<
            I < std::tuple_size<T>::value && 0 < J && is_satisfied<I, P, T>()
      >* = 0)
  {
    return get<I + 1, J - 1, P>(t);
  }

  // I < N, J > 0 and std::get<I>(t) does not satisfies P. Recurse.
  template<std::size_t I, std::size_t J, template<typename> typename P, typename T>
  static constexpr auto get(
      T const& t,
      std::enable_if_t<
          I < std::tuple_size<T>::value && 0 < J && !is_satisfied<I, P, T>()
      >* = 0)
  {
    return get<I + 1, J, P>(t);
  }
};

} // namespace detail


} // inline namespace v1
} // namespace meta
} // namespace cppx

namespace std
{

// Returns the Ith element in the filtered tuple.
template<std::size_t I, typename OrigTup, template<typename> typename Pred>
constexpr auto
get(cppx::meta::filtered_tuple<OrigTup, Pred> const& t)
{
  return cppx::meta::detail::filtered_get::get<0, I, Pred>(t);
}

template<typename Ops, template<typename> typename Pred>
struct tuple_size<cppx::meta::filtered_tuple<Ops, Pred>>
  : std::integral_constant<std::size_t, cppx::meta::filtered_tuple<Ops, Pred>::size()>
{ };

template<std::size_t I, typename Ops, template<typename> typename Pred>
struct tuple_element<I, cppx::meta::filtered_tuple<Ops, Pred>>
{
  using type = decltype(get<I>(std::declval<cppx::meta::filtered_tuple<Ops, Pred>>()));
};

} // namespace std

#endif // CPPX_FILTERED_TUPLE_HPP
