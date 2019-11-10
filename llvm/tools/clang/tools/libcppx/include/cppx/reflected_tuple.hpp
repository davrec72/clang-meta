// -*- C++ -*-

#ifndef CPPX_REFLECTED_TUPLE_HPP
#define CPPX_REFLECTED_TUPLE_HPP

namespace cppx
{
namespace meta
{
inline namespace v2
{

// -------------------------------------------------------------------------- //
// Reflected tuples

// Presents a tuple-like interface for reflection queries.
template<typename Ops>
struct reflected_tuple
{
  static constexpr std::size_t size() {
    return Ops::size();
  }
  static constexpr bool empty() {
    return size() == 0;
  }
};

//DWR TEMP HACK:

// Returns the Ith element of the reflected tuple.
// DWR NB: this doesn't depend on the object; reflected_tuples contain all their
// information in static properties of the instantiations
        template<std::size_t I, typename Ops>
        constexpr auto
        get(cppx::meta::reflected_tuple<Ops> const &) {
            return Ops::template get<I>();
        }


// -------------------------------------------------------------------------- //
// Tuple algorithms

// For each

namespace detail
{

// TODO: Add support for non-const tuples?
//
// Base case.
template<std::size_t I,typename T, typename F>
constexpr std::enable_if_t<I == std::tuple_size<T>::value, void>
tuple_for_each_recursive(T const&, F) {
}

// Recursive step.
template<std::size_t I, typename T, typename F>
constexpr std::enable_if_t<I < std::tuple_size<T>::value, void>
tuple_for_each_recursive(T const& t, F f) {
  f(std::get<I>(t));
  tuple_for_each_recursive<I + 1>(t, f);
}

} // namespace detail

// Driver, for ease of use.
template<typename T, typename F>
constexpr void
tuple_for_each(T const& t, F f) {
  detail::tuple_for_each_recursive<0>(t, f);
}

// -------------------------------------------------------------------------- //
// Tuple traversal/application

        template<typename T, typename F>
        constexpr void
        for_each(T const& t, F f) {
            detail::tuple_for_each_recursive<0>(t, f);
        }

} //v1
} //meta
} //cppx

namespace std
{
// Returns the Ith element of the reflected tuple.
// DWR NB: this doesn't depend on the object; reflected_tuples contain all their
// information in static properties of the instantiations
template<std::size_t I, typename Ops>
constexpr auto
get(cppx::meta::reflected_tuple<Ops> const &) {
  return Ops::template get<I>();
}

template<typename Ops>
struct tuple_size<cppx::meta::reflected_tuple<Ops>>
  : std::integral_constant<std::size_t, Ops::size()>
{};

template<std::size_t I, typename Ops>
struct tuple_element<I, cppx::meta::reflected_tuple<Ops>>
{
  using type = decltype(Ops::template get<I>());
};

} //std

#endif //CPPX_REFLECTED_TUPLE_HPP