// -*- C++ -*-

#ifndef CPPX_TUPLE_REMOVEIDX_HPP
#define CPPX_TUPLE_REMOVEIDX_HPP

namespace cppx
{
namespace meta
{
inline namespace v1
{

// -------------------------------------------------------------------------- //
// Tuple with element of a certain index removed

template<typename OrigTup, std::size_t RIdx>
struct tuple_removeidx
{
  static_assert(RIdx < OrigTup::size(), "Index to remove greater than max!");

  static constexpr std::size_t size() {
    return OrigTup::size() - 1;
  }
  static constexpr bool empty() {
    return size() == 0;
  }
};

} //v1
} //meta
} //cppx

namespace std
{

// Returns the Ith element after accounting for the removed idx.
template<std::size_t I, typename OrigTup, std::size_t RIdx, std::size_t AdjI = (I >= RIdx ? I+1 : I)>
constexpr auto
get(cppx::meta::tuple_removeidx<OrigTup, RIdx> const&) {
  return std::get<AdjI>(OrigTup());
}

} //std

#endif //CPPX_TUPLE_REMOVEIDX_HPP