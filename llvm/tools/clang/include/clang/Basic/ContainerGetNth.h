#ifndef CONTAINERGETNTH_H
#define CONTAINERGETNTH_H

#include <iterator>

namespace reflcontainers {

/// Specialize this for any reflected container with an inefficient
/// std::next implem (i.e. anything whose iters don't implement std::next
/// via simple addition) to be constant time if the N-1th iter is known.
/// I.e. you'll probably just need to add an N-1th iterator cache to the container,
/// and implement as checking that for N>0 or maybe N>1.
/// If you don't, iteration through that
/// container will be O(n^2) instead of O(N), due to how CXXTupleExpansion
/// is implemented (has to start from begin() for each lookup of the Nth elem)
template<typename T>
auto ContainerGetNth(const T& t, std::size_t N) -> decltype(*t.begin()) {
  return *std::next(t.begin(), N); //this implem will work for any container, but...
//  return *(t.begin() + N); //For any T that fails for this implem, iterating will be O(n^2) instead of O(n)
}

} //namespace reflcontainers

#endif // CONTAINERGETNTH_H
