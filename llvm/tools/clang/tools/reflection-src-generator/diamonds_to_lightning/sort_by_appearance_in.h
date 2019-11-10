//
// Created by David Rector on 2019-06-25.
//

#ifndef LLVM_SORT_BY_APPEARANCE_IN_H
#define LLVM_SORT_BY_APPEARANCE_IN_H

#include <cassert>

/// Functor to find which of two values appears in a
/// vector earlier than the other, starting at startit.
/// This can be used to e.g. sort the contents of another
/// vector containing a subset of _vec's elems.
/// @code
/// auto sorter = sort_by_appearance_in<decltype(allclassdecls)>(
//            allclassdecls, /*startit=*/baseit );
//    std::sort(drvdvec.begin(), drvdvec.end(), sorter);
/// @endcode
template<class CONTAINER, bool FORWARD = true>
struct sort_by_appearance_in {
  const CONTAINER &_container;
  using const_iterator = typename CONTAINER::const_iterator;
  const_iterator _startit;
  using value_type = decltype(*_startit);

  bool operator()(const value_type &lhs, const value_type &rhs) const {
    for (auto it = _startit; it != _container.end(); ++it) {
      if (*it == lhs)
        return FORWARD;
      if (*it == rhs)
        return !FORWARD;
    }
    assert(false && "Expected both lhs and rhs to be in vec, after startit!"
                     "Investigate whether they're in there at all, or if they're"
                     "somehow before startit");
  }
  sort_by_appearance_in(const CONTAINER &container,
          typename CONTAINER::const_iterator startit)
      : _container(container), _startit(startit)
  {}
  sort_by_appearance_in(const CONTAINER &container)
          : _container(container), _startit(_container.begin())
  {}
};



//// TODO just incorporate this one into the above one, with an extra bool template param -- or have them each be aliases of a base template with that param.
//template<class CONTAINER>
//struct sort_by_reverse_appearance_in {
//  const CONTAINER &_vec;
//  decltype(_vec.rbegin()) _startrit;
//  using value_type = decltype(*_startrit);
//
//  bool operator()(value_type lhs, value_type rhs) const {
//    for (auto rit = _startrit; rit != _vec.rend(); ++rit) {
//      if (*rit == lhs)
//        return true;
//      if (*rit == rhs)
//        return false;
//    }
//    llvm_unreachable("Expected both lhs and rhs to be in vec, before startrit!"
//                     "Investigate whether they're in there at all, or if they're"
//                     "somehow after startrit (remember this is the REVERSE version)");
//  }
//  sort_by_reverse_appearance_in(const CONTAINER &vec,
//          typename CONTAINER::const_reverse_iterator startrit)
//          : _vec(vec), _startrit(startrit)
//  {}
//  sort_by_reverse_appearance_in(const CONTAINER &vec)
//          : _vec(vec), _startrit(_vec.rbegin())
//  {}
//};


#if __cplusplus > 201402L
/// Deduction guides (C++17):
template<class CONTAINER>
sort_by_appearance_in(const CONTAINER&, typename CONTAINER::const_iterator)
-> sort_by_appearance_in<CONTAINER>;

//template<class CONTAINER>
//sort_by_reverse_appearance_in(const CONTAINER&, typename CONTAINER::const_reverse_iterator)
//-> sort_by_reverse_appearance_in<CONTAINER>;
#endif

#endif //LLVM_SORT_BY_APPEARANCE_IN_H
