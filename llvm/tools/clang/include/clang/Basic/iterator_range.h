//===- iterator_range.h - A range adaptor for iterators ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
/// \file
/// DWR TODO
///
//===----------------------------------------------------------------------===//

#ifndef CLANG_ITERATOR_RANGE_H
#define CLANG_ITERATOR_RANGE_H

#include <iterator>
#include <utility>

#include "llvm/ADT/iterator_range.h"

#include "llvm/Support/raw_ostream.h" //DWR TEMP DEBUG

#include "clang/Basic/ContainerGetNth.h"

namespace clang {
template <typename IteratorT>
class iterator_range;
}

namespace reflcontainers {
  template<typename IteratorT>
  auto ContainerGetNth(const clang::iterator_range<IteratorT>& t, std::size_t N)
    -> decltype(*std::declval<clang::iterator_range<IteratorT>>().begin()) ;
}

namespace clang {

/// A range adaptor for a pair of iterators.
/// (DWR TODO explain)
template <typename IteratorT>
class iterator_range : public llvm::iterator_range<IteratorT> {
  /// Only ContainerGetNth should touch these!!!
  mutable IteratorT containergetnthONLY_cachedlast_iter;
  mutable std::size_t containergetnthONLY_cachedlast_N;
  template<typename _IteratorT>
  friend auto reflcontainers::
  ContainerGetNth(const clang::iterator_range<_IteratorT>& t, std::size_t N)
    -> decltype(*std::declval<clang::iterator_range<_IteratorT>>().begin());

public:
  template <typename Container>
  iterator_range(Container &&c)
      : llvm::iterator_range<IteratorT>(std::move(c)) {}
  iterator_range(IteratorT begin_iterator, IteratorT end_iterator)
      : llvm::iterator_range<IteratorT>(begin_iterator, end_iterator) {}
};

///// Convenience function for iterating over sub-ranges.
/////
///// This provides a bit of syntactic sugar to make using sub-ranges
///// in for loops a bit easier. Analogous to std::make_pair().
//template <class T> iterator_range<T> make_range(T x, T y) {
//  return iterator_range<T>(std::move(x), std::move(y));
//}

//template <typename T> iterator_range<T> make_range(std::pair<T, T> p) {
//  return iterator_range<T>(std::move(p.first), std::move(p.second));
//}

//template <typename T>
//iterator_range<decltype(adl_begin(std::declval<T>()))> drop_begin(T &&t,
//                                                                  int n) {
//  return make_range(std::next(adl_begin(t), n), adl_end(t));
//}

} //namespace clang

namespace reflcontainers {

  template<typename IteratorT>
  auto ContainerGetNth(const clang::iterator_range<IteratorT>& t, std::size_t N)
        -> decltype(*std::declval<clang::iterator_range<IteratorT>>().begin())
  {
    llvm::errs() << "DWR TEMP DEBUG: in iterator_range overload of ContainerGetNth\n";
    if (N==0) {
      t.containergetnthONLY_cachedlast_N = 0;
      t.containergetnthONLY_cachedlast_iter = t.begin();
      return *t.begin();
    } else if (N == t.containergetnthONLY_cachedlast_N + 1) {
       ++t.containergetnthONLY_cachedlast_N;
      return *++t.containergetnthONLY_cachedlast_iter;
    }
    else {
      // Rare case:
      // Probably an outer loop over t with a nested loop over t inside;
      // slows things down a bit for outer loop, but doesn't change complexity
      // since nested loop already O(n^2) at least, so we can live with it.
      // (Also possible somebody called _get_<N> manually with some N.)
      // We'll try to use the exisitng cache to shorten the search at least,
      // and will still cache the result in case this was just a rare disruption
      // so that the cache will be useful on the next iteration (this is not so
      // if there's a nested loop/_get_ call being done every single outer iter,
      // but again that's already O(n^2) so no big deal).
      if (N < t.containergetnthONLY_cachedlast_N )
        // Travel from beginning (though if we have an operator-- perhaps we
        // should consider going backward? Eh whatever, keep it simple for now.)
        t.containergetnthONLY_cachedlast_iter = std::next(t.begin(), N);
      else
        //Travel from cached iter:
        t.containergetnthONLY_cachedlast_iter = std::next(
              t.containergetnthONLY_cachedlast_iter,
              N - t.containergetnthONLY_cachedlast_N);

      t.containergetnthONLY_cachedlast_N = N;
      return *t.containergetnthONLY_cachedlast_iter;
    }
  }

} //namespace reflcontainers

#endif
