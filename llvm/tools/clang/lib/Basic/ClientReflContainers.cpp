#include "clang/Basic/ClientReflContainers.h"

using namespace reflcontainers;







// ContainerGetNth overload defs (DWR COMMENTED OUT, need to solve the O(n^2) issue at a more fundamental level,
// affects almost every range)

//uintptr_t reflcontainers::ContainerGetNth(const reflcontainers::SetInt& t, std::size_t N) {
//  llvm::errs() << "DWR TEMP DEBUG: GOOD, in specialization of ContainerGetNth\n";
//  assert(N < t.size());
//  if (N==0) {
//    t.containergetnthONLY_cachedlast_N = 0;
//    t.containergetnthONLY_cachedlast_iter = t.begin();
//    return *t.begin();
//  } else if (N == t.containergetnthONLY_cachedlast_N + 1) {
//     ++t.containergetnthONLY_cachedlast_N;
//    return *++t.containergetnthONLY_cachedlast_iter;
//  }
//  else {
//    // Rare case:
//    // Probably an outer loop over t with a nested loop over t inside;
//    // slows things down a bit for outer loop, but doesn't change complexity
//    // since nested loop already O(n^2) at least, so we can live with it.
//    // (Also possible somebody called _get_<N> manually with some N.)
//    // We'll try to use the exisitng cache to shorten the search at least,
//    // and will still cache the result in case this was just a rare disruption
//    // so that the cache will be useful on the next iteration (this is not so
//    // if there's a nested loop/_get_ call being done every single outer iter,
//    // but again that's already O(n^2) so no big deal).
//    if (N < t.containergetnthONLY_cachedlast_N )
//      // Travel from beginning (though if we have an operator-- perhaps we
//      // should consider going backward? Eh whatever, keep it simple for now.)
//      t.containergetnthONLY_cachedlast_iter = std::next(t.begin(), N);
//    else
//      //Travel from cached iter:
//      t.containergetnthONLY_cachedlast_iter = std::next(
//            t.containergetnthONLY_cachedlast_iter,
//            N - t.containergetnthONLY_cachedlast_N);

//    t.containergetnthONLY_cachedlast_N = N;
//    return *t.containergetnthONLY_cachedlast_iter;
//  }
//}

//reflcontainers::StringType reflcontainers::ContainerGetNth(const reflcontainers::SetStr& t, std::size_t N) {
//  llvm::errs() << "DWR TEMP DEBUG: GOOD, in specialization of ContainerGetNth\n";
//  assert(N < t.size());
//  if (N==0) {
//    t.containergetnthONLY_cachedlast_N = 0;
//    t.containergetnthONLY_cachedlast_iter = t.begin();
//    return *t.begin();
//  } else if (N == t.containergetnthONLY_cachedlast_N + 1) {
//     ++t.containergetnthONLY_cachedlast_N;
//    return *++t.containergetnthONLY_cachedlast_iter;
//  }
//  else {
//    if (N < t.containergetnthONLY_cachedlast_N )
//      // Travel from beginning:
//      t.containergetnthONLY_cachedlast_iter = std::next(t.begin(), N);
//    else
//      //Travel from cached iter:
//      t.containergetnthONLY_cachedlast_iter = std::next(
//            t.containergetnthONLY_cachedlast_iter,
//            N - t.containergetnthONLY_cachedlast_N);

//    t.containergetnthONLY_cachedlast_N = N;
//    return *t.containergetnthONLY_cachedlast_iter;
//  }
//}


//IntStrPair reflcontainers::ContainerGetNth(const reflcontainers::MapIntStr& t, std::size_t N) {
//  llvm::errs() << "DWR TEMP DEBUG: GOOD, in specialization of ContainerGetNth\n";
//  assert(N < t.size());
//  if (N==0) {
//    t.containergetnthONLY_cachedlast_N = 0;
//    t.containergetnthONLY_cachedlast_iter = t.begin();
//    return *t.begin();
//  } else if (N == t.containergetnthONLY_cachedlast_N + 1) {
//     ++t.containergetnthONLY_cachedlast_N;
//    return *++t.containergetnthONLY_cachedlast_iter;
//  }
//  else {
//    if (N < t.containergetnthONLY_cachedlast_N )
//      // Travel from beginning:
//      t.containergetnthONLY_cachedlast_iter = std::next(t.begin(), N);
//    else
//      //Travel from cached iter:
//      t.containergetnthONLY_cachedlast_iter = std::next(
//            t.containergetnthONLY_cachedlast_iter,
//            N - t.containergetnthONLY_cachedlast_N);

//    t.containergetnthONLY_cachedlast_N = N;
//    return *t.containergetnthONLY_cachedlast_iter;
//  }
//}
