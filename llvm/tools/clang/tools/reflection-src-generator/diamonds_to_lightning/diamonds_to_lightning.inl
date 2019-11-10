//
//  diamonds_to_lightning.inl
//  diamonds_to_lightning
//
//  Created by David Rector on 5/31/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef diamonds_to_lightning_inl
#define diamonds_to_lightning_inl

#include <cassert>
#include "llvm/ADT/SmallVector.h"
#include "coprime.h"
#include "sort_by_appearance_in.h"



# define D2L_METHOD(MP_rettype) \
  template<typename APP> \
  MP_rettype diamonds_to_lightning<APP>
/**/

namespace dwr {

using llvm::SmallVector;

//TODO make this overload present for any container with a "find" function,
//and the above one for anything without.
// (This not actually used here for now, but it will be whenever prntconn_container_t
// is e.g. a std::set instead of a vector).
template<typename T>
bool contains_elem(const std::set<T> &container, T elem) {
  auto res = container.find(elem);
  if (res != container.end())
    return true;
  return false;
}


D2L_METHOD(auto)::createNewHousehold() -> households_iter_t {
  _households.emplace_back();
  return std::prev(_households.end(), 1);
}

D2L_METHOD(auto)::mergeHouseholdsIfDiff(households_iter_t a, households_iter_t b) -> households_iter_t {
  assert(a != _households.end() && "Expected first household to be valid");
  if (a != b && b != _households.end()) {
    // Two valid, distinct households have been supplied; we must do a real merge.
    // The only data in a Household, for now, is its elems list, so splice b's elems
    // onto a's (doesn't matter if B's is larger, splice is a constant time operation
    // for std::list):
    for (auto elem : b->elems()) {
      elem.thehousehold = a;
    }
#ifndef NDEBUG
    for (auto elem : a->elems())
      assert(elem.thehousehold == a);
#endif

    a->elems().splice( a->elems().end(), b->elems() );
    _households.erase(b);
  }
  return a;
}

//     cur[MP_x],
//     /*is_fda=*/res == MFK_direct_equality,
//     /*is_lda=*/OUTERMOST,
//     chld[MP_x],
//     thehousehold

D2L_METHOD(void)::
findOrMakeNewPsRelElem(prntconn_t b,
                       bool is_fda, bool is_lda,
                       prntconn_t child,
                       households_iter_t &thehousehold/*ncref*/) {
  auto mapiter = _prnt_to_psrelinfo.find(b);

  if (mapiter == _prnt_to_psrelinfo.end()) {
    // Elem NOT found, so create one...
    // First, fetch or create the household:
    if (thehousehold == _households.end()) {
      //No household given, so make a new one, and update thehousehold
      thehousehold = createNewHousehold();
    }

    // Insert a new element with the given params within the household's elems list,
    // just before its child (or at the end if no child):
    auto &elems = thehousehold->elems();

    elems.emplace_back( elem_t(b, is_fda, is_lda, child, thehousehold) );
    auto respos = std::prev(elems.end());

//
//      std::cout << "\nDEBUG: node / child = " << b.getClassName() << " / " << APP::getnode(child) << "";
//
//      elemiter_t respos = elems.insert(thehousehold->_inspos, elem_t(b, is_fda, is_lda, child, thehousehold));
//
//#ifndef NDEBUG
//      if (respos != elems.begin()) {
//        elemiter_t prevelem = std::prev(respos);
//        if (auto prevnode = APP::getnode(prevelem->child))
//          assert(prevnode == APP::getnode(b)
//                 && "Hmmm, expected this list insertion code to result in each elem with a "
//                    "parent within the household being inserted after that parent, but it didn't work "
//                    "here -- not a big deal really, so to fix just uncomment the emplace_back code above "
//                    "and comment out the insert stuff, that will at least give you an ordering that "
//                    "won't result in errors.  ");
//      }
//#endif
//
//      if (APP::getnode(child))
//        thehousehold->_inspos = std::next(respos);
//      else
//        thehousehold->_inspos = elems.begin();

    // Add a _prnt_to_psrelinfo entry to help you find this element later whenever you
    // supply b to the map:
    // (Note that since the elemiter is a std::list iterator, it will not be invalidated due
    // to vector resizing, household shuffling, etc.)
    _prnt_to_psrelinfo.insert( { b, respos } );
  } else {
    // Existing elem found, so just fetch its data and make it consistent
    // with the provided fda/lda/etc. params, and update thehousehold.
    auto theelem = mapiter->second;

    //Upgrade the is_fda/is_lda/child status if necessary:
    if (is_fda) {
      theelem->is_fda = true;
      assert(mapiter->second->is_fda); //sanity check
    }
    if (is_lda) {
      theelem->is_lda = true;
      assert(mapiter->second->is_lda); //sanity check
    }
    if (child) {
      assert(!theelem->child || theelem->child == child
             && "Two competing derived nodes! Something's wrong, think this through");
      theelem->child = child;
    }

    // Merge the households if necessary:
    thehousehold = mergeHouseholdsIfDiff(theelem->thehousehold, thehousehold);
  }
}

D2L_METHOD()::PrntConnPair::
PrntConnPair(prntconn_t _first, prntconn_t _second)
    : first(_first), second(_second)
{
  assert(first && second && "Expected these to be nonnull");
  assert(APP::getnode(first) != APP::getnode(second)
         && "Expected these to be different");
  // Standardize the sorting, to make comparison operators valid,
  // since checking the existence of diamonds between the pair
  // should be independent of the ordering in which we supply them.
  // HOWEVER, sort them in REVERSE order -- the first
  // should be the higher sorted (e.g. {D,C} instead of {C,D}).
  // This is to allow us to always insert as far back in the
  // _diamonds_resolved_in_pair cache map as possible for each pair,
  // giving us a very good insertion hint before the next processed
  // pair, which will be lower.
  // I.e. if testing C against E, D, B, A (they should always be in
  // reverse order), and caching each result, we'll construct these
  // pairs in this order:
  // {E,C} {D,C} {C,B}, {C,A}.
  // After each insertion we'll supply that position as the insertion
  // hint for the next pair; as of C++11 this hint is optimal when it
  // is just BEYOND the ultimate insertion point.
  if (APP::getnode(first) < APP::getnode(second)) {
    //Flip them:
    first = _second;
    second = _first;
  }
}

D2L_METHOD(bool)::PrntConnPair::
operator==(const PrntConnPair &other) const {
  return APP::getnode(first) == APP::getnode(other.first)
      && APP::getnode(second) == APP::getnode(other.second);
}

D2L_METHOD(bool)::PrntConnPair::
operator<(const PrntConnPair &other) const {
  if (APP::getnode(first) == APP::getnode(other.first))
    return APP::getnode(second) < APP::getnode(other.second);
  return APP::getnode(first) < APP::getnode(other.first);
}



//HELPER
template<typename APP>
static size_t get_prntconns_size(typename APP::node_t *node) {
  return std::distance(APP::get_prntconns_rbegin(node),
                       APP::get_prntconns_rend(node));
}


D2L_METHOD(void)::
addnode(node_t *node) {
//      std::cout << "\naddnode(" << node->name() << ")...";
  // If you have one or fewer bases, there's nothing to check.
  if (get_prntconns_size<APP>(node) > 1) {
    // There are at least two bases.
    // Get an iterator range over them in REVERSE SORTED order (to make map
    // insertion more efficient as we construct pairs of them -- see
    // comments elsewhere for explanation).

    auto sorted_prnt_riters = APP::get_sorted_prntconns_riters(node);

    if /*constexpr*/ (APP::CHECK_ANCESTORS) {
      // Make sure each of the parent nodes have been processed
      // if you have specified that is necessary (e.g. if you're
      // starting with a node possibly "in the middle", rather than
      // processing all the nodes in order.)
      // Note that the reverse sorting doesn't matter here, so
      // if this causes problems you may change this so it
      // iterates in forward order.
      for (auto prntit = sorted_prnt_riters.first;
           prntit != sorted_prnt_riters.second;
           ++prntit) {
        assert(*prntit && "Did not expect null parent!");
        auto node = APP::getnode(*prntit);
        if (!_addednodes.count(node)) {
          addnode(node);
          assert(_addednodes.count(node));
        }
      }
    } //if APP::CHECK_ANCESTORS

    // Initialize a 'thehousehold' variable; this will be passed by
    // non const reference to addDiamondTops.  Initially this will
    // be a null household; if necessary, we will create a new valid
    // household.
    households_iter_t thehousehold = _households.end();

    // Loop over the all pairs of bases (n(n-1)/2 pairs).
    cache_iter_t cacheinshint = _diamonds_resolved_in_pair.end();
    for (auto prnt0it = sorted_prnt_riters.first;
         prnt0it != std::prev(sorted_prnt_riters.second, 1);
          //^ !! Don't include the "last" (i.e. first) prnt
         ++prnt0it) {
      assert(*prnt0it && "Did not expect null parent!");
      for (auto prnt1it = prnt0it + 1;
           prnt1it != sorted_prnt_riters.second;
           ++prnt1it) {
//            std::cout << "\n---NEW ROOT PAIR COMPARISON---";
        assert(*prnt1it && "Did not expect null parent!");
        if ( addDiamondTops<>(thehousehold/*ncref*/, cacheinshint/*ncref*/,
                              (prntconn_t[2]){*prnt0it, *prnt1it}/*curs*/) ) {
          assert(thehousehold != _households.end()
                 && "Expected thehousehold to have been set to a valid household, "
                 "given that addDiamondTops returned that it did find diamonds/triangles");

          // Note that map::operator[] will find OR default-add, so
          // this the key node will be present and have value at least 1
          // in joined_into_lda_counter after this step:
          ++thehousehold->joined_into_counts[node];

        } //if diamonds/triangles found
      } //inner for loop
    } //outer for loop
  } //if at least two bases

  if /*constexpr*/ (APP::CHECK_ANCESTORS) {
    _addednodes.insert(node);
  }
} //addnode(...)


/*
Okay, now we need to figure out is_lda.
Consider:
*/

template< typename APP >
template< unsigned INSERTCACHEEVERY /*= APP::INSERTCACHEEVERY*/
        , unsigned CHECKCACHEEVERY /*= APP::CHECKCACHEEVERY*/
        , unsigned ICACHEITER /*= 0*/
        , unsigned CCACHEITER /*= 0*/
        , bool OUTERMOST /*= true*/
        , bool RIGHT /*= false*/ >
auto diamonds_to_lightning<APP>::
addDiamondTops(households_iter_t &thehousehold, //ncref
               cache_iter_t &cacheinshint, //ncref
               prntconn_t cur[2],
               prntconn_t chld[2])
-> MatchFoundKind
{
//      std::cout << "\nTesting pair: " << cur[0].theclass()->name() << ", " << cur[1].theclass()->name();

  assert(cur[0] && cur[1] && "Did not expect these to be null!");
  //////////////////////////////
  // 0. Static cache variables
  //////////////////////////////
  // We use static cache frequency and iterator parameters
  // to enable us to benefit from the algorithmic-complexity-reducing
  // benefits of caching while keeping the frequency of the caching
  // to an optimal level (since it is often more expensive than
  // duplicate checking, which has no ill effects other than time wasted).
  // We separate out the frequency of CHECKING the cache vs.
  // INSERTING into it, since these may have different costs AND
  // more importantly to make sure these are relatively prime to avoid
  // "gap frequencies"; i.e. if they are relatively prime we can guarantee
  // that you will EVENTUALLY reach a cache point in finite time
  // IF you are traveling along a previously cached path, regardless
  // of the relative depths at which you began inserting into vs.
  // checking the cache, if that makes sense.
  static_assert(dwr::coprime(INSERTCACHEEVERY, CHECKCACHEEVERY),
                "INSERTCACHEEVERY and CHECKCACHEEVERY should be "
                "co-prime, to avoid the possibility of skipping "
                "over ALL cached entries due to cache-checking "
                "having a harmonic frequency with the original "
                "cache insertion frequency but starting out-of-phase "
                "with it");
  static const bool INSERTCACHE = INSERTCACHEEVERY && (ICACHEITER==INSERTCACHEEVERY-1);
  static const bool CHECKCACHE  = CHECKCACHEEVERY  && (CCACHEITER==CHECKCACHEEVERY-1);
  // These will be used in the recursive calls below:
  static const unsigned NEXT_ICACHEITER =
      INSERTCACHEEVERY ? ((ICACHEITER + 1) % INSERTCACHEEVERY) : 0;
  static const unsigned NEXT_CCACHEITER =
      CHECKCACHEEVERY  ? ((CCACHEITER + 1) % CHECKCACHEEVERY)  : 0;
  static_assert(NEXT_ICACHEITER < INSERTCACHEEVERY, "");
  static_assert(NEXT_CCACHEITER < CHECKCACHEEVERY, "");

//      static const bool NEXT_INSERTCACHE = INSERTCACHEEVERY && (NEXT_ICACHEITER==INSERTCACHEEVERY-1);

  ////////////////////////////
  // 1. Check for equality
  ////////////////////////////
  // Note that we do this before checking the cache, and don't bother inserting
  // into cache if we find equality, since this branch is cheap.
  // We needn't do this check on the outermost call, unless it is possible there
  // were duplicate parent connections.
  if /*constexpr*/ (APP::CHECK_FOR_DUPLICATE_PARENTS || !OUTERMOST) {
    if (APP::getnode(cur[0]) == APP::getnode(cur[1])) {
      return MFK_direct_equality;
    }
  }

  ////////////////////////////
  // 2. Check cache
  ////////////////////////////
  // (To see if we can avoid the expensive recursive calls in step 3...)
  if /*constexpr*/ (CHECKCACHE) {
    auto cacheiter = _diamonds_resolved_in_pair.find(PrntConnPair(cur[0], cur[1]));
    if (cacheiter != _diamonds_resolved_in_pair.end()) {
//          std::cout << "\n[CACHED VALUE FOUND: "
//                    << cacheiter->first.first.theclass()->name() << ", "
//                    << cacheiter->first.second.theclass()->name() << "]";
      // This pair has already been checked for triangles/diamonds.
      if (cacheiter->second != MFK_none) {
        // Diamonds/triangles were found between this pair when previously checked.
        // Before returning the MFK result (which will be anything but MFK_none),
        // we need to merge/update our thehousehold variable to be
        // consistent with the existing pseudo rel(s).
        // Note that if we found a TRIANGLE before, only ONE pseudo rel will have
        // been created; if a DIAMOND, BOTH will exist.
        auto mapres0 = _prnt_to_psrelinfo.find(cur[0]);
        if (mapres0 != _prnt_to_psrelinfo.end()) {
          thehousehold = mergeHouseholdsIfDiff(mapres0->second->thehousehold,
                                               thehousehold);
        }
        auto mapres1 = _prnt_to_psrelinfo.find(cur[1]);
        if (mapres1 != _prnt_to_psrelinfo.end()) {
          thehousehold = mergeHouseholdsIfDiff(mapres1->second->thehousehold,
                                               thehousehold);
        }
        assert(mapres0 != _prnt_to_psrelinfo.end() ||
               mapres1 != _prnt_to_psrelinfo.end()
               && "At least one of these should have existing pseudo rel data, "
               "given that this pair was marked as having diamonds/triangles "
               "found on a previous check");
      }
      return cacheiter->second;
    }
    //No cached result found, so must calculate the recursion results...

  } //if CHECKCACHE

  ////////////////////////////////////////
  // 3. Recursive calls: RIGHT and LEFT
  ///////////////////////////////////////

  /*-------------------------------------------*/
#     define RECURSION_CACHETPMS                    \
    INSERTCACHEEVERY, CHECKCACHEEVERY,          \
    NEXT_ICACHEITER, NEXT_CCACHEITER            \
  /*-------------------------------------------*/

  ////////////////////////////
  // 3a. RIGHT recursion
  // Call with cur[0] and each parent of cur[1].
  // This is done by both the right and left
  // versions of addDiamondTops.
  //
  //            prnt1_a  prnt1_b  ...
  //                 \     |     /
  //    cur[0]         (cur[1])
  //       \              /
  //        ...         ...
  //            \     /
  //       (original join node)
  //
//      std::cout << "\n[RIGHT RECURSION...]";
  MatchFoundKind finalres = MFK_none;
  auto cur1prnts_rbegin = APP::get_prntconns_rbegin(APP::getnode(cur[1]));
  auto cur1prnts_rend   = APP::get_prntconns_rend(  APP::getnode(cur[1]));

  // Remember, the optimal insertion hint is one PAST the ultimate
  // insertion point.  This hint will be passed by ref to the recursive call
  // and updated by it (one update per for loop iteration -- i.e. no further
  // recursion.)
  // We only really need this set when caching on the next recursive call.
  cache_iter_t cacheinshintB = _diamonds_resolved_in_pair.end();
  for (auto prnt1it = cur1prnts_rbegin; prnt1it != cur1prnts_rend; ++prnt1it) {
    assert(*prnt1it && "Did not expect null parent!");
    if (auto recres =
        addDiamondTops< RECURSION_CACHETPMS, /*OUTERMOST=*/false, /*RIGHT=*/true
        >(thehousehold, cacheinshintB, //ncrefs
          (prntconn_t[2]){ cur[0], *prnt1it }/*curs*/,
          (prntconn_t[2]){ chld[0], cur[1] }/*chlds*/ )
        )
    {
//          std::cout << "\nRIGHT recursion succeeded; res = " << recres;
      assert(recres != MFK_l_indirectmatch
             && "Did not expect l_indirectmatch to be returned from a right recursive call");
      if (finalres == MFK_none) { //i.e. if you haven't yet set up the Elem...
        // Set up the pseudo-relation Elem for cur[1].
        // Note that, for right-recursion, is_fda is determined simply by whether
        // cur[0] was found to equal *prnt1it.
        findOrMakeNewPsRelElem(cur[1],
                               /*is_fda=*/(recres == MFK_direct_equality),
                               /*is_lda=*/!RIGHT, //i.e. if this was an initial right-recursion call,
                                                  // e.g. in the OUTERMOST or after some left-recursion,
                                                  // cur[1] is the initial right elem and thus is an lda.
                               chld[1],
                               thehousehold);

        finalres = MFK_r_indirectmatch;

        if (recres == MFK_direct_equality) {
          thehousehold->split_froms.insert(cur[0]);
        }
      }

    }
  } //loop over prnt1its


  if /*constexpr*/ (!RIGHT) {

    auto cur0prnts_rbegin = APP::get_prntconns_rbegin(APP::getnode(cur[0]));
    auto cur0prnts_rend   = APP::get_prntconns_rend(  APP::getnode(cur[0]));

    ////////////////////////////
    // 3c. LEFT recursion
    // Call with cur[1] and each parent of cur[0].
    // This is only done by the left version, and
    // only if neither right- nor both-recursion
    // yielded matches,
    // or if you need to check for cirularity (for
    // C++ classes there is no need).
    // TODO test out the circularity functionality,
    // add more documentation
    //
    // prnt0_a  prnt0_b  ...
    //      \     |     /
    //        (cur[0])         cur[1]
    //            \               /
    //             ...         ...
    //                 \     /
    //             (original join node)
    //

    if (finalres == MFK_none)
        //^ i.e. only need to check left-recursion if right recursion did not find
        // matches, since if right recursino succeeded any left recursion matches
        // would be circular (which we should check at the outset, during addnode,
        // if at all, because if they are logically possible they would probably
        // cause infinite looping somewhere)
    {
     // Reinitialize the insertion hint, since we're dealing with a new set of
     // reverse-sorted bases:
      cacheinshintB = _diamonds_resolved_in_pair.end();
      for (auto prnt0it = cur0prnts_rbegin; prnt0it != cur0prnts_rend; ++prnt0it) {
        assert(*prnt0it && "Did not expect null parent!");
        if (auto recres =
            addDiamondTops< RECURSION_CACHETPMS, /*OUTERMOST=*/false, /*RIGHT=*/false
            >(thehousehold, cacheinshintB, //ncrefs
              (prntconn_t[2]){ *prnt0it, cur[1] }/*curs*/,
              (prntconn_t[2]){ cur[0], chld[1] }/*chlds*/ ) )
        {
//              std::cout << "\nLEFT recursion succeeded; res = " << recres;
          if (finalres == MFK_none) { //i.e. if you haven't yet set up the Elem:
            // Set up the pseudo-relation Elem for cur[0].
            // Note that, for left-recursion, is_fda is determined by whether
            // cur[1] was found to equal *prnt0it, OR if one of cur[1]'s ANCESTORS
            // was found to equal *prnt0it (via a right-recursion match).
            // I.e. you want to know if *prnt0it is a common ancestor; that
            // determines whether it's child cur[0] is an fda or not.
            // Contrast this to right-recursion above.
            findOrMakeNewPsRelElem(cur[0],
                                   /*is_fda=*/(recres == MFK_direct_equality || recres == MFK_r_indirectmatch/*!!*/),
                                   /*is_lda=*/OUTERMOST, // i.e. if this is the outermost left-recursion call,
                                                         // cur[0] is the original left elem and is thus an lda.
                                   chld[0],
                                   thehousehold);

            // Mark that you've already set up the Elem, in case you encounter any subsequent
            // in this loop.  (Plus, this bool will be used to disable left recursion, since
            // it would be circular for both left and right recursion to find matches
            // for the same pair.)
            finalres = MFK_l_indirectmatch;

            if (recres == MFK_direct_equality) {
              thehousehold->split_froms.insert(cur[1]);
            }
          }
        }
      } //loop over prnt0its
    } //if right recursion didn't find matches
//        std::cout << "\n[...DONE WITH LEFT RECURSION]";

  } //if !RIGHT
#     undef SETUP_PSREL_FOR
#     undef RECURSION_COMMONTPMS

  /////////////////////////////////////////
  // 4. Insert result into cache & return
  /////////////////////////////////////////
  if /*constexpr*/ (INSERTCACHE) {
    // Note its critical use the original cacheinshint inputted param,
    // not the cacheinshintB we created for the recursive calls,
    // as our insertion hint, AND update it with the returned
    // insertion position:
    cacheinshint = _diamonds_resolved_in_pair.insert( cacheinshint, {
      PrntConnPair(cur[0], cur[1]), finalres });
  }
  return finalres;

} //addDiamondTops(...)


//static method
// I think these list comparison functions should return true if you need to
// swap elems, and false if they're fine as is.  I.e. is not just operator<,
// as it is with vector sorting -- or perhaps there's some bug in my code
// that is causing the issue.  In any case this works.
D2L_METHOD(bool)::compareElems(const elem_t& lhs, const elem_t& rhs) {
  return lhs.sortval > rhs.sortval + APP::cost_of_sortchange;
}


template<typename APP>
template<class orig_node_container_t>
void diamonds_to_lightning<APP>::
sort_household(household_t &thehousehold, const orig_node_container_t *orig_node_order/*=nullptr*/) {
  // If the original node ordering has been provided,
  // begin by putting the household in its original order of appearance;
  // any reordering will proceed from that point.
  // (In this
  if /*constexpr*/ (!std::is_same<orig_node_container_t, void>::value) {
    assert(orig_node_order);
    thehousehold.elems().sort(
            sort_by_appearance_in<orig_node_container_t>(*orig_node_order) );
  }


  // Then, use the user-defined implementation to set the sortvals for each elem:
  APP::set_sortvals_for(thehousehold);
  //^ TODO add ifndef NVERBOSE messages before and after


  // If you need to manually add parent sortvals to their children,
  // or if you're debugging and thus need to make sure child sortvals are
  // already greater than their parents:
  if /*constexpr*/ (!APP::SORTVALS_WILL_BE_GEQ_PRNT_SORTVALS
#ifndef NDEBUG
                || true
#endif
                )
  {
    for (auto& elem : thehousehold.elems()) {
      if (elem.child) {
        auto mapiter = _prnt_to_psrelinfo.find(elem.child);
        assert (mapiter != _prnt_to_psrelinfo.end()
                && "Expected child element to have been found!");
        auto chldelemit = mapiter->second;
        assert(chldelemit->thehousehold == elem.thehousehold
               && "Expected child element to be in same household!");

        if /*constexpr*/ (!APP::SORTVALS_WILL_BE_GEQ_PRNT_SORTVALS)
          chldelemit->sortval += elem.sortval;
        else
          assert(chldelemit->sortval > elem.sortval
                 && "You guaranteed, via SORTVALS_WILL_BE_GEQ_PRNT_SORTVALS, "
                 "to make child sortvals greater than any of their parent's sortvals, "
                 "but you didn't!  (Did you remember to add an epsilon to the child "
                 "if they're equal?");
      } //if elem has a child
    } //for each elem in thehousehold
  } //if /*constexpr*/

//    thehousehold.elems().reverse(); //HACK
  thehousehold.elems().sort(compareElems);

} //sort_household(...)




D2L_METHOD(void)::make_prntconn_adjustments(household_t &thehousehold) {

  auto &elems = thehousehold.elems();

  // Will be checked and decremented to avoid checking for matches
  // against joins for which you know you've already processed
  // all elems.
  auto &joined_into_counts = thehousehold.joined_into_counts;

  // Will be checked and decremented to avoid checking for matches
  // against splits for which you know you've already processed
  // all elems.
  auto &split_froms = thehousehold.split_froms;

  // Will be used to determine if you have included a certain split_from
  // yet, and whether it is currently active or not.
  std::map<prntconn_t, bool, typename APP::compare_prntconns> included_splits_active_status;

  // A variable we need for inter-element connections:
  // last_pschain_elemptr = pointer to last pseudo-inheritance chain element.
  elem_t *last_pschain_elemptr = nullptr;

  // Initially we will "iterate" last_pschain_elemptr until we finally get to something
  // that needs to use it as a pseudo-parent (i.e. rending it the head of the pseudo-
  // inheritance chain); after that point, we will set this to true and then only "iterate"
  // last_pschain_elemptr as we pseudo-connect new elements into it.
  bool pschain_head_set = false;

  assert(!elems.begin()->parent_moved_into_pschain
         && "Expected this to be default-initialized to false!");

#ifndef NVERBOSE
  APP::debug("(DEBUG NOTE: processing a new household's elems; check "
             "for expected sorting order below...");
#endif

  for (auto elemit = elems.begin();
       elemit != elems.end();
       ++elemit)
  {



    // (We will condition step 2 below on this variable,
    // but may modify it in step 1:)
    // If one of the current element's parents within the household
    // was moved into the pseudo inheritance chain, thereby breaking
    // the current's element's inheritance from it in the process,
    // the current element must also be incorporated into the
    // pseudo inheritance chain to reestablish the inheritance.
    bool must_incorporate_into_pschain = elemit->parent_moved_into_pschain;

    node_t *curnode = APP::getnode(elemit->connectiondata);
#ifndef NVERBOSE
    APP::debug("--Processing node: ", curnode, "...");
#endif
//        std::cout << "\nProcessing curnode: " << curnode->name() << "...";
//        std::cout << "\n\t is_fda/is_lda: " << elemit->is_fda << "/" << elemit->is_lda;

    ///////////////////////////////////////////////////////////////
    // 1. Fix arrows from common 'split' nodes INTO this household
    ///////////////////////////////////////////////////////////////
    SmallVector<prntconn_t, 4> splits_to_remove = {}; //Used in steps 1 and 2.
    if (elemit->is_fda) {
      // This element was marked as a first distinct ancestor.
      // Check the split_froms: for any that have already been included
      // AND are present in this elem's bases, REMOVE them from
      // those bases, and activate them.
      // If they are present in this node's bases
      // but HAVEN'T already been included, add them to
      // already_included_splits.

      // Compare each of the split_froms and each prntconn of curnode.
      // If there is a match, determine if the split_from has already been included.
      // If so, remove it and add an activation, and mark that we need to incorporate
      // this element into the inter-elem pseudo-inheritance chain (step 2 below).
      // If not, mark it as already included.
      // If there is NOT a match, check if that split from is active,
      // and if so, deactivate it.
      //
      for (auto s : split_froms) {
        auto inclsplits_mapres = included_splits_active_status.find(s);
        bool s_wasprevincluded = (inclsplits_mapres != included_splits_active_status.end());
        bool &inclsplits_mapres_isactive_ref = inclsplits_mapres->second;
        auto s_prntit = APP::find_prntconn(s, curnode);
        if (s_prntit != APP::get_prntconns_end(curnode)) {
//              std::cout << "\n\t FOUND splitfrom: " << s_prntit->theclass()->name();
          // s IS among the current elem's bases.
          // -- if it has been included but is inactive, activate it.
          // -- if it hasn't been included yet, mark it as included and activated.
          //    No need to change connections.

          if (s_wasprevincluded) {
            // s has already been included -- remove it here to break the diamond.
            // BUT we can't remove it yet because that would screw up our loop,
            // so just flag it to be removed after the loop
            splits_to_remove.emplace_back(*s_prntit);

            // Mark that in step two we need to connect this into the pseudo
            // inheritance chain (to restore a connection to the parent
            // we just removed):
            must_incorporate_into_pschain = true;
            // If the previously-included split is currently inactive, activate it:
            if (!inclsplits_mapres_isactive_ref) {
              APP::activate_ancestor(s, curnode);
              inclsplits_mapres_isactive_ref = true;
            }
          } else {
            // s has not yet been included -- no need to adjust connections, just
            // mark that s has been included and is active, by adding a map entry:
            included_splits_active_status[s] = true;
          }
          assert( included_splits_active_status.find(s)->second);
        }
        // COMMENTED THE BELOW BRANCH OUT, because it is not right -- all you need to
        // worry about are deactivating/reactivating elems within the pseudo-inheritance chain
        // (step 2) whenever there is a gap relative to the original inheritance; that should
        // automatically take care of any split_from deactivations.
//            else {
//              // s is NOT among the current elem's bases.
//              // --if it has not yet been included or has been included but is not
//              //   active, you needn't do anything.
//              // --But if it's active, you need to de-activate it here.
//              if (s_wasprevincluded && inclsplits_mapres_isactive_ref) {
//                // TODO is it right to not have any exceptions here?  Need to test...
//                APP::deactivate_parent(s, curnode, /*exceptions=*/{});
//                inclsplits_mapres_isactive_ref = false;
//              }
//              assert(!included_splits_active_status.find(s)->second);
//            }
      } //for loop over split_froms

      // Remove all the splits from curnode that you marked above.
      // (We will reactivate them in step 2 after reestablishing
      // inheritance with the pseudo-connection)
      for (auto prntconn : splits_to_remove) {
        auto numbases = curnode->bases().size();
        APP::remove_prntconn(prntconn, curnode);
        assert(curnode->bases().size() == numbases - 1);
      }

    } //if elem is fda


    /////////////////////////////////////////////////////////////////
    // 2. Set up inter-elem pseudo-inheritance WITHIN this household
    /////////////////////////////////////////////////////////////////
    if (must_incorporate_into_pschain) {
      assert(last_pschain_elemptr);
      // We should have a last_pschain_elemptr nonnull up to date.
      // We may need to connect the current elem to
      // the last_pschain_elemptr before iterating:
      //  a) if last_pschain_elemptr has a true child and that child already
      //    is the current elem, no changes required.
      //  b) if not, need create a pseudo-parent connection,
      //    AND if last_pschain_elemptr had a nonnull but different child,
      //    need to mark that that child element needs to be incorporated
      //    into the chain when we subsequently encounter it (since we will
      //    be "cutting it off" from its connection to last_pschain_elemptr
      //    during this iteration).
      bool need_lpce2cur_pseudoconn = true; //may be changed to false below
      if (auto lpce_chld = last_pschain_elemptr->child) {
        // last_pschain_elemptr->child is nonnull.
        // I.e. this is NOT one of the "root" ldas of this household.
        // (a) Find the last_pschain_elemptr's child iterator within the household.
        //     (so we get the Elem data, not just the prntconn_t -- we need it for
        //      setting the Elem::parent_moved_into_pschain field below.)
        assert(_prnt_to_psrelinfo.count(lpce_chld));
        decltype(elemit) lpce_chld_it = _prnt_to_psrelinfo[lpce_chld];
        assert(&*lpce_chld_it->thehousehold == &thehousehold);

        // (b) If it is not already equal to the current element,
        // pseudo-insert the current element into the chain at that point:
        if (lpce_chld_it != elemit) {
          // i. Remove connection from last_pschain_elemptr to ITS child
          //    and replace it with an activation:
          node_t *lpce_chld_node = APP::getnode(lpce_chld);
          auto prntit = APP::find_prntconn(last_pschain_elemptr->connectiondata, lpce_chld_node);
          assert(prntit != APP::get_prntconns_end(lpce_chld_node));
          APP::remove_prntconn(prntit, lpce_chld_node);
          APP::activate_ancestor(last_pschain_elemptr->connectiondata, lpce_chld_node);

          // ii. Mark that one of lpce_chld_it's parents within the household was moved into
          // the pseudoinheritance chain, breaking the inheritance connection -- this will
          // signal that we need to also move lpce_chld_it into the pseudoinheritance chain
          // to reestablish inheritance.  (I.e. by marking this we will reestablish the
          // connection we just broke via remove_prntconn(...) above).
          lpce_chld_it->parent_moved_into_pschain = true;
        } else {
          need_lpce2cur_pseudoconn = false;
          // last_pschain_elemptr's child is already the current elem;
          // no connection adjustments needed here or in lpce_chld.
          assert(!lpce_chld_it->parent_moved_into_pschain
                 && "Expected this to have been default initialized to false!");
        }
      } //if lpce has a nonnull child
      if (need_lpce2cur_pseudoconn) {
        // Add a connection from last_pschain_elemptr's node to cur elem,
        //    then immediately deactivate it:
        prntconn_t psprntconn = APP::make_pseudoprntconn(last_pschain_elemptr->connectiondata);
        APP::add_prntconn(psprntconn, curnode);
        APP::deactivate_parent(psprntconn, curnode, /*exceptions vec=*/std::move(splits_to_remove));
        // Be sure not to reference splits_to_remove after this!!
      }

      // (c) 'Iterate' last_pschain_elemptr by setting it to the current element.
      pschain_head_set = true;
      last_pschain_elemptr = &*elemit;
#ifndef NVERBOSE
      APP::debug(" (New last_pschain_elem's connectiondata: ", last_pschain_elemptr->connectiondata, ")");
#endif
    } //if must_incorporate_into_pschain
    else {
      // You don't need to connect into the pseudo-inheritance chain.
      // But, if you haven't set the pschain head yet, you need to
      // iterate.
      // I.e. the one circumstance you DON'T iterate last_pschain_elemptr
      // is if (!must_incorporate_into_pschain && pschain_head_set).
      if (!pschain_head_set) {
        last_pschain_elemptr = &*elemit;
        assert(last_pschain_elemptr);
      }
    }


    //////////////////////////////////////////////////////////////
    // 3. Fix arrows FROM this household into common 'join' nodes
    //////////////////////////////////////////////////////////////
    if (elemit->is_lda) {
      // This element was marked as a last distinct ancestor,
      // meaning that at least one of the joined_intos for this
      // household inherits from it AND from another element of
      // the household.
      // For each joined_into whose count is at least 1 (indicating
      // we will encounter it later in the element loop), loop over
      // its parents, determining if this is elem among them.  If it is,
      // remove it.
      for (auto jit = joined_into_counts.begin(); jit != joined_into_counts.end(); ++jit) {
        // Skip over the jits whose count has reached one: they don't need
        // any changes.
        if (jit->second) {
          // For those with counts of at least one, check if the cur elem is among
          // their bases:
          node_t *jnode = jit->first;
          auto prntit = APP::find_prntconn(elemit->connectiondata, jnode);

          if (prntit != APP::get_prntconns_end(jnode)) {
            // This node IS a parent of j, but is NOT the last it will encounter;
            // we thus must remove that prntconn and replace it w/an activation:
            APP::remove_prntconn(prntit, jnode);
//                assert(APP::find_prntconn(elemit->connectiondata, jnode) == APP::get_prntconns_end(jnode));
            APP::activate_ancestor(elemit->connectiondata, jnode);
            // And don't forget to decrement the count, indicating there is one
            // less diamond-causing parent of j to process:
            --jit->second;
          } //if the join node has THIS elem as a parent
        } //if the join node has at least two prntconns from elems still not processed
      } //for each joined_into
    } //if elem is lda

  } //for each element

} //make_prntconn_adjustments(...)



template<typename APP>
template<class orig_node_container_t>
void diamonds_to_lightning<APP>::
finish(household_t &thehousehold,
        const orig_node_container_t *orig_node_order/*=nullptr*/) {
  sort_household(thehousehold, orig_node_order);
  make_prntconn_adjustments(thehousehold);
}

D2L_METHOD(void)::sort_all_households() {
  for (auto& h : _households)
    sort_household(h);
}

D2L_METHOD(void)::make_all_prntconn_adjustments() {
  for (auto& h : _households)
    make_prntconn_adjustments(h);
}

template<typename APP>
template<class orig_node_container_t>
void diamonds_to_lightning<APP>::
finish_all(const orig_node_container_t *orig_node_order/*=nullptr*/) {
  for (auto& h : _households)
    finish(h, orig_node_order);
}

/// Note that if somehow you formed circular parent connections,
/// this would go into an infinite loop I think....so if that's possible
/// need to check for such circularity before this.
template<typename APP>
template<class orig_node_container_t>
void diamonds_to_lightning<APP>::
load_adjusted_and_ordered_nodes_into(node_t *curnode,
                                     std::set<node_t *> &already_included,
                                     orig_node_container_t &res,
                                     const orig_node_container_t &orig_node_order) const
{
  if (unsigned numprnts = std::distance(
          APP::get_prntconns_begin(curnode),
          APP::get_prntconns_end(curnode))) {
    // 1. Fill a temporary vector with the prnts:
    std::vector<node_t *> prntnodes;
    prntnodes.reserve(numprnts);
    for (auto prntit = APP::get_prntconns_begin(curnode);
         prntit != APP::get_prntconns_end(curnode);
         ++prntit)
      prntnodes.push_back(APP::getnode(*prntit));

    // 2. Sort this temporary vector by the node's original appearance
    // in orig_node_order (this is to minimize any problems created due
    // to unnecessary re-sorting -- e.g. if there are dependencies that
    // exist outside of base relationships.  We can't guarantee we'll avoid
    // any such issues, but we'll try to avoid any unnecessary ones.
    std::sort(prntnodes.begin(), prntnodes.end(),
            sort_by_appearance_in<orig_node_container_t>(orig_node_order) );

    // 3. Make recursive calls on the members of the vector in order
    for (auto prntnode : prntnodes) {
      if (!already_included.count(prntnode)) {
        load_adjusted_and_ordered_nodes_into(prntnode, already_included, res, orig_node_order);
      }
    }
  }
   //4. Handle the current node:
  res.push_back(curnode);
  already_included.insert(curnode);
}

template<typename APP>
template<class orig_node_container_t>
void diamonds_to_lightning<APP>::reorder_nodes(orig_node_container_t &container) const {
  std::set<node_t *> already_included = {};
  orig_node_container_t res;
  res.reserve(_addednodes.size());
  for (auto node : container) {
    assert(node);
    if (!already_included.count(node)) {
      load_adjusted_and_ordered_nodes_into(node, already_included, res, container);
    }
  }
  container = res;
}

template<typename APP>
template<class orig_node_container_t>
void diamonds_to_lightning<APP>::finish_all_and_reorder_nodes(orig_node_container_t &container) {
  finish_all(&container);
  reorder_nodes(container);
}


    

} //namespace dwr


#undef D2L_METHOD

#endif /* diamonds_to_lightning_inl */
