//
//  diamonds_to_lightning.h
//  diamonds_to_lightning
//
//  Created by David Rector on 5/31/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef diamonds_to_lightning_h
#define diamonds_to_lightning_h

#include <map>

#include "psrel_household_elem.h"


namespace dwr {

  template<typename APP>
  class diamonds_to_lightning : protected APP {
    using prntconn_t = typename APP::prntconn_t;
    using node_t = typename APP::node_t;
    using household_t = psrel::Household<prntconn_t, node_t, typename APP::sortval_t, typename APP::compare_prntconns>;
    using elem_t = typename household_t::Elem;

    /*DATA*/std::set<node_t *> _addednodes; /// only needed/used if APP::CHECK_ANCESTORS
    /*DATA*/typename household_t::container_t _households;

    using households_iter_t = typename household_t::iter_t;
    using elemiter_t = typename household_t::elemiter_t;

    auto createNewHousehold() -> households_iter_t; //cpp
    auto mergeHouseholdsIfDiff(households_iter_t a, households_iter_t b) -> households_iter_t; //cpp

    /*DATA*/std::map<prntconn_t, elemiter_t, typename APP::compare_prntconns> _prnt_to_psrelinfo;

    /// If b is NOT found with _prnt_to_psrelinfo,
    /// creates a new entry and returns it.
    void findOrMakeNewPsRelElem(prntconn_t b,
                                bool is_fda, bool is_lda,
                                prntconn_t child,
                                households_iter_t &thehousehold/*ncref*/);


    struct PrntConnPair {
      prntconn_t first;
      prntconn_t second;
      PrntConnPair(prntconn_t _first, prntconn_t _second); //cpp
      bool operator==(const PrntConnPair &other) const; //cpp
      bool operator<(const PrntConnPair &other) const; //cpp
      bool operator!=(const PrntConnPair &other) const { return !(*this == other); }
      bool operator<=(const PrntConnPair &other) const { return (*this == other) || (*this < other); }
      bool operator>(const PrntConnPair &other) const  { return !(*this <= other); }
      bool operator>=(const PrntConnPair &other) const { return !(*this < other);  }
    };

    enum MatchFoundKind {
      MFK_none = 0,
      MFK_direct_equality,
      MFK_r_indirectmatch,
      MFK_l_indirectmatch
    };
    /// This is what we refer to as the "cache":
    /*DATA*/std::map<PrntConnPair, MatchFoundKind> _diamonds_resolved_in_pair;

    using cache_iter_t = decltype(_diamonds_resolved_in_pair.begin());

    //METHODS:

    template< unsigned INSERTCACHEEVERY = APP::INSERTCACHEEVERY //TODO doc
            , unsigned CHECKCACHEEVERY = APP::CHECKCACHEEVERY //TODO doc
            , unsigned ICACHEITER = 0
            , unsigned CCACHEITER = 0
            , bool OUTERMOST = true
            , bool RIGHT = false >
    auto addDiamondTops(households_iter_t &thehousehold, /*ncref*/
                        cache_iter_t &cacheinshint, /*ncref*/
                        prntconn_t cur[2],
                        prntconn_t chld[2] = (prntconn_t[2]){prntconn_t(), prntconn_t()} )
        -> MatchFoundKind; //cpp
    static bool compareElems(const elem_t& lhs, const elem_t& rhs);
    template<class orig_node_container_t/*e.g. std::vector<node_t *>*/>
    void load_adjusted_and_ordered_nodes_into(node_t *curnode,
                                              std::set<node_t *> &already_included,
                                              orig_node_container_t &res,
                                              const orig_node_container_t &orig_node_order) const;
  protected:
    template<class orig_node_container_t = void>
    void sort_household(household_t &thehousehold,
            const orig_node_container_t *orig_node_order = nullptr); //cpp
    void sort_all_households(); //cpp
    void make_prntconn_adjustments(household_t &thehousehold); //cpp
    void make_all_prntconn_adjustments(); //cpp
  public:
    void addnode(node_t *node); //cpp

    template<class orig_node_container_t = void>
    void finish(household_t &thehousehold,
            const orig_node_container_t *orig_node_order = nullptr); //cpp

    template<class orig_node_container_t = void>
    void finish_all(const orig_node_container_t *orig_node_order = nullptr); //cpp
#ifndef NDEBUG
    decltype(_households) &households() const { return _households; }
#endif
    bool diamonds_detected() const { return !_households.empty(); }

    template<class orig_node_container_t /*e.g. std::vector<node_t *>*/>
    void reorder_nodes(orig_node_container_t &vec) const;

    template<class orig_node_container_t>
    void finish_all_and_reorder_nodes(orig_node_container_t &vec);

    diamonds_to_lightning(APP&& app = {}) : APP(std::move(app)) {}
  };

} //namespace dwr

#include "diamonds_to_lightning.inl"



#endif /* diamonds_to_lightning_h */
