//
//  psrel_household_elem.h
//  diamonds_to_lightning
//
//  Created by David Rector on 5/30/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef psrel_household_elem_h
#define psrel_household_elem_h

#include <set>
#include <list>

namespace dwr {

  //Forward decl so the friend decl will work
  template<typename APP>
  class diamonds_to_lightning;
  
  namespace psrel {
    
    template<typename prntconn_t, typename node_t, typename sortval_t, typename compare_prntconns>
    struct Household {
      using container_t = std::list<Household>;
      using iter_t = typename container_t::iterator;
      
      struct Elem {
        /// This will always be default constructed; it will be up to the user to
        /// set it properly for each elem in their set_sortvals_for(...) function.
        /// Any sortval must be >= any of its parents sortvals; the user has
        /// the option, via the SORTVALS_ALREADY_INCLUDE_PARENTSORTVALS static
        /// field in their APP implementation, to either 0) have our algorithm
        /// add parent sortvals to their children, interpreting sortvals as "delta"
        /// sortvals, or 1) to have our algorithm simply assert that each child >=
        /// their parents.
        sortval_t sortval = {};
        
        /// The connection which corresponds uniquely to this Elem.  In fact,
        /// its node, obtained via getnode(connectiondata), corresponds uniquely
        /// to this Elem -- i.e. for any two Elems elemA and elemB, no matter
        /// whether or not they are in the same household, it is assured that
        /// getnode(elemA.connectiondata) != getnode(elemB.connectiondata).
        prntconn_t connectiondata;
//        operator prntconn_t() { return connectiondata; }
        operator node_t *() { return static_cast<node_t *>(connectiondata); }
        // ^ To assist with a sorting functor
        
        /// This node's child elem within the household.  May be null.
        /// The elems list of a hosuehold will naturally consist of at least two
        /// 'single-child' trees: i.e. each elem may have at most one other elem
        /// within its tree to which it serves as a true parent.
        /// Note that we do not refer to the ultimate join nodes of a household
        /// as children, nor do we even count them among the Elems of the household
        /// -- they belong to a separate set of "joined_into" nodes.
        /// @code
        ///   //        A1
        ///   //      /    \
        ///   //     /  A2  \
        ///   //    /  /  \  \
        ///   //   | B2    C2 |
        ///   //  B1  | A3 |  C1
        ///   //    \ |/  \| /
        ///   //     B3    C3
        ///   //       \  /
        ///   //        D
        ///   struct A1 {};           //split node
        ///   struct B1 : A1 {};      //child = B3, fda
        ///   struct C1 : A1 {};      //child = C3, fda
        ///   struct A2 {};           //split node
        ///   struct B2 : A2 {};      //child = B3, fda
        ///   struct C2 : A2 {};      //child = C3, fda
        ///   struct A3 {};           //split node
        ///   struct B3 : A3, B1, B2 {};  //child = null, fda & lda
        ///   struct C3 : A3, C1, C2 {};  //child = null, fda & lda
        ///   struct D : B3, C3 {};   //join node (NOT a child of B3/C3)
        /// @endcode
        /// Note that any lda node (see below) will have a null child.
        prntconn_t child;
        
      private:
        template<typename APP>
        friend class dwr::diamonds_to_lightning;
        
        /// ONLY the diamonds_to_lightning::finish function should use this; it is only meaningful
        /// AFTER sorting has been performed, and is set during the finish function.
        unsigned parent_moved_into_pschain : 1;
        
      public:
        /// Is this node a first distinct ancestor -- i.e. at least one of its
        /// immediate parents is a split node of its household?
        /// @code
        ///   //      A
        ///   //   /  |  \
        ///   //  B   C   D
        ///   //  |   |   |
        ///   // BB  CC   |
        ///   //  |   |   |
        ///   // BBB CC2  |
        ///   //  |   |   |
        ///   //  |  CCC  |
        ///   //   \  |  /
        ///   //      E
        ///   struct A {};                //split node
        ///   struct B : A {};            //fda
        ///   struct BB : B {};
        ///   struct BBB : BB {};         //lda
        ///   struct C : A {};            //fda
        ///   struct CC : C {};
        ///   struct CC2 : CC {};
        ///   struct CCC : CC2 {};        //lda
        ///   struct D : A {};            //lda & fda
        ///   struct E : BBB, CCC, D {};  //join node
        /// @endcode
        /// Note that a node can be initially marked non-fda, but determined
        /// later to be an fda of a different split node.
        unsigned is_fda : 1;
        
        /// Is this node a last distinct ancestor -- i.e. one of the join
        /// nodes of its household inherits directly from it (and another
        /// lda-marked node in the household)?  (See is_fda doc for diagram.)
        /// Note that a node can be initially marked non-lda, but determined
        /// later to be an lda of a different join node.
        unsigned is_lda : 1;
        
        /// There will be one element within each household, initialized to be
        /// the front elem of each household, that may be sorted into the
        /// diamond resolution chain, and is meaningless except to provide
        /// this guarantee: NO changes will be made to the parents/ancestors of
        /// elements to its left in the household, EVEN IF that leaves diamonds
        /// unresolved, whereas changes MAY be made to the elements to its right
        /// (though it is possible that e.g. the first few may not need any
        /// such changes).
        /// Should the user require all diamonds to be resolved, this should be
        /// left as the leftmost elem (by leaving the special comparator function
        /// that handles this elem to its default base implem.)
        /// Whenever there are diamonds to the left of this elem, the user-definable
        /// "setup_unresolved_diamond" function will be called.
        /// The elem for which this field is true will have null connectiondata,
        /// a null child, etc.
        unsigned is_diamondres_divider_lessleave_greaterfix : 1;
        
      private:
        typename Household::iter_t thehousehold;
      public:
        Elem(prntconn_t connectiondata,
             bool is_fda,
             bool is_lda,
             prntconn_t child,
             decltype(thehousehold) thehousehold)
        : sortval()
        , connectiondata(connectiondata)
        , child(child)
        , parent_moved_into_pschain(false)
        , is_fda(is_fda)
        , is_lda(is_lda)
        , is_diamondres_divider_lessleave_greaterfix(false)
        , thehousehold(thehousehold)
        {}
      }; //struct Elem
      
    private:
      std::list<Elem> _elems = {};
    public:
      using elemiter_t = decltype(_elems.begin());
    public: //TEMP/FIXME can't get friend decl to work so making these public for now...
      template<typename APP>
      friend class diamonds_to_lightning;
      elemiter_t _inspos;
    public:
            decltype(_elems) &elems()       { return _elems; }
      const decltype(_elems) &elems() const { return _elems; }
#ifdef NDEBUG_IMPLEM
    private: //I.e. leave this stuff public when you are debugging implem details.
#endif
      /// The set of parents which are shared by at least two elems
      /// in this household.
      /// All counts will be decremented to zero during the connection
      /// adjustment phase of the algorithm.
      /// @example
      ///  @code
      ///   //    A1
      ///   //   /  \
      ///   //  / A2 \
      ///   //  |/  \|
      ///   //  B    C
      ///   //   \  /
      ///   //    D
      ///   class A1 {};
      ///   class A2 {};
      ///   class B : A1, A2 {};
      ///   class C : A1, A2 {};
      ///   class D : B, C {};
      ///  @endcode
      ///  B and C will both have Elems in a shared household whose split_froms
      ///  will include A1 and A2.
      std::set<prntconn_t, compare_prntconns> split_froms;
      
      /// The set of nodes whose parents include at least two Elems
      /// in this household, mapped to the number of connections of
      /// such connections that still need to be removed (for the
      /// purpose of fixing diamonds).  (TODO what about diamonds
      /// you want to leave in place -- should you decrement for those?
      /// If so make a note of that here.)
      /// All counts will be decremented to zero during the connection
      /// adjustment phase of the algorithm.
      /// @example
      ///  @code
      ///   //     A
      ///   //   / | \
      ///   //  B  E  C
      ///   //  |\ | /|
      ///   //  | D1  |
      ///   //   \    |  Y
      ///   //     \  | /
      ///   //       D2
      ///   class A {};
      ///   class E : A {};
      ///   class Y {}; //(not in household)
      ///   class B : A {};
      ///   class C : A {};
      ///   class D1 : B, C, E {};
      ///   class D2 : B, C, Y {};
      ///  @endcode
      ///  B, C, and E will both have Elems in a shared household, which will
      /// have D1 and D2 in its joined_into_counts map.  D1 will initially have,
      /// count 2, D2 will have count 1.  Both will be decremented to 0
      /// over the course of make_prntconn_adjustments.
      std::map<node_t *, unsigned> joined_into_counts;

      Household() {
        _inspos = _elems.end();
      }
    }; //struct Household
    
  } //namespace psrel
  
} // namespace dwr

#endif /* psrel_household_elem_h */

