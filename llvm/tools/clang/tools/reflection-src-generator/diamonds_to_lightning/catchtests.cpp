//
//  catchtests.cpp
//  diamonds_to_lightning
//
//  Created by David Rector on 5/29/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#include "catch.hpp"
//#define NDEBUG_IMPLEM
#include "test_helpers.h"


TEST_CASE("Diamonds to lightning") {
  
  /*
   TEST CASES TODO (and some functionality you still need to incorporate):
   --For the exising case, add in some tests about elems being sorted before their children
      (i.e. BB1 before B, CC1 before C).
   --Sections for simple diamonds, getting progressively more complex, leading up to the existing Stacked diamonds case.
   --Cases with triangles, and cases where the app has different static params -- e.g. CHECK_ANCESTORS = true etc.
      --Perhaps good to set up an APP base, then have the cpp_inheritance_fixer inherit from that, and add template params for CHECK_ANCESTORS and sorting functions etc.
   --Cases where the diamonds arise from splits with different accesses
   --Households with intermediary nodes, i.e. non-lda, non-fda nodes -- make sure they are registered as such.
   --Cases where you have multiple households
   --Cases with multiple households that nonetheless share some of their join nodes (so that you'll be getting a few different independent diamond fixes)
   --Sorting functions
   --Get the diamondres divider functionality working (each household needs one, it can have a sortval assigned to it to put it further back in the elem list but it should default to be at the front, and while adjusting connections, until you reach it you don't fix diamonds), then add test cases.
   */
  
  using APP = D2L_cpp_class_inheritance;
  
  SECTION("Stacked diamonds, multiple splits and joins") {
    std::vector<ClassDecl *> classdecls;
    MKCLASSDECL(AA1)
    MKCLASSDECL(BB1);
    MKCLASSDECL(CC1);
    add_base_to(AA1, BB1);
    add_base_to(AA1, CC1);
    MKCLASSDECL(A1);
    MKCLASSDECL(A2);
    MKCLASSDECL(CC2);
    MKCLASSDECL(B);
    add_base_to(A1, B);
    add_base_to(A2, B);
    add_base_to(BB1, B);
    MKCLASSDECL(C);
    add_base_to(CC1, C);
    add_base_to(CC2, C);
    add_base_to(A1, C);
    add_base_to(A2, C);
    MKCLASSDECL(DD1);
    MKCLASSDECL(D1);
    MKCLASSDECL(DD2);
    MKCLASSDECL(D2);
    add_base_to(B, D1);
    add_base_to(DD1, D1);
    add_base_to(C, D1);
    add_base_to(DD2, D2);
    add_base_to(B, D2);
    add_base_to(C, D2);
    
    SECTION("Testing number of active bases BEFORE d2l") {
#define ACTIVE_BASE_REQUIREMENTS\
      REQUIRE(only_activebases_are(AA1, {}));\
      REQUIRE(only_activebases_are(BB1, {AA1}));\
      REQUIRE(only_activebases_are(CC1, {AA1}));\
      REQUIRE(only_activebases_are(CC2, {}));\
      REQUIRE(only_activebases_are(A1, {}));\
      REQUIRE(only_activebases_are(A2, {}));\
      REQUIRE(only_activebases_are(B, {BB1, AA1, A1, A2}));\
      REQUIRE(only_activebases_are(C, {CC1, CC2, AA1, A1, A2}));\
      REQUIRE(only_activebases_are(DD1, {}));\
      REQUIRE(only_activebases_are(D1, {DD1, C, B, CC2, CC1, BB1, AA1, A2, A1}));\
      REQUIRE(only_activebases_are(DD2, {}));\
      REQUIRE(only_activebases_are(D2, {DD2, C, B, CC2, CC1, BB1, AA1, A2, A1}));\
/**/
      
      ACTIVE_BASE_REQUIREMENTS
    }
    
    diamonds_to_lightning<APP> d;
    reload_nodes_into(classdecls, d);
    
    SECTION("Implementation details (only needed if subsequent section has failures)") {
      REQUIRE(d.households().size() == 1);
      
      auto &thehousehold = d.households().front();
      REQUIRE(thehousehold.split_froms.size() == 3);
      
      REQUIRE(thehousehold.joined_into_counts.size() == 2);
      
      auto &elems = thehousehold.elems();
      REQUIRE(is_in_elems(BB1, elems));
      REQUIRE(is_in_elems(CC1, elems));
      REQUIRE(is_in_elems(B, elems));
      REQUIRE(is_in_elems(C, elems));
      REQUIRE(elems.size() == 4);
      REQUIRE( getelemit(BB1, elems)->is_fda);
      REQUIRE(!getelemit(BB1, elems)->is_lda);
      REQUIRE( getelemit(CC1, elems)->is_fda);
      REQUIRE(!getelemit(CC1, elems)->is_lda);
      REQUIRE( getelemit(B, elems)->is_fda);
      REQUIRE( getelemit(B, elems)->is_lda);
      REQUIRE( getelemit(C, elems)->is_fda);
      REQUIRE( getelemit(C, elems)->is_lda);
    }
    
    d.finish_all_and_reorder_nodes(classdecls);
    
    SECTION("Testing number of active bases AFTER d2l -- should be the same") {
      ACTIVE_BASE_REQUIREMENTS
    }
#undef ACTIVE_BASE_REQUIREMENTS
  }
  
} //TEST_CASE("Diamonds to lightning")

/*
 Okay
 */
