//
//  main.cpp
//  diamonds_to_lightning
//
//  Created by David Rector on 5/28/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#include <iostream>
#include "test_helpers.h"

using namespace dwr;

int main(int argc, const char * argv[]) {
  
  using APP = D2L_cpp_class_inheritance;
  
  diamonds_to_lightning<APP> d;
  
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
  MKCLASSDECL(D2);
  add_base_to(B, D1);
  add_base_to(DD1, D1);
  add_base_to(C, D1);
  add_base_to(B, D2);
  add_base_to(C, D2);

//  APP::remove_prntconn(&C, &D2);
  
  reload_nodes_into(classdecls, d);
  
  auto &thehousehold = d.households().front();
  
  
  assert(thehousehold.split_froms.size() == 3);
  assert(thehousehold.elems().size() == 4);
  assert(thehousehold.joined_into_counts.size() == 2);

  assert(is_in_elems(CC1, thehousehold.elems()));
  assert(getelemit(CC1, thehousehold.elems())->is_fda);
  
  d.finish_all_and_reorder_nodes(classdecls);
  
  std::cout << "\n\nElems: ";
  for (auto e : thehousehold.elems()) {
    std::cout << e.connectiondata.theclass()->name() << ", ";
  }
  std::cout << "\nSplit froms: ";
  for (auto e : thehousehold.split_froms) {
    std::cout << e.theclass()->name() << ", ";
  }
  std::cout << "\nJoined intos: ";
  for (auto e : thehousehold.joined_into_counts) {
    std::cout << e.first->name() << ", ";
  }
  std::cout << "\n\n";
  
  
  std::cout << activebases2str(D2) << "\n";
  
  
  
  for (auto c : classdecls)
    c->print(std::cout);
  
  reload_nodes_into(classdecls, d);
  assert(!d.diamonds_detected());
  return 0;
}



/*
 
 */


