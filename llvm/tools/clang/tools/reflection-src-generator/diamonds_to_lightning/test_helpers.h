//
//  test_helpers.h
//  diamonds_to_lightning
//
//  Created by David Rector on 6/6/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef test_helpers_h
#define test_helpers_h

#include "diamonds_to_lightning.h"
#include "cpp_multiinheritance_fixer.h"

/*
 PICK UP HERE NEXT:
 --Then, set up some test cases and debug all the problems.
 --Then, set up the set_sortval implem, and set up real implems of the
 --Then get the diamondres divider working too.
 */

using namespace dwr;



#define MKCLASSDECL(MP_name) \
ClassDecl MP_name = ClassDecl(#MP_name); \
classdecls.push_back(&MP_name);
/**/

void add_base_to(ClassDecl &baseclass, ClassDecl &theclass) {
    theclass.bases().emplace_back(BaseSpecifier(&baseclass));
}

bool is_prnt_of(ClassDecl &baseclass, ClassDecl &theclass) {
    for (auto& b : theclass.bases())
        if (b.theclass() == &baseclass)
            return true;
    return false;
}

/*
 Okay how should this work?
 1) First check if there's an activation for it among the class's decls - if so return true.  Also check if there is a deactivation - if so return false.
 2) Then, check if there's a base for it among the class's bases - if so return true.  Then call it recursively on each base; if any are true you return true, I think (i.e., activations should override deactivations)
 */
bool is_active_in(ClassDecl &baseclass, ClassDecl &theclass) {
    // 1. Check if there's an activation or deactivation in theclass corresponding
    // to baseclass; if so return true or false, respectively.  (Note that if there
    // is an activation or deactivation, that should imply we should find baseclass
    // somewhere among theclass's ancestors -- but we'll leave that assert out.)
    for (auto d : theclass.decls()) {
        if (auto ad = d->getAsActivationDecl()) {
            if (ad->theclass() == &baseclass)
                return true;
        } else if (auto dd = d->getAsDeactivationDecl()) {
            if (dd->theclass() == &baseclass)
                return false;
        }
    }
    // 2. Next, see if there's a simple parent relationship between them (with
    // no intervening activations):
    if (is_prnt_of(baseclass, theclass))
        return true;
    
    // 3. Finally, call recursively on the bases; the first time you hit true, return it,
    // since activations should override any deactivations.
    for (auto& b : theclass.bases()) {
        if (is_active_in(baseclass, *b.theclass()))
            return true;
    }
    // baseclass isn't among theclass's ancestors at all:
    return false;
}

/*
 Hmmm, do we need to
 */

//void mark_all_ancestors_inactive(ClassDecl *theclass, std::map<ClassDecl *, bool> &base_activestatus_map) {
//    for (auto& b : theclass->bases()) {
//        base_activestatus_map[b.theclass()] = false;
//        //Call recursively:
//        mark_all_ancestors_inactive(b.theclass(), base_activestatus_map);
//    }
//}

void assemble_base_activestatus_map(ClassDecl *theclass, std::map<ClassDecl *, bool> &base_activestatus_map) {
    // 1. Look for new deactivation decls in the current class, mark them as deactivated, and mark their
    //    exceptions as active PLUS recurse over them.  (We don't recurse over the deactivated ones, because all
    //    we seek from this is to figure which bases are active -- if we omit some inactive ones that's fine.
    //    Recursing over the deactivated ones to complete the map would screw up the logic of this code,
    //    so if you really want that functionality prepare to make other fixes.)
    for (auto d : theclass->decls()) {
        if (auto dd = d->getAsDeactivationDecl()) {
//            std::cout << "[Inactive name] = " << dd->theclass()->name() << "\n";
            // Only process this if there wasn't a map entry in a
            // "previous"/more-derived base than this
            // (since we're recursing from desc -> ancs).
            if (!base_activestatus_map.count(dd->theclass())) {
//                std::cout << "\nDeactivating " << dd->theclass()->name() << "...";
                base_activestatus_map[dd->theclass()] = false;
//                mark_all_ancestors_inactive(dd->theclass(), base_activestatus_map);
                for (auto e : dd->exceptions()) {
//                    std::cout << "\nProcessing exception " << e.theclass()->name() << "...";
                    base_activestatus_map[e.theclass()] = true;
                    assemble_base_activestatus_map(e.theclass(), base_activestatus_map);
                }
            }
            else
                assert( base_activestatus_map[dd->theclass()] &&
                       "Expected this to have been active prior to this deactivation decl");
        }
    }
    
    // 2. For any bases not yet added to the map via deactivation decls,
    //    mark them as active and call recursively on them:
    for (auto& b : theclass->bases()) {
        if (!base_activestatus_map.count(b.theclass())) {
//            std::cout << "\nProcessing base " << b.theclass()->name() << "...";
            base_activestatus_map[b.theclass()] = true;
            //Call recursively on the base:
            assemble_base_activestatus_map(b.theclass(), base_activestatus_map);
        }
//        std::cout << "\nBase " << b.theclass()->name() << " already in map, not processed...";
    }
    
    // 3. Override deactivations encountered in the bases if there is a corresponding activation here:
    for (auto d : theclass->decls()) {
        if (auto ad = d->getAsActivationDecl()) {
//            std::cout << "\nActivating class: " << ad->theclass()->name() << "...";
            assert(!base_activestatus_map.count(ad->theclass()) || !base_activestatus_map[ad->theclass()]
                   && "Expected this to either not yet have an entry, or have an inactive entry, before activation");
            base_activestatus_map[ad->theclass()] = true;
            //Call recursively on the active class:
            assemble_base_activestatus_map(ad->theclass(), base_activestatus_map);
        }
    }
//    std::cout << "\n...DONE assembling bases for class " << theclass->name() << ".";
}
/*
 Okay, the issue seems to be this: on join nodes, we need to be able to perform merge operations, essentially.
 Perhaps the best way to do that is just by assembling separate maps, then merging them.
 Just to be sure though, is it right to consider all of these merged activations independently?  Specifically,
 will each individual merge step only involve activating stuff, not deactivating?  If so we can indeed process them
 one by one in sequence.  And I think that IS the case.  So bottom line we don't need any special solution for merging
 in the same way we needed a special solution for deactivating with exceptions.
 
 So how do we actually do the merge?
 Well, one answer would be to form individual maps and merge them;
    --Specifically, form a result map, add in any deactivations to it, then form separate maps...
 Hold on.  Consider this actually:
    --Forget separate maps, you just have the one result one.
    --You first scan for deactivations, and if they are not already in the map, you add them.
    --Then you scan the bases, add them if not present, and call recursively on them.
    --Lastly, you scan the activations, and set them to active (they should already be in the map), and call recursively on them.
 In this way,
 */

bool only_activebases_are(ClassDecl &theclass, SmallVectorImpl<std::reference_wrapper<ClassDecl>, 8> &&baseclasses) {
    // 1. Assemble the base_activestatus_map
    std::map<ClassDecl *, bool> base_activestatus_map = {};
    assemble_base_activestatus_map(&theclass, base_activestatus_map);
    // 2. Check each baseclasses's value in the map.  If not active, return false.
    //    Otherwise, ERASE the entry.
    for (auto b : baseclasses) {
        auto res = base_activestatus_map.find(&b.get());
        if (res == base_activestatus_map.end()/*not found*/
            || !res->second/*mapped value is false*/)
            return false;
        base_activestatus_map.erase(res);
    }
    // 3. Check each remaining entry in base_activestatus_map.  If any are active, return false.
    for (auto b : base_activestatus_map) {
        if (b.second)
            return false;
    }
    return true;
}

auto activebases(ClassDecl &theclass) -> SmallVectorImpl<std::reference_wrapper<ClassDecl>, 8> {
    SmallVectorImpl<std::reference_wrapper<ClassDecl>, 8> res;
    std::map<ClassDecl *, bool> base_activestatus_map = {};
    res.reserve(base_activestatus_map.size());
    assemble_base_activestatus_map(&theclass, base_activestatus_map);
    for (auto kv : base_activestatus_map) {
        if (kv.second) {
            res.push_back(*kv.first);
        }
    }
    return res;
}

std::string activebases2str(ClassDecl &theclass) {
    std::string res;
    std::map<ClassDecl *, bool> base_activestatus_map = {};
    assemble_base_activestatus_map(&theclass, base_activestatus_map);
    bool isfirst = true;
    res += ("\nActive bases for " + theclass.name() + ": ");
    for (auto kv : base_activestatus_map) {
        if (kv.second) {
            if (isfirst)
                isfirst = false;
            else
                res += ", ";
            res += kv.first->name();
        }
    }
    return res;
}


template<typename elems_t>
bool is_in_elems(ClassDecl &theclass, const elems_t &elems) {
    for (auto elemit = elems.begin(); elemit != elems.end(); ++elemit) {
        if (elemit->connectiondata.theclass() == &theclass)
            return true;
    }
    return false;
}

template<typename elems_t>
bool elems_are(const elems_t &elems, std::vector<std::reference_wrapper<ClassDecl>> theclasses) {
    if (elems.size() != theclasses.size())
        return false;
    for (auto &c : theclasses) {
        if (!is_in_elems(c, elems))
            return false;
    }
    return true;
}

/// Assumes A and B are among elems:
template<typename elems_t>
bool A_is_before_B_in_elems(ClassDecl *A, ClassDecl *B, const elems_t &elems) {
    for (auto elemit = elems.begin(); elemit != elems.end(); ++elemit) {
        if (elemit->connectiondata.theclass() == A)
            return true;
        if (elemit->connectiondata.theclass() == B)
            return false;
    }
    throw "Neither A nor B are in elems!";
}

template<typename elems_t>
auto getelemit(ClassDecl &theclass, const elems_t &elems) {
    for (auto elemit = elems.begin(); elemit != elems.end(); ++elemit) {
        if (elemit->connectiondata.theclass() == &theclass)
            return elemit;
    }
    throw "Not in elems!";
}


template<typename VEC>
void reload_nodes_into(VEC &classdecls, diamonds_to_lightning<D2L_cpp_class_inheritance>& d) {
    d = {};
    for (auto c : classdecls)
        d.addnode(c);
}

class classdeclsRAII {
    std::vector<ClassDecl> _savedstate;
    std::vector<ClassDecl *>& _classdeclsref;
public:
    classdeclsRAII(std::vector<ClassDecl *> &classdeclsref)
    : _classdeclsref(classdeclsref)
    {
        _savedstate.clear();
        for (auto c_ptr : _classdeclsref)
            _savedstate.emplace_back(*c_ptr);
    }
    ~classdeclsRAII() {
        assert(_savedstate.size() == _classdeclsref.size()
               && "Did not expect size to change");
        unsigned idx = 0;
        for (auto c_ptr : _classdeclsref) {
            assert(c_ptr->name() == _savedstate[idx].name()
                   && "Did not expect names to change/classdecls to get shuffled");
            *c_ptr = _savedstate[idx++];
        }
    }
};

#endif /* test_helpers_h */
