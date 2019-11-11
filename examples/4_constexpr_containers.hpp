//
//  5_diamonds_to_lightning_helpers.hpp
//  clang_meta_examples
//
//  Created by David Rector on 10/27/19.
//  Copyright © 2019 David Rector. All rights reserved.
//
//
// INTRO
//
// Sometimes you just plain need heap-allocating constexpr containers.
// The metafunctions are just too difficult to implement without them.
// I have provided some implementations that rely on the reflection
// and metaparsing techniques already discussed.
//
// These aren't the exact same as std::vector et al of course, but
// they work surprisingly well.  They even support move semantics!
// (See the emplace_back example below.)
//
// At a minimum, they offer a safe interface for using
// heap-allocating containers in your metaprograms, and a pretty cool
// implementation until a more efficient/low-level one can be developed.
//
// The key to implementation was to let clang manage the containers,
// and interface with them via reflection statements.
//
// But how can this possibly allow you to define containers of your
// own types?  Clang knew nothing about them when it compiled the
// reflected containers, and we aren't allowed to use 
// reinterpret_casts like you might do at run time.
//
// Simple: for any class whose objects you want to use in a
// container, define a
//
//   constexpr explicit operator const char *() {...}
//
// that produces a STRING that would reconstruct the object in
// its current state.  All Clang needs to do is manage those strings.
//
// Any constexpr constructors you call within this string need to be constexpr.
// And, the constructor string cannot reference variables etc. --
// it needs to be able to stand alone, and be called in any scope
// (i.e. in most cases use fully qualified names.)
//
// The container will convert element types to string literals
// for storage, and when the fully typed objects are needed, it performs
// a __metaparse_expr on the stored strings to reconstruct the objects.
// Check out e.g. ce/vector.hpp for details.
//
//
// CONST VERSION OF THE CONVERSION:
// Whenever you define an element class that itself managers a container
// (by storing a string of it) or a pointer, you should
// always define a const version of your operator const char *() in the
// following standard way (TODO define a metaclass to do this for us):
//
//  constexpr operator const char *() const {
//    return __concatenate("ce::as_const(",
//                         const_cast<MyClass *>(this)->operator const char *(),
//                         ")");
//  }
//
//
// EQUALITY/SORTING OF OBJECTS
// Note that in e.g. ce::map and ce::set, tests for equality will be based on
// string comparisons of the key element's constructor strings.
// So, think carefully about how to define your operator const char *()
// so that it produces a "normal-form"/fully reduced constructor string.
// E.g. "MyElemClass(4)" != "MyElemClass(2+2)".
// Also note that if you want your objects sorted in a certain way into a ce::map
// or ce::set, you must communicate that by producing constructor strings that
// would be alphabetically sorted as desired.
//
//
// ADDING NEW CONTAINERS/ADJUSTING MY IMPLEMS (ADVANCED):
// The reflected containers are defined in clang/Basic/ClientReflContainers.h.
// You're welcome to improve on and add to my implementations; just recompile
// clang_wreflection again and client_reflection_impl.hpp will be regenerated
// with the updated interface/new reflection object kinds.
//
// Then adjust the container interfaces in ce/vector.hpp et al to match.
//
// BUT, best to not bother trying to return any class type from the
// underlying clang containers, e.g. an iterator class, just yet -- you
// probably won't get the expected results, due to the way reflection
// must be implemented right now (classes are turned into templates),
// and idiosyncrasies of how "non-const" reflection objects are evaluated
// (the first time the object is requested, the template is instantiated,
// thereby fixing its value for good.)
//
// HOWEVER, this new __metaparse_expr functionality may be the key to getting
// around this limitation, to reflect classes as classes rather than as template
// instantiations, which would also be the key to getting normal iteration
// techniques to work on reflections, as they do on these containers (see below).
// So check back regularly.
//
//
// STORING REFLECTION OBJECTS
// Because of the current limitations of reflection objects discussed above (the
// template problem), you cannot e.g. have a ce::vector<Decl *>.  Decl is a template
// with different args for each Decl object; no two Decls have the same type.
//
// Until this issue is resolved (again I may finally have the answer!), your best
// bet is to store reflection objects in a ce::vector<const char *> (I have defined a
// NON-explicit operator const char *() for each the reflection type, so you can call
// mystrvec.push_back(somerefl), no hassle), and then use QPARSE to reconstruct
// the objects using DEFERRED_META techniques (see 1_metaparse.hpp).
// (You cannot use __metaparse_expr to reconstruct the objects in place because
// you positively NEED to know the exact type, no way around it -- but you can't
// know it because of the darn template problem!  Hence the need to QPARSE).
//
// Or, an easy alternative: store all the info you will need about a reflection when
// you first encounter it in primitive types within a custom element class, so you
// don't have to go back and fetch any info later via complex QPARSEs and
// DEFERRED_METAs.
//
// Onto the examples:
//

#ifndef diamonds_to_lightning_helpers_hpp
#define diamonds_to_lightning_helpers_hpp

#include "include/metacore.hpp"
#include "include/ce/vector.hpp"
#include "include/ce/set.hpp"
#include "include/ce/map.hpp"

namespace constexpr_containers_example {
    
    static constexpr const char *curqualdname = "constexpr_containers_example";
    //^ TODO sloppy, should really infer the qualified name via reflection
    // (TODO maybe we need a reflcurcontext()...)
    
    using namespace cppx::meta;
    using namespace cppx::meta::clang;
    
    struct MyElemClass {
        int i;
        constexpr MyElemClass(int i = 0) : i(i) {}
        constexpr operator const char *() {
            return __concatenate(curqualdname, "::MyElemClass(", i, ")");
        }
        // Const version: not strictly needed for this class (since it doesn't
        // manage a container/pointer of its own), but can't hurt:
        constexpr operator const char *() const {
          return __concatenate("ce::as_const(",
                               const_cast<MyElemClass *>(this)->operator const char *(),
                               ")");
        }
    };

    DO_META {
        // Quick test of ce::as_const:
        [[maybe_unused]] bool dummy = ce::as_const(ce::vector<MyElemClass>()).empty(); //okay
    //    ce::as_const(ce::vector<MyElemClass>()).push_back(MyElemClass(11)); //ERROR
        
        // Basic push/pop/size/at/assign tests:
        auto myvec = ce::vector<MyElemClass>();
        myvec.push_back(MyElemClass(11));
        nce_assert(myvec.size()==1);
        nce_assert(myvec.at(0).i == 11);
        myvec.assign(0, MyElemClass(111));
        nce_assert(myvec.at(0).i == 111);
        myvec.push_back(MyElemClass(222));
        nce_assert(myvec.at(1).i == 222);
        myvec.pop_back();
        myvec.push_back(MyElemClass(22));
        nce_assert(myvec.at(1).i == 22);
        myvec.push_back(MyElemClass(33));
        
        // Normal iteration techniques work:
        for (unsigned i = 0; i < myvec.size(); ++i) {
            ce_debug_var(myvec.at(i).i);
        }
        for (MAYBE_META_USED auto elem : myvec) {
            ce_debug_var(elem.i);
        }
        
        // Copy construction test:
        auto myvec2 = myvec;
        myvec2.push_back(MyElemClass(44));
        nce_assert(myvec.size()==3);
        nce_assert(myvec2.size()==4);
     
        // Integer-typed vector (optimized -- no string literal conversions done for these)
        auto myvec3 = ce::vector<int>();
        myvec3.push_back(3);
        nce_assert(myvec3.back() == 3);
        myvec3.push_back(-43);
        nce_assert(myvec3.back() == -43);
     
        // Vector of vectors
        auto myvec4 = ce::vector<ce::vector<int>>();
        myvec4.push_back(myvec3); //copy construction (see below)
        nce_assert(myvec4.size() == 1);
        nce_assert(myvec4.back().back() == -43);
        
        // Note that we copy constructed myvec3
        // when we pushed it:
        myvec4.back().push_back(23);
        nce_assert(myvec4.back().back() == 23);
        nce_assert(myvec3.back() == -43); //!
        
        // But we could also have emplaced it to avoid the copy
        // (move semantics!):
        myvec4.emplace_back(std::move(myvec3));
        myvec4.back().push_back(23);
        nce_assert(myvec4.back().back() == 23);
        nce_assert(myvec3.back() == 23); //!
        
        // You should always manually call dealloc when you're done;
        // basically treat these things like new'd objects.  To be sure,
        // clang will handle full deletion at the end of the build,
        // but this frees up space mid-build.  Won't actually delete
        // the objects though, can still push etc after this
        // (can't have risk of nullptr dereferencing during constexpr eval...).
        // Would be nice to put these in destructors but we're not allowed to
        // define constexpr destructors for whatever reason.
        // I might see if I can come up with some pseudo-stack-allocation
        // infrastructure within ConstexprDecl to handle this for you.
        myvec.dealloc();
        myvec2.dealloc();
        myvec3.dealloc(); // This will (not yet - TODO!) be handled in myvec4's dealloc
        myvec4.dealloc(); // since we emplaced it, but no harm in doing it twice.
        
        // TODO get pointer element types/const-qualified types etc. to work by adding
        // overloads of metacast
    }

    // QPARSEs within NORMAL for loops, over a non-const ce container
    DO_META {
        auto mystrvec = ce::vector<const char *>();
        mystrvec.push_back("struct WontExist {};");
        mystrvec.pop_back();
        mystrvec.push_back(__concatenate("struct ", "AA1 {};"));
        const char *myBBstruct = "struct BB1 {};";
        mystrvec.push_back(myBBstruct);
        
        // Regular for loop using regular iterators, no clang modifications needed!
        int num = 0;
        for (MAYBE_META_USED auto s : mystrvec) {
            QPARSE(s);
            ++num;
        }
        nce_assert(num==2);
    }
    #ifndef NMETA
    void dummyfunc10() {
    //    WontExist w; //ERROR
        [[maybe_unused]] AA1 a;
        [[maybe_unused]] BB1 b;
    }
    static_debug("Compiled dummyfunc10 -- the one using normal for loop iteration to QPARSE");
    #endif




    // Important note about ce::maps:
    // at, contains, and assign
    // are implemented within clang (in clang/Basic/ClientReflContainers.h)
    // to avoid extra lookups when you call them in succession on the same key.
    // See their documentation for examples.
    // So don't fret about extra lookup costs since you cannot get an iterator
    // back directly (yet).
    //
    // However right now ITERATION over maps is very inefficient -- but that can
    // easily be improved; I've put it off because I'm working on the
    // even more fundamental improvements already mentioned.
    //
    // [ASIDE] While on the subject, note that FOR/for... loop iteration over reflections
    // is also inefficient -- O(n^2) instead of O(n) for any non-random access
    // iterator, which is...just about every one of them (e.g. DeclContext::decls()
    // uses a list structure, so do most other ranges in clang).
    // Andrew Sutton is aware of this, judging by his comments in his code,
    // but it's a very vexing problem to solve -- another reason it would be great
    // to solve the template problem for good so we could use normal iteration
    // techniques, which would not have this problem.
    //
    // Onto the map example:
    //
    DO_META {
        auto mymap = ce::map<MyElemClass, const char *>();
        mymap.assign(MyElemClass(1), "struct Z1 {};");
        mymap.assign(MyElemClass(3), "struct Z3 {};");
        mymap.assign(MyElemClass(5), "struct Z5 {};");
        nce_assert(mymap.contains(MyElemClass(3)));
        nce_assert(ce_streq(mymap.at(MyElemClass(3)), "struct Z3 {};"));
        mymap.assign(MyElemClass(3), "struct ZZ3 {};"); //reassignment
        nce_assert(ce_streq(mymap.at(MyElemClass(3)), "struct ZZ3 {};"));
        
        for (MAYBE_META_USED auto kv : mymap) {
            QPARSE(kv.second);
        }
    }
    #ifndef NMETA
    void dummyfunc11() {
        [[maybe_unused]] Z1 z1;
    //    Z3 z3; //ERROR
        [[maybe_unused]] ZZ3 zz3;
        [[maybe_unused]] Z5 z5;
    }
    static_debug("Compiled dummyfunc11 (ce::map example with iteration)");
    #endif


    // ce::set example:

    DO_META {
        auto myenumnames = ce::set<const char *>();
        nce_assert(myenumnames.empty());
        
        myenumnames.insert("E_c");
        nce_assert(myenumnames.contains("E_c"));
        nce_assert(!myenumnames.contains("E_a"));
        myenumnames.insert("E_a");
        nce_assert(myenumnames.contains("E_a"));
        
        [[maybe_unused]] bool was_newly_inserted = myenumnames.insert("E_d");
        nce_assert(was_newly_inserted);
        [[maybe_unused]] bool was_reinserted =     myenumnames.insert("E_d");
        nce_assert(!was_reinserted);
        
        myenumnames.insert("E_a");
        myenumnames.insert("E_b");
        myenumnames.insert("E_e");
        myenumnames.insert("E_a");
        myenumnames.insert("E_e");
        
        myenumnames.erase("E_e");
        nce_assert(!myenumnames.contains("E_e"));
        
        QPARSE("enum class MyEnum {");
        for (MAYBE_META_USED auto name : myenumnames) {
            QPARSE(name, ",");
        }
        QPARSE("  E_MAX"
               "};");
    }

#ifndef NMETA
    void dummyfunc12() {
        MyEnum m;
        m = MyEnum::E_a;
        // Note they will be alphabetical. If you want them sorted
        // differently, use a helper element class whose constructor
        // strings would be alphabetically/numerically sorted how you want the
        // elements to be sorted:
        static_assert((int)MyEnum::E_a == 0);
        static_assert((int)MyEnum::E_b == 1);
        static_assert((int)MyEnum::E_c == 2);
        static_assert((int)MyEnum::E_d == 3);
        static_assert((int)MyEnum::E_MAX == 4);
    }
    static_debug("Compiled dummyfunc12 (ce::set example with the enums)");
#endif
    
} //namespace constexpr_containers_example





#endif /* diamonds_to_lightning_helpers_hpp */
