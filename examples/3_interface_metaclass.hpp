//
//  3_interface_metaclass.hpp
//  clang_meta_examples
//
//  Created by David Rector on 8/15/19.
//  Copyright © 2019 David Rector. All rights reserved.
//
//
// We only use features we've already covered to define this metaclass,
// so just dive in.  This isn't a great interface metaclass, just the
// basics in there -- YOU try doing the rest, add the other requirements,
// diagnostics, etc.
//
// For those of you who encountered the "$class" syntax proposed
// by Herb Sutter and partially implemented by Andrew Sutton in his
// original reflection/metaclass implementation on GitHub -- we
// don't bother with that; normal class templates serve just fine
// as metaclass wrappers, and are more general.
// Mr. Sutton's more recent class(metafuncname) { ... } syntax
// for metaclasses, we also abjure.
// [https://gitlab.com/lock3/clang/wikis/Metaprogramming-Introductory-Tutorial]
//
// I.e. instead of either Herb Sutter's old syntax:
//
// $class interface { ... }
// interface MyIfc { ...prototype content... };
// MyIfc *ifcobj;
// 
// or Andrew Sutton's more recent syntax:
// 
// constexpr void interface(meta::info classrefl) { ... }
// class(interface) MyIfc { ...prototype content... };
// MyIfc *ifcobj;
//
// You instead write everything using traditional syntax:
//
// template<typename T> struct interface { ... };
// struct MyProto { ...prototype content... };
// interface<MyProto> *ifcobj;
//
// Why do I prefer the last one?
//
// First, the prototype is decoupled from the interface, so it can be
// used in other metaclasses independent of the interface definition.
//
// Second, should your metaclass's dependencies not be expressible as
// a single parameter of a single prototype class (e.g. you want to
// add policy params, or depend on a param pack of input classes, etc.),
// you can make full use of the existing template capabilities
// without fuss.
//
// Bottom line, what we lose in Java-like syntax, we gain (or rather
// retain) in C++ flexibility.
//
// [ASIDE]
// If there is REALLY a desire for Java-like syntax -- very reasonable --
// perhaps what we really need to accomplish these sorts of things are:
// 1) anonymous structs allowed as typename arguments to templates, e.g.:
// 
//   using myifc = interface<struct {
//     void f(); 
//     void g(); 
//   }>;
// 
// and to give it the desired aesthetics,
// 2) user syntax-definition commands (#syndef?), defined roughly like 
// preprocessor #defines but just a bit more involved, capable in this case 
// of expanding e.g.
//
//   interface myifc { void f(); void g(); };
//
// into the above.  That would expand our flexibility without 
// giving first class status to an arbitrary category of metaclasses 
// -- those that take exactly one typename param.
//
// Onto our interface definition:
//
#ifndef dwrmeta_3_interface_metaclass_hpp
#define dwrmeta_3_interface_metaclass_hpp

#include "include/metacore.hpp"
#include "include/meta/util/apply_to_bases.hpp"

namespace metaclasses_example_0 {
  using namespace cppx::meta;
  using namespace cppx::meta::clang;
  
  template<class T>
  struct interface
    : apply_to_base_classes_t<interface, get_inhwrp_bases_t<T>>
  {
    virtual ~interface() noexcept = 0;
    using prototype = T; //needed for our ce_dump_inst implementation in metaclass_helpers.hpp

    DO_META {
      //get_tmplarg_as_RD: see meta/util/reflect_arg.hpp, same thing we did earlier.
      auto_ RD = cast<CXXRecordDecl>(get_tmplarg_as_RD(reflexpr(T)));
      
      // TODO other checks -- no method definitions, all virtuals or non-virtuals, no
      // template methods, etc.
      
      FOR ((Decl *) D : RD->decls()) {
        IF (D->isImplicit())
          continue;
        IF (auto_ MD = dyn_cast<CXXMethodDecl>(D)) {
          QPARSE("virtual ", MD->getReturnType().getAsString(), " ",
                 MD->getNameAsString(), " (");
          bool first = true;
          FOR ((ParmVarDecl *) P : MD->parameters()) {
            if (first)
              first = false;
            else
              QPARSE(",");
            QPARSE(P->getType().getAsString(), " ",
                   P->getNameAsString()); //TODO need to test if has name?
            //TODO diagnostic about default args
          }
          QPARSE(")");
          IF (MD->isConst())
            QPARSE("const"); // note we don't need to prepend a space:
                             // separate PARSE calls automatically
                             // add whitespace in between.
          QPARSE(" = 0;");
        } ELSE if (isa<FieldDecl>(D)) {
            //TODO diagnostic about fields/other kinds of decls, with fixits
        }
      }
    } //DO_META
  };


  //--------- Test code --------- //
  struct MyPrototype {
    void  f();
    int   g();
    int   h(int i) const;
    void j();
  };
  
  struct MyDerivedProto : MyPrototype {
    void k();
  };

  DO_META {
    // ce_dump doesn't work with instantiations (just dumps something
    // unhelpful), so we need ce_dump_inst if we want to have a look
    // at exactly what our metaclass is producing.
    // TODO generalize ce_dump so we don't need separate functions...
    ce_dump_inst< interface<MyPrototype> >();
    ce_dump_inst< interface<MyDerivedProto> >();
  }
  
#ifndef NMETA
  void dontcallme_justshowing_thiscompiles() {
    interface<MyPrototype> *m = 0;
    m->f();
    [[maybe_unused]] int resg = m->g();
    [[maybe_unused]] int resh = m->h(3);
    m->j();
//    m->doesntexist(); //ERROR
    
    interface<MyDerivedProto> *mm = 0;
    mm->f();
    mm->k();
//    mm->doesntexist(); // ERROR
  }
  
  struct MyBaseImplem : interface<MyPrototype> {
    void f() override;
    int g() override;
    int h(int i) const override;
    void j() override;
//    void k() override; //ERROR: only virtuals can be marked override
  };
  
  struct MyDrvdImplem : interface<MyDerivedProto> {
    void f() override;
    int g() override;
    int h(int i) const override;
    void j() override;
    void k() override;
//    void l() override; //ERROR: only virtuals can be marked override
  };
#endif //NMETA
  
} //namespace metaclasses_example_0


// A little metaclass brainstorming if you're interested.
//
// Suppose you wanted MyDrvdImplem to inherit MyBaseImplem's implems too,
// so you didn't have to manually rewrite them.
// There are a few options -- many more than you had before metaclasses!
// First, the typical solution: define interface so that it adds virtual to each base; i.e.
//
//    struct A : B, C {};
//        v v v v v
//    interface<A> : virtual interface<B>, virtual interface<C> {};
//
//  You would do this using the apply_to_basespecs template in place of apply_to_base_classes above,
//  feeding it a suitable type trait alias that simply flips on the IsVirtual flags of each
//  basespec (see inhwrp.hpp).
//
//  Then, you could write
//
//    struct MyDrvdImplem : interface<MyDerivedProto>, MyBaseImplem {
//      void k() override;
//    };
//
// But if you don't like the overhead/hassle of virtual bases, metaclasses
// offer a world of new possibilities.
//
// Check out the very elaborate "diamonds_to_lightning" metaclass (DWR TODO not
// finished) to see how to eliminate the need for virtual bases while maintaining
// their diamond-resolution ability, for example.
//
// Or, if you don't need to be able to slice a MyDrvdImplem* to a MyBaseImplem* (you
// probably shouldn't -- you should usually only rely on interface<MyDrvdProto>* to
// interface<MyPrototype>* casting), use a metafunction to automatically fetch
// MyBaseImplem's member decls and outright copy them into MyDrvdImplem.
// More generally, you could set up an interface_with_baseimplems<T, BASEIMPLs...>
// to inherit interface<T> then copy the member decls of each BASEIMPL automatically
// into the body.
// Then you could write
//
//    struct MyDrvdImplem : interface_with_baseimplems<MyDerivedProto, MyBaseImplem> {
//      void k() override;
//    };
//
// And you could even automatically generate definitions of conversion operators
// to each BASEIMPL in the interface_with_baseimplems definition, if you really
// wanted some casting ability between the implem classes.
//
// Lots of possibilities.  Best to use policy parameters to isolate away
// the implementation details in cases like this, so you can adjust them later,
// e.g.:
//
//   struct MyDrvdImplem
//       : interface_with_baseimplems< MyDerivedProto
//                                   , DiamondResolutionPolicy::to_lightning //or virtual_bases or copy_implems etc
//                                   , /*EnableCastingToBaseImplems=*/false
//                                   , MyBaseImplem
//                                   /*, ...other implem mixins...*/
//                                   > {
//     void k() override;
//   };
//

#endif //dwrmeta_3_interface_metaclass_hpp
