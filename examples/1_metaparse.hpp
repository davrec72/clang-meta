//
//  1_metaparse.hpp
//  clang_meta_examples
//
//  Created by David Rector on 8/11/19.
//  Copyright © 2019 David Rector. All rights reserved.
//
//  Demonstrates basic meta-parsing functionality,
//  independent of reflection.
//
// SEE THE "README FIRST" in 0_reflection.hpp about occasional unexplained crashes --
// metaparsing is what is somehow causing them.
//

#ifndef dwrmeta_1_metaparse_hpp
#define dwrmeta_1_metaparse_hpp

// Note we do NOT need client_reflection_impl.hpp for this stuff,
// just our helper macros:
#include "include/meta/util/macros.hpp"

namespace metaparse_example_0 {

  // Imagine your preprocessor macros operating as regular functions
  // alongside your high level C++ templates and types and syntax.
  // That is essentially what metaparsing allows.
  //
  // There are two kinds of metaparsing functionality we need:
  // __queue_metaparse and __metaparse_expr.
  //
  // __queue_metaparse parses a string literal's content
  // into the context just after the DO_META block
  // in which it is contained.
  //
  // Always use our helper macro, more brief and convenient:
  //
  //   QPARSE(...) = __queue_metaparse(__concatenate(...))
  //
  // __metaparse_expr parses a string literal's content
  // immediately.
  //
  // This illustrates the difference:
  //
  DO_META {
    
    QPARSE("static const int i = 3;");
    QPARSE("static const int j = i + ");
    MAYBE_META_USED constexpr int jval = __metaparse_expr(__concatenate("3", 2+2), int);
    QPARSE(jval, ";");
    
    //ce_assert(i == 3); //ERROR: undeclared identifier
    //ce_assert(j == 37); //ERROR: undeclared identifier
    ce_assert(jval == 34);
    
  } //...queued metaparses performed here...
  
#ifndef NMETA
  static_assert(i == 3);
  static_assert(j == 37);
//  static_assert(jval == 34); //ERROR: undeclared identifier
#endif
  
  // __metaparse_expr is something I just developed, and is
  // useful only deep inside implementation details;
  // it was developed to get our ce::vector/ce::set/ce::map to work,
  // see ce/vector.hpp for use cases.  VERY useful there.
  //
  // If you must use it, note that it isn't super friendly right now --
  // no implicit casts from one int type to another, no understanding
  // of the enclosing scope, etc.
  //
  // In most metaclasses/metafunctions, you'll be using QPARSE
  // exclusively.  The rest of the examples here are thus devoted to QPARSE.
  //
  DO_META {
    MAYBE_META_USED const char *myfield = "float f;";
    QPARSE("struct MyClassB {", myfield, "};");
  }
  // We enclose our tests in NMETA so the IDE doesn't complain:
#ifndef NMETA
  void dummy2() {
    MyClassB b;
    b.f = 2.2;
  }
  // You'll see this in build output to know we really compiled the above:
  static_debug("Compiled metaparse_example_0::dummy2()");
#endif

  // Since we discussed the close link between metaparsing and preprocessor macro
  // expansion, let's discuss macro usage in metaparse expressions/statements.
  //
  // Even though we CAN recognize macro names in metaparse statements,
  // trying to expand them or define them would cause crashes right now, due to
  // inadequate location info in the metaparse strings, which
  // the preprocessor seems to depend on.
  // Thus, I've disabled macro definition and expansion WITHIN metaparse strings;
  // you'll get diagnostics if you try to use a macro.
  // The only thing that never seems to cause issues is #undefing macros, so
  // that is still allowed:
  //
#define MYMACRO_A
  DO_META {
    QPARSE("#undef MYMACRO_A");
  }
#ifndef NMETA
  static_debug("metaparsed #undef test");
#ifdef MYMACRO_A
  Will be skipped
#endif
#endif //NMETA
  //
  // Lame, I know.
  //
  // The important thing this demonstrates, though, is that
  // the intrinsic capability to use macros in metaprogramming is there
  // -- they're not all "expanded away" by the time we do metaparsing.
  //
  // (Hence there is also the possibility of REFLECTING macro information,
  // which is critical I believe -- need to be able to reflect the
  // original source's EXACT content, even including ordinary comments
  // etc. in my opinion. Need "holistic" reflection.)
  //
  // But METAPARSING macros would take some work to implement.
  // For me it's a low priority, even considering how much I use macros.
  //
  // Why?  Because when you need to use macros, you can usually expand
  // them BEFORE stringizing them.  More on this later.
  //
#define WONT_WORK char thechar = 'a';
#define WILL_WORK "char thechar = 'a';" //manually stringized for now
  
  DO_META {
//    QPARSE("struct MyClassBB0 { WONT_WORK };"); //ERROR: macro expansion not supported in metaparse statements
    QPARSE("struct MyClassBB1 {", WILL_WORK, "};\n");
    QPARSE("#undef WONT_WORK");
    QPARSE("#undef WILL_WORK");
  }

#ifndef NMETA
  void dummy3() {
    MyClassBB1 bb1;
    bb1.thechar = 'b';
  }
  static_debug("Compiled dummy3()\n");
#endif //NMETA

  
  // Now an example that demos recursive usage of the VERY useful
  // '__concatenate' expression for constexpr concatenation
  // of string literals, courtesy of Andrew Sutton:
  //
  constexpr const char *makeNSillyFuncs_AsStr(unsigned N) {
    return (N ? __concatenate("void myfunc", N, "() {}", makeNSillyFuncs_AsStr(N-1)) : "");
  }

  DO_META {
    // We'll fetch the N for our example from a random type trait,
    // just to make it clear why this is so much better than
    // using macro recursion techniques to make N silly funcs
    // or whatever:
    // With QPARSE(...), your generated code can depend on previous
    // FULLY INTERPRETED CODE, and can be individually generated into
    // different instantiations of a class/function template
    // (which is the key to implementing metaclasses).
    // All of that is of course impossible with macros,
    // since they are expanded by the preprocessor before the
    // parsing stage.
    //
    QPARSE("struct MyClassC {",
            makeNSillyFuncs_AsStr( std::tuple_size<std::tuple<int,int,int>>::value ),
          "};");
  };

#ifndef NMETA
  //TESTS
  void dummy4() {
    MyClassC m;
    m.myfunc1();
    m.myfunc2();
    m.myfunc3();
  //  m.myfunc4(); //ERROR
  }
  static_debug("Compiled dummy3()\n");
#endif

  
  // Now, higher order meta-parsing: deferring meta processing by combining
  // DO_META, PARSE, and the preprocessor's stringizing capability.
  //
  DO_META {
    
    QPARSE("static constexpr int TheNumFuncs = 5;");
    
//    static_assert(TheNumFuncs==5); //ERROR: undeclared identifier.
    
    // Works, by deferring parsing by at least the same amount as the declaration:
    QPARSE("static_assert(TheNumFuncs==5);");
    
    // Using our STRINGIZE macro, we don't need to manually add quotes to defer parsing
    // (and, more importantly, allows us to expand inner macros BEFORE the true stringizing):
    QPARSE(STRINGIZE(static_assert(TheNumFuncs==5);));
    
    // We can even defer metaprograms -- again note the DO_META macro
    // will be expanded BEFORE stringizing:
    QPARSE(STRINGIZE(DO_META{
      QPARSE("struct MyClassD {", makeNSillyFuncs_AsStr(TheNumFuncs), "};");
    }));
  
    // That's a lot to write though, so here's a helper macro:
    //
    //  DEFERRED_META {...} = 'QPARSE(STRINGIZE(DO_META{ ... }))'
    //                          at compile time.
    //
    // (NB details about our DEFERRED_META definition at bottom.)
    //
    // A DEFERRED_META statement must be placed inside a DO_META
    // or another DEFERRED_META block.
    //
    QPARSE("static constexpr int TheNumFuncsB = 1;");
    DEFERRED_META {
      QPARSE("struct MyClassE {", makeNSillyFuncs_AsStr(TheNumFuncsB), "};");
      // Pretend we can only define TheNumFuncsC via nested metaparsing:
      QPARSE("static constexpr int TheNumFuncsC = TheNumFuncsB + ", TheNumFuncsB, ";");
//      QPARSE("struct MyClassF0 {", makeNSillyFuncs_AsStr(TheNumFuncsC), "};"); //ERROR: undeclared identifier
      DEFERRED_META {
        QPARSE("struct MyClassF {", makeNSillyFuncs_AsStr(TheNumFuncsC), "};");
      }; // <- note we need a semicolon after DEFERRED_META, since it's really a QPARSE statement.
    };
  }; // <- if you want consistency, can put a semicolon after DO_META too,
     // though it's unneeded (or should it be required? TODO consider).
  
#ifndef NMETA
  void dummy5() {
    MyClassD d;
    d.myfunc1();
    d.myfunc5();
    //d.myfunc6(); //ERROR
    
    MyClassE e;
    e.myfunc1();
   //e.myfunc2(); //ERROR
    
    MyClassF f;
    f.myfunc1();
    f.myfunc2();
    //f.myfunc3(); //ERROR
  }
  static_debug("Compiled dummy4() (DEFERRED_META examples)\n");
#endif
  
// [IMPORTANT: slight change to Preprocessor]
// The DEFERRED_META definition only works at compile time
// because I changed the preprocessor code modestly to allow
// function-like macros to be defined using {} in addition to ().
//
//   #define MymacroA{x} x
//           =
//   #define MymacroB(y) y
//
//  But only call with same parens vs. braces used in original def
//  (so you needn't worry about old code now being misinterpreted):
//
//   MymacroA {arg} //will expand
//   MymacroA (arg) //WON'T expand
//   MymacroB {arg} //WON'T expand
//   MymacroB (arg) //will expand
//
  
} //namespace metaparse_example_0

#endif //dwrmeta_1_metaparse_hpp
