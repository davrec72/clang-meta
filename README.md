*[11/19/19 update -- see notes after Syntax Examples]*

# clang-meta

A C++ compiler with added meta-programming features: 
1) **static reflection** (of just about anything - templates, function definitions, other reflections, you name it), 
2) **metaparsing** (parse string literals, e.g. into individual template instantiations),
3) **custom compiler diagnostics** (pointing to locations *you* define, with fixits *you* define, displayed by your IDE just like built-in diagnostics), and
4) **constexpr containers** (vector, set, map -- or add your own!) of any of your constexpr classes.

Use these to define your own metaclasses, metanamespaces, meta-metafunctions, etc. -- recursion works endlessly. 

## Acknowledgments

This work is based around llvm/clang 7.0.0.  More significantly, this
work is adapted from Andrew Sutton's original reflection/metaclasses repository: 
	https://github.com/asutton/clang.
Many thanks to him for sharing his excellent work; without it these additional 
contributions would not have been possible.  
Please also check out his newer work (https://gitlab.com/lock3/clang) for an alternate implementation along these lines.


## Syntax examples

### Reflection/custom diagnostics:
```
template<typename T>
struct MyClassTmplA { 
  T t; 
  int i;
  void f();
};

constexpr {

  using namespace cppx::meta;
  using namespace cppx::meta::clang;

  auto_ CTD = cast<ClassTemplateDecl>(reflexpr(MyClassTmplA));

  CTD->
  //   ^Your IDE will provide suggestions for all reflection properties 
  //    (100s of them -- just about every public const method in the clang AST!)
  //    And if you make changes to clang, just recompile and the new properties 
  //    are automatically reflected!
  
  ce_assert(CTD->getNumTemplateParameters() == 1);
  
  FOR ((Decl *) D : CTD->getTemplatedDecl()->decls()) { 
    if constexpr (D->isImplicit())
      continue;
    if constexpr (auto_ FD = dyn_cast<FieldDecl>(D)) {
      auto_ QT = FD->getType();
      if (QT->isDependentType())
	ce_debug("Field named ", FD->getQualifiedNameAsString(), " has a dependent type");
    } else {
      D->dump(); //displays in your build-time output
      ce_error( D->getBeginLoc() //your IDE will register an error pointing at D
	      , "Unhandled Decl kind; see dump"
	      , user::FixItHint::CreateRemoval(user::SourceRange(D->getBeginLoc(), D->getEndLoc()));
	      ); 
    }
  }
  
}
```

### Metaparsing:
```
constexpr {

  __queue_metaparse("static const int i = 3;");
  __queue_metaparse("static const int j = i + ");
  constexpr int jval = __metaparse_expr(__concatenate("3", 2+2, " + 5"), int);
  __queue_metaparse(__concatenate(jval, ";"));
  __queue_metaparse(__concatenate(
  	"constexpr { __queue_metaparse(__concatenate(\"static const int k = ", 
	1+1, 
	" + 2 + \", 4)); }" 
	));
  
  //i; //ERROR: undeclared identifier
  //j; //ERROR: undeclared identifier
  ce_assert(jval == 39);
  //k; //ERROR: undeclared identifier
  
} //...queued metaparses performed here...
  //---The last one is first eval'd to:
  //     constexpr { __queue_metaparse(__concatenate("static const int k = 2 + 2 + ", 4)); }
  //---Then that is itself eval'd as it would be during a normal parse, etc.; 
  //   No limit to degree of evaluation/parsing recursion...

static_assert(i == 3);
static_assert(j == 42);
//static_assert(jval == 39); //ERROR: undeclared identifier
static_assert(k == 8);
```

### Constexpr containers:
```
struct MyKeyClass {
  int i;
  constexpr MyKeyClass(int i = 0) : i(i) {}
  
  // Need this for any container elem class (see tutorial):
  constexpr operator const char *() {
    return __concatenate("MyKeyClass(", i, ")");
  }
};

constexpr {
  auto mymap = ce::map<MyKeyClass, const char *>();
  
  mymap.assign(MyKeyClass(1), "struct Z1 {};");
  mymap.assign(MyKeyClass(3), "struct Z3 {};");
  mymap.assign(MyKeyClass(5), "struct Z5 {};");
  
  nce_assert(mymap.contains(MyKeyClass(3)));
  nce_assert(ce_streq(mymap.at(MyKeyClass(3)), "struct Z3 {};"));
  
  mymap.assign(MyKeyClass(3), "struct ZZ3 {};"); //reassignment
  nce_assert(ce_streq(mymap.at(MyKeyClass(3)), "struct ZZ3 {};"));

  for (auto kv : mymap) {
    __queue_metaparse(kv.second);
  }
}

void dummyfunc1() {
  Z1 z1;
  ZZ3 zz3;
  Z5 z5;
}
```

## Status updates
### 11/19/19

Instead of making slight incremental fixes, I'm going to bite the bullet and take a few weeks to really clean up the code and bring it into an up-to-date version of clang.  

Once that is done, I will put the compiler on Matt Godbolt's excellent Compiler Explorer service and provide a godbolt.org link here, to spare you the hours and frustration and uncertainty and gigabytes of installing on your own system.  *Many thanks* to Matt.

To be sure, the code is working great now -- all the examples work, and you can develop sophisticated metaclasses and meta-most things right now.  But you cannot do everything -- template template parameters and non-type template parameters cannot be properly reflected, and there are other small issues.  I'm going to get that all cleaned up.  

Among the things I'll be doing:

1) Merging my source code into Andrew Sutton/Lock3 software's ISO tentative reflection implementation -- in the process renaming our `reflexpr` to `__clangrefl`, so it does not conflict with their reflection interface.  Just imagine being able to use ISO-compliant reflections when they do what you need via `reflexpr(x)`, and for e.g. unsupported reflections resorting to our non-ISO reflections via `__clangrefl(x)` (and perhaps someday, `__msvcrefl(x)` or `__gccrefl(x)`?)

2) Copying/adapting Lock3's *implementation* of `reflexpr` so `__clangrefl` so they both reflect the same entity *and more importantly*, have the same capabilities.  In the current code, our `reflexpr` (basically unchanged from Andrew's old prototype implementation) does not do so well on e.g. template template parameters, or complicated dependent ID expressions, whereas it appears Lock3 has done some great work to get those to work.  It's really quite difficult what has to happen under the hood to get that stuff to work -- kudos to them.  
Our implementations will then differ only in how you extract reflection properties from that point -- via e.g. 
```meta::some_standard_property(reflexpr(x))```  queries in their case, vs.  
```__clangrefl(x)->someClangSpecificProperty()```   in ours.

3) It also makes great sense to incorporate their excellent `unqualid(...)` feature, as that will work excellently with our `const char *`-centric implementation.  Like our `__metaparse_expr(..., T)`, `unqualid(...)` is an excellent tool for accessing meta information *in-place* (the main advantage of their reification infrastructure over our metaparsing, discussed further below), so you needn't always resort to stringizing and metaparsing huge blocks of code (`DEFERRED_META` techniques, discussed in our metaparsing example hpp) that only depend on a few measly "reifications".  See their tutorial at https://gitlab.com/lock3/clang/wikis/Metaprogramming-Introductory-Tutorial for how `unqualid` is used (in our implementation, the arguments will always be string literals or integers, as with `__concatenate`).  

4) Clean the structure of some of the new Expr and Stmt etc. AST nodes we have introduced, so that you can interface with their reflections more easily when meta-metaprogramming.  For example, metaprograms are named `ConstexprDecl`s, which aren't really a suggestive name -- I will rename it to `MetaprogramDecl`.  And, `__metaparse_expr("3", int)` would be better structured as `__metaparse_expr<int>("3")`, and implemented as any other `ExplicitCastExpr` (`static_cast` etc.), to fix the casting issues it has right now.  Perhaps it could be better named as well.  And for my other new nodes -- e.g. `CompilerDiagnosticExpr`, the names could be clearer and the interface could be cleaner.  Reflection forces us to look in the mirror, and I'm not thrilled with the interface I've provided for some of these nodes -- I will improve them.

5) Setting up some great examples.  My "diamonds to lightning" metafunction has been on my plate for awhile; I've already written it as a clang tool, just need to transfer it over.  
But that is just ordinary class/base reflection; it will show how our constexpr containers can be useful in metaprograms, but doesn't illustrate any of the great reflection facilities this implementation offers.  
Herb Sutter suggested a great "litmus test" for function/expression reflection: "autodiff."  Autodifferentiation is used to automatically generate methods/functions that provide the differential of one variable with respect to another -- e.g. for  `float myfunc(x) { return x^2; }`  it might generate, alongside `myfunc`,   `float myfunc_dx(x) { return 2*x; }`  and  `float myfunc_d2x(x) { return 2; }`.  Imagine a metaclass that adds derivatives for all your differentiable methods, with respect to any variables you specify via policy parameters, up to whatever order you specify.  You'd get nice clear diagnostics for any non-differentiable methods.  I'm fairly certain you could write that metaclass, with our current implementation, *right now*.  But after I make the other changes above, I will do it for sure.

Happy Thanksgiving!

Dave


## Discussion

### This reflection interface isn't an agreed-upon C++ standard, why should I bother writing any code with it?
Because it is far more general than other proposed or implemented reflection standards.  You can reflect anything -- down to the individual statements in your function definitions!  You will never want for reflection.  

The C++ standards will surely *eventually* incorporate all such capabilities -- the demand for more reflection capability will be ever-present, insatiable.  But it will take years for them to decide exactly how they want to name things etc.  

To be sure, while our reflected clang methods can *do* anything you want, if often takes some gymnastics to do it -- so we should certainly respect and appreciate the efforts of the standards folks to take the time to get the names and general aesthetics right.

But in the meantime, we can save ourselves the stress and lost productivity by using this "practical" implementation for now, and simply isolating away any dependencies on the eventual naming standards into a limited library of helper functions.  When standards are agreed upon and a compliant compiler is built that supports every reflection you need, alter those helper functions accordingly and switch to the new compiler.

The same goes for metaparsing/"injection"/"reification"/whatever they eventually call it: isolate away such details into helper functions where possible.

(*In fact*, because the standards being considered are all external functions of the form `meta::some_property(refl)` (e.g. `meta::is_constexpr(funcrefl)`), I suggest you just make your own `meta::` library of constexpr functions of that same basic form.  Perhaps collect them in an `inline namespace yourinitialshere {...}` within `namespace meta { ... }`, and implement them for now in terms of our clang-specific reflections.  If you come up with some useful ones, share them with the rest of us, including the standards committee.  Let's make their job easier by providing bottom-up information on which reflections we need, and how we want to interface with them.)

Main takeaway: you can *do* just about any metaprogramming feat with this compiler.  It is a vast new frontier.  You need not wait for the standards to be perfectly worked out to begin exploration.

### I already use Python scripts/clang tools/CMake etc. to metaprogram.  Why is this any better? 
Metaprograms outside your main program used to generate the sources fed to your C++ compiler -- call them "transcendent metaprograms" -- are not reflectible.  There is no meta-metaprogram we can write to check them or modify them or call them conditionally.  Maintenance is up to you.

Our metaprograms -- let's call them "immanent metaprograms" -- ARE reflectible.  Higher level meta-functions can be written about them - to check their logic or depend on them in some way.  The source files become whole representations of the program, giving you AND your metafunctions a birds eye view of the project within a standard set of files.  As more sophisticated metafunctions are developed, the advantages of such standardization may be more fully realized.

## Installation & Use
### Mac/Unix:

1. Download CMake if necessary.

2. Create a folder to house the source, build, and examples.  We'll call it clang-meta.
```
mkdir clang-meta
```
	
3. Clone this repository into that folder:
```
git clone https://github.com/drec357/clang-meta.git clang-meta
```
	
4. The clang-meta folder should now have our `llvm/` and `examples/` folders in it.
   Check that an `llvm/tools/clang` folder exists, and that in `llvm/projects/` are three subdirectories: `compiler-rt`, `libcxx`, and `libcxxabi`.
   
5.  Now, make a `build/` folder (make sure you have 30 GB or so of space for it), and navigate to it:
```   
cd clang-meta
mkdir build
cd build
```
	
6. Run CMake on the llvm sources:
```
cmake ../llvm
```
	
7. Do the initial build (may take a few hours):
```
cmake --build .
```
  (Don't sweat the various warnings.  I'll try to address them soon.  They don't matter.)
	
8. Build the clang-wreflection target:
```
cmake --build . --target clang-wreflection
```
	
9. Navigate to `build/include`, and make sure a `c++` directory has been created, as well as a `client_reflection_impl.hpp` file.  Make note of the full path to the latter.  Also, in `build/bin`, there should be `clang` and `clang++` executables or shortcuts.  Make note of the full path to those as well -- you will need them shortly to instruct your IDE to use them as your C/CXX compilers.

10. Open `clang-meta/examples/include/client_reflection_impl.hpp` file.  It is intended to be a helper shortcut to your main `client_reflection_impl.hpp`.  Right now it contains a relative path that is probably correct for now.  But it would be best to change that to the absolute path to your `/.../clang-meta/build/include/client_reflection_impl.hpp`, in case you want to move examples/include around later.

11. Open your IDE -- I highly recommend XCode for Mac users; its code completer works very well for our purposes, whereas e.g. CLion's does not.  You may need to try a few out.

12. Import our "examples" folder into a new project, or set up a new one from scratch; all you need is to #include each of the example files (`0_reflection.hpp`, `1_metaparsing.hpp` etc.).

13. Change the compilers/other IDE settings for this project:
- In XCode: go to Build Settings for the project, then:
  - Search for "index", and set "Enable Index-While-Building Functionality" to No.
  - Search for "dialect", and set your C++ dialect to either C++17 or GNU++17
  - Click the "+" button, click Add User-Defined Setting, and assign `CXX` to the full path to your new clang++ binary (e.g. `/.../clang-meta/build/bin/clang++`)
  - Click the "+" again and assign `CC` to e.g. `/.../clang-meta/build/bin/clang`.
- In CMake-based IDEs like CLion, you must set `CMAKE_CXX_COMPILER` and `CMAKE_C_COMPILER` appropriately, and the language dialect.

14. Then, build the example project.  I recommend going through the examples in order, there is a lot of new stuff here.

*Please* create an "Issue" if you follow these instructions but cannot build the example project! 

### Updating:
```
cd /PATH/TO/YOUR/clang-meta
git pull https://github.com/drec357/clang-meta.git
cd build
cmake --build . --target clang-wreflection
```

## Implementation notes


### Reflection

There are many C++ reflection proposals floating around.  I believe the way I've done it here is the best.  I have simply automated generation of reflection properties from the public const methods/fields of clang AST nodes and helper types, creating a massive reflection library in a matter of minutes.

E.g., if `clang::NamespaceDecl` has an `isAnonymousNamespace()` public const method, so the user will be able to access a `reflexpr(mynamespace)->isAnonymousNamespace()` reflection.

More generally, my argument is that the C++ reflection standards folks should focus efforts on the naming and e.g. method interfaces of clang AST nodes, cleaning them up as much as possible, and defining the reflection standard as -- ahem -- a mirror image of that.  

It would be difficult -- developers like their idiosyncrasies and would very reasonably resist. But if it were to be done, I see at least three benefits over the existing reflection proposals:
1) No parallel code to maintain long-term -- e.g. you don't need to keep up to date an `NamespaceDecl::isAnonymousNamespace()` member AND some sort of `reflectIsAnonymousNamespace()` function.

2) Reflection implementers needn't answer to complaints about what properties of this or that decl are or are not reflectible.  Any change a user wants, she or he can make by modifying the AST interface and rebuilding `clang-wreflection` -- and indeed they should.  Interested parties can have discussions about what they want reflected without feeling they are hostage to the reflection implementers, and the implementers can work without having to engage in the hundreds of arguments sure to be required to make such decisions.  The two are decoupled.  Everyone benefits.

3) Common programmers naturally become familiar with clang syntax, thus more able to contribute to clang's maintenance and development.

### Metaparsing

The metaparsing feature will, I suspect, have some detractors.  It feels a bit Python-y; a bit out of place in strict typing environment like C++.
This is why, I imagine, other proposals lean toward using cleaner, more C++-like statements; consider Mr. Sutton and co's implementation:
```
  namespace a {
    int foo();
  }
  
  // ASUTTON'S "INJECTION":
  namespace b {
    consteval {
      meta::info  afoorefl = reflexpr(a::foo);
      meta::make_constexpr(afoorefl);
      -> a_foo_refl;
    }
  }
```
Compare that to metaparsing:
```
  // DWR'S "METAPARSING":
  template<typename T>
  constexpr const char *funcSigAsStr(T funcrefl) { 
    /*...textual copying of function signature ...*/
  }
  
  namespace b {
    constexpr {
      auto_ afoorefl = reflexpr(a::foo);
      QPARSE((afoorefl->isConstexpr() ? "" : "constexpr "), funcSigAsStr(afoorefl)); 
    }
  }
  
```
I prefer the latter for the same reasons as above: with the metaparsing solution, there is
1) no parallel code to maintain a la `make_constexpr` -- new parse-able keywords etc. are automatically supported; and
2) Implementers needn't answer to complaints about what is or is not "injectible", or what properties of a reflection are modifiable before injection.

But metaparsing's most obvious benefit: it is *dirt-simple*.  Anybody can figure it out in five minutes.  Anybody can see how a generative metaprogram works by just glancing at it.  This leaves more brainpower for considering the actual logic of the metaprogram.

Mr. Sutton's infrastructure has clearly been laboriously thought out, but it gets fairly complex, once he introduces `__fragment`s and `requires` statements to declare dependencies.  I suggest you see other metaprogramming examples of Mr. Sutton's at https://gitlab.com/lock3/clang/wikis/Metaprogramming-Introductory-Tutorial, think how you might do them with metaparsing, and perhaps consider the converse, see which feels better.  I'd be interested to know if anyone finds anything that can be done his infrastructure that cannot be done with metaparsing.  


### Source code notes

Only the `llvm/tools/clang` folder has been modified from the original llvm 7.0.0.

I have annotated with `//DWR ADDN` or `//DWR MOD` etc. everywhere I have added to or modified code from the original -- hopefully this will make it straightforward to port this into clang 9 or wherever it's at now.

I have done likewise -- `//ASUTTON ADDN` -- for wherever I have copied over or adapted code from Andrew Sutton's original repository, linked above (which is based around clang 5.x).

As mentioned previously Mr. Sutton and co. have made subsequent progress along their own track for reflection & "reification"; I encourage you to check out their newer work:
	https://gitlab.com/lock3/clang.
Their code is very impressive, but owing to the divergence of our implementations, I have not borrowed anything additional from it excepting what was already in Mr. Sutton's original 5.x code (`ConstexprDecl`, `CXXTupleExpansionStmt`, `CXXConcatenateExpr`, the idea for reflection classes as template instantiations to solve constexpr issues, various structural ideas, etc. -- you'll see all of ASUTTON's great contributions all annotated in my code).

The source code is a bit of a mess right now -- lots of big blocks of commented out code, etc.  I will clean it up it subsequent commits; but I thought it was good to have all the various old stuff I tried in there for the first commit, to help others who might try to hack further on the code.

The clang tool for generating the reflection sources -- `llvm/tools/clang/tools/reflection-src-generator` -- is likewise a mess, needs to be completely rewritten for clarity - but darned if isn't working perfectly right now on translating just about the entire clang AST into reflection code.  And I have at least documented all the various hacks needed to make it work.

I would very much welcome others contributing fixes and additions.  I have developed this contribution because of my desperation to USE C++ in this way.  But I would rather not become endlessly bogged down in maintaining it.  You would not be stepping on my toes in the least if you see improvements you'd like to implement and share.

Note that you only need to rebuild the `clang-wreflection` target when you change `clang/tools/reflection-src-generator/GenReflectionSrc.cpp` or one of the `include/clang/AST` headers (i.e. the reflected interfaces).  When just making ordinary adjustments to clang cpps, just rebuild the `clang` target -- much faster. 

Have fun!

Dave
