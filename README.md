Disclaimer: this is an old, defunct project, filled with bugs and failed tests.  It likely has issues out of the box on many platforms.  It should only serve as a proof of concept/guide to how this functionality could be more rigorously implemented.

# clang-meta

A C++ compiler with added meta-programming features: 
1) **static reflection** (of just about anything - templates, function definitions, other reflections, you name it), 
2) **metaparsing** (parse string literals as code, e.g. into individual template instantiations),
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
  //---Then that is parsed and itself eval'd, etc.; 

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


## Discussion

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

Note that you only need to rebuild the `clang-wreflection` target when you change `clang/tools/reflection-src-generator/GenReflectionSrc.cpp` or one of the `include/clang/AST` headers (i.e. the reflected interfaces).  When just making ordinary adjustments to clang cpps, just rebuild the `clang` target -- much faster. 

Dave
