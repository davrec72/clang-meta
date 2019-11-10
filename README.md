# clang-meta

*[UPDATE 11/10: clang folder added, should work now, sorry for the trouble]*

A C++ compiler with added meta-programming features: 
1) **static reflection** (of just about anything - templates, function definitions, other reflections, you name it), 
2) **metaparsing** (turn string literals into parsed code),
3) **custom compiler diagnostics** (pointing to locations *you* define, with fixits *you* define, displayed by your IDE just like built-in diagnostics), and
4) **constexpr containers** (vector, set, map -- or add your own!) of any of your constexpr classes.

Use these to define your own metaclasses, metanamespaces, meta-metafunctions, the sky's the limit -- recursion works endlessly.  You can write an interface metaclass in your sleep.

Stop doing repetitive coding tasks.  Start delegating to your compiler.  
If you're not *using* metafunctions, you *are* one!

## Code examples

(TODO) Please see the examples folder for an in-depth tutorial; here are just some basic syntax examples:

### Reflection:
```
struct MyClassA { float f; };

constexpr {
  auto_ RD = cast<CXXRecordDecl>(reflexpr(MyClassA));
  	     // ^ cast/dyn_cast/isa work exactly as you'd use them in clang

  RD->
  //  ^ Your IDE will provide suggestions for all reflection properties 
  //    (100s of them -- just about every public const method in the clang AST!)
  //    And if you make changes to clang, just recompile and the new properties 
  //    are automatically reflected!
  
  ce_assert(!RD->isClass());
  ce_assert(RD->isStruct());
  
  // Helper macros provided to trick your IDE into providing support --
  // no annoying red flags will be raised for the new keywords we introduce,
  // but you'll still get red flags for the usual misspellings etc.
  FOR((Decl *) D : RD->decls()) {
    D->dump();
  }
}
```

### metaparsing:
```
constexpr {
  __queue_metaparse("static const int i = 3;");
  __queue_metaparse("static const int j = i + ");
  constexpr int jval = __metaparse_expr(__concatenate("3", 2+2), int);
  __queue_metaparse(__concatenate(jval, ";"));
  
  //ce_assert(i == 3); //ERROR: undeclared identifier
  //ce_assert(j == 37); //ERROR: undeclared identifier
  ce_assert(jval == 34);
  
} //...queued metaparses performed here...

static_assert(i == 3);
static_assert(j == 37);
//static_assert(jval == 34); //ERROR: undeclared identifier
```

Check out the examples folder for custom diagnostics and constexpr containers examples, and
more advanced reflection/metaparsing stuff.


## Acknowledgments

This work is based around llvm/clang 7.0.0.  More significantly, this
work is adapted from Andrew Sutton's original reflection/metaclasses repository: 
	https://github.com/asutton/clang.
Many thanks to him for sharing his excellent work; without it these additional 
contributions would not have been possible.


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
	
4. The clang-meta folder should now have our "llvm" and "examples" folders in it.
   In addition, make a "build" folder (make sure you have 20 GB or so of space), 
   and navigate to it:
```   
cd clang-meta
mkdir build
cd build
```
	
5. Run CMake on the llvm sources:
```
cmake ../llvm
```
	
6. Do the initial build (may take a few hours):
```
cmake --build .
```
  (Don't sweat the various warnings.  I'll try to address them soon.  They don't matter.)
	
7. Build the clang-wreflection target:
```
cmake --build . --target clang-wreflection
```
	
8. Navigate to `build/include`, make sure a `client_reflection_impl.hpp` file has been created.  Make note of the full path to it.  Also, in `build/bin`, there should be a `clang++` executable/shortcut.  Make note of the full path to that too -- you will need it shortly to instruct your IDE to use it as your CXX compiler.

9. Open `clang-meta/examples/include/client_reflection_impl.hpp` file.  It is intended to be a helper shortcut to your root `client_reflection_impl.hpp`.  Right now it contains a relative path that is probably correct for now.  But it would be best to change that to the absolute path to your `/.../clang-meta/build/include/client_reflection_impl.hpp`, in case you want to move examples/include around later.

10. Open your IDE -- I highly recommend XCode for Mac users; its code completer works very well for our purposes, whereas e.g. CLion's does not.  You may need to try a few out.

11. Import our "examples" folder into a new project, or set up a new one from scratch; all you need is to #include each of the example files (`0_reflection.hpp`, `1_metaparsing.hpp` etc.).

12. Change the compilers/other IDE settings for this project:
- In XCode: go to Build Settings for the project, then:
  - Search for "index", and set "Enable Index-While-Building Functionality" to No.
  - Search for "dialect", and set your C++ dialect to either C++17 or GNU++17
  - Click the "+" button, click Add User-Defined Setting, and assign `CXX` to the full path to your new clang++ binary (e.g. `/.../clang-meta/build/bin/clang++`)
  - Click the "+" again and assign `CC` to e.g. `/.../clang-meta/build/bin/clang`.
- In CMake-based IDEs like CLion, you must set `CMAKE_CXX_COMPILER` and `CMAKE_C_COMPILER` appropriately, and the language dialect.

13. Then, build the example project.  I recommend going through the examples in order, there is a lot of new stuff here.

### Updating:
```
cd /PATH/TO/YOUR/clang-meta
git pull https://github.com/drec357/clang-meta.git
cd build
cmake --build . --target clang-wreflection
```

## Discussion/implementation notes

### I already use Python scripts/clang tools/CMake etc. to metaprogram.  Why is this any better? 
Metaprograms outside your main program used to generate the sources fed to your C++ compiler -- call them "transcendent metaprograms" -- are not reflectible.  There is no meta-metaprogram we can write to check them or modify them or call them conditionally.  Maintenance is up to you.

Our metaprograms -- let's call them "immanent metaprograms" -- ARE reflectible.  Higher level meta-functions can be written about them - to check their logic or depend on them in some way.  The source files become whole representations of the program, giving you AND your metafunctions a birds eye view of the project within a standard set of files.  As more sophisticated metafunctions are developed, the advantages of such standardization may be more fully realized.


### Reflection

There are many C++ reflection proposals floating around.  I believe the way I've done it here is the best.  I have simply automated generation of reflection properties from the public const methods/fields of clang AST nodes and helper types, creating a massive reflection library in a matter of minutes.

E.g., if `clang::NamespaceDecl` has an `isAnonymousNamespace()` public const method, so the user will be able to access a `reflexpr(mynamespace)->isAnonymousNamespace()` reflection.

More generally, my argument is that the C++ standard should define a minimum compiler AST architecture that all compilers must use; this would be the basis of the reflection library.

I see at least three benefits over the existing reflection proposals:
1) No parallel code to maintain -- e.g. you don't need to keep up to date an `NamespaceDecl::isAnonymousNamespace()` member AND some sort of `reflectIsAnonymousNamespace()` function.

2) Improvements to the compilers' ASTs would be linked inextricably to improvements to the C++ standards, which would benefit all.

3) Reflection implementers needn't answer to complaints about what properties of this or that decl are or are not reflectible.  Any change a user wants, she or he can make by modifying the AST interface and rebuilding `clang-wreflection` -- and indeed they should.  Interested parties can have discussions about what they want reflected without feeling they are hostage to the reflection implementers, and the implementers can work without having to engage in the hundreds of arguments sure to be required to make such decisions.  The two are decoupled.  Everyone benefits.


### Metaparsing

The metaparsing feature will, I suspect, have some detractors.  It feels a bit Python-y; a bit out of place in strict typing environment like C++.
This is why, I imagine, other proposals lean toward methods of source code "injection" or modification using cleaner, more C++-like statements:
```
	// CURRENT INJECTION PROPOSALS:
	myfuncrefl->addVirtual();
```
instead of 
```
	// MY METAPARSING IMPLEM:
	QPARSE("virtual ", ...outright textual copying of function signature...);
```
I disagree, for some of the same reasons as above: with the metaparsing solution, there is
1) no parallel code to maintain -- new parse-able keywords etc. are automatically supported, and
2) Implementers needn't answer to complaints about what is or is not injectible.

Not to mention that it is dirt-simple to understand, and there seem to be problems that ONLY metaparsing can solve; see how I use it to implement constexpr containers, for example.


### Source code

Only the `llvm/tools/clang` folder has been modified from the original llvm 7.0.0.

I have annotated with `//DWR ADDN` or `//DWR MOD` etc. everywhere I have added to or modified code from the original -- hopefully this will make it straightforward to port this into clang 9 or wherever it's at now.

I have done likewise -- `//ASUTTON ADDN` -- for wherever I have copied over or adapted code from Andrew Sutton's original repository, linked above (which is based around clang 5.x).

Mr. Sutton and co. have made subsequent progress along their own track for reflection & "reification" (their analog of metaparsing); I encourage you to also check out their newer work:
	https://gitlab.com/lock3/clang.
Their code is very impressive, but owing to the divergence of our implementations, I have not borrowed anything additional from it excepting what was already in Mr. Sutton's original 5.x code (`ConstexprDecl`, `CXXTupleExpansionStmt`, `CXXConcatenateExpr`, the idea for reflection classes as template instantiations to solve constexpr issues, various structural ideas, etc. -- you'll see all of ASUTTON's great contributions all annotated in my code).

The source code is a bit of a mess right now -- lots of big blocks of commented out code, etc.  I will clean it up it subsequent commits; but I thought it was good to have all the various old stuff I tried in there for the first commit, to help others who might try to hack further on the code.

The clang tool for generating the reflection sources -- `llvm/tools/clang/tools/gen-reflection-src` -- is likewise a mess, needs to be completely rewritten for clarity - but darned if isn't working perfectly right now on translating just about the entire clang AST into reflection code.  And I have at least documented all the various hacks needed to make it work.

I would very much welcome others contributing fixes and additions.  I have developed this contribution because of my desperation to USE C++ in this way.  But I would rather not become endlessly bogged down in maintaining it.  You would not be stepping on my toes in the least if you see improvements you'd like to implement and share.

Note that you only need to rebuild the `clang-wreflection` target when you change `clang/tools/reflection-src-generator/GenReflectionSrc.cpp` or one of the `include/clang/AST` headers (i.e. the reflected interfaces).  When just making ordinary adjustments to clang cpps, just rebuild the `clang` target -- much faster. 

Have fun!

Dave
