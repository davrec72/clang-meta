# clang-meta

A C++ compiler with added meta-programming features: 
1) **static reflection** (of just about anything - templates, other reflection expressions, you name it), 
2) **metaparsing** (turn string literals into parsed code),
3) **custom compile-time diagnostics** (at source locations YOU define, that work with your IDE as is), and
4) **constexpr containers** (vector, set, map -- or add your own!) of any constexpr class you define.

Use these to define your own metaclasses, metanamespaces, meta-metafunctions, the sky's the limit -- recursion works endlessly.  You can write an interface metaclass in your sleep.

Stop doing repetitive coding tasks -- delegate to your compiler.  
If you're not *using* metafunctions, you *are* one!

## Code examples

Please see the examples folder for an in-depth tutorial; here is just the basic syntax:

### Reflection:
```
struct MyClassA { float f; };

DO_META {
	auto_ RD = cast<CXXRecordDecl>(reflexpr(MyClassA));
		// ^ cast/dyn_cast/isa work exactly as you'd use them in clang
	
	RD->
	//  ^ Your IDE will provide suggestions for all reflection properties 
	//    (100s of them -- just about every public const method in the clang AST!)
	//    And if you make changes to clang, just recompile and the new properties 
	//    are automatically reflected!
	
	ce_assert(!RD->isClass());
	ce_assert(RD->isStruct());

	// Note all the helper macros provided to trick your IDE into providing support --
	// no annoying red flags will be raised for the new keywords we introduce,
	// but you'll still get red flags for the usual misspellings etc.
	// (I.e. no need to wait a few years for reflection, THEN wait another few years 
	// for your IDE to support it...it all works now!)
	FOR( (Decl *) D : RD->decls()) {
		D->dump();
	}
}
```

### metaparsing:
```
 DO_META {
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
//  static_assert(jval == 34); //ERROR: undeclared identifier
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
   In addition, make a "build" folder, and navigate to it:
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
  - Search for "dialect", and set your C++ dialect to either C++17 or GNU++17 (for optimal IDE support)
  - Click the "+" button, click Add User-Defined Setting, and assign `CXX` to the full path to your new clang++ binary (e.g. `/.../clang-meta/build/bin/clang++`)
  - Click the "+" again and assign `CC` to e.g. `/.../clang-meta/build/bin/clang`.
- In CMake-based IDEs like CLion, you must set `CMAKE_CXX_COMPILER` and `CMAKE_C_COMPILER` appropriately, and the language dialect.

13. Then, build the example project.  I recommend going through the examples in order, there is a lot of new stuff here.  As explained in `0_reflection.hpp`, you may encounter an unexplained crash now and then -- just rebuild if you do.


Please raise an Issue if you encounter one.  Even better, raise one if you encountered an issue but came up with a solution
to share with others.
Also please raise even aesthetic/naming concerns if you have alternate suggestions, as those are important to address early on.


## Discussion

(Only for other implementers or people concerned with the C++ proposals/future standards for reflection etc; most users can skip...)

### Reflection

There are many C++ reflection proposals floating around.  I believe the way I've done it here is the best.  I have simply automated generation of reflection properties from the public methods/fields of clang AST nodes and helper types, creating a massive reflection library in a matter of minutes.

E.g., if `clang::NamespaceDecl` has an `isAnonymousNamespace()` public method, so the user will be able to access a `reflexpr(mynamespace)->isAnonymousNamespace()` reflection.

More generally, my argument is that the C++ standard should define a minimum compiler AST architecture that all compilers must use; this would be the basis of the reflection library.

This has four main benefits over the existing reflection proposals:
1) No parallel code to maintain -- e.g. you don't need to keep up to date an `NamespaceDecl::isAnonymousNamespace()` member AND some sort of `reflectIsAnonymousNamespace()` function.

2) Improvements to the compilers' ASTs would be linked inextricably to improvements to the C++ standards, which would benefit all.

3) Reflection properties can take advantage of the object oriented architecture of the clang AST.  Existing reflection proposals seem to only allow the user to query reflections via external functions or templates -- e.g. 
```	
	//CURRENT REFLECTION PROPOSALS:
	meta::decls_of(myclassrefl)
	meta::decls_of<myclassrefl>
	        ^   ^    ^
		       Code completer can only offer loose suggestions at best throughout
```
Whereas the object-oriented implementation places the object first in the typical way, allowing the IDE to assist in code completion:
```
	//OBJ-ORIENTED REFLECTION IMPLEM:
	myclassrefl->decls()
		     ^
		     Code completer can make precise suggestions right away
```
(To be sure, the actual implementations of passing reflection statements to the compiler perhaps must involve external function-like statements that place the object inside the function, a la the first way.  BUT, why bother coming up with a detailed naming system for such expressions?  I have simply used `__reflect_prop(...arbitrary arguments...)` for all of them.  They are immediately wrapped into more meaningful methods/fields in the reflection hierarchy in `client_reflection_impl.hpp`.  The user should never interface with them directly; they should always use the object wrappers, and almost certainly will.  So why waste time and clutter up the syntax by giving all these implementation details precise, meaningful names?  Just my opinion -- and perhaps I misunderstand the proposals; please correct me if so.)

4) Reflection implementers needn't answer to complaints about what properties of this or that decl are or are not reflectible.  Any change a user wants, she or he can make by modifying the AST interface and rebuilding `clang-wreflection` -- and indeed they should.  Interested parties can have discussions about what they want reflected without feeling they are hostage to the reflection implementers, and the implementers can work without having to engage in the hundreds of arguments sure to be required to make such decisions.  The two are decoupled.

Everyone wins with this approach.


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

Not to mention that it is dirt-simple to understand, and it seems to me there may be problems that ONLY metaparsing can solve; see how I use it to implement constexpr containers, for example.


### Source code

Only the `llvm/tools/clang` folder has been modified from the original llvm 7.0.0.

I have annotated with `//DWR ADDN` or `//DWR MOD` etc. everywhere I have added to or modified code from the original -- hopefully this will make it straightforward to port this into clang 9 or wherever it's at now.

I have done likewise -- `//ASUTTON ADDN` -- for wherever I have copied over or adapted code from Andrew Sutton's original repository, linked above (which is based around clang 5.x).

Mr. Sutton and co. have made subsequent progress along their own track for reflection & "reification" (their analog of metaparsing); I encourage you to also check out their newer work:
	
	https://gitlab.com/lock3/clang

I have looked at their source code, it is very impressive, but I have not adapted anything from it, as my implementation has diverged fairly significantly from theirs EXCEPTING key elements of Mr. Sutton's original 5.x code (`ConstexprDecl`, `CXXTupleExpansionStmt`, `CXXConcatenateExpr`, the idea for reflection classes as template instantiations to solve constexpr issues, various structural ideas, etc. -- you'll see all of ASUTTON's great contributions all annotated in my code).

The source code is a mess right now -- lots of big blocks of commented out code, etc.  I will clean it up it subsequent commits; but I thought it was good to have all the various old stuff I tried in there for the first commit, to help others who might try to adapt this to other purposes.

The clang tool for generating the reflection sources -- `llvm/tools/clang/tools/gen-reflection-src` -- is likewise a mess, needs to be completely rewritten for clarity - but darned if isn't working perfectly right now on translating just about the entire clang AST into reflection code.  And I have at least documented all the various hacks needed to make it work.

I would very much welcome others contributing to or fixing or adapting my code.  I have developed these contributions because I have been desperate to be able to USE C++ in this way -- got sick of having to BE a program to WRITE a program.  But I would rather not become endlessly bogged down keeping the implementation working.  So you would not be stepping on my toes in the least if you see improvements you'd like to implement and share.


I will address further issues here as they arise.  
Please let me know if you run into problems anywhere.

Have fun!

Dave
