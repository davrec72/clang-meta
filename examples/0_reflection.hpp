//
//  0_reflection.hpp
//  clang_meta_examples
//
//  Created by David Rector on 8/11/19.
//  Copyright © 2019 David Rector. All rights reserved.
//
//  Demonstrates basic reflection capabilities.
//
//
// README FIRST:
//
// You may encounter occasional bugs that cause crashes.  Just before the stack dump,
// you will generally see which assert failed or which llvm_unreachable was reached.
// (Be sure to use only a debug-compiled version of this clang!)
// Let me know about such bugs; don't send them to llvm or whatever it says to do.
//
// HOWEVER, even when everything is working, about 1:10 to 1:20 builds will end in
// a crash, with NO explanation before the stack dump.
//
// Usually in clang this is the calling card of a nullptr dereference, I believe,
// but I am not an expert.
//
// It usually occurs during Sema::DoQueuedMetaparsing.  (If you take out all
// the __queued_metaparse/__metaparse_expr statements I believe you will not
// get any unexplained crashes.)
//
// This bug is my white whale.  I have tried many things, cannot find the cause.
// I've just about given up, have been just living with it for now.  (I guess I'm
// not much of an Ahab!)
//
// If you know your way around clang and can figure it out, I would be very
// impressed and eternally grateful!
//

#ifndef dwrmeta_0_reflection_hpp
#define dwrmeta_0_reflection_hpp

#include "include/metacore.hpp"
#include "dummycode.hpp"

namespace reflection_example_0 {
  
  using namespace cppx::meta;
  using namespace cppx::meta::clang;
  
// All the features introduced here -- reflection, metaparsing,
// custom diagnostics -- must be used within a constexpr function
// or constexpr { ... } block (aka "metaprogram").
//
// For clarity and to avoid erroneous IDE flags (a recurrent theme,
// since no current IDE understands these new keywords/features),
// we have defined a macro, DO_META, that translates to
// constexpr { ... } for the compiler, and to something less
// confusing for the IDE or any consumer that doesn't support
// this stuff.
//
// We'll use a lot more helper macros with the same purpose:
// tricking the IDE into supporting us and not flagging stuff
// it wouldn't understand.  It works surprisingly well.
//
// See include/meta/util/macros.hpp for most of these macros. A few more
// are in client_reflection_impl.hpp.
  
DO_META {
  
  // reflexpr(x) gives us an entry point into reflection.
  // That's all you strictly need to compile
  // (e.g. reflexpr(myns)->isAnonymousNamespace() compiles fine.)
  // But, until IDEs broadly support reflection/metaparsing,
  // to help your IDE make code suggestions etc.,
  // surround reflexpr(...) with a proper cast expression.
  // The cast will be verified at compile time.
  //
  auto_ mynsrefl = cast<NamespaceDecl>(reflexpr(myns));
  auto_ othernsrefl = cast<NamespaceDecl>(reflexpr(otherns));
  
//  auto_ badcast = cast<CXXRecordDecl>(reflexpr(otherns)); //ERROR
  
  auto_ nullcast = dyn_cast<CXXRecordDecl>(reflexpr(otherns));
  //^ [Clang AST detail]
  // A 'CXXRecordDecl' is a class/struct/union.
  // otherns is a NamespaceDecl, so this dyn_cast will return
  // a nullptr reflection.
  // RANT: CXXRecordDecl is poorly named -- to me
  // "record" suggests an INSTANCE of a class, i.e. an object,
  // but a CXXRecordDecl is the exact opposite -- the CLASS OF
  // a class.  Get used to typing it.
  
  ce_assert(!nullcast);
  // ^ ce_assert/nce_assert are defined as simple wrappers around
  // our custom diagnostics, which we'll flesh out later.
  // See include/ce/debug.hpp.
  
//  nullcast->isAnonymousStructOrUnion(); //ERROR
  
  // If your IDE/code completer is good, you should at this point
  // be able to get code suggestions for each cast<...>(reflexpr(...)),
  // along with the documentation as it appears in Clang's AST headers.
  //
  // E.g. type 'mynsrefl->', you should see method suggestions
  // and even their clang documentation!
  //
  // On Mac for me, this works fantastically in XCode...
  // and not at all in CLion.
  //
  // So, look around for an editing environment with a good code
  // completer -- it is very necessary, as the reflection library is massive.
  // EVERY conceivable property -- namespaces, classes, templates, variables,
  // initializer expressions, functions, the statements within functions
  // -- even reflection traits, __queue_metaparse statements, and the other new
  // expression types we introduce here, can be reflected, and later code
  // can be depend upon those reflected properties!
  //
  // Most reflection classes have a dump() method, which is useful for
  // figuring out what to dyn_cast to, etc.:
  //
  auto_ myfuncrefl = cast<FunctionDecl>(reflexpr(myns::myfunc));
  myfuncrefl->dump();
// reflexpr(myns::myfunc)->dump(); //Again, this works too, but IDE will annoyingly flag it.
  
  // Very useful: a ce_debug macro to display build-time messages.
  // Successive string args are concatenated.  Integer args are okay.
  ce_debug("Finished with ", "the above dump ca", "ll for ",
           myfuncrefl->getQualifiedNameAsString(), ";\n2 + 2 = ", 2+2);
  
  ce_debug_noline("ce_debug_noline does not end messages with a li");
  ce_debug_noline("ne break or even whitespace\n");
  
  // And a ce_dump macro, to enclose a dump in some light formatting
  // for readability.  Customize to taste, it's in ce/debug.hpp.
  // 2...N args are string chunks to be concatenated into a heading.
  ce_dump(cast<FunctionDecl>(reflexpr(myns::myfunc)), "myns::", "myfunc");
  
  // ^NOTE the debugs and dumps appear in the command line build
  // output, which is obscured in e.g. XCode.  CLion, QtCreator, and others
  // show the build output automatically.
  //
  // For XCode projects, best to have a command line
  // window open and compile from that when you need to see
  // ce_debug/ce_dump messages, like so:
  //
  // Open the Terminal, navigate to the project folder, and run:
  //
  //   xcodebuild -target mytargetname
  //
  // So long as one of mytargetname's source files has changed even minutely
  // (best to "touch" one automatically before the build),
  // or the last build failed, the target will be rebuilt and you should
  // see all your debug messages and dumps, alongside any diagnostic
  // messages.  (It is useful to clear the screen, via command-K, before
  // each build to distinguish messages between successive builds).
  //
  // Let's verify some reflected properties with ce_assert:
  //
  ce_assert(!mynsrefl->isAnonymousNamespace());
//  ce_assert(mynsrefl->isAnonymousNamespace()); //ERROR: uncomment to see failed assert message
  
  // For NamedDecls, you can get names as const char * literals via
  // getName() or getQualifiedNameAsString() (for the fully scope-qualified name).
  // getNameAsString() returns the same as getName() for us, but is deprecated in
  // clang, so probably best to use getName().
  //  --DWR EDIT: actually, getName() will cause a crash whenever you call it on a
  //    non-"identifier" (e.g. calling getName() from an operator decl) due to a failed
  //    assert within clang; whereas getNameAsString() will always work.
  //
  // Use ce_streq(a, b) to check string equality (ce/str/eq.hpp):
  //
  ce_assert( ce_streq(mynsrefl->getName(), "myns"));
  ce_assert(!ce_streq("myn", mynsrefl->getName()));
  ce_assert(!ce_streq(mynsrefl->getName(), "mynss"));
  ce_assert(!ce_streq(mynsrefl->getName(), "way off"));
  ce_assert(!ce_streq(mynsrefl->getName(), othernsrefl->getName()));
  
} // end of metaprogram


// Note you can make ordinary constexpr functions do metaprogramming tasks,
// and then call them from a metaprogram:

template<typename T, typename U>
constexpr void mymetafcn(const T reflA, const U reflB) {
  // Good practice: always begin a function taking reflection args
  // by cast<T>(...)'ing the arguments to the expected reflection Ts;
  // you will get an error automatically should the cast fail.
  //
  // WITHIN those casts though, ANOTHER note: surround them in the
  //
  //   idrefl(refl) = 'refl' at compile time
  //
  // helper macro, to assure the IDE that refl is a
  // reflection type, so it isn't flagged, and so the IDE
  // cooperates with code completion within the
  // function.
  auto_ nsreflA = cast<NamespaceDecl>(idrefl(reflA));
  auto_ nsreflB = cast<NamespaceDecl>(idrefl(reflB));

  // The IDE can't see the ce_debug usages of these variables,
  // so we mark them MAYBE_META_USED to get rid of the flags.
  // Or for brevity, auto_ works well for arbitrary types in
  // metaprograms, applying the MAYBE_META_USED automatically:
  //
  MAYBE_META_USED const char *Aname = nsreflA->getNameAsString();
  auto_                       Bname = nsreflB->getNameAsString();

  bool Ainl = nsreflA->isInline();
  bool Binl = nsreflA->isInline();

  if (Ainl)
    ce_debug(Aname, " is inline.");
  else
    ce_debug(Aname, " is not inline.");

  if (Binl)
    ce_debug(Bname, " is", (Ainl ? " also inline" : ", however, inline."));
  else
    ce_debug(Bname, (!Ainl ? " is not inline either." : ", however, is inline."));
}

//Now let's call the function from a metaprogram:
DO_META {
  mymetafcn(reflexpr(myns), reflexpr(otherns));
  // Observe the command-line output of the build command
  // to see the debug messages.
  // (They won't be in your run-time output!
  // This is all static reflection --
  // all these commands are done during build-time!)
}


// More convenience macros: static_debug/static_dump, to save yourself
// writing DO_META {...} when you're outside a constexpr context:
//
static_debug("Here's yet another dump of myns::refl, this time via static_dump:");
static_dump(myns::myfunc);
static_dump(myns::MyTmpl);
static_dump(myns::MyTmpl<int, float>);

// Instead of passing type reflections as arguments in constexpr functions,
// you can also reflect the typename args passed to templates, and thereby
// write old-fashioned type traits.
// HOWEVER, note that a template type argument is NOT a Decl,
// it is a Type (in clang AST terms) -- so we have to do some gymnastics to
// get to the underlying CXXRecordDecl, assuming one was passed (and not
// 'int' or something).
// I've factored this out into a get_tparm_as_rd function in
// meta/util/reflect_arg.hpp to save you the headache,
// but we'll do it directly this first time:
//
template<typename T>
class IsInInlineNamespace {
  static constexpr bool calcValue() {
    auto_ TreflQT = cast<SubstTemplateTypeParmType>(reflexpr(T))->getReplacementType();
    // ^Gives us the reflection of the QualType that has supplied at the T argument
    // to some instantiation

    // [Clang AST detail]
    // Use QualType::getCanonicalType() to FULLY dealias a type,
    // if an alias was passed as the T.  The result will not be an alias nor
    // qualified alias (e.g. const volatile myalias).
    // Very often you'll want to do this.
    // Use QualType::getSingleStepDesugaredType() to dealias the type by one step.
    // The result may still be an alias/qualified alias.
    //
    auto_ FullyDeAliasedType = TreflQT.getCanonicalType();

    ce_assert(!FullyDeAliasedType.hasQualifiers() &&
              "Expected a non-qualified type");
    //^ We'll see a MUCH better way to do these kinds of checks/diagnostics
    // in 2_custom_diagnostics.hpp; but we'll keep it simple for now.

    // [Clang AST detail]
    // Note that QualType implements its operator-> such that
    // QT->somemethod(...) = QT.getTypePtr()->somemethod(...).
    // (A QualType is just a Type with const/volatile/restrict/a few other
    // possible qualifiers attached.)
    //
    auto_ TreflRD = FullyDeAliasedType->getAsCXXRecordDecl();
    //^ [Clang AST detail] getAs... functions generally return null
    // if the cast would not be valid; just like dyn_cast,
    // except here we're going from a Type to a Decl.

    // Need to use IF (= if constexpr) here, or else when
    // we access TreflRD->... below we would get a nullptr
    // reflection error:
    IF (!TreflRD)
      return false;

    // [Clang AST detail]
    // DeclContext: very important.
    //
    // Decl::getDeclContext()   = gets the parent context.
    // DeclContext::getParent() = also gets the parent context,
    //   for Decl types that are themselves DeclContexts.
    //
    // So e.g. SomeDecl->getDeclContext()->getParent()
    // gets the grandparent context, if it exists.
    //
    // getDeclContext() / getParent() will return nullptr
    // ONLY for the TranslationUnitDecl.
    //
    // Clang veterans: note that there is no need to do
    // Decl::castFromDeclContext / Decl::castToDeclContext
    // as you have to in clang proper sometimes. Just use ordinary
    // cast<...>(...) to and from DeclContext like anything else.
    //
    auto_ DC = TreflRD->getDeclContext();
    ce_assert(DC && "Sanity check (a class should always have a nonnull context)");
    auto_ ND = dyn_cast<NamespaceDecl>(DC);

    IF (!ND)
      return false; //Its parent is not namespace at all

    return ND->isInline();
  }
public:
  static constexpr bool value = calcValue();
};


// We don't want the IDE to try running the static asserts, since it's
// looking at dummy classes/methods, so always enclose tests like these
// in #ifndef NMETA to make the IDE outright ignore them.
//
// The IDE will skip over the following block, but the compiler will process it:

#ifndef NMETA

static_assert(!IsInInlineNamespace<myns::AAA>::value);
static_assert(IsInInlineNamespace<BBB>::value);
static_assert(!IsInInlineNamespace<CCC>::value);
//static_assert(IsInInlineNamespace<CCC>::value); //ERROR
  
static_debug("static_assert tests of IsInInlineNamespace passed");
  
#endif

} //namespace reflection_example_0

#endif //dwrmeta_0_reflection_hpp
