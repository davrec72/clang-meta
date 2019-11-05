//
//  2_custom_diagnostics.hpp
//  clang_meta_examples
//
//  Created by David Rector on 10/13/19.
//  Copyright © 2019 David Rector. All rights reserved.
//
//  Here we write a metafunction that demonstrates our custom diagnostics capabilities.

#ifndef dwrmeta_2_custom_diagnostics_hpp
#define dwrmeta_2_custom_diagnostics_hpp

#include "include/metacore.hpp"

///////// Dummy code //////////////

namespace myproj1 {

    /// Doc for A
    /// A second line
    struct A {
        
        /// Doc for A::f
        A f() { return A(); }
    };
    
    struct B {
        /// Doc for B::g
        void g() {}
        
        void h() {}
    private:
        void i() {}
    };
    
    namespace mysubprojI {
        
        /// Doc for C
        struct C {
            
        };
        
        // Not a doc, just a comment, so expect a complaint...
        struct D {
            
        };
    }
}
////////// End dummy code //////////


// Let's write a metafunction to check if a reflection,
// and any of its members, where applicable, have documentation.
//
// ce_asserts are quick and simple, much better than nothing,
// but they're very cold and unfeeling, nag you rather than
// helping you fix the issue.  Good for prototyping and sanity checks
// that you expect never to see fail; not so good for verifying complicated
// conditions in metaprograms and metaclasses that are sure to be misused
// by a user.
//
// So I have added a constexpr pipeline directly into clang's elegant
// diagnostics system.
//
// These diagnostics are supported natively in XCode 9 for me --
// when I build, the diagnostics pop up on my screen at the locations
// I expect.  Be sure your coding environment is equally supportive,
// it is a great feature.
//

namespace custom_diagnostics_example {
    using namespace cppx::meta;
    using namespace cppx::meta::clang;
    
    template<typename T>
    constexpr void CheckAllMembersHaveDoc(T refl) {
        
        // Let's recurse to child members first.
        //
        // Need to use IF (= 'if constexpr') here
        // to avoid a null reflection ptr cast when the
        // dyn_cast fails.
        //
        IF (auto_ DC = dyn_cast<DeclContext>(idrefl(refl))) {
            
            // Let's first address constexpr iteration.
            //
            // It is possible in the near future I can make the below obsolete,
            // so we can use normal iteration techniques. (See 4_constexpr_containers.hpp
            // for further discussion.)
            //
            // But for now, whenever we want to loop over reflection objects,
            // we must rely on Andrew Sutton's excellent
            // "for..." iteration facilities, called CXXTupleExpansionStmt in clang.
            // I have simply built a helper macro around it to get the IDE
            // to cooperate, so you can e.g. get code completion within the
            // loop body.
            //
            FOR ((Decl *) D : DC->decls()) {
                
                // [clang AST detail]
                // CRITICAL STEP in loops over class members:
                // You usually want to skip over the implicit (i.e. hidden) members,
                // e.g. the injected class name, the default constructors/destructor,
                // etc.
                // If you don't, note that the location of any implicit decl
                // will be that of the parent, so any error using that location
                // will appear to apply to the parent decl, causing confusion.
                if (D->isImplicit())
                    continue;
                
                // Recursive call:
                CheckAllMembersHaveDoc(D);
            }
        }
        
        // 2. We're done with recursion where applicable.
        // Now, check the current decl for documentation.
        IF (auto_ ND = dyn_cast<NamedDecl>(idrefl(refl))) {
            ce_debug("Processing ", ND->getQualifiedNameAsString());
            
            // Namespaces don't need documentation though.
            if (isa<NamespaceDecl>(ND)) {
                ce_debug("`--it's a namespace, returning");
                return;
            }
            
            // We'll dump the text so you can see how to get it, if you
            // want to futz with it in another metafunction.
            // Gotta do some gymnastics to get it.
            auto_ C = ND->getASTContext();
            IF (auto_ rawcomment = C.getRawCommentForDeclNoCache(ND)) {
                MAYBE_META_USED const char *rawtext = rawcomment->getRawText(C.getSourceManager());
                ce_debug("`--has documentation; displaying text: ", rawtext);
                nce_assert(rawtext && "Didn't expect empty doc!");
            } ELSE {
                // No documentation found!
                // HERE IT IS, our custom diagnostic definition:
                //
                ce_warning(// Arg 0: Provide a meaningful reflected SourceLocation
                           // here, or 0 if you want it to point here like a ce_assert would:
                             ND->getBeginLoc()
                           // Arg 1: Note that user diag messages will always be enclosed in double parens to
                           // distinguish user diags from compiler diags.  I baked that into the clang source,
                           // not just in a macro -- so no you can't use this to prank/gaslight your coworkers
                           // with official-looking errors, sorry...
                           , "Documentation needed"
                           // (Args 2...n): There can be as many additional "FixItHint" args after this point as you want.
                           // Each arg must be a user::FixItHint, which in turn is constructed via one of three static funcs
                           // (try commenting each of the below out, probably best to do one at at time, though not necessary):
                           // (a) user::FixitHint::CreateInsertion
                           , user::FixItHint::CreateInsertion(ND->getBeginLoc(),
                                                              __concatenate("/// @name ", ND->getNameAsString(), "\n"
                                                                            "/// @brief TODO Add real documentation\n"))
    //                       // (b) user::FixitHint::CreateRemoval
    //                       , FixItHint::CreateRemoval(user::SourceRange(ND->getBeginLoc(), ND->getEndLoc()));
                           
    //                       // (c) user::FixitHint::CreateReplacement
    //                       , user::FixItHint::CreateReplacement(user::SourceRange(ND->getBeginLoc(), ND->getEndLoc()),
    //                                                            "/// Replacement for undocumented declaration\n"
    //                                                            "void probablynotagoodfix() {}\n")
                           
                           //, ...
                           // Supply as many of each as you want; ordering doesn't seem to matter.
                           // If you've got a really sophisticated diagnostic, with lots of fixits,
                           // should be no problem supplying a parameter pack of user::FixItHints... here.
                           );
                
                ce_note( ND->getEndLoc(), "Some note at end of decl" );
                // ^ THEN, AFTER a diagnostic, you can tack on as many "ce_note(...)s" as you'd like.
                // The purpose of notes is to add information about the cause(s) of the diagnostic.
                // They will not display if there was no diagnostic triggered before them.
                // But if there was, each note will apply to (each? the last? TESTME) such diagnostic
                // in the current context (DWR TESTME make sure that notes can be added in
                // a separate function call from the orig diagnostic, i.e. while exiting a stack).
                //
                // Might thus generally be good policy to add a ce_note at the end of each call
                // in which there might be a diagnostic triggered.  (Or better yet enforce
                // such policy with a metafunction -- no end to how meta you can get,
                // diagnosing other diagnostics etc.  The AST node of interest would be
                // "CompilerDiagnosticExpr".)
            }
        }
    }

    DO_META {
        auto_ myproj1refl = cast<NamespaceDecl>(reflexpr(myproj1));
        
        // DWR TODO: for custom diagnostics a note should be added here where the call
        // was made, and any other intervening function calls that led to the diagnostic;
        // otherwise the actual call might be heavily obscured in some forsaken .cpp
        // deep in the project...
        CheckAllMembersHaveDoc(myproj1refl);
    }
    
} //namespace custom_diagnostics_example


// Last notes on diagnostics:
// --There is of course a ce_error(...) macro defined,
//   analogous to ce_warning(...) used above
// --You can parametize whether to do a warning vs. error via a generalized
//      ce_diag(warn0err1, loc, msg, FixItHints...)


#endif /* dwrmeta_2_custom_diagnostics_hpp */
