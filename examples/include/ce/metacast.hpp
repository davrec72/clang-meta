//
//  metacast.hpp
//  newclang_test2
//
//  Created by David Rector on 11/4/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_ce_metacast_hpp
#define dwrmeta_ce_metacast_hpp

namespace ce {

    // Right now using something like this is the only reliable way to stringize --
    // i.e. to be sure all the inner string's characters get escaped etc.
    // Preprocessor stringize won't work of course -- we need to stringize the
    // contents of a variable, PP is useless for that.  (It complicates matters
    // that I haven't implemented scope entry/lookup for __metaparse_expr --
    // you always have to provide fully-qualified, publicly accessible names.)
    // TODO figure out how to restrict access -- only only push_back publicly.
    // Probably set up another container wrapper to handle that.
    static auto metacast_string_storage = *reflnew(reflcontainers::VectorStr);

    //TODO: pointer/cv-qualified overloads for metacast

    /// metacast: for normal class Ts, relies on their
    /// operator const char *().  For string literals,
    /// stringizes the result.  For integers, just does
    /// a simple cast, for now; not sure if anything more
    /// is needed for them.
    /// This is only really used to implement the constexpr containers
    /// (needed to store arbitrary types as their meta-type -- a string literal).
    template<typename TARG,
             typename T>
    constexpr TARG metacast(T t) {
        if constexpr (std::is_same<TARG, const char *>::value) {
            if constexpr (std::is_same<T, const char *>::value) {
                // a variable stringizing feature (NOT preprocessor) would also work here, but
                // we don't have that; here's what I came up with:
                metacast_string_storage.push_back(t);
                return __metaparse_expr(__concatenate("\"ce::metacast_string_storage.at(", metacast_string_storage.size() - 1, ")\""), const char *);
            } else if constexpr (std::is_integral<T>::value) {
                return __concatenate(t); //I doubt you'll use this one much
            } else {
                static_assert(std::is_class<T>::value,
                              "Only integer/string literal and class source types are permitted for now");
                return static_cast<TARG>(t); //
            }
        } else {
            static_assert (std::is_integral<TARG>::value,
                           "Only integer and const char * TARG types are supported for now");
            static_assert(std::is_integral<T>::value,
                          "Only ints can be metacast to ints for now");
            return static_cast<TARG>(t);
        }
    }
    
}

#endif /* dwrmeta_ce_metacast_hpp */
