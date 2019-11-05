//
//  str/eq.hpp
//  clang_meta_examples
//
//  Created by David Rector on 11/3/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_ce_str_eq_hpp
#define dwrmeta_ce_str_eq_hpp

constexpr bool ce_streq(char const * a, char const * b) {
    //  return *a == *b && (*a == '\0' || ce_streq(a + 1, b + 1));
    return __builtin_strcmp(a, b) == 0;
}

#endif /* dwrmeta_ce_str_eq_hpp */
