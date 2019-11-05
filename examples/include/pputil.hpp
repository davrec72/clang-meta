//
//  pputil.hpp
//  clang_meta_examples
//
//  Created by David Rector on 11/3/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_pputil_hpp
#define dwrmeta_pputil_hpp

// Source of most of the preprocessor infrastructure here (great resource):
// https://github.com/pfultz2/Cloak/wiki/C-Preprocessor-tricks,-tips,-and-idioms

#define PP_CHECK_N(x, n, ...) n
#define PP_CHECK(...) PP_CHECK_N(__VA_ARGS__, 0,)
#define PP_PROBE(x) x, 1,

#define PP_CAT(a, ...) PP_PRIMITIVE_CAT(a, __VA_ARGS__)
#define PP_PRIMITIVE_CAT(a, ...) a ## __VA_ARGS__

#define PP_IIF(c) PP_PRIMITIVE_CAT(IIF_, c)
#define IIF_0(t, ...) __VA_ARGS__
#define IIF_1(t, ...) t

#define PP_IS_PAREN(x) PP_CHECK(IS_PAREN_PROBE x)
#define IS_PAREN_PROBE(...) PP_PROBE(~)

#ifndef PP_EXPAND
Expected PP_EXPAND to have been defined in client_reflection_impl.hpp! Did you remove it?
#endif
#ifndef PP_UNCNDL_REMOVE_PARENS
Expected PP_UNCNDL_REMOVE_PARENS to have been defined in client_reflection_impl.hpp! Did you rename it?
#endif
#define PP_REMOVE_PARENS(...)\
  PP_IIF(PP_IS_PAREN(__VA_ARGS__))(\
    /*true: */PP_UNCNDL_REMOVE_PARENS(__VA_ARGS__),\
    /*false:*/PP_EXPAND(__VA_ARGS__)\
  )
/**/

#define PP_LBRACE()   {
#define PP_LPAREN()   (

// (FIXME this macro doesn't always work -- maybe just port over the BOOST_PP_STRINGIZE,
// since it's super important to have a good working stringizer.)
//
/// This expands any macros in x and stringizes the result.
/// That is important because right now we're having some difficulty
/// parsing certain macros in __queue_metaparse statements --
/// this lets us get rid of them.
#define STRINGIZE(x) PP_DEFER(PP_STRINGIZE_SIMPLE)(x)
//`-Helpers:
#   define PP_EMPTY()
#   define PP_DEFER(id) id PP_EMPTY()
#   define PP_STRINGIZE_SIMPLE(x) #x

#endif /* dwrmeta_pputil_hpp */
