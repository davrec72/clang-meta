//
//  apply_to_bases.hpp
//  clang_meta_examples
//
//  Created by David Rector on 10/18/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_meta_util_apply_to_bases_hpp
#define dwrmeta_meta_util_apply_to_bases_hpp

#include "./inhwrp.hpp"

/// Modifies an inhwrp tuple by changing each component
/// basespec's class BC to TMPL<BC>.
///
/// The access and virtual-ness remain unchanged.
///
/// @tparam TMPL should have a 'using type = ' line.
///
template<template<typename> class TMPL, typename T>
struct apply_to_base_classes;

template<template<typename> class TMPL, typename T>
using apply_to_base_classes_t = typename apply_to_base_classes<TMPL, T>::type;

//SPEC: T is a basespec
template< template<typename> class TMPL
        , typename BC, AccessSpecifier ACC, bool VIR >
struct apply_to_base_classes< TMPL, basespec<BC, ACC, VIR> > {
    using type = basespec<TMPL<BC>, ACC, VIR>; //maintain same ACC and VIR
};

//SPEC: T is an inhwrp
template< template<typename> class TMPL
, typename... Ts >
struct apply_to_base_classes< TMPL, inhwrp<Ts...> > {
    using type = inhwrp< apply_to_base_classes_t<TMPL, Ts>... >;
};


/// Modifies an inhwrp tuple by changing each component
/// basespec BASESPEC to TMPL<BASESPEC>::type.
///
/// Use this when you want to be able to modify the
/// access and virtual-ness of each basespec.
///
/// @tparam TMPL should have a 'using type = ' line.
///
template<template<typename> class TMPL, typename T>
struct apply_to_basespecs {
    using type = typename TMPL<T>::type;
};

template<template<typename> class TMPL, typename T>
using apply_to_basespecs_t = typename apply_to_basespecs<TMPL, T>::type;

//SPEC: T is an inhwrp
template< template<typename> class TMPL
, typename... Ts >
struct apply_to_basespecs< TMPL, inhwrp<Ts...> > {
    using type = inhwrp< apply_to_basespecs_t<TMPL, Ts>... >;
};

#endif /* dwrmeta_meta_util_apply_to_bases_hpp */
