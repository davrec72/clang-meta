//
//  reflect_arg.hpp
//  clang_meta_examples
//
//  Created by David Rector on 11/3/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_meta_util_reflect_arg_hpp
#define dwrmeta_meta_util_reflect_arg_hpp

#include "./macros.hpp"

template<typename T>
constexpr auto get_tmplarg_as_RD(T tparmrefl) {
    using namespace cppx::meta;
    using namespace cppx::meta::clang;
    auto TreflQT = cast<SubstTemplateTypeParmType>(idrefl(tparmrefl))->getReplacementType();
    auto TreflRD = TreflQT->getAsCXXRecordDecl();
    return TreflRD;
}

#endif /* dwrmeta_meta_util_reflect_arg_hpp */
