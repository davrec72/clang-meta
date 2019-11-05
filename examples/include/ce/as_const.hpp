//
//  as_const.hpp
//  newclang_test2
//
//  Created by David Rector on 11/4/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_ce_as_const_hpp
#define dwrmeta_ce_as_const_hpp

namespace ce {
    
    /// Your const-qualified operator const char *() for classes you want to be
    /// able to push onto vectors should __concatenate this around
    /// the non-const qualified version (see the ce::vector/map/set examples):
    template<typename T>
    constexpr const T as_const(T &&t) { return std::move(t); }
    
}

#endif /* dwrmeta_ce_as_const_hpp */
