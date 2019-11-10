//
//  SmallVector.h
//  diamonds_to_lightning
//
//  Created by David Rector on 5/28/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef SmallVector_h
#define SmallVector_h

#include <vector>

namespace dwr {
    
    //TEMP
    template<typename T>
    using SmallVector = std::vector<T>;
    template<typename T, size_t N>
    using SmallVectorImpl = std::vector<T>;
    
    // Helper function
    template<typename T>
    bool contains_elem(const SmallVector<T> &container, T elem) {
        for (auto m : container)
            if (m==elem)
                return true;
        return false;
    }
    
}

#endif /* SmallVector_h */
