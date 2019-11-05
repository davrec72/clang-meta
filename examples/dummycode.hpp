//
//  dummycode.hpp
//  clang_meta_examples
//
//  Created by David Rector on 10/14/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dummycode_hpp
#define dummycode_hpp

namespace myns {
    class AAA {};
    AAA* myfunc(int i, char c = 'q');
    
    template<typename T, typename U>
    struct MyTmpl {
        T t;
        U u;
    };
}

inline namespace otherns {
    class BBB {};
}

class CCC {};


myns::AAA *myns::myfunc(int i, char c) {
    return (i ? new AAA() : nullptr);
}

#endif /* dummycode_hpp */
