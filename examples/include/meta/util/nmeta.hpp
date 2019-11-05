//
//  nmeta.hpp
//  clang_meta_examples
//
//  Created by David Rector on 11/3/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_meta_util_nmeta_hpp
#define dwrmeta_meta_util_nmeta_hpp

// Simply defines NMETA from the inverse of an existing
// __CONSUMER_SUPPORTS_REFLECTION_AND_META__ macro (which
// meta-supporting compilers/IDEs should pre-define),
// for brevity.

#ifdef __CONSUMER_SUPPORTS_REFLECTION_AND_META__

# ifdef NMETA
    Problem -- NMETA defined elsewhere, but we need it undefined.
    Undef it before you get here, or change the name.
# endif

#else

# define NMETA

#endif //__CONSUMER_SUPPORTS_REFLECTION_AND_META__


#endif /* dwrmeta_meta_util_nmeta_hpp */
