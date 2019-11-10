//
//  MetaparseDebug.h
//  forceSingleInheritance
//
//  Created by David Rector on 5/14/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef METAPARSEDEBUG_h
#define METAPARSEDEBUG_h

#include <cassert>

#ifndef DEBUGOUT
#ifndef NDEBUG
# define DEBUGOUT(x) std::cout << "//////[DEBUG] " << #x << ": " << x << "\n"
#else
# define DEBUGOUT(x)
#endif
#endif

#endif /* METAPARSEDEBUG_h */
