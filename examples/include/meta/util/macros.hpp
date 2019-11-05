//
//  macros.hpp
//  clang_meta_examples
//
//  Created by David Rector on 11/3/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_meta_util_macros_hpp
#define dwrmeta_meta_util_macros_hpp

#include "./nmeta.hpp"
#include "../../pputil.hpp"

///////
///
/// \def DO_META {...}   //use when in a non-function scope
/// \def DO_META_F {...} //use when in a function scope
///
#ifndef NMETA //meta-supporting compiler sees this:

# define DO_META    constexpr
# define DO_META_F  constexpr

#else //...whereas non-meta supporting IDEs see this:

// (From stackoverflow:)
# define PP_CAT0(a, b) PP_CAT0_I(a, b)
# define PP_CAT0_I(a, b) PP_CAT0_II(~, a ## b)
# define PP_CAT0_II(p, res) res
# define UNIQUE_NAME(base) PP_CAT0(base, __COUNTER__)
# define DO_META    constexpr void UNIQUE_NAME(dummy_cefunc) ()
  /// For ce decls within a function, need to use this one to avoid IDE flags:
# define DO_META_F  [[maybe_unused]] auto UNIQUE_NAME(dummylambda) = [&]()

#endif


///////////
///
/// \def DEFERRED_META { ... }; <-- note you need a semi at the end of this one
/// \def DEFERRED_META { ...; DM_BREAK ...; DM_BREAK ... }; //obsolete usage
///
#ifndef NMETA
/// NB new feature: brace-enclosed macro params.
/// All calls to DEFERRED_META must use braces too.
# define DEFERRED_META_0{...} QPARSE(STRINGIZE(DO_META {__VA_ARGS__} ))
# define DEFERRED_META{...}   PP_EXPAND(PP_DEFER(DEFERRED_META_0) {__VA_ARGS__} )
/// \def DM_BREAK
/// Can be placed these within a DEFERRED_META block to create "evaluation break points".
/// (We needed this for early versions of non-const reflected container usage, but I think
/// they are obsolete now.)
# define DM_BREAK             }; DEFERRED_META_0 PP_LBRACE()
#else
# define DEFERRED_META  DO_META_F
# define DM_BREAK
#endif



//////////
///
/// \def FOR (...)
///
/// \note I might be able to do away with the need for FOR/for...
/// in the next big update -- we might be able to use normal
/// for loops/normal iteration techniques.  Stay tuned. [DWR]
///
/// Usually best to give the type as an opening parenthetical, e.g.:
/// \code
///   FOR ((Decl *) D : reflDC->decls()) {...}
/// \endcode
/// This should help most IDEs provide code completion suggestions.
/// But you have to get the type right manually -- if IDE is depending on
/// this, it also has no way to check that that's the right type.
/// If you don't care about IDE support the below also works:
/// \code
///   FOR (auto D : reflDC->decls()) {...}
/// \endcode
/// or with no helper macro at all,
/// \code
///   for... (auto D : reflDC->decls()) {...}
/// \endcode
///
#ifndef NMETA
  // Again, meta-supporting compiler sees this...
# define PP_REPLACE_WITH_AUTO(...) auto
# define PP_REPLACE_INIT_PARENTHETICAL_WITH_AUTO(...)\
    PP_EXPAND(PP_REPLACE_WITH_AUTO __VA_ARGS__)
  /**/
# define PP_CNDL_REPLACE_INIT_PARENTHETICAL_WITH_AUTO(...)\
    PP_IIF(PP_IS_PAREN(__VA_ARGS__))(\
      /*true: */PP_REPLACE_INIT_PARENTHETICAL_WITH_AUTO(__VA_ARGS__),\
      /*false:*/PP_EXPAND(__VA_ARGS__)\
    )
  /**/
# define FOR(...) for... (PP_CNDL_REPLACE_INIT_PARENTHETICAL_WITH_AUTO(__VA_ARGS__))

#else

  // ... whereas non-meta-supporting IDEs see this:
# define PP_EXPAND_ANDMARKUSED_ANDCONST(...)\
    [[maybe_unused]] const  __VA_ARGS__ 
  /**/
# define PP_UNPARENTHESIZE_INIT_PARENTHETICAL(...)\
    PP_EXPAND(PP_EXPAND_ANDMARKUSED_ANDCONST __VA_ARGS__)
  /**/
# define PP_CNDL_UNPARENTHESIZE_INIT_PARENTHETICAL(...)\
    PP_IIF(PP_IS_PAREN(__VA_ARGS__))(\
      /*true: */PP_UNPARENTHESIZE_INIT_PARENTHETICAL(__VA_ARGS__),\
      /*false:*/PP_EXPAND(__VA_ARGS__)\
    )
  /**/
# define FOR(...) for (PP_CNDL_UNPARENTHESIZE_INIT_PARENTHETICAL(__VA_ARGS__))

#endif //NMETA


///////////
///
/// IF(...)
///
#ifndef NMETA
# define IF(...)          if constexpr (__VA_ARGS__)
#else
# define IF(...)          if (__VA_ARGS__)
#endif

// Use to taste:
#define ELSE  else


///////////
///
/// QPARSE("int i", "= 3;", "float", "f = "); QPARSE("2.2;");
///
#ifndef NMETA
# define QPARSE(...)       __queue_metaparse(__concatenate(__VA_ARGS__))
#else
# define QPARSE(...)
#endif


//////////
///
/// auto_
/// MAYBE_META_USED
///
#ifndef NMETA
# define auto_       constexpr auto
# define MAYBE_META_USED //blank
#else

/// Sometimes the IDE flags a reflection as non constexpr, because our
/// dummy implem doens't quite fool it; you can get rid of the
/// error by only applying the constexpr keyword when compiling (and,
/// for conveneince, adds the unused attribute when not compiling,
/// to keep the IDE from erroneously complaining about those unused
/// reflection variables).
/// Most reflection types should be typed with this.
# define auto_            [[maybe_unused]] auto

/// When you can't use auto_a and are getting the erroneous unused
/// variable flags, use MAYBE_META_UNUSED.  Use will still be checked
/// at compile time, but the IDE will ignore it.
# define MAYBE_META_USED  [[maybe_unused]]

#endif //NMETA

//////////
///
/// idrefl(refl)
///
/// To get IDE support/code completion to work on templated functions,
/// surround any uses of reflected parameter names with this.
/// Note it has no effect when compiling.
///
#ifndef NMETA
# define idrefl(...) __VA_ARGS__
#else
# define idrefl reflexpr
#endif



#endif /* dwrmeta_meta_util_macros_hpp */
