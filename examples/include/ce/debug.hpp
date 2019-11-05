//
//  debug.hpp
//  clang_meta_examples
//
//  Created by David Rector on 11/3/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_ce_debug_hpp
#define dwrmeta_ce_debug_hpp

#include "../client_reflection_impl.hpp"
#include "../meta/util/macros.hpp"

/////////
///
/// ce_debug_noline("no line");
/// ce_debug_noline("break");
///
#ifndef NMETA //meta-supporting compiler sees this:
# define ce_debug_noline(...) __compiler_debug(__concatenate(__VA_ARGS__))
#else         //non-meta-supporting IDE sees this:
# define ce_debug_noline(...)
#endif

/////////
///
/// ce_debug("first line");
/// ce_debug("second line");
///
#define ce_debug(...) ce_debug_noline(__VA_ARGS__, "\n")

/////////
///
/// ce_debug_var(x);
/// ce_debug_var( (SurroundAnythingW<Commas,In,Extra,Parens>()) );
///
#define ce_debug_var(x) ce_debug(#x, ": ", x)

// Helpers for ce_dump formatting (or rather "framing")
// (TODO allow user to adjust width via macro, e.g. #define DEBUG_WINDOW_WIDTH 80)
#define ce_debug_linesep_0() ce_debug("================================================================================")
#define ce_debug_linesep_1() ce_debug("--------------------------------------------------------------------------------")
#define ce_debug_linesep_2() ce_debug("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ")
#define ce_debug_newline()   ce_debug("")

/////////
///
/// ce_dump(obj, "optional header ", "string concatenation segments ", namestr)
///
/// ce_dump( (x->f(a,b,c)), "x->f(a,b,c) result dump" ); //<-surround the first arg in extra parens because of the commas
///
/// Adjust formatting below to taste.
///
#ifndef NMETA
# define ce_dump(x, ...) \
    ce_debug_newline();\
    ce_debug_linesep_0();\
    ce_debug("DUMP OF: \n", __VA_ARGS__);\
    ce_debug_linesep_1();\
    x->dump();\
    ce_debug_linesep_0();\
    ce_debug_newline();\
  /**/
#else
# define ce_dump(x, ...)
#endif

/////////
///
/// static_debug("a message");
/// static_dump(x, "x dump header");
///
/// Same as ce_debug/ce_dump, but for use outside a constexpr scope
/// (use in the same places you'd use e.g. a static_assert):
///

#define static_debug(...) DO_META { ce_debug(__VA_ARGS__); }
/// Be sure to enclose x in parens if it has commas:
#define static_dump(...)  DO_META { ce_dump(reflexpr(__VA_ARGS__), STRINGIZE((__VA_ARGS__))); }


/// To see the a dump of a metaclass instantiation, a macro won't suffice,
/// we need to do a little reflection first.
/// @tparam INST should be an instance of a template with
/// "using prototype = ..." line (just a known member we use
/// to force instantiation).
// TODO generalize this so it works for any instantiation, no prototype alias needed.
// And in fact perhaps try to merge this with ce_dump/static_dump so you don't need
// to select the right one, it does so automatically if a template is detected.
template<class INST>
constexpr void ce_dump_inst() {
  using namespace cppx::meta;
  using namespace cppx::meta::clang;
  [[maybe_unused]] typename INST::prototype *dummy_to_force_instantiation = {};
  [[maybe_unused]] INST *dummy_type_to_get_specialized_name = {};
  constexpr auto dummytyperefl = cast<VarDecl>(reflexpr(dummy_type_to_get_specialized_name));
  const char *name = dummytyperefl->getType()->getPointeeType().getAsString();

  ce_dump(cast<SubstTemplateTypeParmType>(reflexpr(INST))
              ->getReplacementType()->getAsCXXRecordDecl(),
              name);
}


//////////
///
/// ce_error / ce_warning / ce_note
///
/// See example in 2_custom_diagnostics for arg formatting instructions.
///
/// (NB ce_diag is defined in client_reflection_impl.hpp)
///
# define ce_error(loc, ...)    ce_diag(0, loc, __VA_ARGS__)
# define ce_warning(loc, ...)  ce_diag(1, loc, __VA_ARGS__)
# define ce_note(loc, ...)     ce_diag(2, loc, __VA_ARGS__)

/////////
///
/// ce_assert(constexpr_cond)
/// nce_assert(cond)
///
/// Just a wrapper of an if constexpr/if statement around a ce_error
/// fed with a null loc so it points back to itself.
///
/// nce_assert is probably sufficient in all cases now.
/// Initially it was important to use the ce_assert version where possible
/// to avoid a fold failure in the condition occurring without warning,
/// but I fixed that issue.  Still, maybe there are times when the
/// ce_assert version is needed, so I left it in.
///
#ifndef NMETA
#   define ce_assert(cond)  if constexpr ( !(cond) ) ce_error(0/*null loc*/, __concatenate("false: ", #cond))
#   define nce_assert(cond) if           ( !(cond) ) ce_error(0/*null loc*/, __concatenate("false: ", #cond))
#else
#   define ce_assert(cond)
#   define nce_assert(cond)
#endif



#endif /* dwrmeta_ce_debug_hpp */
