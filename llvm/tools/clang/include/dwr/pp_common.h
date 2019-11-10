//
//  pp_common.h
//
//  Created by David Rector on 4/17/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef pp_common_h
#define pp_common_h

//Commonly used BOOST_PP stuff:
#include <boost/preprocessor/repetition/for.hpp>
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/seq.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/debug/assert.hpp>
#include <boost/preprocessor/control.hpp> //IF, EXPR_IIF, etc.
#include <boost/preprocessor/logical.hpp> //OR, AND, etc.
#include <boost/preprocessor/punctuation/comma_if.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/vmd/is_empty.hpp>
//^ DWR TODO: is this variadic?  If so perhaps see if you can implem a non-variadic version,
// just to eliminate that dependency.

/*=========================================================*/
//
// Abbreviations etc:
//
#define PP_TUPELEM      BOOST_PP_TUPLE_ELEM
#define PP_SEQELEM      BOOST_PP_SEQ_ELEM
#define PP_SEQSIZE      BOOST_PP_SEQ_SIZE
#define PP_SEQPOPFRONT  BOOST_PP_SEQ_POP_FRONT
#define PP_IS_EMPTY     BOOST_VMD_IS_EMPTY
#define PP_COMMA        BOOST_PP_COMMA
#define PP_EMPTY()
#define PP_DEFER(id) id PP_EMPTY()
#define PP_OBSTRUCT(...) __VA_ARGS__ PP_DEFER(PP_EMPTY)()
#define PP_EXPAND(...) __VA_ARGS__
/*=========================================================*/
//
// DWR_PP_IS_NULL(x)
//
#define ID_DWR_PP_NULL_DWR_PP_NULL /*=empty*/
#define DWR_PP_IS_NULL(x) PP_IS_EMPTY(BOOST_PP_CAT(ID_DWR_PP_NULL_,x))
#ifdef DWR_PP_NULL
  "Must leave DWR_PP_NULL undefined!"
#endif
BOOST_PP_ASSERT_MSG( DWR_PP_IS_NULL(DWR_PP_NULL)
                    , "DWR_PP_IS_NULL(x) doesn't work")
/*=========================================================*/
//
// ALLOW_PP_EXPANDED_COMMENTS, BLOCK_COMMENT, etc.
//
// If this setting is engaged, you should ONLY be running
// the preprocessor, e.g. to generate a copy of a file with
// macros expanded but still with comments/documentation.
//
// If instead you try to also parse code with preprocessor-
// generated comments, you will probably get errors, as the
// compiler is probably expecting comments to have been removed
// after macro expansion.
//
#ifdef ALLOW_PP_EXPANDED_COMMENTS
#   define BLOCK_COMMENT(...) \/\*__VA_ARGS__\*\/
#   define LINE_COMMENT(...) \/\/__VA_ARGS__
#else
#   define BLOCK_COMMENT(...)
#   define LINE_COMMENT(...)
#endif
#define BLOCK_DOCUMENTATION(...) BLOCK_COMMENT(*__VA_ARGS__)
#define LINE_DOCUMENTATION(...) LINE_COMMENT(\/__VA_ARGS__)
/*=========================================================*/
//
// BOOST_PP_FOR preds/ops:
//
/*---------------------------------------------------------*/
#define PP_PRED_SEQSIZE(r, state) PP_SEQSIZE(state)
/*---------------------------------------------------------*/
#define PP_OP_SEQPOPFRONT(r, state) PP_SEQPOPFRONT(state)
/*---------------------------------------------------------*/
#define PP_PRED_T20_SEQSIZE(r, state)                       \
    PP_SEQSIZE(PP_TUPELEM(2,0,state))                       \
/*---------------------------------------------------------*/
#define PP_OP_T20_SEQPOPFRONT(r, state)                     \
  ( PP_SEQPOPFRONT(PP_TUPELEM(2,0,state))                   \
  , PP_TUPELEM(2,1,state)                                   \
  )                                                         \
/*---------------------------------------------------------*/
#define PP_PRED_T30_SEQSIZE(r, state)                       \
    PP_SEQSIZE(PP_TUPELEM(3,0,state))                       \
/*---------------------------------------------------------*/
#define PP_OP_T30_SEQPOPFRONT(r, state)                     \
  ( PP_SEQPOPFRONT(PP_TUPELEM(3,0,state))                   \
  , PP_TUPELEM(3,1,state)                                   \
  , PP_TUPELEM(3,2,state)                                   \
  )                                                         \
/*---------------------------------------------------------*/
#define PP_PRED_T40_SEQSIZE(r, state)                       \
    PP_SEQSIZE(PP_TUPELEM(4,0,state))                       \
/*---------------------------------------------------------*/
#define PP_OP_T40_SEQPOPFRONT(r, state)                     \
  ( PP_SEQPOPFRONT(PP_TUPELEM(4,0,state))                   \
  , PP_TUPELEM(4,1,state)                                   \
  , PP_TUPELEM(4,2,state)                                   \
  , PP_TUPELEM(4,3,state)                                   \
  )                                                         \
/*---------------------------------------------------------*/
// ...add more commonly-used preds/ops as you need them...
/*=========================================================*/
//
// PP_OP_Tx0_SEQPOPFRONT_REST_SET0
// These are useful when you only need certain data
// the first iteration; the 1st elem holds is_first,
// the 2...N elems hold the other data:
//
#define PP_OP_T20_SEQPOPFRONT_REST_SET0(r, state)           \
  ( PP_SEQPOPFRONT(PP_TUPELEM(2,0,state))                   \
  , 0/*is first = false*/                                   \
  )                                                         \
/*---------------------------------------------------------*/
#define PP_OP_T30_SEQPOPFRONT_REST_SET0(r, state)           \
  ( PP_SEQPOPFRONT(PP_TUPELEM(3,0,state))                   \
  , 0/*is first = false*/                                   \
  , /*omit*/                                                \
  )                                                         \
/*---------------------------------------------------------*/

#endif /* pp_common_h */
