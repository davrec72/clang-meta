//
//  pp_tree.h
//
//  Created by David Rector on 4/17/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef pp_tree_h
#define pp_tree_h

#include "pp_common.h"

/// \brief
///     Generalized macro (i.e. output depends not only on user
///     data but also on other macros whose names it takes as
///     parameters) for recursively applying functional macros to
///     tree-like data macros.
/// \param r
///     If calling from an outer macro that is also "r-parameterized",
///     i.e. is a BOOST_PP_... macro with an r
///     parameter or a macro defined using such macros,
///     pass the r, unchanged.  Otherwise, pass 1.  This is needed
///     to properly implement recursion.
/// \param t
///     Again, if calling outside a function macro, pass 1.
///     What r is for BOOST_PP_FOR, t is for DWR_PP_TREE_FOR_EACH:
///     the t is incremented and passed through to the
///     helper macros (see below), so that if your macros
///     need to perform a nested DWR_PP_TREE_FOR_EACH
///     call (in add'n to that being performed on the children),
///     you may pass their t to the nested call,
///     unchanged, and thereby avoid nested expansion issues.
/// \param aux
///     Holds any common aux data to be passed to each
///     of the macros in Macro3tup for each of the nodes.
/// \param NodeID
///     An identifier that will be used with aux and the
///     GET_NODE_DATA macro to get its associated data.
///     Note this can be blank, if the aux holds all the info needed
///     to get the root data.
/// \param OwnerID
///     The NodeID of the owner.  For root nodes (the usual
///     case), specify DWR_PP_NULL.
/// \param MacroName3tup
///     A 3-element tuple of functional macro names, each of which
///     should accept (r, t, aux, NodeID, OwnerID) input.
///     The elems should be structured as follows:
/// \code
///     ( /*0*/ GET_NODE_DATA
///     , /*1*/ NODE_OPEN
///     , /*2*/ NODE_CLOSE
///     )
/// \endcode
///
/// \param GET_NODE_DATA (r, t, aux, NodeID, OwnerID):
///     This macro must produce data with the following
///     structure:<BR>
///     \code
///       ( /*0*/ //...Arbitrary format...
///       , /*1*/ (ChildID1)(ChildID2)...
///       )
///     \endcode
/// \param NODE_OPEN, NODE_CLOSE
///     (r, aux, NodeID, OwnerID):<BR>
///     For each node we will call:
///     \code
///       NODE_OPEN(r, t, aux, TreeNodeID, OwnerID)
///       //Process children...
///       NODE_CLOSE(r, t, aux TreeNodeID, OwnerID)
///     \endcode
///     If you are using the OwnerID, you may test
///     if it equals DWR_PP_NULL -- i.e.
///     if the current node is a root -- by calling
///     DWR_PP_IS_NULL(OwnerID).
///
/*---------------------------------------------------------*/
#define DWR_PP_TREE_FOR_EACH( r, t                          \
                            , aux                           \
                            , NodeID                        \
                            , OwnerID                       \
                            , Macro3tup )                   \
    DWR_PP_TREE_FOR_EACH_ ## t( r, aux, NodeID              \
                              , OwnerID, Macro3tup )        \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_FOR_EACH_0( r                           \
                              , aux                         \
                              , NodeID                      \
                              , OwnerID                     \
                              , Macro3tup )                 \
  PP_TUPELEM(3,1,Macro3tup)(r, 1, aux, NodeID, OwnerID)     \
  DWR_PP_TREE_CHILDREN_0(r, aux, Macro3tup                  \
  , PP_TUPELEM(3,0,Macro3tup)(r, 1, aux, NodeID, OwnerID)   \
  , NodeID /*children's owner ID*/                          \
  )                                                         \
  PP_TUPELEM(3,2,Macro3tup)(r, 1, aux, NodeID, OwnerID)     \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_CHILDREN_0( r                           \
                              , aux                         \
                              , Macro3tup                   \
                              , TreeData2Tup                \
                              , OwnerID )                   \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(2,1,TreeData2Tup)                          \
    , aux                                                   \
    , Macro3tup                                             \
    , OwnerID                                               \
    )                                                       \
  , PP_PRED_T40_SEQSIZE                                     \
  , PP_OP_T40_SEQPOPFRONT                                   \
  , DWR_PP_ITER_TREE_0                                      \
  )                                                         \
/*---------------------------------------------------------*/
#define DWR_PP_ITER_TREE_0(r, state)                        \
  DWR_PP_TREE_FOR_EACH_1( r                                 \
  , PP_TUPELEM(4,1,state)/*aux*/                            \
  , PP_SEQELEM(0,PP_TUPELEM(4,0,state))/*NodeID*/           \
  , PP_TUPELEM(4,3,state)/*OwnerID*/                        \
  , PP_TUPELEM(4,2,state)/*Macro3tup*/                      \
  )                                                         \
/*=========================================================*/
/// RECURSIVE MACROS for processing branches
/// Exact copies of everything above that will be nested
/// within the above calls, except with incremented suffixes
#define DWR_PP_TREE_FOR_EACH_1( r                           \
                              , aux                         \
                              , NodeID                      \
                              , OwnerID                     \
                              , Macro3tup )                 \
  PP_TUPELEM(3,1,Macro3tup)(r, 2, aux, NodeID, OwnerID)     \
  DWR_PP_TREE_CHILDREN_1/*!*/( r, aux, Macro3tup            \
  , PP_TUPELEM(3,0,Macro3tup)(r, 2, aux, NodeID, OwnerID)   \
  , NodeID /*children's owner ID*/                          \
  )                                                         \
  PP_TUPELEM(3,2,Macro3tup)(r, 2, aux, NodeID, OwnerID)     \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_CHILDREN_1( r                           \
                              , aux                         \
                              , Macro3tup                   \
                              , TreeData2Tup                \
                              , OwnerID )                   \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(2,1,TreeData2Tup)                          \
    , aux                                                   \
    , Macro3tup                                             \
    , OwnerID                                               \
    )                                                       \
  , PP_PRED_T40_SEQSIZE                                     \
  , PP_OP_T40_SEQPOPFRONT                                   \
  , DWR_PP_ITER_TREE_1/*!*/                                 \
  )                                                         \
/*---------------------------------------------------------*/
#define DWR_PP_ITER_TREE_1(r, state)                        \
  DWR_PP_TREE_FOR_EACH_2/*!*/( r                            \
  , PP_TUPELEM(4,1,state)/*aux*/                            \
  , PP_SEQELEM(0,PP_TUPELEM(4,0,state))/*NodeID*/           \
  , PP_TUPELEM(4,3,state)/*OwnerID*/                        \
  , PP_TUPELEM(4,2,state)/*Macro3tup*/                      \
  )                                                         \
/*=========================================================*/
#define DWR_PP_TREE_FOR_EACH_2( r                           \
                              , aux                         \
                              , NodeID                      \
                              , OwnerID                     \
                              , Macro3tup )                 \
  PP_TUPELEM(3,1,Macro3tup)(r, 3, aux, NodeID, OwnerID)     \
  DWR_PP_TREE_CHILDREN_2/*!*/( r, aux, Macro3tup            \
  , PP_TUPELEM(3,0,Macro3tup)(r, 3, aux, NodeID, OwnerID)   \
  , NodeID /*children's owner ID*/                          \
  )                                                         \
  PP_TUPELEM(3,2,Macro3tup)(r, 3, aux, NodeID, OwnerID)     \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_CHILDREN_2( r                           \
                              , aux                         \
                              , Macro3tup                   \
                              , TreeData2Tup                \
                              , OwnerID )                   \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(2,1,TreeData2Tup)                          \
    , aux                                                   \
    , Macro3tup                                             \
    , OwnerID                                               \
    )                                                       \
  , PP_PRED_T40_SEQSIZE                                     \
  , PP_OP_T40_SEQPOPFRONT                                   \
  , DWR_PP_ITER_TREE_2/*!*/                                 \
  )                                                         \
/*---------------------------------------------------------*/
#define DWR_PP_ITER_TREE_2(r, state)                        \
  DWR_PP_TREE_FOR_EACH_3/*!*/( r                            \
  , PP_TUPELEM(4,1,state)/*aux*/                            \
  , PP_SEQELEM(0,PP_TUPELEM(4,0,state))/*NodeID*/           \
  , PP_TUPELEM(4,3,state)/*OwnerID*/                        \
  , PP_TUPELEM(4,2,state)/*Macro3tup*/                      \
  )                                                         \
/*=========================================================*/
#define DWR_PP_TREE_FOR_EACH_3( r                           \
                              , aux                         \
                              , NodeID                      \
                              , OwnerID                     \
                              , Macro3tup )                 \
  PP_TUPELEM(3,1,Macro3tup)(r, 4, aux, NodeID, OwnerID)     \
  DWR_PP_TREE_CHILDREN_3/*!*/( r, aux, Macro3tup            \
  , PP_TUPELEM(3,0,Macro3tup)(r, 4, aux, NodeID, OwnerID)   \
  , NodeID /*children's owner ID*/                          \
  )                                                         \
  PP_TUPELEM(3,2,Macro3tup)(r, 4, aux, NodeID, OwnerID)     \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_CHILDREN_3( r                           \
                              , aux                         \
                              , Macro3tup                   \
                              , TreeData2Tup                \
                              , OwnerID )                   \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(2,1,TreeData2Tup)                          \
    , aux                                                   \
    , Macro3tup                                             \
    , OwnerID                                               \
    )                                                       \
  , PP_PRED_T40_SEQSIZE                                     \
  , PP_OP_T40_SEQPOPFRONT                                   \
  , DWR_PP_ITER_TREE_3/*!*/                                 \
  )                                                         \
/*---------------------------------------------------------*/
#define DWR_PP_ITER_TREE_3(r, state)                        \
  DWR_PP_TREE_FOR_EACH_4/*!*/( r                            \
  , PP_TUPELEM(4,1,state)/*aux*/                            \
  , PP_SEQELEM(0,PP_TUPELEM(4,0,state))/*NodeID*/           \
  , PP_TUPELEM(4,3,state)/*OwnerID*/                        \
  , PP_TUPELEM(4,2,state)/*Macro3tup*/                      \
  )                                                         \
/*=========================================================*/
#define DWR_PP_TREE_FOR_EACH_4( r                           \
                              , aux                         \
                              , NodeID                      \
                              , OwnerID                     \
                              , Macro3tup )                 \
  PP_TUPELEM(3,1,Macro3tup)(r, 5, aux, NodeID, OwnerID)     \
  DWR_PP_TREE_CHILDREN_4/*!*/( r, aux, Macro3tup            \
  , PP_TUPELEM(3,0,Macro3tup)(r, 5, aux, NodeID, OwnerID)   \
  , NodeID /*children's owner ID*/                          \
  )                                                         \
  PP_TUPELEM(3,2,Macro3tup)(r, 5, aux, NodeID, OwnerID)     \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_CHILDREN_4( r                           \
                              , aux                         \
                              , Macro3tup                   \
                              , TreeData2Tup                \
                              , OwnerID )                   \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(2,1,TreeData2Tup)                          \
    , aux                                                   \
    , Macro3tup                                             \
    , OwnerID                                               \
    )                                                       \
  , PP_PRED_T40_SEQSIZE                                     \
  , PP_OP_T40_SEQPOPFRONT                                   \
  , DWR_PP_ITER_TREE_4/*!*/                                 \
  )                                                         \
/*---------------------------------------------------------*/
#define DWR_PP_ITER_TREE_4(r, state)                        \
  DWR_PP_TREE_FOR_EACH_5/*!*/( r                            \
  , PP_TUPELEM(4,1,state)/*aux*/                            \
  , PP_SEQELEM(0,PP_TUPELEM(4,0,state))/*NodeID*/           \
  , PP_TUPELEM(4,3,state)/*OwnerID*/                        \
  , PP_TUPELEM(4,2,state)/*Macro3tup*/                      \
  )                                                         \
/*=========================================================*/
#define DWR_PP_TREE_FOR_EACH_5( r                           \
                              , aux                         \
                              , NodeID                      \
                              , OwnerID                     \
                              , Macro3tup )                 \
  PP_TUPELEM(3,1,Macro3tup)(r, 6, aux, NodeID, OwnerID)     \
  DWR_PP_TREE_CHILDREN_5/*!*/( r, aux, Macro3tup            \
  , PP_TUPELEM(3,0,Macro3tup)(r, 6, aux, NodeID, OwnerID)   \
  , NodeID /*children's owner ID*/                          \
  )                                                         \
  PP_TUPELEM(3,2,Macro3tup)(r, 6, aux, NodeID, OwnerID)     \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_CHILDREN_5( r                           \
                              , aux                         \
                              , Macro3tup                   \
                              , TreeData2Tup                \
                              , OwnerID )                   \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(2,1,TreeData2Tup)                          \
    , aux                                                   \
    , Macro3tup                                             \
    , OwnerID                                               \
    )                                                       \
  , PP_PRED_T40_SEQSIZE                                     \
  , PP_OP_T40_SEQPOPFRONT                                   \
  , DWR_PP_ITER_TREE_5/*!*/                                 \
  )                                                         \
/*---------------------------------------------------------*/
#define DWR_PP_ITER_TREE_5(r, state)                        \
  DWR_PP_TREE_FOR_EACH_6/*!*/( r                            \
  , PP_TUPELEM(4,1,state)/*aux*/                            \
  , PP_SEQELEM(0,PP_TUPELEM(4,0,state))/*NodeID*/           \
  , PP_TUPELEM(4,3,state)/*OwnerID*/                        \
  , PP_TUPELEM(4,2,state)/*Macro3tup*/                      \
  )                                                         \
/*=========================================================*/
#define DWR_PP_TREE_FOR_EACH_6( r                           \
                              , aux                         \
                              , NodeID                      \
                              , OwnerID                     \
                              , Macro3tup )                 \
  PP_TUPELEM(3,1,Macro3tup)(r, 7, aux, NodeID, OwnerID)     \
  DWR_PP_TREE_CHILDREN_6/*!*/( r, aux, Macro3tup            \
  , PP_TUPELEM(3,0,Macro3tup)(r, 7, aux, NodeID, OwnerID)   \
  , NodeID /*children's owner ID*/                          \
  )                                                         \
  PP_TUPELEM(3,2,Macro3tup)(r, 7, aux, NodeID, OwnerID)     \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_CHILDREN_6( r                           \
                              , aux                         \
                              , Macro3tup                   \
                              , TreeData2Tup                \
                              , OwnerID )                   \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(2,1,TreeData2Tup)                          \
    , aux                                                   \
    , Macro3tup                                             \
    , OwnerID                                               \
    )                                                       \
  , PP_PRED_T40_SEQSIZE                                     \
  , PP_OP_T40_SEQPOPFRONT                                   \
  , DWR_PP_ITER_TREE_6/*!*/                                 \
  )                                                         \
/*---------------------------------------------------------*/
#define DWR_PP_ITER_TREE_6(r, state)                        \
  DWR_PP_TREE_FOR_EACH_7/*!*/( r                            \
  , PP_TUPELEM(4,1,state)/*aux*/                            \
  , PP_SEQELEM(0,PP_TUPELEM(4,0,state))/*NodeID*/           \
  , PP_TUPELEM(4,3,state)/*OwnerID*/                        \
  , PP_TUPELEM(4,2,state)/*Macro3tup*/                      \
  )                                                         \
/*=========================================================*/
#define DWR_PP_TREE_FOR_EACH_7( r                           \
                              , aux                         \
                              , NodeID                      \
                              , OwnerID                     \
                              , Macro3tup )                 \
  PP_TUPELEM(3,1,Macro3tup)(r, 8, aux, NodeID, OwnerID)     \
  DWR_PP_TREE_CHILDREN_7/*!*/( r, aux, Macro3tup            \
  , PP_TUPELEM(3,0,Macro3tup)(r, 8, aux, NodeID, OwnerID)   \
  , NodeID /*children's owner ID*/                          \
  )                                                         \
  PP_TUPELEM(3,2,Macro3tup)(r, 8, aux, NodeID, OwnerID)     \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_CHILDREN_7( r                           \
                              , aux                         \
                              , Macro3tup                   \
                              , TreeData2Tup                \
                              , OwnerID )                   \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(2,1,TreeData2Tup)                          \
    , aux                                                   \
    , Macro3tup                                             \
    , OwnerID                                               \
    )                                                       \
  , PP_PRED_T40_SEQSIZE                                     \
  , PP_OP_T40_SEQPOPFRONT                                   \
  , DWR_PP_ITER_TREE_7/*!*/                                 \
  )                                                         \
/*---------------------------------------------------------*/
#define DWR_PP_ITER_TREE_7(r, state)                        \
  DWR_PP_TREE_FOR_EACH_8/*!*/( r                            \
  , PP_TUPELEM(4,1,state)/*aux*/                            \
  , PP_SEQELEM(0,PP_TUPELEM(4,0,state))/*NodeID*/           \
  , PP_TUPELEM(4,3,state)/*OwnerID*/                        \
  , PP_TUPELEM(4,2,state)/*Macro3tup*/                      \
  )                                                         \
/*=========================================================*/
#define DWR_PP_TREE_FOR_EACH_8( r                           \
                              , aux                         \
                              , NodeID                      \
                              , OwnerID                     \
                              , Macro3tup )                 \
  PP_TUPELEM(3,1,Macro3tup)(r, 9, aux, NodeID, OwnerID)     \
  DWR_PP_TREE_CHILDREN_8/*!*/( r, aux, Macro3tup            \
  , PP_TUPELEM(3,0,Macro3tup)(r, 9, aux, NodeID, OwnerID)   \
  , NodeID /*children's owner ID*/                          \
  )                                                         \
  PP_TUPELEM(3,2,Macro3tup)(r, 9, aux, NodeID, OwnerID)     \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_CHILDREN_8( r                           \
                              , aux                         \
                              , Macro3tup                   \
                              , TreeData2Tup                \
                              , OwnerID )                   \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(2,1,TreeData2Tup)                          \
    , aux                                                   \
    , Macro3tup                                             \
    , OwnerID                                               \
    )                                                       \
  , PP_PRED_T40_SEQSIZE                                     \
  , PP_OP_T40_SEQPOPFRONT                                   \
  , DWR_PP_ITER_TREE_8/*!*/                                 \
  )                                                         \
/*---------------------------------------------------------*/
#define DWR_PP_ITER_TREE_8(r, state)                        \
  DWR_PP_TREE_FOR_EACH_9/*!*/( r                            \
  , PP_TUPELEM(4,1,state)/*aux*/                            \
  , PP_SEQELEM(0,PP_TUPELEM(4,0,state))/*NodeID*/           \
  , PP_TUPELEM(4,3,state)/*OwnerID*/                        \
  , PP_TUPELEM(4,2,state)/*Macro3tup*/                      \
  )                                                         \
/*=========================================================*/
#define DWR_PP_TREE_FOR_EACH_9( r                           \
                              , aux                         \
                              , NodeID                      \
                              , OwnerID                     \
                              , Macro3tup )                 \
  PP_TUPELEM(3,1,Macro3tup)(r, 10, aux, NodeID, OwnerID)    \
  DWR_PP_TREE_CHILDREN_9/*!*/( r, aux, Macro3tup            \
  , PP_TUPELEM(3,0,Macro3tup)(r, 10, aux, NodeID, OwnerID)  \
  , NodeID /*children's owner ID*/                          \
  )                                                         \
  PP_TUPELEM(3,2,Macro3tup)(r, 10, aux, NodeID, OwnerID)    \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_CHILDREN_9( r                           \
                              , aux                         \
                              , Macro3tup                   \
                              , TreeData2Tup                \
                              , OwnerID )                   \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(2,1,TreeData2Tup)                          \
    , aux                                                   \
    , Macro3tup                                             \
    , OwnerID                                               \
    )                                                       \
  , PP_PRED_T40_SEQSIZE                                     \
  , PP_OP_T40_SEQPOPFRONT                                   \
  , DWR_PP_ITER_TREE_9/*!*/                                 \
  )                                                         \
/*---------------------------------------------------------*/
#define DWR_PP_ITER_TREE_9(r, state)                        \
  DWR_PP_TREE_FOR_EACH_10/*!*/( r                           \
  , PP_TUPELEM(4,1,state)/*aux*/                            \
  , PP_SEQELEM(0,PP_TUPELEM(4,0,state))/*NodeID*/           \
  , PP_TUPELEM(4,3,state)/*OwnerID*/                        \
  , PP_TUPELEM(4,2,state)/*Macro3tup*/                      \
  )                                                         \
/*=========================================================*/
#define DWR_PP_TREE_FOR_EACH_10(r                           \
                              , aux                         \
                              , NodeID                      \
                              , OwnerID                     \
                              , Macro3tup )                 \
  PP_TUPELEM(3,1,Macro3tup)(r, 11, aux, NodeID, OwnerID)    \
  DWR_PP_TREE_CHILDREN_10/*!*/(r, aux, Macro3tup            \
  , PP_TUPELEM(3,0,Macro3tup)(r, 11, aux, NodeID, OwnerID)  \
  , NodeID /*children's owner ID*/                          \
  )                                                         \
  PP_TUPELEM(3,2,Macro3tup)(r, 11, aux, NodeID, OwnerID)    \
/*---------------------------------------------------------*/
#define DWR_PP_TREE_CHILDREN_10(r                           \
                              , aux                         \
                              , Macro3tup                   \
                              , TreeData2Tup                \
                              , OwnerID )                   \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(2,1,TreeData2Tup)                          \
    , aux                                                   \
    , Macro3tup                                             \
    , OwnerID                                               \
    )                                                       \
  , PP_PRED_T40_SEQSIZE                                     \
  , PP_OP_T40_SEQPOPFRONT                                   \
  , DWR_PP_ITER_TREE_10/*!*/                                \
  )                                                         \
/*---------------------------------------------------------*/
#define DWR_PP_ITER_TREE_10(r, state)                       \
  DWR_PP_TREE_FOR_EACH_11/*!*/( r                           \
  , PP_TUPELEM(4,1,state)/*aux*/                            \
  , PP_SEQELEM(0,PP_TUPELEM(4,0,state))/*NodeID*/           \
  , PP_TUPELEM(4,3,state)/*Macro3tup*/                      \
  , PP_TUPELEM(4,3,state)/*OwnerID*/                        \
  )                                                         \
/*=========================================================*/
#define DWR_PP_TREE_FOR_EACH_11(r, aux, NodeID, Macro3tup)  \
  BOOST_PP_ASSERT_MSG(0                                     \
  , "Need more copies of DWR_PP_TREE_FOR_EACH_x etc. "      \
    "in dwr/pp_tree.h"                                      \
  )                                                         \
/*=========================================================*/

#endif /* pp_tree_h */
