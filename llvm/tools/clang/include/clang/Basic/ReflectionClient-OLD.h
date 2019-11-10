//
//  ReflectionClient.h
//
//  Created by David Rector on 4/19/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef LLVM_REFLECTIONCLIENT_H
#define LLVM_REFLECTIONCLIENT_H

#if 0

//Feed CLIENT_HEADER_GEN into the preprocessor when generating
// the client header.  For Clang's internal use, leave it
// undefined -- this will prevent BLOCK_COMMENT/BLOCK_DOCUMENTATION
// from adding comments and will only generate what is minimally
// needed by Clang (specifically, the method enums and derived
// enums for each node):
#ifdef CLIENT_HEADER_GEN
# define ALLOW_PP_EXPANDED_COMMENTS
# define GEN_EVERYTHING 1
#else
# define GEN_EVERYTHING 0
#endif

#include "dwr/pp_tree.h"

#include "DeclNodesRefl.h"
//#include "StmtNodesRefl.h"
//DWR TODO any others?  Type?


/*=========================================================*/
//
// First, we need define the helper macros we need for
// our call to DWR_PP_TREE_FOR_EACH.
//
/*---------------------------------------------------------*/
#define GET_NODE_DATA(r, t, aux2tup, NodeID, OwnerID)       \
  GET_NODE_DATA_IMPL(aux2tup, NodeID)                       \
/*---------------------------------------------------------*/
#define GET_NODE_DATA_IMPL(aux2tup, NodeID)                 \
    BOOST_PP_CAT(                                           \
      BOOST_PP_CAT(                                         \
        PP_TUPELEM(2,0,aux2tup)/*prefix, e.g. REFL_*/       \
      , NodeID /*e.g. CXXRecord*/                           \
      )                                                     \
    , PP_TUPELEM(2,1,aux2tup) /*suffix, e.g. Decl */        \
    )                                                       \
/*---------------------------------------------------------*/
#define NODE_OPEN(r, t, aux2tup, NodeID, OwnerID)           \
  PP_EXPAND(BOOST_PP_CAT(                                   \
    NODE_HLPR_A_,                                           \
    BOOST_PP_IIF(GEN_EVERYTHING, EVERYTHING, JUSTCLANGSTUFF)\
  ))(r                                                      \
   , aux2tup                                                \
   , NodeID                                                 \
   , OwnerID                                                \
   , GET_NODE_DATA_IMPL(aux2tup,NodeID)                     \
   )                                                        \
/*=========================================================*/
#define NODE_HLPR_A_JUSTCLANGSTUFF(r                        \
                     , aux2tup, NodeID, OwnerID, TNodeData) \
NODE_HLPR_B_JUSTCLANGSTUFF(r                                \
, BOOST_PP_CAT(NodeID,PP_TUPELEM(2,1,aux2tup))  /*NodeName*/\
, PP_TUPELEM(2,1,TNodeData)                  /*ChildrenSeq*/\
, PP_TUPELEM(3,0,PP_TUPELEM(2,0,TNodeData))    /*MethodSeq*/\
, BOOST_PP_IIF(DWR_PP_IS_NULL(OwnerID)                      \
  , Base                                                    \
  , BOOST_PP_CAT(OwnerID,PP_TUPELEM(2,1,aux2tup))           \
  )                                             /*BaseName*/\
)                                                           \
/*---------------------------------------------------------*/
#define NODE_HLPR_B_JUSTCLANGSTUFF(r                        \
          , NodeName, ChildrenSeq, MethodSeq, BaseName)     \
namespace NodeName {                                        \
  inline namespace enums {                                  \
    CHILDREN_KINDS_ENUM(ChildrenSeq)                        \
    METHODS_ENUM(r, MethodSeq, BaseName)                    \
  }                                                         \
}                                                           \
/*=========================================================*/
#define NODE_HLPR_A_EVERYTHING(r                            \
                     , aux2tup, NodeID, OwnerID, TNodeData) \
NODE_HLPR_B_EVERYTHING(r, aux2tup, NodeID, OwnerID          \
, BOOST_PP_CAT(NodeID,PP_TUPELEM(2,1,aux2tup))  /*NodeName*/\
, DWR_PP_IS_NULL(OwnerID)                         /*IsRoot*/\
, BOOST_PP_IIF(DWR_PP_IS_NULL(OwnerID)                      \
  , XBase                                                   \
  , BOOST_PP_CAT(OwnerID,PP_TUPELEM(2,1,aux2tup))           \
  )<X>                                          /*BaseName*/\
, PP_TUPELEM(3,0,PP_TUPELEM(2,0,TNodeData))    /*MethodSeq*/\
, PP_TUPELEM(3,1,PP_TUPELEM(2,0,TNodeData))/*PsBaseActvSeq*/\
, PP_TUPELEM(3,2,PP_TUPELEM(2,0,TNodeData))/*IsPseudochild*/\
, PP_TUPELEM(2,1,TNodeData)                  /*ChildrenSeq*/\
, BOOST_PP_NOT(PP_SEQSIZE(PP_TUPELEM(2,1,TNodeData)))       \
                                              /* ^ IsLeaf*/ \
)                                                           \
/*---------------------------------------------------------*/
#define NODE_HLPR_B_EVERYTHING( r                           \
                   , aux2tup, NodeID, OwnerID, NodeName     \
                   , IsRoot, BaseName                       \
                   , MethodSeq, PsBaseActvSeq               \
                   , IsPseudochild, ChildrenSeq, IsLeaf )   \
template<reflection_t X>                                    \
struct NodeName : BaseName {                                \
/* 1. Static infrastructure for constructing/interpreting   \
      the dynamic type from the _fullnodenum stored in      \
      OrigBase.  This is needed for our cast/dyn_cast   \
      implems.*/                                            \
protected:                                                  \
  CHILDREN_KINDS_ENUM(ChildrenSeq)                          \
  static constexpr uint8_t child_nodenum_reltoprnt_numbits  \
    = detail::log2<num_children_plus_one_>::value;          \
  static constexpr uint8_t child_nodenum_reltoroot_numbits  \
    = BaseName::child_nodenum_reltoroot_numbits             \
      + child_nodenum_reltoprnt_numbits;                    \
  static_assert(child_nodenum_reltoroot_numbits             \
                <= OrigBase::MaxNodenumBits );              \
private:                                                    \
   static constexpr SmallNodeNumT nodenum_reltoprnt         \
     = BOOST_PP_IIF(IsRoot,0,BaseName::kinds::NodeID);      \
   static constexpr uint8_t nodenum_reltoprnt_numbits()     \
   { return BaseName::child_nodenum_reltoprnt_numbits; }    \
protected:                                                  \
  static constexpr NodeNumT nodenum_reltoroot               \
    = ( nodenum_reltoprnt                                   \
        << BaseName::nodenum_reltoroot_numbits()            \
      ) + BaseName::nodenum_reltoroot;                      \
  static constexpr NodeNumT nodenum_reltoroot_numbits() {   \
    return BaseName::child_nodenum_reltoroot_numbits;       \
  }                                                         \
  static const uint8_t ancdepth = BaseName::ancdepth + 1;   \
  static_assert(ancdepth <= OrigBase::MaxAncDepth);         \
/* 2. Public Constructor                                    \
 * RE the descnum_reltothis:                                \
 * If this is a root, you just pass on descnum_reltothis to \
 * OrigBase.  Otherwise, you tack on the                    \
 * current nodesmallnum info to that supplied by            \
 * kindsnum, and call the BaseName constructor              \
 * (which does the same thing ... until reaching ___Base)*/ \
public:                                                     \
  constexpr NodeName(                                       \
      BOOST_PP_IIF(IsLeaf,,NodeNumT descnum_reltothis = 0)  \
  )                                                         \
  : BaseName(                                               \
      BOOST_PP_IIF(IsLeaf,,                                 \
       (descnum_reltothis << nodenum_reltoprnt_numbits()) + \
      ) nodenum_reltoprnt                                   \
    )                                                       \
  {                                                         \
    BOOST_PP_EXPR_IIF(IsPseudochild,                        \
      BaseName::disableUpcast(BaseName::ancdepth);          \
    )                                                       \
    BOOST_PP_FOR_ ## r(                                     \
      ( PsBaseActvSeq, PP_TUPELEM(2,1,aux2tup)/*sfx*/ )     \
    , PP_PRED_T20_SEQSIZE                                   \
    , PP_OP_T20_SEQPOPFRONT                                 \
    , ENABLE_UPCAST_ITER                                    \
    )                                                       \
  }                                                         \
/*======= 3. Further casting infrastructure... =======*/    \
protected:                                                  \
/* --b) Protected constructor that assumes proper casting*/ \
  constexpr NodeName(const XBase<X>& other)                 \
  : BaseName(other)                                         \
  {} /*^ NB eventually calls OrigBase's copy ctor*/         \
/* --c) Protected constructor for constructing NULL object  \
        (obj whose operator bool() returns false): */       \
  constexpr NodeName(OrigBase::NullIndicator null)          \
  : BaseName(null)                                          \
  {}                                                        \
/* --d) friend decl for external cast/dyn_cast funcs:       \
   (so they can access the protected ctor) */               \
  template<template<reflection_t> class T, reflection_t Y>  \
  friend constexpr T<Y> cast(const XBase<Y>& u);            \
  template<template<reflection_t> class T, reflection_t Y>  \
  friend constexpr T<Y> dyn_cast(const XBase<Y>& u);        \
/* so isa can access nodenum stuff: */                      \
  template<template<reflection_t> class T, reflection_t Y>  \
  friend constexpr bool isa(const XBase<Y>& u);             \
/* so upcastEnabled can access ancdepth: */                 \
  template<template<reflection_t> class T>                  \
  friend constexpr bool OrigBase::upcastEnabled() const;    \
/*======= 4. Now for the actual METHODS...  =======*/       \
/* --a) If pseudochild, reversibly-DISABLE BaseName's       \
        methods by moving them into protected access: */    \
BOOST_PP_EXPR_IIF(IsPseudochild,                            \
 protected:                                                 \
  GEN_METHOD_USING_DECLS(r, aux2tup, OwnerID)               \
)                                                           \
/* --b) For any ancestor nodes specified in PsBaseActvSeq,  \
        RE-ENABLE their methods by making them public: */   \
 BOOST_PP_EXPR_IF(PP_SEQSIZE(PsBaseActvSeq), public:)       \
  BOOST_PP_FOR_ ## r(                                       \
    ( PsBaseActvSeq, aux2tup )                              \
  , PP_PRED_T20_SEQSIZE                                     \
  , PP_OP_T20_SEQPOPFRONT                                   \
  , GEN_METHOD_USING_DECLS_ITER                             \
  )                                                         \
/* --c) Define a methods enum that assign unique indices    \
        to each NEW method name.  The enum will prepend     \
        m_ to each so the names don't get confused          \
        with the actual method names. */                    \
protected:                                                  \
  METHODS_ENUM(r, MethodSeq, BaseName)                      \
/* --d) Define the current node's NEW methods (finally!): */\
public:                                                     \
  BOOST_PP_FOR_ ## r(                                       \
    MethodSeq                                               \
  , PP_PRED_SEQSIZE                                         \
  , PP_OP_SEQPOPFRONT                                       \
  , GEN_METHOD_DEF_ITER                                     \
  )                                                         \
};                                                          \
/*=========================================================*/
#define CHILDREN_KINDS_ENUM(ChildrenSeq)                    \
   enum kinds : SmallNodeNumT {                             \
     BOOST_PP_SEQ_ENUM(                                     \
      (root_ = 0)                                           \
      ChildrenSeq                                           \
      (num_children_plus_one_)                              \
     )                                                      \
   };                                                       \
/*---------------------------------------------------------*/
#define METHODS_ENUM(r, MethodSeq, BaseName)                \
  enum methods {                                            \
    BOOST_PP_FOR_ ## r(                                     \
      ( MethodSeq ((num_methods))/*tacked onto seq*/        \
      , 1/*is first = true*/                                \
      , BaseName                                            \
      )                                                     \
    , PP_PRED_T30_SEQSIZE /*defined in pp_common.h*/        \
    , PP_OP_T30_SEQPOPFRONT_REST_SET0 /*""*/                \
    , LIST_METHOD_NAMES_ITER                                \
    )                                                       \
  };                                                        \
/*---------------------------------------------------------*/
#define GEN_METHOD_USING_DECLS_ITER(r, state)               \
  GEN_METHOD_USING_DECLS(r                                  \
  , PP_TUPELEM(2,1,state) /*aux2tup*/                       \
  , PP_SEQELEM(0,PP_TUPELEM(2,0,state))/*ActivatedBaseID*/  \
  )                                                         \
/*---------------------------------------------------------*/
#define GEN_METHOD_USING_DECLS(r, aux2tup, NodeID)          \
  BOOST_PP_FOR_ ## r(                                       \
    ( PP_TUPELEM(3,0,PP_TUPELEM(2,0,                        \
        GET_NODE_DATA_IMPL(aux2tup, NodeID)                 \
      ))/*Owner's MethodSeq*/                               \
    , BOOST_PP_CAT(NodeID, PP_TUPELEM(2,1,aux2tup))<X>      \
    )                                                       \
  , PP_PRED_T20_SEQSIZE                                     \
  , PP_OP_T20_SEQPOPFRONT                                   \
  , PP_METHOD_USING_DECL_ITER                               \
  )                                                         \
/*---------------------------------------------------------*/
#define PP_METHOD_USING_DECL_ITER(r, state)                 \
  using PP_TUPELEM(2,1,state)/*NodeName*/                   \
    ::PP_TUPELEM(2,0,PP_SEQELEM(0,PP_TUPELEM(2,0,state)));  \
/*---------------------------------------------------------*/
#define ENABLE_UPCAST_ITER(r, state)                        \
  OrigBase::enableUpcast(                                   \
    BOOST_PP_CAT(PP_SEQELEM(0,PP_TUPELEM(2,0,state))/*ID*/  \
    , PP_TUPELEM(2,1,state) /*suffix*/                      \
    )<X>::ancdepth                                          \
  );                                                        \
/*---------------------------------------------------------*/
#define LIST_METHOD_NAMES_ITER(r, state)                    \
  BOOST_PP_CAT(  /*append _ to each method name*/           \
    PP_TUPELEM(0, PP_SEQELEM(0, PP_TUPELEM(3,0,state)))     \
  , _                                                       \
  )                                                         \
  BOOST_PP_EXPR_IIF(PP_TUPELEM(3,1,state)/*if first...*/    \
  , = PP_TUPELEM(3,2,state)/*BaseName*/::num_methods_       \
  ) BOOST_PP_COMMA_IF(                                      \
      BOOST_PP_DEC(PP_SEQSIZE(PP_TUPELEM(2,0,state)))       \
    )                                                       \
/*---------------------------------------------------------*/
#define GEN_METHOD_DEF_ITER(r, MethodSeq)                   \
  constexpr                                                 \
  PP_TUPELEM(2,1,PP_SEQELEM(0,MethodSeq))/*return type*/    \
  PP_TUPELEM(2,0,PP_SEQELEM(0,MethodSeq))/*name*/           \
  ()/*every method is nullary for now*/ const {             \
    return __reflect<                                       \
      BOOST_PP_CAT(   /*append _*/                          \
        PP_TUPELEM(2,0,PP_SEQELEM(0,MethodSeq))/*name*/     \
      , _                                                   \
      )                                                     \
    >();                                                    \
  }                                                         \
/*=========================================================*/
#define NODE_CLOSE(r, t, aux2tup, NodeID, OwnerID)
/*---------------------------------------------------------*/

//
// Now, we define the stuff referenced in the above macros,
// then make the call to DWR_PP_TREE_FOR_EACH for each
// root node (Decl, Stmt, etc.):
//
namespace Reflection {

  using SmallNodeNumT = uint8_t;

#ifdef CLIENT_HEADER_GEN

  using NodeNumT = unsigned; //probably best to infer this using metaprogramming
  using BitForEachBase = uint16_t; //assumes maxmimum of 16 bases -- a static assert will trip if you exceed

  using reflection_t = uintptr_t; //FIXME

  namespace detail {
    //log2 (from https://hbfs.wordpress.com/2016/03/22/log2-with-c-metaprogramming/)
    template <int x> struct log2    { enum { value = 1 + log2<x/2>::value }; };
    template <>      struct log2<1> { enum { value = 1 }; };

    template<typename INT>
    constexpr INT firstnbits(INT x, uint8_t n) {
      return x & ((1 << n) - 1);
    }

  } //detail

  template<reflection_t X>
  struct XBase;

  class OrigBase {
  protected: public: //DWR TEMP
    const NodeNumT      _fullnodenum;
  private:
    uint16_t            _AllowedBaseUpcasts;
    bool                _notnull;
    constexpr bool upcastEnabledImpl(uint8_t num) const {
      return (_AllowedBaseUpcasts >> num) % 2;
    }
  protected: public: //DWR TEMP
    static const auto MaxAncDepth = (sizeof(_AllowedBaseUpcasts)/sizeof(uint8_t)) * 8;
    static const auto MaxNodenumBits = (sizeof(_fullnodenum)/sizeof(uint8_t)) * 8;

    //Friend decls:
    template<template<reflection_t> class T, reflection_t X>
    friend constexpr bool isa(const XBase<X>& u);
    template<template<reflection_t> class T, reflection_t X>
    friend constexpr T<X> dyn_cast(const XBase<X>& u);

    //DWR FIXME need to rethink this since constexpr can't do assignment!!

    constexpr void disableUpcast(uint8_t num) {
      assert( upcastEnabledImpl(num)
             && "Expected upcast to be enabled before disabling");
      _AllowedBaseUpcasts = _AllowedBaseUpcasts & (~(1 << num));
    }

    constexpr void enableUpcast(uint8_t num) {
      assert(!upcastEnabledImpl(num)
             && "Expected upcast to be disabled before enabling");
      _AllowedBaseUpcasts = _AllowedBaseUpcasts | (1 << num);
    }

  public:
    template<template<reflection_t> class T>
    constexpr bool upcastEnabled() const {
      return upcastEnabledImpl(T<0>::ancdepth);
    }

    constexpr operator bool() const {
      return _notnull;
    }

    // Public Ctor
    constexpr OrigBase(NodeNumT fullnodenum)
      : _fullnodenum(fullnodenum), _AllowedBaseUpcasts(-1/*= all 1s*/), _notnull(true)
    {
      static_assert(std::is_unsigned<decltype(_AllowedBaseUpcasts)>::value);
    }

  protected: public: //DWR TEMP
    //Null-constructing ctor:
    struct NullIndicator {};
    constexpr OrigBase(NullIndicator) : _fullnodenum(0), _AllowedBaseUpcasts(0), _notnull(false)
    {}

    enum kinds : SmallNodeNumT {
      root_ = 0,
      Decl_,
//      Stmt_,
      //...etc...,
      num_children_plus_one_
    };
    enum methods {
      //Put any common methods here
      num_methods_
    };
    static constexpr uint8_t nodenum_reltoroot_numbits() { return 0; }
    static constexpr NodeNumT nodenum_reltoroot = 0;
    static constexpr uint8_t child_nodenum_reltoprnt_numbits
      = detail::log2<num_children_plus_one_>::value;
    static constexpr uint8_t child_nodenum_reltoroot_numbits
      = child_nodenum_reltoprnt_numbits;
    static const uint8_t ancdepth = 0;

  }; //class OrigBase

  template<reflection_t X>
  struct XBase : OrigBase
  {
    // Public Ctor
    constexpr XBase(NodeNumT fullnodenum) : OrigBase(fullnodenum) {}
  protected:
    //Null-constructing ctor:
    constexpr XBase(OrigBase::NullIndicator null) : OrigBase(null) {}
  };



  template<template<reflection_t> class T, reflection_t X>
  constexpr bool isa(const XBase<X>& u) {
      static_assert(std::is_base_of<XBase<X>,T<X>>::value,
                    "isa is only meaningful when testing a Reflection::Base "
                    "against its derived classes.");
      return ( detail::firstnbits(u._fullnodenum, T<X>::nodenum_reltoroot_numbits())
              == T<X>::nodenum_reltoroot
              && u.template upcastEnabled<T>()
              );
  }


  template<template<reflection_t> class T, reflection_t X>
  constexpr T<X> cast(const XBase<X>& u) {
    static_assert(std::is_base_of<XBase<X>,T<X>>::value,
                  "Cannot cast to class with no shared ancestor");
    assert(isa<T>(u) && "Bad cast!");
    return T<X>(u);
  }



  template<template<reflection_t> class T, reflection_t X>
  constexpr T<X> dyn_cast(const XBase<X>& u) {
    static_assert(std::is_base_of<XBase<X>,T<X>>::value,
                  "A dynamic cast to a class with no shared ancestor will always fail");
    if ( isa<T>(u) )
      return T<X>(u);
    return T<X>(OrigBase::NullIndicator());
  }


  /*
   TODO:
   --First of all, you can no longer use operator bool as returning _X.
    ---So instead, have a bool data member, and a construtor for it.
   --Secondly, you need to fix upcastEnabled etc.

   Okay hold on a sec.  To avoid having to always construct the same bases, can we have a
   */

#else //if JUSTCLANGSTUFF
  namespace Base {
    static const unsigned num_methods_ = 0;
  }
#endif

/*-------------------------------------------------------------*/
#define REFLECTION_SET_UP_METHOD_NUMBERING(MP_Root)             \
  DWR_PP_TREE_FOR_EACH( /*r*/1, /*t*/1                          \
    , /*aux*/       (REFL_,MP_Root)                             \
    , /*NodeID*/    /*blank: root data will be REFL_##MP_Root*/ \
    , /*OwnerID*/   DWR_PP_NULL                                 \
    , /*MacroNames*/(GET_NODE_DATA, NODE_OPEN, NODE_CLOSE)      \
    )                                                           \
/*-------------------------------------------------------------*/

//  REFLECTION_SET_UP_METHOD_NUMBERING(Decl)
//  REFLECTION_SET_UP_METHOD_NUMBERING(Stmt) //DWR TODO
//DWR TODO: actually, set up a sequence/tuple with Decl,Stmt, etc.
// then enumerate them in the Base::derived above, then do a loop over
// them here.








} //Reflection

// Undefines (may temporarily comment out during debug)
#undef REFLECTION_SET_UP_METHOD_NUMBERING
#undef GET_NODE_DATA
#undef NODE_OPEN
#undef NODE_CLOSE
//DWR TODO undefine the rest

#endif //0

#endif /* LLVM_REFLECTIONCLIENT_H */

