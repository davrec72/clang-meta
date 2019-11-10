// -*- C++ -*-

#ifndef CPPX_TRAITS_H
#define CPPX_TRAITS_H

#include <cstdint>

//DWR TODO: put all the meaningful get functions (e.g. bool is_const {...})
// in THESE structs, then the reflection stuff should use PUBLIC traits()
// functions, and no pass-through functions.  That way, the user will be encouraged to separately construct a traits variable and test against that instead of constructing it multiple times, whenever they need to check multiple traits.

namespace cppx
{
namespace meta
{
inline namespace v2
{

  namespace kw {
#define DEF_KEY(X) static constexpr struct X##T {} X;
    DEF_KEY(Static)
    DEF_KEY(Extern)
    DEF_KEY(ThreadLocal)
    DEF_KEY(Constexpr)
    DEF_KEY(Inline)
    DEF_KEY(Virtual)
    DEF_KEY(Pure)
    DEF_KEY(Override)
    DEF_KEY(Final)
    DEF_KEY(Noexcept)
    DEF_KEY(Const)
    DEF_KEY(Volatile)
    DEF_KEY(Restrict)
    DEF_KEY(Default)
    DEF_KEY(Delete)
    DEF_KEY(Public)
    DEF_KEY(Private)
    DEF_KEY(Protected)
    DEF_KEY(Mutable)
    DEF_KEY(Register)
    DEF_KEY(Explicit)
    DEF_KEY(Class) //for scoped enums
#undef DEF_KEY

  }

  /// Bitwise-or operator
  /// @brief
  ///     For combining new keywords onto an existing traits structure.
  /// @example
  ///     auto myfunctraits = function_traits() | StaticT() | ConstexprT();
  template<class TRAITS, class KW>
  inline constexpr TRAITS operator| (TRAITS a, KW k) {
    return TRAITS( a, TRAITS(k) );
  }

  
namespace detail {
  template<class KEY, class... Ts>
  struct first_matches_any_of_rest;

  template<class KEY>
  struct first_matches_any_of_rest<KEY>
      : std::false_type {};

  template<class KEY, class... Ts>
  struct first_matches_any_of_rest<KEY, KEY, Ts...>
      : std::true_type {};

  template<class KEY, class NOMATCH, class... Ts>
  struct first_matches_any_of_rest<KEY, NOMATCH, Ts...>
      : first_matches_any_of_rest<KEY, Ts...> {};
} //detail

  


// -------------------------------------------------------------------------- //
// Specifiers and traits
//
// NOTE: These order and structure of the _info classes needs to be kept in
// sync with the compiler.
// ^-DWR ADDN: specifically, lib/Sema/SemaReflect.cpp -- see ReflectTraits(...) etc.

enum access_kind : unsigned {
  AK_none,
  AK_public,
  AK_private,
  AK_protected,
};
  
enum linkage_kind : unsigned {
  LK_none,
  LK_internal,
  LK_external
};

enum storage_kind : unsigned {
  SK_none,
  SK_static,
  SK_automatic,
  SK_thread_local,
};

namespace detail {
  // The access of a bitfield is always stored in the first 2 bits.
  static constexpr access_kind get_access(unsigned n = 0) {
    return access_kind(n & 0x03);
  }
  // Linkage specifiers are always stored in bits 3 and 4.
  static constexpr linkage_kind get_linkage(unsigned n = 0) {
    return linkage_kind((n & 0x0c) >> 2);
  }
  // Storage specifiers are stored in bits 5 and 6.
  static constexpr storage_kind get_storage(unsigned n = 0) {
    return storage_kind((n & 0x30) >> 4); //DWR changed bitshift from 2 to 4
  }
}




struct access_spec_traits {
  constexpr explicit access_spec_traits(unsigned n = 0)
    : access       (detail::get_access(n)) // 0x04 | 0x08
  { }
  access_kind access : 2;
  
};


struct variable_traits {
  constexpr explicit variable_traits(unsigned n = 0)
    : access       (detail::get_access(n)),  // 0x01 | 0x02
      linkage      (detail::get_linkage(n)), // 0x04 | 0x08
      storage      (detail::get_storage(n)), // 0x10 | 0x20
      is_constexpr (n & 0x40),
      is_defined   (n & 0x80),
      is_inline    (n & 0x0100)
  { }

  access_kind access : 2;
  linkage_kind linkage : 2;
  storage_kind storage : 2;
  unsigned is_constexpr : 1;
  unsigned is_defined : 1;
  unsigned is_inline : 1;

//DWR ADDN:
  template<class K>
  constexpr explicit variable_traits(K)
    : access       (std::is_same<K, kw::PublicT>::value
                    ? AK_public
                    : std::is_same<K, kw::PrivateT>::value
                      ? AK_private
                      : std::is_same<K, kw::ProtectedT>::value
                        ? AK_protected : AK_none),
      linkage      (std::is_same<K, kw::StaticT>::value
                    ? LK_internal
                    : std::is_same<K, kw::ExternT>::value
                      ? LK_external : LK_none),
      storage      (std::is_same<K, kw::ThreadLocalT>::value // this test must come
                    ? SK_thread_local                        // before linkage test
                    : linkage != LK_none //DWR is this right?
                      ? SK_static
                      : SK_none),
      is_constexpr (std::is_same<K, kw::ConstexprT>::value),
      is_defined   (0),
      is_inline    (std::is_same<K, kw::InlineT>::value)
  {
    static_assert(
        detail::first_matches_any_of_rest<K, kw::PublicT, kw::PrivateT, kw::ProtectedT,
            kw::StaticT, kw::ExternT, kw::ThreadLocalT, kw::ConstexprT,
            kw::InlineT>::value,
            "Invalid variable trait!");
  }

  constexpr explicit variable_traits(variable_traits A, variable_traits B)
          : access       (A.access != AK_none ? A.access : B.access),
            linkage      (A.linkage != LK_none ? A.linkage : B.linkage),
            storage      (A.storage != SK_none && B.storage != SK_thread_local
                          ? A.storage : B.storage),
            is_constexpr (A.is_constexpr || B.is_constexpr),
            is_defined   (A.is_defined || B.is_defined),
            is_inline    (A.is_inline || B.is_inline)
  {
    if (A.access != AK_none && B.access != AK_none && A.access != B.access)
      __compiler_error("Conflicting accesses specified");

    if (A.linkage != LK_none && B.linkage != LK_none && A.linkage != B.linkage)
      __compiler_error("Conflicting linkages specified");
  }
//END
};

struct field_traits {
  constexpr explicit field_traits(unsigned n = 0)
    : access       (detail::get_access(n)),  // 0x01 | 0x02
      is_mutable   (n & 0x04)
  { }

  access_kind access : 2;
  unsigned is_mutable : 1;

//DWR ADDN:
  template<class K>
  constexpr explicit field_traits(K)
    : access       (std::is_same<K, kw::PublicT>::value
                    ? AK_public
                    : std::is_same<K, kw::PrivateT>::value
                      ? AK_private
                      : std::is_same<K, kw::ProtectedT>::value
                        ? AK_protected : AK_none),
      is_mutable (std::is_same<K, kw::MutableT>::value)
  {
    static_assert(
        detail::first_matches_any_of_rest<K, kw::PublicT,
            kw::PrivateT, kw::ProtectedT, kw::MutableT>::value,
            "Invalid field qualifier!");
  }

  constexpr explicit field_traits(field_traits A, field_traits B)
          : access       (A.access != AK_none ? A.access : B.access),
            is_mutable    (A.is_mutable || B.is_mutable)
  {
    if (A.access != AK_none && B.access != AK_none && A.access != B.access)
      __compiler_error("Conflicting accesses specified");
  }
//END
};

//DWR ADDN:
struct function_type_traits {
  constexpr explicit function_type_traits(unsigned n = 0)
    : is_noexcept(n & 0x01),
      is_const   (n & 0x02)
  { }
  unsigned is_noexcept : 1;
  unsigned is_const : 1; //for methods only

  template<class K>
  constexpr explicit function_type_traits(K)
    : is_noexcept (std::is_same<K, kw::NoexceptT>::value),
      is_const    (std::is_same<K, kw::ConstT>::value)
  {
    static_assert(
        detail::first_matches_any_of_rest<K, kw::NoexceptT, kw::ConstT>::value,
          "Invalid function type qualifier!");
  }

  constexpr explicit function_type_traits(function_type_traits A, function_type_traits B)
          : is_noexcept(A.is_noexcept || B.is_noexcept),
            is_const   (A.is_const || B.is_const)
  { }
};
//END

enum function_def_kind : unsigned {
  FDK_undefined,
  FDK_defined_inline,
  FDK_defined_outofline,
  FDK_deleted,
};

namespace detail {
  // function definition_kind is stored in bits 5 and 6.
  static constexpr function_def_kind get_function_def_kind(unsigned n = 0) {
    return function_def_kind((n & 0x30) >> 4);
  }
}

struct function_traits {
  constexpr explicit function_traits(unsigned n = 0)
    : access       (detail::get_access(n)),    // 0x01 | 0x02
      linkage      (detail::get_linkage(n)), // 0x04 | 0x08
      definition_kind      (detail::get_function_def_kind(n)),  // 0x10 | 0x20
      is_constexpr (n & 0x40),
      is_noexcept  (n & 0x80)
  { }

  access_kind access : 2;
  linkage_kind linkage : 2;
  function_def_kind definition_kind : 2;
  unsigned is_constexpr : 1;
  unsigned is_noexcept : 1;

//DWR ADDN:
  template<class K>
  constexpr explicit function_traits(K)
    : access       (std::is_same<K, kw::PublicT>::value
                    ? AK_public
                    : std::is_same<K, kw::PrivateT>::value
                      ? AK_private
                      : std::is_same<K, kw::ProtectedT>::value
                        ? AK_protected : AK_none),
      linkage      (std::is_same<K, kw::StaticT>::value
                    ? LK_internal
                    : std::is_same<K, kw::ExternT>::value
                      ? LK_external : LK_none),
      definition_kind(std::is_same<K, kw::InlineT>::value
                      ? FDK_defined_inline
                      : std::is_same<K, kw::DeleteT>::value
                        ? FDK_deleted : FDK_undefined),
      is_constexpr (std::is_same<K, kw::ConstexprT>::value),
      is_noexcept  (std::is_same<K, kw::NoexceptT>::value)
  {
    static_assert(
            detail::first_matches_any_of_rest<K, kw::PublicT, kw::PrivateT, kw::ProtectedT,
                kw::StaticT, kw::ExternT, kw::ConstexprT, kw::NoexceptT,
                kw::InlineT, kw::DeleteT>::value,
            "Invalid function trait!");
  }

  constexpr explicit function_traits(function_traits A, function_traits B)
          : access       (A.access != AK_none ? A.access : B.access),
            linkage      (A.linkage != LK_none ? A.linkage : B.linkage),
            definition_kind(A.definition_kind != FDK_undefined
                            ? A.definition_kind : B.definition_kind),
            is_constexpr (A.is_constexpr || B.is_constexpr),
            is_noexcept  (A.is_noexcept || B.is_noexcept)
  {
    if (A.access != AK_none && B.access != AK_none
        && A.access != B.access)
      __compiler_error("Conflicting accesses specified");
    if (A.linkage != LK_none && B.linkage != LK_none
        && A.linkage != B.linkage)
      __compiler_error("Conflicting linkages specified");
    if (A.definition_kind != FDK_undefined && B.definition_kind != FDK_undefined
        && A.definition_kind != B.definition_kind)
      __compiler_error("Inline and Delete, or other conflicting definition_kinds,"
                       " specified");
  }
//END
};


// Methods

//Requires 2 bits
enum virtual_kind : unsigned {
  VK_none = 0,
  VK_virtual = 1, //DWR TODO determine if this only flags new virtuals, or is dependent on keyword
  VK_override = 2,
  VK_final = 3,
};

//Requires 4 bits:
enum method_kind : unsigned {
  MK_normal,
  MK_static,
  MK_dtor,
  MK_conv,
  MK_ctor_dflt,
  MK_ctor_copy,
  MK_ctor_move,
  MK_ctor_other,
  MK_assign_copy,
  MK_assign_move,
};

// Requires 3 bits:
enum method_def_kind : unsigned {
  MDK_undefined,
  MDK_defined_inline,
  MDK_defined_outofline,
  MDK_deleted,
  MDK_defaulted,
  MDK_pure_virtual,
};

namespace detail {
  // The virtual_kind of a bitfield is stored in bits 3 and 4:
  static constexpr virtual_kind get_virtual_kind(unsigned n = 0) {
    return virtual_kind((n & 0x0c) >> 2); //DWR TODO TEST
  }
  // The method_kind of a bitfield is stored in bits 5, 6, 7, and 8:
  static constexpr method_kind get_method_kind(unsigned n = 0) {
    return method_kind((n & 0xf0) >> 4); //DWR TODO TEST
  }
  // The method_def_kind of a bitfield is stored in bits 9, 10, and 11:
  static constexpr method_def_kind get_method_def_kind(unsigned n = 0) {
    return method_def_kind((n & 0x700) >> 8); //DWR TODO TEST
  }

} //detail

struct method_traits {
  constexpr explicit method_traits(unsigned n = 0)
    : access         (detail::get_access(n)),         // 0x01 | 0x02
      virtual_kind   (detail::get_virtual_kind(n)),   // 0x04 | 0x08
      kind           (detail::get_method_kind(n)),    // 0x10 | 0x20 | 0x40 | 0x80
      definition_kind(detail::get_method_def_kind(n)),// 0x100| 0x200| 0x400
      is_constexpr   (n & 0x800),
      is_explicit    (n & 0x1000),
      is_const       (n & 0x2000),
      is_noexcept    (n & 0x4000),
      is_trivial     (n & 0x8000)
  { }

  access_kind access : 2;
  virtual_kind virtual_kind : 2;
  method_kind kind : 4;
  method_def_kind definition_kind : 3;

  unsigned is_constexpr : 1;
  unsigned is_explicit : 1;
  unsigned is_const : 1;
  unsigned is_noexcept : 1;
  unsigned is_trivial : 1;

//DWR ADDN:
  template<class K>
  constexpr explicit method_traits(K k)
    : access       (std::is_same<K, kw::PublicT>::value
                    ? AK_public
                    : std::is_same<K, kw::PrivateT>::value
                      ? AK_private
                      : std::is_same<K, kw::ProtectedT>::value
                        ? AK_protected
                        : AK_none),
      virtual_kind (std::is_same<K, kw::FinalT>::value
                    ? VK_final
                    : std::is_same<K, kw::OverrideT>::value
                      ? VK_override
                      : std::is_same<K, kw::VirtualT>::value
                        ? VK_virtual
                        : VK_none),
      kind         (MK_normal),
      definition_kind(std::is_same<K, kw::DeleteT>::value
                      ? MDK_deleted
                      : std::is_same<K, kw::DefaultT>::value
                        ? MDK_defaulted
                        : std::is_same<K, kw::PureT>::value
                          ? MDK_pure_virtual
                          : MDK_undefined),
      is_constexpr  (std::is_same<K, kw::ConstexprT>::value),
      is_explicit   (std::is_same<K, kw::ExplicitT>::value),
      is_const      (std::is_same<K, kw::ConstT>::value),
      is_noexcept   (std::is_same<K, kw::NoexceptT>::value),
      is_trivial    (0)
  {
    static_assert(
        detail::first_matches_any_of_rest<K,
            kw::PublicT, kw::PrivateT, kw::ProtectedT,
            kw::ConstexprT, kw::ExplicitT, kw::VirtualT, kw::PureT,
            kw::FinalT, kw::OverrideT, kw::NoexceptT, kw::InlineT,
            kw::DeleteT, kw::DefaultT>::value,
        "Invalid method trait!");
  }

  //DWR TODO change all these two-pm constructors to be operator|
  constexpr explicit method_traits(method_traits A, method_traits B)
          : access       (A.access != AK_none ? A.access : B.access),
            virtual_kind ((unsigned)A.virtual_kind > (unsigned)B.virtual_kind
                          ? A.virtual_kind : B.virtual_kind),
            kind         (A.kind != MK_normal ? A.kind : B.kind),
            definition_kind(A.definition_kind != MDK_undefined
                            ? A.definition_kind : B.definition_kind),
            is_constexpr (A.is_constexpr || B.is_constexpr),
            is_explicit  (A.is_explicit || B.is_explicit),
            is_const     (A.is_const || B.is_const),
            is_noexcept  (A.is_noexcept || B.is_noexcept),
            is_trivial   (A.is_trivial || B.is_trivial)
  {
    if (A.access != AK_none && B.access != AK_none && A.access != B.access)
      __compiler_error("Conflicting accesses specified");
    if (A.kind != MK_normal && B.kind != MK_normal && A.kind != B.kind)
      __compiler_error("Conflicting method kinds!");
    if (A.definition_kind != MDK_undefined && B.definition_kind != MDK_undefined 
        && A.definition_kind != B.definition_kind)
      __compiler_error("Conflicting method definition_kinds!");
  }
//END

};


// Namespace

struct namespace_traits {
  constexpr explicit namespace_traits(unsigned n = 0)
    : is_inline    (n & 0x01)
  { }

  unsigned is_inline : 1;
};

// Classes

// TODO: Accumulate all known type traits for classes.
// DWR NOTE see note in SemaReflect.cpp in ClassTraits; there are a lot
// of possible traits, perhaps best to split them up so you only calulate
// and return advanced traits only when you want them...
//DWR TODO one key trait you need here: class/union(/struct); set up an enum.  Perhaps
// add in interface to fill it out.
struct class_traits {
  constexpr explicit class_traits(unsigned n = 0)
    : access        (detail::get_access(n)), // 0x01 | 0x02
      is_complete   (n & 0x04),
      is_polymorphic(n & 0x08),
      is_abstract   (n & 0x10),
      is_final      (n & 0x20),
      is_empty      (n & 0x40)
  { }

  access_kind access : 2;
  unsigned is_complete : 1;
  unsigned is_polymorphic : 1;
  unsigned is_abstract : 1;
  unsigned is_final : 1;
  unsigned is_empty : 1;

//DWR ADDN:
  template<class K>
  constexpr explicit class_traits(K)
    : access         (std::is_same<K, kw::PublicT>::value
                      ? AK_public
                      : std::is_same<K, kw::PrivateT>::value
                        ? AK_private
                        : std::is_same<K, kw::ProtectedT>::value
                          ? AK_protected : AK_none),
      is_complete    (0),
      is_polymorphic (0),
      is_abstract    (0),
      is_final       (std::is_same<K, kw::FinalT>::value),
      is_empty       (0)
  {
    static_assert(
        detail::first_matches_any_of_rest<K, kw::PublicT,
            kw::PrivateT, kw::ProtectedT, kw::FinalT>::value,
            "Invalid class qualifier!");
  }

  constexpr explicit class_traits(class_traits A, class_traits B)
          : access         (A.access != AK_none ? A.access : B.access),
            is_complete    (A.is_complete || B.is_complete),
            is_polymorphic (A.is_polymorphic || B.is_polymorphic),
            is_abstract    (A.is_abstract || B.is_abstract),
            is_final       (A.is_final || B.is_final),
            is_empty       (A.is_empty || B.is_empty)
  {
    if (A.access != AK_none && B.access != AK_none && A.access != B.access)
      __compiler_error("Conflicting accesses specified");
  }
//END
};

struct enum_traits {
  constexpr explicit enum_traits(unsigned n = 0)
    : access     (detail::get_access(n)), // 0x01 | 0x02
      is_scoped  (n & 0x04),
      is_complete(n & 0x08)
  { }

  access_kind access : 2;
  unsigned is_scoped : 1;
  unsigned is_complete : 1;

//DWR ADDN:
  template<class K>
  constexpr explicit enum_traits(K)
    : access         (std::is_same<K, kw::PublicT>::value
                      ? AK_public
                      : std::is_same<K, kw::PrivateT>::value
                        ? AK_private
                        : std::is_same<K, kw::ProtectedT>::value
                          ? AK_protected : AK_none),
      is_scoped      (std::is_same<K, kw::ClassT>::value),
      is_complete    (0)
  {
    static_assert(
        detail::first_matches_any_of_rest<K, kw::PublicT,
            kw::PrivateT, kw::ProtectedT, kw::ClassT>::value,
            "Invalid enum qualifier!");
  }

  constexpr explicit enum_traits(enum_traits A, enum_traits B)
          : access         (A.access != AK_none ? A.access : B.access),
            is_scoped      (A.is_scoped || B.is_scoped),
            is_complete    (A.is_complete || B.is_complete)
  {
    if (A.access != AK_none && B.access != AK_none && A.access != B.access)
      __compiler_error("Conflicting accesses specified");
  }
//END
};

struct base_spec_traits {
  constexpr explicit base_spec_traits(unsigned n = 0)
    : access(detail::get_access(n)),   // 0x01 | 0x02
      is_virtual(n & 0x04)
  { }
  access_kind access : 2;
  unsigned is_virtual : 1;

//DWR ADDN:
  template<class K>
  constexpr explicit base_spec_traits(K)
    : access       (std::is_same<K, kw::PublicT>::value
                    ? AK_public
                    : std::is_same<K, kw::PrivateT>::value
                      ? AK_private
                      : std::is_same<K, kw::ProtectedT>::value
                        ? AK_protected : AK_none),
      is_virtual   (std::is_same<K, kw::VirtualT>::value)
  {
    static_assert(
        detail::first_matches_any_of_rest<K, kw::PublicT,
            kw::PrivateT, kw::ProtectedT, kw::VirtualT>::value,
            "Invalid base spec. qualifier!");
  }

  constexpr explicit base_spec_traits(base_spec_traits A, base_spec_traits B)
          : access       (A.access != AK_none ? A.access : B.access),
            is_virtual   (A.is_virtual || B.is_virtual)
  {
    if (A.access != AK_none && B.access != AK_none && A.access != B.access)
      __compiler_error("Conflicting accesses specified");
  }
//END
};

//DWR ADDN:
struct qualtype_traits {
  constexpr explicit qualtype_traits(unsigned n = 0)
    : is_const    (n & 0x01),
      is_volatile (n & 0x02),
      is_restrict (n & 0x04)
  { }
  unsigned is_const : 1;
  unsigned is_volatile : 1;
  unsigned is_restrict : 1;

  template<class K>
  constexpr explicit qualtype_traits(K)
    : is_const    (std::is_same<K, kw::ConstT>::value),
      is_volatile (std::is_same<K, kw::VolatileT>::value),
      is_restrict (std::is_same<K, kw::RestrictT>::value)
  {
    static_assert(
            detail::first_matches_any_of_rest<K, kw::ConstT, kw::VolatileT, kw::RestrictT>::value,
            "Invalid type qualifier!");
  }

  constexpr explicit qualtype_traits(qualtype_traits A, qualtype_traits B)
          : is_const    (A.is_const || B.is_const),
            is_volatile (A.is_volatile || B.is_volatile),
            is_restrict (A.is_restrict || B.is_restrict)
  { }
};
//END


} // inline namespace v2
} // namespace meta
} // namespace cppx

#endif // CPPX_TRAITS_H
