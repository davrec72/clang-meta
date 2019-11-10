//
// Created by David Rector on 2019-04-05.
//

#ifndef UNTITLED4_TYPES_H
#define UNTITLED4_TYPES_H

#include <cppx/reflection_base.h>
#include <cppx/traits.h>
#include <cppx/reflected_tuple.hpp>

namespace cppx {
  namespace meta {
    inline namespace v2 {

      namespace detail {

        /// A helper macro for naming the underlying type-accessor functions
        /// for different types accurately.
        /*-------------------------------------------------------------------*/
        #define GET_UTYPE_FCNS(X, MP_utypename)                               \
          static constexpr auto MP_utypename(bool PreserveAliases = false) {  \
            return __reflect_type(X, PreserveAliases);                        \
          }                                                                   \
          static constexpr const char * MP_utypename ## _as_string() {        \
            return __reflect_type_name(X);                                    \
          }                                                                   \
        /*-------------------------------------------------------------------*/
        #define GET_SECONDARYTYPE_FCNS(X, MP_stypename)                       \
          static constexpr auto MP_stypename(bool PreserveAliases = false) {  \
            return __reflect_secondary_type(X, PreserveAliases);              \
          }                                                                   \
          static constexpr const char * MP_stypename ## _as_string() {        \
            return __reflect_secondary_type_name(X);                          \
          }                                                                   \
        /*-------------------------------------------------------------------*/

      } //detail

      //DWR TODO: set up a types_of_nameable_decls.h that has QualType and class/typename/template<...> class etc.
      //Then put the rest in types_of_qualtypes.h


      //DWR: test whether the QualType equality tests, and bool operators,
      // indeed work as expected (e.g. test fetching the dflt_qtype of a
      // TemplateTypeParmDecl with no default, see if the QualType is indeed empty.
      //
      /// A wrapper class that combines a base_type with qualifiers.
      template<reflection_t X>
      struct QualType :  detail::reflection_base<X> {
        static constexpr const char * kind() {
          return "QualType";
        }
        static constexpr auto traits() {
          return qualtype_traits(__reflect_traits(X));
        }
        GET_UTYPE_FCNS(X, base_type)

        //I'm not sure this will ever be necessary, but just in case
        // this should work:
        constexpr operator bool() {
          return base_type();
        }
      };
      template<reflection_t X>
      struct EmptyQualType :  detail::reflection_base<X> {
        constexpr operator bool() {
          return false;
        }
      };

      namespace Type {
        /// The many different kinds of type reflections.
        enum Kind {
          Auto,
          Builtin,
          Complex,
          UnresolvedUsing,
          Typedef,
          TypeOfExpr,
          TypeOf,
          Decltype,
          Reflected,
          UnaryTransform,
          Record,
          Enum,
          Elaborated,
          TemplateTypeParm,
          SubstTemplateTypeParmPack,
          DeducedTemplateSpecialization,
          TemplateSpecialization,
          InjectedClassName,
          DependentName,
          DependentTemplateSpecialization,
          ObjCObject,
          ObjCTypeParam,
          ObjCInterface,
          ObjCObjectPointer,
          Atomic,
          Pipe,
          ConstantArray,
          IncompleteArray,
          VariableArray,
          DependentSizedArray,
          Adjusted,
          Decayed,
          Pointer,
          BlockPointer,
          LValueReference,
          RValueReference,
          MemberPointer,
          DependentAddressSpace,
          DependentVector,
          DependentSizedExtVector,
          Vector,
          ExtVector,
          FunctionProto,
          FunctionNoProto,
          Paren,
          Attributed,
          PackExpansion,
          SubstTemplateTypeParm,
        };
      }




      static constexpr const char * to_string(Type::Kind K) {

        switch (K) {
#define PP_STRINGIZE(A) #A
#define RETURN_STR(MP_typekind)\
          case Type::MP_typekind:\
            return PP_STRINGIZE(MP_typekind ## Type);
/**/
          RETURN_STR(Auto)
          RETURN_STR(Builtin)
          RETURN_STR(Complex)
          RETURN_STR(UnresolvedUsing)
          RETURN_STR(Typedef)
          RETURN_STR(TypeOfExpr)
          RETURN_STR(TypeOf)
          RETURN_STR(Decltype)
          RETURN_STR(Reflected)
          RETURN_STR(UnaryTransform)
          RETURN_STR(Record)
          RETURN_STR(Enum)
          RETURN_STR(Elaborated)
          RETURN_STR(TemplateTypeParm)
          RETURN_STR(SubstTemplateTypeParmPack)
          RETURN_STR(DeducedTemplateSpecialization)
          RETURN_STR(TemplateSpecialization)
          RETURN_STR(InjectedClassName)
          RETURN_STR(DependentName)
          RETURN_STR(DependentTemplateSpecialization)
          RETURN_STR(ObjCObject)
          RETURN_STR(ObjCTypeParam)
          RETURN_STR(ObjCInterface)
          RETURN_STR(ObjCObjectPointer)
          RETURN_STR(Atomic)
          RETURN_STR(Pipe)
          RETURN_STR(ConstantArray)
          RETURN_STR(IncompleteArray)
          RETURN_STR(VariableArray)
          RETURN_STR(DependentSizedArray)
          RETURN_STR(Adjusted)
          RETURN_STR(Decayed)
          RETURN_STR(Pointer)
          RETURN_STR(BlockPointer)
          RETURN_STR(LValueReference)
          RETURN_STR(RValueReference)
          RETURN_STR(MemberPointer)
          RETURN_STR(DependentAddressSpace)
          RETURN_STR(DependentVector)
          RETURN_STR(DependentSizedExtVector)
          RETURN_STR(Vector)
          RETURN_STR(ExtVector)
          RETURN_STR(FunctionProto)
          RETURN_STR(FunctionNoProto)
          RETURN_STR(Paren)
          RETURN_STR(Attributed)
          RETURN_STR(PackExpansion)
          RETURN_STR(SubstTemplateTypeParm)
#undef RETURN_STR
#undef PP_STRINGIZE
        } //switch
      } //to_string(K)

      namespace detail {
        template<Type::Kind K>
        struct type_discrim {
          static constexpr Type::Kind kind() {
            return K;
          }
          static constexpr const char * kindstr() {
            return to_string(K);
          }

          //DWR TODO: develop more bool functions that group the kinds
          // in useful ways
        };

        template<reflection_t X, Type::Kind K>
        struct type : detail::reflection_base<X>, detail::type_discrim<K>
        {};

      } //detail



#define TYPE_REFL_DEF(MP_typekind)\
      template<reflection_t X>\
      struct MP_typekind ## Type : detail::type<X, Type::MP_typekind>\
/**/
    TYPE_REFL_DEF(Auto) {};
    TYPE_REFL_DEF(Builtin) {};
    TYPE_REFL_DEF(Complex) {};
    TYPE_REFL_DEF(UnresolvedUsing) {};
    TYPE_REFL_DEF(Typedef) {};
    TYPE_REFL_DEF(TypeOfExpr) {};
    TYPE_REFL_DEF(TypeOf) {};
    TYPE_REFL_DEF(Decltype) {};
    TYPE_REFL_DEF(Reflected) {};
    TYPE_REFL_DEF(UnaryTransform) {};
    TYPE_REFL_DEF(Record) {};
    TYPE_REFL_DEF(Enum) {};
    TYPE_REFL_DEF(Elaborated) {};
    TYPE_REFL_DEF(TemplateTypeParm) {};
    TYPE_REFL_DEF(SubstTemplateTypeParmPack) {};
    TYPE_REFL_DEF(DeducedTemplateSpecialization) {};
    TYPE_REFL_DEF(TemplateSpecialization) {};
    TYPE_REFL_DEF(InjectedClassName) {};
    TYPE_REFL_DEF(DependentName) {};
    TYPE_REFL_DEF(DependentTemplateSpecialization) {};
    TYPE_REFL_DEF(ObjCObject) {};
    TYPE_REFL_DEF(ObjCTypeParam) {};
    TYPE_REFL_DEF(ObjCInterface) {};
    TYPE_REFL_DEF(ObjCObjectPointer) {};
    TYPE_REFL_DEF(Atomic) {};
    TYPE_REFL_DEF(Pipe) {};
    TYPE_REFL_DEF(ConstantArray) {};
    TYPE_REFL_DEF(IncompleteArray) {};
    TYPE_REFL_DEF(VariableArray) {};
    TYPE_REFL_DEF(DependentSizedArray) {};
    TYPE_REFL_DEF(Adjusted) {};
    TYPE_REFL_DEF(Decayed) {};
    TYPE_REFL_DEF(Pointer) {};
    TYPE_REFL_DEF(BlockPointer) {};
    TYPE_REFL_DEF(LValueReference) {};
    TYPE_REFL_DEF(RValueReference) {};
    TYPE_REFL_DEF(MemberPointer) {};
    TYPE_REFL_DEF(DependentAddressSpace) {};
    TYPE_REFL_DEF(DependentVector) {};
    TYPE_REFL_DEF(DependentSizedExtVector) {};
    TYPE_REFL_DEF(Vector) {};
    TYPE_REFL_DEF(ExtVector) {};
    TYPE_REFL_DEF(FunctionProto) {};
    TYPE_REFL_DEF(FunctionNoProto) {};
    TYPE_REFL_DEF(Paren) {};
    TYPE_REFL_DEF(Attributed) {};
    TYPE_REFL_DEF(PackExpansion) {};
    TYPE_REFL_DEF(SubstTemplateTypeParm) {};
#undef TYPE_REFL_DEF






//
//
//      namespace detail {
//
//        template<type_reflection_kind K>
//        struct type_discrim {
//          static constexpr type_reflection_kind kind() {
//            return K;
//          }
//          static constexpr bool is_reference() {
//            return kind() == lvalue_reference_type_kind
//                || kind() == rvalue_reference_type_kind;
//          }
//          static constexpr bool is_pointer() {
//            return kind() == pointer_type_kind;
//          }
//          //DWR TODO is_... functions for each type kind
//          // --and things like:
//          // is_reference() {return is_lvalue_reference() || is_rvalue_reference();}
//        };
//
//        template<reflection_t X, type_reflection_kind K>
//        struct type : reflection_base<X>
//                    , type_discrim<K> {
//        };
//
//      } //detail
//
//      template<reflection_t X>
//      struct BuiltinType {
//        static constexpr const char * kindstr() {
//          return "BuiltinType";
//        }
//      };
//
//
//
//
//
//      template<reflection_t X>
//      struct primitive_type : detail::type<X, primitive_type_kind> {
//      };
//
//      template<reflection_t X>
//      struct alias_type : detail::type<X, alias_type_kind> {
//        GET_UTYPE_FCNS(X, aliased_qtype)
//      };
//
//      namespace detail {
//        template<reflection_t X>
//        struct has_base_qtype {
//          GET_UTYPE_FCNS(X, base_qtype)
//        };
//      }
//
//      template<reflection_t X>
//      struct lvalue_reference_type : detail::type<X, lvalue_reference_type_kind>
//                                   , detail::has_base_qtype<X> {
//      };
//
//      template<reflection_t X>
//      struct rvalue_reference_type : detail::type<X, rvalue_reference_type_kind>
//                                   , detail::has_base_qtype<X> {
//      };
//
//      template<reflection_t X>
//      struct pointer_type : detail::type<X, pointer_type_kind>
//                          , detail::has_base_qtype<X> {
//      };
//
//      template<reflection_t X>
//      struct member_pointer_type : pointer_type<X> {
//        GET_SECONDARYTYPE_FCNS(X, owner_class)
//      };
//
//
//      template<reflection_t X>
//      struct array_type : detail::type<X, array_type_kind> {
//        GET_UTYPE_FCNS(X, element_qtype)
//        static constexpr bool has_constant_size() {
//          return false;
//        }
//        /// When an array size is dependent/variable,
//        /// this should return the size expression as
//        /// a string; when unspecified,
//        /// it should return an empty string; when it is
//        /// constant size, this will return the array size
//        /// as a string.
//        static constexpr const char * array_size_as_string() {
//          return __reflect_value_expr_str(X);
//        }
//      };
//      template<reflection_t X>
//      struct constant_array_type : array_type<X> {
//        static constexpr bool has_constant_size() {
//          return true;
//        }
//        static constexpr size_t array_size() {
//          return __reflect_value(X);
//        }
//      };
//
//      template<reflection_t X>
//      class function_type : public detail::type<X, function_type_kind> {
//        struct parm_info {
//          static constexpr std::size_t size() {
//            return __reflect_num_parameters(X);
//          }
//          template<std::size_t I>
//          static constexpr auto get(bool PreserveAliases = false) {
//            return __reflect_parameter(X, I, PreserveAliases);
//          }
//        };
//        static constexpr function_type_traits traits() {
//          return function_type_traits(__reflect_traits(X));
//        }
//      public:
//        GET_SECONDARYTYPE_FCNS(X, return_type)
//
//        static constexpr reflected_tuple<parm_info> parameters() {
//          return {};
//        }
//        static constexpr bool is_noexcept() {
//          return traits().is_noexcept;
//        }
//        /// Only applicable to methods (i.e. member pointer types
//        /// whose base_qtype().base_type() is a function_type
//        static constexpr bool is_const() {
//          return traits().is_const;
//        }
//      };
//
//      /// A "tag" is a class/struct/union/enum.
//      template<reflection_t X>
//      struct tag_type : detail::type<X, tag_type_kind> {
//        static constexpr auto get_definition() {
//          return __reflect_definition(X);
//        }
//      };


    } //v2
  } //meta
} //cppx

#endif //UNTITLED4_TYPES_H
