//
// Created by David Rector on 2019-04-05.
//

#ifndef UNTITLED4_DECLS_H
#define UNTITLED4_DECLS_H


#include <cppx/reflection_base.h>
#include <cppx/reflected_tuple.hpp>
#include <cppx/traits.h>
#include "generate.h"


namespace cppx {
  namespace meta {
    inline namespace v2 {







//      namespace Decl {
//        enum Kind {
//          TranslationUnit,
//          Namespace,
//          NamespaceAlias,
//          Typedef,
//          TypeAlias,
//          Enum,
//          Record,
//          CXXRecord,
//          ClassTemplateSpecialization,
//          ClassTemplatePartialSpecialization,
//          VarTemplateSpecialization,
//          VarTemplatePartialSpecialization,
//          Function,
//          CXXDeductionGuide,
//          CXXMethod,
//          CXXConstructor,
//          CXXDestructor,
//          CXXConversion,
//          UsingShadow, //internal
//          ConstructorUsingShadow, //internal
//          Var,
//          FunctionTemplate,
//          ClassTemplate,
//          VarTemplate,
//          TypeAliasTemplate,
//          Empty,
//          UsingDirective,
//          Label,
//          UnresolvedUsingTypename,
//          TemplateTypeParm,
//          EnumConstant,
//          UnresolvedUsingValue,
//          IndirectField, //internal
//          Field,
//          NonTypeTemplateParm,
//          TemplateTemplateParm,
//          Using,
//          UsingPack, //internal
//          Constexpr, //internal
//          LinkageSpec,
//          Export,
//          MSProperty,
//          ObjCProtocol,
//          ObjCInterface,
//          ObjCIvar,
//          ObjCAtDefsField,
//          ObjCMethod,
//          ObjCCategory,
//          ObjCCategoryImpl,
//          ObjCImplementation,
//          ObjCProperty,
//          ObjCCompatibleAlias,
//          ObjCPropertyImpl,
//          ObjCTypeParam,
//          Import, //Obj C
//          OMPThreadPrivate,
//          OMPCapturedExpr,
//          OMPDeclareReduction,
//          PragmaComment,
//          PragmaDetectMismatch,
//          FileScopeAsm,
//          AccessSpec,
//          Friend,
//          FriendTemplate,
//          StaticAssert,
//          Block, //Apple extension
//          Captured,
//          ClassScopeFunctionSpecialization, //MS extension
//          BuiltinTemplate, //internal
//          Decomposition,
//          Binding,
//          ImplicitParam, //internal
//          ParmVar,
//        };
//      }
//
//
//      static constexpr const char * to_string(Decl::Kind K) {
//        switch(K) {
//#define PP_STRINGIZE(x) #x
//#define RETURN_STR(MP_declkind)\
//          case Decl::MP_declkind:\
//            return PP_STRINGIZE(MP_declkind ## Decl);
///**/
//          RETURN_STR(TranslationUnit)
//          RETURN_STR(Namespace)
//          RETURN_STR(NamespaceAlias)
//          RETURN_STR(Typedef)
//          RETURN_STR(TypeAlias)
//          RETURN_STR(Enum)
//          RETURN_STR(Record)
//          RETURN_STR(CXXRecord)
//          RETURN_STR(ClassTemplateSpecialization)
//          RETURN_STR(ClassTemplatePartialSpecialization)
//          RETURN_STR(VarTemplateSpecialization)
//          RETURN_STR(VarTemplatePartialSpecialization)
//          RETURN_STR(Function)
//          RETURN_STR(CXXDeductionGuide)
//          RETURN_STR(CXXMethod)
//          RETURN_STR(CXXConstructor)
//          RETURN_STR(CXXDestructor)
//          RETURN_STR(CXXConversion)
//          RETURN_STR(UsingShadow)
//          RETURN_STR(ConstructorUsingShadow)
//          RETURN_STR(Var)
//          RETURN_STR(FunctionTemplate)
//          RETURN_STR(ClassTemplate)
//          RETURN_STR(VarTemplate)
//          RETURN_STR(TypeAliasTemplate)
//          RETURN_STR(Empty)
//          RETURN_STR(UsingDirective)
//          RETURN_STR(Label)
//          RETURN_STR(UnresolvedUsingTypename)
//          RETURN_STR(TemplateTypeParm)
//          RETURN_STR(EnumConstant)
//          RETURN_STR(UnresolvedUsingValue)
//          RETURN_STR(IndirectField)
//          RETURN_STR(Field)
//          RETURN_STR(NonTypeTemplateParm)
//          RETURN_STR(TemplateTemplateParm)
//          RETURN_STR(Using)
//          RETURN_STR(UsingPack)
//          RETURN_STR(Constexpr)
//          RETURN_STR(LinkageSpec)
//          RETURN_STR(Export)
//          RETURN_STR(MSProperty)
//          RETURN_STR(ObjCProtocol)
//          RETURN_STR(ObjCInterface)
//          RETURN_STR(ObjCIvar)
//          RETURN_STR(ObjCAtDefsField)
//          RETURN_STR(ObjCMethod)
//          RETURN_STR(ObjCCategory)
//          RETURN_STR(ObjCCategoryImpl)
//          RETURN_STR(ObjCImplementation)
//          RETURN_STR(ObjCProperty)
//          RETURN_STR(ObjCCompatibleAlias)
//          RETURN_STR(ObjCPropertyImpl)
//          RETURN_STR(ObjCTypeParam)
//          RETURN_STR(Import)
//          RETURN_STR(OMPThreadPrivate)
//          RETURN_STR(OMPCapturedExpr)
//          RETURN_STR(OMPDeclareReduction)
//          RETURN_STR(PragmaComment)
//          RETURN_STR(PragmaDetectMismatch)
//          RETURN_STR(FileScopeAsm)
//          RETURN_STR(AccessSpec)
//          RETURN_STR(Friend)
//          RETURN_STR(FriendTemplate)
//          RETURN_STR(StaticAssert)
//          RETURN_STR(Block)
//          RETURN_STR(Captured)
//          RETURN_STR(ClassScopeFunctionSpecialization)
//          RETURN_STR(BuiltinTemplate)
//          RETURN_STR(Decomposition)
//          RETURN_STR(Binding)
//          RETURN_STR(ImplicitParam)
//          RETURN_STR(ParmVar)
//#undef RETURN_STR
//#undef PP_STRINGIZE
//        } //switch
//      } //to_string(K)
//
//      namespace detail {
//        template<Decl::Kind K>
//        struct decl_discrim {
//          static constexpr Decl::Kind kind() {
//            return K;
//          }
//
//          static constexpr bool is_visible() {
//            switch(K) {
//              case Decl::UsingShadow:
//              case Decl::ConstructorUsingShadow:
//              case Decl::IndirectField:
//              case Decl::UsingPack:
//              case Decl::Constexpr:
//              case Decl::ImplicitParam:
//                return false;
//              default:
//                return true;
//            }
//          }
//          //DWR TODO: develop more bool functions that group the kinds
//          // in useful ways
//        };
//      } //detail
//
//
//      /// Decl as a combination of other decls.
//      /// Note that we cannot use virtual inheritance, so we must
//      /// redefine base functions like the bool conversion operator
//      /// to avoid conflicts.  Plus, we can never use static_cast etc.
//      /// to convert to a base decl: instead, use the cast and dyn_cast
//      /// functions defined in reflection_base<X>, which avoid doing any
//      /// actual casting.
//      template<reflection_t X, template<reflection_t> class... DECLs>
//      struct decl : DECLs<X>... {
//        /// To avoid ambiguous conversions
//        constexpr operator bool() {
//          return X;
//        }
//      };
//      // Decl with no mixins, i.e. initial building block decl
//      template<reflection_t X>
//      struct decl<X> : detail::reflection_base<X> {
//      };
//
//
//      // DECL BASE NODES
//
//
//#define DECL_BASE_DEF(MP_declbasename, ...)\
//      template<reflection_t X>\
//      struct MP_declbasename : decl<X COMMA_VA_ARGS(__VA_ARGS__)>
///**/
//
//      /// A helper base for entities with member decls.
//      DECL_BASE_DEF(DeclWMembers) {
//      private:
//        struct member_info {
//          static constexpr std::size_t size() {
//            return __reflect_num_members(X);
//          }
//
//          template<std::size_t I>
//          static constexpr auto get() {
//            return __reflect_member(X, I);
//          }
//        };
//        using member_tuple = reflected_tuple<member_info>;
//      public:
//        static constexpr member_tuple members() {
//          return {};
//        }
//      };
//
//      DECL_BASE_DEF(NameableDecl) {
//        static constexpr const char *name() {
//          return __reflect_unqualified_name(X);
//        }
//        static constexpr bool is_anonymous() {
//          return *name() == '\0'; //DWR TODO TEST
//        }
//        //DWR TODO comments explaining difference, usage of these:
//        static constexpr auto declaration_context() {
//          return __reflect_declaration_context(X);
//        }
//        static constexpr auto lexical_context() {
//          return __reflect_lexical_context(X);
//        }
//        /// Everything that is a NameableDecl has a "type", though
//        /// the nature of this type will be depend on the
//        /// reflection kind.  For Vars/ParmVars,
//        /// it will return a QualType reflection; for
//        /// Functions/EnumConstants it will return a non-qualified Type
//        /// (a FunctionType or an EnumType); for classes/
//        /// templates it will return a meta-type.
//        // DWR TODO need to properly implement ReflectType overloads
//        static constexpr auto type(bool PreserveAliases = false) {
//          return __reflect_type(X, PreserveAliases);
//        }
//      };
//
//      DECL_BASE_DEF(NameableDeclWMembers, NameableDecl, DeclWMembers) {};
//
//      DECL_BASE_DEF(NameableDeclWInitExpr, NameableDecl) {
//        static constexpr auto init_expr() {
//          return __reflect_value_expr(X);
//          //^ returns an EmptyExpr<0> reflection if none
//        }
//      };
//      DECL_BASE_DEF(VarDeclBase, NameableDeclWInitExpr) {
//        DEF_TRAITS(variable)
//      }
//      DECL_BASE_DEF(NameableDeclWDfltExpr, NameableDecl) {
//        static constexpr auto dflt_expr() {
//          return __reflect_value_expr(X);
//          //^ returns an EmptyExpr<0> reflection if none
//        }
//      };
//
//      DECL_BASE_DEF(TemplateDecl) {
//      private:
//        struct template_parameter_info {
//          static constexpr std::size_t size() {
//            return __reflect_num_template_parameters(X);
//          }
//          template<std::size_t I>
//          static constexpr auto get() {
//            return __reflect_template_parameter(X, I);
//          }
//        };
//        using template_parameter_tuple = reflected_tuple<template_parameter_info>;
//      public:
//        static constexpr template_parameter_tuple template_parameters() {
//          return {};
//        }
//      };
//      ///
//      DECL_BASE_DEF(SpecializationDecl) {
//      private:
//        struct template_argument_info {
//          static constexpr std::size_t size() {
//            return __reflect_num_template_arguments(X);
//          }
//          template<std::size_t I>
//          static constexpr auto get() {
//            return __reflect_template_argument(X, I);
//          }
//        };
//        using template_argument_tuple = reflected_tuple<template_argument_info>;
//      public:
//        static constexpr template_argument_tuple template_arguments() {
//          return {};
//        }
//      };
//      DECL_BASE_DEF(PartialSpecializationDecl, TemplateDecl, SpecializationDecl) {};
//
//
//      DECL_BASE_DEF(ClassDeclBase) {
//      private:
//        struct bases_info {
//          static constexpr std::size_t size() {
//            return __reflect_num_bases(X);
//          }
//          template<std::size_t I>
//          static constexpr auto get() {
//            return __reflect_base(X, I);
//          }
//        };
//        using bases_tuple = reflected_tuple<bases_info>;
//      public:
//        static constexpr bases_tuple bases() {
//          return {};
//        }
//        DEF_TRAITS(class)
//      };
//
//      DECL_BASE_DEF(FuncOrMethodDecl, SpecializationDecl) {
//      private:
//        struct parameter_info {
//          static constexpr std::size_t size() {
//            return __reflect_num_parameters(X);
//          }
//          template<std::size_t I>
//          static constexpr auto get() {
//            return __reflect_parameter(X, I);
//          }
//        };
//        using parameter_tuple = reflected_tuple<parameter_info>;
//      public:
//        static constexpr bool is_specialization() {
//          return SpecializationDecl<X>::template_arguments().size();
//        }
//        static constexpr parameter_tuple parameters() {
//          return {};
//        }
//        static constexpr auto return_type() {
//          return __reflect_return(X);
//        }
//      };
//
//      DECL_BASE_DEF(FunctionDeclBase, FuncOrMethodDecl) {
//        DEF_TRAITS(function)
//      };
//
//      DECL_BASE_DEF(MethodDeclBase, FuncOrMethodDecl) {
//        DEF_TRAITS(method)
//      };
//
//      DECL_BASE_DEF(TypeAliasDeclBase, NameableDecl) {
//        static constexpr auto aliased_qtype() {
//          return __reflect_value(X);
//        }
//      };
//
//      DECL_BASE_DEF(FriendDeclBase) {
//        /// This may return a CXXRecordDecl/ClassTemplate/etc. reflection,
//        /// or a FunctionDecl/FunctionTemplateDecl/etc. reflection, etc.
//        /// For silly-but-permissable friend decls like "friend int",
//        /// for which the friend type is not convertible to a named decl,
//        /// this will return an EmptyDecl<0>.
//        static constexpr auto friend_decl() {
//          return __reflect_value(X);
//        }
//      };
//
//      // DECL LEAF NODES
//
//      /// Decl leaf nodes have a Decl::Kind
//      template<reflection_t X, Decl::Kind K, template<reflection_t> class... MIXINs>
//      struct declleaf final : decl<X, MIXINs...>, detail::decl_discrim<K> {
//      };
//
//#define DECL_LEAF_DEF(MP_declkind, ...)\
//      template<reflection_t X>\
//      struct MP_declkind ## Decl \
//          : declleaf<X, Decl::MP_declkind COMMA_VA_ARGS(__VA_ARGS__)>\
///**/
//      DECL_LEAF_DEF(Empty) {};
//      DECL_LEAF_DEF(TranslationUnit,
//              DeclWMembers) {};
//      DECL_LEAF_DEF(Namespace,
//              NameableDeclWMembers) {
//        DEF_TRAITS(namespace)
//      };
//      DECL_LEAF_DEF(Enum,
//              NameableDeclWMembers) {};
//      DECL_LEAF_DEF(EnumConstant,
//              NameableDeclWInitExpr) {
//        static constexpr auto value() {
//          return __reflect_value(X);
//        }
//      };
//      DECL_LEAF_DEF(Var,
//              VarDeclBase) {};
//      DECL_LEAF_DEF(VarTemplate,
//              VarDeclBase, TemplateDecl) {};
//      DECL_LEAF_DEF(VarTemplateSpecialization,
//              VarDeclBase, SpecializationDecl) {};
//      DECL_LEAF_DEF(VarTemplatePartialSpecialization,
//              VarDeclBase, PartialSpecializationDecl) {};
//      DECL_LEAF_DEF(Field,
//              NameableDeclWInitExpr) {
//        DEF_TRAITS(field)
//      };
//      DECL_LEAF_DEF(ParmVar,
//              NameableDeclWDfltExpr) {};
//      DECL_LEAF_DEF(NonTypeTemplateParm,
//              NameableDeclWDfltExpr) {};
//      DECL_LEAF_DEF(TemplateTemplateParm,
//              NameableDecl) {
//        static constexpr auto dflt_template_decl() {
//          return __reflect_value(X);
//          //^ returns an EmptyDecl<0> if none
//        }
//      };
//      DECL_LEAF_DEF(TemplateTypeParm,
//              NameableDecl) {
//        static constexpr auto dflt_qtype() { //returns a QualType
//          return __reflect_value(X);
//          //^ returns an EmptyType<0> reflection if none
//        }
//      };
//      DECL_LEAF_DEF(Function,
//              FunctionDeclBase) {};
//      DECL_LEAF_DEF(FunctionTemplate,
//              FunctionDeclBase, TemplateDecl) {};
//      DECL_LEAF_DEF(CXXMethod,
//              MethodDeclBase) {};
//      DECL_LEAF_DEF(CXXConstructor,
//              MethodDeclBase) {};
//      DECL_LEAF_DEF(CXXDestructor,
//              MethodDeclBase) {};
//      DECL_LEAF_DEF(CXXConversion,
//              MethodDeclBase) {};
//      DECL_LEAF_DEF(CXXRecord,
//              ClassDeclBase) {};
//      DECL_LEAF_DEF(ClassTemplate,
//              ClassDeclBase, TemplateDecl) {};
//      DECL_LEAF_DEF(ClassTemplateSpecialization,
//              ClassDeclBase, SpecializationDecl) {};
//      DECL_LEAF_DEF(ClassTemplatePartialSpecialization,
//              ClassDeclBase, PartialSpecializationDecl) {};
//      DECL_LEAF_DEF(AccessSpec) {
//        DEF_TRAITS(access_spec)
//      };
//      DECL_LEAF_DEF(Typedef,
//              TypeAliasDeclBase) {};
//      DECL_LEAF_DEF(TypeAlias,
//              TypeAliasDeclBase) {};
//      DECL_LEAF_DEF(TypeAliasTemplate,
//              TypeAliasDeclBase, TemplateDecl) {};
//      DECL_LEAF_DEF(NamespaceAlias,
//              NameableDecl) {
//        static constexpr auto aliased_namespace_decl() {
//          return __reflect_value(X);
//        }
//      };
//      DECL_LEAF_DEF(Friend,
//              FriendDeclBase) {};
//      DECL_LEAF_DEF(FriendTemplate,
//              FriendDeclBase) {};
//      DECL_LEAF_DEF(StaticAssert) {
//        static constexpr auto condition() {
//          return __reflect_value_expr(X);
//        }
//        static constexpr const char *msg() {
//          return __reflect_value(X);
//        }
//      };
///*
// Okay.  Re the Using decls: I think perhaps you should
// develop a reflection of a nested name specifier
// or something, and DeclarationNameInfo.
//
// OR, would be better if we could just come up with a way to use __reflect_member
// to get each specifier...think that through...
//
// Okay, here's how to do it
// --For both resolved and unresolved using decls, you want need
// a reflect type that gets the type just before the ::name;
// however note that this may be a DependentType.
// --Only in the non-dependent case can you
//
//
//    ---BY THE WAY, this applies to FriendDecls too -- the type may
//  be a dependent type, so you can't just cast to NamedDecl like you're
//  doing -- you really do need to reflect the type.  Probably
//  need multiple FriendDecls just like multiple Using Decls to account
//  for the different dependent status.
// --Anyways though, you have that type, and...
//
//
//
//
//
// */
//      DECL_LEAF_DEF(Using) {
////        static constexpr auto useddecl() {
////          return __reflect_value(X);
////        }
//      };
//      DECL_LEAF_DEF(UsingDirective) {
////        static constexpr auto used_namespace() {
////          return __reflect_value(X);
////        }
//      };
//      DECL_LEAF_DEF(UnresolvedUsingTypename) {
//
//      };
//      DECL_LEAF_DEF(UnresolvedUsingValue) {
//
//      };
//      DECL_LEAF_DEF(CXXDeductionGuide) {};
//      DECL_LEAF_DEF(LinkageSpec) {};
//      DECL_LEAF_DEF(FileScopeAsm) {};
//      DECL_LEAF_DEF(Label) {};
//      DECL_LEAF_DEF(Export) {};
//      DECL_LEAF_DEF(MSProperty) {};
//      DECL_LEAF_DEF(ObjCProtocol) {};
//      DECL_LEAF_DEF(ObjCInterface) {};
//      DECL_LEAF_DEF(ObjCIvar) {};
//      DECL_LEAF_DEF(ObjCAtDefsField) {};
//      DECL_LEAF_DEF(ObjCMethod) {};
//      DECL_LEAF_DEF(ObjCCategory) {};
//      DECL_LEAF_DEF(ObjCCategoryImpl) {};
//      DECL_LEAF_DEF(ObjCImplementation) {};
//      DECL_LEAF_DEF(ObjCProperty) {};
//      DECL_LEAF_DEF(ObjCCompatibleAlias) {};
//      DECL_LEAF_DEF(ObjCPropertyImpl) {};
//      DECL_LEAF_DEF(ObjCTypeParam) {};
//      DECL_LEAF_DEF(Import) {}; //Obj C
//      DECL_LEAF_DEF(OMPThreadPrivate) {};
//      DECL_LEAF_DEF(OMPCapturedExpr) {};
//      DECL_LEAF_DEF(OMPDeclareReduction) {};
//      DECL_LEAF_DEF(PragmaComment) {};
//      DECL_LEAF_DEF(PragmaDetectMismatch) {};
//      DECL_LEAF_DEF(Captured) {};
//      DECL_LEAF_DEF(Decomposition) {};
//      DECL_LEAF_DEF(Binding) {};
//      DECL_LEAF_DEF(Block) {}; //Apple extension
//      DECL_LEAF_DEF(ClassScopeFunctionSpecialization) {}; //MS extension
//      DECL_LEAF_DEF(BuiltinTemplate) {}; //internal
//      DECL_LEAF_DEF(UsingPack) {}; //internal
//      DECL_LEAF_DEF(UsingShadow) {}; //internal
//      DECL_LEAF_DEF(ConstructorUsingShadow) {}; //internal
//      DECL_LEAF_DEF(Constexpr) {}; //internal
//      DECL_LEAF_DEF(IndirectField) {}; //internal
//      DECL_LEAF_DEF(ImplicitParam) {}; //internal
//#undef DECL_LEAF_DEF
//
//
//
//
//
//
//
//
//
//
////      namespace detail {
////        /// The many different kinds of decl reflections.
////        enum decl_reflection_kind {
////          internal_decl_kind,
////          access_spec_decl_kind,
////          translation_unit_kind,
////          namespace_decl_kind,
////          variable_decl_kind,
////          function_decl_kind,
////          parameter_decl_kind,
////          member_variable_decl_kind,
////          member_function_decl_kind,
////          enumerator_decl_kind,
////          alias_decl_kind,
////          class_decl_kind,
////          union_decl_kind,
////          enum_decl_kind,
////        };
////
////        template<decl_reflection_kind K>
////        struct decl_discrim {
////          static constexpr decl_reflection_kind kind() {
////            return K;
////          }
////          //DWR TODO is_... functions for each decl kind
////        };
////
////        // The base class of all declaration reflections.
////        template<reflection_t X, decl_reflection_kind K>
////        struct decl : reflection_base<X>
////                    , decl_discrim<K> {
////        };
////
////      } //detail
////
////      // A reflection of an internal (i.e. non-observable) entity.
////      template<reflection_t X>
////      struct internal : detail::decl<X, internal_decl_kind> {
////      };
////
////      // A reflection of an access specifier within a class definition.
////      template<reflection_t X>
////      struct access_spec : detail::decl<X, access_spec_decl_kind> {
//
////      };
////
////      namespace detail {
////        /// A helper base for entities with member decls.
////        template<reflection_t X>
////        class has_members {
////          struct member_info {
////            static constexpr std::size_t size() {
////              return __reflect_num_members(X);
////            }
////
////            template<std::size_t I>
////            static constexpr auto get() {
////              return __reflect_member(X, I);
////            }
////          };
////
////          using member_tuple = member_info;
////        public:
////          static constexpr member_tuple members() {
////            return {};
////          }
////      } //detail
////
////      /// Reflects the current translation unit.
////      template<reflection_t X>
////      struct tu : detail::decl<X, translation_unit_kind>
////                , detail::has_members<X>
////      {};
////
////      namespace detail {
////        /// named<X,K>: A helper base for named decls.
////        template<reflection_t X, decl_reflection_kind K>
////        struct named : decl<X,K> {
////          static constexpr const char *name() {
////            return __reflect_unqualified_name(X);
////          }
////
////          static constexpr const char *qualified_name() {
////            return __reflect_qualified_name(X);
////          }
////
////          static constexpr auto declaration_context() {
////            return __reflect_declaration_context(X);
////          }
////
////          static constexpr auto lexical_context() {
////            return __reflect_lexical_context(X);
////          }
////        };
////
////        /// A helper base for non-translation unit
////        /// decls that are a scope for other members.
////        template<reflection_t X>
////        struct scope : has_members<X>
////        {};
////
////        /// A helper base for named decls that are a scope
////        /// for other decls.
////        template<reflection_t X, decl_reflection_kind K>
////        struct namedscope : named<X, K>
////                          , scope<X>
////        {};
////
////      } //detail
////
////
////      /// Reflects a namespace.
////      template<reflection_t X>
////      struct ns : detail::namedscope<X, namespace_decl_kind> {
////        static constexpr bool is_inline() {
////          return namespace_traits(__reflect_traits(X)).is_inline;
////        }
////      };
////
////      //DWR TODO anonymous namespace?
////
////      namespace detail {
////        // typed: an entity described by a type and an instance of it
////        template<reflection_t X, decl_reflection_kind K>
////        struct typed : named<X, K> {
////          static constexpr auto type() {
////            return __reflect_type(X);
////          }
////
////          //This one is no longer all that important...
////          static constexpr const char *type_name() {
////            return typed_name("");
////            //DWR FIXME?  Perhaps change __reflect_type_name to
////            // be specific to underlying types?
////          }
////
////          static constexpr const char *init_or_dflt_str() {
////            return __reflect_definition(X); //DWR TODO define this overload.
////          }
////
////        };
////
////      } //detail
////
////      template<reflection_t X, decl_reflection_kind K>
////      struct parameter : detail::typed<X,K> {
////        static constexpr void generate(bool wdflt = true) {
////          detail::typed<X,K>::generate(wdflt, 0);
////        }
////      };
////
////
////      namespace detail {
////
////        /*---------------------------------------------------*/
////#define DEF_X_PARAMETERS_BASE(MP_x)                           \
////        template<reflection_t X>                              \
////        class MP_x ## parameters_base  {                      \
////          struct parm_info {                                  \
////            static constexpr std::size_t size() {             \
////              return __reflect_num_ ## MP_x ## parameters(X); \
////            }                                                 \
////            template<std::size_t I>                           \
////            static constexpr auto get() {                     \
////              return __reflect_ ## MP_x ## parameter(X, I);   \
////            }                                                 \
////          };                                                  \
////        public:                                               \
////          static constexpr reflected_tuple<parm_info>         \
////          MP_x ## parameters() {                              \
////            return {};                                        \
////          }                                                   \
////          static constexpr bool                               \
////          has_ ## MP_x ## parameters() {                      \
////            return MP_x ## parameters().size();               \
////          }                                                   \
////        };                                                    \
////        /*---------------------------------------------------*/
////        //parameters_base  { parameters(); has_parameters(); }
////        DEF_X_PARAMETERS_BASE()
////
////        //template_parameters_base
////        // { template_parameters(); has_template_parameters(); }
////        DEF_X_PARAMETERS_BASE(template_)
////
////        //specialization_parameters_base
////        // { specialization_parameters(); has_specialization_parameters(); }
////        DEF_X_PARAMETERS_BASE(specialization_)
////
////#undef  DEF_X_PARAMETERS_BASE
////        /*---------------------------------------------------*/
////      } //detail
////
////
////      template<reflection_t X, decl_reflection_kind K>
////      struct function : detail::typed<X,K>
////                      , detail::parameters_base<X>
////                      , detail::template_parameters_base<X>
////                      , detail::specialization_parameters_base<X> {
////        static constexpr function_traits traits() {
////          return function_traits(__reflect_traits(X));
////        }
////        static constexpr auto return_type() {
////          return __reflect_secondary_type(X);
////        }
////        static constexpr auto return_name() {
////          return __reflect_secondary_type_name(X);
////        }
////        //DWR what is this?
////        static constexpr auto pointer() {
////          return __reflect_pointer(X);
////        }
////        static constexpr auto generate() {
////          if (has_template_parameters() || has_specialization_parameters())
////            gen::template_decl(template_parameters());
////          if (has_specialization_parameters())
////
////          gen::function_decl(traits(), return_name, name)
////        }
////      };
////
////      template<reflection_t X, decl_reflection_kind K>
////      struct variable : detail::typed<X,K>
////                      , detail::template_parameters_base<X> //for variable templates
////                      , detail::specialization_parameters_base<X> //""
////                              {
////        static constexpr variable_traits traits() {
////          return variable_traits(__reflect_traits(X));
////        }
////        //DWR Figure out exactly what this one is, add comments and example usages
////        static constexpr auto pointer() {
////          return __reflect_pointer(X);
////        }
////      };
////
////      //DWR RANDOM TODO: maybe you should set up a BuildQualTypeReflection, so that
////      // perhaps you could avoid needing the type_name stuff everywhere -- worth a try.
////
////
////
//////      function_decl_kind,
//////      member_variable_decl_kind,
//////      member_function_decl_kind,
//////      enumerator_decl_kind,
//////      alias_decl_kind,
//////      class_decl_kind,
//////      union_decl_kind,
//////      enum_decl_kind,


    } //v2
  } //meta
} //cppx

#endif //UNTITLED4_DECLS_H
