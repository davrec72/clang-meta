//===--- TypeTraits.h - C++ Type Traits Support Enumerations ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines enumerations for the type traits support.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_BASIC_TYPETRAITS_H
#define LLVM_CLANG_BASIC_TYPETRAITS_H

namespace clang {

  /// Names for traits that operate specifically on types.
  enum TypeTrait {
    UTT_HasNothrowAssign,
    UTT_HasNothrowMoveAssign,
    UTT_HasNothrowCopy,
    UTT_HasNothrowConstructor,
    UTT_HasTrivialAssign,
    UTT_HasTrivialMoveAssign,
    UTT_HasTrivialCopy,
    UTT_HasTrivialDefaultConstructor,
    UTT_HasTrivialMoveConstructor,
    UTT_HasTrivialDestructor,
    UTT_HasVirtualDestructor,
    UTT_IsAbstract,
    UTT_IsAggregate,
    UTT_IsArithmetic,
    UTT_IsArray,
    UTT_IsClass,
    UTT_IsCompleteType,
    UTT_IsCompound,
    UTT_IsConst,
    UTT_IsDestructible,
    UTT_IsEmpty,
    UTT_IsEnum,
    UTT_IsFinal,
    UTT_IsFloatingPoint,
    UTT_IsFunction,
    UTT_IsFundamental,
    UTT_IsIntegral,
    UTT_IsInterfaceClass,
    UTT_IsLiteral,
    UTT_IsLvalueReference,
    UTT_IsMemberFunctionPointer,
    UTT_IsMemberObjectPointer,
    UTT_IsMemberPointer,
    UTT_IsNothrowDestructible,
    UTT_IsObject,
    UTT_IsPOD,
    UTT_IsPointer,
    UTT_IsPolymorphic,
    UTT_IsReference,
    UTT_IsRvalueReference,
    UTT_IsScalar,
    UTT_IsSealed,
    UTT_IsSigned,
    UTT_IsStandardLayout,
    UTT_IsTrivial,
    UTT_IsTriviallyCopyable,
    UTT_IsTriviallyDestructible,
    UTT_IsUnion,
    UTT_IsUnsigned,
    UTT_IsVoid,
    UTT_IsVolatile,
    UTT_HasUniqueObjectRepresentations,
    UTT_Last = UTT_HasUniqueObjectRepresentations,
    BTT_IsBaseOf,
    BTT_IsConvertible,
    BTT_IsConvertibleTo,
    BTT_IsSame,
    BTT_TypeCompatible,
    BTT_IsAssignable,
    BTT_IsNothrowAssignable,
    BTT_IsTriviallyAssignable,
    BTT_ReferenceBindsToTemporary,
    BTT_Last = BTT_ReferenceBindsToTemporary,
    TT_IsConstructible,
    TT_IsNothrowConstructible,
    TT_IsTriviallyConstructible
  };

  /// Names for the array type traits.
  enum ArrayTypeTrait {
    ATT_ArrayRank,
    ATT_ArrayExtent
  };

  /// Names for the "expression or type" traits.
  enum UnaryExprOrTypeTrait {
    UETT_SizeOf,
    UETT_AlignOf,
    UETT_VecStep,
    UETT_OpenMPRequiredSimdAlign,
  };

////ASUTTON ADDN:
//  /// \brief Names for reflectors.
//  ///
//  /// Unary reflectors (prefixed by 'U') take a single argument, an expression
//  /// yielding a reflected node, while binary reflectors (prefixed by 'B') take
//  /// two arguments, both expressions. The first is the reflected node, and the
//  /// second is usually an integer value that indexes into an array.
//  enum ReflectionTrait {
//    BRT_ReflectProp, ///< __refect_prop(X,N) Reflects the Nth property of X [DWR ADDN]
////    BRT_ReflectDoc, ///< Reflects any user-added documentation for the Nth property of X
//    URT_ReflectPrint, ///< Emits debug info about a reflection.
//
////
////    URT_ReflectUnqualifiedName,
////    URT_ReflectQualifiedName,
////    URT_ReflectDeclarationContext,
////    URT_ReflectLexicalContext,
////    URT_ReflectTraits, ///< Computed properties of declarations.
////    URT_ReflectHasDefaultAccess, ///< True if an entity has default access.
////    URT_ReflectSpecifiers, ///< Written properties of declarations.
////
////    BRT_ReflectType,
////    URT_ReflectTypeName, //DWR ADDN (needed to produce minimally-necessary scope prefix)
////    URT_ReflectDecl, //DWR ADDN
////    URT_ReflectPointer, ///< For stored values.
////    URT_ReflectValue,
////    URT_ReflectValueExpr,
////    URT_ReflectAsString, //DWR ADDN
////
////    URT_ReflectNumParameters,
////    TRT_ReflectParameter,
////    URT_ReflectNumTemplateParameters, //DWR ADDN
////    BRT_ReflectTemplateParameter, //DWR ADDN
////    URT_ReflectNumTemplateArguments, //DWR ADDN
////    BRT_ReflectTemplateArgument, //DWR ADDN
////    BRT_ReflectSecondaryType,
////    URT_ReflectSecondaryTypeName, //DWR ADDN (needed to produce minimally-necessary scope prefix)
////    URT_ReflectNumMembers,
////    BRT_ReflectMember,
////    URT_ReflectNumBases,
////    BRT_ReflectBase,
//////    BRT_ModifyAccess,  ///< Second operand indicates access level. //ASUTTON ADDN
//////    BRT_ModifyVirtual, ///< Second operand is \c true to indicate pure virtual. //ASUTTON ADDN
//////    URT_ModifyConstexpr //ASUTTON ADDN
//  };
////END
}

#endif
