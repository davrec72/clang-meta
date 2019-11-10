//NB these are just placeholders

#define STMT(DERIVED, BASE) \
template<> struct GetReflectionObjKind<DERIVED>\
{ static constexpr ReflectionObjKind value = RK_clang__ ## DERIVED; };

#include "clang/AST/StmtNodes.inc"

#define TYPE(DERIVED, BASE) \
template<> struct GetReflectionObjKind<DERIVED ## Type>\
{ static constexpr ReflectionObjKind value = RK_clang__ ## DERIVED ## Type; };

#include "clang/AST/TypeNodes.def"

#define DECL(DERIVED, BASE) \
template<> struct GetReflectionObjKind<DERIVED ## Decl>\
{ static constexpr ReflectionObjKind value = RK_clang__ ## DERIVED ## Decl; };

#include "clang/AST/DeclNodes.inc"

template<> struct GetReflectionObjKind<Stmt>
{ static constexpr ReflectionObjKind value = RK_clang__Stmt; };
template<> struct GetReflectionObjKind<Type>
{ static constexpr ReflectionObjKind value = RK_clang__Type; };
template<> struct GetReflectionObjKind<Decl>
{ static constexpr ReflectionObjKind value = RK_clang__Decl; };
template<> struct GetReflectionObjKind<DeclContext>
{ static constexpr ReflectionObjKind value = RK_clang__DeclContext; };
