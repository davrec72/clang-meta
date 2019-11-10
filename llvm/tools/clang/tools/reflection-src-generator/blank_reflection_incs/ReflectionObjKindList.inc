//NB these are just placeholders

RK_none,

#define STMT(DERIVED, BASE) RK_clang__ ## DERIVED,
#include "clang/AST/StmtNodes.inc"

#define TYPE(DERIVED, BASE) RK_clang__ ## DERIVED ## Type,
#include "clang/AST/TypeNodes.def"

#define DECL(DERIVED, BASE) RK_clang__ ## DERIVED ## Decl,
#include "clang/AST/DeclNodes.inc"

RK_clang__Stmt,
RK_clang__Type,
RK_clang__Decl,
RK_clang__DeclContext,
