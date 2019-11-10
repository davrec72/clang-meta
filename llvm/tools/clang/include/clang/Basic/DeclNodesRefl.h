////  Created by David Rector on 4/17/19.
////  Copyright © 2019 David Rector. All rights reserved.
////
//// The data macro node definitions here should mostly parallel
//// the structure defined in DeclNodes.td, with the exception of
//// DeclContext -- see below.
////
//// The structure of each data macro is a 2-element tuple:
//// [0]  A 3-element tuple of node-specific data:
////      [0,0] A method name/info tuple sequence; this specifies
////            which methods of the existing methods in that type
////            the client should be able to access via reflection,
////            plus other useful info for the client about each
////            such method.
////      [0,1] A sequence of pseudobase activations (see below)
////      [0,2] 0 or 1 indicating whether this is a pseudochild node
////            (see below)
//// [1]  A sequence of all the derived node names,
////      without the Decl suffix.
////
//// We will use these macros together with DWR_PP_TREE_FOR_EACH
//// and some helper macros to generate the Reflection client
//// library header, plus enums for use by Clang to keep it
//// in sync with the client library (specifically, to keep the
//// method numbering system used by each in sync).
////
//// Any changes to these method sequences should manually be kept
//// in sync with the Reflector::Reflect(D, unsigned MemNum)
//// overload definitions in SemaReflect.cpp.
////
////
//// THE PROBLEM OF MULTIPLE-INHERITANCE, AND SOLUTION
//// VIA PSEUDOCHILDREN
////
//// In the Clang code, multiple inheritance arises from the need
//// for some Decl nodes to inherit from DeclContext.
////
//// The usual diamond inheritance problems are avoided by having DeclContext
//// (aka ContextDecl to us) NOT inherit from Decl. However this is an artificial
//// solution -- every DeclContext is a Decl after all -- and more
//// to the point, our client Reflection library would be difficult to implement
//// if ContextDecl did NOT inherit from a shared base with all the other
//// ___Decls, because all cast-checking is done via one common base.
////
//// E.g. consider:
//// Our declaration_context() reflection function
//// will return a ContextDecl to the user.  If this does NOT
//// inherit from Decl, then the user will have no way of dyn_casting to
//// e.g. TagDecl or TranslationUnitDecl or otherwise testing the dynamic
//// type of the ContextDecl to tweak behavior for different kinds of Contexts
//// -- NamespaceDecls, CXXRecordDecls, TranslationUnitDecls etc.  (Virtual
//// functions are not allowed in a constexpr context, meaning we
//// absolutely need to be able to query dynamic type info of ANY static type.)
////
//// Virtual inheritance of the common base would be the usual solution -- but
//// it is not allowed in a constexpr context.
////
//// We could consider non-virtual multiple inheritance, inheriting the common base
//// multiple times and manually resolving ambiguity, but even if this could
//// be done perfectly, the common base has data -- meaning that would involve
//// duplicating our data, costing us efficiency.
////
//// Our solution is to implement an artificial single-inheritance
//// hierarchy wherever we have multiple branch nodes that could
//// be inherited alongside one another, and manually disable/reenable
//// methods and casts to mimic the true desired hierarchy.
////
//// In more detail:
////
//// Call a collection of competing sibling nodes that might
//// be inherited alongside one another a "household".
//// We form an arbitrary ordering over any such household so that
//// the "youngest is a a "pseudochild" of the next-"oldest" sibling,
//// which may in turn may be a pseudochild of the next-oldest sibling
//// etc; the eldest sibling's parent is the traditional parent, and
//// is not considered a pseudochild.
////
//// We specify such an ordering by specifying a 1 in the [0,2] elem
//// for each pseudochild; this reversibly hides the parent's methods,
//// and reversibly disables casting to the parent.
//// (Note though that we have no current means of disabling SLICING
//// to the parent, so this solution isn't perfect.)
////
//// Then, you may re-activate the methods/casts to pseudo-ancestors
//// for a desc. node by simply adding the anc. names to its
//// pseudobase activation sequence in [0,1].  I.e. the [0,1] elem
//// is where you specify any additional bases beyond the first,
//// assuming you have set up pseudochild relations properly.
////
//// ---------------------------------------------------------------------
////     DIAMOND-INHERITANCE ==> SINGLE INHERITANCE
//// ---------------------------------------------------------------------
////
////          Decl           ==>          Decl
////        (/)   \          ==>         /
//// ContextDecl NamedDecl   ==>  ContextDecl -pseudoparent_of-> NamedDecl
////   |     \    /      |   ==>   |                             /   |
////   |    TagDecl      |   ==>   |                         TagDecl |
////   |                 |   ==>   |                          ^      |
//// TranslationUnitDecl |   ==>  TranslationUnitDecl         ^      |
////               LabelDecl ==>                              ^  LabelDecl
////                         ==>                              ^
////                         ==>     Pseudochild (NamedDecl) inherits from
////                         ==>      pseudoparent (ContextDecl) but hides
////                         ==>    its methods etc; TagDecl must manually
////                                   unhide them.  LabelDecl fine as is.
//// ---------------------------------------------------------------------
////
//
//#ifndef LLVM_DECLNODESREFL_H
//#define LLVM_DECLNODESREFL_H
//
//#define REFL_Decl \
//  ( ( /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (AccessSpec)(Friend)(FriendTemplate)\
//    (PragmaComment)(PragmaDetectMismatch)(StaticAssert)\
//    (FileScopeAsm)(ClassScopeFunctionSpecialization)\
//    (OMPThreadPrivate)(Empty)(Constexpr)(Import)(ObjCPropertyImpl)\
//    /*(CXXInjection) [ASUTTON]*/\
//    (Context) /*(Named) [DWR moved to ContextDecl's children -- see above]*/\
//  )
///**/
//#define REFL_Decl_m0  (Loc, getLocation/*name*/, SourceLocation/*ret type*/\
//  , ( Feed this to custom errors/warnings/fixits.  Same as getBeginLoc I presume...DWR TODO TEST ) /*documentation line seq*/\
//  )
//#define REFL_Decl_m1  (Loc, getBeginLoc/*name*/, SourceLocation/*ret type*/\
//  , ( Feed this to custom errors/warnings/fixits ) /*documentation line seq*/\
//  )
//#define REFL_Decl_m2  (Loc, getEndLoc/*name*/, SourceLocation/*ret type*/\
//  , ( Feed this to custom errors/warnings/fixits ) /*documentation line seq*/\
//  )
//#define REFL_Decl_m3  (Context, getDeclContext, DeclContext \
//  , ( @code )(\
//      namespace A { )(\
//        void f(); \/\/ DC == LexicalDC == 'namespace A' )(\
//      } )(\
//      void A::f(); \/\/ DC == namespace 'A', LexicalDC == global namespace )(\
//      @endcode )\
//  )
//#define REFL_Decl_m4  (Context, getLexicalDeclContext, DeclContext \
//  , ( @code )(\
//      namespace A { )(\
//        void f(); \/\/ DC == LexicalDC == 'namespace A' )(\
//      } )(\
//      void A::f(); \/\/ DC == namespace 'A', LexicalDC == global namespace )(\
//      @endcode )\
//  )
//#define REFL_Decl_m5  (Context, getNonClosureContext, Decl \
//  , ( Find the innermost non-closure ancestor of this declaration, )(\
//      walking up through blocks, lambdas, etc.  If that ancestor is )(\
//      not a code context (!isFunctionOrMethod()), returns null. )(\
//      A declaration may be its own non-closure context. )\
//  )
//#define REFL_Decl_m6  (Context, isInAnonymousNamespace, bool)
//#define REFL_Decl_m7  (Context, isInStdNamespace, bool)
//#define REFL_Decl_m8  (, getAccessUnsafe, AccessSpecifier \
//  , ( Retrieves the access specifier for this declaration, )(\
//      or AK_None if it has not been set. )\
//  )
//#define REFL_Decl_m9  (Attrs, hasAttrs, bool)
//#define REFL_Decl_m10  (Attrs, attr_begin, attr_iterator)
//#define REFL_Decl_m11  (Attrs, attr_end, attr_iterator)
//#define REFL_Decl_m12 (Attrs, getMaxAlignment, unsigned\
//  , ( Returns the maximum alignment specified by attributes )(\
//      on this decl, 0 if there are none. )\
//  )
//#define REFL_Decl_m13 (, isImplicit, bool\
//  , ( Indicates whether the declaration was implicitly )(\
//      generated by the compiler. If false, this declaration )(\
//      was written explicitly in the source code. )\
//  )
//#define REFL_Decl_m14 (Debug, isUsed, bool\
//  , ( Whether *any* (re-)declaration of the entity was used, )(\
//      meaning that a a definition is required. )(\
//      )(\
//      Note: any decl with the "used" attribute counts as used. )\
//  )
//#define REFL_Decl_m15 (Debug, isReferenced, bool\
//  , ( Whether any declaration of this entity was referenced. )\
//  )
//#define REFL_Decl_m16 (Debug, instantiationsWillNeedParsing, bool\
//  , ( True if this is a template with meta-programs whose output )(\
//      will need to be parsed at instantiation. )\
//  )
//#define REFL_Decl_m17 (ObjC, isTopLevelDeclInObjCContainer, bool\
//  , ( Whether this declaration is a top-level declaration (function, )(\
//      global variable, etc.) that is lexically inside an objc container )(\
//      definition. )\
//  )
//#define REFL_Decl_m18 (ObjC, getExternalSourceSymbolAttr, ExternalSourceSymbolAttr\
//  , ( Looks on this and related declarations for an applicable )(\
//      external source symbol attribute. )\
//  )
//#define REFL_Decl_m19 (Module, isModulePrivate, bool\
//  , ( Whether this declaration was marked as being private to the )(\
//      module in which it was defined. )\
//  )
//#define REFL_Decl_m20 (Module, isExported, bool\
//  , ( Whether this declaration is exported (by virtue of being lexically )(\
//      within an ExportDecl or by being a NamespaceDecl). )\
//  )
//#define REFL_Decl_m21 (Attrs, getDefiningAttr, Attr\
//  , ( Returns the attribute which acts as the definition of the entity, )(\
//      such as 'alias' or 'ifunc', if it exists; otherwise returns null. )\
//  )
//#define REFL_Decl_m22 (Attrs, getVersionIntroduced, VersionTuple\
//  , ( Retrieve the version of the target platform in which this )(\
//      declaration was introduced. )(\
//      )(\
//      \\returns An empty version tuple if this declaration has no 'introduced' )(\
//      availability attributes, or the version tuple that is specified in the )(\
//      attribute otherwise. )\
//  )
//#define REFL_Decl_m23 (Attrs, isDeprecated, bool\
//  , ( Determine whether this declaration is marked 'deprecated'. )\
//  )
//#define REFL_Decl_m24 (Attrs, isUnavailable, bool\
//  , ( Determine whether this declaration is marked 'unavailable'. )\
//  )
//#define REFL_Decl_m25 (Attrs, isWeakImported, bool\
//  , ( Determine whether this is a weak-imported symbol. )(\
//      Weak-imported symbols are typically marked with the )(\
//      'weak_import' attribute, but may also be marked with an )(\
//      'availability' attribute where we are targing a platform prior to )(\
//      the introduction of this feature. )\
//  )
//#define REFL_Decl_m26 (Debug, isFromASTFile, bool\
//  , ( Determine whether this declaration came from an AST file, such as )(\
//      a precompiled header or module, rather than having been parsed. )\
//  )
//#define REFL_Decl_m27 (Module, hasOwningModule, bool\
//  , ( Is this declaration owned by some module? )\
//  )
//#define REFL_Decl_m28 (Module, getOwningModule, Module\
//  , ( Get the module that owns this declaration (for visibility purposes). )(\
//      If isFromASTFile, this will return an imported owning module; if not, )(\
//      this will return the local owning module. )\
//  )
//#define REFL_Decl_m29 (Module, getOwningModuleForLinkage, Module\
//  , ( Get the module that owns this declaration for linkage purposes. )(\
//      There only ever is such a module under the C++ Modules TS. )\
//  )
//#define REFL_Decl_m30 (Module, isHidden, bool\
//  , ( Determine whether this declaration might be hidden from name )(\
//     lookup. Note that the declaration might be visible even if this returns )(\
//     \\c false, if the owning module is visible within the query context. )\
//  )
//#define REFL_Decl_m31 (Module, getModuleOwnershipKind, ModuleOwnershipKind\
//  , ( Get the kind of module ownership for this declaration. )\
//  )
//#define REFL_Decl_m32 (Context, isOutOfLine, bool\
//  , ( Determine whether this declaration is declared out of line )(\
//      (outside its semantic context). )\
//  )
//#define REFL_Decl_m33 (Context, isTemplated, bool\
//  , ( Determine whether this declaration is a templated entity )(\
//      (whether it is within the scope of a template parameter). )\
//  )
//#define REFL_Decl_m34 (Context, isLexicallyWithinFunctionOrMethod, bool\
//  , ( Returns true if this declaration lexically is inside a function. )(\
//      It recognizes non-defining declarations as well as members of local )(\
//      classes: )(\
//      @code )(\
//        void foo() { void bar(); } )(\
//        void foo2() { class ABC { void bar(); }; } )(\
//      @endcode )\
//  )
//#define REFL_Decl_m35 (Context, getParentFunctionOrMethod, DeclContext\
//  , ( If this decl is defined inside a function/method/block it returns )(\
//      the corresponding DeclContext, otherwise it returns null. )\
//  )
//#define REFL_Decl_m36 (Redecl, getCanonicalDecl, Decl\
//  , ( Retrieves the "canonical" declaration of the given declaration. (TODO example/clarify))\
//  )
//#define REFL_Decl_m37 (Redecl, isCanonicalDecl, bool\
//  , ( Whether this particular Decl is a canonical one. )\
//  )
//#define REFL_Decl_m38 (Redecl, redecls_begin, redecl_iterator\
//  , ( Returns the beginning of an iterator range for all the redeclarations of the same )(\
//      decl. This range will have at least one member (when this decl is the only one). )\
//  )
//#define REFL_Decl_m39 (Redecl, redecls_end, redecl_iterator\
//  , ( Returns the end of an iterator range for all the redeclarations of the same )(\
//      decl. This range will have at least one member (when this decl is the only one). )\
//  )
//#define REFL_Decl_m40 (Redecl, getPreviousDecl, Decl\
//  , ( Retrieve the previous declaration that declares the same entity )(\
//      as this declaration, or NULL if there is no previous declaration. )\
//  )
//#define REFL_Decl_m41 (Redecl, getMostRecentDecl, Decl\
//  , ( Retrieve the most recent declaration that declares the same entity )(\
//      as this declaration (which may be this declaration). )\
//  )
//#define REFL_Decl_m42 (, getBody, Stmt\
//  , ( If this Decl represents a declaration for a body of code, )(\
//      such as a function or method definition, this method returns the )(\
//      top-level Stmt* of that body.  Otherwise this method returns null. )\
//  )
//#define REFL_Decl_m43 (, hasBody, bool\
//  , ( Returns true if this \\c Decl represents a declaration for a body of )(\
//      code, such as a function or method definition. )(\
//      Note that \\c hasBody can also return true if any redeclaration of this )(\
//      \\c Decl represents a declaration for a body of code. )\
//  )
//#define REFL_Decl_m44 (Loc, getBodyRBrace, SourceLocation\
//  , ( Gets the right brace of the body, if a body exists. )(\
//      This works whether the body is a CompoundStmt or a CXXTryStmt. )\
//  )
//#define REFL_Decl_m45 (Debug, isTemplateParameter, bool)
//#define REFL_Decl_m46 (Debug, isTemplateParameterPack, bool)
//#define REFL_Decl_m47 (Debug, isParameterPack, bool)
//#define REFL_Decl_m48 (Debug, isTemplateDecl, bool)
//#define REFL_Decl_m49 (Debug, getDescribedTemplate, TemplateDecl\
//  , ( If this is a declaration that describes some template )(\
//      (e.g. the CXXRecordDecl returned by ClassTemplateDecl::getTemplatedDecl()), )(\
//      this method returns that template declaration. )\
//  )
//#define REFL_Decl_m50 (Debug, getAsFunction, FunctionDecl\
//  , ( Returns the function itself, or the templated function if this is a )(\
//      function template, or null if neither. )\
//  )
//#define REFL_Decl_m51 (Debug, isLocalExternDecl, bool\
//  , ( Determine whether this is a block-scope declaration with linkage. )(\
//      This will either be a local variable declaration declared 'extern', )(\
//      or a local function declaration. )\
//  )
//#define REFL_Decl_m52 (Debug, dump, void)
//#define REFL_Decl_m53 (Debug, getFunctionType, FunctionType\
//  , ( Looks through the Decl's underlying type to extract a FunctionType )(\
//      when possible. Will return null if the type underlying the Decl does not )(\
//      have a FunctionType. )\
//  )
//
//
//
//
//#define REFL_ContextDecl\
//  ( ( /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//, /*Children:*/\
//  (TranslationUnit)(LinkageSpec)(Export)\
//  (Block)(Captured)(ExternCContext)\
//  /*(CXXFragment) [ASUTTON]*/\
//  (Named)/*A pseudochild (marked as such in REFL_NamedDecl)*/\
//  )
//
//
///// Determines whether this context is dependent on a
///// template parameter.
//bool isDependentContext() const;
//
///// isTransparentContext - Determines whether this context is a
///// "transparent" context, meaning that the members declared in this
///// context are semantically declared in the nearest enclosing
///// non-transparent (opaque) context but are lexically declared in
///// this context. For example, consider the enumerators of an
///// enumeration type:
///// @code
///// enum E {
/////   Val1
///// };
///// @endcode
///// Here, E is a transparent context, so its enumerator (Val1) will
///// appear (semantically) that it is in the same context of E.
///// Examples of transparent contexts include: enumerations (except for
///// C++0x scoped enums), and C++ linkage specifications.
//bool isTransparentContext() const;
//
///// Retrieve the nearest enclosing C linkage specification context,
///// or null if none.
//const LinkageSpecDecl *getExternCContext() const;
//
///// Determines whether this context or some of its ancestors is a
///// linkage specification context that specifies C++ linkage.
//bool isExternCXXContext() const;
//
///// getRedeclContext - Retrieve the context in which an entity conflicts with
///// other entities of the same name, or where it is a redeclaration if the
///// two entities are compatible. This skips through transparent contexts.
//DeclContext *getRedeclContext();
//
///// Retrieve the nearest enclosing namespace context.
//DeclContext *getEnclosingNamespaceContext();
//
///// Retrieve the outermost lexically enclosing record context.
//RecordDecl *getOuterLexicalRecordContext();
//
///// decls_begin/decls_end - Iterate over the declarations stored in
///// this context.
//decl_iterator decls_begin() const;
//decl_iterator decls_end() const;
//
///// noload_decls_begin/end - Iterate over the declarations stored in this
///// context that are currently loaded; don't attempt to retrieve anything
///// from an external source.
//decl_iterator noload_decls_begin() const;
//decl_iterator noload_decls_end() const;
//
///// Whether this DeclContext has external storage containing
///// additional declarations that are lexically in this context.
//bool hasExternalLexicalStorage() const;
//
///// Whether this DeclContext has external storage containing
///// additional declarations that are visible in this context.
//bool hasExternalVisibleStorage() const;
//
//
//
//
//
//
//#define REFL_AccessSpecDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_FriendDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_FriendTemplateDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
///// Represents a `#pragma comment` line. Always a child of
///// TranslationUnitDecl.
//#define REFL_PragmaCommentDecl\
//  ( ( /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//PragmaMSCommentKind getCommentKind() const { return CommentKind; }
//StringRef getArg() const { return getTrailingObjects<char>(); }
//
//
///// Represents a `#pragma detect_mismatch` line. Always a child of
///// TranslationUnitDecl.
//#define REFL_PragmaDetectMismatchDecl\
//  ( ( /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//StringRef getName() const { return getTrailingObjects<char>(); }
//StringRef getValue() const { return getTrailingObjects<char>() + ValueStart; }
//
//
//#define REFL_StaticAssertDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_FileScopeAsmDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ClassScopeFunctionSpecializationDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_OMPThreadPrivateDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_EmptyDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ConstexprDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ImportDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ObjCPropertyImplDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//////ASUTTON ADDN:
////#define REFL_CXXInjectionDecl\
////  ( ( ((FirstMethodHere, ReturnTypeHere))\
////    , /*no pseudobase activations*/\
////    , 0/*not a pseudochild*/\
////    )\
////  , /*no children*/\
////  )
////  )
/////**/
//////END
//
///**/
//#define REFL_TranslationUnitDecl\
//  ( ( /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_TranslationUnitDecl_m0 (, getAnonymousNamespace, NamespaceDecl)
//
//
//
//
//
//
//
//#define REFL_LinkageSpecDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ExportDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_BlockDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_CapturedDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
///// Declaration context for names declared as extern "C" in C++. This
///// is neither the semantic nor lexical context for such declarations, but is
///// used to check for conflicts with other extern "C" declarations. Example:
/////
///// \code
/////   namespace N { extern "C" void f(); } // #1
/////   void N::f() {}                       // #2
/////   namespace M { extern "C" void f(); } // #3
///// \endcode
/////
///// The semantic context of #1 is namespace N and its lexical context is the
///// LinkageSpecDecl; the semantic context of #2 is namespace N and its lexical
///// context is the TU. However, both declarations are also visible in the
///// extern "C" context.
/////
///// The declaration at #3 finds it is a redeclaration of \c N::f through
///// lookup in the extern "C" context.
//#define REFL_ExternCContextDecl\
//  ( ( /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
////(No methods)
//
//
//
//////ASUTTON ADDN:
////#define REFL_CXXFragmentDecl\
////  ( ( /*no pseudobase activations*/\
////    , 0/*not a pseudochild*/\
////    )\
////  , /*no children*/\
////  )
////  )
/////**/
//////END
//
///**/
//#define REFL_NamedDecl\
//  ( ( /*no pseudobase activations*/\
//    , 1/*!!!IS a pseudochild (of ContextDecl):
//        * Means that anything that wants access to
//        * ContextDecl's methods must add Context
//        * to their pseudobase activations*/\
//    )\
//  , (NamedNonTypeContext)/*DWR ADDN: helper base*/\
//    (Type)/*NB This one has mixed context- and non-context
//           * derived nodes, to be handled later*/\
//    /*These are non-context Named decls:*/\
//    (UsingDirective)(NamespaceAlias)(Label)(Using)\
//    (UsingPack)(ObjCProperty)(ObjCCompatibleAlias)\
//    (Value)(Template)(UsingShadow)\
//  )
///**/
///// Get the name of identifier for this declaration as a StringRef.
/////
///// This requires that the declaration have a name and that it be a simple
///// identifier.
//StringRef getName() const;
///// Get the actual, stored name of the declaration, which may be a special
///// name.
//DeclarationName getDeclName() const { return Name; }
//
///// Determine whether this declaration is a C++ class member.
//bool isCXXClassMember() const;
//
///// Determine whether the given declaration is an instance member of
///// a C++ class.
//bool isCXXInstanceMember() const;
//
////LINKAGE/VISIBILITY:
//
///// Determine what kind of linkage this entity has.
/////
///// This is not the linkage as defined by the standard or the codegen notion
///// of linkage. It is just an implementation detail that is used to compute
///// those.
//Linkage getLinkageInternal() const;
//
///// Get the linkage from a semantic point of view. Entities in
///// anonymous namespaces are external (in c++98).
//Linkage getFormalLinkage() const {
//  return clang::getFormalLinkage(getLinkageInternal());
//}
//
///// True if this decl has external linkage.
//bool hasExternalFormalLinkage() const {
//  return isExternalFormalLinkage(getLinkageInternal());
//}
//
//bool isExternallyVisible() const {
//  return clang::isExternallyVisible(getLinkageInternal());
//}
//
///// Determine whether this declaration can be redeclared in a
///// different translation unit.
//bool isExternallyDeclarable() const {
//  return isExternallyVisible() && !getOwningModuleForLinkage();
//}
//
///// Determines the visibility of this entity.
//Visibility getVisibility() const {
//  return getLinkageAndVisibility().getVisibility();
//}
//
//// DEBUG:
///// True if the computed linkage is valid. Used for consistency
///// checking. Should always return true.
//bool isLinkageValid() const;
//
///// True if something has required us to compute the linkage
///// of this declaration.
/////
///// Language features which can retroactively change linkage (like a
///// typedef name for linkage purposes) may need to consider this,
///// but hopefully only in transitory ways during parsing.
//bool hasLinkageBeenComputed() const {
//  return hasCachedLinkage();
//}
//
///// Looks through UsingDecls and ObjCCompatibleAliasDecls for
///// the underlying named decl.
//NamedDecl *getUnderlyingDecl();
//
//
//
//
//
//
//
//
//
///*DWR ADDN:*/
//#define REFL_NamedNonTypeContextDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , (Context)/*!!!Pseudobase activation of ContextDecl*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (Namespace)(ObjCMethod)\
//    /*(Metaclass) [ASUTTON]*/\
//    (ObjCContainer)\
//  )
///**/
//#define REFL_NamespaceDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
///// Returns true if this is an anonymous namespace declaration.
/////
///// For example:
///// \code
/////   namespace {
/////     ...
/////   };
///// \endcode
///// q.v. C++ [namespace.unnamed]
//bool isAnonymousNamespace();
//
///// Returns true if this is an inline namespace declaration.
//bool isInline() const;
//
///// Get the original (first) namespace declaration.
//const NamespaceDecl *getOriginalNamespace() const;
//
///// Retrieve the anonymous namespace nested inside this namespace,
///// if any.
//NamespaceDecl *getAnonymousNamespace() const;
//
//
//
//
//#define REFL_UsingDirectiveDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_NamespaceAliasDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_LabelDecl\
//  ( ( /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//LabelStmt *getStmt() const;
//bool isGnuLocal() const;
//bool isMSAsmLabel() const;
//bool isResolvedMSAsmLabel() const;
//StringRef getMSAsmLabel() const
//
//
//
//#define REFL_UsingDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_UsingPackDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ObjCMethodDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ObjCPropertyDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ObjCCompatibleAliasDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//////ASUTTON ADDN:
////#define REFL_MetaclassDecl\
////  ( ( ((FirstMethodHere, ReturnTypeHere))\
////    , /*no pseudobase activations*/\
////    , 0/*not a pseudochild*/\
////    )\
////  , /*no children*/\
////  )
////  )
/////**/
//////END
//
//#define REFL_TypeDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (UnresolvedUsingTypename)(TemplateTypeParm)\
//    (TypedefName)/* <^ non-context type decls*/\
//    (Tag)/*Context base will be activated for this one*/\
//  )
///**/
//#define REFL_UnresolvedUsingTypenameDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_TemplateTypeParmDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
//#define REFL_TypedefNameDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (Typedef)(TypeAlias)(ObjCTypeParam)\
//  )
///**/
//#define REFL_TypedefDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_TypeAliasDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ObjCTypeParamDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
//#define REFL_TagDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , (Context)/*!!!Pseudobase activation of ContextDecl*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (Enum) /*(CXXGeneratedType)*/\
//    (Record)\
//  )
///**/
//#define REFL_EnumDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//////ASUTTON ADDN:
////#define REFL_CXXGeneratedTypeDecl\
////  ( ( ((FirstMethodHere, ReturnTypeHere))\
////    , /*no pseudobase activations*/\
////    , 0/*not a pseudochild*/\
////    )\
////  , /*no children*/\
////  )\
/////**/
//////END
//#define REFL_RecordDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (CXXRecord)\
//  )
///**/
//
//#define REFL_CXXRecordDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (ClassTemplateSpecialization)\
//  )
///**/
//
//#define REFL_ClassTemplateSpecializationDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (ClassTemplatePartialSpecialization)\
//  )
///**/
//#define REFL_ClassTemplatePartialSpecializationDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
//#define REFL_ValueDecl\
//  ( ( /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (EnumConstant)(UnresolvedUsingValue)(IndirectField)\
//    (Binding)(OMPDeclareReduction)\
//    (Declarator)\
//  )
///**/
//QualType getType() const;
///// Determine whether this symbol is weakly-imported,
/////        or declared with the weak or weak-ref attr.
//bool isWeak() const;
//
//
//
//
//#define REFL_EnumConstantDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_UnresolvedUsingValueDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_IndirectFieldDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_BindingDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_OMPDeclareReductionDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
///// Represents a ValueDecl that came out of a declarator.
///// Contains type source information through TypeSourceInfo.
//#define REFL_DeclaratorDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (MSProperty)(NonTypeTemplateParm)\
//    (Field)(Function)(Var)\
//  )
///**/
//TypeSourceInfo *getTypeSourceInfo() const;
///// Return start of source range ignoring outer template declarations.
//SourceLocation getInnerLocStart() const;
///// Return start of source range taking into account any outer template
///// declarations.
//SourceLocation getOuterLocStart() const;
///// Retrieve the nested-name-specifier that qualifies the name of this
///// declaration, if it was present in the source.
//NestedNameSpecifier *getQualifier();
///// Retrieve the nested-name-specifier (with source-location
///// information) that qualifies the name of this declaration, if it was
///// present in the source.
//NestedNameSpecifierLoc getQualifierLoc() const;
//
///// DWR: For iterating over template parameter lists
///// (there may be multiple for an out-of-line definition).
//templparamlist_iterator templparamlists_begin() const { return templparamlists().begin(); }
//templparamlist_iterator templparamlists_end() const { return templparamlists().end(); }
//
////DWR TODO figure out what exactly this gets
//SourceLocation getTypeSpecStartLoc() const;
//
//
//#define REFL_MSPropertyDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_NonTypeTemplateParmDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
//
//
///**/
//#define REFL_FieldDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (ObjCIvar)(ObjCAtDefsField)\
//  )
///**/
//
//
//
//
//#define REFL_ObjCIvarDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ObjCAtDefsFieldDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
///// Represents a function declaration or definition.
/////
///// Since a given function can be declared several times in a program,
///// there may be several FunctionDecls that correspond to that
///// function. Only one of those FunctionDecls will be found when
///// traversing the list of declarations in the context of the
///// FunctionDecl (e.g., the translation unit); this FunctionDecl
///// contains all of the information known about the function. Other,
///// previous declarations of the function are available via the
///// getPreviousDecl() chain.
//#define REFL_FunctionDecl\
//  ( ( /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (CXXDeductionGuide)\
//    (CXXMethod)\
//  )
///**/
//// DWR TODO: extract out the bitfield content of FunctionDecl, etc.
//
///// Returns whether the function has a trivial body that does not require any
///// specific codegen.
//bool hasTrivialBody() const;
//
//
///// Returns true if the function has a definition that does not need to be
///// instantiated.
//virtual bool isDefined() const;
//
///// Get the definition for this declaration.
//FunctionDecl *getDefinition();
//
////GROUPING: FUNCTIONDECL BITFIELD TRAITS
//
///// Returns whether this specific declaration of the function is also a
///// definition that does not contain uninstantiated body.
/////
///// This does not determine whether the function has been defined (e.g., in a
///// previous definition); for that information, use isDefined.
//bool isThisDeclarationADefinition() const {
//  return IsDeleted || IsDefaulted || Body || HasSkippedBody ||
//         IsLateTemplateParsed || WillHaveBody || hasDefiningAttr();
//}
//
///// Returns whether this specific declaration of the function has a body.
//bool doesThisDeclarationHaveABody() const {
//  return Body || IsLateTemplateParsed;
//}
//
///// Whether this function is marked as virtual explicitly.
//bool isVirtualAsWritten() const { return IsVirtualAsWritten; }
//
///// Whether this virtual function is pure, i.e. makes the containing class
///// abstract.
//bool isPure() const { return IsPure; }
//
///// Whether this templated function will be late parsed.
//bool isLateTemplateParsed() const { return IsLateTemplateParsed; }
//
///// Whether this function is "trivial" in some specialized C++ senses.
///// Can only be true for default constructors, copy constructors,
///// copy assignment operators, and destructors.  Not meaningful until
///// the class has been fully built by Sema.
//bool isTrivial() const { return IsTrivial; }
//
//bool isTrivialForCall() const { return IsTrivialForCall; }
//
///// Whether this function is defaulted per C++0x. Only valid for
///// special member functions.
//bool isDefaulted() const { return IsDefaulted; }
//
///// Whether this function is explicitly defaulted per C++0x. Only valid
///// for special member functions.
//bool isExplicitlyDefaulted() const { return IsExplicitlyDefaulted; }
//
///// Whether falling off this function implicitly returns null/zero.
///// If a more specific implicit return value is required, front-ends
///// should synthesize the appropriate return statements.
//bool hasImplicitReturnZero() const { return HasImplicitReturnZero; }
//
///// Whether this function has a prototype, either because one
///// was explicitly written or because it was "inherited" by merging
///// a declaration without a prototype with a declaration that has a
///// prototype.
//bool hasPrototype() const {
//  return HasWrittenPrototype || HasInheritedPrototype;
//}
//
//bool hasWrittenPrototype() const { return HasWrittenPrototype; }
//
///// Whether this function inherited its prototype from a
///// previous declaration.
//bool hasInheritedPrototype() const { return HasInheritedPrototype; }
//
///// Whether this is a (C++11) constexpr function or constexpr constructor.
//bool isConstexpr() const { return IsConstexpr; }
//
///// Whether the instantiation of this function is pending.
///// This bit is set when the decision to instantiate this function is made
///// and unset if and when the function body is created. That leaves out
///// cases where instantiation did not happen because the template definition
///// was not seen in this TU. This bit remains set in those cases, under the
///// assumption that the instantiation will happen in some other TU.
//bool instantiationIsPending() const { return InstantiationIsPending; }
//
///// Indicates the function uses __try.
//bool usesSEHTry() const { return UsesSEHTry; }
//
///// Whether this function has been deleted.
/////
///// A function that is "deleted" (via the C++0x "= delete" syntax)
///// acts like a normal function, except that it cannot actually be
///// called or have its address taken. Deleted functions are
///// typically used in C++ overload resolution to attract arguments
///// whose type or lvalue/rvalue-ness would permit the use of a
///// different overload that would behave incorrectly. For example,
///// one might use deleted functions to ban implicit conversion from
///// a floating-point number to an Integer type:
/////
///// @code
///// struct Integer {
/////   Integer(long); // construct from a long
/////   Integer(double) = delete; // no construction from float or double
/////   Integer(long double) = delete; // no construction from long double
///// };
///// @endcode
//// If a function is deleted, its first declaration must be.
//bool isDeleted() const { return getCanonicalDecl()->IsDeleted; }
//bool isDeletedAsWritten() const { return IsDeleted && !IsDefaulted; }
//
///// True if the function was a definition but its body was skipped.
//bool hasSkippedBody() const { return HasSkippedBody; }
//
///// True if this function will eventually have a body, once it's fully parsed.
//bool willHaveBody() const { return WillHaveBody; }
//
////ASUTTON ADDN:
///// \brief True if this function is a metaprogram.
//bool isMetaprogram() const { return IsMetaprogram; }
////END
//
////END GROUPING
//
///// Whether this function is variadic.
//bool isVariadic() const;
//
///// Determines whether this function is "main", which is the
///// entry point into an executable program.
//bool isMain() const;
//
///// Determines whether this function is a MSVCRT user defined entry
///// point.
//bool isMSVCRTEntryPoint() const;
//
///// Determines whether this operator new or delete is one
///// of the reserved global placement operators:
/////    void *operator new(size_t, void *);
/////    void *operator new[](size_t, void *);
/////    void operator delete(void *, void *);
/////    void operator delete[](void *, void *);
///// These functions have special behavior under [new.delete.placement]:
/////    These functions are reserved, a C++ program may not define
/////    functions that displace the versions in the Standard C++ library.
/////    The provisions of [basic.stc.dynamic] do not apply to these
/////    reserved placement forms of operator new and operator delete.
/////
///// This function must be an allocation or deallocation function.
//bool isReservedGlobalPlacementOperator() const;
//
///// Determines whether this function is one of the replaceable
///// global allocation functions:
/////    void *operator new(size_t);
/////    void *operator new(size_t, const std::nothrow_t &) noexcept;
/////    void *operator new[](size_t);
/////    void *operator new[](size_t, const std::nothrow_t &) noexcept;
/////    void operator delete(void *) noexcept;
/////    void operator delete(void *, std::size_t) noexcept;      [C++1y]
/////    void operator delete(void *, const std::nothrow_t &) noexcept;
/////    void operator delete[](void *) noexcept;
/////    void operator delete[](void *, std::size_t) noexcept;    [C++1y]
/////    void operator delete[](void *, const std::nothrow_t &) noexcept;
///// These functions have special behavior under C++1y [expr.new]:
/////    An implementation is allowed to omit a call to a replaceable global
/////    allocation function. [...]
/////
///// If this function is an aligned allocation/deallocation function, return
///// true through IsAligned.
//bool isReplaceableGlobalAllocationFunction(bool *IsAligned = nullptr) const;
//
///// Determine whether this is a destroying operator delete.
//bool isDestroyingOperatorDelete() const;
//
////DWR TODO the language linkage/storage class/inline stuff is also in
////VarDecl, would it be useful to have a common base class?
//
///// Compute the language linkage.
//LanguageLinkage getLanguageLinkage() const;
//
///// Determines whether this function is a function with
///// external, C linkage.
//bool isExternC() const;
//
///// Determines whether this function's context is, or is nested within,
///// a C++ extern "C" linkage spec.
//bool isInExternCContext() const;
//
///// Determines whether this function's context is, or is nested within,
///// a C++ extern "C++" linkage spec.
//bool isInExternCXXContext() const;
//
///// Returns the storage class as written in the source. For the
///// computed linkage of symbol, see getLinkage.
//StorageClass getStorageClass() const { return StorageClass(SClass); }
//
///// Determine whether the "inline" keyword was specified for this
///// function.
//bool isInlineSpecified() const { return IsInlineSpecified; }
//
///// Determine whether this function should be inlined, because it is
///// either marked "inline" or "constexpr" or is a member function of a class
///// that was defined in the class body.
//bool isInlined() const { return IsInline; }
//
//
//
///// Determines whether this is a global function.
//bool isGlobal() const;
//
///// Determines whether this function is known to be 'noreturn', through
///// an attribute on its declaration or its type.
//bool isNoReturn() const;
//
///// True if this function is considered a multiversioned function.
//bool isMultiVersion() const { return getCanonicalDecl()->IsMultiVersion; }
//
///// True if this function is a multiversioned dispatch function as a part of
///// the cpu_specific/cpu_dispatch functionality.
//bool isCPUDispatchMultiVersion() const;
///// True if this function is a multiversioned processor specific function as a
///// part of the cpu_specific/cpu_dispatch functionality.
//bool isCPUSpecificMultiVersion() const;
//
//unsigned getBuiltinID() const;
//
//// Iterator access to formal parameters.
//param_iterator param_begin() { return parameters().begin(); }
//param_iterator param_end() { return parameters().end(); }
//unsigned getNumParams() const;
//
///// Returns the minimum number of arguments needed to call this function. This
///// may be fewer than the number of function parameters, if some of the
///// parameters have default arguments (in C++).
//unsigned getMinRequiredArguments() const;
//
//QualType getReturnType() const;
//
///// Attempt to compute an informative source range covering the
///// function return type. This may omit qualifiers and other information with
///// limited representation in the AST.
//SourceRange getReturnTypeSourceRange() const;
//
///// Attempt to compute an informative source range covering the
///// function exception specification, if any.
//SourceRange getExceptionSpecSourceRange() const;
//
///// Determine the type of an expression that calls this function.
///// TODO clarify meaning.
//QualType getCallResultType() const;
//
///// Returns the WarnUnusedResultAttr that is either declared on this
///// function, or its return type declaration -- or nullptr if
///// this function does not have that attribute.
//const Attr *getUnusedResultAttr() const;
//
//bool isInlineDefinitionExternallyVisible() const;
//
//bool isMSExternInline() const;
//
//bool doesDeclarationForceExternallyVisibleDefinition() const;
//
///// Whether this function declaration represents an C++ overloaded
///// operator, e.g., "operator+".
//bool isOverloadedOperator() const {
//  return getOverloadedOperator() != OO_None;
//}
//
//OverloadedOperatorKind getOverloadedOperator() const;
//
///// The literal suffix identifier this function represents, if any.
///// TODO clarify
//const IdentifierInfo *getLiteralIdentifier() const;
//
///// If this function is an instantiation of a member function
///// of a class template specialization, retrieves the function from
///// which it was instantiated.
/////
///// This routine will return non-NULL for (non-templated) member
///// functions of class templates and for instantiations of function
///// templates. For example, given:
/////
///// \code
///// template<typename T>
///// struct X {
/////   void f(T);
///// };
///// \endcode
/////
///// The declaration for X<int>::f is a (non-templated) FunctionDecl
///// whose parent is the class template specialization X<int>. For
///// this declaration, getInstantiatedFromFunction() will return
///// the FunctionDecl X<T>::f. When a complete definition of
///// X<int>::f is required, it will be instantiated from the
///// declaration returned by getInstantiatedFromMemberFunction().
//FunctionDecl *getInstantiatedFromMemberFunction() const;
//
///// What kind of templated function this is.
//TemplatedKind getTemplatedKind() const;
//
///// If this function is an instantiation of a member function of a
///// class template specialization, retrieves the member specialization
///// information.
//MemberSpecializationInfo *getMemberSpecializationInfo() const;
//
///// Retrieves the function template that is described by this
///// function declaration.
/////
///// Every function template is represented as a FunctionTemplateDecl
///// and a FunctionDecl (or something derived from FunctionDecl). The
///// former contains template properties (such as the template
///// parameter lists) while the latter contains the actual
///// description of the template's
///// contents. FunctionTemplateDecl::getTemplatedDecl() retrieves the
///// FunctionDecl that describes the function template,
///// getDescribedFunctionTemplate() retrieves the
///// FunctionTemplateDecl from a FunctionDecl.
//FunctionTemplateDecl *getDescribedFunctionTemplate() const;
//
///// Retrieve the class scope template pattern that this function
/////  template specialization is instantiated from.
//FunctionDecl *getClassScopeSpecializationPattern() const;
//
///// If this function is actually a function template specialization,
///// retrieve information about this function template specialization.
///// Otherwise, returns NULL.
//FunctionTemplateSpecializationInfo *getTemplateSpecializationInfo() const;
//
///// Determines whether this function is a function template
///// specialization or a member of a class template specialization that can
///// be implicitly instantiated.
//bool isImplicitlyInstantiable() const;
//
///// Determines if the given function was instantiated from a
///// function template.
//bool isTemplateInstantiation() const;
//
///// Retrieve the function declaration from which this function could
///// be instantiated, if it is an instantiation (rather than a non-template
///// or a specialization, for example).
//FunctionDecl *getTemplateInstantiationPattern() const;
//
///// Retrieve the primary template that this function template
///// specialization either specializes or was instantiated from.
/////
///// If this function declaration is not a function template specialization,
///// returns NULL.
//FunctionTemplateDecl *getPrimaryTemplate() const;
//
///// Retrieve the template arguments used to produce this function
///// template specialization from the primary template.
/////
///// If this function declaration is not a function template specialization,
///// returns NULL.
//const TemplateArgumentList *getTemplateSpecializationArgs() const;
//
///// Retrieve the template argument list as written in the sources,
///// if any.
/////
///// If this function declaration is not a function template specialization
///// or if it had no explicit template argument list, returns NULL.
///// Note that it an explicit template argument list may be written empty,
///// e.g., template<> void foo<>(char* s);
//const ASTTemplateArgumentListInfo*
//getTemplateSpecializationArgsAsWritten() const;
//
//
//
///// Retrieve the (first) point of instantiation of a function template
///// specialization or a member of a class template specialization.
/////
///// \returns the first point of instantiation, if this function was
///// instantiated from a template; otherwise, returns an invalid source
///// location.
//SourceLocation getPointOfInstantiation() const;
//
///// Determine whether this is or was instantiated from an out-of-line
///// definition of a member function.
//bool isOutOfLine() const override;
//
///// Identify a memory copying or setting function.
///// If the given function is a memory copy or setting function, returns
///// the corresponding Builtin ID. If the function is not a memory function,
///// returns 0.
//unsigned getMemoryFunctionKind() const;
//
///// Returns ODRHash of the function.  This value is calculated and
///// stored on first call, then the stored value returned on the other calls.
//unsigned getODRHash();
//
///// Returns cached ODRHash of the function.  This must have been previously
///// computed and stored.
//unsigned getODRHash() const;
//
//
//
//
//
//
//
//
//
//#define REFL_CXXDeductionGuideDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
//#define REFL_CXXMethodDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (CXXConstructor)(CXXDestructor)(CXXConversion)\
//  )
///**/
//#define REFL_CXXConstructorDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_CXXDestructorDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_CXXConversionDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
//#define REFL_VarDecl\
//  ( ( /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (ImplicitParam)(ParmVar)(Decomposition)(OMPCapturedExpr)\
//    (VarTemplateSpecialization)\
//  )
///**/
///// Returns the storage class as written in the source. For the
///// computed linkage of symbol, see getLinkage.
//StorageClass getStorageClass() const;
//ThreadStorageClassSpecifier getTSCSpec() const;
//TLSKind getTLSKind() const;
///// Returns true if a variable with function scope is a non-static local
///// variable.  (If false, this variable has Global storage.)
//bool hasLocalStorage() const;
///// Returns true if a variable with function scope is a static local
///// variable.
//bool isStaticLocal() const;
///// Returns true if a variable has extern or __private_extern__
///// storage.
//bool hasExternalStorage() const;
//
///// Compute the language linkage.
//LanguageLinkage getLanguageLinkage() const;
//
///// Determines whether this variable is a variable with external, C linkage.
//bool isExternC() const;
//
///// Determines whether this variable's context is, or is nested within,
///// a C++ extern "C" linkage spec.
//bool isInExternCContext() const;
//
///// Determines whether this variable's context is, or is nested within,
///// a C++ extern "C++" linkage spec.
//bool isInExternCXXContext() const;
//
///// Returns true for local variable declarations other than parameters.
///// Note that this includes static variables inside of functions. It also
///// includes variables inside blocks.
/////
/////   void foo() { int x; static int y; extern int z; }
//bool isLocalVarDecl() const;
//
///// Similar to isLocalVarDecl but also includes parameters.
//bool isLocalVarDeclOrParm() const;
//
///// Similar to isLocalVarDecl, but excludes variables declared in blocks.
//bool isFunctionOrMethodVarDecl() const;
//
///// Determines whether this is a static data member.
/////
///// This will only be true in C++, and applies to, e.g., the
///// variable 'x' in:
///// \code
///// struct S {
/////   static int x;
///// };
///// \endcode
//bool isStaticDataMember() const;
//
///// Check whether this declaration is a definition. If this could be
///// a tentative definition (in C), don't check whether there's an overriding
///// definition.
//DefinitionKind isThisDeclarationADefinition();
//
///// Check whether this variable is defined in this translation unit.
//DefinitionKind hasDefinition();
//
///// Get the tentative definition that acts as the real definition in a TU.
///// Returns null if there is a proper definition available.
//VarDecl *getActingDefinition();
//
///// Get the real (not just tentative) definition for this declaration.
//VarDecl *getDefinition();
//
///// Returns true for file scoped variable declaration.
//bool isFileVarDecl() const;
//
///// Get the initializer for this variable, no matter which
///// declaration it is attached to.
//const Expr *getAnyInitializer();
//
///// DWR: presumably this one will return null if the current decl
///// doens't have the initializer, even if another one does.
//const Expr *getInit() const;
//
///// The style of initialization for this declaration.
/////
///// C-style initialization is "int x = 1;". Call-style initialization is
///// a C++98 direct-initializer, e.g. "int x(1);". The Init expression will be
///// the expression inside the parens or a "ClassType(a,b,c)" class constructor
///// expression for class types. List-style initialization is C++11 syntax,
///// e.g. "int x{1};". Clients can distinguish between different forms of
///// initialization by checking this value. In particular, "int x = {1};" is
///// C-style, "int x({1})" is call-style, and "int x{1};" is list-style; the
///// Init expression in all three cases is an InitListExpr.
//InitializationStyle getInitStyle() const;
//
///// Whether the initializer is a direct-initializer (list or call).
//bool isDirectInit() const;
//
//
//// GROUPING: NonParmVarDeclBits stuff (by grouping it there, you could optimize
//// by constructing with the bitfield).
//
///// If this definition should pretend to be a declaration.
/////
///// In some cases (mostly module merging) we can end up with two visible
///// definitions one of which needs to be demoted to a declaration to keep
///// the AST invariants.
//bool isThisDeclarationADemotedDefinition() const {
//  return isa<ParmVarDecl>(this) ? false :
//         NonParmVarDeclBits.IsThisDeclarationADemotedDefinition;
//}
//
///// Determine whether this variable is the exception variable in a
///// C++ catch statememt or an Objective-C \@catch statement.
//bool isExceptionVariable() const {
//  return isa<ParmVarDecl>(this) ? false : NonParmVarDeclBits.ExceptionVar;
//}
//
///// Determine whether this local variable can be used with the named
///// return value optimization (NRVO).
/////
///// The named return value optimization (NRVO) works by marking certain
///// non-volatile local variables of class type as NRVO objects. These
///// locals can be allocated within the return slot of their containing
///// function, in which case there is no need to copy the object to the
///// return slot when returning from the function. Within the function body,
///// each return that returns the NRVO object will have this variable as its
///// NRVO candidate.
//bool isNRVOVariable() const {
//  return isa<ParmVarDecl>(this) ? false : NonParmVarDeclBits.NRVOVariable;
//}
//
///// Determine whether this variable is the for-range-declaration in
///// a C++0x for-range statement.
//bool isCXXForRangeDecl() const {
//  return isa<ParmVarDecl>(this) ? false : NonParmVarDeclBits.CXXForRangeDecl;
//}
//
///// Determine whether this variable is a for-loop declaration for a
///// for-in statement in Objective-C.
//bool isObjCForDecl() const {
//  return NonParmVarDeclBits.ObjCForDecl;
//}
//
///// Determine whether this variable is an ARC pseudo-__strong
///// variable.  A pseudo-__strong variable has a __strong-qualified
///// type but does not actually retain the object written into it.
///// Generally such variables are also 'const' for safety.
//bool isARCPseudoStrong() const {
//  return isa<ParmVarDecl>(this) ? false : NonParmVarDeclBits.ARCPseudoStrong;
//}
//
///// Whether this variable is (C++1z) inline (whether specified or implicitly so)
//bool isInline() const {
//  return isa<ParmVarDecl>(this) ? false : NonParmVarDeclBits.IsInline;
//}
//bool isInlineSpecified() const {
//  return isa<ParmVarDecl>(this) ? false
//                                : NonParmVarDeclBits.IsInlineSpecified;
//}
//
///// Whether this variable is (C++11) constexpr.
//bool isConstexpr() const {
//  return isa<ParmVarDecl>(this) ? false : NonParmVarDeclBits.IsConstexpr;
//}
//
///// Whether this variable is the implicit variable for a lambda init-capture.
//bool isInitCapture() const {
//  return isa<ParmVarDecl>(this) ? false : NonParmVarDeclBits.IsInitCapture;
//}
//
///// Whether this local extern variable declaration's previous declaration
///// was declared in the same block scope. Only correct in C++.
//bool isPreviousDeclInSameBlockScope() const {
//  return isa<ParmVarDecl>(this)
//         ? false
//         : NonParmVarDeclBits.PreviousDeclInSameBlockScope;
//}
//
//// END OF GROUPING
//
///// Retrieve the variable declaration from which this variable could
///// be instantiated, if it is an instantiation (rather than a non-template).
//VarDecl *getTemplateInstantiationPattern() const;
//
///// If this variable is an instantiated static data member of a
///// class template specialization, returns the templated static data member
///// from which it was instantiated.
//VarDecl *getInstantiatedFromStaticDataMember() const;
//
///// If this variable is an instantiation of a variable template or a
///// static data member of a class template, determine what kind of
///// template specialization or instantiation this is.
//TemplateSpecializationKind getTemplateSpecializationKind() const;
//
///// If this variable is an instantiation of a variable template or a
///// static data member of a class template, determine its point of
///// instantiation.
//SourceLocation getPointOfInstantiation() const;
//
///// If this variable is an instantiation of a static data member of a
///// class template specialization, retrieves the member specialization
///// information.
//MemberSpecializationInfo *getMemberSpecializationInfo() const;
//
///// Retrieves the variable template that is described by this
///// variable declaration.
/////
///// Every variable template is represented as a VarTemplateDecl and a
///// VarDecl. The former contains template properties (such as
///// the template parameter lists) while the latter contains the
///// actual description of the template's
///// contents. VarTemplateDecl::getTemplatedDecl() retrieves the
///// VarDecl that from a VarTemplateDecl, while
///// getDescribedVarTemplate() retrieves the VarTemplateDecl from
///// a VarDecl.
//VarTemplateDecl *getDescribedVarTemplate() const;
//
//// Is this variable known to have a definition somewhere in the complete
//// program? This may be true even if the declaration has internal linkage and
//// has no definition within this source file.
//bool isKnownToBeDefined() const;
//
//
//
//
//
//
//
//
//#define REFL_ImplicitParamDecl\
//  ( ( /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
////GROUPING WITHIN NONPARMVARDECL:
///// Returns the implicit parameter kind.
//ImplicitParamKind getParameterKind() const {
//  return static_cast<ImplicitParamKind>(NonParmVarDeclBits.ImplicitParamKind);
//}
//
//
//#define REFL_ParmVarDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
////GROUP WITHIN PARMVARDECL:
//bool isObjCMethodParameter() const {
//  return ParmVarDeclBits.IsObjCMethodParam;
//}
//
//unsigned getFunctionScopeDepth() const {
//  if (ParmVarDeclBits.IsObjCMethodParam) return 0;
//  return ParmVarDeclBits.ScopeDepthOrObjCQuals;
//}
//
///// Returns the index of this parameter in its prototype or method scope.
//unsigned getFunctionScopeIndex() const {
//  return getParameterIndex(); //DWR note this is a private method def'd in terms of ParmVarDeclBits
//}
//
//ObjCDeclQualifier getObjCDeclQualifier() const {
//  if (!ParmVarDeclBits.IsObjCMethodParam) return OBJC_TQ_None;
//  return ObjCDeclQualifier(ParmVarDeclBits.ScopeDepthOrObjCQuals);
//}
//void setObjCDeclQualifier(ObjCDeclQualifier QTVal) {
//  assert(ParmVarDeclBits.IsObjCMethodParam);
//  ParmVarDeclBits.ScopeDepthOrObjCQuals = QTVal;
//}
//
///// True if the value passed to this parameter must undergo
///// K&R-style default argument promotion:
/////
///// C99 6.5.2.2.
/////   If the expression that denotes the called function has a type
/////   that does not include a prototype, the integer promotions are
/////   performed on each argument, and arguments that have type float
/////   are promoted to double.
//bool isKNRPromoted() const {
//  return ParmVarDeclBits.IsKNRPromoted;
//}
///// Determines whether this parameter has a default argument that has not
///// yet been parsed. This will occur during the processing of a C++ class
///// whose member functions have default arguments, e.g.,
///// @code
/////   class X {
/////   public:
/////     void f(int x = 17); // x has an unparsed default argument now
/////   }; // x has a regular default argument now
///// @endcode
//bool hasUnparsedDefaultArg() const;
//bool hasUninstantiatedDefaultArg() const {
//  return ParmVarDeclBits.DefaultArgKind == DAK_Uninstantiated;
//}
//bool hasInheritedDefaultArg() const {
//  return ParmVarDeclBits.HasInheritedDefaultArg;
//}
////END GROUPING
//
//Expr *getDefaultArg();
///// Retrieve the source range that covers the entire default
///// argument.
//SourceRange getDefaultArgRange() const;
//
//QualType getOriginalType() const;
//
///// Determine whether this parameter is actually a function
///// parameter pack.
//bool isParameterPack() const;
//
//
//
//
//
//
//
//#define REFL_DecompositionDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_OMPCapturedExprDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
//#define REFL_VarTemplateSpecializationDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (VarTemplatePartialSpecialization)\
//  )
///**/
//#define REFL_VarTemplatePartialSpecializationDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
//#define REFL_TemplateDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (TemplateTemplateParm)(BuiltinTemplate)\
//    (RedeclarableTemplate)\
//  )
///**/
//#define REFL_TemplateTemplateParmDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_BuiltinTemplateDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
//#define REFL_RedeclarableTemplateDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (FunctionTemplate)(ClassTemplate)(VarTemplate)(TypeAliasTemplate)\
//  )
///**/
//#define REFL_FunctionTemplateDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ClassTemplateDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_VarTemplateDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_TypeAliasTemplateDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
//#define REFL_UsingShadowDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (ConstructorUsingShadow)\
//  )
///**/
//#define REFL_ConstructorUsingShadowDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//
//#define REFL_ObjCContainerDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (ObjCCategory)(ObjCProtocol)(ObjCInterface)\
//    (ObjCImpl)\
//  )
///**/
//#define REFL_ObjCCategoryDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ObjCProtocolDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ObjCInterfaceDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
//
///**/
//#define REFL_ObjCImplDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , (ObjCCategoryImpl)(ObjCImplementation)\
//  )
///**/
//#define REFL_ObjCCategoryImplDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
///**/
//#define REFL_ObjCImplementationDecl\
//  ( ( ((FirstMethodHere, ReturnTypeHere))\
//    , /*no pseudobase activations*/\
//    , 0/*not a pseudochild*/\
//    )\
//  , /*no children*/\
//  )
//
//#endif /* LLVM_DECLNODESREFL_H */
