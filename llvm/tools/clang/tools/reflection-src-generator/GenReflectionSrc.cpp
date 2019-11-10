//
// Created by David Rector on 2019-07-13.
//

#include <iostream> //TEMP
#include <fstream> //ofstream
#include <sstream> //stringstream

#include <set>
#include <map>

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecordLayout.h" // for getting base class offsets
#include "clang/AST/RecursiveASTVisitor.h" //FIXME no longer needed, but need stuff included from this
#include "clang/AST/Expr.h" // for evaluating default args etc.
#include "clang/Driver/Options.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

#include "clang/Parse/Parser.h"

#include "generated_files/reflection_incs/ReflectionHeaderID.h"

#ifndef CMAKE_GEN_REFLHEADERID
The header ID has not been defined!
#endif


using namespace llvm;
using namespace clang;
using namespace clang::tooling;

/// Starts at the definition, works backward until it finds a decl/redecl that is
/// in the proper semantic context.
/// We need this because the "canonical" decl will sometimes be a friend decl,
/// lexically inside a struct which we may not include, causing us to miss out
/// on the inclusion
static TagDecl *getRealCanonicalDecl(TagDecl *TD) {
//  //DWR hack-ish: this for member classes of CTSD bases we have inlined,
//  // we need to be sure to include each class individually, so return the original
//  // thing if its an instantiation.  That might get problematic, we'll see...
//  if (auto RD = dyn_cast<CXXRecordDecl>(TD)) {
//    if (RD->isInstantiation())
//      return TD;
//  }
  auto SCPrimary = TD->getDeclContext()->getPrimaryContext(); //SEMANTIC context
  auto CurTD = TD->getDefinition();
  while (CurTD->getLexicalDeclContext()->getPrimaryContext() != SCPrimary) {
    CurTD = CurTD->getPreviousDecl();
    if (!CurTD) {
      return TD; //Probably a template specialization, which we'll omit anyway...
//      TD->dump();
//      llvm_unreachable("Where's the original semantically-placed def?");
    }

  }
  return CurTD;
}

static std::string getdoc(NamedDecl *ND) {
  ASTContext &C = ND->getASTContext();
  if (auto rawcomment = C.getRawCommentForDeclNoCache(ND))
    return rawcomment->getRawText(C.getSourceManager());
  else
    return "";
}
static std::string ExprToString(Expr *E, ASTContext &C) {
  if (!E)
    return "";
  std::string ExprStr;
  llvm::raw_string_ostream OS(ExprStr);
  E->printPretty(OS, nullptr, C.getPrintingPolicy());
  return OS.str();
}
//static std::string get_qualdname_without_tparms(const NamedDecl *ND,
//                                                std::string rootname, std::string sep) {
//  std::string res = "";
//  if (auto DC = ND->getDeclContext()) {
//    Decl *prntdecl = Decl::castFromDeclContext(DC);
//    if (auto prntND = dyn_cast<NamedDecl>(prntdecl)) {
//      if (prntND->getName() != rootname) {
//        res = get_qualdname_without_tparms(prntND, rootname, sep);
//        res += sep;
//      }
//    }
//  }
//  return res + ND->getDeclName().getAsString();
//}
//static std::string get_qualdname_without_tparms_wrtclang(const NamedDecl *ND) {
//  return get_qualdname_without_tparms(ND, "clang", "::"); //defined at bottom
//}
/// e.g. ReplaceAll(RD->getQualifiedName(), ":", "_")
static std::string ReplaceAll(std::string str, const std::string &from, const std::string &to) {
  size_t start_pos = 0;
  while((start_pos = str.find(from, start_pos)) != std::string::npos) {
    str.replace(start_pos, from.length(), to);
    start_pos += to.length(); // Handles case where 'to' is a substring of 'from'
  }
  return str;
}

/// Only performs the replacement when surrounded by non-alphanumeric characters.
/// @example
///  assert(ReplaceAllWords("_Bool_Cool _Bool Cool_Bool", "_Bool", "bool")
///        == "_Bool_Cool bool Cool_Bool" );
///
static std::string ReplaceAllWords(std::string str, const std::string &from, const std::string &to) {
  size_t start_pos = 0;
  while((start_pos = str.find(from, start_pos)) != std::string::npos) {
    auto end_pos = start_pos + from.length();
    if ( (start_pos == 0 || !std::isalnum(str[start_pos-1]))
         && (end_pos == str.length() || std::isspace(str[end_pos]))
            ) {
      str.replace(start_pos, from.length(), to);
      start_pos += to.length(); // Handles case where 'to' is a substring of 'from'
    } else
      start_pos += from.length(); //Skip over this instance of from

  }
  return str;
}

static std::string getFixedDeclName(DeclarationName declname) {
  if (declname.isIdentifier())
    return declname.getAsString();
  else {
    assert(!declname.getAsString().empty() && "Probably should not have included this");
    return ReplaceAll(ReplaceAll(ReplaceAll(ReplaceAll(ReplaceAll(
            ReplaceAll(ReplaceAll(ReplaceAll(ReplaceAll(ReplaceAll(
            ReplaceAll(ReplaceAll(ReplaceAll(ReplaceAll(ReplaceAll(
            ReplaceAll(declname.getAsString(),
                     " ", "_"),
                     "->", "_arrow"),
                     "*", "_star"),
                     "+", "_plus"),
                     "-", "_minus"),
                     "&", "_and"),
                     "|", "_or"),
                     "/", "_div"),
                     "=", "_eq"),
                     "!", "_not"),
                     "<", "_less"),
                     ">", "_gr"),
                     "%", "_mod"),
                     "^", "_exp"),
                     "~", "_tilde"),
                     "[]", "_sub");
    }
  //^ TODO Add more as needed...

}

//static bool outer_namespace_is(TagDecl *TD, std::string targ_ns_str = "std") {
//  NamespaceDecl *outermost_ns;
//  DeclContext *cur = TD->getDeclContext();
//  while (!cur->getParent()->isTranslationUnit()) {
//    cur = cur->getParent();
//  }
//  outermost_ns = dyn_cast<NamespaceDecl>(Decl::castFromDeclContext(cur));
//  if (outermost_ns && outermost_ns->getName() == targ_ns_str)
//    return true;
//  return false;
//}

static QualType getUnderlyingType(QualType qt) {
  QualType res = qt.getCanonicalType().getNonReferenceType();

  while (res->isPointerType())
    res = res->getPointeeType().getCanonicalType();

  res = res.getUnqualifiedType().getCanonicalType();

  //For now, we're not including array types, so in order that we
  // may test them we'll just return them as arrays.
  // But eventually if we include array types we would want to
  // uncomment this.
//  if (auto arrty = dyn_cast<clang::ArrayType>(res.getTypePtr()))
//    return getUnderlyingType(arrty->getElementType());

  return res;
}

static bool willConvertToStrLit(QualType QT, /*hack param*/bool CheckForUnhandledNameFrags = true) {

  if (isConstCharPtrType(QT))
    return true;

  std::string typestr = QT.getCanonicalType().getUnqualifiedType().getAsString();

  if (typestr == "class llvm::StringRef")
    return true;
  if (typestr == "llvm::StringRef")
    return true;
  if (typestr == "class std::__1::basic_string<char>") //std::string alias for me
    return true;

  //Note we won't convert llvm::StringRef though -- we'll reflect that directly.

#ifndef NDEBUG
  else if (auto CAT = dyn_cast<ConstantArrayType>(QT.getTypePtr())) {
    QualType elem_t = CAT->getElementType();
    assert(!(elem_t.isConstQualified() && elem_t->isCharType()) && "Should have handled this above");
  }
  if (CheckForUnhandledNameFrags && typestr.find("StringRef") != std::string::npos
      && typestr.find("reflcontainers::") == std::string::npos //okay in reflcontainers namespace
      ) {
    std::cout << "<" << typestr << ">\n";
    assert(false && "Still non-templated StringRef in there!");
  }
  if (CheckForUnhandledNameFrags && typestr.find("basic_string") != std::string::npos
       && typestr.find("reflcontainers::") == std::string::npos //okay in reflcontainers namespace
      ) {
    std::cout << "<" << typestr << ">\n";
    assert(false && "Seems to still be a std::string alias in there in there!  See name above.  Fix the checks.");
  }
#endif
  return false;
}


static bool reflectionTemplateNeeded(QualType QT) {
  if (willConvertToStrLit(QT, false/*don't check for unhandled name fragments*/))
    return false;

  QT = getUnderlyingType(QT);



  //TEMP/TODO for now we disallow fields/methods that require array/function
  // types -- too hard to handle the name printing right now.
  // Probably will implement eventually, though barely used.
  if (QT->isArrayType() || QT->isFunctionType())
    return false;

  if (auto *TD = QT.getTypePtr()->getAsTagDecl()) {
    if (isa<EnumDecl>(TD))
      return false; //Enums don't need templates

//    if (outer_namespace_is(TD, "std"))
//      return false; //is a tag decl in std namespace: no reflection template needed.

    return true; //is a non-enum tag decl: needs a reflection template
  }
  return false; //is a non-tag decl, i.e. a primitive type: no reflection template needed.
}


/// Changes _Bool to bool, std::string to const char *, etc.  HACKY.
static std::string getFixedTypeName(QualType qt) {
  std::string name = qt.getAsString();
  // Just in case the std::string alias changes from the weird thing below,
  //  this will at least catch any unaliased, non-const std::strings.
  if (name == "std::string")
    return "const char *";

  // Keep intptr_t types as they are, for clarity
  if (name == "intptr_t")
    return name;

  name = qt.getCanonicalType().getAsString();


  //This is so stupid and hacky and inefficient.
  if (qt.isConstQualified()) {
    name = ReplaceAll(name, "const class llvm::StringRef", "const char *");
    name = ReplaceAll(name, "const llvm::StringRef", "const char *");
    name = ReplaceAll(name, "const class std::__1::basic_string<char>", "const char *"); //std::string alias for me
  } else {
    name = ReplaceAll(name, "class llvm::StringRef", "const char *");
    name = ReplaceAll(name, "llvm::StringRef", "const char *");
    name = ReplaceAll(name, "class std::__1::basic_string<char>", "const char *");
  }

  // We don't reflect aliases presently so need
  // to get the canonical type.
  // Replace any instances of _Bool with bool while
  // you're there.
  name = ReplaceAllWords(name, "_Bool", "bool");

#ifndef NDEBUG
  // Note that if we decide to allow methods with
  // reference parameters these asserts
  // may fail --
  if (name.find("_Bool") != std::string::npos) {
    std::cout << "<" << name << ">\n";
    assert(false && "Still _Bools in there!");
  }
  if (name.find("StringRef") != std::string::npos) {
    std::cout << "<" << name << ">\n";
    assert(false && "Still StringRef in there!  See the name above.");
  }
  if (name.find("basic_string") != std::string::npos) {
    std::cout << "<" << name << ">\n";
    assert(false && "Probably still a std::string alias (basic_string) in there in there!  See name above.");
  }
#endif



#ifndef NDEBUG
  if (name.find("_Bool") != std::string::npos) {
    std::cout << "<" << name << ">\n";
    assert(false && "Still bools in there!");
  }
#endif

  return name;

  ////OLD:
//  if (qt.getAsString().find("std::string") != std::string::npos)
//    return ReplaceAll(qt.getAsString(), "std::string", "const char *");
//
//  // Note that if you don't do this, enums are labeled classes for some reason,
//  // plus aliases won't be dealiased, etc.
//  qt = qt.getCanonicalType();
//
//  return ReplaceAll(qt.getAsString(),
//          "_Bool", "bool");

}

static std::string justBasicTypeNameFixes(QualType QT) {
  std::string name = QT.getAsString();

  //First, a bug in clang: enum types sometimes have "class " prepended instead of "enum "
  if (auto td = QT->getAsTagDecl()) {
    if (isa<EnumDecl>(td)) {
      return ReplaceAllWords(name, "class", "enum");
    }

  }
  if (QT->isIntegerType()) {
    if (name == "_Bool")
      return "bool";
    if (name == "const _Bool")
      return "const bool";
  }


  return name;
}


enum ReflAnnotKind {
  RAK_none = 0,
  RAK_no_reflect,
  RAK_reflect,
  RAK_constructible,
};




static FunctionDecl *get_friendfunc_if_should_include(FriendDecl *frienddecl) {
  if (auto thefriend = frienddecl->getFriendDecl()) {
    if (auto func = dyn_cast<FunctionDecl>(thefriend)) {
      if (func->isOverloadedOperator()
          && func->getNumParams() == 2
          && func->getMinRequiredArguments() == 2) //only include friend decls for binary ops.
        return func;
    }
  }
  return nullptr;
}

static QualType get_operator_star_ret_type_from(CXXRecordDecl *RD) {
  assert(RD);
  assert(RD->getDefinition() && "Can only access this on a complete type");

  RD = RD->getDefinition();

  for (auto D : RD->decls()) {
    if (auto FD = dyn_cast<FunctionDecl>(D)) {
      if (FD->getOverloadedOperator() == OO_Star) { //i.e. if is operator*()
        return FD->getReturnType().getCanonicalType();
      }
    }
  }

  for (auto b : RD->bases()) {
    CXXRecordDecl *thebaseclass = b.getType().getCanonicalType().getTypePtr()->getAsCXXRecordDecl();
    assert(thebaseclass);
    assert(thebaseclass->getDefinition());
    QualType baseres = get_operator_star_ret_type_from(thebaseclass);
    if (!baseres.isNull())
      return baseres;
  }
  return QualType();
}

static QualType get_elem_t_from_array_ref(QualType arrayRefTy) {
  auto *TD = arrayRefTy->getAsTagDecl();
  assert(TD->getDefinition() && "Should not have included this type if no def available");
  if (!TD) {
    arrayRefTy->dump();
    llvm_unreachable("Expected this to be an llvm::ArrayRef<T> instance, but its not a TagDecl!");
  }

  ClassTemplateSpecializationDecl *CTSD = dyn_cast<ClassTemplateSpecializationDecl>(TD);
  if (!CTSD) {
    TD->dump();
    llvm_unreachable("Expected this to be an llvm::ArrayRef<T> instance, but its not a CTSD!");
  }

  //Return the typename parameter:
  return CTSD->getTemplateArgs().get(0).getAsType().getCanonicalType();
}

/// Note that if the type of operator* is a reference,
/// we dereference it, so that we won't register it as being rbptr
/// (since in practice we will return it by value when iterating,
/// rather than by reference).
/// Not a big deal, just adds some efficiency by preventing some types
/// from being marked that they are sometimes returned by pointer
/// even though they are not.
static QualType get_deref_t_from_iterator(QualType IteratorT) {
  if (TagDecl * TD2 = IteratorT->getAsTagDecl()) {
    assert(isa<CXXRecordDecl>(TD2) && "an iterator that is a tag decl but not a class?  Wha??");
    auto RD = cast<CXXRecordDecl>(TD2);
    if (!RD->getDefinition()) {
      RD->dump();
//      llvm::unreachable("Cannot get the derefernce type for above iterator class because it is not complete. "
//                       "Probably due to it being defined or instantiated first inside the template.  "
//                       "To solve, manually instantiate your iterator types, when they are enclosed with");
      return QualType();
    }
    QualType res = get_operator_star_ret_type_from(RD);
    if (res.isNull()) {
      TD2->dump();
      llvm::errs() << "Could not find operator* in the return type (dumped above) of a begin/end function\n";
      return QualType();
    }
    if (res->isReferenceType()
//        && res->getPointeeType()->isPointerType() //DWR COMMENTED OUT: dereference ANY reference type.
        )
      return res->getPointeeType();
    return res;

  } else {
    assert(IteratorT->isPointerType());
    return IteratorT->getPointeeType();
  }
}

// DWR TODO: only call this once, load result into the
static QualType get_elem_t_from_iterator_range(QualType iteratorRangeTy) {
  auto *TD = iteratorRangeTy->getAsTagDecl();
  assert(TD->getDefinition() && "Should not have included this type if no def available");
  if (!TD) {
    iteratorRangeTy->dump();
    llvm::errs() << "Expected this to be an llvm::iterator_range<T> instance, but its not a TagDecl!\n";
    return QualType();
  }

  ClassTemplateSpecializationDecl *CTSD = dyn_cast<ClassTemplateSpecializationDecl>(TD);
  if (!CTSD) {
    TD->dump();
    llvm::errs() << "Expected this to be an llvm::iterator_range<T> instance, but not a CTSD!\n";
    return QualType();
  }

  QualType IteratorT = CTSD->getTemplateArgs().get(0).getAsType().getCanonicalType();

  if (IteratorT.isNull()) {
    CTSD->dump();
    llvm::errs() << "Expected this to be an llvm::iterator_range<T>, but couldn't get first template arg as type\n";
    return QualType();
  }

  return get_deref_t_from_iterator(IteratorT);
}


LLVM_ATTRIBUTE_UNUSED static bool is_in_reflcontainer_ns(Decl *D) {
  DeclContext *DC = D->getDeclContext();
  DeclContext *Parent;
  while (true) {
    Parent = DC->getParent();
    if (Parent->isTranslationUnit())
      break;
    DC = Parent;
  }
  if (DC->isNamespace() && cast<NamespaceDecl>(DC)->getNameAsString() == "reflcontainers")
    return true;
  return false;
}


static std::string getClientQualdName(NamedDecl *ND, class ReflGenASTConsumer &mainobj);
static std::string getRKname(CXXRecordDecl *RD, class ReflGenASTConsumer &mainobj);

using stream_t = std::stringstream;

static void generate_BuildReflectNewCase(CXXRecordDecl *RD,
                                         std::string RKname, stream_t &fstream) {

  fstream << "  case RK_" << RKname << ": {\n"
          << "    if (autodelete) return defaultConstructType(SemaRef, KWLoc,\n"
             "        BuildReflectionType( *this, KWLoc, new (Context) "
                      << RD->getQualifiedNameAsString() << "(Context), (RefOvld<1>*)0 ) );\n"
          << "    auto res = new " << RD->getQualifiedNameAsString()/*okay*/ << "(Context);\n"
          << "    //ClientPtrMap.insert( {reinterpret_cast<intptr_t>(res), RK_" << RKname << "} );\n"
          << "    return defaultConstructType(SemaRef, KWLoc, BuildReflectionType(*this, KWLoc, res) );\n" //Note no (Context)
             "  }\n";
  //DWR TODO: in debug mode at least, you should add it to some std::set of pointers for the given type, for debugging at delete time
}
static void generate_ReflectDeleteCase(CXXRecordDecl *RD,
                                       std::string RKname, stream_t &fstream) {
  fstream << "  case RK_" << RKname << ": {\n"
          << "    //auto mapres = ClientPtrMap.find(X);\n"
             "    //if (mapres == ClientPtrMap.end()) {\n"
             "    //  Diag(KWLoc, diag::err_refldelete_nonalloc_ptr) << X << RK_" << RKname << ";\n" //TODO create this error message
             "    //  return ExprError();\n"
             "    //}\n"
             "    //if (mapres.second != RK_" << RKname << ")\n"
          << "    //  Diag(KWLoc, diag::err_refldelete_different_ptr) << X << RK_" << RKname << " << mapres.second;\n"
          << "    //  return ExprError();\n"
             "    //}\n"
             "    //ClientPtrMap.erase(mapres);\n"
             "    auto deleter_lambda = [&](QualType, ArrayRef<intptr_t>) -> ExprResult {\n"
             "      delete reinterpret_cast<" << RD->getQualifiedNameAsString()/*okay*/ << " *>(X);\n"
          << "      return ExprResult();\n"
          << "    };\n"
          << "    return new (Context) ReflectionTraitExpr(Context, KWLoc, RTK_prop, RK_" << RKname << ",\n"
          << "        0/*MemNum (dummy)*/, 1/*IsPtr (dummy)*/, Context.VoidTy, deleter_lambda,\n"
          << "        ArrayRef<Expr*>()/*RemArgs (dummy)*/, RParenLoc);\n" //DWR TEST ME
//          << "    return new (Context) VoidReflectionExpr(Context, KWLoc, RParenLoc, deleterfunc, reinterpret_cast<" << RD->getQualifiedNameAsString() << " *>(X) );\n" //OLD
          << "  }\n";
}



static bool isInAnonymousCtx(Decl *D) {
  auto DC = D->getDeclContext();
  assert(DC);
  if (DC->isTranslationUnit())
    return false;
  if (auto NS = dyn_cast<NamespaceDecl>(DC)) {
    if (NS->isAnonymousNamespace())
      return true;
  } else if (auto RD = dyn_cast<RecordDecl>(DC)) {
    if (RD->isAnonymousStructOrUnion())
      return true;
  }
  return isInAnonymousCtx(Decl::castFromDeclContext(DC));
}


////////////////////////////////////
// clang::ASTConsumer
////////////////////////////////////

class ReflGenASTConsumer : public clang::ASTConsumer {

public:


  /// This will hold pointers to all the decls we're including, whether
  /// due to being annotated with "reflect" or being required by something
  /// that is so annotated -- classes, methods, fields, etc.
  std::set<Decl *> included_decls;

  /// This will hold, in the key types, pointers to all ClassTemplateSpecializationDecls
  /// whose member decls we want to load into the derived class.
  /// I.e. we won't give these things their own name anywhere (yet)
  /// but we will load all the functionality into their derived classes.
  /// The val types will be the final non-CTSD RDs they are inherited into
  std::map<ClassTemplateSpecializationDecl *, CXXRecordDecl *> included_CTSDs;

//  /// The classes a client can construct via __reflect_new.
//  /// For now, will contain just the reflected container classes
//  /// in ClientReflContainers.h (e.g. VectorDecl, SetStr, MapDeclStr, etc.).
//  std::set<CXXRecordDecl *> default_contructible_reflections;

  /// Mainly, the set of function decls encountered whose return type is an llvm::iterator_range<...>.
  /// But we also map to the element type, so we don't have to look that up each time.
  /// We will need to treat these specially.
  std::map<FunctionDecl *, QualType> range_returning_funcs;

  /// For any classes that are themselves ranges -- have begin() and end() funcs that return
  /// iterators.
  std::map<CXXRecordDecl *, QualType> range_classes;

  struct class_usage {
    bool rbvalue;
    bool rbptr;
  };
  std::map<CXXRecordDecl *, class_usage> class_usage_map;



  std::set<std::string> clang_classnames_to_include;

private:

  std::stringstream logstream;
  void log_decision(bool included, NamedDecl *ND, std::string explanation) {
    //TODO
  }

  void log_error(NamedDecl *ND, std::string explanation) {
    //TODO
  }

  static std::string to_log_string(ReflAnnotKind rak) {
    switch(rak) {
      case RAK_no_reflect:  return " [no_reflect-annotated]";
      case RAK_reflect:     return " [reflect-annotated]";
      case RAK_constructible:   return " [reflect_constructible-annotated]";
      case RAK_none:        return "";
    }
  }

public:


  ReflAnnotKind get_reflannot(NamedDecl *ND) {
    assert(ND);
    ReflAnnotKind prevres;
    //TODO also set up a recursive function to that look ahead to redeclarations,
    // just for completeness
    if (auto prevdecl = ND->getPreviousDecl())
      prevres = get_reflannot(cast<NamedDecl>(prevdecl));
    else
      prevres = RAK_none;

    if (auto *Annot = ND->getAttr<AnnotateAttr>()) {
      ReflAnnotKind res;
      if (Annot->getAnnotation() == "reflect_constructible")
        res = RAK_constructible;
      else if (Annot->getAnnotation() == "reflect")
        res = RAK_reflect;
      else if (Annot->getAnnotation() == "no_reflect")
        res = RAK_no_reflect;
      else
        res = RAK_none;

      if (res && prevres)
        log_error(ND, "Reflection-related annotations should only be given on "
                      "declaration of this entity.  Ignoring earlier annotation.");

      return res ? res : prevres;

    }
    return RAK_none;
  }
  
  /// Note that we only call this once we've loaded all the class definitions, to avoid
  /// skipping over forward declared classes's bases if that makes sense.
  void propagate_rb_to_from_bases(CXXRecordDecl *theclass, bool &rbptr, bool &rbvalue) {

    for (auto b : theclass->bases()) {
      CXXRecordDecl *thebaseclass = b.getType().getCanonicalType().getTypePtr()->getAsCXXRecordDecl();
//      assert(thebaseclass->isCompleteDefinition());
//      assert(included_decls.count(getRealCanonicalDecl(thebaseclass)));

      auto &baseusage = class_usage_map[thebaseclass];
      rbptr = rbptr || baseusage.rbptr;
      baseusage.rbptr = rbptr;
      rbvalue = rbvalue || baseusage.rbvalue;
      baseusage.rbvalue = rbvalue;

      propagate_rb_to_from_bases(thebaseclass, rbptr, rbvalue);
    }


  }

private:
  bool templated_scope(Decl *D) const {
    DeclContext *prnt = D->getDeclContext();
    if (prnt->isTranslationUnit())
      return false;
    Decl *prntasdecl = Decl::castFromDeclContext(prnt);
    if (prntasdecl->isTemplated() //only one of these should...
        || prntasdecl->isTemplateDecl() ///...be necessary, TODO figure out which
        )
      return true;
    return templated_scope(prntasdecl);
  }

  /// @returns whether the inclusion could not happen,
  /// false if we encounter a templated context
  /// Note that getDeclContext always gets the semantic context,
  /// so no need to navigate to the proper semantic TD.
  bool include_prntscopes_of(TagDecl *TD) {
    if (templated_scope(TD))
      return false;

    // Anonymous namespaces/structs are too much of a headache
    // at least for now:
    if (isInAnonymousCtx(TD))
      return false;

    DeclContext *prnt = TD->getDeclContext();
    while (!prnt->isTranslationUnit()) {
      Decl *pdecl = Decl::castFromDeclContext(prnt);
      // If its a record, let's add in all the other members,
      // so that if later something needs access to those and
      // tries to load inclusions for that RD, it doesn't
      // have to determine whether the members have been added.
      // (Probably would be good to abstract away the included_decls set
      // to handle this kind of stuff...TODO).
      if (auto RD = dyn_cast<CXXRecordDecl>(pdecl)) {
        assert(RD->getNameAsString() != "(anonymous)");
        bool dummy;
        //Once you get to a CTSD, you can return if its included.
        if (!isa<ClassTemplateSpecializationDecl>(RD))
          load_inclusions_for(RD, dummy);
        else {
          return included_CTSDs.count(cast<ClassTemplateSpecializationDecl>(RD));
        }
      } else {
        assert(isa<NamespaceDecl>(pdecl) && "unexpected DeclContext type -- "
                                            "may need to handle like above");
        included_decls.insert(pdecl);
        //^ For namespaces, we can just mark that they are included.
      }
      prnt = prnt->getParent();
    }
    return true;
  }




  /// @returns whether this QT had an underlying
  /// TagDecl not marked no_reflect, OR the definition
  /// was not parsed, OR was otherwise unusable (a template).
  /// I.e. returns true usually, false if can't
  /// be reflected.  This tells us whether a func/field
  /// that depends on this type needs to be omitted.
  // TODO add log_decisions for any decision not to include something,
  // with a good explanation.
  bool load_inclusions_for(QualType QT, bool debug = false) {

    std::string name = QT.getAsString();
//    //DWR TEMP:
//    if (name.find("kvpair_t") != std::string::npos)
//      llvm::outs() << "In load_inclusions_for(QualType), processing : " << name << "\n";

    if (name.find("std::string") != std::string::npos
        && name.find("reflcontainers::") == std::string::npos //reflcontainer templates of std::string don't apply here
        ) {
      //SPECIAL CASE: we mark std::string as included without actually loading it.
      // We will always turn std::strings into const char * in clang.
      if (name == "std::string")
        return true;
      return false; //we don't want std::string *, std::string::iterator etc; too hard
      // to combine with the conversion to const char *.
    }
    assert( name != "std::string *");

    QualType UQT = getUnderlyingType(QT);
    name = UQT.getAsString();

    if (name.find("StringRef") != std::string::npos
        && name.find("reflcontainers::") == std::string::npos //reflcontainer templates of StringRef don't apply here
        ) {
      if (QT.isConstQualified())
        name = QT.getUnqualifiedType().getAsString();
      if (name == "class llvm::StringRef")
        return true;
      if (name == "llvm::StringRef")
        return true;





      assert(name != "StringRef");
      //Don't want other templates, e.g. ArrayRef<StringRef>...
      return false;
    }

    if (debug)
      llvm::errs() << "|--after string checks...\n";

    // No arrays/function types for now, too hard to deal with names
    // and they're pretty rare, doubtful we really need them
    if (UQT->isArrayType() || UQT->isFunctionType())
      return false;

    if (debug)
      llvm::errs() << "|--after array/function checks...\n";

    assert(UQT.getTypePtr());
    if (auto *TD = UQT.getTypePtr()->getAsTagDecl()) {

      if (debug)
        llvm::errs() << "|--Is TagDecl...\n";

      TD = TD->getDefinition();
      if (!TD) {
        log_error(UQT.getTypePtr()->getAsTagDecl(), "Type definition not available in provided translation unit");
        return false;
      }
      if (TD->getAccess() == AS_protected || TD->getAccess() == AS_private) {
        return false;
      }

      if (debug)
        llvm::errs() << "   |--after access checks...\n";

      assert(TD->isCompleteDefinition());

      if (get_reflannot(TD) == RAK_no_reflect)
        return false;

      if (debug)
        llvm::errs() << "   |--after no_reflect check...\n";

      //COMMENTED the below out, because we still have the problem of not being able to use e.g. std::vector at compile time -- by forcing the std lib to be reflected, maybe we can get around that (but first need to set up templates).
//      // If this is in namespace std, return true but don't include it -- we want to include
//      // those functions, but don't want to reflect the std lib
//      if (outer_namespace_is(TD, "std")) {
//        return true;
//      }

      if (auto ctd = dyn_cast<ClassTemplateSpecializationDecl>(TD)) {
        // For now, we can't handle templates, so return false.  TODO.
        if (debug)
          llvm::errs() << "   |--returning FALSE because is CTSD...\n";
        return false;
      }

      if (debug)
        llvm::errs() << "   |--after CTSD check...\n";

      // Not sure if we need this, so leaving commented.
      // Perhaps the way to do it is pass in to this functino a bool
      // indicating whether this was reflect-annotated, indicating
      // that we should make as many allowances as possible, including
      // letting non-public types in...
//      if ( TD->getAccessUnsafe() == AS_protected ||
//           TD->getAccessUnsafe() == AS_private )
//        return false;

      // Include the parent contexts to be sure the new inclusions are not
      // cut off (due to being in a distinct NamespaceDecl/CXXRecordDecl from what we
      // have already included).
      // If any are templated, return false.
      if (!include_prntscopes_of(TD))
        return false;

      if (debug)
        llvm::errs() << "   |--after prntscopes check...\n";

//      // Leave out the ObjC stuff:
//      if (TD->getNameAsString().find("ObjC") != std::string::npos)
//        return false;

      if (auto RD = dyn_cast<CXXRecordDecl>(TD)) {

        if (debug)
          llvm::errs() << "   |--is CXXRecordDecl...\n";

        // For now, no unions.  But would be straightforward to support them if necessary later.
        if (RD->isUnion())
          return false;

        // This covers fields defined via anonymous structs; they're just a headache
        // right now to handle so we'll omit them.
        // Note we already checked if the parent scopes were anonymous
        // during include_prntscopes_of.
        if (RD->isAnonymousStructOrUnion() || RD->getNameAsString() == "")
          return false;

        if (debug)
          llvm::errs() << "      |--after union/anonymous checks...\n";

        // If this type is a value type but we can't assign it,
        // (i.e. can't say T t; T other = {...}; t = other;)
        // that makes our callbacks trickier, so we'll omit it.
        // (e.g. functions returning a DiagnosticBuilder by value
        //  are omitted because of this.)
        if (!QT->isPointerType() && !QT->isReferenceType()
            && !RD->hasSimpleMoveAssignment()) {
          if (debug)
            llvm::errs() << "      |--returning FALSE because is value type without assignment operator...\n";
          return false;
        }

        bool is_container;


//        //DWR TEMP DEBUG
//        if (RD->getName().str() == "kvpair_t")
//          llvm::outs() << "DWR TEMP DEBUG loading inclusions for kvpair from load_inclusions_for(QualType); "
//                          "full type name = " << QT.getAsString() << "; RD = " << RD << "; about to call load_inclusions_for(RD):\n";


        load_inclusions_for(RD, is_container);
        if (QT->isPointerType() || QT->isReferenceType()) {
//          if (RD->getNameAsString() == "StringRef") {
//            QT.dump();
//            llvm_unreachable("Did not expect StringRef to be returned by ptr/ref, allowing this"
//                             "is causing baffling crashes so I'll stop you right now.  (If you "
//                             "really did return it by ptr/ref somewhere comment this stuff out I guess.) ");
//          }
          class_usage_map[RD].rbptr = true;
        }
        else
          class_usage_map[RD].rbvalue = true;
      } else if (auto ED = dyn_cast<EnumDecl>(TD)) {
        load_inclusions_for(ED);
      } else {
        log_error(TD, "Unhandled TagDecl type");
        return false;
      }
//
//      //TEMP DEBUG
//      if (TD->getQualifiedNameAsString() == "clang::NamedDecl") {
//        std::cout << "\n~~~~FOUND NamedDecl; inclusions up to TU decl: " << included_decls.count(getRealCanonicalDecl(TD));
//        DeclContext *prnt = TD->getDeclContext();
//        while (!prnt->isTranslationUnit()) {
//          auto prntdecl = dyn_cast<NamedDecl>(Decl::castFromDeclContext(prnt));
//          assert(prntdecl);
//          std::cout << ", " << prntdecl->getQualifiedNameAsString() << "(" << included_decls.count(prntdecl) << ")";
//          prnt = prnt->getParent();
//        }
//        getRealCanonicalDecl(TD)->dump();
//      }
//      //END DEBUG

    }



    return true;
  }

public:
  bool parm_included(ParmVarDecl *parm) {
    if (get_reflannot(parm) == RAK_no_reflect) {
      if (!parm->hasDefaultArg())
        llvm_unreachable("No reflect annotation on parameter which doesn't have a default arg");
      return false;
    }
    return true;
  }
private:

  /// @returns whether it was included in the end.  All these should return that,
  /// but this is the only one I needed right now.
  bool load_inclusions_for(FunctionDecl *FD
                           , bool &beginfound, bool &endfound, QualType &elemtype
                           , bool prnt_is_refl_constructible
                           , bool debug = false
                           ) {

    assert(FD);
    auto MD = dyn_cast<CXXMethodDecl>(FD);
//    assert(MD->getAccess() != AS_none); //not sure why this was here
    auto reflannot = get_reflannot(FD);
    std::string explanation;

    // We'll add more conditions to default_include later, but we'll
    // get the basics out of the way now: has to be public and, if
    // a method (which is always, for now), either static or const,
    // and is not annotated no_reflect:
    bool default_include = (FD->getAccess() == AS_public)
                           && (!MD || MD->isStatic() || MD->isConst()
                               || prnt_is_refl_constructible //anything whose parent class is annotated as
                                                             //reflect_constructible-annotated can include non-const methods
                                                             //(perhaps dangerous, for non-containers -- should check if was
                                                             //constructed by user before modifying.
                                                             //Irrelvant at present, only containers constructible.)
                              )
                           && reflannot != RAK_no_reflect;
    auto ReturnType = FD->getReturnType();
    auto DealiasedReturnType = ReturnType.getCanonicalType();
    bool ReturnTypeReflectionTemplateNeeded = reflectionTemplateNeeded(DealiasedReturnType);
    bool RetTypeIsIterRange = default_include
            && DealiasedReturnType.getAsString().find("iterator_range") != std::string::npos; //HACK
    bool RetTypeIsArrayRef = default_include
            && DealiasedReturnType.getAsString().find("ArrayRef") != std::string::npos; //HACK

//    if (RetTypeIsIterRange) {
//      if (FD->getNumParams() != 0) {
//        FD->dump();
//        assert(false && "Expected a function whose return type contains \"iterator_range\" "
//                        "to be nullary, e.g. decls(), getBases() etc. -- fix logic");
//      }
//    }


    // Does name CONTAIN "_begin" or "_end"? (or begin_ or end_)?
    std::string FDName = FD->getNameAsString();
    bool NameContainsBeginOrEnd = default_include
            && ( (   FDName.find("_begin") != std::string::npos)
                 || (FDName.find("begin_") != std::string::npos)
                 || (FDName.find("_end") != std::string::npos)
                 || (FDName.find("end_") != std::string::npos)
               )
//            && ReturnTypeReflectionTemplateNeeded
            ;

//    //DWR TEMP DEBUG
//    if (is_in_reflcontainer_ns(FD)) {
//      llvm::outs() << "`--Processing " << FD->getName() << "; default_include/NameContainsBeginOrEnd/FDName: " << default_include << "/" << NameContainsBeginOrEnd << "/" << FDName << "\n";
//    }

    //DWR TEMP DEBUG:


    //IS the name begin() or end()?
    // If so set the beginfound, endfound, and elem_t types, so we can
    // interpret the enclosing class as a range.
    if (default_include && !NameContainsBeginOrEnd) {
      if (FDName == "begin") {

        NameContainsBeginOrEnd = true;
        // We only support range infrastructure when the begin && end funcs are nullary...
        if (FD->getNumParams() == 0) {
          QualType cur_elem_t = get_deref_t_from_iterator(DealiasedReturnType);



//          //DWR TEMP DEBUG
//          if (cur_elem_t.getAsString().find("kvpair") != std::string::npos) {
//            llvm::errs() << "DWR TEMP DEBUG FOUND begin function whose derefed type is kvpair...\n";
//            if (!load_inclusions_for(cur_elem_t, /*debug*/true))
//              llvm_unreachable("^ DID NOT LOAD INCLUSIONS! FIXME");
//          }
//          //END

          if (!cur_elem_t.isNull() && load_inclusions_for(cur_elem_t)) {
            if (endfound && cur_elem_t != elemtype) {
              FD->dump();
              llvm::errs() << "begin() and end() return different types -- very odd!";
            } else {
              beginfound = true;
              elemtype = cur_elem_t;
            }
          }
//          //DEBUG
//          else {
//            if (cur_elem_t.isNull()) {
//              FD->dump();
//              DealiasedReturnType.dump();
//              llvm::errs() << "^ Cannot find deref_t for return type of begin/end func, omitting it; see method/return type dump above\n";
//            } else {
//              cur_elem_t.dump();
//              llvm::errs() << "^ Found the deref_t, but load_inclusions failed for elem_t above.\n";
//            }
//          }
//          //END DEBUG
        }
      }
      else if (FDName == "end") {
        NameContainsBeginOrEnd = true;
        // We only support range infrastructure when the begin && end funcs are nullary...
        if (FD->getNumParams() == 0) {
          QualType cur_elem_t = get_deref_t_from_iterator(DealiasedReturnType);
          if (!cur_elem_t.isNull() && load_inclusions_for(cur_elem_t)) {
            if (beginfound && cur_elem_t != elemtype) {
              FD->dump();
              llvm::errs() << "begin() and end() return different types -- very odd!";
            } else {
              endfound = true;
              elemtype = cur_elem_t;
            }
          }
        }
      }
    }

    assert(FDName != "end" || NameContainsBeginOrEnd || !default_include && "Problem with logic above!");

//    if (NameContainsBeginOrEnd) {
//      if (ReturnType.getAsString().find("iterator") == std::string::npos) {
//        FD->dump();
//        assert(false && "Expected a x_begin or x_end function that doesn't return a primitive "
//                        "to return a type/type alias whose name contains \"iterator\" -- "
//                        "may need to fix logic?");
//      }
//      if (FD->getNumParams() != 0) {
//        FD->dump();
//        llvm_unreachable("Expected a function whose name contains \"_begin\" or \"_end\" "
//                         "to be nullary, e.g. decls_begin(), bases_end() etc. -- may need to "
//                         "fix logic?");
//      }

//    }

/*
 DWR TODO: Adjust thi
 */


    if (default_include//i.e. is public, not no_reflect annotated, and,
                       //if a method, static or const
//        && (FD->isStatic() ? !FD->getReturnType()->isVoidType() : true)
//        && (FD->isStatic() ? (FD->getNameAsString().find("classof") != std::string::npos)
//                           : true) //only include classof static methods
//        && (FD->getMinRequiredArguments() == 0
//            || FD->getMinRequiredArguments() == 1 //TEMP TEST DELETEME
//           )
        && !NameContainsBeginOrEnd // don't want iterators, we'll instead handle range-returning funcs only
        && !FD->isDeleted()
        && !isa<CXXConstructorDecl>(FD)
        && !(isa<CXXConversionDecl>(FD)
                && (ReturnTypeReflectionTemplateNeeded /*|| ReturnType.getAsString() == "std::string" */) )
                //^ conversion decls to reflected templates are impossible
                // b/c we can only specify a template, not a class,
                // so can't define the conversion target type.
                // But operator bool() etc. are fine.
                // We don't want conversion operators to std::string either,
                // even though no reflect template needed -- WAIT we're gonna try something with that...
        && !FD->getPreviousDecl() //omit if this is an ool def of a previously-encountered func decl
        //DWR commented these out because if return type is overridden we want to include the override decls:
//        && !FD->template hasAttr<OverrideAttr>() // The original virtual func decl will suffice
//        && !FD->template hasAttr<FinalAttr>()    // for us, we don't need to also add the overrides.
//        && (FDName.find("ObjC") == std::string::npos)
           // Omit castToDeclContext and castFromDeclContext (both static functions)
           // -- we don't need them, since we're shifting the alignment properly via template args.
        && (!MD || !MD->isStatic() || FDName.find("cast") == std::string::npos || FDName.find("DeclContext") == std::string::npos)
        )
      default_include = true;
#ifndef NDEBUG
    else {
      default_include = false;
      explanation = "CXXMethodDecl with these disqualifying properties:";
      if (MD && !MD->isStatic() && !MD->isConst()) explanation += " [non-const/static]";
//      if (FD->getMinRequiredArguments() != 0) explanation += " [non-nullary]";
      if (reflannot == RAK_no_reflect) explanation += " [no_reflect annotated]";
      if (FD->isDeleted()) explanation += " [deleted]";
      if (isa<CXXConstructorDecl>(FD)) explanation += " [ctor]";
      if (isa<CXXConversionDecl>(FD)
              && ReturnTypeReflectionTemplateNeeded)
        explanation += " [conversion operator to non-primitive]";
      if (FD->getPreviousDecl()) explanation += " [already processed]";
//      if (FD->template hasAttr<OverrideAttr>()) explanation += " [override]";
//      if (FD->template hasAttr<FinalAttr>()) explanation += " [final]";
//      if (FD->getNameAsString().find("ObjC") != std::string::npos)
//        explanation += " [ObjC]";
      if (NameContainsBeginOrEnd)
        explanation += " [begin/end]";
      if ((MD && MD->isStatic() && FDName.find("cast") != std::string::npos && FDName.find("DeclContext") != std::string::npos))
        explanation += " [castTo/FromDeclContext]";
    }

#endif


    // Make sure if the return type is an llvm::iterator_range
    // or llvm::ArrayRef instance, it is complete
    if ( RetTypeIsIterRange || RetTypeIsArrayRef) {
      if (auto TD = DealiasedReturnType->getAsTagDecl()) {
        if (!TD->getDefinition()) {
          assert(reflannot != RAK_reflect &&
                 "Reflecting a range-returning method whose return type has no definition ");
          default_include = false;
          explanation += " [returns range, no iterator def]";
        }
      }

      //Also, for now, no range-returning functions that take arguments; that is too difficult to implement:
      if (FD->getNumParams() != 0) {
        assert(reflannot != RAK_reflect &&
               "Reflecting a range-returning method that is non-nullary -- we haven't yet "
               "implemented how to pass args to such a function.");
        default_include = false;
        explanation += " [returns range, requires params]";
      }

    } //if RetTypeIsIterRange/ArrayRef


    for (auto parm : FD->parameters()) {
      QualType parmtype = parm->getType();
      // For now we'll exclude methods that take reference parameters to primitives
      // (e.g. myfunc(unsigned &valref)), but references to classes
      // (e.g. myfunc(ASTContext &C) are fine: (WAIT, see below)
      if (parmtype->isReferenceType()
          && !reflectionTemplateNeeded(parmtype) //DWR COMMENT OUT if references to classes cause problems
          ) {
        default_include = false;
        explanation += " [has reference-to-primitive param]";
        break;
      }
      // Here's a tricky but necessary case.  If we are returning
      // a reflection template instance, we cannot accept any
      // string literal arguments, since we cannot Evaluate string
      // literals to calculate the reflection type at the time when
      // we need to, in Sema::ActOnReflectionTrait (believe me I've
      // tried).
      // Now, IF you really need such functions, you can have StringRef
      // reflected instead of converted to string literal, and then
      // you can pass StringRefs as integers no problem.
      // But this would still exclude you from supplying your own
      // string literals, UNLESS you figured out some way to
      // wrap them as integers -- unfortunately reinterpret_cast from
      // a const char * to a intptr_t isn't allowed in constexpr contexts;
      // if we could just do that we'd be fine.  Perhaps look into making
      // an exception to that rule, if you really need that functionality.
      if (willConvertToStrLit(parmtype, false/*hack param so we dont do asserts yet*/)
          && ReturnTypeReflectionTemplateNeeded) {
        default_include = false;
        explanation += " [class-returning reflection with string-literal param]";
        break;
      }

    }


    bool included = false;


//    //TEMP DEBUG:
//    if (!default_include && FD->getNameAsString() == "classof") {
//      FD->dump();
//      std::cout << explanation;
//      llvm_unreachable("Why is a classof func not being reflected?");
//    }

    if ( reflannot == RAK_reflect || (default_include) ) {
      // If this is decls(), getBases() etc, we need to load inclusions for
      // the typename template param to llvm::iterator_range.  There are no
      // parameters to load inclusions for, note.
      if (RetTypeIsIterRange || RetTypeIsArrayRef) {
        assert(FD->getNumParams() == 0);
        QualType elem_t;
        if (RetTypeIsIterRange)
          elem_t = get_elem_t_from_iterator_range(DealiasedReturnType);
        else
          elem_t = get_elem_t_from_array_ref(DealiasedReturnType);
        if (!elem_t.isNull() && load_inclusions_for(elem_t)) {
          included_decls.insert(FD);
          included = true;
          range_returning_funcs.insert({FD, elem_t});
        }
//        else {
//          llvm::errs() << "// Note: failed inclusion load for elem_t (" << elem_t.getAsString() << ") of range-returning function " << FD->getNameAsString() << "\n";
//        }
      } else {
        bool dep_types_reflected = load_inclusions_for(FD->getReturnType());

        for (auto parm : FD->parameters()) {
          if (!parm_included(parm))
            break;
          if (!dep_types_reflected)
            break;
          dep_types_reflected = load_inclusions_for(parm->getType());


        }

        if (dep_types_reflected) {
          included_decls.insert(FD);
          included = true;
        } else if (reflannot == RAK_reflect)
          log_error(FD,
                    "Method annotated reflect, but its return type and/or "
                    "one or more of its parameter types are annotated no_reflect");
      }
    }

    return included;

    // DWR TODO log decision and explanation into a file, so you know
    // why something was omitted
  }



  /// @returns whether it was included
  bool load_inclusions_for(FieldDecl *FD) {
    assert(FD);
    assert(FD->getAccess() != AS_none);
    auto reflannot = get_reflannot(FD);
    // By default, public fields are reflected, all other not,
    // but a reflection annotation can override.
    // Btw, I think FieldDecls are all nonstatic...if not that should also be a requirement.
    if ( reflannot == RAK_reflect ||
            ( FD->getAccess() == AS_public
//             && FD->getNameAsString().find("ObjC") == std::string::npos
             && reflannot != RAK_no_reflect) ) {

      // If the type of the field is a value, but it is not copy constructible, omit it.
      // (e.g. ASTContext::DeclarationNames)
      QualType type = FD->getType();
      if (!type->isReferenceType() && !type->isPointerType()) {
        if (auto TD = type->getAsTagDecl()) {
          if (auto RD = dyn_cast<CXXRecordDecl>(TD)) {
            if (!RD->hasSimpleCopyConstructor()) {
              return false;
              //TODO log_decision
            }
          }
        }
      }

      // Load inclusions for the type of the FieldDecl
      if (load_inclusions_for(FD->getType())) {

////DWR TEMP DEBUG:
//        if (FD->getNameAsString() == "ImmRange") {
//          QualType UQT = getUnderlyingType(FD->getType());
//          UQT.dump();
//          assert(UQT->isRecordType());
//          assert(UQT->getAsTagDecl());
//          assert(UQT->getAsRecordDecl());
//          assert(UQT->getAsRecordDecl()->getNameAsString() == "");
//          // For some reason isAnonymousStructOrUnion doesn't
//          // work here, so need to test the name manually.
//        }
////END

        included_decls.insert(FD);
        return true;
      }
      else if (reflannot == RAK_reflect)
        log_error(FD, "Field annotated reflect, but its type annotated no_reflect");
      // TODO log decision
    }
    return false;
  }





  /// Just handles methods and fields
  void load_inclusions_for_mem(Decl *d
                               , bool &beginfound, bool &endfound, QualType &elemtype
                               , bool prnt_is_refl_constructible
                               , std::set<std::string> &curRDmemIDs
                               ) {
    assert(d);

    // Skip over implicit decls (e.g. the injected identity
    // CXXRecordDecl within each class)
    if (d->isImplicit())
      return;

    // Note that we skip over nested CXXRecordDecls, EnumDecls etc.
    // We may "come back" for them later when we see
    // they are needed by fields/methods/later classes.
    if (auto MD = dyn_cast<CXXMethodDecl>(d)) {
      //Skip over out-of-line implems of methods:
      if (MD->getPreviousDecl()) {
        if (get_reflannot(MD) != RAK_none)
          log_error(MD, "Annotation given on out of line implem of func "
                        "-- expected it only on original Decl, for clarity.  "
                        "Annotation ignored.");
        return;
      }
      //Gotta make sure we don't already have this method name in this class (due to CTSD base inlining):
      std::string curmemID = MD->getNameAsString();
      for (auto p : MD->parameters()) {
        //Tack on the parameter name, so overloads are okay.
        curmemID += p->getType().getCanonicalType().getAsString();
      }


      if (curRDmemIDs.insert(curmemID).second) {//if this was a new insertion
        bool incl = load_inclusions_for(MD, beginfound, endfound, elemtype, prnt_is_refl_constructible, true/*debug*/);
        // if you didn't end up including it, erase the memID:
        if (!incl)
          curRDmemIDs.erase(curmemID);
      }

    }
    else if (auto fd = dyn_cast<FieldDecl>(d)) {
      if (curRDmemIDs.insert(fd->getName().str()).second) {
        bool incl = load_inclusions_for(fd);
        // if you didn't end up including it, erase the memID:
        if (!incl)
          curRDmemIDs.erase(fd->getName().str());
      }

    }
    // Probably should do memID stuff here too, but haven't needed it:
    else if (auto frienddecl = dyn_cast<FriendDecl>(d)) {
      if (auto thefriend = get_friendfunc_if_should_include(frienddecl)) {
        bool dummybeg, dummyend;
        QualType dummy_elem_t;
        load_inclusions_for(thefriend
                            , dummybeg, dummyend, dummy_elem_t
                            , prnt_is_refl_constructible/*is this right? prob. doesn't matter*/
                            );
      }
    }
  }



  void load_inclusions_for_bases_of(CXXRecordDecl *RD, bool &is_container
                                    , bool &beginfound, bool &endfound, QualType &iter_elem_t
                                        //^ only used if we have a CTSD base whose mem
                                        //^ decls we need to treat as those of RD
                                    , std::set<std::string> &curRDmemIDs
                                    , bool drvd_is_reflconstructible
                                    , CXXRecordDecl *TargRD //if RD is a CTSD, this is the original derived non-CTSD everything will be added into
                                    ) {
    assert(RD);

//    std::cout << "loading inclusions for bases of:" << RD->getNameAsString() << "\n";

    for (auto b : RD->bases()) {
      assert(b.getAccessSpecifier() != AS_none && "guess you need to account for AS_none");

      // Skip over the non-public bases
      if (b.getAccessSpecifier() != AS_public)
        continue;

      CXXRecordDecl *thebaseclass = b.getType().getCanonicalType().getTypePtr()->getAsCXXRecordDecl();
      assert(thebaseclass);

      // I think this helps out for detecting class template specialization decls, not sure though:
      thebaseclass = thebaseclass->getDefinition();
      assert(thebaseclass);

      auto reflannot = get_reflannot(thebaseclass);

      // If a base class has been annotated no_reflect, or is
      // not reflected for another reason (e.g. templates, for now) we
      // skip over it BUT NOT ITS base classes -- i.e. we won't
      // have access to its direct fields/methods but we may
      // still have access to its bases' fields/methods,
      // excepting those which are also non-reflected:
      if (reflannot == RAK_no_reflect
          || isa<ClassTemplateSpecializationDecl>(thebaseclass)
                  ) {
//        std::cout << "---DEBUG: not including base " << thebaseclass->getQualifiedNameAsString() << " because no_reflect/CTSD ? " << (get_reflannot(thebaseclass) == RAK_no_reflect) << "/"
//            << isa<ClassTemplateSpecializationDecl>(thebaseclass) << "\n";
        load_inclusions_for_bases_of(thebaseclass, is_container
                                     , beginfound, endfound, iter_elem_t
                                     , curRDmemIDs
                                     , drvd_is_reflconstructible
                                     , TargRD
                                     );

        //DWR TEST: NOW, if it is a ClassTemplateSpecializationDecl, let's try
        //loading the member decls anyway: i.e. we won't include the class, but we're
        // going to try to treat all of the template instantiation's decls as
        // the derived class's decls (this is the same loop we do
        // in load_inclusions_for(RD), just after processing the bases;
        // keep them the same):
        if (auto CTSD = dyn_cast<ClassTemplateSpecializationDecl>(thebaseclass)) {
            //TODO you should also test whether the original template was annotated no_reflect
          included_CTSDs.insert({ CTSD, TargRD} );


          for (auto memdecl : CTSD->decls()) {

            // HACK: skip isFirstDecl, that one gets named in Redeclarable and elsewhere
            // and is troublesome.
            if (auto ND = dyn_cast<CXXMethodDecl>(memdecl)) {
              if (ND->getDeclName().isIdentifier() && ND->getName() == "isFirstDecl")
                continue;
            }

            load_inclusions_for_mem(memdecl
                                    , beginfound, endfound, iter_elem_t
                                    , drvd_is_reflconstructible
                                    , curRDmemIDs
                                    );
          }
        }


      } else if (include_prntscopes_of(thebaseclass)) {
//        std::cout << "-------DEBUG: including base " << thebaseclass->getQualifiedNameAsString() << "\n";
        load_inclusions_for(thebaseclass, is_container);
        assert(included_decls.count(getRealCanonicalDecl(thebaseclass)));
      }
    }

  }

  void load_inclusions_for(EnumDecl *ED) {
    assert(ED->getDefinition());
    included_decls.insert(getRealCanonicalDecl(ED));
  }

  void load_inclusions_for(CXXRecordDecl *RD, bool &is_container) {

    assert(RD);
    assert(!RD->isAnonymousStructOrUnion() && "Need to address this");
    // Only need to process bases/members if not previously inserted.
    // Note we insert the "canonical" declaration of this, so that we know
    // where we to define it in the proper semantic context later.

    assert(RD->getDefinition());

    assert(!isa<ClassTemplateSpecializationDecl>(RD) &&
           "should be inlining these -- if you handle them here they'll be included");

//    //DWR DEBUG
//    if (RD->getName().str() == "kvpair_t") {
//      llvm::errs() << "\n\nDWR TEMP DEBUG Processing kvpair_t (decl = " << RD << ", quald name = " << RD->getQualifiedNameAsString()
//                   << ", canonical decl=" << getRealCanonicalDecl(RD) << ", isInstantiation = " << RD->isInstantiation()
//                    << ", isTemplate = " << RD->isTemplateDecl() << ", RD->getInstantiatedFromMemberClass(): "
//                    << RD->getInstantiatedFromMemberClass() << ") in load_inclusions_for(CXXRecordDecl*)...\n\n\n\n";
//    }


    if (included_decls.insert(getRealCanonicalDecl(RD)).second) {

      RD = RD->getDefinition();
      assert(RD);

      auto reflannot = get_reflannot(RD);

      // Load the bases and the members, AND determine whether this is a container
      // for which we need to add infrastructure to be able to iterate over via for...
      bool is_container = false;
      bool beginfound = false, endfound = false;
      QualType iter_elem_t;
      std::set<std::string> curRDmemIDs; //we use this to be sure we don't add two things of the same name to the RD.
                                         //(Needed cause of the CTSD stuff we're doing.)

      load_inclusions_for_bases_of( RD, is_container
                                  , beginfound, endfound, iter_elem_t //NB we only look in CTSD bases here
                                  , curRDmemIDs
                                  , (reflannot == RAK_constructible)
                                  , RD //TargRD
                                  );

      for (auto memdecl : RD->decls()) {
        load_inclusions_for_mem( memdecl
                               , beginfound, endfound, iter_elem_t
                               , (reflannot == RAK_constructible)
                               , curRDmemIDs
                               );
      }

//      //DWR TEMP DEBUG
//      if (is_in_reflcontainer_ns(RD)) {
//        llvm::outs() << "\nLoaded inclusions for " << RD->getQualifiedNameAsString()
//                     << "; beginfound/endfound = " << beginfound << "/" << endfound << "\n";
//      } //END TEMP



//      if ((beginfound || endfound) && !(beginfound && endfound)) {
//        llvm::errs() << "Odd - begin() was " << (beginfound ? "" : "not ")
//                     << "found while end() was " << (endfound ? "" : "not ")
//                     << "found in " << RD->getQualifiedNameAsString()
//                     << ".  Omitting from range_classes, but check out if the "
//                        "code logic is right.  (Perhaps one was non-nullary)?\n";
//      }

      if (beginfound && endfound) {
        assert(!iter_elem_t.isNull() && "Expected the iter_elem_t to have been set!");
        range_classes.insert( {RD, iter_elem_t} );
      }

//      if (RD->getNameAsString() == "MapIntStr" && (!beginfound || !endfound)) {
//        llvm::errs() << "DWR TEMP DEBUG: ";
//      }

      //If it is annotated with __attribute__((annotate("reflect_constructible"))),
      // set up the reflection cases now.
      // Also, add it to the rbinfo map as being returned by pointer or reference (rbptr).
      // For now, only the containers in clang/Basic/ClientReflContainers.h
      // are so annotated; if we add others, we might want need to
      // account for how the new construction should be returned -- value or pointer,
      // etc., and thus move the actual generation down below with the other
      // generations.  But this should work for now.
      if (get_reflannot(RD) == RAK_constructible) {
          //DWR TODO add an assert about having a public constructor that takes an ASTContext & param.
          // Below no longer valid:
//        if (!RD->hasDefaultConstructor())
//          llvm_unreachable("Expected a reflect_constructible-annotated class to have a "
//                           "default constructor"); //it should be public too, but can't test for that easily :(
        if (RD->isAbstract())
          llvm_unreachable("Did not expect a reflect_constructible-annotated class to be abstract");

        std::string RKname = getRKname(RD, *this);
        generate_BuildReflectNewCase(RD, RKname, BuildReflectNewCases_inc);
        generate_ReflectDeleteCase(RD, RKname, ReflectDeleteCases_inc); //unused

        class_usage_map[RD].rbptr = true; // NB all our new constructions will return references
                                          // for now (rbptr covers ptrs AND refs);
                                          // if that changes need to adjust logic here.
      } else {
        //It's not reflect_constructible -- just add it to the class uage map, if not there already,
        // with a blank rbinfo struct to be added to as necessary.
        class_usage_map.insert( {RD, {}} );
      }
    }

  }


  void load_inclusions_for_ns(NamespaceDecl *NsD) {
    assert(NsD);
    bool isClang = NsD->getParent()->isTranslationUnit() && (NsD->getName() == "clang");
    for (Decl *d : NsD->decls()) {
      assert(d);
      if (auto RD = dyn_cast<CXXRecordDecl>(d)) {
//        std::cout << "[DEBUG] looking at " << RD->getNameAsString() << "...\n";

        // Only try including those records that have a definition, now or later:
        if (auto *def = RD->getDefinition()) {
          // Only worry about the first decls
          if (RD == getRealCanonicalDecl(RD)) {
            // Note that you can override inclusion within the initial
            // class name list by annotating class with no_reflect.
            //---ACTUALLY for now that would result in errors about GetReflectionObjKind<...> being uninstantiated,
            // because we're relying on the DeclNodes.inc etc. macros to handle all the classes.
            //DWR FIXME take out this DeclNodes.inc stuff, replace with your own incs
            // (for the Decl/Stmt/Type nodes for cast cases)
            // so you can once again omit ObjC stuff and no_reflect stuff...
            if ((isClang && clang_classnames_to_include.count(RD->getName())
//                && get_reflannot(def) != RAK_no_reflect
//                && RD->getNameAsString().find("ObjC") == std::string::npos
                 ) || get_reflannot(RD) == RAK_constructible //ALSO include anything annotated as a reflection container
                ) {
              bool is_container;
              load_inclusions_for(RD, is_container);
              class_usage_map[RD].rbptr = true;
              // We'll mark these all as being returned by pointer, since at the very least
              // the cast expressions will be able to reflect each of these by pointer.
            }
          }

        } //if the class has a definition
        else if (isClang && clang_classnames_to_include.count(RD->getName())) {
          log_error(RD, "This matches the name of a class name marked to be included, "
                        "and yet the definition was not found anywhere -- need to add "
                        "more #includes to your input file");
        }

        } //if the decl is a class
      } //for each decl in this clang namespace
    }


private:
  stream_t BuildReflectNewCases_inc,
    ReflectDeleteCases_inc,
    ReflectionCTDLookupCases_inc,
    ReflectionGetRKSpecs_inc,
    ClientGetReflectionObjKindSpecs_stream,
    ReflectionObjKindList_inc,
    ReflectionObjKindNodes_inc,
//    ReflectorCallOperator_PointerCases_inc,
//    ReflectorCallOperator_NonPointerCases_inc,
//    ReflectPropDecls_inc,
//    ReflectPropDefs_inc,
    ReflTrait_SetTypeAndCBs_Cases_inc,
    ReflKindGetTotalMemNumCases_inc,
    ReflKindToStringCases_inc,
    ReflKindAndMemNumToQTorCTD_inc,
    ReflInfoNamespaces_inc,
    ClientReflection_h_preamble,
    ClientReflection_h_fwddecls,
    ClientReflection_h_impldefs,
    ClientReflection_h_aliases,
    ClientReflection_h_close_cppxmeta,
    ClientReflection_h_std_tuple_specs,
    ClientReflection_h_finalendif;


//  unsigned char cur_nesting_level = 0;
  std::string curindentstr = "";
//  DeclContext *cur_scope;



  void writeChunksForDecl(Decl *D); //defined at bottom

  void generate_client_reflheader_preamble(stream_t &fstream); //defined at bottom

private:
  void writeChunksForMemberEnumsOf_declsloop(CXXRecordDecl *RD) {
    for (Decl *DD : RD->decls()) {

//      //TEMP DEBUG
//      if (auto TD = dyn_cast<TagDecl>(DD)) {
//        if (TD->getQualifiedNameAsString() == "clang::NamedDecl") {
//          std::cout << "FOUND NamedDecl while writing; is first decl: " << (TD == getRealCanonicalDecl(TD)) << "\n";
//        }
//      }
//      //END DEBUG-

      if (auto ED = dyn_cast<EnumDecl>(DD)) {
        // We only process the canonical decl of each tag; that
        // way we put them in the proper semantic context.
        if (ED->getDefinition() && ED == getRealCanonicalDecl(ED)) {
          if (included_decls.count(ED))
            writeChunksForDecl(ED);
        }
        else

          assert(!included_decls.count(ED)
                 && "Should only have canonical decl of each"
                    "TagDecl (in this case EnumDecl) in included_decls");
      }
    }
  }
  void writeChunksForMemberEnumsOf_CTSDbasehlpr(CXXRecordDecl *RD) {
    for (auto b : RD->bases()) {
      auto typeptr = b.getType().getCanonicalType().getTypePtr();
      assert(typeptr);
      CXXRecordDecl *thebaseclass = typeptr->getAsCXXRecordDecl();
      assert(thebaseclass);
      if (auto CTSD = dyn_cast<ClassTemplateSpecializationDecl>(thebaseclass)) {
        if (included_CTSDs.count(CTSD)) {
          //1. Process the CTSD bases:
          writeChunksForMemberEnumsOf_CTSDbasehlpr(thebaseclass);
          //2. Process the current decls:
          writeChunksForMemberEnumsOf_declsloop(thebaseclass);
        }
      }
    }
  }
  void writeChunksForMemberClassesOf_declsloop(CXXRecordDecl *RD) {
    for (Decl *DD : RD->decls()) {
      if (auto RDD = dyn_cast<CXXRecordDecl>(DD)) {

//        if (RDD->getName().str() == "kvpair_t") {
//          llvm::errs() << "DWR TEMP DEBUG: kvpair_t, see dump above; getDefinition()/getRealCanonicalDecl(RD)/included_decls.count(RD): "
//                       << RDD->getDefinition() << "/" << getRealCanonicalDecl(RDD) << "/" << included_decls.count(RDD) << "\n";
////          RDD->
//        }

        // We only process the canonical decl of each tag; that
        // way we put them in the proper semantic context.
        if (RDD->getDefinition() && RDD == getRealCanonicalDecl(RDD)) {
          if (included_decls.count(RDD))
            writeChunksForDecl(RDD);
        }
        else
          assert(!included_decls.count(RDD)
                 && "Should only have canonical decl of each"
                    "TagDecl (in this case CXXRecordDecl) in included_decls");
      }
    }
  }
  void writeChunksForMemberClassesOf_CTSDbasehlpr(CXXRecordDecl *RD) {
    for (auto b : RD->bases()) {
      auto typeptr = b.getType().getCanonicalType().getTypePtr();
      assert(typeptr);
      CXXRecordDecl *thebaseclass = typeptr->getAsCXXRecordDecl();
      assert(thebaseclass);
      if (auto CTSD = dyn_cast<ClassTemplateSpecializationDecl>(thebaseclass)) {
        if (included_CTSDs.count(CTSD)) {
          //1. Process the CTSD bases:
          writeChunksForMemberClassesOf_CTSDbasehlpr(thebaseclass);
          //2. Process the current decls:
          writeChunksForMemberClassesOf_declsloop(thebaseclass);
        }
      }
    }
  }
  void writeChunksForMembersOf_declsloop(DeclContext *DC) {
    for (Decl *DD : DC->decls()) {

      // Note that we only care about Namespaces/Enums/Classes here -- the methods and fields
      // will get iterated through when we process the classes.
      if (isa<NamespaceDecl>(DD) && included_decls.count(DD))
        writeChunksForDecl(DD);
      else if (auto TD = dyn_cast<TagDecl>(DD)) {
        // We only process the canonical decl of each tag; that
        // way we put them in the proper semantic context.
        if (TD->getDefinition() && TD == getRealCanonicalDecl(TD)) {
          if (included_decls.count(TD))
            writeChunksForDecl(TD);
        }
        else

          assert(!included_decls.count(TD)
                 && "Should only have canonical decl of each"
                    "TagDecls in included_decls");
      }
    }
  }
  void writeChunksForMembersOf_CTSDbasehlpr(DeclContext *DC) {
    if (!DC->isRecord())
      return;
    auto RD = cast<CXXRecordDecl>(DC);
    for (auto b : RD->bases()) {
      auto typeptr = b.getType().getCanonicalType().getTypePtr();
      assert(typeptr);
      CXXRecordDecl *thebaseclass = typeptr->getAsCXXRecordDecl();
      assert(thebaseclass);
      if (auto CTSD = dyn_cast<ClassTemplateSpecializationDecl>(thebaseclass)) {
        if (included_CTSDs.count(CTSD)) {
          //1. Process the CTSD bases:
          writeChunksForMemberClassesOf_CTSDbasehlpr(thebaseclass);
          //2. Process the current decls:
          writeChunksForMemberClassesOf_declsloop(thebaseclass);
        }
      }
    }
  }
public:

  void writeChunksForMemberEnumsOf(CXXRecordDecl *RD, unsigned extraindentspaces = 2) {
    std::string oldindentstr = curindentstr;
    for (unsigned i = 0; i != extraindentspaces; ++i)
      curindentstr += " ";

    //1. Process the CTSD bases:
    writeChunksForMemberEnumsOf_CTSDbasehlpr(RD);
    //2. Process the current decls:
    writeChunksForMemberEnumsOf_declsloop(RD);

    curindentstr = oldindentstr;
//    cur_scope = oldscope;
  }

  void writeChunksForMemberClassesOf(CXXRecordDecl *RD, unsigned extraindentspaces = 2) {
    std::string oldindentstr = curindentstr;
    for (unsigned i = 0; i != extraindentspaces; ++i)
      curindentstr += " ";

    //1. Process the CTSD bases:
    writeChunksForMemberClassesOf_CTSDbasehlpr(RD);
    //2. Process the current decls:
    writeChunksForMemberClassesOf_declsloop(RD);

    curindentstr = oldindentstr;
  }

  /// General purpose version, but NB only deals with namespaces/classes/enums
  void writeChunksForMembersOf(DeclContext *DC, unsigned extraindentspaces = 2) {
//    DeclContext *oldscope = cur_scope;
//    cur_scope = DC;

    std::string oldindentstr = curindentstr;
    for (unsigned i = 0; i != extraindentspaces; ++i)
      curindentstr += " ";

    //1. Process the CTSD bases if DC is a class:
    writeChunksForMembersOf_CTSDbasehlpr(DC);
    //2. Process the current decls:
    writeChunksForMembersOf_declsloop(DC);

    curindentstr = oldindentstr;
//    cur_scope = oldscope;
  }



public:
  NamedDecl *is_included_field_or_method(Decl *d) const {

    if ((isa<FieldDecl>(d) || isa<CXXMethodDecl>(d))
           && !d->isImplicit() //just to reduce the std::set searches, doesn't change result
           && included_decls.count(d)) {
      return cast<NamedDecl>(d);
    }
    if (isa<FriendDecl>(d))
      return get_friendfunc_if_should_include(cast<FriendDecl>(d));
    return nullptr;
  }
  static NamedDecl *is_nonstatic_member(Decl *d) {
    if ((isa<FieldDecl>(d) || isa<FunctionDecl>(d))) {
      return cast<NamedDecl>(d);
    }
    return nullptr;
  }

  virtual void HandleTranslationUnit(clang::ASTContext &Context) {


//    intptr_t Dnum = 1000;
//    Decl *D = (Decl *)Dnum;
//    CXXRecordDecl *RD = cast<CXXRecordDecl>(D);
//    intptr_t RDnum = (intptr_t)RD;
//
//    DeclContext *D2DC = Decl::castToDeclContext(D);
//    intptr_t D2DCnum = (intptr_t)D2DC;
//
//    DeclContext *RD2DC = TagDecl::castToDeclContext(RD);
//    intptr_t RD2DCnum = (intptr_t)RD2DC;
//
//    std::cout << "D, RD, D2DC, RD2DC: " << D << ", " << RD << ", " << D2DC << ", " << RD2DC << "\n";
//
//    std::cout << "D-RD, D-D2DC, D-RD2DC: "
//            << std::to_string((intptr_t)D-(intptr_t)RD) << ", "
//            << std::to_string((intptr_t)D-(intptr_t)D2DC) << ", "
//            << std::to_string((intptr_t)D-(intptr_t)RD2DC) << "\n";

//    llvm_unreachable("halt here"); //DWR TEMP
//    std::cout << "\n--------------------------\n\n";



    auto tudecl = Context.getTranslationUnitDecl();

    ReflectionObjKindList_inc << "  RK_none = 0,\n";

    ClientReflection_h_std_tuple_specs << "\n"
                                          "#ifdef __CONSUMER_SUPPORTS_REFLECTION_AND_META__\n"
                                          "namespace std {\n";

    // 1. Load the included_decls set
    for (Decl *d : tudecl->decls()) {
      assert(d);
      if (auto ND = dyn_cast<NamespaceDecl>(d)) {
        // Since all the node types are within clang, we can just look for
        // those namespaces:
        if (ND->getName() == "clang" || ND->getName() == "reflcontainers") {
          included_decls.insert(ND);
          load_inclusions_for_ns(ND);
        }

      }
    }

    // 1b. Propagate rb info to bases:
    for (auto &mapit : class_usage_map) {
      assert(mapit.first && "Null record in class_usage_map");
      propagate_rb_to_from_bases(mapit.first, mapit.second.rbptr, mapit.second.rbvalue);
    }

//    //DEBUG
//    std::cout << "\n\n[DEBUG] included decls in tudecl after processing:\n";
//    for (auto d : included_decls) {
//      DeclContext *DC = d->getDeclContext();
//      while (!DC->isTranslationUnit()) {
//        DC = DC->getParent();
//        std::cout << "  ";
//      }
//      if (auto ND = dyn_cast<NamedDecl>(d))
//        std::cout << ND->getDeclName().getAsString() << "\n";
//      else
//        std::cout << "!!SKIPPING included unnamed decl, not expected...\n";
//    }
//    std::cout << "[END included decls]\n\n\n\n";
//    //END DEBUG

    ClientReflection_h_fwddecls << "namespace refldetail {\n\n";
    ClientReflection_h_impldefs << "namespace refldetail {\n\n";

    ReflectionObjKindNodes_inc << "#ifndef DECL\n"
                                  "# define DECL(RK)\n"
                                  "#endif\n"
                                  "#ifndef STMT\n"
                                  "# define STMT(RK)\n"
                                  "#endif\n"
                                  "#ifndef TYPE\n"
                                  "# define TYPE(RK)\n"
                                  "#endif\n"
                                  "#ifndef OTHER\n"
                                  "# define OTHER(RK)\n"
                                  "#endif\n"
                                  "\n";

    // 2. Write the necessary files by calling a helper function that
    // iteratively writes to each file (and updates the curtabs member)
    // depending on the type of the decl encountered.
    writeChunksForMembersOf(tudecl);

    ClientReflection_h_fwddecls
      << "\n"
         "#ifdef __CONSUMER_SUPPORTS_REFLECTION_AND_META__\n"
         "  template<class T, int PTROFFSET, intptr_t... Xs>\n"
         "  struct impl_offset;\n"
         "  template<class T, int PTROFFSET, intptr_t... Xs>\n"
         "  using impl_offset_t = typename impl_offset<T, PTROFFSET, Xs...>::type;\n"
         "\n"
         "  // Pointer specialization\n"
         "  template<class T, int PTROFFSET, intptr_t PTRVAL>\n"
         "  struct impl_offset<T, PTROFFSET, /*IsPtr*/1, PTRVAL>\n"
         "  {\n"
         "    using type = typename T::template impl</*IsPtr*/1, PTRVAL + PTROFFSET>;\n"
         "  };\n"
         "\n"
         "  // Value specialization (TODO)\n"
         "  template<class T, int PTROFFSET, intptr_t... OBJDATACHUNKs>\n"
         "  struct impl_offset<T, PTROFFSET, /*IsPtr*/0, OBJDATACHUNKs...>\n"
         "  {\n"
         "    static_assert(sizeof(T) == -1,\n"
         "                  \"This specialization not yet implemented! \"\n"
         "                  \"To implement, need to bitshift each OBJDATACHUNK by PTROFFSET, \"\n"
         "                  \"but also move the shifted stuff into the end of val to its left, \"\n"
         "                  \"i.e. ignore the boundaries between chunks. Definitely doable.\" );\n"
         "  };\n"
         "#else\n"
         "  template<class T, int PTROFFSET>\n"
         "  using impl_offset_t = typename T::impl;\n"
         "#endif\n"
         "\n"
         "} //namespace refldetail\n";


//         "\n"
//         "#ifdef __CONSUMER_SUPPORTS_REFLECTION_AND_META__\n"
//         "  /// We need this to shift the pointer alignment for any Decl that has DeclContext\n"
//         "  /// as a base (e.g. TagDecl).  We assume we'll always get a single intptr_t\n"
//         "  /// pointer val -- i.e. if something inherits from DeclContext and Decl AND is returned\n"
//         "  /// by value, we would need to specialize for IsPtr==0, but that is never the case.\n"
//         "  ///\n"
//         "  template<std::size_t ALIGNSHIFT, intptr_t... Xs>      struct getDeclContextBase;\n"
//         "  template<std::size_t ALIGNSHIFT, intptr_t DeclPtrVal> struct getDeclContextBase<ALIGNSHIFT, /*IsPtr*/1, DeclPtrVal> {\n"
//         "    using type = clang::DeclContext::template impl<1, DeclPtrVal + ALIGNSHIFT>;\n"
//         "  };\n"
//         "# define DECLCONTEXTBASE(...) getDeclContextBase<__VA_ARGS__>::type\n"
//         "#else\n"
//         "# define DECLCONTEXTBASE(...) clang::DeclContext::impl\n"
//         "#endif\n"
//         "\n"
//         "} //namespace refldetail\n";


    ClientReflection_h_impldefs << "\n} //namespace refldetail\n";

    //3. Generate the preamble for the client reflection header (that will go
    // before the body)
    generate_client_reflheader_preamble(ClientReflection_h_preamble);

    //4a. Add custom diagnostic support, anything else that relies on previously defined types
    // to the close_cppxmeta section:





    ClientReflection_h_close_cppxmeta
            << "\n"
               "/////////// reflnew //////////////\n"
               "\n"
               "#ifdef __CONSUMER_SUPPORTS_REFLECTION_AND_META__\n"
               "# define reflnew(T)  __reflect_new(cppx::meta::refldetail::GetReflectionObjKind<cppx::meta::refldetail::T>::value)\n"
               "\n"
               "  namespace refldetail {\n"
               "    template<typename T>\n"
               "    struct GetReflectionObjKind;\n"
//               "    template<reflenums::ReflectionObjKind RK, intptr_t...Xs>\n"
//               "    struct GetReflTypeFromInts;\n"
//               "    template<reflenums::ReflectionObjKind RK, intptr_t...Xs>\n"
//               "    using ReflTypeFromInts = typename GetReflTypeFromInts<RK, Xs...>::type;\n"
               "\n"
            << ClientGetReflectionObjKindSpecs_stream.str()
            << "  } //namespace refldetail\n"
               "\n"
               "#else\n"
               "# define reflnew(T)  (cppx::meta::refldetail::ptrwrp<T>())\n"
               "#endif //!__CONSUMER_SUPPORTS_REFLECTION_AND_META__\n";


    ClientReflection_h_close_cppxmeta
           <<  "\n"
               "/////////// CUSTOM DIAGNOSTICS //////////////\n"
               "  \n"
               "#ifdef __CONSUMER_SUPPORTS_REFLECTION_AND_META__\n"
               "  \n"
               "  template<template<intptr_t...> class TMPL, intptr_t ISPTR, intptr_t X>\n"
               "  constexpr intptr_t getReflVal(TMPL<ISPTR, X>) {\n"
               "    return X;\n"
               "  }\n"
               "  template<typename T>\n"
               "  constexpr intptr_t getReflVal(refldetail::ptrwrp<T>) {\n"
               "    return getReflVal(T());\n"
               "  }\n"
               "  constexpr intptr_t getReflVal(intptr_t X) {\n"
               "    return X;\n"
               "  }\n"
               "  \n"
               "  namespace user {\n"
               "    \n"
               "    /// A trivial tuple used to represent a source range,\n"
               "    /// constructed by the user rather than returned by reflection.\n"
               "    template<intptr_t Y0, intptr_t Y1>\n"
               "    struct SourceRange {\n"
               "      constexpr SourceRange(clang::SourceLocation<0, Y0> begin,\n"
               "                            clang::SourceLocation<0, Y1> end)\n"
               "      {}\n"
               "    };\n"
               "    // Deduction guide\n"
               "    template<intptr_t Y0, intptr_t Y1>\n"
               "    SourceRange(clang::SourceLocation<0, Y0> begin,\n"
               "                clang::SourceLocation<0, Y1> end) -> SourceRange<Y0, Y1>;\n"
               "    \n"
               "    /// Represents a character-granular source range, constructed by the user\n"
               "    /// rather than returned via reflection.\n"
               "    ///\n"
               "    /// The underlying SourceRange can either specify the starting/ending character\n"
               "    /// of the range, or it can specify the start of the range and the start of the\n"
               "    /// last token of the range (a \"token range\").  In the token range case, the\n"
               "    /// size of the last token must be measured to determine the actual end of the\n"
               "    /// range.\n"
               "    template<intptr_t Y0, intptr_t Y1>\n"
               "    struct CharSourceRange {\n"
               "      bool IsTokenRange;\n"
               "      \n"
               "      /// @param EndToken1Char0 -- true if the \"end\" location is to\n"
               "      /// (the starting char of) the last token; false if the end\n"
               "      /// is to the very last char (of the last token).\n"
               "      constexpr CharSourceRange(clang::SourceLocation<0, Y0> begin,\n"
               "                                clang::SourceLocation<0, Y1> end,\n"
               "                                bool EndToken1Char0)\n"
               "          : IsTokenRange(EndToken1Char0)\n"
               "      {}\n"
               "    };\n"
               "    // Deduction guide\n"
               "    template<intptr_t Y0, intptr_t Y1>\n"
               "    CharSourceRange(clang::SourceLocation<0, Y0> begin,\n"
               "                    clang::SourceLocation<0, Y1> end,\n"
               "                    bool EndToken1Char0) -> CharSourceRange<Y0, Y1>;\n"
               "    \n"
               "    \n"
               "    /// Annotates a diagnostic with some code that should be\n"
               "    /// inserted, removed, or replaced to fix a user-defined\n"
               "    /// problem with code that is otherwise acceptable to the compiler;\n"
               "    /// e.g. a policy that the user wants to apply in a certain namespace,\n"
               "    /// etc.\n"
               "    ///\n"
               "    /// These will behave identically to compiler-defined FixItHints (you will\n"
               "    /// see the replacement suggestions in your IDE, can automatically apply\n"
               "    /// them by running clang with certain flags, etc.), except that the message\n"
               "    /// will be ((surrounded in double-parentheses)), to visually distinguish it from\n"
               "    /// compiler-defined fixits.\n"
               "    class FixItHint {\n"
               "      template<typename THIRD_FIELD_T>\n"
               "      struct fixitdata {\n"
               "        /*[0]*/intptr_t locA;\n"
               "        /*[1]*/intptr_t locB;\n"
               "        /*[2]*/THIRD_FIELD_T locC_or_Str;\n"
               "        /*[3]*/unsigned IsTokenRange = 2; //2 = unset\n"
               "        constexpr fixitdata(intptr_t locA, intptr_t locB, THIRD_FIELD_T locC_or_Str, unsigned IsTokenRange)\n"
               "                : locA(locA), locB(locB), locC_or_Str(locC_or_Str), IsTokenRange(IsTokenRange)\n"
               "        {}\n"
               "      };\n"
               "      //Deduction guide\n"
               "      template<typename THIRD_FIELD_T>\n"
               "      fixitdata(intptr_t, intptr_t, THIRD_FIELD_T, unsigned itr = 2) -> fixitdata<THIRD_FIELD_T>;\n"
               "      \n"
               "    public:\n"
               "      /// Create a code modification hint that inserts the given\n"
               "      /// code string at a specific location.\n"
               "      template<intptr_t X>\n"
               "      static constexpr auto CreateInsertion(clang::SourceLocation</*IsPtr=*/0, X> InsLoc,\n"
               "                                            const char *Code) {\n"
               "        return fixitdata(X, 0, Code, 2);\n"
               "      }\n"
               "      \n"
               "      /// Create a code modification hint that inserts the given\n"
               "      /// code from \\p FromRange at a specific location.\n"
               "      template<intptr_t X0, intptr_t... X1s>\n"
               "      static constexpr auto CreateInsertionFromRange(clang::SourceLocation</*IsPtr=*/0, X0> InsLoc,\n"
               "                                                     clang::CharSourceRange<0, X1s...> Range) {\n"
               "        return fixitdata(X0, getReflVal(Range.getBegin()), getReflVal(Range.getEnd()), Range.isTokenRange());\n"
               "      }\n"
               "      /// Create a code modification hint that inserts the given\n"
               "      /// code from \\p FromRange at a specific location.\n"
               "      /// This overload takes a user-constructed CharSourceRange.\n"
               "      template<intptr_t X0, intptr_t X1, intptr_t X2>\n"
               "      static constexpr auto CreateInsertionFromRange(clang::SourceLocation<0, X0> InsLoc,\n"
               "                                                     user::CharSourceRange<X1, X2> CustomRange) {\n"
               "        return fixitdata(X0, X1, X2, CustomRange.IsTokenRange);\n"
               "      }\n"
               "      \n"
               "      /// Create a code modification hint that removes the given\n"
               "      /// source range.\n"
               "      template<intptr_t... Xs>\n"
               "      static constexpr auto CreateRemoval(clang::CharSourceRange<0, Xs...> Range) {\n"
               "        return fixitdata(getReflVal(Range.getBegin()), getReflVal(Range.getEnd()), 0, Range.isTokenRange());\n"
               "      }\n"
               "      /// Create a code modification hint that removes the given\n"
               "      /// source range.\n"
               "      /// This overload takes a reflected clang::SourceRange.\n"
               "      template<intptr_t... Xs>\n"
               "      static constexpr auto CreateRemoval(clang::SourceRange<0, Xs...> Range) {\n"
               "        return fixitdata(getReflVal(Range.getBegin()), getReflVal(Range.getEnd()), 0, 2);\n"
               "      }\n"
               "      /// Create a code modification hint that removes the given\n"
               "      /// source range.\n"
               "      /// This overload takes a user-constructed CharSourceRange.\n"
               "      template<intptr_t X0, intptr_t X1>\n"
               "      static constexpr auto CreateRemoval(user::CharSourceRange<X0, X1> CustomRange) {\n"
               "        return fixitdata(X0, X1, 0, CustomRange.IsTokenRange);\n"
               "      }\n"
               "      /// Create a code modification hint that removes the given\n"
               "      /// source range.\n"
               "      /// This overload takes a user-constructed SourceRange.\n"
               "      template<intptr_t X0, intptr_t X1>\n"
               "      static constexpr auto CreateRemoval(user::SourceRange<X0, X1> CustomRange) {\n"
               "        return fixitdata(X0, X1, 0, 2);\n"
               "      }\n"
               "      \n"
               "      /// Create a code modification hint that replaces the given\n"
               "      /// source range with the given code string.\n"
               "      template<intptr_t... Xs>\n"
               "      static constexpr auto CreateReplacement(clang::CharSourceRange<0, Xs...> Range, const char *Code) {\n"
               "        return fixitdata(getReflVal(Range.getBegin()), getReflVal(Range.getEnd()), Code, Range.isTokenRange());\n"
               "      }\n"
               "      /// Create a code modification hint that replaces the given\n"
               "      /// source range with the given code string.\n"
               "      /// This overload takes a reflected clang::SourceRange.\n"
               "      template<intptr_t... Xs>\n"
               "      static constexpr auto CreateReplacement(clang::SourceRange<0, Xs...> Range, const char *Code) {\n"
               "        return fixitdata(getReflVal(Range.getBegin()), getReflVal(Range.getEnd()), Code, 2);\n"
               "      }\n"
               "      /// Create a code modification hint that replaces the given\n"
               "      /// source range with the given code string.\n"
               "      /// This overload takes a user-constructed CharSourceRange.\n"
               "      template<intptr_t X0, intptr_t X1>\n"
               "      static constexpr auto CreateReplacement(user::CharSourceRange<X0, X1> CustomRange, const char *Code) {\n"
               "        return fixitdata(X0, X1, Code, CustomRange.IsTokenRange);\n"
               "      }\n"
               "      /// Create a code modification hint that replaces the given\n"
               "      /// source range with the given code string.\n"
               "      /// This overload takes a user-constructed SourceRange.\n"
               "      template<intptr_t X0, intptr_t X1>\n"
               "      static constexpr auto CreateReplacement(user::SourceRange<X0, X1> CustomRange, const char *Code) {\n"
               "        return fixitdata(X0, X1, Code, 2);\n"
               "      }\n"
               "    };\n"
               "  } //namespace user\n"
               "\n"
               "# define ce_diag(Kind, Loc, ...) __compiler_diag(Kind, cppx::meta::getReflVal(Loc), __VA_ARGS__)\n"
               "  \n"
               "#else //!__CONSUMER_SUPPORTS_REFLECTION_AND_META__\n"
               "  \n"
               "  template<typename T> constexpr intptr_t getReflVal(T t) { return 0; }\n"
               "  \n"
               "  namespace user {\n"
               "    /// A trivial tuple used to represent a source range,\n"
               "    /// constructed by the user rather than returned by reflection.\n"
               "    struct SourceRange {\n"
               "      constexpr SourceRange(clang::SourceLocation begin, clang::SourceLocation end) {}\n"
               "    };\n"
               "    \n"
               "    /// Represents a character-granular source range, constructed by the user\n"
               "    /// rather than returned via reflection.\n"
               "    ///\n"
               "    /// The underlying SourceRange can either specify the starting/ending character\n"
               "    /// of the range, or it can specify the start of the range and the start of the\n"
               "    /// last token of the range (a \"token range\").  In the token range case, the\n"
               "    /// size of the last token must be measured to determine the actual end of the\n"
               "    /// range.\n"
               "    struct CharSourceRange {\n"
               "      bool IsTokenRange;\n"
               "      \n"
               "      /// @param EndToken1Char0 -- true if the \"end\" location is to\n"
               "      /// (the starting char of) the last token; false if the end\n"
               "      /// is to the very last char (of the last token).\n"
               "      constexpr CharSourceRange(clang::SourceLocation begin,\n"
               "                                clang::SourceLocation end,\n"
               "                                bool EndToken1Char0)\n"
               "          : IsTokenRange(EndToken1Char0)\n"
               "      {}\n"
               "    };\n"
               "    \n"
               "    /// Annotates a diagnostic with some code that should be\n"
               "    /// inserted, removed, or replaced to fix a user-defined\n"
               "    /// problem with code that is otherwise acceptable to the compiler;\n"
               "    /// e.g. a policy that the user wants to apply in a certain namespace,\n"
               "    /// etc.\n"
               "    ///\n"
               "    /// These will behave identically to compiler-defined FixItHints (you will\n"
               "    /// see the replacement suggestions in your IDE, can automatically apply\n"
               "    /// them by running clang with certain flags, etc.), except that the message\n"
               "    /// will be surrounded in parentheses, to visually distinguish it from\n"
               "    /// compiler-defined fixits.\n"
               "    class FixItHint {\n"
               "      struct fixitdata {};\n"
               "    public:\n"
               "      /// Create a code modification hint that inserts the given\n"
               "      /// code string at a specific location.\n"
               "      static constexpr auto CreateInsertion(clang::SourceLocation InsLoc,\n"
               "                                            const char *Code) {\n"
               "        return fixitdata();\n"
               "      }\n"
               "      \n"
               "      /// Create a code modification hint that inserts the given\n"
               "      /// code from \\p FromRange at a specific location.\n"
               "      /// This overload takes a reflected clang::CharSourceRange.\n"
               "      static constexpr auto CreateInsertionFromRange(clang::SourceLocation InsLoc,\n"
               "                                                     clang::CharSourceRange Range) {\n"
               "        return fixitdata();\n"
               "      }\n"
               "      \n"
               "      /// Create a code modification hint that inserts the given\n"
               "      /// code from \\p FromRange at a specific location.\n"
               "      /// This overload takes a user-constructed CharSourceRange.\n"
               "      static constexpr auto CreateInsertionFromRange(clang::SourceLocation InsLoc,\n"
               "                                                     user::CharSourceRange CustomRange) {\n"
               "        return fixitdata();\n"
               "      }\n"
               "      \n"
               "      /// Create a code modification hint that removes the given\n"
               "      /// source range.\n"
               "      static constexpr auto CreateRemoval(clang::CharSourceRange Range) {\n"
               "        return fixitdata();\n"
               "      }\n"
               "      /// Create a code modification hint that removes the given\n"
               "      /// source range.\n"
               "      /// This overload takes a reflected clang::SourceRange.\n"
               "      static constexpr auto CreateRemoval(clang::SourceRange Range) {\n"
               "        return fixitdata();\n"
               "      }\n"
               "      /// Create a code modification hint that removes the given\n"
               "      /// source range.\n"
               "      /// This overload takes a user-constructed CharSourceRange.\n"
               "      static constexpr auto CreateRemoval(user::CharSourceRange CustomRange) {\n"
               "        return fixitdata();\n"
               "      }\n"
               "      /// Create a code modification hint that removes the given\n"
               "      /// source range.\n"
               "      /// This overload takes a user-constructed SourceRange.\n"
               "      static constexpr auto CreateRemoval(user::SourceRange CustomRange) {\n"
               "        return fixitdata();\n"
               "      }\n"
               "      \n"
               "      /// Create a code modification hint that replaces the given\n"
               "      /// source range with the given code string.\n"
               "      static constexpr auto CreateReplacement(clang::CharSourceRange Range, const char *Code) {\n"
               "        return fixitdata();\n"
               "      }\n"
               "      /// Create a code modification hint that replaces the given\n"
               "      /// source range with the given code string.\n"
               "      /// This overload takes a reflected clang::SourceRange.\n"
               "      static constexpr auto CreateReplacement(clang::SourceRange Range, const char *Code) {\n"
               "        return fixitdata();\n"
               "      }\n"
               "      /// Create a code modification hint that replaces the given\n"
               "      /// source range with the given code string.\n"
               "      /// This overload takes a user-constructed CharSourceRange.\n"
               "      static constexpr auto CreateReplacement(user::CharSourceRange CustomRange, const char *Code) {\n"
               "        return fixitdata();\n"
               "      }\n"
               "      /// Create a code modification hint that replaces the given\n"
               "      /// source range with the given code string.\n"
               "      /// This overload takes a user-constructed SourceRange.\n"
               "      static constexpr auto CreateReplacement(user::SourceRange CustomRange, const char *Code) {\n"
               "        return fixitdata();\n"
               "      }\n"
               "    };\n"
               "  } //namespace user\n"
               "\n"
               "# define ce_diag(Kind, Loc, ...)\n"
               "  \n"
               "#endif\n"
               "\n"
               "\n";

    //4b. Close out the namespaces etc. introduced in the preamble
    ClientReflection_h_close_cppxmeta << "} // namespace meta \n"
                                        "\n"
                                        "} //namespace cppx\n"
                                        "\n";
    ClientReflection_h_std_tuple_specs << "\n"
                                          "} // namespace std\n"
                                          "#endif //__CONSUMER_SUPPORTS_REFLECTION_AND_META__\n"
                                          "\n";

    ClientReflection_h_finalendif <<  "#endif //CPPX_META_client_reflection_impl_HPP";

    ReflectionObjKindNodes_inc << "\n"
                                  "#undef DECL\n"
                                  "#undef STMT\n"
                                  "#undef TYPE\n"
                                  "#undef OTHER\n";


    std::string pathpfx = std::string(GENERATED_FILES_DIR);

    std::string incs_pathpfx = pathpfx + "reflection_incs/";

    {
      std::ofstream outfile (incs_pathpfx + "BuildReflectNewCases.inc");
      outfile << BuildReflectNewCases_inc.str();
    }
    {
      std::ofstream outfile (incs_pathpfx + "ReflectDeleteCases.inc");
      outfile << ReflectDeleteCases_inc.str();
    }
    {
      std::ofstream outfile (incs_pathpfx + "ReflectionCTDLookupCases.inc");
      outfile << ReflectionCTDLookupCases_inc.str();
    }
    {
      std::ofstream outfile (incs_pathpfx + "ReflectionGetRKSpecs.inc");
      outfile << ReflectionGetRKSpecs_inc.str();
    }
    {
      std::ofstream outfile (incs_pathpfx + "ReflectionObjKindList.inc");
      outfile << ReflectionObjKindList_inc.str();
    }
    {
      std::ofstream outfile (incs_pathpfx + "ReflectionObjKindNodes.inc");
      outfile << ReflectionObjKindNodes_inc.str();
    }
//    {
//      std::ofstream outfile (incs_pathpfx + "ReflectorCallOperator_NonPointerCases.inc");
//      outfile << ReflectorCallOperator_NonPointerCases_inc.str();
//    }
//    {
//      std::ofstream outfile (incs_pathpfx + "ReflectorCallOperator_PointerCases.inc");
//      outfile << ReflectorCallOperator_PointerCases_inc.str();
//    }
//    {
//      std::ofstream outfile (incs_pathpfx + "ReflectPropDecls.inc");
//      outfile << ReflectPropDecls_inc.str();
//    }
//    {
//      std::ofstream outfile (incs_pathpfx + "ReflectPropDefs.inc");
//      outfile << ReflectPropDefs_inc.str();
//    }
    {
      std::ofstream outfile (incs_pathpfx + "ReflTrait_SetTypeAndCBs_Cases.inc");
      outfile << ReflTrait_SetTypeAndCBs_Cases_inc.str();
    }

    {
      std::ofstream outfile (incs_pathpfx + "ReflInfoNamespaces.inc");
      outfile << ReflInfoNamespaces_inc.str();
    }
    {
      std::ofstream outfile (incs_pathpfx + "ReflKindGetTotalMemNumCases.inc");
      outfile << ReflKindGetTotalMemNumCases_inc.str();
    }
    {
      std::ofstream outfile (incs_pathpfx + "ReflKindAndMemNumToQTorCTD.inc");
      outfile << ReflKindAndMemNumToQTorCTD_inc.str();
    }
    {
      std::ofstream outfile (incs_pathpfx + "ReflKindToStringCases.inc");
      outfile << ReflKindToStringCases_inc.str();
    }
    {
      std::ofstream outfile (pathpfx + "client_reflection_impl.hpp");
      outfile << ClientReflection_h_preamble.str();
      outfile << ClientReflection_h_fwddecls.str();
      outfile << ClientReflection_h_aliases.str();
      outfile << ClientReflection_h_impldefs.str();
      outfile << ClientReflection_h_close_cppxmeta.str();
      outfile << ClientReflection_h_std_tuple_specs.str();
      outfile << ClientReflection_h_finalendif.str();
    }


    // If you don't see this after the program runs, it means clang crashed somewhere
    // without emitting any diagnostic, which almost always means a null pointer
    // access, it seems.
    std::cout << ".......FINISHED generating reflection source files\n";
  }

  /// In this constructor we initialize the classsinfomap to contain all the
  /// leaf nodes -- Decl most importantly, but also Type and Stmt/Expr
  ReflGenASTConsumer()
      : clang_classnames_to_include({
#define DECL(DERIVED, BASE) \
          std::string(#DERIVED).append("Decl"),
#define ABSTRACT_DECL(DECL)
#include "clang/AST/DeclNodes.inc"
#define TYPE(DERIVED, BASE) \
          std::string(#DERIVED).append("Type"),
#define ABSTRACT_TYPE(DERIVED, BASE)
#include "clang/AST/TypeNodes.def"
#define STMT(DERIVED, BASE) std::string(#DERIVED),
#define ABSTRACT_STMT(type)
#include "clang/AST/StmtNodes.inc"
//#define REFL_CONTAINER(type) std::string(#type),
//#include "clang/Basic/ClientReflContainerNodes.inc"
                     })
  {
//    std::cout << "[DEBUG] DEFAULT REFLECTION-INCLUDED CLASS NAME SET:\n";
//    for (auto kv : clang_classnames_to_include) {
//      std::cout << "  " << kv << ",\n";
//    }
//    std::cout << "[END NAME SET]\n\n";
  }
};



class ReflectionSrcGenerator : public clang::ASTFrontendAction {

public:
  /// Boilerplate:
  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
          clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    return std::unique_ptr<clang::ASTConsumer>(new ReflGenASTConsumer());
  }
};





// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("my-tool options"); //DWR TODO

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...\n"); //DWR TODO

int main(int argc, const char **argv) {



  // For simplicity, we're going to IGNORE any command line inputs for now, and just
  // pipe in the appropriate clang files.
  // If we want to generalize the function to set up reflection sources for non-clang
  // compilers, we'd make use of the command line inputs instead of the ersatz ones here.
  int argc_ = 5;
  const char **argv_ = new const char*[5];
  std::vector<std::string> Sources;
  Sources.push_back(GENERATED_FILES_DIR "temp/ReflectionIncludesBig.hpp");
  argv_[0] = argv[0];
  argv_[1] = &Sources[0][0];
  argv_[2] = "--";
  argv_[3] = "c++";
  argv_[4] = "-Wno-everything"; //We don't need warnings, we're compiling the same stuff elsewhere
  CommonOptionsParser OptionsParser(argc_, argv_, MyToolCategory);
  //  CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);

//  std::vector<std::string> Sources;
//  Sources.push_back("tools/clang/include/clang/AST/ReflectionIncludes.h");
//
//// We hand the CompilationDatabase we created and the sources to run over into
//// the tool constructor.
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList()
//                 Sources
                 );


  if (int error = Tool.run(newFrontendActionFactory<ReflectionSrcGenerator>().get()))
    return error;


}



//TODO move all of this stuff out of line






/// Need to use this instead of ND->getQualifiedNameAsString() for client
/// types, because of how we are inlining CTSD bases now.
/// Has gotten much more complicated that it was originally :(, very hacky.
/// Probably should have just included CTSDs, and renamed them in this function
/// by chanigng < and > and , into legit characters.
static std::string getClientQualdName(NamedDecl *ND, ReflGenASTConsumer &mainobj) {
  std::string prefix;
  DeclContext *parent = ND->getDeclContext();
  if (parent->isTranslationUnit())
    prefix = "";
  else if (auto NND = dyn_cast<NamedDecl>(parent)) {
    // If the parent is a CTSD, replace it with the non-CTSD RD we have put its decls into:
    if (auto CTSD = dyn_cast<ClassTemplateSpecializationDecl>(NND)) {
      assert(mainobj.included_CTSDs.count(CTSD) && "should have included this");
      NND = mainobj.included_CTSDs[CTSD];
      assert(NND);
    }
    prefix = getClientQualdName(NND, mainobj);
  }
  std::string suffix = ND->getNameAsString();
  std::string sep = (!prefix.empty() && !suffix.empty()) ? "::" : "";
  return prefix + sep + suffix;
}



/// getRKname: takes a qualified name, converts the :: etc. to give a single-token name
static std::string getRKname(CXXRecordDecl *RD, ReflGenASTConsumer &mainobj) {
  return ReplaceAll(getClientQualdName(RD, mainobj), ":", "_");
}







// HELPERS, ReflectionCTDLookupCases.inc:

static void generate_lookup_enclscope(
        DeclContext *curlookup,
        std::string rootstr,
        stream_t &fstream) {
  assert(curlookup);
  fstream << "LookupDC(S, Loc, ";
  auto prnt = curlookup->getParent();
  if (!prnt->isTranslationUnit())
    generate_lookup_enclscope(prnt, rootstr, fstream);
  else
    fstream << rootstr;

  auto ND = dyn_cast<NamedDecl>(Decl::castFromDeclContext(curlookup));
  assert(ND);
  fstream << ", \"" << ND->getNameAsString() << "\")";
}
static void generate_CTDLookupCase(CXXRecordDecl *RD,
        std::string RKname, stream_t &fstream) {
  fstream << "    case RK_" << RKname << ":\n"
          << "      return LookupCTD(S, Loc, ";
  generate_lookup_enclscope(RD, "ReflNs", fstream);
  fstream <<            ", \"impl\");\n";
}


static void generate_ClientReflectionRKTypeConversionsSpec(CXXRecordDecl *RD,
          std::string RKname, stream_t &fstream, std::string clientqualdname) {
    fstream << "    template<> struct GetReflectionObjKind<"
            << clientqualdname << ">\n"
            << "    { static constexpr reflenums::ReflectionObjKind value = reflenums::RK_"
            << RKname << "; };\n";
//    fstream << "    template<intptr_t... Xs> struct GetReflTypeFromInts<reflenums::RK_" << RKname << ", Xs...>\n"
//            << "    { using type = " << RD->getQualifiedNameAsString() << "::impl<Xs...>; };\n";
}


// DWR FIXME: this is where our CTSD inlining could get problematic.
// If two or more derived classes inherit the same CTSD specialization,
// and that CTSD contains member classes, then we will have MULTIPLE
// mappings from types to RKs here, and will get an error.
// The temp hack solution: we only define a GetTypeFromRK specialization
// when we're sure no conflict exist, which is when the qualified names
// for the client and clang are the same.
// The long term lesson: this whole system is a mess, you really need to just
// reflect templates direclty or give dummy names to specializations.
static void generate_ReflectionRKTypeConversionsSpec(CXXRecordDecl *RD,
        std::string RKname, stream_t &fstream, std::string clientqualdname) {
  fstream << "template<> struct GetReflectionObjKind<"
          << RD->getQualifiedNameAsString()/*okay*/ << ">\n"
          << "{ static constexpr ReflectionObjKind value = RK_"
          << RKname << "; };\n";
  if (RD->getQualifiedNameAsString() == clientqualdname) {//HACK, see explanation above
    fstream << "template<> struct GetTypeFromRK<RK_" << RKname << ">\n"
            << "{ using type = " << RD->getQualifiedNameAsString()/*okay*/ << "; };\n";
  }
}



//static void generate_ReflectorCallOperator_PointerCase(CXXRecordDecl *RD,
//                                       std::string RKname, stream_t &fstream) {
//fstream << "    case RK_" << RKname << ":\n"
//        << "      return ReflectProp( MemNum, reinterpret_cast<"
//        << RD->getQualifiedNameAsString()
//        << " *>(Val) );\n";
//}

//static void generate_ReflectorCallOperator_NonPointerCase(CXXRecordDecl *RD,
//                                                      std::string RKname, stream_t &fstream) {
//  fstream << "    case RK_" << RKname << ":\n"
//          << "      return ReflectProp( MemNum, get_next_as<"
//          << RD->getQualifiedNameAsString()
//          << ">() );\n";
//}
//static void generate_ReflectorCallOperator_PointerCase(CXXRecordDecl *RD,
//                                       std::string RKname, stream_t &fstream) {
//fstream << "    case RK_" << RKname << ":\n"
//        << "      return ReflectProp( MemNum, reinterpret_cast<"
//        << RD->getQualifiedNameAsString()
//        << " *>(Val) );\n";
//}

//static void generate_ReflectorCallOperator_NonPointerCase(CXXRecordDecl *RD,
//                                                      std::string RKname, stream_t &fstream) {
//  fstream << "    case RK_" << RKname << ":\n"
//          << "      return ReflectProp( MemNum, get_next_as<"
//          << RD->getQualifiedNameAsString()
//          << ">() );\n";
//}

/*
 Okay lemme think.

 */



static void generate_ReflKindGetMaxMemNumCase(CXXRecordDecl *RD, std::string RKname, stream_t &fstream) {
  fstream << "case RK_" << RKname << ":\n"
          << "  return refl::" << RKname << "::_total_num_mems_;\n";
}
static void generate_ReflKindToStringCase(CXXRecordDecl *RD, std::string RKname, stream_t &fstream) {
  fstream << "case RK_" << RKname << ":\n"
          << "  return \"RK_" << RKname << "\";\n";
}


void generate_ReflInfoNamespaces_declsloop(CXXRecordDecl *RD, stream_t &fstream, ReflGenASTConsumer &mainobj,
                                           std::map<std::string, unsigned char> &memnames_to_curovldID,
                                           std::string &memname) {
  for (auto d : RD->decls()) {
    if (NamedDecl *ND = mainobj.is_included_field_or_method(d)) {
      memname = getFixedDeclName(ND->getDeclName());
      auto &ovldID = memnames_to_curovldID[memname];
      if (ovldID)
        memname += std::to_string(ovldID); //add numeric suffix for subsequent overloads
      ++ovldID;
      fstream << "    " << memname << ",\n";
    }
  }
}

/// Helps us load a template instantiation's decls into RD, i.e. "inline" it, since right now we
/// don't support template specializations directly
void generate_ReflInfoNamespaces_CTSDbasehlpr(CXXRecordDecl *RD, stream_t &fstream, ReflGenASTConsumer &mainobj,
                                              std::map<std::string, unsigned char> &memnames_to_curovldID,
                                              std::string &memname) {
  for (auto b : RD->bases()) {
    auto typeptr = b.getType().getCanonicalType().getTypePtr();
    assert(typeptr);
    CXXRecordDecl *thebaseclass = typeptr->getAsCXXRecordDecl();
    assert(thebaseclass);
    if (auto CTSD = dyn_cast<ClassTemplateSpecializationDecl>(thebaseclass)) {
      if (mainobj.included_CTSDs.count(CTSD)) {
        //Handle ITS CTSD bases recursively:
        generate_ReflInfoNamespaces_CTSDbasehlpr(thebaseclass, fstream, mainobj, memnames_to_curovldID, memname);
        //Then handle its decls:
        generate_ReflInfoNamespaces_declsloop(thebaseclass, fstream, mainobj, memnames_to_curovldID, memname);
      }
    }
  }
}



void generate_ReflInfoNamespaces(CXXRecordDecl *RD, std::string RKname, stream_t &fstream, ReflGenASTConsumer &mainobj) {

  fstream << "namespace " << RKname << " {\n";
    fstream << "  enum memnames {\n";
    if (mainobj.range_classes.count(RD))
      fstream << "    _this_,\n"; //reflects back the calling object -- used for the __reflect_range_X implems

    std::map<std::string, unsigned char> memnames_to_curovldID;
    std::string memname;

    //Handle ITS CTSD bases recursively:
    generate_ReflInfoNamespaces_CTSDbasehlpr(RD, fstream, mainobj, memnames_to_curovldID, memname);
    //Then handle its decls:
    generate_ReflInfoNamespaces_declsloop(RD, fstream, mainobj, memnames_to_curovldID, memname);

    fstream << "  };\n";
    fstream << "  static const unsigned _total_num_mems_ = "
            << (memname.empty() ? /*"_first_mem_num_;\n"*/ "0;\n" : (memname + " + 1;\n"));
    fstream << "}\n";
}

static bool isDeclTypeorStmt(CXXRecordDecl *RD) {
  assert(RD);
    std::string qualdname = RD->getQualifiedNameAsString();
    if (  qualdname == "clang::Decl"
       || qualdname == "clang::DeclContext"
       || qualdname == "clang::Type"
       || qualdname == "clang::Stmt"
       )
      return true;
  for (auto b : RD->bases()) {
    auto typeptr = b.getType().getCanonicalType().getTypePtr();
    assert(typeptr);
    CXXRecordDecl *thebaseclass = typeptr->getAsCXXRecordDecl();
    assert(thebaseclass);
    if (isDeclTypeorStmt(thebaseclass))
      return true;
  }
  return false;
}


static void generate_GetReflQTorCTD_case_declsloop(CXXRecordDecl *RD, std::string RKname, stream_t &fstream, ReflGenASTConsumer &mainobj,
                                                   std::map<std::string, unsigned char> &memnames_to_curovldID) {
  std::string memname, typestr;
  for (auto d : RD->decls()) {
    if (NamedDecl *ND = mainobj.is_included_field_or_method(d)) {
      if (auto field = dyn_cast<FieldDecl>(ND)) {
        memname = field->getDeclName().getAsString();
        typestr = getFixedTypeName(field->getType());
      }
      else if (auto func = dyn_cast<FunctionDecl>(ND)) {
        memname = getFixedDeclName(ND->getDeclName());
        auto &ovldID = memnames_to_curovldID[memname];
        if (ovldID)
          memname += std::to_string(ovldID);
        ++ovldID;

        typestr = getFixedTypeName(func->getReturnType());
      }
    } //is an included field/func decl

    fstream << "    case reflenums::" << RKname << "::" << memname << ":\n"
            << "      return toClientReflType<" << typestr << ">(C);\n";

  } //loop over member decls
}
static void generate_GetReflQTorCTD_case_CTSDbasehlpr(CXXRecordDecl *RD, std::string RKname, stream_t &fstream, ReflGenASTConsumer &mainobj,
                                                      std::map<std::string, unsigned char> &memnames_to_curovldID) {
  for (auto b : RD->bases()) {
    auto typeptr = b.getType().getCanonicalType().getTypePtr();
    assert(typeptr);
    CXXRecordDecl *thebaseclass = typeptr->getAsCXXRecordDecl();
    assert(thebaseclass);
    if (auto CTSD = dyn_cast<ClassTemplateSpecializationDecl>(thebaseclass)) {
      if (mainobj.included_CTSDs.count(CTSD)) {
        //1. Process any CTSD bases:
        generate_GetReflQTorCTD_case_CTSDbasehlpr(thebaseclass, RKname, fstream, mainobj, memnames_to_curovldID);
        //2. Process the current decls:
        generate_GetReflQTorCTD_case_declsloop(thebaseclass, RKname, fstream, mainobj, memnames_to_curovldID);
      }
    }
  }
}

static void generate_GetReflQTorCTD_case(CXXRecordDecl *RD, std::string RKname, stream_t &fstream, ReflGenASTConsumer &mainobj) {

  fstream << "case reflenums::RK_" << RKname << ":\n"
          << "  switch (static_cast<reflenums::" << RKname << "::memnames>(MemNum)) {\n";

  if (mainobj.range_classes.count(RD)) {
    fstream << "    case reflenums::" << RKname << "::_this_:\n"
            << "      if (IsPtr)\n"
            << "        return toClientReflType<" << RD->getQualifiedNameAsString() << " *>(C);\n"
            << "      return toClientReflType<" << RD->getQualifiedNameAsString() << ">(C);\n";
  }

  std::map<std::string, unsigned char> memnames_to_curovldID;

  //1. Process any CTSD bases:
  generate_GetReflQTorCTD_case_CTSDbasehlpr(RD, RKname, fstream, mainobj, memnames_to_curovldID);
  //2. Process the current decls:
  generate_GetReflQTorCTD_case_declsloop(RD, RKname, fstream, mainobj, memnames_to_curovldID);

  fstream << "    default:\n"
             "      llvm_unreachable(\"Unhandled mem\");\n"
             "  }\n";

}








static void generate_refltrait_mem_lambda_defs_declsloop(std::string ind, CXXRecordDecl *RD, std::string RKname, stream_t &fstream, ReflGenASTConsumer &mainobj, std::map<std::string, unsigned char> &memnames_to_curovldID ) {
  std::string memname;
  std::string VPSstr;
  std::string argstr;

  for (auto d : RD->decls()) {
    bool isFriendDecl = isa<FriendDecl>(d);
    if (NamedDecl *ND = mainobj.is_included_field_or_method(d)) {
      if (auto field = dyn_cast<FieldDecl>(ND)) {
        //////////////////////////////
        // Handle __reflect_prop traits for fields
        fstream << ind << "  case_REFLPROP_0ARGS("
                       << field->getDeclName().getAsString() //ID str
                       << ( reflectionTemplateNeeded(field->getType())
                            ? ", Type, " : (willConvertToStrLit(field->getType()) ? ", Str, " : ", Result, ") )
                       << "X->" << field->getDeclName().getAsString() << ")\n";
        continue;
      }
      else if (auto func = dyn_cast<FunctionDecl>(ND)) {
//        auto method = dyn_cast<CXXMethodDecl>(func);
//        VPSstr = (method && method->isStatic() ? "S" : VPstr);
        std::string methodname = ND->getDeclName().getAsString();
        std::string memname = getFixedDeclName(ND->getDeclName());

        auto &ovldID = memnames_to_curovldID[methodname];
        if (ovldID)
          memname += std::to_string(ovldID);
        ++ovldID;

        unsigned minnumextraargs = func->getMinRequiredArguments();
        unsigned maxnumextraargs = func->getNumParams();
        assert(minnumextraargs <= maxnumextraargs);

        bool isVoidTyped = func->getReturnType().getCanonicalType()->isVoidType();
        bool returns_range = mainobj.range_returning_funcs.count(func);

        QualType thetype;
        bool IsConvToStdString = false;

        if (returns_range)
          thetype = mainobj.range_returning_funcs[func];
        else {
          thetype = func->getReturnType();
          IsConvToStdString = isa<CXXConversionDecl>(func)
              && func->getReturnType().getAsString() == "std::string";
        }

        assert(!thetype.isNull());
        bool reflTempNeeded = reflectionTemplateNeeded(thetype);
        bool IsStringTy = willConvertToStrLit(thetype);


        std::string ResultOrTypeOrStr = IsStringTy ? "Str" : (reflTempNeeded ? "Type" : "Result");

#ifndef NDEBUG
        if (!IsStringTy) {
          if (thetype.getCanonicalType().getAsString() == "llvm::StringRef")
            llvm_unreachable("Didn't pick up llvm::StringRef");
          if (thetype.getCanonicalType().getAsString() == "class llvm::StringRef")
            llvm_unreachable("Didn't pick up class llvm::StringRef");
          if (thetype.getAsString() == "llvm::StringRef")
            llvm_unreachable("Didn't pick up llvm::StringRef, non-canon");
          if (thetype.getAsString() == "class llvm::StringRef")
            llvm_unreachable("Didn't pick up class llvm::StringRef, non-canon");
        }


#endif

        std::string BuildReflCallPfx = isVoidTyped ? "VOID_REFLECTION("
                                                   : "BUILD_REFLECTION_CALL_NORMAL(" + ResultOrTypeOrStr + ", ";

//        std::string MacroPfx =
//                ( isVoidTyped ? "VOID"
//                  : ( returns_range ? "RANGE"
//                      : ( reflectionTemplateNeeded(func->getReturnType())
//                          ? "CLASS" : "PRIM" ) ) );


        std::string fixedRetTypeName = getFixedTypeName(func->getReturnType());

        //////////////////////////////
        // Handle __reflect_prop traits for binary operator mems.
        // They should always be declared as friend decls
        // to get picked up properly here.

        if (isFriendDecl) {
          assert(memname.substr(0, 8) == "operator"
                 && "Expected only operators to be declared friends");

          std::string binopsym = ReplaceAll(methodname, "operator", "");

          assert(minnumextraargs == 2);
          assert(maxnumextraargs == 2);
          assert(!isVoidTyped);

          std::string type0str = getFixedTypeName(func->parameters()[0]->getType());
          std::string type1str = getFixedTypeName(func->parameters()[1]->getType());

          fstream << ind << "  case currefl::" << memname << ": {\n"
                  << ind << "    ASSERT_TRAIT_IS_REFLPROP(" << memname << ")\n";
          if (!reflTempNeeded
//              && !IsStringTy
              ) {
//            fstream
//                  << ind << "    SET_PRIM_TYPE(decltype( DV(" << type0str << ") "
//                                        << binopsym << " DV(" << type1str << ") ))\n";
            fstream
                  << ind << "    SET_PRIM_TYPE(" << fixedRetTypeName << ")\n";
          }
          fstream << ind << "    SET_CALLBACK(" << ResultOrTypeOrStr << ") {\n"
                  << ind << "      LOAD_VAL((" << type0str << "), p0 )\n"
                  << ind << "      LOAD_VAL((" << type1str << "), p1 )\n"
                  << ind << "      ASSERT_NO_MORE\n"
                  << ind << "      " << BuildReflCallPfx << "p0 " << binopsym << " p1)\n"
                  << ind << "    };\n"
                  << ind << "    break;\n"
                  << ind << "  }\n";
          continue;
        }

        //////////////////////////////
        // Handle __reflect_prop traits for nullary methods, excepting void-returning
        // methods and range-returning methods:
        if (maxnumextraargs == 0 && !isVoidTyped && !returns_range) {
          fstream << ind << "  case_REFLPROP_0ARGS( "
                         << memname //ID str
                         << ", " << ResultOrTypeOrStr << ", "
                         << "X->"
                         << (IsConvToStdString ? "operator std::string" : methodname) //HACK
                         << "() )\n";
          continue;
        }

        //////////////////////////////
        // Handle non-nullary __reflect_prop traits AND (nullary) __reflect_range_X traits.
        // These are more difficult: can't use a helper macro, plus some of the args
        // may be optional.
        // First we'll load the main reflection content into a couple helper streams, then
        // add those to fstream bolstered by whatever other stuff is necessary
        // for the given TraitKind.

        fstream << ind << "  case currefl::" << memname << ": {\n";

        std::stringstream tempstream;
        std::stringstream decltypestream;

        decltypestream << "X->" << methodname << "(";

        if (minnumextraargs == 0) { //i.e. if all args are optional
          tempstream << ind << "      "
                            << (maxnumextraargs ? "IF_NO_MORE  "
                                                : "ASSERT_NO_MORE  ")
                            << BuildReflCallPfx
                                    << "X->" << methodname << "())\n";
        }

        argstr = "";
        unsigned parmidx = 0;

        auto params = func->parameters();
        for (auto param : params) {
          // Keep this in sync with however you determined which params to include
          // originally, by using a common parm_included function:
          if (!mainobj.parm_included(param))
            break;

          std::string parmtypestr = getFixedTypeName(param->getType());

          decltypestream << (parmidx ? ", " : "") << "DV(" << parmtypestr << ")";

          tempstream << ind << "      LOAD_VAL((" << parmtypestr << "), p" << parmidx << ")\n";

          argstr += (argstr.empty() ? "" : ", ")
                    + (isVoidTyped ? "(" + justBasicTypeNameFixes(param->getType()) + ")" : "")
                    + "p" + std::to_string(parmidx);
          if (++parmidx >= minnumextraargs) {
            if (parmidx == params.size())
              tempstream << ind << "      ASSERT_NO_MORE\n"
                         << ind << "      ";
            else
              tempstream << ind << "      IF_NO_MORE  ";

            tempstream << BuildReflCallPfx
                       << "X->" << methodname << "(" << argstr << "))\n";

          } //if enough params supplied so far to return even if no more are supplied
        } //loop over params

        decltypestream << ")";


        // If this returns a range, determine if this is the range_nth
        // trait or the range_size trait; if the former, load the
        // first argument as an n.
        // TODO not sure this is the best way to implement this.
        if (returns_range) {
          fstream << ind << "    switch (TraitKind) {\n"
                  << ind << "      case RTK_range_nth: {\n";

          if (!reflTempNeeded
//              && !IsStringTy
              ) {
            fstream
                  << ind << "        SET_PRIM_TYPE(std::remove_reference<decltype(\n"
                  << ind << "          *" << decltypestream.str() << ".begin() )>::type)\n";
          }
          fstream << ind << "        SET_CALLBACK(" << ResultOrTypeOrStr << ") {\n"
                  << ind << "          LOAD_VAL_X(" << ResultOrTypeOrStr << ")\n"
                  << ind << "          LOAD_VAL((std::size_t), N)\n"
                         << "    " <<  ReplaceAll(tempstream.str(), "_NORMAL", "_RANGE_NTH") //hacky
                  << ind << "        };\n"
                  << ind << "        break;\n"
                  << ind << "      }\n"
                  << ind << "      case RTK_range_size:\n"
                  << ind << "        SET_PRIM_TYPE(std::size_t)\n"
                  << ind << "        SET_CALLBACK(Result) {\n"
                  << ind << "          LOAD_VAL_X(Result)\n"
                         << "    " <<  ReplaceAll(ReplaceAll(ReplaceAll(tempstream.str(),
                                                  "_NORMAL(Type, ", "_RANGE_SIZE("),
                                                  "_NORMAL(Result, ", "_RANGE_SIZE("),
                                                  "_NORMAL(Str, ", "_RANGE_SIZE(")    //SO hacky
                  << ind << "        };\n"
                  << ind << "        break;\n"
                  << ind << "      default:\n"
                  << ind << "        Diag(KWLoc, diag::err_refl_not_supported)\n"
                  << ind << "               << (\"(iterator_range/ArrayRef-returning) Mem num \"\n"
                  << ind << "                  + std::to_string(currefl::" << memname << "))\n"
                  << ind << "                << to_string(TraitKind);\n"
                  << ind << "        return ExprError();\n"
                  << ind << "    }\n"
                  << ind << "    break;\n";

        } //if returns llvm::iterator_range<...>/llvm::ArrayRef<...>
        else { // just a normal reflection property:
          fstream << ind << "    ASSERT_TRAIT_IS_REFLPROP(" << memname << ")\n";
          if (!reflTempNeeded
//              && !IsStringTy
              ) {
            fstream
//                  << ind << "    SET_PRIM_TYPE(decltype( " << decltypestream.str() << " ))\n";
                  << ind << "    SET_PRIM_TYPE(" << fixedRetTypeName << ")\n";
          }
          fstream << ind << "    SET_CALLBACK(" << ResultOrTypeOrStr << ") {\n"
                  << ind << "      LOAD_VAL_X(" << ResultOrTypeOrStr << ")\n"
                         <<        tempstream.str()
                  << ind << "    };\n"
                  << ind << "    break;\n";
        }

        fstream << ind << "  }\n"; //end of "case currefl::(memname) {"

      } //if a method
    } //is an included field/func decl
  } //loop over member decls
}


static void generate_refltrait_mem_lambda_defs_CTSDbasehlpr(std::string ind, CXXRecordDecl *RD, std::string RKname, stream_t &fstream, ReflGenASTConsumer &mainobj, std::map<std::string, unsigned char> &memnames_to_curovldID ) {
  for (auto b : RD->bases()) {
    auto typeptr = b.getType().getCanonicalType().getTypePtr();
    assert(typeptr);
    CXXRecordDecl *thebaseclass = typeptr->getAsCXXRecordDecl();
    assert(thebaseclass);
    if (auto CTSD = dyn_cast<ClassTemplateSpecializationDecl>(thebaseclass)) {
      if (mainobj.included_CTSDs.count(CTSD)) {
        //First handle its CTSD bases recursively:
        generate_refltrait_mem_lambda_defs_CTSDbasehlpr(ind, thebaseclass, RKname, fstream, mainobj, memnames_to_curovldID);
        //Then handle its decls:
        generate_refltrait_mem_lambda_defs_declsloop(ind, thebaseclass, RKname, fstream, mainobj, memnames_to_curovldID);
      }
    }
  }
}


static void generate_refltrait_mem_lambda_defs(std::string ind, CXXRecordDecl *RD, std::string RKname, stream_t &fstream, ReflGenASTConsumer &mainobj) {

  auto rbinfo = mainobj.class_usage_map[RD];

//  assert(rbinfo.rbptr || rbinfo.rbvalue && "Expected at least rbptr OR rbvalue; check"
//                                           "inclusion rules and make sure propagation "
//                                           "of rbinfo to bases occurred.");

  fstream << "case RK_" << RKname << ": {\n";

  if (!rbinfo.rbptr && !rbinfo.rbvalue) {
    //I think this can happen if we included somethign as a parent scope for something else.
    // Better thing to do would be to omit the RK entirely.
    fstream << "  llvm_unreachable(\"Didn't expect RK_" << RKname << " to be used\");"
            << "}";
    return;
  }

  auto classname = RD->getQualifiedNameAsString();
  fstream << "# define CLASSNAME (" << classname << ")\n";

  bool Xisptr;
//  std::string cndlptrstr;
  std::string VPstr;
  std::string dotarrow;

//  fstream << ind << "auto X = get_next_as<" << RD->getQualifiedNameAsString() << " *>(Args);\n";


  fstream << "# define LOAD_VAL_X(MP_ResultOrTypeOrStr)  ";
  if (rbinfo.rbptr) {
    if (rbinfo.rbvalue)
      fstream << "LOADX_MAYBEPTR(" << RD->hasDefaultConstructor() << ", ";
    else
      fstream << "LOADX_ALWAYSPTR(";
    Xisptr = true;
//    cndlptrstr = " *";
//    VPstr = "P";
//    dotarrow = "->";
  } else /*if (rbinfo.rbvalue)*/ {
    fstream << "LOADX_NEVERPTR(";
    Xisptr = false;
//    cndlptrstr = (RD->hasSimpleCopyConstructor() ? " " : " const &");
//    VPstr = "V";
//    dotarrow = ".";
  }

  fstream << "CLASSNAME, MP_ResultOrTypeOrStr)\n";

  //////////////////////////////
  // Handle __reflect_cast trait:

  if (isDeclTypeorStmt(RD)) { //is pointer among clang:: Decl*/Type*/Stmt*/Expr* hierarchies
    std::string Xcaststr;
    if (classname == "clang::DeclContext")
      Xcaststr = "Decl::castFromDeclContext(X)";
    else
      Xcaststr = "X";
    fstream << ind << "HANDLE_RTK_CAST_TRAIT(" << Xcaststr << ")\n";
  } else {
    fstream << ind << "if (TraitKind == RTK_cast)\n"
            << ind << "  Diag(KWLoc, diag::err_reflcast_nonpointer);\n"; //Note this diag is outside a callback, so we can just use Sema::Diag.
  }
  fstream << ind << "namespace currefl = refl::" << RKname << ";\n";

  fstream << ind << "switch( (currefl::memnames)MemNum ) {\n";

  //////////////////////////////
  // Handle __reflect_range_X traits with memnum = currefl::_this_
  // (used when a class is a range, e.g. reflcontainers):

  auto range_class_it = mainobj.range_classes.find(RD);
  if (range_class_it != mainobj.range_classes.end()) {
    QualType iter_elem_t = range_class_it->second;
    bool reflTempNeeded = reflectionTemplateNeeded(iter_elem_t);
    bool IsStringTy = willConvertToStrLit(iter_elem_t);
    assert((iter_elem_t.getAsString() == "const char *" ? IsStringTy : true )
           && "const char * not being interpreted as a string type!");
    std::string ResultOrTypeOrStr = reflTempNeeded ? "Type" : (IsStringTy ? "Str" : "Result");
    fstream << ind << "  case currefl::_this_: {\n"
            << ind << "    switch (TraitKind) {\n"
            << ind << "      case RTK_range_nth:\n";
    if (!reflTempNeeded
//        && !IsStringTy
        ) {
      fstream
            << ind << "        SET_PRIM_TYPE(std::remove_reference<decltype(\n"
            << ind << "          *DV(PP_REMOVE_PARENS(CLASSNAME)).begin() )>::type)\n";
    }
    fstream << ind << "        SET_CALLBACK(" << ResultOrTypeOrStr << ") {\n"
            << ind << "          LOAD_VAL_X(" << ResultOrTypeOrStr << ")\n"
            << ind << "          LOAD_VAL((std::size_t), N)\n"
            << ind << "          ASSERT_NO_MORE\n"
            << ind << "          BUILD_REFLECTION_CALL_RANGE_NTH(" << ResultOrTypeOrStr << ", *X)\n"
            << ind << "        };\n"
            << ind << "        break;\n";
    fstream << ind << "      case RTK_range_size:\n"
            << ind << "        SET_PRIM_TYPE(std::size_t)\n"
            << ind << "        SET_CALLBACK(Result) {\n"
            << ind << "          LOAD_VAL_X(Result)\n"
            << ind << "          ASSERT_NO_MORE\n"
            << ind << "          BUILD_REFLECTION_CALL_RANGE_SIZE(*X)\n"
            << ind << "        };\n"
            << ind << "        break;\n"
            << ind << "      default:\n"
            << ind << "        Diag(KWLoc, diag::err_refl_not_supported)\n"
            << ind << "               << \"_this_\"\n"
            << ind << "               << to_string(TraitKind);\n"
            << ind << "        return ExprError();\n"
            << ind << "    }\n"
            << ind << "    break;\n"
            << ind << "  }\n";
  }

  std::map<std::string, unsigned char> memnames_to_curovldID;
  // First load decls for any CTSD bases:
  generate_refltrait_mem_lambda_defs_CTSDbasehlpr(ind, RD, RKname, fstream, mainobj, memnames_to_curovldID);
  // THen load in the main decls:
  generate_refltrait_mem_lambda_defs_declsloop(ind, RD, RKname, fstream, mainobj, memnames_to_curovldID); //see above



  fstream << ind << "}\n"
          << "# undef CLASSNAME\n"
          << "# undef LOAD_VAL_X\n"
             "  break;\n"
             "}\n"
             "\n";

}


//static void generate_reflectprop_overload(bool decl0def1, bool val0ptr1, CXXRecordDecl *RD, std::string RKname, stream_t &fstream, ReflGenASTConsumer &mainobj) {

//  auto classname = RD->getQualifiedNameAsString();

//  std::string cndlptrstr = ( val0ptr1 ? " *"
//                             : ( RD->hasSimpleCopyConstructor() ? " "
//                                 : " const &") );


//  fstream << (decl0def1 ? "" : "  ") << "ExprResult "  << (decl0def1 ? "Reflector::" : "")
//          << "ReflectProp(unsigned N , "
//          << classname << cndlptrstr << "X)";
//  if (!decl0def1) {
//    fstream << ";\n";
//    return;
//  }

//  fstream << " {\n";

//  // Note that we should have already checked the
//  // N, i.e. the MemNum/target ReflectionObjKind in
//  // ActOnReflectionTrait, so we needn't worry about
//  // bad conversions to the respective enums.



//  if (val0ptr1 && isDeclTypeorStmt(RD)) { //is pointer among clang:: Decl*/Type*/Stmt*/Expr* hierarchies
//    std::string Xstr;
//    if (RD->getQualifiedNameAsString() == "clang::DeclContext")
//      Xstr = "Decl::castFromDeclContext(X)";
//    else
//      Xstr = "X";
//    fstream << "  if (TraitKind == RTK_cast) {\n"
//               "    LOAD_VAL(bool, dyn)\n"
//               "    return ReflectCast( (ReflectionObjKind)N, " << Xstr << ", dyn );\n"
//               "  }\n";
//  } else {
//    fstream << "  if (TraitKind == RTK_cast)\n"
//               "    MAKE_DIAG(diag::err_reflcast_nonpointer);\n";
//  }
//  fstream << "  namespace currefl = refl::" << RKname << ";\n";
//  fstream << "# define CLASSNAME " << classname << "\n";



//  fstream << "  switch( (currefl::memnames)MemNum ) {\n";

//  std::string VPstr = (val0ptr1 ? "P" : "V");

//  // Add the _this_ processing case for classes that are ranges:
//  if (mainobj.range_classes.count(RD)) {
//    fstream << "    case currefl::_this_: {\n"
//               "      std::size_t N;\n"
//               "      switch (TraitKind) {\n"
//               "        case RTK_range_nth: {\n"
//               "          LOAD_VAL(std::size_t, M)\n"
//               "          N = M;\n"
//               "          break;\n"
//               "        }\n"
//               "        case RTK_range_size: break;\n"
//               "        default:\n"
//               "          Diag(KWLoc, diag::err_refl_not_supported) \n"
//               "                 << \"_this_\"\n"
//               "                 << \"__reflect_prop\";\n"
//               "          return ExprError();\n"
//               "      }\n"
//               "      IF_NO_MORE  CLASS_RANGE_REFLECTION(" << VPstr << ")\n"
//               "      TOO_MANY;\n"
//             "};\n";
//  }


//  std::string argstr;

//  std::map<std::string, unsigned char> memnames_to_curovldID;
//  std::string memname;
//  std::string VPSstr;
//  for (auto d : RD->decls()) {
//    bool isFriendDecl = isa<FriendDecl>(d);
//    if (NamedDecl *ND = mainobj.is_included_field_or_method(d)) {
//      if (auto field = dyn_cast<FieldDecl>(ND)) {
//        // Fields are super simple:
//        fstream << "    case_FIELD(" << ( reflectionTemplateNeeded(field->getType())
//                                          ? "CLASS, " : "PRIM, " )
//                << VPstr << ", " << field->getDeclName().getAsString() << ")\n";
//        continue;
//      }
//      else if (auto func = dyn_cast<FunctionDecl>(ND)) {
//        auto method = dyn_cast<CXXMethodDecl>(func);

//        VPSstr = (method && method->isStatic() ? "S" : VPstr);
//        std::string methodname = ND->getDeclName().getAsString();
//        std::string memname = getFixedDeclName(ND->getDeclName());


//        auto &ovldID = memnames_to_curovldID[methodname];
//        if (ovldID)
//          memname += std::to_string(ovldID);
//        ++ovldID;

//        unsigned minnumextraargs = func->getMinRequiredArguments();
//        unsigned maxnumextraargs = func->getNumParams();
//        assert(minnumextraargs <= maxnumextraargs);

//        bool isVoidTyped = func->getReturnType().getCanonicalType()->isVoidType();
//        bool returns_range = mainobj.range_returning_funcs.count(func);

//        std::string MacroPfx =
//                ( isVoidTyped ? "VOID"
//                  : ( returns_range ? "RANGE"
//                      : ( reflectionTemplateNeeded(func->getReturnType())
//                          ? "CLASS" : "PRIM" ) ) );

//        // Special case: binary operators.
//        // They should always be declared as friend decls to get picked up properly here.
//        if (isFriendDecl) {
//          assert(memname.substr(0, 8) == "operator"
//                 && "Expected only operators to be declared friends");

//          std::string binopsym = ReplaceAll(methodname, "operator", "");

//          assert(minnumextraargs == 2);
//          assert(maxnumextraargs == 2);
//          assert(!isVoidTyped);

//          fstream << "    case currefl::" << memname << ": {\n";
//          fstream << "      LOAD_VAL(" << getFixedTypeName(func->parameters()[0]->getType())
//                  << ", p0 )\n";
//          fstream << "      LOAD_VAL(" << getFixedTypeName(func->parameters()[1]->getType())
//                  << ", p1 )\n";
//          fstream << "      IF_NO_MORE  " << MacroPfx << "_REFLECTION_CALL(p0 " << binopsym << " p1)\n";
//          fstream << "      TOO_MANY;\n"
//                     "    }\n";
//          continue;
//        }

//        // The simple, common cases of nullary funcs, EXCEPTING range-returning
//        // functions, area handled by a macro:
//        if (maxnumextraargs == 0 && !returns_range) {
//          fstream << "    case_METHOD_NOARGS(" << memname << ", " << MacroPfx << ", "
//                  << VPSstr << ", " << methodname << ")\n";
//          continue;
//        }
//        // The more difficult case of a func that either is returns an iterator_range
//        // or accepts arguments, some of which may be optional:
//        // TODO TEST ME!!
//        fstream << "    case currefl::" << memname << ": {\n";

//        // If this returns a range, determine if this is the range_nth
//        // trait or the range_size trait; if the former, load the
//        // first argument as an n.
//        // TODO not sure this is the best way to implement this.
//        if (returns_range) {
//          fstream << "      std::size_t N;\n"
//                     "      switch (TraitKind) {\n"
//                     "        case RTK_range_nth: {\n"
//                     "          LOAD_VAL(std::size_t, M)\n"
//                     "          N = M;\n"
//                     "          break;\n"
//                     "        }\n"
//                     "        case RTK_range_size: break;\n"
//                     "        default:\n"
//                     "          Diag(KWLoc, diag::err_refl_not_supported) \n"
//                     "                 << (\"(iterator_range/ArrayRef-returning) Mem num \"\n"
//                     "                    + std::to_string(currefl::" << memname << "))\n"
//                     "                  << \"__reflect_prop\";\n"
//                     "          return ExprError();\n"
//                     "      }\n";

//        } //if returns llvm::iterator_range<...>/llvm::ArrayRef<...>
//        else {
//          fstream << "      ASSERT_TRAIT_IS_REFLPROP(" << memname << ")\n";
//        }



//        if (minnumextraargs == 0) { //i.e. if all args are optional
//          fstream << "      IF_NO_MORE  " << MacroPfx << "_REFLECTION_0(" << VPSstr << ", " << methodname << ")\n";
//        }
//        argstr = "";
//        unsigned parmidx = 0;

//        auto params = func->parameters();
//        for (auto param : params) {
//          // Keep this in sync with however you determined which params to include
//          // originally, by using a common parm_included function:
//          if (!mainobj.parm_included(param))
//            break;

//          fstream << "      LOAD_VAL(" << getFixedTypeName(param->getType())
//                  << ", p" << parmidx << ")\n";

//          argstr += (argstr.empty() ? "" : ", ")
//                    + (isVoidTyped ? "(" + justBasicTypeNameFixes(param->getType()) + ")" : "")
//                    + "p" + std::to_string(parmidx);
//          if (++parmidx >= minnumextraargs) {
//            fstream << "      IF_NO_MORE  " << MacroPfx << "_REFLECTION(" << VPSstr << ", " << methodname << ", " << argstr;

//            if (isVoidTyped) {
//              // Tack on any default args so that you get the right func signature
//              // for the VoidReflectionExpr.
//              // TODO should instead construct a SmallVector of default arg strings before the param loop, then
//              //  add it on and pop from the front as you go.  This is a bit inefficient.  But not used much so
//              //  okay.
//              for (auto remparmidx = parmidx; remparmidx != func->parameters().size(); ++remparmidx) {
//                assert(params[remparmidx]->hasDefaultArg());
//                fstream << ", (" << ReplaceAll(params[remparmidx]->getType().getAsString(), "_Bool", "bool")
//                        << ")(" << ExprToString(params[remparmidx]->getDefaultArg(), params[remparmidx]->getASTContext()) << ")";
//              }
//            }


//            fstream << ")\n";
//          }
//        }
//        fstream << "      TOO_MANY;\n"
//                   "    }\n";
//      }
//    } //is an included field/func decl

//  } //loop over member decls

//  fstream << "  }\n"
//             "  reflprop_unreachable();\n"
//             "# undef CLASSNAME\n"
//             "}\n";
//}



/// Note: this should match the structure of load_inclusions_for_bases_of. (Except for the inlined CTSDs.)
static void assemble_bases(CXXRecordDecl *RD, SmallVectorImpl<CXXRecordDecl *> &baseclassvec, SmallVectorImpl<int64_t> &offsets, ReflGenASTConsumer &mainobj, int64_t startingoffset = 0) {
  for (auto b : RD->bases()) {
    //Skip over non-public bases:
    if (b.getAccessSpecifier() != AS_public)
      continue;


    CXXRecordDecl *thebaseclass = b.getType().getCanonicalType().getTypePtr()->getAsCXXRecordDecl();
    assert(thebaseclass);
    assert(thebaseclass->getDefinition());

    const auto &Context = RD->getASTContext();
    const auto &DerivedLayout = Context.getASTRecordLayout(RD);

    // BaseOffset: this will be zero (I think) for the first base in any list of bases, but for
    // subsequent bases, it will be nonzero, and it is critical to account for this shift in the pointer value
    // for these subsequent bases in the client reflection header, i.e. to do the pointer arithmetic
    // manually for the template parameters, or else we will be looking at the wrong Xs... data whenever
    // we slice a derived to e.g. its second base.  I.e. bad pointer access will result.
    // The reason for the startingoffset: if we can't add a base for some reason, we will try adding
    // THAT thing's bases directly, i.e. bypassing it to get to its bases,
    // but we still need to account for the original offset.
//    llvm::outs() << "[DWR DEBUG] `--base: " << bc->getQualifiedNameAsString() << "\n";
    int64_t BaseOffset = startingoffset + (Context.toBits(DerivedLayout.getBaseClassOffset(thebaseclass)) / 8);
//    llvm::outs() << "[DWR DEBUG]    `--offset : " << BaseOffset << "\n";


    // If this base has been included, add it to the base class vec.  Otherwise, try to add ITS
    // bases to the base class vec.
    if (mainobj.included_decls.count(getRealCanonicalDecl(thebaseclass))) {
      baseclassvec.push_back(thebaseclass);
      offsets.push_back(BaseOffset);
    } else {
//      std::cout << "\n//!!!!DEBUG: base not included: " << thebaseclass->getNameAsString() << "!!!!//\n";
      assemble_bases(thebaseclass, baseclassvec, offsets,mainobj, BaseOffset);
    }
  }
}

/// We use this for fixing non-reflection type names in the client reflection header.
/// Does the usual converting std::strings to const char *s and _Bool to bool, then also
/// change anything else required specifically for the client header.
static std::string fixOrigTypeNonRefl(QualType qt) {
  return
//  ReplaceAll(
          getFixedTypeName(qt)
//          ,"enum ", "meta::")
          ;
}

static std::string fixOrigTypeReflStr(std::string origtypestr) {
  return ReplaceAll(ReplaceAll(ReplaceAll(ReplaceAll(ReplaceAll(origtypestr,
          "class llvm::StringRef", "const char *"),
          "llvm::StringRef", "const char *"),
          "const const", "const"),
          "class ", "typename meta::"),
          "struct ", "typename meta::");
}


static std::string getClientReflectionTypeStr(QualType qt, unsigned tparmpack_idx) {
  std::string tmplversion, prefix = "const ", suffix = "";
  qt = qt.getCanonicalType();
  std::string origtypestr = qt.getAsString();

  bool isptrorref = qt->isReferenceType();
  while(qt->isPointerType()) {
    isptrorref = true;
    suffix += " *";

    if (qt.isConstQualified())
      suffix += " const";
    qt = qt->getPointeeType();
  }

  qt = getUnderlyingType(qt);
  auto *TD = qt->getAsTagDecl();
  assert(TD && "Expected this to only be called for tag decl based QualTypes!");

  if (auto RD = dyn_cast<CXXRecordDecl>(TD)) {
    if (isa<ClassTemplateSpecializationDecl>(RD)) {
      qt.dump();
      RD->dump();
      llvm_unreachable("Didn't expect this kind of type.  I presume it was included via a base CTSD that was inline. "
                       "If so, the qualified name for this type will be wrong -- need to fix if you want to use.");
    }
  }

  tmplversion = prefix
      + TD->getQualifiedNameAsString()
      + "::template impl</*ptrorref=*/" + std::to_string(isptrorref)
      + ", Y" + std::to_string(tparmpack_idx) + "s...>"
      + suffix;

  return "IFMETA_ELSE((" + tmplversion + "), (" + fixOrigTypeReflStr(origtypestr) + "))";
}

//static std::string get_idehlpr_cast_prefix(QualType qt) {
//  std::string res = "idehlpr::cast";
//  while (qt->isPointerType()) {
//    res += "_p";
//    qt = qt->getPointeeType();
//  }
//
//  qt = getUnderlyingType(qt);
//
//  auto *TD = qt->getAsTagDecl();
//  assert(TD && "Expected this to only be called for tag decl based QualTypes!");
//
//  return res + "<" + TD->getQualifiedNameAsString() + "::impl>(";
//}


#ifndef DWR_PP_STRINGIZE
//   Helpers:
#   define DWR_PP_EMPTY()
#   define DWR_PP_DEFER(id) id DWR_PP_EMPTY()
#   define DWR_PP_EXPAND(...) __VA_ARGS__
#   define DWR_PP_STRINGIZE_SIMPLE(x) #x
/// This expands any macros in x and stringizes the result.
/// That is important because right now we're having some difficulty
/// parsing certain macros in __queue_metaparse statements --
/// this lets us get rid of them.
#  define DWR_PP_STRINGIZE(x) DWR_PP_EXPAND(DWR_PP_DEFER(DWR_PP_STRINGIZE_SIMPLE)(x))
#endif

//static size_t getAlignRelToDCBase(CXXRecordDecl *RD) {
//  std::string name = ReplaceAll(RD->getNameAsString(), "clang::","");
//  assert(name.find("::") == std::string::npos);
////  std::cout << "name: " << name << "\n";
//  intptr_t DCdummynum = 1000;
//  DeclContext *DCdummy = reinterpret_cast<DeclContext *>(DCdummynum);
//#define DECL(CLASS, BASE)
//#define DECL_CONTEXT(DECL)\
//  /*std::cout << "stringized possible match: " << DWR_PP_STRINGIZE(DECL ## Decl) << "\n";*/\
//  if (DWR_PP_STRINGIZE(DECL ## Decl) == name) {\
//    auto D = DECL ## Decl::castFromDeclContext(DCdummy);\
//    assert(DCdummynum >= (intptr_t)D && "switch them");\
//    /*std::cout << "`--MATCH found, alignment difference: " << std::to_string(DCdummynum - (intptr_t)D) << "\n";*/\
//    return DCdummynum - (intptr_t)D;\
//  }
//#include "clang/AST/DeclNodes.inc"
//  llvm_unreachable("Unhandled DeclContext derived case, or this"
//                   " was called on something that doesn't inherit DeclContext");
//  return 0;
//}


static void generate_client_impltmpl_declsloop(CXXRecordDecl *RD, std::string RKname, std::string curindentstr,
                                               std::string fwddecls_indentstr, stream_t &fstream, stream_t &fwd_decls_stream,
                                               stream_t &std_tuple_specs, ReflGenASTConsumer &mainobj,
                                               std::map<std::string, unsigned char> &memnames_to_curovldID,
                                               std::set<std::string> &ptrwrp_impl_sigs,
                                               CXXRecordDecl *TargRD/**the RD all the decls will go into, in case RD is a CTSD base*/) {

  const std::string reflectprop_initargstr_hlpr =
          "reflenums::RK_" + RKname + ", reflenums::" + RKname + "::";
  std::string reflectprop_initargstr;
  std::string hlprcast_pfx;
  std::string hlprcast_sfx;
  curindentstr += "  ";


  std::string memname;
  bool isFriendDecl;

  for (auto d : RD->decls()) {
    if (d->isImplicit())
      continue;

    // Bit of a hack: allow static const integral fields (e.g. StringRef::npos).
    if (auto vd = dyn_cast<VarDecl>(d)) { //VarDecl in a record = static field
      assert(vd->isStaticDataMember());
      QualType qt = vd->getType().getCanonicalType();
      if (qt.isConstQualified() && qt->isIntegerType() && vd->hasInit()
          && !qt->getAsTagDecl()/*i.e. no enums -- if you need those, load the inclusions above
                                 * before commenting this out*/
          ) {
        auto &C = vd->getASTContext();

        Expr::EvalResult Result;
        if (!vd->getInit()->EvaluateAsRValue(Result, C)) {
          vd->dump();
          llvm_unreachable("Couldn't evaluate static const integral variable initializer");
        }
        auto initstr = Result.Val.getAsString(C, qt);

        // Hack to avoid warning about a huge integer (e.g. evaluation of ~size_t(0)):
        // if its not negative, add a u at the end so its interpreted as unsigned right away.
        // (Would be better to add the proper suffix based on the signedness/size of qt, but
        // can't figure out how to do that easily, and this works for the few cases for which
        // this matters.)
        if (std::isdigit(initstr[0])/*i.e. no minus sign*/) {
          initstr.push_back('u');
        } else if (initstr[0] != '-' && initstr != "true" && initstr != "false") {
          vd->dump();
          std::cout << "// Make sure it was correct to interpret the following as an unsigned int:   " << initstr << "\n";
        }

        fstream << curindentstr << "static " << justBasicTypeNameFixes(qt) << " "
                << vd->getNameAsString() << " = " << initstr << ";\n";
      }
    }

    // If this is a friend decl for a function, set d to be that function decl
    // for subsequent testing, and mark that its a friend
    isFriendDecl = isa<FriendDecl>(d);
    if (isFriendDecl) {
      if (auto thefriend = get_friendfunc_if_should_include(cast<FriendDecl>(d))) {
        d = thefriend;
      } else
        continue;
    }

    if (mainobj.included_decls.count(d)) {

      if (NamedDecl *ND = mainobj.is_nonstatic_member(d)) {
        hlprcast_pfx = "";
        hlprcast_sfx = "";
        std::string memdoc = getdoc(ND);
        if (!memdoc.empty())
          fstream << curindentstr << ReplaceAll(memdoc, "  ///", curindentstr + "///") << "\n";

        std::string memname = getFixedDeclName(ND->getDeclName());
        auto &ovldID = memnames_to_curovldID[memname];
        if (ovldID)
          memname += std::to_string(ovldID); //add numeric suffix for subsequent overloads
        ++ovldID;

        reflectprop_initargstr = reflectprop_initargstr_hlpr + memname + ", Xs...";

        auto func = dyn_cast<FunctionDecl>(ND);
        auto method = dyn_cast<CXXMethodDecl>(ND);
        auto field = dyn_cast<FieldDecl>(ND);
        assert(func || field && "expected a field or func");
        if (func) {
          bool returns_range = mainobj.range_returning_funcs.count(func);
          std::string sig = curindentstr;
          if (isFriendDecl)
            sig += "friend ";
          if (method && method->isStatic())
            sig += "static ";
          sig += "constexpr ";
          if (auto co = dyn_cast<CXXConversionDecl>(func)) {
            if (co->isExplicit())
              sig += "explicit ";
          }
          //TODO any other quals to add?
          QualType retty = func->getReturnType();
          bool retty_refltemplneeded = reflectionTemplateNeeded(retty);
          std::string rettystr;
          if (!isa<CXXConversionDecl>(func)) {
            if (returns_range)
              sig += "auto ";
            else if (retty_refltemplneeded) {
              rettystr = fixOrigTypeReflStr(retty.getCanonicalType().getAsString());

              sig += "IFMETA_ELSE( (auto), (" + rettystr + ") )\n" + curindentstr; //new line
//            hlprcast_pfx = get_idehlpr_cast_prefix(retty);
//            hlprcast_sfx = ")";
            } else {
              rettystr = fixOrigTypeNonRefl(retty);
              sig += rettystr + " ";
            }
          } else {
            assert(!retty_refltemplneeded
                   && "Need to account for conversion operators to reflection template types, "
                      "or annotate out this function");
          }

          //NB: the signature uses the ordinary name without
          // any numeric overload id suffix
          sig += ReplaceAll(
                func->getDeclName().getAsString(),
                "operator basic_string", "operator const char *") //hack
              + "(";
          std::string call_ptrwrps = curindentstr + "  return " + func->getDeclName().getAsString() + "(";

          // Regarding the ptrwrp strings: they are only actually used when there is at least one
          // parameter whose type will be a "ptrwrp<T>" instance (i.e. a reflectionTemplateNeeded=true
          // type that is a pointer.)  We need these ptrwrp strings to implement an overload
          // of the function for

          unsigned numtparmpacks = 0;
          unsigned parmidx = 0;
          std::string parmname;
          std::string reflectprop_paramstr = "";
          std::string sig_ptrwrps = sig;

          bool has_ptrwrp_params = false;
          for (ParmVarDecl *param : func->parameters()) {
            // Keep this in sync with however you determined which params to include
            // originally, by using a common parm_included function:
            if (!mainobj.parm_included(param))
              break;

            QualType type = param->getType();
            bool refltemplneeded = reflectionTemplateNeeded(type);

//            type.addConst(); //Just in case -- really only effect is to keep the IDE
//            // from raising erroneous flags, since the objects we construct will
//            // usually be const.

            parmname = param->getNameAsString();
            std::string ptrwrp_pmname = "p" + std::to_string(parmidx);
            if (parmname.empty()) //in case no name was specified in sig
              parmname = ptrwrp_pmname;

            if (parmidx++) {
              sig += ", ";
              sig_ptrwrps += ", ";
              call_ptrwrps += ", ";
            }

            call_ptrwrps += ptrwrp_pmname;
            if (refltemplneeded) {
              if (type->isPointerType()) {
                sig_ptrwrps += "ptrwrp<";
                has_ptrwrp_params = true;
                call_ptrwrps += ".get()";
              }
              sig += getClientReflectionTypeStr(type, numtparmpacks);
              sig_ptrwrps += "Y" + std::to_string(numtparmpacks);
              ++numtparmpacks;
              if (type->isPointerType())
                sig_ptrwrps += ">";
            } else {
              std::string primtypestr = fixOrigTypeNonRefl(type);
              sig           += primtypestr;
              sig_ptrwrps   += primtypestr;
            }

            sig           += " " + parmname;
            sig_ptrwrps   += " " + ptrwrp_pmname;

            if (param->hasDefaultArg()) {
              if (refltemplneeded) {
                //HACK -- default args for reflection-templated types will be a tough nut to crack,
                // will have to alter the expressions properly etc.  Since they're usually just
                // default constructed this will be fine -- but in practice since you can't
                // default-arg the parameter pack, you'll have to supply an arg anyway.
                sig += " = {}";
                sig_ptrwrps += " = {}";
              } else {
                std::string dfltargstr = " = " + ReplaceAll(ExprToString(param->getDefaultArg(), param->getASTContext()), "llvm::StringRef()", "0"); //HACK
                sig += dfltargstr;
                sig_ptrwrps += dfltargstr;

                //// If you have initializers that reference static variables that we cannot/have not added,
                //// you may need to use the below formulation instead of the above:
//                Expr::EvalResult Result;
//                auto &C = param->getASTContext();
//                if (!param->getDefaultArg()->EvaluateAsRValue(Result, C)) {
//                  param->dump();
//                  llvm_unreachable("Couldn't evaluate default arg -- non integer type perhaps");
//                }
//                sig += " = " + Result.Val.getAsString(C, C.Int128Ty);
              }

            }

            reflectprop_paramstr += (refltemplneeded
                               ? (", Y" + std::to_string(numtparmpacks-1) + "s...")
                               : (", " + parmname) );
          }
          sig += ") ";
          sig_ptrwrps += ") ";
          call_ptrwrps += ");";
          if (method && method->isConst()) {
            sig += "const ";
            sig_ptrwrps += "const ";
          } else if (method && !method->isStatic()) {
            sig += "/*NON-CONST*/ ";
            sig_ptrwrps += "/*NON-CONST*/ ";
          }

          if (returns_range) {
            QualType elem_t = mainobj.range_returning_funcs[func];
            assert(!elem_t.isNull());
            fwd_decls_stream << fwddecls_indentstr << "DEF_RANGE_REFLECTION_TUPLE(" << memname << ",\n"
                             << "    "  << sig << ",\n"
                             << fwddecls_indentstr << "    (" << elem_t.getAsString() << "),\n"
                             << fwddecls_indentstr << "    (" << reflectprop_initargstr << "), ("
                             << reflectprop_paramstr << ") )\n";

            fstream << curindentstr << "RANGE_REFLECTION(" << TargRD->getQualifiedNameAsString() << ", " << memname << ",\n"
                                    << "    "  << sig << ",\n"
                    << curindentstr << "    (" << fixOrigTypeReflStr(elem_t.getAsString()) << "),\n"
                    << curindentstr << "    (" << reflectprop_initargstr << "), ("
                                               << reflectprop_paramstr << ") )\n";

            std::string tuplename = "cppx::meta::refldetail::"
                    + TargRD->getQualifiedNameAsString() + "::" + memname + "_tuple<Xs...>";

            std_tuple_specs << "  template<intptr_t... Xs>\n"
                               "  struct tuple_size<" << tuplename << ">\n"
                               "      : std::integral_constant<std::size_t, " << tuplename << "::_size_()>\n"
                               "  { };\n"
                               "  template<std::size_t I, intptr_t... Xs>\n"
                               "  struct tuple_element<I, " << tuplename << ">\n"
                               "  {\n"
                               "    using type = decltype(" << tuplename << "::template _get_<I>());\n"
                               "  };\n"
                               "\n";




          } else {

            // TODO: when you get around to setting up REAL templated methods etc.,
            // you should distinguish between real template parameters and the
            // additional packs, and do a real template opening when needed
            if (numtparmpacks) {
              fstream << curindentstr << "M_template M_tbeg ";
              for (unsigned i = 0; i != numtparmpacks; ++i) {
                if (i) fstream << " M_c ";
                fstream << "M_rtpack(Y" << i << "s)";
              }
              fstream << " M_tend\n";
            }

            //HACK: enums need proper casting...
            if (rettystr.find("enum ") != std::string::npos) {
              assert(hlprcast_pfx.empty() && "Overwriting existing cast pfx");
              hlprcast_pfx = "(" + rettystr + ")";
            }

            fstream << sig << "IFMETA_ELSE( ({  " << (retty->isVoidType() ? "  " : "  return ")
                    << hlprcast_pfx << "__reflect_prop(" << reflectprop_initargstr << reflectprop_paramstr
                    << hlprcast_sfx << "); }) ,"
                    << " (;) )\n";
//                    << " ({ return " << (retty_refltemplneeded && retty->isPointerType()
//                                         ? ("&DummyVars<" + fixOrigTypeNonRefl(retty->getPointeeType()) + ">")
//                                         : "{}") //^ To get rid of erroneous IDE flag due to nullptr returns
//                                     << "; }) )\n";
          }


          //Now, if you need the
          if (has_ptrwrp_params
              && ptrwrp_impl_sigs.insert(sig_ptrwrps).second //this particular sig_ptrwrps had not yet been added
              ) {
            fstream << curindentstr << "IFMETA_ELSE( (template<";
            for (unsigned i = 0; i != numtparmpacks; ++i) {
              if (i) fstream << ", ";
              fstream << "typename Y" << i;
            }
            fstream << ">\n"
                    << sig_ptrwrps << "{\n"
                    << curindentstr << call_ptrwrps << "\n"
                    << curindentstr << "}), () )\n";
          }
        } //if func
        else if (field) {
          QualType type = field->getType();

          // Special case: change 'unsigned somefield : 1' into 'bool somefield'.
          // Would be nice to just give the bit width but that would prevent
          // us from giving a default arg under earlier C++ versions I think.
          // At least with the bool the type is clearer.
          if (type->isIntegerType()) {
            if ( field->isBitField() &&
                 field->getBitWidthValue(field->getASTContext()) == 1 )
              type = field->getASTContext().BoolTy;
          }

          fstream << curindentstr;
          if (reflectionTemplateNeeded(type)) {
            fstream << "M_REFLTYPED_FIELD("
                    << field->getNameAsString() << ", "
                    << "(" << fixOrigTypeReflStr(type.getCanonicalType().getAsString()) << "), "
                    //                  << get_idehlpr_cast_prefix(type)
                    << "__reflect_prop(" << reflectprop_initargstr
                    //                  << ")"
                    << "))\n";
          } else {
            fstream << fixOrigTypeNonRefl(type) << " " << field->getNameAsString()
                    << "\n" << curindentstr << "    IFMETA_ELSE( (= "
                    << "__reflect_prop(" << reflectprop_initargstr << ");), "
                    << "(;) )\n"; //NB no reflection type needed, so no dummyvars needed
          }
        }
      } // end if nonstatic member (i.e. method or field)
      else if (auto nstdRD = dyn_cast<CXXRecordDecl>(d)) {
        fstream << "  M_template_rtpack(Zs) using "
                << nstdRD->getName().str() << " = struct refldetail::"
                << TargRD->getQualifiedNameAsString() << "::" << nstdRD->getName().str() << "::M_template impl M_targpack(Zs);\n";
//                << nstdRD->getQualifiedNameAsString() << "::M_template impl M_targpack(Zs);\n";
      } else if (auto nstdED = dyn_cast<EnumDecl>(d)) {
        fstream << "  using "
                << nstdED->getNameAsString() << " = enum refldetail::"
                << TargRD->getQualifiedNameAsString() << "::" << nstdED->getName().str() << ";\n";
//                << nstdED->getQualifiedNameAsString() << ";\n";
      }
    }
  } //for all decls
}

static void generate_client_impltmpl_CTSDbasehlpr(CXXRecordDecl *RD, std::string RKname, std::string curindentstr,
                                                  std::string fwddecls_indentstr, stream_t &fstream, stream_t &fwd_decls_stream,
                                                  stream_t &std_tuple_specs, ReflGenASTConsumer &mainobj,
                                                  std::map<std::string, unsigned char> &memnames_to_curovldID,
                                                  std::set<std::string> &ptrwrp_impl_sigs,
                                                  CXXRecordDecl *TargRD/**the RD all the decls will go into, in case RD is a CTSD base*/) {
  for (auto b : RD->bases()) {
      auto typeptr = b.getType().getCanonicalType().getTypePtr();
      assert(typeptr);
      CXXRecordDecl *thebaseclass = typeptr->getAsCXXRecordDecl();
      assert(thebaseclass);
      if (auto CTSD = dyn_cast<ClassTemplateSpecializationDecl>(thebaseclass)) {
        if (mainobj.included_CTSDs.count(CTSD)) {
          //First process any CTSD bases:
          generate_client_impltmpl_CTSDbasehlpr(thebaseclass, RKname, curindentstr, fwddecls_indentstr,
                                                fstream, fwd_decls_stream, std_tuple_specs, mainobj,
                                                memnames_to_curovldID, ptrwrp_impl_sigs, TargRD);
          //Then process the decls:
          generate_client_impltmpl_declsloop(thebaseclass, RKname, curindentstr, fwddecls_indentstr,
                                             fstream, fwd_decls_stream, std_tuple_specs, mainobj,
                                             memnames_to_curovldID, ptrwrp_impl_sigs, TargRD);
        }
      }
    }




}


static void generate_client_impltmpl(CXXRecordDecl *RD, std::string RKname, std::string curindentstr, std::string fwddecls_indentstr, stream_t &fstream, stream_t &fwd_decls_stream, stream_t &std_tuple_specs, ReflGenASTConsumer &mainobj) {
  fstream << "\n";
  std::string classdoc = getdoc(RD);
  if (!classdoc.empty())
    fstream << curindentstr << ReplaceAll(classdoc, "\n", "\n" + curindentstr) << "\n";

  std::string clientQualdName = getClientQualdName(RD, mainobj);
  fstream << curindentstr << "M_template_rtpack(Xs)\n"
          << curindentstr << "struct " << clientQualdName << "::impl";
  SmallVector<CXXRecordDecl *, 2> bases;
  SmallVector<int64_t, 2> offsets;
  assemble_bases(RD, bases, offsets, mainobj);
  bool first = true;
  bool hadDCbase = false;

  unsigned idx = 0;
  for (auto bc : bases) {
    if (first) {
      fstream << " : ";
      first = false;
    } else
      fstream << ", ";

    fstream << "impl_offset_t<" << getClientQualdName(bc, mainobj) << ", "
                           << offsets[idx++] << " M_c M_pack(Xs)>";
  }
  fstream << " {\n"
             "  static constexpr reflenums::ReflectionObjKind ReflObjKind = reflenums::ReflectionObjKind::RK_" << getRKname(RD, mainobj) << ";\n";

  // Conversion operator to a string literal constructor:
  fstream << "  constexpr explicit operator const char *() const IFMETA_ELSE( ({\n"
             "    return __concatenate(\"cppx::meta::refldetail::" << getClientQualdName(RD, mainobj) << "::impl<\",\n"
          << "                         concat_ints_w_commas<Xs...>::value, \">()\");\n"
          << "  }) , ( {return \"\";} ) )\n";

//  fstream << "  IFMETA_ELSE( (), (static constexpr impl DfltNonnullObj = {};) )\n";

  if (hadDCbase) {
//    assert(bases.size() > 1); //sanity check
    //HACK:  Decl and DeclContext share at least one named member: getDeclKindName; I'm not sure why this
    // doesn't cause ambiguous name lookup in clang but it sure does in the reflection library, so
    // we will resolve it here.  TODO should really search through the included decls of each for all
    // such conflicts, but for now any you get, add in using decls here:
    fstream << curindentstr << "  using clang::Decl::M_template impl M_targpack(Xs)::getDeclKindName;\n";
  }


  std::string origindentstr = curindentstr;

  std::map<std::string, unsigned char> memnames_to_curovldID;
  std::set<std::string> ptrwrp_impl_sigs;

  //First process any CTSD bases:
  generate_client_impltmpl_CTSDbasehlpr(RD, RKname, curindentstr, fwddecls_indentstr, fstream, fwd_decls_stream, std_tuple_specs, mainobj, memnames_to_curovldID, ptrwrp_impl_sigs, RD);
  //Then process the decls:
  generate_client_impltmpl_declsloop(RD, RKname, curindentstr, fwddecls_indentstr, fstream, fwd_decls_stream, std_tuple_specs, mainobj, memnames_to_curovldID, ptrwrp_impl_sigs, RD);

  //asdfkjas;dlkfjasldkfasl;dkjfal;skdjfals;kdjf HERE


  //If this is a container, add the macro to add the static size() and get<I>() functions,
  // plus set up the tuple_size/tuple_element specializations.
  // (DWR moved this to end to try to get the IDE to pick up any nstdRD aliases defined above
  // in decoding the element type.
  auto rangeclsmapres = mainobj.range_classes.find(RD);
  if (rangeclsmapres != mainobj.range_classes.end()) {
    fstream << curindentstr << "  RANGECLASS_SIZE_AND_GET(" << RKname << ", "
                            << /*ElemT=*/fixOrigTypeReflStr(rangeclsmapres->second.getAsString())
                            << ");\n";

    std::string tuplename = "cppx::meta::refldetail::"
            + getClientQualdName(RD, mainobj) + "::impl<Xs...>";

    std_tuple_specs << "  template<intptr_t... Xs>\n"
                       "  struct tuple_size<" << tuplename << ">\n"
                       "      : std::integral_constant<std::size_t, " << tuplename << "::_size_()>\n"
                       "  { };\n"
                       "  template<std::size_t I, intptr_t... Xs>\n"
                       "  struct tuple_element<I, " << tuplename << ">\n"
                       "  {\n"
                       "    using type = decltype(" << tuplename << "::template _get_<I>());\n"
                       "  };\n"
                       "\n";
  }

  fstream << origindentstr << "};\n";

}

void copy_enum_def_to(EnumDecl *ED, stream_t &fstream, std::string curindentstr, ReflGenASTConsumer &mainobj, bool useQualifiedName = true) {
  fstream << "\n";
  std::string enumdoc = getdoc(ED);
  if (!enumdoc.empty())
    fstream << curindentstr << ReplaceAll(enumdoc, "\n  ", "\n" + curindentstr) << "\n";
  fstream << curindentstr << "enum " << (ED->isScoped() ? "class " : "")
          << (useQualifiedName ? getClientQualdName(ED, mainobj) : ED->getNameAsString())
          << " : " << ED->getIntegerType().getAsString() << " {\n";

  curindentstr += "  ";

  std::string memdoc;
  for (auto e : ED->enumerators()) {
    memdoc = getdoc(e);
    if (!memdoc.empty())
      fstream << curindentstr << ReplaceAll(memdoc, "\n    ", "\n" + curindentstr) << "\n";
    fstream << curindentstr << e->getNameAsString();

    // Note we don't copy over the whole init expression, just the value,
    // to avoid name resolution issues.
    if (e->getInitExpr())
      fstream << " = " << e->getInitVal().toString(10);

    fstream << ",\n";
  }
  curindentstr.pop_back();
  curindentstr.pop_back();
  fstream << curindentstr << "};\n";
}





void ReflGenASTConsumer::generate_client_reflheader_preamble(stream_t &fstream)
{
  fstream << "//===---------------- client_reflection_impl.hpp ----------------===//\n"
             "//\n"
             "//    AUTO-GENERATED -- DO NOT EDIT THIS FILE DIRECTLY\n"
             "//\n"
             "//===----------------------------------------------===//\n"
             "#ifndef CPPX_META_client_reflection_impl_HPP\n"
             "#define CPPX_META_client_reflection_impl_HPP\n"
             "\n"
             "#include <cstdint> //for intptr_t\n"
             "#include <utility> //for std::move\n"
             "#include <cassert>\n"
             "\n"
             "#define PP_EXPAND(...) __VA_ARGS__\n"
             "#define PP_UNCNDL_REMOVE_PARENS(x) PP_EXPAND x\n"
             "\n"
             "#ifdef __CONSUMER_SUPPORTS_REFLECTION_AND_META__\n"
             "#   define M_template       template\n"
             "#   define M_tbeg           <\n"
             "#   define M_rtpack(Zs)     intptr_t... Zs\n"
             "#   define M_c              , \n"
             "#   define M_pack(Zs)       Zs...\n"
             "#   define M_tend           >\n"
             "#   define M_getPtrVal_T_value  refldetail::getPtrVal<T>::value\n"
             "#   define IFMETA_ELSE(MP_compiler_version, MP_idehlpr_version/*ignored*/)\\\n"
             "      PP_UNCNDL_REMOVE_PARENS(MP_compiler_version)\n"
             "    /// DWR HACK so we can get non-static fields (for accessing with . or ->\n"
             "    /// syntax, instead of :: syntax); we were getting errors using decltype\n"
             "    /// on the __reflect_prop/cast statements but this fixes it.\n"
             "#   define M_REFLTYPED_FIELD(MP_name, MP_idehlpr_t/*ignored*/, .../*expr*/)\\\n"
             "      private: static constexpr auto S_ ## MP_name = __VA_ARGS__;\\\n"
             "      public: decltype(S_ ## MP_name) MP_name = S_ ## MP_name;\n"
             "    /**/\n"
             "#   define RANGECLASS_SIZE_AND_GET(RKname, ElemT/*unused*/)\\\n"
             "            static constexpr size_t _size_() {\\\n"
             "              return __reflect_range_size(reflenums::RK_ ## RKname, reflenums::RKname::_this_, Xs...);\\\n"
             "            }\\\n"
             "            template<size_t _N_>\\\n"
             "            static constexpr auto _get_() {\\\n"
             "              return __reflect_range_nth(reflenums::RK_ ## RKname, reflenums::RKname::_this_, Xs..., _N_);\\\n"
             "            }\n"
             "    /**/\n"
             "#   define DEF_RANGE_REFLECTION_TUPLE(MemName, MethodSig, ElemT/*unused*/, InitArgs, ParamArgs)\\\n"
             "        template<intptr_t... Xs>\\\n"
             "        struct MemName ## _tuple {\\\n"
             "            static constexpr size_t _size_() {\\\n"
             "              return __reflect_range_size(\\\n"
             "                PP_UNCNDL_REMOVE_PARENS(InitArgs) PP_UNCNDL_REMOVE_PARENS(ParamArgs) );\\\n"
             "            }\\\n"
             "            template<size_t _N_>\\\n"
             "            static constexpr auto _get_() {\\\n"
             "              return __reflect_range_nth(\\\n"
             "                PP_UNCNDL_REMOVE_PARENS(InitArgs), _N_ PP_UNCNDL_REMOVE_PARENS(ParamArgs) );\\\n"
             "            }\\\n"
             "        };\\\n"
//             "        template<std::size_t N, intptr_t... Xs>\\\n"
//             "        static constexpr auto get(MemName ## _tuple<Xs...> const &) {\\\n"
//             "          return MemName ## _tuple<Xs...>::template get<N>();\\\n"
//             "        }\n" //DWR commented out because I changed the TupleExpansion implem so you don't need to go the DeclContext and fetch its "get" implem.
             "    /**/\n"
             "#   define RANGE_REFLECTION(ClassName, MemName, MethodSig, ElemT, InitArgs, ParamArgs)\\\n"
             "        MethodSig -> typename ClassName:: template MemName ## _tuple<Xs...> { return {}; }\n"
             "    /**/\n"
             "#else //just code-completing\n"
             "#   define M_template         \n"
             "#   define M_tbeg             \n"
             "#   define M_rtpack(Zs)       \n"
             "#   define M_c                \n"
             "#   define M_pack(Zs)         \n"
             "#   define M_tend             \n"
             "#   define M_getPtrVal_T_value  true\n"
             "#   define IFMETA_ELSE(MP_compiler_version/*ignored*/, MP_idehlpr_version)\\\n"
             "      PP_UNCNDL_REMOVE_PARENS(MP_idehlpr_version)\n"
             "#   define M_REFLTYPED_FIELD(MP_name, MP_idehlpr_t, .../*ignored*/)\\\n"
             "      PP_UNCNDL_REMOVE_PARENS(MP_idehlpr_t) MP_name;\n" //TODO if this is a reflTemplateNeeded pointer, should default it to &DummyVars<ptrremoved_t>.
             "    /**/\n"
             "#   define DUMMY_ITERATOR_BEGIN_END(...)\\\n"
             "      private:\\\n"
             "          struct iterator_t {\\\n"
             "              constexpr __VA_ARGS__ operator*() const;\\\n"
             "              constexpr iterator_t &operator++();\\\n"
             "              constexpr iterator_t operator++(int);\\\n"
             "              constexpr bool operator==(const iterator_t &other) const;\\\n"
             "              constexpr bool operator!=(const iterator_t &other) const;\\\n"
             "          };\\\n"
             "      public:\\\n"
             "          iterator_t begin() const;\\\n"
             "          iterator_t end() const;\n"
             "    /**/\n"
             "#   define RANGECLASS_SIZE_AND_GET(RKname, ...) DUMMY_ITERATOR_BEGIN_END(__VA_ARGS__)\n"
             "#   define DEF_RANGE_REFLECTION_TUPLE(MemName, MethodSig, ElemT/*unused*/, InitArgs, ParamArgs)\n"
             "#   define RANGE_REFLECTION(ClassName, MemName, MethodSig, ElemT, InitArgs, ParamArgs)\\\n"
             "    MethodSig {\\\n"
             "      class dummy_range {\\\n"
             "          DUMMY_ITERATOR_BEGIN_END(PP_UNCNDL_REMOVE_PARENS(ElemT))\\\n"
             "      };\\\n"
             "      return dummy_range();\\\n"
             "    }\n"
             "#   define DONT_USE \\\n"
             "      static_assert(false && \"Not intended to be used, just to aid the IDE with code completion. \"\\\n"
             "                  \"You must #define __CONSUMER_SUPPORTS_REFLECTION_AND_META__ during actual compilation to turn the real implem on.\")\n"
             "\n"
             "\n"
             "#endif // end of stuff used when not really __CONSUMER_SUPPORTS_REFLECTION_AND_META__; \n"
             "       // rather, just helping IDE with code completion\n"
             "\n"
             "#define M_template_rtpack(Xs)    M_template M_tbeg M_rtpack(Xs) M_tend\n"
             "#define M_targpack(Xs)           M_tbeg M_pack(Xs) M_tend\n"
             "\n"
             "\n"
             "/////// CASTING SUPPORT /////////\n"
             "\n"
//             "/// GetDfltNonnullVal -- to avoid IDE flagging nullptr accesses in our dummy implems,\n"
//             "/// we must use \"real\" values in our dummy cast and reflexpr and method return statements.\n"
//             "/// This template, along with static variables in each impl class, permits us to do that.\n"
//             "///\n"
//             "template<typename T>\n"
//             "struct GetDfltNonnullObj {\n"
//             "  static_assert(std::is_class<T>::value, \"Specialize non-class types please.\");\n"
//             "  static constexpr T value = T::DfltNonnullObj;\n"
//             "};\n"
//             "\n"
//             "template<typename T> struct GetDfltNonnullObj<T *> {\n"
//             "  static constexpr T *value = &GetDfltNonnullObj<T>::value;\n"
//             "};\n"
//             "\n"
//             "template<typename T> struct GetDfltNonnullObj<const T *> {\n"
//             "  static constexpr const T *value = &GetDfltNonnullObj<T>::value;\n"
//             "};\n"
//             "\n"
//             "template<typename T> struct GetDfltNonnullObj<T &> {\n"
//             "  static constexpr T &value = GetDfltNonnullObj<T>::value;\n"
//             "};\n"
//             "\n"
//             "template<typename T> struct GetDfltNonnullObj<const T &> {\n"
//             "  static constexpr const T &value = GetDfltNonnullObj<T>::value;\n"
//             "};\n"
//             "\n"
             "\n"
             "// COPIED FROM llvm/Support/Compiler.h:\n"
             "# define LLVM_NODISCARD [[nodiscard]]\n"
             "\n"
             "namespace llvm {\n"
             "\n"
             "// COPIED FROM llvm/Support/type_traits:\n"
             "\n"
             "/// If T is a pointer, just return it. If it is not, return T&.\n"
             "template<typename T, typename Enable = void>\n"
             "struct add_lvalue_reference_if_not_pointer {\n"
             "  using type = T &;\n"
             "};\n"
             "\n"
             "template<typename T>\n"
             "struct add_lvalue_reference_if_not_pointer<\n"
             "        T, typename std::enable_if<std::is_pointer<T>::value>::type> {\n"
             "  using type = T;\n"
             "};\n"
             "\n"
             "/// If T is a pointer to X, return a pointer to const X. If it is not,\n"
             "/// return const T.\n"
             "template<typename T, typename Enable = void>\n"
             "struct add_const_past_pointer {\n"
             "  using type = const T;\n"
             "};\n"
             "\n"
             "template<typename T>\n"
             "struct add_const_past_pointer<\n"
             "        T, typename std::enable_if<std::is_pointer<T>::value>::type> {\n"
             "  using type = const typename std::remove_pointer<T>::type *;\n"
             "};\n"
             "\n"
             // DWR TODO: all this isa stuff should be done by the inner clang as well,
             // via a reflect_cast -- just add a third option in addition to dyn 0/1; 2=isa.
             "// FROM llvm/Support/Casting.h:\n"
             "\n"
             "//===----------------------------------------------------------------------===//\n"
             "//                          isa<x> Support Templates\n"
             "//===----------------------------------------------------------------------===//\n"
             "\n"
             "/// Define a template that can be specialized by smart pointers to reflect the\n"
             "/// fact that they are automatically dereferenced, and are not involved with the\n"
             "/// template selection process...  the default implementation is a noop.\n"
             "template<typename From>\n"
             "struct simplify_type {\n"
             "  using SimpleType = From; // The real type this represents...\n"
             "\n"
             "  // An accessor to get the real value...\n"
             "  static constexpr SimpleType &getSimplifiedValue(From &Val) { return Val; }\n"
             "};\n"
             "\n"
             "template<typename From>\n"
             "struct simplify_type<const From> {\n"
             "  using NonConstSimpleType = typename simplify_type<From>::SimpleType;\n"
             "  using SimpleType =\n"
             "  typename add_const_past_pointer<NonConstSimpleType>::type;\n"
             "  using RetType =\n"
             "  typename add_lvalue_reference_if_not_pointer<SimpleType>::type;\n"
             "\n"
             "  static constexpr RetType getSimplifiedValue(const From &Val) {\n"
             "    return simplify_type<From>::getSimplifiedValue(const_cast<From &>(Val));\n"
             "  }\n"
             "};\n"
             "\n"
             "template<class X>\n"
             "struct is_simple_type {\n"
             "  static const bool value =\n"
             "          std::is_same<X, typename simplify_type<X>::SimpleType>::value;\n"
             "};\n"
             "\n"
             "// The core of the implementation of isa<X> is here; To and From should be\n"
             "// the names of classes.  This template can be specialized to customize the\n"
             "// implementation of isa<> without rewriting it from scratch.\n"
             "template<typename To, typename From, typename Enabler = void>\n"
             "struct isa_impl {\n"
             "  static constexpr bool doit(const From &Val) {\n"
             "    return To::classof(&Val);\n"
             "    // ^ NB: every castable reflection type needs a\n"
             "    // static bool classof(...) member function reflected!\n"
             "  }\n"
             "};\n"
             "\n"
             "/// Always allow upcasts, and perform no dynamic check for them.\n"
             "template<typename To, typename From>\n"
             "struct isa_impl<\n"
             "        To, From, typename std::enable_if<std::is_base_of<To, From>::value>::type> {\n"
             "  static constexpr bool doit(const From &) { return true; }\n"
             "};\n"
             "\n"
             "template<typename To, typename From>\n"
             "struct isa_impl_cl {\n"
             "  static constexpr bool doit(const From &Val) {\n"
             "    return isa_impl<To, From>::doit(Val);\n"
             "  }\n"
             "};\n"
             "\n"
             "template<typename To, typename From>\n"
             "struct isa_impl_cl<To, const From> {\n"
             "  static constexpr bool doit(const From &Val) {\n"
             "    return isa_impl<To, From>::doit(Val);\n"
             "  }\n"
             "};\n"
             "\n"
             "\n"
             "template<typename To, typename From>\n"
             "struct isa_impl_cl<To, From *> {\n"
             "  static constexpr bool doit(const From *Val) {\n"
             "    assert(Val && \"isa<> used on a null pointer\");\n"
             "    return isa_impl<To, From>::doit(*Val);\n"
             "  }\n"
             "};\n"
             "\n"
             "template<typename To, typename From>\n"
             "struct isa_impl_cl<To, From *const> {\n"
             "  static constexpr bool doit(const From *Val) {\n"
             "    assert(Val && \"isa<> used on a null pointer\");\n"
             "    return isa_impl<To, From>::doit(*Val);\n"
             "  }\n"
             "};\n"
             "\n"
             "template<typename To, typename From>\n"
             "struct isa_impl_cl<To, const From *> {\n"
             "  static constexpr bool doit(const From *Val) {\n"
             "    assert(Val && \"isa<> used on a null pointer\");\n"
             "    return isa_impl<To, From>::doit(*Val);\n"
             "  }\n"
             "};\n"
             "\n"
             "template<typename To, typename From>\n"
             "struct isa_impl_cl<To, const From *const> {\n"
             "  static constexpr bool doit(const From *Val) {\n"
             "    assert(Val && \"isa<> used on a null pointer\");\n"
             "    return isa_impl<To, From>::doit(*Val);\n"
             "  }\n"
             "};\n"
             "\n"
             "template<typename To, typename From, typename SimpleFrom>\n"
             "struct isa_impl_wrap {\n"
             "  // When From != SimplifiedType, we can simplify the type some more by using\n"
             "  // the simplify_type template.\n"
             "  static constexpr bool doit(const From &Val) {\n"
             "    return isa_impl_wrap<To, SimpleFrom,\n"
             "            typename simplify_type<SimpleFrom>::SimpleType>::doit(\n"
             "            simplify_type<const From>::getSimplifiedValue(Val));\n"
             "  }\n"
             "};\n"
             "\n"
             "template<typename To, typename FromTy>\n"
             "struct isa_impl_wrap<To, FromTy, FromTy> {\n"
             "  // When From == SimpleType, we are as simple as we are going to get.\n"
             "  static constexpr bool doit(const FromTy &Val) {\n"
             "    return isa_impl_cl<To, FromTy>::doit(Val);\n"
             "  }\n"
             "};\n"
             "\n"
             "/// isa<X> - Return true if the parameter to the template is an instance of the\n"
             "/// template type argument.  Used like this:\n"
             "///\n"
             "///  if (isa<Type>(myVal)) { ... }\n"
             "///\n"
             "template <class X, class Y> LLVM_NODISCARD constexpr bool isa(const Y &Val) {\n"
             "  return isa_impl_wrap<X, const Y,\n"
             "          typename simplify_type<const Y>::SimpleType>::doit(Val);\n"
             "}\n"
             "\n"
             "} // end namespace llvm\n"
             "\n"
             "\n"
             "\n"
             "\n"
             "namespace cppx {\n"
             "\n"
             "namespace meta {\n"
             "\n"
             "\n"
             "namespace refldetail {\n"
             "\n";
  fstream << "/// A number generated while building the version of Clang\n"
             "/// which this reflection info was generated.  Reflection will\n"
             "/// not be permitted unless this number matches that of the \n"
             "/// compiler.\n"
             "static const unsigned __reflheaderid__ = "
          << CMAKE_GEN_REFLHEADERID << ";\n"
          << "\n"
             "\n"
             "template<typename T> struct ptrwrp;\n"
             "template<typename T> struct getPtrVal;\n"
             "\n"
             "template< template<intptr_t...> class TMPL\n"
             "        , intptr_t PtrVal >\n"
             "struct getPtrVal<TMPL</*IsPtr=*/1, PtrVal>> {\n"
             "  static constexpr intptr_t value = PtrVal;\n"
             "};\n"
             "\n"
             "template<typename T>\n"
             "struct getPtrVal<ptrwrp<T>> {\n"
             "  static constexpr intptr_t value = getPtrVal<T>::value;\n"
             "};\n"
             "\n"
             "\n"
             "/// ptrwrp_to_ptrs: helps us implement get() in ptrwrp so that\n"
             "/// it removes all ptrwrp wrappings, replacing them with pointers.\n"
             "/// e.g. ptrwrp_full_unwrp(ptrwrp<ptrwrp<A>> obj) returns obj of\n"
             "/// type A**. We need this to permit proper derived->base conversions\n"
             "/// for accessing proper function overloads from ptrwrp param types.\n"
             "template<typename T>\n"
             "constexpr auto ptrwrp_to_ptrs(ptrwrp<T> t) {\n"
             "  return ptrwrp_full_get(t.get());\n"
             "}\n"
             "template<typename T>\n"
             "constexpr auto ptrwrp_to_ptrs(const ptrwrp<T> *t) {\n"
             "  return &ptrwrp_full_get(t->get());\n"
             "}\n"
             "template<typename T>\n"
             "constexpr auto ptrwrp_to_ptrs(ptrwrp<T> *t) {\n"
             "  return &ptrwrp_full_get(t->get());\n"
             "}\n"
             "template<typename T>\n"
             "constexpr auto ptrwrp_to_ptrs(const T *t) {\n"
             "  return t;\n"
             "}\n"
             "template<typename T>\n"
             "constexpr auto ptrwrp_to_ptrs(T *t) {\n"
             "  return t;\n"
             "}\n"
             "\n"
             "/// e.g.:\n"
             "/// FunctionDecl*  <=>  ptrwrp<FunctionDecl::impl<Xs...>>\n"
             "template<typename T>\n"
             "class ptrwrp {\n"
             "  T t;\n"
             "public:\n"
             "  constexpr T operator*() const noexcept { return t; }\n"
             "    //^NB no need for ref return; reflection T behaves like a ref naturally\n"
             "  constexpr const T * operator->() const noexcept { return &t; }\n"
             "  constexpr T * operator->() noexcept { return &t; }\n"
             "  constexpr auto get() const { return ptrwrp_to_ptrs(&t); }\n"
             "  constexpr operator bool() const {\n"
             "    return M_getPtrVal_T_value;\n"
             "  }\n"
             "  constexpr operator intptr_t() const {//TODO explicit perhaps\n"
             "    return M_getPtrVal_T_value;\n"
             "  }\n"
             "  constexpr ptrwrp(T t = {}) : t(t) {}\n"
             "  constexpr explicit operator const char *() const IFMETA_ELSE( ({\n"
             "    return __concatenate(\"cppx::meta::refldetail::ptrwrp(\", (const char *)t, \")\");\n"
             "  }) , ( { return \"\"; } ) )\n"
             "};\n"
             "//Deduction guide: used by operator const char*() above:\n"
             "IFMETA_ELSE( (template<typename T> ptrwrp(T t) -> ptrwrp<T>;), () )\n"
             "\n"
             "} //namespace refldetail\n"
             "\n"
             "#ifdef __CONSUMER_SUPPORTS_REFLECTION_AND_META__\n"
             "  /// isa<X> - Return true if the parameter to the template is an instance of the\n"
             "  /// template type argument.  Used like this:\n"
             "  ///\n"
             "  ///  if (isa<Tmpl>(myVal)) { ... }\n"
             "  ///\n"
             "  template< template<intptr_t...> class To\n"
             "          , template<intptr_t...> class From\n"
             "          , intptr_t... Xs >\n"
             "  LLVM_NODISCARD constexpr bool isa(refldetail::ptrwrp<From<Xs...>> Val) {\n"
             "    return ::llvm::isa<To<Xs...>>(Val.get());\n"
             "  }\n"
             "\n"
             "  /// cast<To> - Return the argument parameter cast to the specified type.  This\n"
             "  /// casting operator asserts that the type is correct, so it does not return null\n"
             "  /// on failure.  It does not allow a null argument (use cast_or_null for that).\n"
             "  /// It is typically used like this:\n"
             "  ///\n"
             "  ///  cast<CXXRecordDecl>(myReflDecl)->bases()\n"
             "  ///\n"
             "  /// Note that cast may change the pointer value due to alignment issues --\n"
             "  /// rather than messing around with that here we let clang handle it\n"
             "  /// via __reflect_cast with an auto return type.\n"
             "  template< template<intptr_t...> class To\n"
             "          , template<intptr_t...> class From\n"
             "          , intptr_t X >\n"
             "  constexpr auto cast(const refldetail::ptrwrp<From</*IsPtr=*/1, X>> &Val) {\n"
             "    return __reflect_cast(From<1,0>::ReflObjKind, To<1,0>::ReflObjKind, /*dyn=*/0, X);\n"
             "  }\n"
             "\n"
             "  /// Special case: for a non-ptrwrp argument (usually a non-pointer),\n"
             "  /// just do a traditional slice if its possible.\n"
             "  template< template<intptr_t...> class To\n"
             "          , template<intptr_t...> class From\n"
             "          , intptr_t... Xs >\n"
             "  constexpr To<Xs...> cast(const From<Xs...> &Val) {\n"
             "    static_assert(std::is_base_of<To<Xs...>, From<Xs...>>::value,\n"
             "                  \"Invalid reflection slice\");\n"
             "    return Val;\n"
             "  }\n"
             "\n"
             "  /// dyn_cast<To> -\n"
             "  /// Return the argument parameter cast to the specified type.  This\n"
             "  /// casting operator returns null if the argument is of the wrong type, so it can\n"
             "  /// be used to test for a type as well as cast if successful.  This should be\n"
             "  /// used in the context of an if statement like this:\n"
             "  ///\n"
             "  ///  if (auto RD = dyn_cast<CXXRecordDecl>(myReflectedDecl)) { ... }\n"
             "  ///\n"
             "  template< template<intptr_t...> class To\n"
             "          , template<intptr_t...> class From\n"
             "          , intptr_t X >\n"
             "  constexpr auto dyn_cast(const refldetail::ptrwrp<From</*IsPtr=*/1, X>> &Val) {\n"
             "    return __reflect_cast(From<1,0>::ReflObjKind, To<1,0>::ReflObjKind, /*dyn=*/1, X);\n"
             "  }\n"
             "\n"
             "#else\n"
//             "  template<typename T>\n"
//             "  static constexpr T DummyVars;\n"
             "  template<typename To, typename From>\n"
             "  constexpr bool isa(const From *from) { return true; }\n"
             "  template<typename To, typename From>\n"
             "  constexpr const To *cast(const From *from) { return (const To *)0; }\n"
             "  template<typename To, typename From>\n"
             "  constexpr const To *dyn_cast(const From *from) { return cast<To>(from); }\n"
             "  \n"
             "  //Non-const overloads (only needed to help IDE):\n"
             "  template<typename To, typename From>\n"
             "  constexpr To *cast(From *from) { return (To *)0; }\n"
             "  template<typename To, typename From>\n"
             "  constexpr refldetail::ptrwrp<To> cast(refldetail::ptrwrp<From> from) { return {}; }\n"
             "  template<typename To, typename From>\n"
             "  constexpr To cast(From from) { return from; }\n"
             "  template<typename To, typename From>\n"
             "  constexpr To *dyn_cast(From *from) { return cast<To>(from); }\n"
             "  template<typename To, typename From>\n"
             "  constexpr refldetail::ptrwrp<To> dyn_cast(refldetail::ptrwrp<From> from) { return cast<To>(from); }\n"
             "\n"
             "  /// We define reflexpr(x) as a macro for non-meta-supporters, that returns\n"
             "  /// a pointer to a dummy type instructing the user to properly cast it.\n"
             "  /// Note that it doens't matter that this dummy type isn't related to the\n"
             "  /// the types we cast to, as our dummy cast implems above don't do any actual\n"
             "  /// casting; their goal here is to just helping the IDE navigate the types,\n"
             "  /// without resorting to casting nullptrs (which raises other errors).\n"
             "# define reflexpr(...)  (&cppx::meta::DfltNonnullObj<cppx::meta::_RememberToManuallyCastReflexprForNow_>)\n"
             "\n"
             "  /// Cast this to whatever makes sense in the context; e.g. `cast<CXXRecordDecl>(reflexpr(SomeClass))`.\n"
             "  /// Not necessary to compile, but definitely a good practice to help the IDE help you.\n"
             "  /// We return a base Decl by default, since that's usually what you reflect,\n"
             "  /// but sometimes a reflection will return a type;\n"
             "  /// e.g. `template<typename T> MyMetaCls { auto proto_t_refl = reflexpr(T); };\n"
             "  /// In such cases it's even more important to cast the reflexpr to avoid user confusion.\n"
             "  struct _RememberToManuallyCastReflexprForNow_ {\n"
             "    constexpr _RememberToManuallyCastReflexprForNow_() {}\n"
             "  };\n"
             "  template<typename T> T DfltNonnullObj;\n"
             "\n"
             "\n"
             "  // While we're here let's also define __concatenate and __metaparse_expr for non-supporters:\n"
             "# define __concatenate(...) \"\"\n"
             "# define __metaparse_expr(expr, /**type*/...) (__VA_ARGS__)0\n"
             "\n"
             "#endif\n"
             "\n"
             "/// Turns a parameter pack of integers into a string literal, each int separated by a comma.\n"
             "/// Needed to define the operator const char*()'s for each reflection type.\n"
             "template<intptr_t... Xs>\n"
             "struct concat_ints_w_commas;\n"
             "\n"
             "template<intptr_t X, intptr_t... Xs>\n"
             "struct concat_ints_w_commas<X, Xs...> {\n"
             "  static constexpr const char *value = __concatenate(X, \",\", concat_ints_w_commas<Xs...>::value);\n"
             "};\n"
             "\n"
             "template<intptr_t X>\n"
             "struct concat_ints_w_commas<X> {\n"
             "  static constexpr const char *value = __concatenate(X); //need __concatenate to turn the int into a string\n"
             "};\n"
             "namespace reflenums {\n"
             "\n"
             "enum ReflectionObjKind {\n";
  fstream << ReflectionObjKindList_inc.str();
  fstream << "}; //enum ReflectionObjKind\n"
             "\n"
             "\n";
  fstream << "  " << ReplaceAll(ReflInfoNamespaces_inc.str(), "\n", "\n  ");
  fstream << "\n"
             "} //namespace reflenums\n"
             "\n"
             "\n";
}




/// Used to check if Derived is, e.g., a "clang::Decl" or a "clang::Stmt" etc.
static bool inheritsFromRoot(CXXRecordDecl *Derived, StringRef rootqualdname) {
  if (Derived->getNumBases()) {
    for (auto b : Derived->bases()) {
      CXXRecordDecl *thebaseclass =
          b.getType().getCanonicalType().getTypePtr()->getAsCXXRecordDecl();
      if (inheritsFromRoot(thebaseclass, rootqualdname))
        return true;
    }
  }
  // Lastly, check against rootqualdname:
  return (Derived->getQualifiedNameAsString() == rootqualdname);
}


void ReflGenASTConsumer::writeChunksForDecl(Decl *D) {
  //TODO: to avoid empty namespaces, don't actually write the namespaces here -- instead,
  // enter a "currentDC" state, and for each tag decl you encounter, check its DC, and
  // enter/exit as needed using a helper function, which will close/enter namespaces
  if (auto NsD = dyn_cast<NamespaceDecl>(D)) {
    ClientReflection_h_fwddecls << "\n" << curindentstr << "namespace "
                                << NsD->getNameAsString() << " {\n";
    ClientReflection_h_aliases  << "\n" << curindentstr << "namespace "
                                << NsD->getNameAsString() << " {\n";

    writeChunksForMembersOf(NsD, /*extraindentspaces=*/2);
    ClientReflection_h_fwddecls << curindentstr << "}\n";
    ClientReflection_h_aliases  << curindentstr << "}\n";
    return;
  }
  if (auto RD = dyn_cast<CXXRecordDecl>(D)) {
    RD = RD->getDefinition();
    assert(RD);

    std::string RKname = getRKname(RD, *this);
    std::string clientQualdName = getClientQualdName(RD, *this);
    ReflectionObjKindList_inc << "  RK_" << RKname << ",\n";
    
    if (inheritsFromRoot(RD, "clang::Stmt"))
      ReflectionObjKindNodes_inc << "STMT(";
    else if (inheritsFromRoot(RD, "clang::Type"))
      ReflectionObjKindNodes_inc << "TYPE(";
    else if (inheritsFromRoot(RD, "clang::Decl") ||
             inheritsFromRoot(RD, "clang::DeclContext"))
      ReflectionObjKindNodes_inc << "DECL(";
    else
      ReflectionObjKindNodes_inc << "OTHER(";
    ReflectionObjKindNodes_inc           << "RK_" << RKname << ")\n";
    
    generate_CTDLookupCase(RD, RKname, ReflectionCTDLookupCases_inc);
    generate_ReflectionRKTypeConversionsSpec(RD, RKname, ReflectionGetRKSpecs_inc, clientQualdName);
    generate_ClientReflectionRKTypeConversionsSpec(RD, RKname, ClientGetReflectionObjKindSpecs_stream, clientQualdName);

//    this->default_contructible_reflections

//    auto rbinfo = class_usage_map[RD];


    std::string ind = "  ";

    generate_refltrait_mem_lambda_defs(ind, RD, RKname, ReflTrait_SetTypeAndCBs_Cases_inc, *this);


//    if (rbinfo.rbptr) {
//      if (rbinfo.rbvalue) {
//        ReflTrait_SetTypeAndCBs_Cases_inc << "  if (IsPtr) {\n";
//        ind += "  ";
//      }

//      ReflTrait_SetTypeAndCBs_Cases_inc << ind << "auto X = get_next_as<" << RD->getQualifiedNameAsString() << " *>(Args);\n";
//      generate_refltrait_mem_lambda_defs(ind, /*val0ptr1=*/1, RD, RKname, ReflTrait_SetTypeAndCBs_Cases_inc, *this);

//      if (rbinfo.rbvalue)
//        ReflTrait_SetTypeAndCBs_Cases_inc << "  } /* IsPtr */\n"
//                                             "  else {\n";
//    }
//    if (rbinfo.rbvalue) {
//      ReflTrait_SetTypeAndCBs_Cases_inc << ind << "auto X = get_next_as<" << RD->getQualifiedNameAsString() << ">(Args);\n";
//      generate_refltrait_mem_lambda_defs(ind, /*val0ptr1=*/0, RD, RKname, ReflTrait_SetTypeAndCBs_Cases_inc, *this);

//      if (rbinfo.rbptr) {
//        ReflTrait_SetTypeAndCBs_Cases_inc << "} /* !IsPtr */\n";
//        ind.pop_back();
//        ind.pop_back();
//      }
//    }




//    if (rbinfo.rbptr) {
//      generate_ReflectorCallOperator_PointerCase(RD, RKname, ReflectorCallOperator_PointerCases_inc);
//      generate_reflectprop_overload(/*decl0def1=*/0, /*val0ptr1=*/1, RD, RKname, ReflectPropDecls_inc, *this);
//      generate_reflectprop_overload(/*decl0def1=*/1, /*val0ptr1=*/1, RD, RKname, ReflectPropDefs_inc, *this);
//    }

//    if (rbinfo.rbvalue) {
//      generate_ReflectorCallOperator_NonPointerCase(RD, RKname, ReflectorCallOperator_NonPointerCases_inc);
//      generate_reflectprop_overload(/*decl0def1=*/0, /*val0ptr1=*/0, RD, RKname, ReflectPropDecls_inc, *this);
//      generate_reflectprop_overload(/*decl0def1=*/1, /*val0ptr1=*/0, RD, RKname, ReflectPropDefs_inc, *this);
//    }

    generate_GetReflQTorCTD_case(RD, RKname, ReflKindAndMemNumToQTorCTD_inc, *this);

    generate_ReflKindGetMaxMemNumCase(RD, RKname, ReflKindGetTotalMemNumCases_inc);
    generate_ReflKindToStringCase(RD, RKname, ReflKindToStringCases_inc);
    generate_ReflInfoNamespaces(RD, RKname, ReflInfoNamespaces_inc, *this);

    // TODO, set up some logic so that if you don't add any methods for a certain class, you can skip over it
    // --perhaps just set up a map to the number of methods in a class (I suppose alongside the rbvalue/rbptr status), and test against that

    ClientReflection_h_fwddecls << curindentstr << "struct "
        << RD->getNameAsString() << " {\n"
        << curindentstr << "  M_template_rtpack(Xs) struct impl;\n";

    // Must do this before generating the impl def, so that default enum args can be interpreted
    writeChunksForMemberEnumsOf(RD);

    generate_client_impltmpl(RD, RKname, /*curindentstr*/"", curindentstr, ClientReflection_h_impldefs, ClientReflection_h_fwddecls, ClientReflection_h_std_tuple_specs, *this);

    // We do this one after the impl def since usually the nested classes have members that need
    // to reference the structure of the enclosing class (i.e. the enclosing class's impl def).
    // Still could conceivably get errors here though, in which case gotta think through the ordering
    // some more.
    writeChunksForMemberClassesOf(RD);

    ClientReflection_h_fwddecls << curindentstr << "};\n";

    if (!RD->getDeclContext()->isRecord()) {
      ClientReflection_h_aliases << "  M_template_rtpack(Zs) using "
                                 << RD->getNameAsString() << " = struct refldetail::"
                                 << clientQualdName << "::M_template impl M_targpack(Zs);\n";
    }

    return;
  }
  if (auto ED = dyn_cast<EnumDecl>(D)) {
    ED = ED->getDefinition();
    assert(ED);

//    // If its nested within a class, put it in the proper impl.
//    if (ED->getDeclContext()->isRecord()) {
//      ClientReflection_h_fwddecls << curindentstr << "enum " << (ED->isScoped() ? "class " : "")
//                                  << ED->getNameAsString() << " : " << ED->getIntegerType().getAsString() << ";\n";
//      copy_enum_def_to(ED, ClientReflection_h_impldefs,"", *this);
//
//    }
//    // If its not nested, put the definition directly in the aliases area so that the enumerators are placed into the proper scope (which they would not be if we placed them elsewhere and simply put an alias here):
//    else {
//      copy_enum_def_to(ED,ClientReflection_h_aliases, curindentstr, *this, false);
//    }



    ClientReflection_h_fwddecls << curindentstr << "enum " << (ED->isScoped() ? "class " : "")
                                << ED->getNameAsString() << " : " << ED->getIntegerType().getAsString() << ";\n";
    copy_enum_def_to(ED, ClientReflection_h_impldefs, "", *this);

    // Set aliases for any enums not nested within structs
    // (nested enums too hard to properly set up, and
    // we probably won't even use enum aliases at all anyway.)
    if (ED->getDeclContext()->isNamespace())
      ClientReflection_h_aliases << "  using " << ED->getNameAsString() << " = enum refldetail::"
                                 << getClientQualdName(ED, *this) << ";\n";



    return;

  }



}
