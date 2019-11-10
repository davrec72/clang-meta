//
//  decldata.h
//  forceSingleInheritance
//
//  Created by David Rector on 5/14/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef decldata_h
#define decldata_h

#include <vector>
#include <iostream>
#include "SmallVector.h"
#include "MetaparseDebug.h"

namespace dwr {
  
  using stream_t = decltype(std::cout); //TEMP: later make this a file stream.
  
  struct Printable {
    virtual void print(stream_t &s) = 0;
  };
  
  enum AccessKind {
    AK_none,
    AK_private,
    AK_protected,
    AK_public
  };
  std::string to_string(AccessKind access) {
    switch(access) {
      case AK_none: return "";
      case AK_private: return "private ";
      case AK_protected: return "protected ";
      case AK_public: return "public ";
    }
  }
  
  
  enum DeclKind {
    DK_Class,
    DK_Enum,
    DK_Enumerator,
    DK_Method,
    DK_Field,
    DK_Alias,
    DK_Activation,
    DK_Deactivation,
    DK_Using,
    DK_Unnamed,
  };
  
  //Abstract:
  struct NamedDecl;
  
  //Concrete:
  struct ClassDecl;
  struct EnumDecl;
  struct MethodDecl;
  struct FieldDecl;
  struct AliasDecl;
  struct ActivationDecl;
  struct DeactivationDecl;
  struct UsingDecl;
  struct UnnamedDecl;
  
  
  class VisibleDecl : public Printable {
    AccessKind _access;
    DeclKind _kind;
  public:
    VisibleDecl(DeclKind kind, AccessKind access = AK_none) : _kind(kind), _access(access) {}
    DeclKind kind() const { return _kind; }
    AccessKind access() const { return _access; }
    
    NamedDecl * getAsNamedDecl();
    ClassDecl * getAsClassDecl();
    EnumDecl * getAsEnumDecl();
    MethodDecl * getAsMethodDecl();
    FieldDecl * getAsFieldDecl();
    AliasDecl * getAsAliasDecl();
    ActivationDecl * getAsActivationDecl();
    DeactivationDecl * getAsDeactivationDecl();
    UnnamedDecl * getAsUnnamedDecl();
  };
  
  
  
  class NamedDecl : public VisibleDecl {
    std::string _name;
    std::string _doc;
  public:
    NamedDecl(std::string name, std::string doc, DeclKind kind, AccessKind access = AK_none)
    : VisibleDecl(kind, access), _name(name), _doc(doc)
    {}
    std::string &name() { return _name; }
    const std::string &name() const { return _name; }
  };
  
  class BaseSpecifier {
    ClassDecl *_theclass;
    AccessKind _access;
    bool _is_virtual;
  public:
    BaseSpecifier(ClassDecl *theclass,
                  AccessKind access = AK_public,
                  bool is_virtual = false)
    : _theclass(theclass), _access(access), _is_virtual(is_virtual)
    {
      assert(_access != AK_none && "Expected valid access");
      assert(theclass && "Expected nonnull class!");
    }
    BaseSpecifier() : _theclass(nullptr) {
      assert(!this->operator bool());
    }
    AccessKind access() const { return _access; };
    bool is_virtual() { return _is_virtual; };
    ClassDecl *theclass() { return _theclass; };
    const ClassDecl *theclass() const { return _theclass; };
    operator bool() const { return _theclass; }
  };
  
  class ClassDecl : public NamedDecl {
    SmallVectorImpl<BaseSpecifier, 4> _bases;
    SmallVectorImpl<VisibleDecl *, 16> _decls;
  public:
    auto &bases() { return _bases; }
    const auto &bases() const { return _bases; }
    auto &decls() { return _decls; }
    const auto &decls() const { return _decls; }
    void adddecl(VisibleDecl *d) {
      _decls.push_back(d);
    }
    ClassDecl(std::string name, std::string doc = "", AccessKind access = AK_none)
        : NamedDecl(name, doc, DK_Class, access), _bases(), _decls() {
          _decls.clear();
          _bases.clear();
        }
    void print(stream_t &s) final {
      s << "class " << name();
      if (!bases().empty()) {
        bool first = true;
        for (auto& b : bases()) {
          if (first) {
            s << " : ";
            first = false;
          } else {
            s << ", ";
          }
          s << to_string(b.access()) << (b.is_virtual() ? "virtual " : "") << b.theclass()->name();
        }
      }
      s << " {";
      AccessKind curaccess = AK_private;
      for (auto d : decls()) {
        if (d->access() != curaccess) {
          s << "\n" << to_string(d->access()) << ":";
          curaccess = d->access();
        }
        s << "\n\t";
        d->print(s);
      }
      if (!decls().empty())
        s << "\n";
      s << "};\n";
      //TODO
      //When printing decls, be sure to check access access of each.
    }
  };
  
  
  class EnumeratorDecl : public NamedDecl {
  public:
    EnumeratorDecl(std::string name, std::string doc = "", AccessKind access = AK_none)
        : NamedDecl(name, doc, DK_Enumerator, access)
    {}
    void print(stream_t &s) final {
      //TODO
    }
  };
  class EnumDecl : public NamedDecl {
    SmallVectorImpl<EnumeratorDecl *, 8> _mems;
  public:
    EnumDecl(std::string name, std::string doc = "", AccessKind access = AK_none)
        : NamedDecl(name, doc, DK_Enum, access)
    {}
    void print(stream_t &s) final {
      //TODO
    }
  };
  
  
  
  class QualType : Printable {
    std::string _prename_str;
    std::string _postname_str;
    size_t _sizeinbits;
  public:
    QualType(const char *prestr, const char *poststr, size_t sizeinbits)
        : _prename_str(prestr), _postname_str(poststr), _sizeinbits(sizeinbits)
    {}
    size_t sizeinbits() const { return _sizeinbits; }
    void print_prename(stream_t &s) {
      s << _prename_str;
    }
    void print_postname(stream_t &s) {
      s << _postname_str;
    }
    void print(stream_t &s) final {
      print_prename(s);
      print_postname(s);
    }
  };
  
  class Expr : public Printable {
    std::string _str;
  public:
    Expr(const char *str) : _str(str) {}
    std::string &str() { return _str; }
    const std::string &str() const { return _str; }
    bool empty() const { return _str.empty(); }
    void print(stream_t &s) final {
      //TODO
    }
  };
  
  class FieldDecl : public NamedDecl {
    QualType _qtype;
    Expr _initializer;
  protected:
    void print_nosemi(stream_t &s) {
      getType().print_prename(s);
      s << name();
      getType().print_postname(s);
      if (Expr *initarg = getInitArg())
        initarg->print(s);
    }
  public:
    FieldDecl(const char *name, const char *doc, AccessKind access, QualType qtype, Expr initializer = {""})
    : NamedDecl(name, doc, DK_Field, access), _qtype(qtype), _initializer(initializer)
    {}
    Expr *getInitArg() {
      return (_initializer.empty() ? nullptr : &_initializer);
    }
    QualType &getType() {
      return _qtype;
    }
    void print(stream_t &s) override {
      print_nosemi(s);
      s << ";\n";
    }
  };
  
  class ParmDecl : public FieldDecl {
    using FieldDecl::getInitArg; //make it private
  public:
    ParmDecl(const char *name, const char *doc, QualType qtype, Expr defaultarg)
    : FieldDecl(name, doc, AK_none, qtype, defaultarg)
    {}
    Expr *getDefaultArg() {
      return FieldDecl::getInitArg();
    }
    void print(stream_t &s) final {
      print_nosemi(s);
    }
  };
  
  class MethodDecl : public NamedDecl {
    QualType _return_type;
    SmallVectorImpl<ParmDecl *, 4> _params;
    Expr _body;
    unsigned _is_static : 1;
    unsigned _is_constexpr : 1;
    unsigned _is_const : 1;
    
  public:
    QualType return_type() { return _return_type; }
    SmallVector<ParmDecl *> &params() { return _params; }
    Expr *getBody() { return (_body.empty() ? nullptr : &_body); }
    bool is_static() const { return _is_static; }
    bool is_constexpr() const { return _is_const; }
    bool is_const() const { return _is_const; }
    
    MethodDecl(const char *name, const char *doc,
               AccessKind access,
               QualType return_type,
               SmallVectorImpl<ParmDecl *, 4>&& params,
               Expr body = {""},
               bool is_static = 0, bool is_constexpr = 0, bool is_const = 0)
    : NamedDecl(name, doc, DK_Method, access)
    , _return_type(return_type)
    , _params(std::move(params))
    , _body(std::move(body))
    , _is_static(is_static), _is_const(is_const)
    {}
    
    void print(stream_t &s) final {
      if (is_static())
        s << "static ";
      if (is_constexpr())
        s << "constexpr ";
      return_type().print(s);
      s << name() << "(";
      bool first = true;
      for (auto param : params()) {
        if (first) first = false;
        else s << ", ";
        param->print(s);
      }
      s << ")";
      if (is_const())
        s << " const";
      if (auto body = getBody())
        body->print(s);
      else
        s << ";";
      s << "\n";
    }
  };
     
  class AliasDecl : public NamedDecl {
    QualType _aliased;
  public:
    AliasDecl(const char *name, const char *doc, AccessKind access, QualType aliased)
    : NamedDecl(name, doc, DK_Alias, access), _aliased(aliased)
    {}
    QualType &aliased() { return _aliased; }
    void print(stream_t &s) final {
      s << "using " << name() << " = ";
      aliased().print(s);
    }
  };
      
  void print_memusingdecls_wbaseexcepts(stream_t &s,
                                        ClassDecl *theclass,
                                        SmallVector<BaseSpecifier>& exceptions) {
    auto prefix = "\n\tusing " + theclass->name() + "::";
    for (auto d : theclass->decls()) {
      //For each public, named decl, set up a using decl.
      if (d->access() == AK_public) {
        if (auto nd = d->getAsNamedDecl())
          s << prefix << nd->name() << ";";
      }
    }
    for (auto b : theclass->bases()) {
      //For all public bases non among the exceptions vec, call this recursively:
      if (b.access() == AK_public && !contains_elem(exceptions, b)) {
        print_memusingdecls_wbaseexcepts(s, b.theclass(), exceptions);
      }
    }
  }
  
  class ActivationDecl : public VisibleDecl {
    ClassDecl *_theclass;
  public:
    ActivationDecl(BaseSpecifier deactivated_basespec)
        : VisibleDecl(DK_Activation, deactivated_basespec.access())
        , _theclass(deactivated_basespec.theclass())
    {
      assert(_theclass);
      assert(getAsActivationDecl());
    }
    ClassDecl *theclass() { return _theclass; }
    void print(stream_t &s) final {
      s << "//operator " << _theclass->name() << "() = default; //activation";
      SmallVector<BaseSpecifier> noexceptions = {};
      print_memusingdecls_wbaseexcepts(s, _theclass, noexceptions);
//      s << "\n\t//(end activation)";
    }
    static ActivationDecl *create(BaseSpecifier pseudo_basespec);
  };
  
  std::list<ActivationDecl> _activationdecls;
  ActivationDecl * ActivationDecl::create(BaseSpecifier pseudo_basespec) {
    _activationdecls.emplace_back(ActivationDecl(pseudo_basespec));
    return &_activationdecls.back();
  }
  
  
      
      
  class DeactivationDecl : public VisibleDecl {
    ClassDecl *_theclass;
    SmallVectorImpl<BaseSpecifier, 4> _exceptions;
  public:
    DeactivationDecl(BaseSpecifier pseudo_basespec, SmallVector<BaseSpecifier>&& exceptions)
    : VisibleDecl(DK_Deactivation, AK_protected)
    , _theclass(pseudo_basespec.theclass())
    , _exceptions(std::move(exceptions))
    {}
    ClassDecl *theclass() { return _theclass; }
    SmallVector<BaseSpecifier> &exceptions() { return _exceptions; }
    void print(stream_t &s) final {
      s << "//operator " << _theclass->name() << "() = delete; //deactivation"
                         << (_exceptions.empty() ? "" : "...\n//public:");
      for (auto b : _exceptions) {
        s << "\n\t//operator " << b.theclass()->name() << "() = default; //...exception";
      }
      print_memusingdecls_wbaseexcepts(s, _theclass, _exceptions);
//      s << "\n\t//(end deactivation)";
      //TODO instead, just print using decls for all the named decls, AND any in the base classes of theclass()
    }
    static DeactivationDecl *create(BaseSpecifier pseudo_basespec, SmallVector<BaseSpecifier>&& exceptions);
  };
  
  std::list<DeactivationDecl> _deactivationdecls;
  DeactivationDecl * DeactivationDecl::create(BaseSpecifier pseudo_basespec, SmallVector<BaseSpecifier>&& exceptions) {
    _deactivationdecls.emplace_back(DeactivationDecl(pseudo_basespec, std::move(exceptions)));
    return &_deactivationdecls.back();
  }
  
  class UsingDecl : public VisibleDecl {
    ClassDecl *_origscope;
    NamedDecl *_theuseddecl;
  public:
    UsingDecl(NamedDecl *theuseddecl, ClassDecl *origscope, AccessKind access)
        : VisibleDecl(DK_Using, access), _origscope(origscope), _theuseddecl(theuseddecl)
    {}
    void print(stream_t &s) final {
      s << "using " << _origscope->name() << "::" << _theuseddecl->name() << ";";
    }
    static UsingDecl *create(NamedDecl *theuseddecl, ClassDecl *origscope, AccessKind access);
  };
      
  std::list<UsingDecl> _usingdecls;
  UsingDecl * UsingDecl::create(NamedDecl *theuseddecl, ClassDecl *origscope, AccessKind access) {
    _usingdecls.emplace_back(UsingDecl(theuseddecl, origscope, access));
    return &_usingdecls.back();
  }
  
  
  class UnnamedDecl : public VisibleDecl {
    std::string _str;
  public:
    UnnamedDecl(std::string str, AccessKind access) : VisibleDecl(DK_Unnamed, access), _str(str) {}
    void print(stream_t &s) final {
      s << _str;
    }
    static UnnamedDecl *create(std::string &&str, AccessKind access);
  };
  
  std::list<UnnamedDecl> _unnameddecls;
  UnnamedDecl * UnnamedDecl::create(std::string &&str, AccessKind access) {
    _unnameddecls.emplace_back(UnnamedDecl(std::move(str), access));
    return &_unnameddecls.back();
  }
  
  bool isnamed(DeclKind kind) {
    switch (kind) {
      case DK_Class:
      case DK_Enum:
      case DK_Method:
      case DK_Field:
      case DK_Alias:
        return true;
      default:
        return false;
    }
  }
      
  NamedDecl *VisibleDecl::getAsNamedDecl() {
    return (isnamed(kind()) ? static_cast<NamedDecl *>(this) : nullptr);
  }
  ClassDecl *VisibleDecl::getAsClassDecl() {
    return (kind() == DK_Class ? static_cast<ClassDecl *>(this) : nullptr);
  }
  EnumDecl *VisibleDecl::getAsEnumDecl() {
    return (kind() == DK_Enum ? static_cast<EnumDecl *>(this) : nullptr);
  }
  MethodDecl *VisibleDecl::getAsMethodDecl() {
    return (kind() == DK_Method ? static_cast<MethodDecl *>(this) : nullptr);
  }
  FieldDecl *VisibleDecl::getAsFieldDecl() {
    return (kind() == DK_Field ? static_cast<FieldDecl *>(this) : nullptr);
  }
  AliasDecl *VisibleDecl::getAsAliasDecl() {
    return (kind() == DK_Alias ? static_cast<AliasDecl *>(this) : nullptr);
  }
  ActivationDecl *VisibleDecl::getAsActivationDecl() {
    return (kind() == DK_Activation ? static_cast<ActivationDecl *>(this) : nullptr);
  }
  DeactivationDecl *VisibleDecl::getAsDeactivationDecl() {
    return (kind() == DK_Deactivation ? static_cast<DeactivationDecl *>(this) : nullptr);
  }
  UnnamedDecl *VisibleDecl::getAsUnnamedDecl() {
    return (kind() == DK_Unnamed ? static_cast<UnnamedDecl *>(this) : nullptr);
  }
  
} //namespace dwr


#endif /* decldata_h */
        
        
        
        
        
        
        
        
