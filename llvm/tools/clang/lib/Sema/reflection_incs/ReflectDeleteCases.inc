  case RK_reflcontainers__VectorStr: {
    //auto mapres = ClientPtrMap.find(X);
    //if (mapres == ClientPtrMap.end()) {
    //  Diag(KWLoc, diag::err_refldelete_nonalloc_ptr) << X << RK_reflcontainers__VectorStr;
    //  return ExprError();
    //}
    //if (mapres.second != RK_reflcontainers__VectorStr)
    //  Diag(KWLoc, diag::err_refldelete_different_ptr) << X << RK_reflcontainers__VectorStr << mapres.second;
    //  return ExprError();
    //}
    //ClientPtrMap.erase(mapres);
    auto deleter_lambda = [&](QualType, ArrayRef<intptr_t>) -> ExprResult {
      delete reinterpret_cast<reflcontainers::VectorStr *>(X);
      return ExprResult();
    };
    return new (Context) ReflectionTraitExpr(Context, KWLoc, RTK_prop, RK_reflcontainers__VectorStr,
        0/*MemNum (dummy)*/, 1/*IsPtr (dummy)*/, Context.VoidTy, deleter_lambda,
        ArrayRef<Expr*>()/*RemArgs (dummy)*/, RParenLoc);
  }
  case RK_reflcontainers__VectorInt: {
    //auto mapres = ClientPtrMap.find(X);
    //if (mapres == ClientPtrMap.end()) {
    //  Diag(KWLoc, diag::err_refldelete_nonalloc_ptr) << X << RK_reflcontainers__VectorInt;
    //  return ExprError();
    //}
    //if (mapres.second != RK_reflcontainers__VectorInt)
    //  Diag(KWLoc, diag::err_refldelete_different_ptr) << X << RK_reflcontainers__VectorInt << mapres.second;
    //  return ExprError();
    //}
    //ClientPtrMap.erase(mapres);
    auto deleter_lambda = [&](QualType, ArrayRef<intptr_t>) -> ExprResult {
      delete reinterpret_cast<reflcontainers::VectorInt *>(X);
      return ExprResult();
    };
    return new (Context) ReflectionTraitExpr(Context, KWLoc, RTK_prop, RK_reflcontainers__VectorInt,
        0/*MemNum (dummy)*/, 1/*IsPtr (dummy)*/, Context.VoidTy, deleter_lambda,
        ArrayRef<Expr*>()/*RemArgs (dummy)*/, RParenLoc);
  }
  case RK_reflcontainers__SetInt: {
    //auto mapres = ClientPtrMap.find(X);
    //if (mapres == ClientPtrMap.end()) {
    //  Diag(KWLoc, diag::err_refldelete_nonalloc_ptr) << X << RK_reflcontainers__SetInt;
    //  return ExprError();
    //}
    //if (mapres.second != RK_reflcontainers__SetInt)
    //  Diag(KWLoc, diag::err_refldelete_different_ptr) << X << RK_reflcontainers__SetInt << mapres.second;
    //  return ExprError();
    //}
    //ClientPtrMap.erase(mapres);
    auto deleter_lambda = [&](QualType, ArrayRef<intptr_t>) -> ExprResult {
      delete reinterpret_cast<reflcontainers::SetInt *>(X);
      return ExprResult();
    };
    return new (Context) ReflectionTraitExpr(Context, KWLoc, RTK_prop, RK_reflcontainers__SetInt,
        0/*MemNum (dummy)*/, 1/*IsPtr (dummy)*/, Context.VoidTy, deleter_lambda,
        ArrayRef<Expr*>()/*RemArgs (dummy)*/, RParenLoc);
  }
  case RK_reflcontainers__SetStr: {
    //auto mapres = ClientPtrMap.find(X);
    //if (mapres == ClientPtrMap.end()) {
    //  Diag(KWLoc, diag::err_refldelete_nonalloc_ptr) << X << RK_reflcontainers__SetStr;
    //  return ExprError();
    //}
    //if (mapres.second != RK_reflcontainers__SetStr)
    //  Diag(KWLoc, diag::err_refldelete_different_ptr) << X << RK_reflcontainers__SetStr << mapres.second;
    //  return ExprError();
    //}
    //ClientPtrMap.erase(mapres);
    auto deleter_lambda = [&](QualType, ArrayRef<intptr_t>) -> ExprResult {
      delete reinterpret_cast<reflcontainers::SetStr *>(X);
      return ExprResult();
    };
    return new (Context) ReflectionTraitExpr(Context, KWLoc, RTK_prop, RK_reflcontainers__SetStr,
        0/*MemNum (dummy)*/, 1/*IsPtr (dummy)*/, Context.VoidTy, deleter_lambda,
        ArrayRef<Expr*>()/*RemArgs (dummy)*/, RParenLoc);
  }
  case RK_reflcontainers__MapIntStr: {
    //auto mapres = ClientPtrMap.find(X);
    //if (mapres == ClientPtrMap.end()) {
    //  Diag(KWLoc, diag::err_refldelete_nonalloc_ptr) << X << RK_reflcontainers__MapIntStr;
    //  return ExprError();
    //}
    //if (mapres.second != RK_reflcontainers__MapIntStr)
    //  Diag(KWLoc, diag::err_refldelete_different_ptr) << X << RK_reflcontainers__MapIntStr << mapres.second;
    //  return ExprError();
    //}
    //ClientPtrMap.erase(mapres);
    auto deleter_lambda = [&](QualType, ArrayRef<intptr_t>) -> ExprResult {
      delete reinterpret_cast<reflcontainers::MapIntStr *>(X);
      return ExprResult();
    };
    return new (Context) ReflectionTraitExpr(Context, KWLoc, RTK_prop, RK_reflcontainers__MapIntStr,
        0/*MemNum (dummy)*/, 1/*IsPtr (dummy)*/, Context.VoidTy, deleter_lambda,
        ArrayRef<Expr*>()/*RemArgs (dummy)*/, RParenLoc);
  }
  case RK_reflcontainers__MapStrInt: {
    //auto mapres = ClientPtrMap.find(X);
    //if (mapres == ClientPtrMap.end()) {
    //  Diag(KWLoc, diag::err_refldelete_nonalloc_ptr) << X << RK_reflcontainers__MapStrInt;
    //  return ExprError();
    //}
    //if (mapres.second != RK_reflcontainers__MapStrInt)
    //  Diag(KWLoc, diag::err_refldelete_different_ptr) << X << RK_reflcontainers__MapStrInt << mapres.second;
    //  return ExprError();
    //}
    //ClientPtrMap.erase(mapres);
    auto deleter_lambda = [&](QualType, ArrayRef<intptr_t>) -> ExprResult {
      delete reinterpret_cast<reflcontainers::MapStrInt *>(X);
      return ExprResult();
    };
    return new (Context) ReflectionTraitExpr(Context, KWLoc, RTK_prop, RK_reflcontainers__MapStrInt,
        0/*MemNum (dummy)*/, 1/*IsPtr (dummy)*/, Context.VoidTy, deleter_lambda,
        ArrayRef<Expr*>()/*RemArgs (dummy)*/, RParenLoc);
  }
  case RK_reflcontainers__MapStrStr: {
    //auto mapres = ClientPtrMap.find(X);
    //if (mapres == ClientPtrMap.end()) {
    //  Diag(KWLoc, diag::err_refldelete_nonalloc_ptr) << X << RK_reflcontainers__MapStrStr;
    //  return ExprError();
    //}
    //if (mapres.second != RK_reflcontainers__MapStrStr)
    //  Diag(KWLoc, diag::err_refldelete_different_ptr) << X << RK_reflcontainers__MapStrStr << mapres.second;
    //  return ExprError();
    //}
    //ClientPtrMap.erase(mapres);
    auto deleter_lambda = [&](QualType, ArrayRef<intptr_t>) -> ExprResult {
      delete reinterpret_cast<reflcontainers::MapStrStr *>(X);
      return ExprResult();
    };
    return new (Context) ReflectionTraitExpr(Context, KWLoc, RTK_prop, RK_reflcontainers__MapStrStr,
        0/*MemNum (dummy)*/, 1/*IsPtr (dummy)*/, Context.VoidTy, deleter_lambda,
        ArrayRef<Expr*>()/*RemArgs (dummy)*/, RParenLoc);
  }
  case RK_reflcontainers__MapIntInt: {
    //auto mapres = ClientPtrMap.find(X);
    //if (mapres == ClientPtrMap.end()) {
    //  Diag(KWLoc, diag::err_refldelete_nonalloc_ptr) << X << RK_reflcontainers__MapIntInt;
    //  return ExprError();
    //}
    //if (mapres.second != RK_reflcontainers__MapIntInt)
    //  Diag(KWLoc, diag::err_refldelete_different_ptr) << X << RK_reflcontainers__MapIntInt << mapres.second;
    //  return ExprError();
    //}
    //ClientPtrMap.erase(mapres);
    auto deleter_lambda = [&](QualType, ArrayRef<intptr_t>) -> ExprResult {
      delete reinterpret_cast<reflcontainers::MapIntInt *>(X);
      return ExprResult();
    };
    return new (Context) ReflectionTraitExpr(Context, KWLoc, RTK_prop, RK_reflcontainers__MapIntInt,
        0/*MemNum (dummy)*/, 1/*IsPtr (dummy)*/, Context.VoidTy, deleter_lambda,
        ArrayRef<Expr*>()/*RemArgs (dummy)*/, RParenLoc);
  }
