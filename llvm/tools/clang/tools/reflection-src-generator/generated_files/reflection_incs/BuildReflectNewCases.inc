  case RK_reflcontainers__VectorStr: {
    if (autodelete) return defaultConstructType(SemaRef, KWLoc,
        BuildReflectionType( *this, KWLoc, new (Context) reflcontainers::VectorStr(Context), (RefOvld<1>*)0 ) );
    auto res = new reflcontainers::VectorStr(Context);
    //ClientPtrMap.insert( {reinterpret_cast<intptr_t>(res), RK_reflcontainers__VectorStr} );
    return defaultConstructType(SemaRef, KWLoc, BuildReflectionType(*this, KWLoc, res) );
  }
  case RK_reflcontainers__VectorInt: {
    if (autodelete) return defaultConstructType(SemaRef, KWLoc,
        BuildReflectionType( *this, KWLoc, new (Context) reflcontainers::VectorInt(Context), (RefOvld<1>*)0 ) );
    auto res = new reflcontainers::VectorInt(Context);
    //ClientPtrMap.insert( {reinterpret_cast<intptr_t>(res), RK_reflcontainers__VectorInt} );
    return defaultConstructType(SemaRef, KWLoc, BuildReflectionType(*this, KWLoc, res) );
  }
  case RK_reflcontainers__SetInt: {
    if (autodelete) return defaultConstructType(SemaRef, KWLoc,
        BuildReflectionType( *this, KWLoc, new (Context) reflcontainers::SetInt(Context), (RefOvld<1>*)0 ) );
    auto res = new reflcontainers::SetInt(Context);
    //ClientPtrMap.insert( {reinterpret_cast<intptr_t>(res), RK_reflcontainers__SetInt} );
    return defaultConstructType(SemaRef, KWLoc, BuildReflectionType(*this, KWLoc, res) );
  }
  case RK_reflcontainers__SetStr: {
    if (autodelete) return defaultConstructType(SemaRef, KWLoc,
        BuildReflectionType( *this, KWLoc, new (Context) reflcontainers::SetStr(Context), (RefOvld<1>*)0 ) );
    auto res = new reflcontainers::SetStr(Context);
    //ClientPtrMap.insert( {reinterpret_cast<intptr_t>(res), RK_reflcontainers__SetStr} );
    return defaultConstructType(SemaRef, KWLoc, BuildReflectionType(*this, KWLoc, res) );
  }
  case RK_reflcontainers__MapIntStr: {
    if (autodelete) return defaultConstructType(SemaRef, KWLoc,
        BuildReflectionType( *this, KWLoc, new (Context) reflcontainers::MapIntStr(Context), (RefOvld<1>*)0 ) );
    auto res = new reflcontainers::MapIntStr(Context);
    //ClientPtrMap.insert( {reinterpret_cast<intptr_t>(res), RK_reflcontainers__MapIntStr} );
    return defaultConstructType(SemaRef, KWLoc, BuildReflectionType(*this, KWLoc, res) );
  }
  case RK_reflcontainers__MapStrInt: {
    if (autodelete) return defaultConstructType(SemaRef, KWLoc,
        BuildReflectionType( *this, KWLoc, new (Context) reflcontainers::MapStrInt(Context), (RefOvld<1>*)0 ) );
    auto res = new reflcontainers::MapStrInt(Context);
    //ClientPtrMap.insert( {reinterpret_cast<intptr_t>(res), RK_reflcontainers__MapStrInt} );
    return defaultConstructType(SemaRef, KWLoc, BuildReflectionType(*this, KWLoc, res) );
  }
  case RK_reflcontainers__MapStrStr: {
    if (autodelete) return defaultConstructType(SemaRef, KWLoc,
        BuildReflectionType( *this, KWLoc, new (Context) reflcontainers::MapStrStr(Context), (RefOvld<1>*)0 ) );
    auto res = new reflcontainers::MapStrStr(Context);
    //ClientPtrMap.insert( {reinterpret_cast<intptr_t>(res), RK_reflcontainers__MapStrStr} );
    return defaultConstructType(SemaRef, KWLoc, BuildReflectionType(*this, KWLoc, res) );
  }
  case RK_reflcontainers__MapIntInt: {
    if (autodelete) return defaultConstructType(SemaRef, KWLoc,
        BuildReflectionType( *this, KWLoc, new (Context) reflcontainers::MapIntInt(Context), (RefOvld<1>*)0 ) );
    auto res = new reflcontainers::MapIntInt(Context);
    //ClientPtrMap.insert( {reinterpret_cast<intptr_t>(res), RK_reflcontainers__MapIntInt} );
    return defaultConstructType(SemaRef, KWLoc, BuildReflectionType(*this, KWLoc, res) );
  }
