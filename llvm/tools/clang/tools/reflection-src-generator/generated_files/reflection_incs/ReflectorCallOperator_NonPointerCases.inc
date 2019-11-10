    case RK_llvm__APInt:
      return ReflectProp( MemNum, get_next_as<llvm::APInt>() );
    case RK_llvm__APInt__ms:
      return ReflectProp( MemNum, get_next_as<llvm::APInt::ms>() );
    case RK_llvm__APInt__mu:
      return ReflectProp( MemNum, get_next_as<llvm::APInt::mu>() );
    case RK_llvm__StringRef:
      return ReflectProp( MemNum, get_next_as<llvm::StringRef>() );
    case RK_llvm__APFloatBase:
      return ReflectProp( MemNum, get_next_as<llvm::APFloatBase>() );
    case RK_llvm__APFloat:
      return ReflectProp( MemNum, get_next_as<llvm::APFloat>() );
    case RK_llvm__APSInt:
      return ReflectProp( MemNum, get_next_as<llvm::APSInt>() );
    case RK_clang__APValue__LValueBase:
      return ReflectProp( MemNum, get_next_as<clang::APValue::LValueBase>() );
    case RK_clang__FileID:
      return ReflectProp( MemNum, get_next_as<clang::FileID>() );
    case RK_clang__SourceLocation:
      return ReflectProp( MemNum, get_next_as<clang::SourceLocation>() );
    case RK_clang__SourceRange:
      return ReflectProp( MemNum, get_next_as<clang::SourceRange>() );
    case RK_clang__CharSourceRange:
      return ReflectProp( MemNum, get_next_as<clang::CharSourceRange>() );
    case RK_clang__PresumedLoc:
      return ReflectProp( MemNum, get_next_as<clang::PresumedLoc>() );
    case RK_clang__FullSourceLoc:
      return ReflectProp( MemNum, get_next_as<clang::FullSourceLoc>() );
    case RK_clang__Selector:
      return ReflectProp( MemNum, get_next_as<clang::Selector>() );
    case RK_clang__DeclarationName:
      return ReflectProp( MemNum, get_next_as<clang::DeclarationName>() );
    case RK_clang__DeclarationNameInfo:
      return ReflectProp( MemNum, get_next_as<clang::DeclarationNameInfo>() );
    case RK_llvm__VersionTuple:
      return ReflectProp( MemNum, get_next_as<llvm::VersionTuple>() );
    case RK_clang__DeclContextLookupResult:
      return ReflectProp( MemNum, get_next_as<clang::DeclContextLookupResult>() );
    case RK_clang__CharUnits:
      return ReflectProp( MemNum, get_next_as<clang::CharUnits>() );
    case RK_llvm__MemoryBufferRef:
      return ReflectProp( MemNum, get_next_as<llvm::MemoryBufferRef>() );
    case RK_clang__ASTFileSignature:
      return ReflectProp( MemNum, get_next_as<clang::ASTFileSignature>() );
    case RK_clang__Module__Header:
      return ReflectProp( MemNum, get_next_as<clang::Module::Header>() );
    case RK_clang__Module__DirectoryName:
      return ReflectProp( MemNum, get_next_as<clang::Module::DirectoryName>() );
    case RK_clang__ExternalASTSource__MemoryBufferSizes:
      return ReflectProp( MemNum, get_next_as<clang::ExternalASTSource::MemoryBufferSizes>() );
    case RK_clang__NestedNameSpecifierLoc:
      return ReflectProp( MemNum, get_next_as<clang::NestedNameSpecifierLoc>() );
    case RK_clang__TemplateName:
      return ReflectProp( MemNum, get_next_as<clang::TemplateName>() );
    case RK_clang__LinkageInfo:
      return ReflectProp( MemNum, get_next_as<clang::LinkageInfo>() );
    case RK_clang__Qualifiers:
      return ReflectProp( MemNum, get_next_as<clang::Qualifiers>() );
    case RK_clang__SplitQualType:
      return ReflectProp( MemNum, get_next_as<clang::SplitQualType>() );
    case RK_clang__QualType:
      return ReflectProp( MemNum, get_next_as<clang::QualType>() );
    case RK_clang__FunctionType__ExtInfo:
      return ReflectProp( MemNum, get_next_as<clang::FunctionType::ExtInfo>() );
    case RK_clang__FunctionProtoType__ExtParameterInfo:
      return ReflectProp( MemNum, get_next_as<clang::FunctionProtoType::ExtParameterInfo>() );
    case RK_clang__FunctionProtoType__ExceptionSpecInfo:
      return ReflectProp( MemNum, get_next_as<clang::FunctionProtoType::ExceptionSpecInfo>() );
    case RK_clang__FunctionProtoType__ExtProtoInfo:
      return ReflectProp( MemNum, get_next_as<clang::FunctionProtoType::ExtProtoInfo>() );
    case RK_clang__TypeWithKeyword__CannotCastToThisType:
      return ReflectProp( MemNum, get_next_as<clang::TypeWithKeyword::CannotCastToThisType>() );
    case RK_clang__BlockDecl__Capture:
      return ReflectProp( MemNum, get_next_as<clang::BlockDecl::Capture>() );
    case RK_clang__DeclGroupRef:
      return ReflectProp( MemNum, get_next_as<clang::DeclGroupRef>() );
    case RK_clang__CapturedStmt__Capture:
      return ReflectProp( MemNum, get_next_as<clang::CapturedStmt::Capture>() );
    case RK_clang__TemplateArgument:
      return ReflectProp( MemNum, get_next_as<clang::TemplateArgument>() );
    case RK_clang__TemplateArgumentLocInfo:
      return ReflectProp( MemNum, get_next_as<clang::TemplateArgumentLocInfo>() );
    case RK_clang__TemplateArgumentLoc:
      return ReflectProp( MemNum, get_next_as<clang::TemplateArgumentLoc>() );
    case RK_clang__TypeLoc:
      return ReflectProp( MemNum, get_next_as<clang::TypeLoc>() );
    case RK_clang__UnqualTypeLoc:
      return ReflectProp( MemNum, get_next_as<clang::UnqualTypeLoc>() );
    case RK_clang__CommentOptions:
      return ReflectProp( MemNum, get_next_as<clang::CommentOptions>() );
    case RK_clang__ComparisonCategories:
      return ReflectProp( MemNum, get_next_as<clang::ComparisonCategories>() );
    case RK_llvm__Triple:
      return ReflectProp( MemNum, get_next_as<llvm::Triple>() );
    case RK_clang__ObjCRuntime:
      return ReflectProp( MemNum, get_next_as<clang::ObjCRuntime>() );
    case RK_clang__SanitizerSet:
      return ReflectProp( MemNum, get_next_as<clang::SanitizerSet>() );
    case RK_clang__FPOptions:
      return ReflectProp( MemNum, get_next_as<clang::FPOptions>() );
    case RK_clang__SrcMgr__FileInfo:
      return ReflectProp( MemNum, get_next_as<clang::SrcMgr::FileInfo>() );
    case RK_clang__SrcMgr__ExpansionInfo:
      return ReflectProp( MemNum, get_next_as<clang::SrcMgr::ExpansionInfo>() );
    case RK_clang__SourceManager__MemoryBufferSizes:
      return ReflectProp( MemNum, get_next_as<clang::SourceManager::MemoryBufferSizes>() );
    case RK_clang__RawCommentList:
      return ReflectProp( MemNum, get_next_as<clang::RawCommentList>() );
    case RK_clang__TargetCXXABI:
      return ReflectProp( MemNum, get_next_as<clang::TargetCXXABI>() );
    case RK_clang__OpenCLOptions:
      return ReflectProp( MemNum, get_next_as<clang::OpenCLOptions>() );
    case RK_clang__TypeInfo:
      return ReflectProp( MemNum, get_next_as<clang::TypeInfo>() );
    case RK_clang__DeclAccessPair:
      return ReflectProp( MemNum, get_next_as<clang::DeclAccessPair>() );
    case RK_clang__UnresolvedSetIterator:
      return ReflectProp( MemNum, get_next_as<clang::UnresolvedSetIterator>() );
    case RK_clang__DesignatedInitExpr__Designator:
      return ReflectProp( MemNum, get_next_as<clang::DesignatedInitExpr::Designator>() );
    case RK_clang__LambdaCapture:
      return ReflectProp( MemNum, get_next_as<clang::LambdaCapture>() );
    case RK_clang__CXXBaseSpecifier:
      return ReflectProp( MemNum, get_next_as<clang::CXXBaseSpecifier>() );
    case RK_clang__InheritedConstructor:
      return ReflectProp( MemNum, get_next_as<clang::InheritedConstructor>() );
    case RK_clang__OverloadExpr__FindResult:
      return ReflectProp( MemNum, get_next_as<clang::OverloadExpr::FindResult>() );
    case RK_clang__ObjCDictionaryElement:
      return ReflectProp( MemNum, get_next_as<clang::ObjCDictionaryElement>() );
