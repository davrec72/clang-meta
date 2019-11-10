    case RK_llvm__APInt:
      return ReflectProp( MemNum, reinterpret_cast<llvm::APInt *>(Val) );
    case RK_llvm__APFloatBase:
      return ReflectProp( MemNum, reinterpret_cast<llvm::APFloatBase *>(Val) );
    case RK_llvm__APFloat:
      return ReflectProp( MemNum, reinterpret_cast<llvm::APFloat *>(Val) );
    case RK_llvm__APSInt:
      return ReflectProp( MemNum, reinterpret_cast<llvm::APSInt *>(Val) );
    case RK_clang__APValue:
      return ReflectProp( MemNum, reinterpret_cast<clang::APValue *>(Val) );
    case RK_clang__DiagnosticOptions:
      return ReflectProp( MemNum, reinterpret_cast<clang::DiagnosticOptions *>(Val) );
    case RK_clang__SourceLocation:
      return ReflectProp( MemNum, reinterpret_cast<clang::SourceLocation *>(Val) );
    case RK_clang__FullSourceLoc:
      return ReflectProp( MemNum, reinterpret_cast<clang::FullSourceLoc *>(Val) );
    case RK_llvm__DebugEpochBase:
      return ReflectProp( MemNum, reinterpret_cast<llvm::DebugEpochBase *>(Val) );
    case RK_clang__DiagnosticsEngine:
      return ReflectProp( MemNum, reinterpret_cast<clang::DiagnosticsEngine *>(Val) );
    case RK_clang__DiagnosticConsumer:
      return ReflectProp( MemNum, reinterpret_cast<clang::DiagnosticConsumer *>(Val) );
    case RK_clang__IdentifierInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::IdentifierInfo *>(Val) );
    case RK_clang__IdentifierInfoLookup:
      return ReflectProp( MemNum, reinterpret_cast<clang::IdentifierInfoLookup *>(Val) );
    case RK_clang__IdentifierTable:
      return ReflectProp( MemNum, reinterpret_cast<clang::IdentifierTable *>(Val) );
    case RK_clang__SelectorTable:
      return ReflectProp( MemNum, reinterpret_cast<clang::SelectorTable *>(Val) );
    case RK_clang__PartialDiagnostic:
      return ReflectProp( MemNum, reinterpret_cast<clang::PartialDiagnostic *>(Val) );
    case RK_clang__DeclarationNameLoc:
      return ReflectProp( MemNum, reinterpret_cast<clang::DeclarationNameLoc *>(Val) );
    case RK_clang__DeclarationNameInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::DeclarationNameInfo *>(Val) );
    case RK_llvm__VersionTuple:
      return ReflectProp( MemNum, reinterpret_cast<llvm::VersionTuple *>(Val) );
    case RK_clang__Decl:
      return ReflectProp( MemNum, reinterpret_cast<clang::Decl *>(Val) );
    case RK_clang__DeclContext:
      return ReflectProp( MemNum, reinterpret_cast<clang::DeclContext *>(Val) );
    case RK_clang__CharUnits:
      return ReflectProp( MemNum, reinterpret_cast<clang::CharUnits *>(Val) );
    case RK_clang__FileSystemOptions:
      return ReflectProp( MemNum, reinterpret_cast<clang::FileSystemOptions *>(Val) );
    case RK_llvm__sys__fs__UniqueID:
      return ReflectProp( MemNum, reinterpret_cast<llvm::sys::fs::UniqueID *>(Val) );
    case RK_llvm__MemoryBuffer:
      return ReflectProp( MemNum, reinterpret_cast<llvm::MemoryBuffer *>(Val) );
    case RK_clang__DirectoryEntry:
      return ReflectProp( MemNum, reinterpret_cast<clang::DirectoryEntry *>(Val) );
    case RK_clang__FileEntry:
      return ReflectProp( MemNum, reinterpret_cast<clang::FileEntry *>(Val) );
    case RK_clang__FileManager:
      return ReflectProp( MemNum, reinterpret_cast<clang::FileManager *>(Val) );
    case RK_clang__Module:
      return ReflectProp( MemNum, reinterpret_cast<clang::Module *>(Val) );
    case RK_clang__ExternalASTSource:
      return ReflectProp( MemNum, reinterpret_cast<clang::ExternalASTSource *>(Val) );
    case RK_llvm__FoldingSetBase__Node:
      return ReflectProp( MemNum, reinterpret_cast<llvm::FoldingSetBase::Node *>(Val) );
    case RK_clang__NestedNameSpecifier:
      return ReflectProp( MemNum, reinterpret_cast<clang::NestedNameSpecifier *>(Val) );
    case RK_clang__UncommonTemplateNameStorage:
      return ReflectProp( MemNum, reinterpret_cast<clang::UncommonTemplateNameStorage *>(Val) );
    case RK_clang__OverloadedTemplateStorage:
      return ReflectProp( MemNum, reinterpret_cast<clang::OverloadedTemplateStorage *>(Val) );
    case RK_clang__SubstTemplateTemplateParmPackStorage:
      return ReflectProp( MemNum, reinterpret_cast<clang::SubstTemplateTemplateParmPackStorage *>(Val) );
    case RK_clang__SubstTemplateTemplateParmStorage:
      return ReflectProp( MemNum, reinterpret_cast<clang::SubstTemplateTemplateParmStorage *>(Val) );
    case RK_clang__QualifiedTemplateName:
      return ReflectProp( MemNum, reinterpret_cast<clang::QualifiedTemplateName *>(Val) );
    case RK_clang__DependentTemplateName:
      return ReflectProp( MemNum, reinterpret_cast<clang::DependentTemplateName *>(Val) );
    case RK_clang__ExtQualsTypeCommonBase:
      return ReflectProp( MemNum, reinterpret_cast<clang::ExtQualsTypeCommonBase *>(Val) );
    case RK_clang__Type:
      return ReflectProp( MemNum, reinterpret_cast<clang::Type *>(Val) );
    case RK_clang__BuiltinType:
      return ReflectProp( MemNum, reinterpret_cast<clang::BuiltinType *>(Val) );
    case RK_clang__ComplexType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ComplexType *>(Val) );
    case RK_clang__ParenType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ParenType *>(Val) );
    case RK_clang__PointerType:
      return ReflectProp( MemNum, reinterpret_cast<clang::PointerType *>(Val) );
    case RK_clang__AdjustedType:
      return ReflectProp( MemNum, reinterpret_cast<clang::AdjustedType *>(Val) );
    case RK_clang__DecayedType:
      return ReflectProp( MemNum, reinterpret_cast<clang::DecayedType *>(Val) );
    case RK_clang__BlockPointerType:
      return ReflectProp( MemNum, reinterpret_cast<clang::BlockPointerType *>(Val) );
    case RK_clang__ReferenceType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ReferenceType *>(Val) );
    case RK_clang__LValueReferenceType:
      return ReflectProp( MemNum, reinterpret_cast<clang::LValueReferenceType *>(Val) );
    case RK_clang__RValueReferenceType:
      return ReflectProp( MemNum, reinterpret_cast<clang::RValueReferenceType *>(Val) );
    case RK_clang__MemberPointerType:
      return ReflectProp( MemNum, reinterpret_cast<clang::MemberPointerType *>(Val) );
    case RK_clang__ArrayType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ArrayType *>(Val) );
    case RK_clang__ConstantArrayType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ConstantArrayType *>(Val) );
    case RK_clang__IncompleteArrayType:
      return ReflectProp( MemNum, reinterpret_cast<clang::IncompleteArrayType *>(Val) );
    case RK_clang__VariableArrayType:
      return ReflectProp( MemNum, reinterpret_cast<clang::VariableArrayType *>(Val) );
    case RK_clang__DependentSizedArrayType:
      return ReflectProp( MemNum, reinterpret_cast<clang::DependentSizedArrayType *>(Val) );
    case RK_clang__DependentAddressSpaceType:
      return ReflectProp( MemNum, reinterpret_cast<clang::DependentAddressSpaceType *>(Val) );
    case RK_clang__DependentSizedExtVectorType:
      return ReflectProp( MemNum, reinterpret_cast<clang::DependentSizedExtVectorType *>(Val) );
    case RK_clang__VectorType:
      return ReflectProp( MemNum, reinterpret_cast<clang::VectorType *>(Val) );
    case RK_clang__DependentVectorType:
      return ReflectProp( MemNum, reinterpret_cast<clang::DependentVectorType *>(Val) );
    case RK_clang__ExtVectorType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ExtVectorType *>(Val) );
    case RK_clang__FunctionType:
      return ReflectProp( MemNum, reinterpret_cast<clang::FunctionType *>(Val) );
    case RK_clang__FunctionNoProtoType:
      return ReflectProp( MemNum, reinterpret_cast<clang::FunctionNoProtoType *>(Val) );
    case RK_clang__FunctionProtoType:
      return ReflectProp( MemNum, reinterpret_cast<clang::FunctionProtoType *>(Val) );
    case RK_clang__FunctionProtoType__ExtParameterInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::FunctionProtoType::ExtParameterInfo *>(Val) );
    case RK_clang__UnresolvedUsingType:
      return ReflectProp( MemNum, reinterpret_cast<clang::UnresolvedUsingType *>(Val) );
    case RK_clang__TypedefType:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypedefType *>(Val) );
    case RK_clang__TypeOfExprType:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypeOfExprType *>(Val) );
    case RK_clang__TypeOfType:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypeOfType *>(Val) );
    case RK_clang__DecltypeType:
      return ReflectProp( MemNum, reinterpret_cast<clang::DecltypeType *>(Val) );
    case RK_clang__ReflectedType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ReflectedType *>(Val) );
    case RK_clang__UnaryTransformType:
      return ReflectProp( MemNum, reinterpret_cast<clang::UnaryTransformType *>(Val) );
    case RK_clang__TagType:
      return ReflectProp( MemNum, reinterpret_cast<clang::TagType *>(Val) );
    case RK_clang__RecordType:
      return ReflectProp( MemNum, reinterpret_cast<clang::RecordType *>(Val) );
    case RK_clang__EnumType:
      return ReflectProp( MemNum, reinterpret_cast<clang::EnumType *>(Val) );
    case RK_clang__AttributedType:
      return ReflectProp( MemNum, reinterpret_cast<clang::AttributedType *>(Val) );
    case RK_clang__TemplateTypeParmType:
      return ReflectProp( MemNum, reinterpret_cast<clang::TemplateTypeParmType *>(Val) );
    case RK_clang__SubstTemplateTypeParmType:
      return ReflectProp( MemNum, reinterpret_cast<clang::SubstTemplateTypeParmType *>(Val) );
    case RK_clang__SubstTemplateTypeParmPackType:
      return ReflectProp( MemNum, reinterpret_cast<clang::SubstTemplateTypeParmPackType *>(Val) );
    case RK_clang__DeducedType:
      return ReflectProp( MemNum, reinterpret_cast<clang::DeducedType *>(Val) );
    case RK_clang__AutoType:
      return ReflectProp( MemNum, reinterpret_cast<clang::AutoType *>(Val) );
    case RK_clang__DeducedTemplateSpecializationType:
      return ReflectProp( MemNum, reinterpret_cast<clang::DeducedTemplateSpecializationType *>(Val) );
    case RK_clang__TemplateSpecializationType:
      return ReflectProp( MemNum, reinterpret_cast<clang::TemplateSpecializationType *>(Val) );
    case RK_clang__InjectedClassNameType:
      return ReflectProp( MemNum, reinterpret_cast<clang::InjectedClassNameType *>(Val) );
    case RK_clang__TypeWithKeyword:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypeWithKeyword *>(Val) );
    case RK_clang__ElaboratedType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ElaboratedType *>(Val) );
    case RK_clang__DependentNameType:
      return ReflectProp( MemNum, reinterpret_cast<clang::DependentNameType *>(Val) );
    case RK_clang__DependentTemplateSpecializationType:
      return ReflectProp( MemNum, reinterpret_cast<clang::DependentTemplateSpecializationType *>(Val) );
    case RK_clang__PackExpansionType:
      return ReflectProp( MemNum, reinterpret_cast<clang::PackExpansionType *>(Val) );
    case RK_clang__ObjCTypeParamType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCTypeParamType *>(Val) );
    case RK_clang__ObjCObjectType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCObjectType *>(Val) );
    case RK_clang__ObjCInterfaceType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCInterfaceType *>(Val) );
    case RK_clang__ObjCObjectPointerType:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCObjectPointerType *>(Val) );
    case RK_clang__AtomicType:
      return ReflectProp( MemNum, reinterpret_cast<clang::AtomicType *>(Val) );
    case RK_clang__PipeType:
      return ReflectProp( MemNum, reinterpret_cast<clang::PipeType *>(Val) );
    case RK_clang__TypeSourceInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypeSourceInfo *>(Val) );
    case RK_clang__TranslationUnitDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::TranslationUnitDecl *>(Val) );
    case RK_clang__PragmaCommentDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::PragmaCommentDecl *>(Val) );
    case RK_clang__PragmaDetectMismatchDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::PragmaDetectMismatchDecl *>(Val) );
    case RK_clang__ExternCContextDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ExternCContextDecl *>(Val) );
    case RK_clang__NamedDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::NamedDecl *>(Val) );
    case RK_clang__LabelDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::LabelDecl *>(Val) );
    case RK_clang__NamespaceDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::NamespaceDecl *>(Val) );
    case RK_clang__ValueDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ValueDecl *>(Val) );
    case RK_clang__DeclaratorDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::DeclaratorDecl *>(Val) );
    case RK_clang__EvaluatedStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::EvaluatedStmt *>(Val) );
    case RK_clang__VarDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::VarDecl *>(Val) );
    case RK_clang__ImplicitParamDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ImplicitParamDecl *>(Val) );
    case RK_clang__ParmVarDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ParmVarDecl *>(Val) );
    case RK_clang__FunctionDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::FunctionDecl *>(Val) );
    case RK_clang__FieldDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::FieldDecl *>(Val) );
    case RK_clang__EnumConstantDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::EnumConstantDecl *>(Val) );
    case RK_clang__IndirectFieldDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::IndirectFieldDecl *>(Val) );
    case RK_clang__TypeDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypeDecl *>(Val) );
    case RK_clang__TypedefNameDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypedefNameDecl *>(Val) );
    case RK_clang__TypedefDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypedefDecl *>(Val) );
    case RK_clang__TypeAliasDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypeAliasDecl *>(Val) );
    case RK_clang__TagDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::TagDecl *>(Val) );
    case RK_clang__EnumDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::EnumDecl *>(Val) );
    case RK_clang__RecordDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::RecordDecl *>(Val) );
    case RK_clang__FileScopeAsmDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::FileScopeAsmDecl *>(Val) );
    case RK_clang__BlockDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::BlockDecl *>(Val) );
    case RK_clang__CapturedDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::CapturedDecl *>(Val) );
    case RK_clang__ImportDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ImportDecl *>(Val) );
    case RK_clang__ExportDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ExportDecl *>(Val) );
    case RK_clang__EmptyDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::EmptyDecl *>(Val) );
    case RK_clang__DeclGroup:
      return ReflectProp( MemNum, reinterpret_cast<clang::DeclGroup *>(Val) );
    case RK_clang__Stmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::Stmt *>(Val) );
    case RK_clang__DeclStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::DeclStmt *>(Val) );
    case RK_clang__NullStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::NullStmt *>(Val) );
    case RK_clang__CompoundStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CompoundStmt *>(Val) );
    case RK_clang__SwitchCase:
      return ReflectProp( MemNum, reinterpret_cast<clang::SwitchCase *>(Val) );
    case RK_clang__CaseStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CaseStmt *>(Val) );
    case RK_clang__DefaultStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::DefaultStmt *>(Val) );
    case RK_clang__LabelStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::LabelStmt *>(Val) );
    case RK_clang__AttributedStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::AttributedStmt *>(Val) );
    case RK_clang__IfStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::IfStmt *>(Val) );
    case RK_clang__SwitchStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::SwitchStmt *>(Val) );
    case RK_clang__WhileStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::WhileStmt *>(Val) );
    case RK_clang__DoStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::DoStmt *>(Val) );
    case RK_clang__ForStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::ForStmt *>(Val) );
    case RK_clang__GotoStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::GotoStmt *>(Val) );
    case RK_clang__IndirectGotoStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::IndirectGotoStmt *>(Val) );
    case RK_clang__ContinueStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::ContinueStmt *>(Val) );
    case RK_clang__BreakStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::BreakStmt *>(Val) );
    case RK_clang__ReturnStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::ReturnStmt *>(Val) );
    case RK_clang__AsmStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::AsmStmt *>(Val) );
    case RK_clang__GCCAsmStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::GCCAsmStmt *>(Val) );
    case RK_clang__MSAsmStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::MSAsmStmt *>(Val) );
    case RK_clang__SEHExceptStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::SEHExceptStmt *>(Val) );
    case RK_clang__SEHFinallyStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::SEHFinallyStmt *>(Val) );
    case RK_clang__SEHTryStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::SEHTryStmt *>(Val) );
    case RK_clang__SEHLeaveStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::SEHLeaveStmt *>(Val) );
    case RK_clang__CapturedStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CapturedStmt *>(Val) );
    case RK_clang__TemplateArgument:
      return ReflectProp( MemNum, reinterpret_cast<clang::TemplateArgument *>(Val) );
    case RK_clang__TemplateArgumentLoc:
      return ReflectProp( MemNum, reinterpret_cast<clang::TemplateArgumentLoc *>(Val) );
    case RK_clang__TemplateArgumentListInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::TemplateArgumentListInfo *>(Val) );
    case RK_clang__ASTTemplateArgumentListInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::ASTTemplateArgumentListInfo *>(Val) );
    case RK_clang__comments__CommandInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::comments::CommandInfo *>(Val) );
    case RK_clang__comments__CommandTraits:
      return ReflectProp( MemNum, reinterpret_cast<clang::comments::CommandTraits *>(Val) );
    case RK_clang__ComparisonCategoryInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::ComparisonCategoryInfo *>(Val) );
    case RK_clang__ComparisonCategoryInfo__ValueInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::ComparisonCategoryInfo::ValueInfo *>(Val) );
    case RK_llvm__Triple:
      return ReflectProp( MemNum, reinterpret_cast<llvm::Triple *>(Val) );
    case RK_clang__LangOptionsBase:
      return ReflectProp( MemNum, reinterpret_cast<clang::LangOptionsBase *>(Val) );
    case RK_clang__LangOptions:
      return ReflectProp( MemNum, reinterpret_cast<clang::LangOptions *>(Val) );
    case RK_clang__PrintingPolicy:
      return ReflectProp( MemNum, reinterpret_cast<clang::PrintingPolicy *>(Val) );
    case RK_clang__SrcMgr__ContentCache:
      return ReflectProp( MemNum, reinterpret_cast<clang::SrcMgr::ContentCache *>(Val) );
    case RK_clang__SrcMgr__FileInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::SrcMgr::FileInfo *>(Val) );
    case RK_clang__SrcMgr__ExpansionInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::SrcMgr::ExpansionInfo *>(Val) );
    case RK_clang__SrcMgr__SLocEntry:
      return ReflectProp( MemNum, reinterpret_cast<clang::SrcMgr::SLocEntry *>(Val) );
    case RK_clang__SourceManager:
      return ReflectProp( MemNum, reinterpret_cast<clang::SourceManager *>(Val) );
    case RK_clang__RawComment:
      return ReflectProp( MemNum, reinterpret_cast<clang::RawComment *>(Val) );
    case RK_clang__SanitizerBlacklist:
      return ReflectProp( MemNum, reinterpret_cast<clang::SanitizerBlacklist *>(Val) );
    case RK_clang__OpenCLOptions:
      return ReflectProp( MemNum, reinterpret_cast<clang::OpenCLOptions *>(Val) );
    case RK_clang__TargetOptions:
      return ReflectProp( MemNum, reinterpret_cast<clang::TargetOptions *>(Val) );
    case RK_llvm__Type:
      return ReflectProp( MemNum, reinterpret_cast<llvm::Type *>(Val) );
    case RK_llvm__CompositeType:
      return ReflectProp( MemNum, reinterpret_cast<llvm::CompositeType *>(Val) );
    case RK_llvm__StructType:
      return ReflectProp( MemNum, reinterpret_cast<llvm::StructType *>(Val) );
    case RK_llvm__PointerType:
      return ReflectProp( MemNum, reinterpret_cast<llvm::PointerType *>(Val) );
    case RK_llvm__DataLayout:
      return ReflectProp( MemNum, reinterpret_cast<llvm::DataLayout *>(Val) );
    case RK_llvm__StructLayout:
      return ReflectProp( MemNum, reinterpret_cast<llvm::StructLayout *>(Val) );
    case RK_clang__TargetInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::TargetInfo *>(Val) );
    case RK_clang__XRayFunctionFilter:
      return ReflectProp( MemNum, reinterpret_cast<clang::XRayFunctionFilter *>(Val) );
    case RK_clang__ASTContext:
      return ReflectProp( MemNum, reinterpret_cast<clang::ASTContext *>(Val) );
    case RK_clang__DeclAccessPair:
      return ReflectProp( MemNum, reinterpret_cast<clang::DeclAccessPair *>(Val) );
    case RK_clang__Expr:
      return ReflectProp( MemNum, reinterpret_cast<clang::Expr *>(Val) );
    case RK_clang__OpaqueValueExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::OpaqueValueExpr *>(Val) );
    case RK_clang__DeclRefExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::DeclRefExpr *>(Val) );
    case RK_clang__PredefinedExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::PredefinedExpr *>(Val) );
    case RK_clang__APIntStorage:
      return ReflectProp( MemNum, reinterpret_cast<clang::APIntStorage *>(Val) );
    case RK_clang__IntegerLiteral:
      return ReflectProp( MemNum, reinterpret_cast<clang::IntegerLiteral *>(Val) );
    case RK_clang__FixedPointLiteral:
      return ReflectProp( MemNum, reinterpret_cast<clang::FixedPointLiteral *>(Val) );
    case RK_clang__CharacterLiteral:
      return ReflectProp( MemNum, reinterpret_cast<clang::CharacterLiteral *>(Val) );
    case RK_clang__FloatingLiteral:
      return ReflectProp( MemNum, reinterpret_cast<clang::FloatingLiteral *>(Val) );
    case RK_clang__ImaginaryLiteral:
      return ReflectProp( MemNum, reinterpret_cast<clang::ImaginaryLiteral *>(Val) );
    case RK_clang__StringLiteral:
      return ReflectProp( MemNum, reinterpret_cast<clang::StringLiteral *>(Val) );
    case RK_clang__ParenExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ParenExpr *>(Val) );
    case RK_clang__UnaryOperator:
      return ReflectProp( MemNum, reinterpret_cast<clang::UnaryOperator *>(Val) );
    case RK_clang__OffsetOfNode:
      return ReflectProp( MemNum, reinterpret_cast<clang::OffsetOfNode *>(Val) );
    case RK_clang__OffsetOfExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::OffsetOfExpr *>(Val) );
    case RK_clang__UnaryExprOrTypeTraitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::UnaryExprOrTypeTraitExpr *>(Val) );
    case RK_clang__ArraySubscriptExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ArraySubscriptExpr *>(Val) );
    case RK_clang__CallExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CallExpr *>(Val) );
    case RK_clang__MemberExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::MemberExpr *>(Val) );
    case RK_clang__CompoundLiteralExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CompoundLiteralExpr *>(Val) );
    case RK_clang__CastExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CastExpr *>(Val) );
    case RK_clang__ImplicitCastExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ImplicitCastExpr *>(Val) );
    case RK_clang__ExplicitCastExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ExplicitCastExpr *>(Val) );
    case RK_clang__CStyleCastExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CStyleCastExpr *>(Val) );
    case RK_clang__BinaryOperator:
      return ReflectProp( MemNum, reinterpret_cast<clang::BinaryOperator *>(Val) );
    case RK_clang__CompoundAssignOperator:
      return ReflectProp( MemNum, reinterpret_cast<clang::CompoundAssignOperator *>(Val) );
    case RK_clang__AbstractConditionalOperator:
      return ReflectProp( MemNum, reinterpret_cast<clang::AbstractConditionalOperator *>(Val) );
    case RK_clang__ConditionalOperator:
      return ReflectProp( MemNum, reinterpret_cast<clang::ConditionalOperator *>(Val) );
    case RK_clang__BinaryConditionalOperator:
      return ReflectProp( MemNum, reinterpret_cast<clang::BinaryConditionalOperator *>(Val) );
    case RK_clang__AddrLabelExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::AddrLabelExpr *>(Val) );
    case RK_clang__StmtExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::StmtExpr *>(Val) );
    case RK_clang__ShuffleVectorExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ShuffleVectorExpr *>(Val) );
    case RK_clang__ConvertVectorExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ConvertVectorExpr *>(Val) );
    case RK_clang__ChooseExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ChooseExpr *>(Val) );
    case RK_clang__GNUNullExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::GNUNullExpr *>(Val) );
    case RK_clang__VAArgExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::VAArgExpr *>(Val) );
    case RK_clang__InitListExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::InitListExpr *>(Val) );
    case RK_clang__DesignatedInitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::DesignatedInitExpr *>(Val) );
    case RK_clang__DesignatedInitExpr__Designator:
      return ReflectProp( MemNum, reinterpret_cast<clang::DesignatedInitExpr::Designator *>(Val) );
    case RK_clang__NoInitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::NoInitExpr *>(Val) );
    case RK_clang__DesignatedInitUpdateExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::DesignatedInitUpdateExpr *>(Val) );
    case RK_clang__ArrayInitLoopExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ArrayInitLoopExpr *>(Val) );
    case RK_clang__ArrayInitIndexExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ArrayInitIndexExpr *>(Val) );
    case RK_clang__ImplicitValueInitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ImplicitValueInitExpr *>(Val) );
    case RK_clang__ParenListExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ParenListExpr *>(Val) );
    case RK_clang__GenericSelectionExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::GenericSelectionExpr *>(Val) );
    case RK_clang__ExtVectorElementExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ExtVectorElementExpr *>(Val) );
    case RK_clang__BlockExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::BlockExpr *>(Val) );
    case RK_clang__AsTypeExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::AsTypeExpr *>(Val) );
    case RK_clang__PseudoObjectExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::PseudoObjectExpr *>(Val) );
    case RK_clang__AtomicExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::AtomicExpr *>(Val) );
    case RK_clang__TypoExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypoExpr *>(Val) );
    case RK_clang__Attr:
      return ReflectProp( MemNum, reinterpret_cast<clang::Attr *>(Val) );
    case RK_clang__InheritableAttr:
      return ReflectProp( MemNum, reinterpret_cast<clang::InheritableAttr *>(Val) );
    case RK_clang__ExternalSourceSymbolAttr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ExternalSourceSymbolAttr *>(Val) );
    case RK_clang__MSInheritanceAttr:
      return ReflectProp( MemNum, reinterpret_cast<clang::MSInheritanceAttr *>(Val) );
    case RK_clang__MSVtorDispAttr:
      return ReflectProp( MemNum, reinterpret_cast<clang::MSVtorDispAttr *>(Val) );
    case RK_clang__LambdaCapture:
      return ReflectProp( MemNum, reinterpret_cast<clang::LambdaCapture *>(Val) );
    case RK_clang__AccessSpecDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::AccessSpecDecl *>(Val) );
    case RK_clang__CXXBaseSpecifier:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXBaseSpecifier *>(Val) );
    case RK_clang__CXXRecordDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXRecordDecl *>(Val) );
    case RK_clang__CXXDeductionGuideDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXDeductionGuideDecl *>(Val) );
    case RK_clang__CXXMethodDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXMethodDecl *>(Val) );
    case RK_clang__CXXCtorInitializer:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXCtorInitializer *>(Val) );
    case RK_clang__CXXConstructorDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXConstructorDecl *>(Val) );
    case RK_clang__CXXDestructorDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXDestructorDecl *>(Val) );
    case RK_clang__CXXConversionDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXConversionDecl *>(Val) );
    case RK_clang__LinkageSpecDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::LinkageSpecDecl *>(Val) );
    case RK_clang__UsingDirectiveDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::UsingDirectiveDecl *>(Val) );
    case RK_clang__NamespaceAliasDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::NamespaceAliasDecl *>(Val) );
    case RK_clang__UsingShadowDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::UsingShadowDecl *>(Val) );
    case RK_clang__ConstructorUsingShadowDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ConstructorUsingShadowDecl *>(Val) );
    case RK_clang__UsingDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::UsingDecl *>(Val) );
    case RK_clang__UsingPackDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::UsingPackDecl *>(Val) );
    case RK_clang__UnresolvedUsingValueDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::UnresolvedUsingValueDecl *>(Val) );
    case RK_clang__UnresolvedUsingTypenameDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::UnresolvedUsingTypenameDecl *>(Val) );
    case RK_clang__StaticAssertDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::StaticAssertDecl *>(Val) );
    case RK_clang__BindingDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::BindingDecl *>(Val) );
    case RK_clang__DecompositionDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::DecompositionDecl *>(Val) );
    case RK_clang__MSPropertyDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::MSPropertyDecl *>(Val) );
    case RK_clang__ConstexprDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ConstexprDecl *>(Val) );
    case RK_clang__TemplateParameterList:
      return ReflectProp( MemNum, reinterpret_cast<clang::TemplateParameterList *>(Val) );
    case RK_clang__TemplateArgumentList:
      return ReflectProp( MemNum, reinterpret_cast<clang::TemplateArgumentList *>(Val) );
    case RK_clang__TemplateDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::TemplateDecl *>(Val) );
    case RK_clang__FunctionTemplateSpecializationInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::FunctionTemplateSpecializationInfo *>(Val) );
    case RK_clang__MemberSpecializationInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::MemberSpecializationInfo *>(Val) );
    case RK_clang__DependentFunctionTemplateSpecializationInfo:
      return ReflectProp( MemNum, reinterpret_cast<clang::DependentFunctionTemplateSpecializationInfo *>(Val) );
    case RK_clang__RedeclarableTemplateDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::RedeclarableTemplateDecl *>(Val) );
    case RK_clang__FunctionTemplateDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::FunctionTemplateDecl *>(Val) );
    case RK_clang__TemplateTypeParmDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::TemplateTypeParmDecl *>(Val) );
    case RK_clang__NonTypeTemplateParmDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::NonTypeTemplateParmDecl *>(Val) );
    case RK_clang__TemplateTemplateParmDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::TemplateTemplateParmDecl *>(Val) );
    case RK_clang__BuiltinTemplateDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::BuiltinTemplateDecl *>(Val) );
    case RK_clang__ClassTemplateSpecializationDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ClassTemplateSpecializationDecl *>(Val) );
    case RK_clang__ClassTemplatePartialSpecializationDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ClassTemplatePartialSpecializationDecl *>(Val) );
    case RK_clang__ClassTemplateDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ClassTemplateDecl *>(Val) );
    case RK_clang__FriendTemplateDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::FriendTemplateDecl *>(Val) );
    case RK_clang__TypeAliasTemplateDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypeAliasTemplateDecl *>(Val) );
    case RK_clang__ClassScopeFunctionSpecializationDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ClassScopeFunctionSpecializationDecl *>(Val) );
    case RK_clang__VarTemplateSpecializationDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::VarTemplateSpecializationDecl *>(Val) );
    case RK_clang__VarTemplatePartialSpecializationDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::VarTemplatePartialSpecializationDecl *>(Val) );
    case RK_clang__VarTemplateDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::VarTemplateDecl *>(Val) );
    case RK_clang__FriendDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::FriendDecl *>(Val) );
    case RK_clang__ObjCListBase:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCListBase *>(Val) );
    case RK_clang__ObjCProtocolList:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCProtocolList *>(Val) );
    case RK_clang__ObjCMethodDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCMethodDecl *>(Val) );
    case RK_clang__ObjCTypeParamDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCTypeParamDecl *>(Val) );
    case RK_clang__ObjCTypeParamList:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCTypeParamList *>(Val) );
    case RK_clang__ObjCPropertyDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCPropertyDecl *>(Val) );
    case RK_clang__ObjCContainerDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCContainerDecl *>(Val) );
    case RK_clang__ObjCInterfaceDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCInterfaceDecl *>(Val) );
    case RK_clang__ObjCIvarDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCIvarDecl *>(Val) );
    case RK_clang__ObjCAtDefsFieldDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCAtDefsFieldDecl *>(Val) );
    case RK_clang__ObjCProtocolDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCProtocolDecl *>(Val) );
    case RK_clang__ObjCCategoryDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCCategoryDecl *>(Val) );
    case RK_clang__ObjCImplDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCImplDecl *>(Val) );
    case RK_clang__ObjCCategoryImplDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCCategoryImplDecl *>(Val) );
    case RK_clang__ObjCImplementationDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCImplementationDecl *>(Val) );
    case RK_clang__ObjCCompatibleAliasDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCCompatibleAliasDecl *>(Val) );
    case RK_clang__ObjCPropertyImplDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCPropertyImplDecl *>(Val) );
    case RK_clang__OMPThreadPrivateDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPThreadPrivateDecl *>(Val) );
    case RK_clang__OMPDeclareReductionDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPDeclareReductionDecl *>(Val) );
    case RK_clang__OMPCapturedExprDecl:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPCapturedExprDecl *>(Val) );
    case RK_clang__CXXOperatorCallExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXOperatorCallExpr *>(Val) );
    case RK_clang__CXXMemberCallExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXMemberCallExpr *>(Val) );
    case RK_clang__CUDAKernelCallExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CUDAKernelCallExpr *>(Val) );
    case RK_clang__CXXNamedCastExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXNamedCastExpr *>(Val) );
    case RK_clang__CXXStaticCastExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXStaticCastExpr *>(Val) );
    case RK_clang__CXXDynamicCastExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXDynamicCastExpr *>(Val) );
    case RK_clang__CXXReinterpretCastExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXReinterpretCastExpr *>(Val) );
    case RK_clang__CXXConstCastExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXConstCastExpr *>(Val) );
    case RK_clang__UserDefinedLiteral:
      return ReflectProp( MemNum, reinterpret_cast<clang::UserDefinedLiteral *>(Val) );
    case RK_clang__CXXBoolLiteralExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXBoolLiteralExpr *>(Val) );
    case RK_clang__CXXNullPtrLiteralExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXNullPtrLiteralExpr *>(Val) );
    case RK_clang__CXXStdInitializerListExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXStdInitializerListExpr *>(Val) );
    case RK_clang__CXXTypeidExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXTypeidExpr *>(Val) );
    case RK_clang__MSPropertyRefExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::MSPropertyRefExpr *>(Val) );
    case RK_clang__MSPropertySubscriptExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::MSPropertySubscriptExpr *>(Val) );
    case RK_clang__CXXUuidofExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXUuidofExpr *>(Val) );
    case RK_clang__CXXThisExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXThisExpr *>(Val) );
    case RK_clang__CXXThrowExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXThrowExpr *>(Val) );
    case RK_clang__CXXDefaultArgExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXDefaultArgExpr *>(Val) );
    case RK_clang__CXXDefaultInitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXDefaultInitExpr *>(Val) );
    case RK_clang__CXXTemporary:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXTemporary *>(Val) );
    case RK_clang__CXXBindTemporaryExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXBindTemporaryExpr *>(Val) );
    case RK_clang__CXXConstructExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXConstructExpr *>(Val) );
    case RK_clang__CXXInheritedCtorInitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXInheritedCtorInitExpr *>(Val) );
    case RK_clang__CXXFunctionalCastExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXFunctionalCastExpr *>(Val) );
    case RK_clang__CXXTemporaryObjectExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXTemporaryObjectExpr *>(Val) );
    case RK_clang__LambdaExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::LambdaExpr *>(Val) );
    case RK_clang__CXXScalarValueInitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXScalarValueInitExpr *>(Val) );
    case RK_clang__CXXNewExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXNewExpr *>(Val) );
    case RK_clang__CXXDeleteExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXDeleteExpr *>(Val) );
    case RK_clang__CXXPseudoDestructorExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXPseudoDestructorExpr *>(Val) );
    case RK_clang__TypeTraitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::TypeTraitExpr *>(Val) );
    case RK_clang__ArrayTypeTraitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ArrayTypeTraitExpr *>(Val) );
    case RK_clang__ExpressionTraitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ExpressionTraitExpr *>(Val) );
    case RK_clang__OverloadExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::OverloadExpr *>(Val) );
    case RK_clang__UnresolvedLookupExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::UnresolvedLookupExpr *>(Val) );
    case RK_clang__DependentScopeDeclRefExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::DependentScopeDeclRefExpr *>(Val) );
    case RK_clang__ExprWithCleanups:
      return ReflectProp( MemNum, reinterpret_cast<clang::ExprWithCleanups *>(Val) );
    case RK_clang__CXXUnresolvedConstructExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXUnresolvedConstructExpr *>(Val) );
    case RK_clang__CXXDependentScopeMemberExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXDependentScopeMemberExpr *>(Val) );
    case RK_clang__UnresolvedMemberExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::UnresolvedMemberExpr *>(Val) );
    case RK_clang__CXXNoexceptExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXNoexceptExpr *>(Val) );
    case RK_clang__PackExpansionExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::PackExpansionExpr *>(Val) );
    case RK_clang__SizeOfPackExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::SizeOfPackExpr *>(Val) );
    case RK_clang__SubstNonTypeTemplateParmExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::SubstNonTypeTemplateParmExpr *>(Val) );
    case RK_clang__SubstNonTypeTemplateParmPackExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::SubstNonTypeTemplateParmPackExpr *>(Val) );
    case RK_clang__FunctionParmPackExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::FunctionParmPackExpr *>(Val) );
    case RK_clang__MaterializeTemporaryExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::MaterializeTemporaryExpr *>(Val) );
    case RK_clang__CXXFoldExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXFoldExpr *>(Val) );
    case RK_clang__CoroutineSuspendExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CoroutineSuspendExpr *>(Val) );
    case RK_clang__CoawaitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CoawaitExpr *>(Val) );
    case RK_clang__DependentCoawaitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::DependentCoawaitExpr *>(Val) );
    case RK_clang__CoyieldExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CoyieldExpr *>(Val) );
    case RK_clang__ReflectionExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ReflectionExpr *>(Val) );
    case RK_clang__CompilerMessageExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CompilerMessageExpr *>(Val) );
    case RK_clang__CompilerDiagnosticExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CompilerDiagnosticExpr *>(Val) );
    case RK_clang__ReflectionTraitExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ReflectionTraitExpr *>(Val) );
    case RK_clang__ReflectionTraitTypedExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ReflectionTraitTypedExpr *>(Val) );
    case RK_clang__VoidReflectionExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::VoidReflectionExpr *>(Val) );
    case RK_clang__NonvoidReflectionExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::NonvoidReflectionExpr *>(Val) );
    case RK_clang__ReflectNewExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ReflectNewExpr *>(Val) );
    case RK_clang__ReflectDeleteExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ReflectDeleteExpr *>(Val) );
    case RK_clang__CXXConstantExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXConstantExpr *>(Val) );
    case RK_clang__CXXDependentIdExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXDependentIdExpr *>(Val) );
    case RK_clang__CXXConcatenateExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXConcatenateExpr *>(Val) );
    case RK_clang__ObjCStringLiteral:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCStringLiteral *>(Val) );
    case RK_clang__ObjCBoolLiteralExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCBoolLiteralExpr *>(Val) );
    case RK_clang__ObjCBoxedExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCBoxedExpr *>(Val) );
    case RK_clang__ObjCArrayLiteral:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCArrayLiteral *>(Val) );
    case RK_clang__ObjCDictionaryLiteral:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCDictionaryLiteral *>(Val) );
    case RK_clang__ObjCEncodeExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCEncodeExpr *>(Val) );
    case RK_clang__ObjCSelectorExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCSelectorExpr *>(Val) );
    case RK_clang__ObjCProtocolExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCProtocolExpr *>(Val) );
    case RK_clang__ObjCIvarRefExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCIvarRefExpr *>(Val) );
    case RK_clang__ObjCPropertyRefExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCPropertyRefExpr *>(Val) );
    case RK_clang__ObjCSubscriptRefExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCSubscriptRefExpr *>(Val) );
    case RK_clang__ObjCMessageExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCMessageExpr *>(Val) );
    case RK_clang__ObjCIsaExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCIsaExpr *>(Val) );
    case RK_clang__ObjCIndirectCopyRestoreExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCIndirectCopyRestoreExpr *>(Val) );
    case RK_clang__ObjCBridgedCastExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCBridgedCastExpr *>(Val) );
    case RK_clang__ObjCAvailabilityCheckExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCAvailabilityCheckExpr *>(Val) );
    case RK_clang__OMPArraySectionExpr:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPArraySectionExpr *>(Val) );
    case RK_clang__CXXCatchStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXCatchStmt *>(Val) );
    case RK_clang__CXXTryStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXTryStmt *>(Val) );
    case RK_clang__CXXForRangeStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXForRangeStmt *>(Val) );
    case RK_clang__CXXExpansionStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXExpansionStmt *>(Val) );
    case RK_clang__CXXTupleExpansionStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXTupleExpansionStmt *>(Val) );
    case RK_clang__CXXPackExpansionStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXPackExpansionStmt *>(Val) );
    case RK_clang__MSDependentExistsStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::MSDependentExistsStmt *>(Val) );
    case RK_clang__CoroutineBodyStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CoroutineBodyStmt *>(Val) );
    case RK_clang__CoreturnStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CoreturnStmt *>(Val) );
    case RK_clang__CXXMetaparseStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::CXXMetaparseStmt *>(Val) );
    case RK_clang__ObjCForCollectionStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCForCollectionStmt *>(Val) );
    case RK_clang__ObjCAtCatchStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCAtCatchStmt *>(Val) );
    case RK_clang__ObjCAtFinallyStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCAtFinallyStmt *>(Val) );
    case RK_clang__ObjCAtTryStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCAtTryStmt *>(Val) );
    case RK_clang__ObjCAtSynchronizedStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCAtSynchronizedStmt *>(Val) );
    case RK_clang__ObjCAtThrowStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCAtThrowStmt *>(Val) );
    case RK_clang__ObjCAutoreleasePoolStmt:
      return ReflectProp( MemNum, reinterpret_cast<clang::ObjCAutoreleasePoolStmt *>(Val) );
    case RK_clang__OMPClause:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPClause *>(Val) );
    case RK_clang__OMPExecutableDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPExecutableDirective *>(Val) );
    case RK_clang__OMPParallelDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPParallelDirective *>(Val) );
    case RK_clang__OMPLoopDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPLoopDirective *>(Val) );
    case RK_clang__OMPSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPSimdDirective *>(Val) );
    case RK_clang__OMPForDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPForDirective *>(Val) );
    case RK_clang__OMPForSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPForSimdDirective *>(Val) );
    case RK_clang__OMPSectionsDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPSectionsDirective *>(Val) );
    case RK_clang__OMPSectionDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPSectionDirective *>(Val) );
    case RK_clang__OMPSingleDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPSingleDirective *>(Val) );
    case RK_clang__OMPMasterDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPMasterDirective *>(Val) );
    case RK_clang__OMPCriticalDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPCriticalDirective *>(Val) );
    case RK_clang__OMPParallelForDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPParallelForDirective *>(Val) );
    case RK_clang__OMPParallelForSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPParallelForSimdDirective *>(Val) );
    case RK_clang__OMPParallelSectionsDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPParallelSectionsDirective *>(Val) );
    case RK_clang__OMPTaskDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTaskDirective *>(Val) );
    case RK_clang__OMPTaskyieldDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTaskyieldDirective *>(Val) );
    case RK_clang__OMPBarrierDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPBarrierDirective *>(Val) );
    case RK_clang__OMPTaskwaitDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTaskwaitDirective *>(Val) );
    case RK_clang__OMPTaskgroupDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTaskgroupDirective *>(Val) );
    case RK_clang__OMPFlushDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPFlushDirective *>(Val) );
    case RK_clang__OMPOrderedDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPOrderedDirective *>(Val) );
    case RK_clang__OMPAtomicDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPAtomicDirective *>(Val) );
    case RK_clang__OMPTargetDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetDirective *>(Val) );
    case RK_clang__OMPTargetDataDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetDataDirective *>(Val) );
    case RK_clang__OMPTargetEnterDataDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetEnterDataDirective *>(Val) );
    case RK_clang__OMPTargetExitDataDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetExitDataDirective *>(Val) );
    case RK_clang__OMPTargetParallelDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetParallelDirective *>(Val) );
    case RK_clang__OMPTargetParallelForDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetParallelForDirective *>(Val) );
    case RK_clang__OMPTeamsDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTeamsDirective *>(Val) );
    case RK_clang__OMPCancellationPointDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPCancellationPointDirective *>(Val) );
    case RK_clang__OMPCancelDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPCancelDirective *>(Val) );
    case RK_clang__OMPTaskLoopDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTaskLoopDirective *>(Val) );
    case RK_clang__OMPTaskLoopSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTaskLoopSimdDirective *>(Val) );
    case RK_clang__OMPDistributeDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPDistributeDirective *>(Val) );
    case RK_clang__OMPTargetUpdateDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetUpdateDirective *>(Val) );
    case RK_clang__OMPDistributeParallelForDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPDistributeParallelForDirective *>(Val) );
    case RK_clang__OMPDistributeParallelForSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPDistributeParallelForSimdDirective *>(Val) );
    case RK_clang__OMPDistributeSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPDistributeSimdDirective *>(Val) );
    case RK_clang__OMPTargetParallelForSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetParallelForSimdDirective *>(Val) );
    case RK_clang__OMPTargetSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetSimdDirective *>(Val) );
    case RK_clang__OMPTeamsDistributeDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTeamsDistributeDirective *>(Val) );
    case RK_clang__OMPTeamsDistributeSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTeamsDistributeSimdDirective *>(Val) );
    case RK_clang__OMPTeamsDistributeParallelForSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTeamsDistributeParallelForSimdDirective *>(Val) );
    case RK_clang__OMPTeamsDistributeParallelForDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTeamsDistributeParallelForDirective *>(Val) );
    case RK_clang__OMPTargetTeamsDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetTeamsDirective *>(Val) );
    case RK_clang__OMPTargetTeamsDistributeDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetTeamsDistributeDirective *>(Val) );
    case RK_clang__OMPTargetTeamsDistributeParallelForDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetTeamsDistributeParallelForDirective *>(Val) );
    case RK_clang__OMPTargetTeamsDistributeParallelForSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetTeamsDistributeParallelForSimdDirective *>(Val) );
    case RK_clang__OMPTargetTeamsDistributeSimdDirective:
      return ReflectProp( MemNum, reinterpret_cast<clang::OMPTargetTeamsDistributeSimdDirective *>(Val) );
    case RK_clang__StoredDeclsMap:
      return ReflectProp( MemNum, reinterpret_cast<clang::StoredDeclsMap *>(Val) );
    case RK_clang__DependentDiagnostic:
      return ReflectProp( MemNum, reinterpret_cast<clang::DependentDiagnostic *>(Val) );
    case RK_reflcontainers__VectorDecl:
      return ReflectProp( MemNum, reinterpret_cast<reflcontainers::VectorDecl *>(Val) );
    case RK_reflcontainers__VectorStr:
      return ReflectProp( MemNum, reinterpret_cast<reflcontainers::VectorStr *>(Val) );
    case RK_reflcontainers__SetDecl:
      return ReflectProp( MemNum, reinterpret_cast<reflcontainers::SetDecl *>(Val) );
    case RK_reflcontainers__SetStr:
      return ReflectProp( MemNum, reinterpret_cast<reflcontainers::SetStr *>(Val) );
    case RK_reflcontainers__MapDeclStr:
      return ReflectProp( MemNum, reinterpret_cast<reflcontainers::MapDeclStr *>(Val) );
    case RK_reflcontainers__MapStrStr:
      return ReflectProp( MemNum, reinterpret_cast<reflcontainers::MapStrStr *>(Val) );
