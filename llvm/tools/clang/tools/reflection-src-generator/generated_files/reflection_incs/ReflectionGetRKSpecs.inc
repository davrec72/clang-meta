template<> struct GetReflectionObjKind<llvm::APInt>
{ static constexpr ReflectionObjKind value = RK_llvm__APInt; };
template<> struct GetTypeFromRK<RK_llvm__APInt>
{ using type = llvm::APInt; };
template<> struct GetReflectionObjKind<llvm::APInt::ms>
{ static constexpr ReflectionObjKind value = RK_llvm__APInt__ms; };
template<> struct GetTypeFromRK<RK_llvm__APInt__ms>
{ using type = llvm::APInt::ms; };
template<> struct GetReflectionObjKind<llvm::APInt::mu>
{ static constexpr ReflectionObjKind value = RK_llvm__APInt__mu; };
template<> struct GetTypeFromRK<RK_llvm__APInt__mu>
{ using type = llvm::APInt::mu; };
template<> struct GetReflectionObjKind<llvm::APFloatBase>
{ static constexpr ReflectionObjKind value = RK_llvm__APFloatBase; };
template<> struct GetTypeFromRK<RK_llvm__APFloatBase>
{ using type = llvm::APFloatBase; };
template<> struct GetReflectionObjKind<llvm::APFloat>
{ static constexpr ReflectionObjKind value = RK_llvm__APFloat; };
template<> struct GetTypeFromRK<RK_llvm__APFloat>
{ using type = llvm::APFloat; };
template<> struct GetReflectionObjKind<llvm::APSInt>
{ static constexpr ReflectionObjKind value = RK_llvm__APSInt; };
template<> struct GetTypeFromRK<RK_llvm__APSInt>
{ using type = llvm::APSInt; };
template<> struct GetReflectionObjKind<clang::APValue>
{ static constexpr ReflectionObjKind value = RK_clang__APValue; };
template<> struct GetTypeFromRK<RK_clang__APValue>
{ using type = clang::APValue; };
template<> struct GetReflectionObjKind<clang::APValue::LValueBase>
{ static constexpr ReflectionObjKind value = RK_clang__APValue__LValueBase; };
template<> struct GetTypeFromRK<RK_clang__APValue__LValueBase>
{ using type = clang::APValue::LValueBase; };
template<> struct GetReflectionObjKind<clang::DiagnosticOptions>
{ static constexpr ReflectionObjKind value = RK_clang__DiagnosticOptions; };
template<> struct GetTypeFromRK<RK_clang__DiagnosticOptions>
{ using type = clang::DiagnosticOptions; };
template<> struct GetReflectionObjKind<clang::FileID>
{ static constexpr ReflectionObjKind value = RK_clang__FileID; };
template<> struct GetTypeFromRK<RK_clang__FileID>
{ using type = clang::FileID; };
template<> struct GetReflectionObjKind<clang::SourceLocation>
{ static constexpr ReflectionObjKind value = RK_clang__SourceLocation; };
template<> struct GetTypeFromRK<RK_clang__SourceLocation>
{ using type = clang::SourceLocation; };
template<> struct GetReflectionObjKind<clang::SourceRange>
{ static constexpr ReflectionObjKind value = RK_clang__SourceRange; };
template<> struct GetTypeFromRK<RK_clang__SourceRange>
{ using type = clang::SourceRange; };
template<> struct GetReflectionObjKind<clang::CharSourceRange>
{ static constexpr ReflectionObjKind value = RK_clang__CharSourceRange; };
template<> struct GetTypeFromRK<RK_clang__CharSourceRange>
{ using type = clang::CharSourceRange; };
template<> struct GetReflectionObjKind<clang::PresumedLoc>
{ static constexpr ReflectionObjKind value = RK_clang__PresumedLoc; };
template<> struct GetTypeFromRK<RK_clang__PresumedLoc>
{ using type = clang::PresumedLoc; };
template<> struct GetReflectionObjKind<clang::FullSourceLoc>
{ static constexpr ReflectionObjKind value = RK_clang__FullSourceLoc; };
template<> struct GetTypeFromRK<RK_clang__FullSourceLoc>
{ using type = clang::FullSourceLoc; };
template<> struct GetReflectionObjKind<llvm::DebugEpochBase>
{ static constexpr ReflectionObjKind value = RK_llvm__DebugEpochBase; };
template<> struct GetTypeFromRK<RK_llvm__DebugEpochBase>
{ using type = llvm::DebugEpochBase; };
template<> struct GetReflectionObjKind<clang::FixItHint>
{ static constexpr ReflectionObjKind value = RK_clang__FixItHint; };
template<> struct GetTypeFromRK<RK_clang__FixItHint>
{ using type = clang::FixItHint; };
template<> struct GetReflectionObjKind<clang::DiagnosticsEngine>
{ static constexpr ReflectionObjKind value = RK_clang__DiagnosticsEngine; };
template<> struct GetTypeFromRK<RK_clang__DiagnosticsEngine>
{ using type = clang::DiagnosticsEngine; };
template<> struct GetReflectionObjKind<clang::DiagnosticBuilder>
{ static constexpr ReflectionObjKind value = RK_clang__DiagnosticBuilder; };
template<> struct GetTypeFromRK<RK_clang__DiagnosticBuilder>
{ using type = clang::DiagnosticBuilder; };
template<> struct GetReflectionObjKind<clang::DiagnosticConsumer>
{ static constexpr ReflectionObjKind value = RK_clang__DiagnosticConsumer; };
template<> struct GetTypeFromRK<RK_clang__DiagnosticConsumer>
{ using type = clang::DiagnosticConsumer; };
template<> struct GetReflectionObjKind<clang::IdentifierInfo>
{ static constexpr ReflectionObjKind value = RK_clang__IdentifierInfo; };
template<> struct GetTypeFromRK<RK_clang__IdentifierInfo>
{ using type = clang::IdentifierInfo; };
template<> struct GetReflectionObjKind<clang::IdentifierInfoLookup>
{ static constexpr ReflectionObjKind value = RK_clang__IdentifierInfoLookup; };
template<> struct GetTypeFromRK<RK_clang__IdentifierInfoLookup>
{ using type = clang::IdentifierInfoLookup; };
template<> struct GetReflectionObjKind<clang::IdentifierTable>
{ static constexpr ReflectionObjKind value = RK_clang__IdentifierTable; };
template<> struct GetTypeFromRK<RK_clang__IdentifierTable>
{ using type = clang::IdentifierTable; };
template<> struct GetReflectionObjKind<clang::Selector>
{ static constexpr ReflectionObjKind value = RK_clang__Selector; };
template<> struct GetTypeFromRK<RK_clang__Selector>
{ using type = clang::Selector; };
template<> struct GetReflectionObjKind<clang::SelectorTable>
{ static constexpr ReflectionObjKind value = RK_clang__SelectorTable; };
template<> struct GetTypeFromRK<RK_clang__SelectorTable>
{ using type = clang::SelectorTable; };
template<> struct GetReflectionObjKind<clang::PartialDiagnostic>
{ static constexpr ReflectionObjKind value = RK_clang__PartialDiagnostic; };
template<> struct GetTypeFromRK<RK_clang__PartialDiagnostic>
{ using type = clang::PartialDiagnostic; };
template<> struct GetReflectionObjKind<clang::DeclarationName>
{ static constexpr ReflectionObjKind value = RK_clang__DeclarationName; };
template<> struct GetTypeFromRK<RK_clang__DeclarationName>
{ using type = clang::DeclarationName; };
template<> struct GetReflectionObjKind<clang::DeclarationNameLoc>
{ static constexpr ReflectionObjKind value = RK_clang__DeclarationNameLoc; };
template<> struct GetTypeFromRK<RK_clang__DeclarationNameLoc>
{ using type = clang::DeclarationNameLoc; };
template<> struct GetReflectionObjKind<clang::DeclarationNameInfo>
{ static constexpr ReflectionObjKind value = RK_clang__DeclarationNameInfo; };
template<> struct GetTypeFromRK<RK_clang__DeclarationNameInfo>
{ using type = clang::DeclarationNameInfo; };
template<> struct GetReflectionObjKind<llvm::raw_ostream>
{ static constexpr ReflectionObjKind value = RK_llvm__raw_ostream; };
template<> struct GetTypeFromRK<RK_llvm__raw_ostream>
{ using type = llvm::raw_ostream; };
template<> struct GetReflectionObjKind<llvm::VersionTuple>
{ static constexpr ReflectionObjKind value = RK_llvm__VersionTuple; };
template<> struct GetTypeFromRK<RK_llvm__VersionTuple>
{ using type = llvm::VersionTuple; };
template<> struct GetReflectionObjKind<clang::Decl>
{ static constexpr ReflectionObjKind value = RK_clang__Decl; };
template<> struct GetTypeFromRK<RK_clang__Decl>
{ using type = clang::Decl; };
template<> struct GetReflectionObjKind<clang::DeclContextLookupResult>
{ static constexpr ReflectionObjKind value = RK_clang__DeclContextLookupResult; };
template<> struct GetTypeFromRK<RK_clang__DeclContextLookupResult>
{ using type = clang::DeclContextLookupResult; };
template<> struct GetReflectionObjKind<clang::DeclContext>
{ static constexpr ReflectionObjKind value = RK_clang__DeclContext; };
template<> struct GetTypeFromRK<RK_clang__DeclContext>
{ using type = clang::DeclContext; };
template<> struct GetReflectionObjKind<clang::CharUnits>
{ static constexpr ReflectionObjKind value = RK_clang__CharUnits; };
template<> struct GetTypeFromRK<RK_clang__CharUnits>
{ using type = clang::CharUnits; };
template<> struct GetReflectionObjKind<clang::FileSystemOptions>
{ static constexpr ReflectionObjKind value = RK_clang__FileSystemOptions; };
template<> struct GetTypeFromRK<RK_clang__FileSystemOptions>
{ using type = clang::FileSystemOptions; };
template<> struct GetReflectionObjKind<llvm::Twine>
{ static constexpr ReflectionObjKind value = RK_llvm__Twine; };
template<> struct GetTypeFromRK<RK_llvm__Twine>
{ using type = llvm::Twine; };
template<> struct GetReflectionObjKind<llvm::sys::fs::UniqueID>
{ static constexpr ReflectionObjKind value = RK_llvm__sys__fs__UniqueID; };
template<> struct GetTypeFromRK<RK_llvm__sys__fs__UniqueID>
{ using type = llvm::sys::fs::UniqueID; };
template<> struct GetReflectionObjKind<llvm::MemoryBuffer>
{ static constexpr ReflectionObjKind value = RK_llvm__MemoryBuffer; };
template<> struct GetTypeFromRK<RK_llvm__MemoryBuffer>
{ using type = llvm::MemoryBuffer; };
template<> struct GetReflectionObjKind<llvm::MemoryBufferRef>
{ static constexpr ReflectionObjKind value = RK_llvm__MemoryBufferRef; };
template<> struct GetTypeFromRK<RK_llvm__MemoryBufferRef>
{ using type = llvm::MemoryBufferRef; };
template<> struct GetReflectionObjKind<clang::DirectoryEntry>
{ static constexpr ReflectionObjKind value = RK_clang__DirectoryEntry; };
template<> struct GetTypeFromRK<RK_clang__DirectoryEntry>
{ using type = clang::DirectoryEntry; };
template<> struct GetReflectionObjKind<clang::FileEntry>
{ static constexpr ReflectionObjKind value = RK_clang__FileEntry; };
template<> struct GetTypeFromRK<RK_clang__FileEntry>
{ using type = clang::FileEntry; };
template<> struct GetReflectionObjKind<clang::FileManager>
{ static constexpr ReflectionObjKind value = RK_clang__FileManager; };
template<> struct GetTypeFromRK<RK_clang__FileManager>
{ using type = clang::FileManager; };
template<> struct GetReflectionObjKind<clang::ASTFileSignature>
{ static constexpr ReflectionObjKind value = RK_clang__ASTFileSignature; };
template<> struct GetTypeFromRK<RK_clang__ASTFileSignature>
{ using type = clang::ASTFileSignature; };
template<> struct GetReflectionObjKind<clang::Module>
{ static constexpr ReflectionObjKind value = RK_clang__Module; };
template<> struct GetTypeFromRK<RK_clang__Module>
{ using type = clang::Module; };
template<> struct GetReflectionObjKind<clang::Module::Header>
{ static constexpr ReflectionObjKind value = RK_clang__Module__Header; };
template<> struct GetTypeFromRK<RK_clang__Module__Header>
{ using type = clang::Module::Header; };
template<> struct GetReflectionObjKind<clang::Module::DirectoryName>
{ static constexpr ReflectionObjKind value = RK_clang__Module__DirectoryName; };
template<> struct GetTypeFromRK<RK_clang__Module__DirectoryName>
{ using type = clang::Module::DirectoryName; };
template<> struct GetReflectionObjKind<clang::ExternalASTSource>
{ static constexpr ReflectionObjKind value = RK_clang__ExternalASTSource; };
template<> struct GetTypeFromRK<RK_clang__ExternalASTSource>
{ using type = clang::ExternalASTSource; };
template<> struct GetReflectionObjKind<clang::ExternalASTSource::MemoryBufferSizes>
{ static constexpr ReflectionObjKind value = RK_clang__ExternalASTSource__MemoryBufferSizes; };
template<> struct GetTypeFromRK<RK_clang__ExternalASTSource__MemoryBufferSizes>
{ using type = clang::ExternalASTSource::MemoryBufferSizes; };
template<> struct GetReflectionObjKind<llvm::FoldingSetBase>
{ static constexpr ReflectionObjKind value = RK_llvm__FoldingSetBase; };
template<> struct GetTypeFromRK<RK_llvm__FoldingSetBase>
{ using type = llvm::FoldingSetBase; };
template<> struct GetReflectionObjKind<llvm::FoldingSetBase::Node>
{ static constexpr ReflectionObjKind value = RK_llvm__FoldingSetBase__Node; };
template<> struct GetTypeFromRK<RK_llvm__FoldingSetBase__Node>
{ using type = llvm::FoldingSetBase::Node; };
template<> struct GetReflectionObjKind<llvm::FoldingSetNodeIDRef>
{ static constexpr ReflectionObjKind value = RK_llvm__FoldingSetNodeIDRef; };
template<> struct GetTypeFromRK<RK_llvm__FoldingSetNodeIDRef>
{ using type = llvm::FoldingSetNodeIDRef; };
template<> struct GetReflectionObjKind<llvm::FoldingSetNodeID>
{ static constexpr ReflectionObjKind value = RK_llvm__FoldingSetNodeID; };
template<> struct GetTypeFromRK<RK_llvm__FoldingSetNodeID>
{ using type = llvm::FoldingSetNodeID; };
template<> struct GetReflectionObjKind<clang::NestedNameSpecifier>
{ static constexpr ReflectionObjKind value = RK_clang__NestedNameSpecifier; };
template<> struct GetTypeFromRK<RK_clang__NestedNameSpecifier>
{ using type = clang::NestedNameSpecifier; };
template<> struct GetReflectionObjKind<clang::NestedNameSpecifierLoc>
{ static constexpr ReflectionObjKind value = RK_clang__NestedNameSpecifierLoc; };
template<> struct GetTypeFromRK<RK_clang__NestedNameSpecifierLoc>
{ using type = clang::NestedNameSpecifierLoc; };
template<> struct GetReflectionObjKind<clang::UncommonTemplateNameStorage>
{ static constexpr ReflectionObjKind value = RK_clang__UncommonTemplateNameStorage; };
template<> struct GetTypeFromRK<RK_clang__UncommonTemplateNameStorage>
{ using type = clang::UncommonTemplateNameStorage; };
template<> struct GetReflectionObjKind<clang::OverloadedTemplateStorage>
{ static constexpr ReflectionObjKind value = RK_clang__OverloadedTemplateStorage; };
template<> struct GetTypeFromRK<RK_clang__OverloadedTemplateStorage>
{ using type = clang::OverloadedTemplateStorage; };
template<> struct GetReflectionObjKind<clang::SubstTemplateTemplateParmPackStorage>
{ static constexpr ReflectionObjKind value = RK_clang__SubstTemplateTemplateParmPackStorage; };
template<> struct GetTypeFromRK<RK_clang__SubstTemplateTemplateParmPackStorage>
{ using type = clang::SubstTemplateTemplateParmPackStorage; };
template<> struct GetReflectionObjKind<clang::TemplateName>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateName; };
template<> struct GetTypeFromRK<RK_clang__TemplateName>
{ using type = clang::TemplateName; };
template<> struct GetReflectionObjKind<clang::SubstTemplateTemplateParmStorage>
{ static constexpr ReflectionObjKind value = RK_clang__SubstTemplateTemplateParmStorage; };
template<> struct GetTypeFromRK<RK_clang__SubstTemplateTemplateParmStorage>
{ using type = clang::SubstTemplateTemplateParmStorage; };
template<> struct GetReflectionObjKind<clang::QualifiedTemplateName>
{ static constexpr ReflectionObjKind value = RK_clang__QualifiedTemplateName; };
template<> struct GetTypeFromRK<RK_clang__QualifiedTemplateName>
{ using type = clang::QualifiedTemplateName; };
template<> struct GetReflectionObjKind<clang::DependentTemplateName>
{ static constexpr ReflectionObjKind value = RK_clang__DependentTemplateName; };
template<> struct GetTypeFromRK<RK_clang__DependentTemplateName>
{ using type = clang::DependentTemplateName; };
template<> struct GetReflectionObjKind<clang::LinkageInfo>
{ static constexpr ReflectionObjKind value = RK_clang__LinkageInfo; };
template<> struct GetTypeFromRK<RK_clang__LinkageInfo>
{ using type = clang::LinkageInfo; };
template<> struct GetReflectionObjKind<clang::Qualifiers>
{ static constexpr ReflectionObjKind value = RK_clang__Qualifiers; };
template<> struct GetTypeFromRK<RK_clang__Qualifiers>
{ using type = clang::Qualifiers; };
template<> struct GetReflectionObjKind<clang::SplitQualType>
{ static constexpr ReflectionObjKind value = RK_clang__SplitQualType; };
template<> struct GetTypeFromRK<RK_clang__SplitQualType>
{ using type = clang::SplitQualType; };
template<> struct GetReflectionObjKind<clang::QualType>
{ static constexpr ReflectionObjKind value = RK_clang__QualType; };
template<> struct GetTypeFromRK<RK_clang__QualType>
{ using type = clang::QualType; };
template<> struct GetReflectionObjKind<clang::ExtQualsTypeCommonBase>
{ static constexpr ReflectionObjKind value = RK_clang__ExtQualsTypeCommonBase; };
template<> struct GetTypeFromRK<RK_clang__ExtQualsTypeCommonBase>
{ using type = clang::ExtQualsTypeCommonBase; };
template<> struct GetReflectionObjKind<clang::Type>
{ static constexpr ReflectionObjKind value = RK_clang__Type; };
template<> struct GetTypeFromRK<RK_clang__Type>
{ using type = clang::Type; };
template<> struct GetReflectionObjKind<clang::BuiltinType>
{ static constexpr ReflectionObjKind value = RK_clang__BuiltinType; };
template<> struct GetTypeFromRK<RK_clang__BuiltinType>
{ using type = clang::BuiltinType; };
template<> struct GetReflectionObjKind<clang::ComplexType>
{ static constexpr ReflectionObjKind value = RK_clang__ComplexType; };
template<> struct GetTypeFromRK<RK_clang__ComplexType>
{ using type = clang::ComplexType; };
template<> struct GetReflectionObjKind<clang::ParenType>
{ static constexpr ReflectionObjKind value = RK_clang__ParenType; };
template<> struct GetTypeFromRK<RK_clang__ParenType>
{ using type = clang::ParenType; };
template<> struct GetReflectionObjKind<clang::PointerType>
{ static constexpr ReflectionObjKind value = RK_clang__PointerType; };
template<> struct GetTypeFromRK<RK_clang__PointerType>
{ using type = clang::PointerType; };
template<> struct GetReflectionObjKind<clang::AdjustedType>
{ static constexpr ReflectionObjKind value = RK_clang__AdjustedType; };
template<> struct GetTypeFromRK<RK_clang__AdjustedType>
{ using type = clang::AdjustedType; };
template<> struct GetReflectionObjKind<clang::DecayedType>
{ static constexpr ReflectionObjKind value = RK_clang__DecayedType; };
template<> struct GetTypeFromRK<RK_clang__DecayedType>
{ using type = clang::DecayedType; };
template<> struct GetReflectionObjKind<clang::BlockPointerType>
{ static constexpr ReflectionObjKind value = RK_clang__BlockPointerType; };
template<> struct GetTypeFromRK<RK_clang__BlockPointerType>
{ using type = clang::BlockPointerType; };
template<> struct GetReflectionObjKind<clang::ReferenceType>
{ static constexpr ReflectionObjKind value = RK_clang__ReferenceType; };
template<> struct GetTypeFromRK<RK_clang__ReferenceType>
{ using type = clang::ReferenceType; };
template<> struct GetReflectionObjKind<clang::LValueReferenceType>
{ static constexpr ReflectionObjKind value = RK_clang__LValueReferenceType; };
template<> struct GetTypeFromRK<RK_clang__LValueReferenceType>
{ using type = clang::LValueReferenceType; };
template<> struct GetReflectionObjKind<clang::RValueReferenceType>
{ static constexpr ReflectionObjKind value = RK_clang__RValueReferenceType; };
template<> struct GetTypeFromRK<RK_clang__RValueReferenceType>
{ using type = clang::RValueReferenceType; };
template<> struct GetReflectionObjKind<clang::MemberPointerType>
{ static constexpr ReflectionObjKind value = RK_clang__MemberPointerType; };
template<> struct GetTypeFromRK<RK_clang__MemberPointerType>
{ using type = clang::MemberPointerType; };
template<> struct GetReflectionObjKind<clang::ArrayType>
{ static constexpr ReflectionObjKind value = RK_clang__ArrayType; };
template<> struct GetTypeFromRK<RK_clang__ArrayType>
{ using type = clang::ArrayType; };
template<> struct GetReflectionObjKind<clang::ConstantArrayType>
{ static constexpr ReflectionObjKind value = RK_clang__ConstantArrayType; };
template<> struct GetTypeFromRK<RK_clang__ConstantArrayType>
{ using type = clang::ConstantArrayType; };
template<> struct GetReflectionObjKind<clang::IncompleteArrayType>
{ static constexpr ReflectionObjKind value = RK_clang__IncompleteArrayType; };
template<> struct GetTypeFromRK<RK_clang__IncompleteArrayType>
{ using type = clang::IncompleteArrayType; };
template<> struct GetReflectionObjKind<clang::VariableArrayType>
{ static constexpr ReflectionObjKind value = RK_clang__VariableArrayType; };
template<> struct GetTypeFromRK<RK_clang__VariableArrayType>
{ using type = clang::VariableArrayType; };
template<> struct GetReflectionObjKind<clang::DependentSizedArrayType>
{ static constexpr ReflectionObjKind value = RK_clang__DependentSizedArrayType; };
template<> struct GetTypeFromRK<RK_clang__DependentSizedArrayType>
{ using type = clang::DependentSizedArrayType; };
template<> struct GetReflectionObjKind<clang::DependentAddressSpaceType>
{ static constexpr ReflectionObjKind value = RK_clang__DependentAddressSpaceType; };
template<> struct GetTypeFromRK<RK_clang__DependentAddressSpaceType>
{ using type = clang::DependentAddressSpaceType; };
template<> struct GetReflectionObjKind<clang::DependentSizedExtVectorType>
{ static constexpr ReflectionObjKind value = RK_clang__DependentSizedExtVectorType; };
template<> struct GetTypeFromRK<RK_clang__DependentSizedExtVectorType>
{ using type = clang::DependentSizedExtVectorType; };
template<> struct GetReflectionObjKind<clang::VectorType>
{ static constexpr ReflectionObjKind value = RK_clang__VectorType; };
template<> struct GetTypeFromRK<RK_clang__VectorType>
{ using type = clang::VectorType; };
template<> struct GetReflectionObjKind<clang::DependentVectorType>
{ static constexpr ReflectionObjKind value = RK_clang__DependentVectorType; };
template<> struct GetTypeFromRK<RK_clang__DependentVectorType>
{ using type = clang::DependentVectorType; };
template<> struct GetReflectionObjKind<clang::ExtVectorType>
{ static constexpr ReflectionObjKind value = RK_clang__ExtVectorType; };
template<> struct GetTypeFromRK<RK_clang__ExtVectorType>
{ using type = clang::ExtVectorType; };
template<> struct GetReflectionObjKind<clang::FunctionType>
{ static constexpr ReflectionObjKind value = RK_clang__FunctionType; };
template<> struct GetTypeFromRK<RK_clang__FunctionType>
{ using type = clang::FunctionType; };
template<> struct GetReflectionObjKind<clang::FunctionType::ExtInfo>
{ static constexpr ReflectionObjKind value = RK_clang__FunctionType__ExtInfo; };
template<> struct GetTypeFromRK<RK_clang__FunctionType__ExtInfo>
{ using type = clang::FunctionType::ExtInfo; };
template<> struct GetReflectionObjKind<clang::FunctionNoProtoType>
{ static constexpr ReflectionObjKind value = RK_clang__FunctionNoProtoType; };
template<> struct GetTypeFromRK<RK_clang__FunctionNoProtoType>
{ using type = clang::FunctionNoProtoType; };
template<> struct GetReflectionObjKind<clang::FunctionProtoType>
{ static constexpr ReflectionObjKind value = RK_clang__FunctionProtoType; };
template<> struct GetTypeFromRK<RK_clang__FunctionProtoType>
{ using type = clang::FunctionProtoType; };
template<> struct GetReflectionObjKind<clang::FunctionProtoType::ExtParameterInfo>
{ static constexpr ReflectionObjKind value = RK_clang__FunctionProtoType__ExtParameterInfo; };
template<> struct GetTypeFromRK<RK_clang__FunctionProtoType__ExtParameterInfo>
{ using type = clang::FunctionProtoType::ExtParameterInfo; };
template<> struct GetReflectionObjKind<clang::FunctionProtoType::ExceptionSpecInfo>
{ static constexpr ReflectionObjKind value = RK_clang__FunctionProtoType__ExceptionSpecInfo; };
template<> struct GetTypeFromRK<RK_clang__FunctionProtoType__ExceptionSpecInfo>
{ using type = clang::FunctionProtoType::ExceptionSpecInfo; };
template<> struct GetReflectionObjKind<clang::FunctionProtoType::ExtProtoInfo>
{ static constexpr ReflectionObjKind value = RK_clang__FunctionProtoType__ExtProtoInfo; };
template<> struct GetTypeFromRK<RK_clang__FunctionProtoType__ExtProtoInfo>
{ using type = clang::FunctionProtoType::ExtProtoInfo; };
template<> struct GetReflectionObjKind<clang::UnresolvedUsingType>
{ static constexpr ReflectionObjKind value = RK_clang__UnresolvedUsingType; };
template<> struct GetTypeFromRK<RK_clang__UnresolvedUsingType>
{ using type = clang::UnresolvedUsingType; };
template<> struct GetReflectionObjKind<clang::TypedefType>
{ static constexpr ReflectionObjKind value = RK_clang__TypedefType; };
template<> struct GetTypeFromRK<RK_clang__TypedefType>
{ using type = clang::TypedefType; };
template<> struct GetReflectionObjKind<clang::TypeOfExprType>
{ static constexpr ReflectionObjKind value = RK_clang__TypeOfExprType; };
template<> struct GetTypeFromRK<RK_clang__TypeOfExprType>
{ using type = clang::TypeOfExprType; };
template<> struct GetReflectionObjKind<clang::TypeOfType>
{ static constexpr ReflectionObjKind value = RK_clang__TypeOfType; };
template<> struct GetTypeFromRK<RK_clang__TypeOfType>
{ using type = clang::TypeOfType; };
template<> struct GetReflectionObjKind<clang::DecltypeType>
{ static constexpr ReflectionObjKind value = RK_clang__DecltypeType; };
template<> struct GetTypeFromRK<RK_clang__DecltypeType>
{ using type = clang::DecltypeType; };
template<> struct GetReflectionObjKind<clang::ReflectedType>
{ static constexpr ReflectionObjKind value = RK_clang__ReflectedType; };
template<> struct GetTypeFromRK<RK_clang__ReflectedType>
{ using type = clang::ReflectedType; };
template<> struct GetReflectionObjKind<clang::UnaryTransformType>
{ static constexpr ReflectionObjKind value = RK_clang__UnaryTransformType; };
template<> struct GetTypeFromRK<RK_clang__UnaryTransformType>
{ using type = clang::UnaryTransformType; };
template<> struct GetReflectionObjKind<clang::TagType>
{ static constexpr ReflectionObjKind value = RK_clang__TagType; };
template<> struct GetTypeFromRK<RK_clang__TagType>
{ using type = clang::TagType; };
template<> struct GetReflectionObjKind<clang::RecordType>
{ static constexpr ReflectionObjKind value = RK_clang__RecordType; };
template<> struct GetTypeFromRK<RK_clang__RecordType>
{ using type = clang::RecordType; };
template<> struct GetReflectionObjKind<clang::EnumType>
{ static constexpr ReflectionObjKind value = RK_clang__EnumType; };
template<> struct GetTypeFromRK<RK_clang__EnumType>
{ using type = clang::EnumType; };
template<> struct GetReflectionObjKind<clang::AttributedType>
{ static constexpr ReflectionObjKind value = RK_clang__AttributedType; };
template<> struct GetTypeFromRK<RK_clang__AttributedType>
{ using type = clang::AttributedType; };
template<> struct GetReflectionObjKind<clang::TemplateTypeParmType>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateTypeParmType; };
template<> struct GetTypeFromRK<RK_clang__TemplateTypeParmType>
{ using type = clang::TemplateTypeParmType; };
template<> struct GetReflectionObjKind<clang::SubstTemplateTypeParmType>
{ static constexpr ReflectionObjKind value = RK_clang__SubstTemplateTypeParmType; };
template<> struct GetTypeFromRK<RK_clang__SubstTemplateTypeParmType>
{ using type = clang::SubstTemplateTypeParmType; };
template<> struct GetReflectionObjKind<clang::SubstTemplateTypeParmPackType>
{ static constexpr ReflectionObjKind value = RK_clang__SubstTemplateTypeParmPackType; };
template<> struct GetTypeFromRK<RK_clang__SubstTemplateTypeParmPackType>
{ using type = clang::SubstTemplateTypeParmPackType; };
template<> struct GetReflectionObjKind<clang::DeducedType>
{ static constexpr ReflectionObjKind value = RK_clang__DeducedType; };
template<> struct GetTypeFromRK<RK_clang__DeducedType>
{ using type = clang::DeducedType; };
template<> struct GetReflectionObjKind<clang::AutoType>
{ static constexpr ReflectionObjKind value = RK_clang__AutoType; };
template<> struct GetTypeFromRK<RK_clang__AutoType>
{ using type = clang::AutoType; };
template<> struct GetReflectionObjKind<clang::DeducedTemplateSpecializationType>
{ static constexpr ReflectionObjKind value = RK_clang__DeducedTemplateSpecializationType; };
template<> struct GetTypeFromRK<RK_clang__DeducedTemplateSpecializationType>
{ using type = clang::DeducedTemplateSpecializationType; };
template<> struct GetReflectionObjKind<clang::TemplateSpecializationType>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateSpecializationType; };
template<> struct GetTypeFromRK<RK_clang__TemplateSpecializationType>
{ using type = clang::TemplateSpecializationType; };
template<> struct GetReflectionObjKind<clang::InjectedClassNameType>
{ static constexpr ReflectionObjKind value = RK_clang__InjectedClassNameType; };
template<> struct GetTypeFromRK<RK_clang__InjectedClassNameType>
{ using type = clang::InjectedClassNameType; };
template<> struct GetReflectionObjKind<clang::TypeWithKeyword>
{ static constexpr ReflectionObjKind value = RK_clang__TypeWithKeyword; };
template<> struct GetTypeFromRK<RK_clang__TypeWithKeyword>
{ using type = clang::TypeWithKeyword; };
template<> struct GetReflectionObjKind<clang::ElaboratedType>
{ static constexpr ReflectionObjKind value = RK_clang__ElaboratedType; };
template<> struct GetTypeFromRK<RK_clang__ElaboratedType>
{ using type = clang::ElaboratedType; };
template<> struct GetReflectionObjKind<clang::DependentNameType>
{ static constexpr ReflectionObjKind value = RK_clang__DependentNameType; };
template<> struct GetTypeFromRK<RK_clang__DependentNameType>
{ using type = clang::DependentNameType; };
template<> struct GetReflectionObjKind<clang::DependentTemplateSpecializationType>
{ static constexpr ReflectionObjKind value = RK_clang__DependentTemplateSpecializationType; };
template<> struct GetTypeFromRK<RK_clang__DependentTemplateSpecializationType>
{ using type = clang::DependentTemplateSpecializationType; };
template<> struct GetReflectionObjKind<clang::PackExpansionType>
{ static constexpr ReflectionObjKind value = RK_clang__PackExpansionType; };
template<> struct GetTypeFromRK<RK_clang__PackExpansionType>
{ using type = clang::PackExpansionType; };
template<> struct GetReflectionObjKind<clang::ObjCTypeParamType>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCTypeParamType; };
template<> struct GetTypeFromRK<RK_clang__ObjCTypeParamType>
{ using type = clang::ObjCTypeParamType; };
template<> struct GetReflectionObjKind<clang::ObjCObjectType>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCObjectType; };
template<> struct GetTypeFromRK<RK_clang__ObjCObjectType>
{ using type = clang::ObjCObjectType; };
template<> struct GetReflectionObjKind<clang::ObjCInterfaceType>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCInterfaceType; };
template<> struct GetTypeFromRK<RK_clang__ObjCInterfaceType>
{ using type = clang::ObjCInterfaceType; };
template<> struct GetReflectionObjKind<clang::ObjCObjectPointerType>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCObjectPointerType; };
template<> struct GetTypeFromRK<RK_clang__ObjCObjectPointerType>
{ using type = clang::ObjCObjectPointerType; };
template<> struct GetReflectionObjKind<clang::AtomicType>
{ static constexpr ReflectionObjKind value = RK_clang__AtomicType; };
template<> struct GetTypeFromRK<RK_clang__AtomicType>
{ using type = clang::AtomicType; };
template<> struct GetReflectionObjKind<clang::PipeType>
{ static constexpr ReflectionObjKind value = RK_clang__PipeType; };
template<> struct GetTypeFromRK<RK_clang__PipeType>
{ using type = clang::PipeType; };
template<> struct GetReflectionObjKind<clang::TypeSourceInfo>
{ static constexpr ReflectionObjKind value = RK_clang__TypeSourceInfo; };
template<> struct GetTypeFromRK<RK_clang__TypeSourceInfo>
{ using type = clang::TypeSourceInfo; };
template<> struct GetReflectionObjKind<clang::TranslationUnitDecl>
{ static constexpr ReflectionObjKind value = RK_clang__TranslationUnitDecl; };
template<> struct GetTypeFromRK<RK_clang__TranslationUnitDecl>
{ using type = clang::TranslationUnitDecl; };
template<> struct GetReflectionObjKind<clang::PragmaCommentDecl>
{ static constexpr ReflectionObjKind value = RK_clang__PragmaCommentDecl; };
template<> struct GetTypeFromRK<RK_clang__PragmaCommentDecl>
{ using type = clang::PragmaCommentDecl; };
template<> struct GetReflectionObjKind<clang::PragmaDetectMismatchDecl>
{ static constexpr ReflectionObjKind value = RK_clang__PragmaDetectMismatchDecl; };
template<> struct GetTypeFromRK<RK_clang__PragmaDetectMismatchDecl>
{ using type = clang::PragmaDetectMismatchDecl; };
template<> struct GetReflectionObjKind<clang::ExternCContextDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ExternCContextDecl; };
template<> struct GetTypeFromRK<RK_clang__ExternCContextDecl>
{ using type = clang::ExternCContextDecl; };
template<> struct GetReflectionObjKind<clang::NamedDecl>
{ static constexpr ReflectionObjKind value = RK_clang__NamedDecl; };
template<> struct GetTypeFromRK<RK_clang__NamedDecl>
{ using type = clang::NamedDecl; };
template<> struct GetReflectionObjKind<clang::LabelDecl>
{ static constexpr ReflectionObjKind value = RK_clang__LabelDecl; };
template<> struct GetTypeFromRK<RK_clang__LabelDecl>
{ using type = clang::LabelDecl; };
template<> struct GetReflectionObjKind<clang::NamespaceDecl>
{ static constexpr ReflectionObjKind value = RK_clang__NamespaceDecl; };
template<> struct GetTypeFromRK<RK_clang__NamespaceDecl>
{ using type = clang::NamespaceDecl; };
template<> struct GetReflectionObjKind<clang::ValueDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ValueDecl; };
template<> struct GetTypeFromRK<RK_clang__ValueDecl>
{ using type = clang::ValueDecl; };
template<> struct GetReflectionObjKind<clang::DeclaratorDecl>
{ static constexpr ReflectionObjKind value = RK_clang__DeclaratorDecl; };
template<> struct GetTypeFromRK<RK_clang__DeclaratorDecl>
{ using type = clang::DeclaratorDecl; };
template<> struct GetReflectionObjKind<clang::EvaluatedStmt>
{ static constexpr ReflectionObjKind value = RK_clang__EvaluatedStmt; };
template<> struct GetTypeFromRK<RK_clang__EvaluatedStmt>
{ using type = clang::EvaluatedStmt; };
template<> struct GetReflectionObjKind<clang::VarDecl>
{ static constexpr ReflectionObjKind value = RK_clang__VarDecl; };
template<> struct GetTypeFromRK<RK_clang__VarDecl>
{ using type = clang::VarDecl; };
template<> struct GetReflectionObjKind<clang::ImplicitParamDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ImplicitParamDecl; };
template<> struct GetTypeFromRK<RK_clang__ImplicitParamDecl>
{ using type = clang::ImplicitParamDecl; };
template<> struct GetReflectionObjKind<clang::ParmVarDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ParmVarDecl; };
template<> struct GetTypeFromRK<RK_clang__ParmVarDecl>
{ using type = clang::ParmVarDecl; };
template<> struct GetReflectionObjKind<clang::FunctionDecl>
{ static constexpr ReflectionObjKind value = RK_clang__FunctionDecl; };
template<> struct GetTypeFromRK<RK_clang__FunctionDecl>
{ using type = clang::FunctionDecl; };
template<> struct GetReflectionObjKind<clang::FieldDecl>
{ static constexpr ReflectionObjKind value = RK_clang__FieldDecl; };
template<> struct GetTypeFromRK<RK_clang__FieldDecl>
{ using type = clang::FieldDecl; };
template<> struct GetReflectionObjKind<clang::EnumConstantDecl>
{ static constexpr ReflectionObjKind value = RK_clang__EnumConstantDecl; };
template<> struct GetTypeFromRK<RK_clang__EnumConstantDecl>
{ using type = clang::EnumConstantDecl; };
template<> struct GetReflectionObjKind<clang::IndirectFieldDecl>
{ static constexpr ReflectionObjKind value = RK_clang__IndirectFieldDecl; };
template<> struct GetTypeFromRK<RK_clang__IndirectFieldDecl>
{ using type = clang::IndirectFieldDecl; };
template<> struct GetReflectionObjKind<clang::TypeDecl>
{ static constexpr ReflectionObjKind value = RK_clang__TypeDecl; };
template<> struct GetTypeFromRK<RK_clang__TypeDecl>
{ using type = clang::TypeDecl; };
template<> struct GetReflectionObjKind<clang::TypedefNameDecl>
{ static constexpr ReflectionObjKind value = RK_clang__TypedefNameDecl; };
template<> struct GetTypeFromRK<RK_clang__TypedefNameDecl>
{ using type = clang::TypedefNameDecl; };
template<> struct GetReflectionObjKind<clang::TypedefDecl>
{ static constexpr ReflectionObjKind value = RK_clang__TypedefDecl; };
template<> struct GetTypeFromRK<RK_clang__TypedefDecl>
{ using type = clang::TypedefDecl; };
template<> struct GetReflectionObjKind<clang::TypeAliasDecl>
{ static constexpr ReflectionObjKind value = RK_clang__TypeAliasDecl; };
template<> struct GetTypeFromRK<RK_clang__TypeAliasDecl>
{ using type = clang::TypeAliasDecl; };
template<> struct GetReflectionObjKind<clang::TagDecl>
{ static constexpr ReflectionObjKind value = RK_clang__TagDecl; };
template<> struct GetTypeFromRK<RK_clang__TagDecl>
{ using type = clang::TagDecl; };
template<> struct GetReflectionObjKind<clang::EnumDecl>
{ static constexpr ReflectionObjKind value = RK_clang__EnumDecl; };
template<> struct GetTypeFromRK<RK_clang__EnumDecl>
{ using type = clang::EnumDecl; };
template<> struct GetReflectionObjKind<clang::RecordDecl>
{ static constexpr ReflectionObjKind value = RK_clang__RecordDecl; };
template<> struct GetTypeFromRK<RK_clang__RecordDecl>
{ using type = clang::RecordDecl; };
template<> struct GetReflectionObjKind<clang::FileScopeAsmDecl>
{ static constexpr ReflectionObjKind value = RK_clang__FileScopeAsmDecl; };
template<> struct GetTypeFromRK<RK_clang__FileScopeAsmDecl>
{ using type = clang::FileScopeAsmDecl; };
template<> struct GetReflectionObjKind<clang::BlockDecl>
{ static constexpr ReflectionObjKind value = RK_clang__BlockDecl; };
template<> struct GetTypeFromRK<RK_clang__BlockDecl>
{ using type = clang::BlockDecl; };
template<> struct GetReflectionObjKind<clang::BlockDecl::Capture>
{ static constexpr ReflectionObjKind value = RK_clang__BlockDecl__Capture; };
template<> struct GetTypeFromRK<RK_clang__BlockDecl__Capture>
{ using type = clang::BlockDecl::Capture; };
template<> struct GetReflectionObjKind<clang::CapturedDecl>
{ static constexpr ReflectionObjKind value = RK_clang__CapturedDecl; };
template<> struct GetTypeFromRK<RK_clang__CapturedDecl>
{ using type = clang::CapturedDecl; };
template<> struct GetReflectionObjKind<clang::ImportDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ImportDecl; };
template<> struct GetTypeFromRK<RK_clang__ImportDecl>
{ using type = clang::ImportDecl; };
template<> struct GetReflectionObjKind<clang::ExportDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ExportDecl; };
template<> struct GetTypeFromRK<RK_clang__ExportDecl>
{ using type = clang::ExportDecl; };
template<> struct GetReflectionObjKind<clang::EmptyDecl>
{ static constexpr ReflectionObjKind value = RK_clang__EmptyDecl; };
template<> struct GetTypeFromRK<RK_clang__EmptyDecl>
{ using type = clang::EmptyDecl; };
template<> struct GetReflectionObjKind<clang::DeclGroup>
{ static constexpr ReflectionObjKind value = RK_clang__DeclGroup; };
template<> struct GetTypeFromRK<RK_clang__DeclGroup>
{ using type = clang::DeclGroup; };
template<> struct GetReflectionObjKind<clang::DeclGroupRef>
{ static constexpr ReflectionObjKind value = RK_clang__DeclGroupRef; };
template<> struct GetTypeFromRK<RK_clang__DeclGroupRef>
{ using type = clang::DeclGroupRef; };
template<> struct GetReflectionObjKind<clang::Stmt>
{ static constexpr ReflectionObjKind value = RK_clang__Stmt; };
template<> struct GetTypeFromRK<RK_clang__Stmt>
{ using type = clang::Stmt; };
template<> struct GetReflectionObjKind<clang::Stmt::EmptyShell>
{ static constexpr ReflectionObjKind value = RK_clang__Stmt__EmptyShell; };
template<> struct GetTypeFromRK<RK_clang__Stmt__EmptyShell>
{ using type = clang::Stmt::EmptyShell; };
template<> struct GetReflectionObjKind<clang::DeclStmt>
{ static constexpr ReflectionObjKind value = RK_clang__DeclStmt; };
template<> struct GetTypeFromRK<RK_clang__DeclStmt>
{ using type = clang::DeclStmt; };
template<> struct GetReflectionObjKind<clang::NullStmt>
{ static constexpr ReflectionObjKind value = RK_clang__NullStmt; };
template<> struct GetTypeFromRK<RK_clang__NullStmt>
{ using type = clang::NullStmt; };
template<> struct GetReflectionObjKind<clang::CompoundStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CompoundStmt; };
template<> struct GetTypeFromRK<RK_clang__CompoundStmt>
{ using type = clang::CompoundStmt; };
template<> struct GetReflectionObjKind<clang::SwitchCase>
{ static constexpr ReflectionObjKind value = RK_clang__SwitchCase; };
template<> struct GetTypeFromRK<RK_clang__SwitchCase>
{ using type = clang::SwitchCase; };
template<> struct GetReflectionObjKind<clang::CaseStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CaseStmt; };
template<> struct GetTypeFromRK<RK_clang__CaseStmt>
{ using type = clang::CaseStmt; };
template<> struct GetReflectionObjKind<clang::DefaultStmt>
{ static constexpr ReflectionObjKind value = RK_clang__DefaultStmt; };
template<> struct GetTypeFromRK<RK_clang__DefaultStmt>
{ using type = clang::DefaultStmt; };
template<> struct GetReflectionObjKind<clang::LabelStmt>
{ static constexpr ReflectionObjKind value = RK_clang__LabelStmt; };
template<> struct GetTypeFromRK<RK_clang__LabelStmt>
{ using type = clang::LabelStmt; };
template<> struct GetReflectionObjKind<clang::AttributedStmt>
{ static constexpr ReflectionObjKind value = RK_clang__AttributedStmt; };
template<> struct GetTypeFromRK<RK_clang__AttributedStmt>
{ using type = clang::AttributedStmt; };
template<> struct GetReflectionObjKind<clang::IfStmt>
{ static constexpr ReflectionObjKind value = RK_clang__IfStmt; };
template<> struct GetTypeFromRK<RK_clang__IfStmt>
{ using type = clang::IfStmt; };
template<> struct GetReflectionObjKind<clang::SwitchStmt>
{ static constexpr ReflectionObjKind value = RK_clang__SwitchStmt; };
template<> struct GetTypeFromRK<RK_clang__SwitchStmt>
{ using type = clang::SwitchStmt; };
template<> struct GetReflectionObjKind<clang::WhileStmt>
{ static constexpr ReflectionObjKind value = RK_clang__WhileStmt; };
template<> struct GetTypeFromRK<RK_clang__WhileStmt>
{ using type = clang::WhileStmt; };
template<> struct GetReflectionObjKind<clang::DoStmt>
{ static constexpr ReflectionObjKind value = RK_clang__DoStmt; };
template<> struct GetTypeFromRK<RK_clang__DoStmt>
{ using type = clang::DoStmt; };
template<> struct GetReflectionObjKind<clang::ForStmt>
{ static constexpr ReflectionObjKind value = RK_clang__ForStmt; };
template<> struct GetTypeFromRK<RK_clang__ForStmt>
{ using type = clang::ForStmt; };
template<> struct GetReflectionObjKind<clang::GotoStmt>
{ static constexpr ReflectionObjKind value = RK_clang__GotoStmt; };
template<> struct GetTypeFromRK<RK_clang__GotoStmt>
{ using type = clang::GotoStmt; };
template<> struct GetReflectionObjKind<clang::IndirectGotoStmt>
{ static constexpr ReflectionObjKind value = RK_clang__IndirectGotoStmt; };
template<> struct GetTypeFromRK<RK_clang__IndirectGotoStmt>
{ using type = clang::IndirectGotoStmt; };
template<> struct GetReflectionObjKind<clang::ContinueStmt>
{ static constexpr ReflectionObjKind value = RK_clang__ContinueStmt; };
template<> struct GetTypeFromRK<RK_clang__ContinueStmt>
{ using type = clang::ContinueStmt; };
template<> struct GetReflectionObjKind<clang::BreakStmt>
{ static constexpr ReflectionObjKind value = RK_clang__BreakStmt; };
template<> struct GetTypeFromRK<RK_clang__BreakStmt>
{ using type = clang::BreakStmt; };
template<> struct GetReflectionObjKind<clang::ReturnStmt>
{ static constexpr ReflectionObjKind value = RK_clang__ReturnStmt; };
template<> struct GetTypeFromRK<RK_clang__ReturnStmt>
{ using type = clang::ReturnStmt; };
template<> struct GetReflectionObjKind<clang::AsmStmt>
{ static constexpr ReflectionObjKind value = RK_clang__AsmStmt; };
template<> struct GetTypeFromRK<RK_clang__AsmStmt>
{ using type = clang::AsmStmt; };
template<> struct GetReflectionObjKind<clang::GCCAsmStmt>
{ static constexpr ReflectionObjKind value = RK_clang__GCCAsmStmt; };
template<> struct GetTypeFromRK<RK_clang__GCCAsmStmt>
{ using type = clang::GCCAsmStmt; };
template<> struct GetReflectionObjKind<clang::MSAsmStmt>
{ static constexpr ReflectionObjKind value = RK_clang__MSAsmStmt; };
template<> struct GetTypeFromRK<RK_clang__MSAsmStmt>
{ using type = clang::MSAsmStmt; };
template<> struct GetReflectionObjKind<clang::SEHExceptStmt>
{ static constexpr ReflectionObjKind value = RK_clang__SEHExceptStmt; };
template<> struct GetTypeFromRK<RK_clang__SEHExceptStmt>
{ using type = clang::SEHExceptStmt; };
template<> struct GetReflectionObjKind<clang::SEHFinallyStmt>
{ static constexpr ReflectionObjKind value = RK_clang__SEHFinallyStmt; };
template<> struct GetTypeFromRK<RK_clang__SEHFinallyStmt>
{ using type = clang::SEHFinallyStmt; };
template<> struct GetReflectionObjKind<clang::SEHTryStmt>
{ static constexpr ReflectionObjKind value = RK_clang__SEHTryStmt; };
template<> struct GetTypeFromRK<RK_clang__SEHTryStmt>
{ using type = clang::SEHTryStmt; };
template<> struct GetReflectionObjKind<clang::SEHLeaveStmt>
{ static constexpr ReflectionObjKind value = RK_clang__SEHLeaveStmt; };
template<> struct GetTypeFromRK<RK_clang__SEHLeaveStmt>
{ using type = clang::SEHLeaveStmt; };
template<> struct GetReflectionObjKind<clang::CapturedStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CapturedStmt; };
template<> struct GetTypeFromRK<RK_clang__CapturedStmt>
{ using type = clang::CapturedStmt; };
template<> struct GetReflectionObjKind<clang::CapturedStmt::Capture>
{ static constexpr ReflectionObjKind value = RK_clang__CapturedStmt__Capture; };
template<> struct GetTypeFromRK<RK_clang__CapturedStmt__Capture>
{ using type = clang::CapturedStmt::Capture; };
template<> struct GetReflectionObjKind<clang::TemplateArgument>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateArgument; };
template<> struct GetTypeFromRK<RK_clang__TemplateArgument>
{ using type = clang::TemplateArgument; };
template<> struct GetReflectionObjKind<clang::TemplateArgumentLocInfo>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateArgumentLocInfo; };
template<> struct GetTypeFromRK<RK_clang__TemplateArgumentLocInfo>
{ using type = clang::TemplateArgumentLocInfo; };
template<> struct GetReflectionObjKind<clang::TemplateArgumentLoc>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateArgumentLoc; };
template<> struct GetTypeFromRK<RK_clang__TemplateArgumentLoc>
{ using type = clang::TemplateArgumentLoc; };
template<> struct GetReflectionObjKind<clang::TemplateArgumentListInfo>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateArgumentListInfo; };
template<> struct GetTypeFromRK<RK_clang__TemplateArgumentListInfo>
{ using type = clang::TemplateArgumentListInfo; };
template<> struct GetReflectionObjKind<clang::ASTTemplateArgumentListInfo>
{ static constexpr ReflectionObjKind value = RK_clang__ASTTemplateArgumentListInfo; };
template<> struct GetTypeFromRK<RK_clang__ASTTemplateArgumentListInfo>
{ using type = clang::ASTTemplateArgumentListInfo; };
template<> struct GetReflectionObjKind<clang::TypeLoc>
{ static constexpr ReflectionObjKind value = RK_clang__TypeLoc; };
template<> struct GetTypeFromRK<RK_clang__TypeLoc>
{ using type = clang::TypeLoc; };
template<> struct GetReflectionObjKind<clang::UnqualTypeLoc>
{ static constexpr ReflectionObjKind value = RK_clang__UnqualTypeLoc; };
template<> struct GetTypeFromRK<RK_clang__UnqualTypeLoc>
{ using type = clang::UnqualTypeLoc; };
template<> struct GetReflectionObjKind<clang::CommentOptions>
{ static constexpr ReflectionObjKind value = RK_clang__CommentOptions; };
template<> struct GetTypeFromRK<RK_clang__CommentOptions>
{ using type = clang::CommentOptions; };
template<> struct GetReflectionObjKind<clang::comments::CommandInfo>
{ static constexpr ReflectionObjKind value = RK_clang__comments__CommandInfo; };
template<> struct GetTypeFromRK<RK_clang__comments__CommandInfo>
{ using type = clang::comments::CommandInfo; };
template<> struct GetReflectionObjKind<clang::comments::CommandTraits>
{ static constexpr ReflectionObjKind value = RK_clang__comments__CommandTraits; };
template<> struct GetTypeFromRK<RK_clang__comments__CommandTraits>
{ using type = clang::comments::CommandTraits; };
template<> struct GetReflectionObjKind<llvm::Triple>
{ static constexpr ReflectionObjKind value = RK_llvm__Triple; };
template<> struct GetTypeFromRK<RK_llvm__Triple>
{ using type = llvm::Triple; };
template<> struct GetReflectionObjKind<clang::ObjCRuntime>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCRuntime; };
template<> struct GetTypeFromRK<RK_clang__ObjCRuntime>
{ using type = clang::ObjCRuntime; };
template<> struct GetReflectionObjKind<clang::SanitizerSet>
{ static constexpr ReflectionObjKind value = RK_clang__SanitizerSet; };
template<> struct GetTypeFromRK<RK_clang__SanitizerSet>
{ using type = clang::SanitizerSet; };
template<> struct GetReflectionObjKind<clang::LangOptionsBase>
{ static constexpr ReflectionObjKind value = RK_clang__LangOptionsBase; };
template<> struct GetTypeFromRK<RK_clang__LangOptionsBase>
{ using type = clang::LangOptionsBase; };
template<> struct GetReflectionObjKind<clang::LangOptions>
{ static constexpr ReflectionObjKind value = RK_clang__LangOptions; };
template<> struct GetTypeFromRK<RK_clang__LangOptions>
{ using type = clang::LangOptions; };
template<> struct GetReflectionObjKind<clang::FPOptions>
{ static constexpr ReflectionObjKind value = RK_clang__FPOptions; };
template<> struct GetTypeFromRK<RK_clang__FPOptions>
{ using type = clang::FPOptions; };
template<> struct GetReflectionObjKind<clang::PrinterHelper>
{ static constexpr ReflectionObjKind value = RK_clang__PrinterHelper; };
template<> struct GetTypeFromRK<RK_clang__PrinterHelper>
{ using type = clang::PrinterHelper; };
template<> struct GetReflectionObjKind<clang::PrintingPolicy>
{ static constexpr ReflectionObjKind value = RK_clang__PrintingPolicy; };
template<> struct GetTypeFromRK<RK_clang__PrintingPolicy>
{ using type = clang::PrintingPolicy; };
template<> struct GetReflectionObjKind<clang::SrcMgr::ContentCache>
{ static constexpr ReflectionObjKind value = RK_clang__SrcMgr__ContentCache; };
template<> struct GetTypeFromRK<RK_clang__SrcMgr__ContentCache>
{ using type = clang::SrcMgr::ContentCache; };
template<> struct GetReflectionObjKind<clang::SrcMgr::FileInfo>
{ static constexpr ReflectionObjKind value = RK_clang__SrcMgr__FileInfo; };
template<> struct GetTypeFromRK<RK_clang__SrcMgr__FileInfo>
{ using type = clang::SrcMgr::FileInfo; };
template<> struct GetReflectionObjKind<clang::SrcMgr::ExpansionInfo>
{ static constexpr ReflectionObjKind value = RK_clang__SrcMgr__ExpansionInfo; };
template<> struct GetTypeFromRK<RK_clang__SrcMgr__ExpansionInfo>
{ using type = clang::SrcMgr::ExpansionInfo; };
template<> struct GetReflectionObjKind<clang::SrcMgr::SLocEntry>
{ static constexpr ReflectionObjKind value = RK_clang__SrcMgr__SLocEntry; };
template<> struct GetTypeFromRK<RK_clang__SrcMgr__SLocEntry>
{ using type = clang::SrcMgr::SLocEntry; };
template<> struct GetReflectionObjKind<clang::SourceManager>
{ static constexpr ReflectionObjKind value = RK_clang__SourceManager; };
template<> struct GetTypeFromRK<RK_clang__SourceManager>
{ using type = clang::SourceManager; };
template<> struct GetReflectionObjKind<clang::RawComment>
{ static constexpr ReflectionObjKind value = RK_clang__RawComment; };
template<> struct GetTypeFromRK<RK_clang__RawComment>
{ using type = clang::RawComment; };
template<> struct GetReflectionObjKind<clang::SanitizerBlacklist>
{ static constexpr ReflectionObjKind value = RK_clang__SanitizerBlacklist; };
template<> struct GetTypeFromRK<RK_clang__SanitizerBlacklist>
{ using type = clang::SanitizerBlacklist; };
template<> struct GetReflectionObjKind<clang::TargetCXXABI>
{ static constexpr ReflectionObjKind value = RK_clang__TargetCXXABI; };
template<> struct GetTypeFromRK<RK_clang__TargetCXXABI>
{ using type = clang::TargetCXXABI; };
template<> struct GetReflectionObjKind<clang::OpenCLOptions>
{ static constexpr ReflectionObjKind value = RK_clang__OpenCLOptions; };
template<> struct GetTypeFromRK<RK_clang__OpenCLOptions>
{ using type = clang::OpenCLOptions; };
template<> struct GetReflectionObjKind<clang::TargetOptions>
{ static constexpr ReflectionObjKind value = RK_clang__TargetOptions; };
template<> struct GetTypeFromRK<RK_clang__TargetOptions>
{ using type = clang::TargetOptions; };
template<> struct GetReflectionObjKind<llvm::Type>
{ static constexpr ReflectionObjKind value = RK_llvm__Type; };
template<> struct GetTypeFromRK<RK_llvm__Type>
{ using type = llvm::Type; };
template<> struct GetReflectionObjKind<llvm::IntegerType>
{ static constexpr ReflectionObjKind value = RK_llvm__IntegerType; };
template<> struct GetTypeFromRK<RK_llvm__IntegerType>
{ using type = llvm::IntegerType; };
template<> struct GetReflectionObjKind<llvm::CompositeType>
{ static constexpr ReflectionObjKind value = RK_llvm__CompositeType; };
template<> struct GetTypeFromRK<RK_llvm__CompositeType>
{ using type = llvm::CompositeType; };
template<> struct GetReflectionObjKind<llvm::StructType>
{ static constexpr ReflectionObjKind value = RK_llvm__StructType; };
template<> struct GetTypeFromRK<RK_llvm__StructType>
{ using type = llvm::StructType; };
template<> struct GetReflectionObjKind<llvm::PointerType>
{ static constexpr ReflectionObjKind value = RK_llvm__PointerType; };
template<> struct GetTypeFromRK<RK_llvm__PointerType>
{ using type = llvm::PointerType; };
template<> struct GetReflectionObjKind<llvm::DataLayout>
{ static constexpr ReflectionObjKind value = RK_llvm__DataLayout; };
template<> struct GetTypeFromRK<RK_llvm__DataLayout>
{ using type = llvm::DataLayout; };
template<> struct GetReflectionObjKind<llvm::StructLayout>
{ static constexpr ReflectionObjKind value = RK_llvm__StructLayout; };
template<> struct GetTypeFromRK<RK_llvm__StructLayout>
{ using type = llvm::StructLayout; };
template<> struct GetReflectionObjKind<clang::TargetInfo>
{ static constexpr ReflectionObjKind value = RK_clang__TargetInfo; };
template<> struct GetTypeFromRK<RK_clang__TargetInfo>
{ using type = clang::TargetInfo; };
template<> struct GetReflectionObjKind<clang::TargetInfo::ConstraintInfo>
{ static constexpr ReflectionObjKind value = RK_clang__TargetInfo__ConstraintInfo; };
template<> struct GetTypeFromRK<RK_clang__TargetInfo__ConstraintInfo>
{ using type = clang::TargetInfo::ConstraintInfo; };
template<> struct GetReflectionObjKind<clang::XRayFunctionFilter>
{ static constexpr ReflectionObjKind value = RK_clang__XRayFunctionFilter; };
template<> struct GetTypeFromRK<RK_clang__XRayFunctionFilter>
{ using type = clang::XRayFunctionFilter; };
template<> struct GetReflectionObjKind<clang::TypeInfo>
{ static constexpr ReflectionObjKind value = RK_clang__TypeInfo; };
template<> struct GetTypeFromRK<RK_clang__TypeInfo>
{ using type = clang::TypeInfo; };
template<> struct GetReflectionObjKind<clang::ASTContext>
{ static constexpr ReflectionObjKind value = RK_clang__ASTContext; };
template<> struct GetTypeFromRK<RK_clang__ASTContext>
{ using type = clang::ASTContext; };
template<> struct GetReflectionObjKind<clang::DeclAccessPair>
{ static constexpr ReflectionObjKind value = RK_clang__DeclAccessPair; };
template<> struct GetTypeFromRK<RK_clang__DeclAccessPair>
{ using type = clang::DeclAccessPair; };
template<> struct GetReflectionObjKind<clang::UnresolvedSetIterator>
{ static constexpr ReflectionObjKind value = RK_clang__UnresolvedSetIterator; };
template<> struct GetTypeFromRK<RK_clang__UnresolvedSetIterator>
{ using type = clang::UnresolvedSetIterator; };
template<> struct GetReflectionObjKind<clang::UnresolvedSetImpl>
{ static constexpr ReflectionObjKind value = RK_clang__UnresolvedSetImpl; };
template<> struct GetTypeFromRK<RK_clang__UnresolvedSetImpl>
{ using type = clang::UnresolvedSetImpl; };
template<> struct GetReflectionObjKind<clang::Expr>
{ static constexpr ReflectionObjKind value = RK_clang__Expr; };
template<> struct GetTypeFromRK<RK_clang__Expr>
{ using type = clang::Expr; };
template<> struct GetReflectionObjKind<clang::Expr::Classification>
{ static constexpr ReflectionObjKind value = RK_clang__Expr__Classification; };
template<> struct GetTypeFromRK<RK_clang__Expr__Classification>
{ using type = clang::Expr::Classification; };
template<> struct GetReflectionObjKind<clang::Expr::EvalStatus>
{ static constexpr ReflectionObjKind value = RK_clang__Expr__EvalStatus; };
template<> struct GetTypeFromRK<RK_clang__Expr__EvalStatus>
{ using type = clang::Expr::EvalStatus; };
template<> struct GetReflectionObjKind<clang::Expr::EvalResult>
{ static constexpr ReflectionObjKind value = RK_clang__Expr__EvalResult; };
template<> struct GetTypeFromRK<RK_clang__Expr__EvalResult>
{ using type = clang::Expr::EvalResult; };
template<> struct GetReflectionObjKind<clang::OpaqueValueExpr>
{ static constexpr ReflectionObjKind value = RK_clang__OpaqueValueExpr; };
template<> struct GetTypeFromRK<RK_clang__OpaqueValueExpr>
{ using type = clang::OpaqueValueExpr; };
template<> struct GetReflectionObjKind<clang::DeclRefExpr>
{ static constexpr ReflectionObjKind value = RK_clang__DeclRefExpr; };
template<> struct GetTypeFromRK<RK_clang__DeclRefExpr>
{ using type = clang::DeclRefExpr; };
template<> struct GetReflectionObjKind<clang::PredefinedExpr>
{ static constexpr ReflectionObjKind value = RK_clang__PredefinedExpr; };
template<> struct GetTypeFromRK<RK_clang__PredefinedExpr>
{ using type = clang::PredefinedExpr; };
template<> struct GetReflectionObjKind<clang::APIntStorage>
{ static constexpr ReflectionObjKind value = RK_clang__APIntStorage; };
template<> struct GetTypeFromRK<RK_clang__APIntStorage>
{ using type = clang::APIntStorage; };
template<> struct GetReflectionObjKind<clang::IntegerLiteral>
{ static constexpr ReflectionObjKind value = RK_clang__IntegerLiteral; };
template<> struct GetTypeFromRK<RK_clang__IntegerLiteral>
{ using type = clang::IntegerLiteral; };
template<> struct GetReflectionObjKind<clang::FixedPointLiteral>
{ static constexpr ReflectionObjKind value = RK_clang__FixedPointLiteral; };
template<> struct GetTypeFromRK<RK_clang__FixedPointLiteral>
{ using type = clang::FixedPointLiteral; };
template<> struct GetReflectionObjKind<clang::CharacterLiteral>
{ static constexpr ReflectionObjKind value = RK_clang__CharacterLiteral; };
template<> struct GetTypeFromRK<RK_clang__CharacterLiteral>
{ using type = clang::CharacterLiteral; };
template<> struct GetReflectionObjKind<clang::FloatingLiteral>
{ static constexpr ReflectionObjKind value = RK_clang__FloatingLiteral; };
template<> struct GetTypeFromRK<RK_clang__FloatingLiteral>
{ using type = clang::FloatingLiteral; };
template<> struct GetReflectionObjKind<clang::ImaginaryLiteral>
{ static constexpr ReflectionObjKind value = RK_clang__ImaginaryLiteral; };
template<> struct GetTypeFromRK<RK_clang__ImaginaryLiteral>
{ using type = clang::ImaginaryLiteral; };
template<> struct GetReflectionObjKind<clang::StringLiteral>
{ static constexpr ReflectionObjKind value = RK_clang__StringLiteral; };
template<> struct GetTypeFromRK<RK_clang__StringLiteral>
{ using type = clang::StringLiteral; };
template<> struct GetReflectionObjKind<clang::ParenExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ParenExpr; };
template<> struct GetTypeFromRK<RK_clang__ParenExpr>
{ using type = clang::ParenExpr; };
template<> struct GetReflectionObjKind<clang::UnaryOperator>
{ static constexpr ReflectionObjKind value = RK_clang__UnaryOperator; };
template<> struct GetTypeFromRK<RK_clang__UnaryOperator>
{ using type = clang::UnaryOperator; };
template<> struct GetReflectionObjKind<clang::OffsetOfNode>
{ static constexpr ReflectionObjKind value = RK_clang__OffsetOfNode; };
template<> struct GetTypeFromRK<RK_clang__OffsetOfNode>
{ using type = clang::OffsetOfNode; };
template<> struct GetReflectionObjKind<clang::OffsetOfExpr>
{ static constexpr ReflectionObjKind value = RK_clang__OffsetOfExpr; };
template<> struct GetTypeFromRK<RK_clang__OffsetOfExpr>
{ using type = clang::OffsetOfExpr; };
template<> struct GetReflectionObjKind<clang::UnaryExprOrTypeTraitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__UnaryExprOrTypeTraitExpr; };
template<> struct GetTypeFromRK<RK_clang__UnaryExprOrTypeTraitExpr>
{ using type = clang::UnaryExprOrTypeTraitExpr; };
template<> struct GetReflectionObjKind<clang::ArraySubscriptExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ArraySubscriptExpr; };
template<> struct GetTypeFromRK<RK_clang__ArraySubscriptExpr>
{ using type = clang::ArraySubscriptExpr; };
template<> struct GetReflectionObjKind<clang::CallExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CallExpr; };
template<> struct GetTypeFromRK<RK_clang__CallExpr>
{ using type = clang::CallExpr; };
template<> struct GetReflectionObjKind<clang::MemberExpr>
{ static constexpr ReflectionObjKind value = RK_clang__MemberExpr; };
template<> struct GetTypeFromRK<RK_clang__MemberExpr>
{ using type = clang::MemberExpr; };
template<> struct GetReflectionObjKind<clang::CompoundLiteralExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CompoundLiteralExpr; };
template<> struct GetTypeFromRK<RK_clang__CompoundLiteralExpr>
{ using type = clang::CompoundLiteralExpr; };
template<> struct GetReflectionObjKind<clang::CastExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CastExpr; };
template<> struct GetTypeFromRK<RK_clang__CastExpr>
{ using type = clang::CastExpr; };
template<> struct GetReflectionObjKind<clang::ImplicitCastExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ImplicitCastExpr; };
template<> struct GetTypeFromRK<RK_clang__ImplicitCastExpr>
{ using type = clang::ImplicitCastExpr; };
template<> struct GetReflectionObjKind<clang::ExplicitCastExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ExplicitCastExpr; };
template<> struct GetTypeFromRK<RK_clang__ExplicitCastExpr>
{ using type = clang::ExplicitCastExpr; };
template<> struct GetReflectionObjKind<clang::CStyleCastExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CStyleCastExpr; };
template<> struct GetTypeFromRK<RK_clang__CStyleCastExpr>
{ using type = clang::CStyleCastExpr; };
template<> struct GetReflectionObjKind<clang::BinaryOperator>
{ static constexpr ReflectionObjKind value = RK_clang__BinaryOperator; };
template<> struct GetTypeFromRK<RK_clang__BinaryOperator>
{ using type = clang::BinaryOperator; };
template<> struct GetReflectionObjKind<clang::CompoundAssignOperator>
{ static constexpr ReflectionObjKind value = RK_clang__CompoundAssignOperator; };
template<> struct GetTypeFromRK<RK_clang__CompoundAssignOperator>
{ using type = clang::CompoundAssignOperator; };
template<> struct GetReflectionObjKind<clang::AbstractConditionalOperator>
{ static constexpr ReflectionObjKind value = RK_clang__AbstractConditionalOperator; };
template<> struct GetTypeFromRK<RK_clang__AbstractConditionalOperator>
{ using type = clang::AbstractConditionalOperator; };
template<> struct GetReflectionObjKind<clang::ConditionalOperator>
{ static constexpr ReflectionObjKind value = RK_clang__ConditionalOperator; };
template<> struct GetTypeFromRK<RK_clang__ConditionalOperator>
{ using type = clang::ConditionalOperator; };
template<> struct GetReflectionObjKind<clang::BinaryConditionalOperator>
{ static constexpr ReflectionObjKind value = RK_clang__BinaryConditionalOperator; };
template<> struct GetTypeFromRK<RK_clang__BinaryConditionalOperator>
{ using type = clang::BinaryConditionalOperator; };
template<> struct GetReflectionObjKind<clang::AddrLabelExpr>
{ static constexpr ReflectionObjKind value = RK_clang__AddrLabelExpr; };
template<> struct GetTypeFromRK<RK_clang__AddrLabelExpr>
{ using type = clang::AddrLabelExpr; };
template<> struct GetReflectionObjKind<clang::StmtExpr>
{ static constexpr ReflectionObjKind value = RK_clang__StmtExpr; };
template<> struct GetTypeFromRK<RK_clang__StmtExpr>
{ using type = clang::StmtExpr; };
template<> struct GetReflectionObjKind<clang::ShuffleVectorExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ShuffleVectorExpr; };
template<> struct GetTypeFromRK<RK_clang__ShuffleVectorExpr>
{ using type = clang::ShuffleVectorExpr; };
template<> struct GetReflectionObjKind<clang::ConvertVectorExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ConvertVectorExpr; };
template<> struct GetTypeFromRK<RK_clang__ConvertVectorExpr>
{ using type = clang::ConvertVectorExpr; };
template<> struct GetReflectionObjKind<clang::ChooseExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ChooseExpr; };
template<> struct GetTypeFromRK<RK_clang__ChooseExpr>
{ using type = clang::ChooseExpr; };
template<> struct GetReflectionObjKind<clang::GNUNullExpr>
{ static constexpr ReflectionObjKind value = RK_clang__GNUNullExpr; };
template<> struct GetTypeFromRK<RK_clang__GNUNullExpr>
{ using type = clang::GNUNullExpr; };
template<> struct GetReflectionObjKind<clang::VAArgExpr>
{ static constexpr ReflectionObjKind value = RK_clang__VAArgExpr; };
template<> struct GetTypeFromRK<RK_clang__VAArgExpr>
{ using type = clang::VAArgExpr; };
template<> struct GetReflectionObjKind<clang::InitListExpr>
{ static constexpr ReflectionObjKind value = RK_clang__InitListExpr; };
template<> struct GetTypeFromRK<RK_clang__InitListExpr>
{ using type = clang::InitListExpr; };
template<> struct GetReflectionObjKind<clang::DesignatedInitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__DesignatedInitExpr; };
template<> struct GetTypeFromRK<RK_clang__DesignatedInitExpr>
{ using type = clang::DesignatedInitExpr; };
template<> struct GetReflectionObjKind<clang::DesignatedInitExpr::Designator>
{ static constexpr ReflectionObjKind value = RK_clang__DesignatedInitExpr__Designator; };
template<> struct GetTypeFromRK<RK_clang__DesignatedInitExpr__Designator>
{ using type = clang::DesignatedInitExpr::Designator; };
template<> struct GetReflectionObjKind<clang::NoInitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__NoInitExpr; };
template<> struct GetTypeFromRK<RK_clang__NoInitExpr>
{ using type = clang::NoInitExpr; };
template<> struct GetReflectionObjKind<clang::DesignatedInitUpdateExpr>
{ static constexpr ReflectionObjKind value = RK_clang__DesignatedInitUpdateExpr; };
template<> struct GetTypeFromRK<RK_clang__DesignatedInitUpdateExpr>
{ using type = clang::DesignatedInitUpdateExpr; };
template<> struct GetReflectionObjKind<clang::ArrayInitLoopExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ArrayInitLoopExpr; };
template<> struct GetTypeFromRK<RK_clang__ArrayInitLoopExpr>
{ using type = clang::ArrayInitLoopExpr; };
template<> struct GetReflectionObjKind<clang::ArrayInitIndexExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ArrayInitIndexExpr; };
template<> struct GetTypeFromRK<RK_clang__ArrayInitIndexExpr>
{ using type = clang::ArrayInitIndexExpr; };
template<> struct GetReflectionObjKind<clang::ImplicitValueInitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ImplicitValueInitExpr; };
template<> struct GetTypeFromRK<RK_clang__ImplicitValueInitExpr>
{ using type = clang::ImplicitValueInitExpr; };
template<> struct GetReflectionObjKind<clang::ParenListExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ParenListExpr; };
template<> struct GetTypeFromRK<RK_clang__ParenListExpr>
{ using type = clang::ParenListExpr; };
template<> struct GetReflectionObjKind<clang::GenericSelectionExpr>
{ static constexpr ReflectionObjKind value = RK_clang__GenericSelectionExpr; };
template<> struct GetTypeFromRK<RK_clang__GenericSelectionExpr>
{ using type = clang::GenericSelectionExpr; };
template<> struct GetReflectionObjKind<clang::ExtVectorElementExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ExtVectorElementExpr; };
template<> struct GetTypeFromRK<RK_clang__ExtVectorElementExpr>
{ using type = clang::ExtVectorElementExpr; };
template<> struct GetReflectionObjKind<clang::BlockExpr>
{ static constexpr ReflectionObjKind value = RK_clang__BlockExpr; };
template<> struct GetTypeFromRK<RK_clang__BlockExpr>
{ using type = clang::BlockExpr; };
template<> struct GetReflectionObjKind<clang::AsTypeExpr>
{ static constexpr ReflectionObjKind value = RK_clang__AsTypeExpr; };
template<> struct GetTypeFromRK<RK_clang__AsTypeExpr>
{ using type = clang::AsTypeExpr; };
template<> struct GetReflectionObjKind<clang::PseudoObjectExpr>
{ static constexpr ReflectionObjKind value = RK_clang__PseudoObjectExpr; };
template<> struct GetTypeFromRK<RK_clang__PseudoObjectExpr>
{ using type = clang::PseudoObjectExpr; };
template<> struct GetReflectionObjKind<clang::AtomicExpr>
{ static constexpr ReflectionObjKind value = RK_clang__AtomicExpr; };
template<> struct GetTypeFromRK<RK_clang__AtomicExpr>
{ using type = clang::AtomicExpr; };
template<> struct GetReflectionObjKind<clang::TypoExpr>
{ static constexpr ReflectionObjKind value = RK_clang__TypoExpr; };
template<> struct GetTypeFromRK<RK_clang__TypoExpr>
{ using type = clang::TypoExpr; };
template<> struct GetReflectionObjKind<clang::Attr>
{ static constexpr ReflectionObjKind value = RK_clang__Attr; };
template<> struct GetTypeFromRK<RK_clang__Attr>
{ using type = clang::Attr; };
template<> struct GetReflectionObjKind<clang::InheritableAttr>
{ static constexpr ReflectionObjKind value = RK_clang__InheritableAttr; };
template<> struct GetTypeFromRK<RK_clang__InheritableAttr>
{ using type = clang::InheritableAttr; };
template<> struct GetReflectionObjKind<clang::ExternalSourceSymbolAttr>
{ static constexpr ReflectionObjKind value = RK_clang__ExternalSourceSymbolAttr; };
template<> struct GetTypeFromRK<RK_clang__ExternalSourceSymbolAttr>
{ using type = clang::ExternalSourceSymbolAttr; };
template<> struct GetReflectionObjKind<clang::MSInheritanceAttr>
{ static constexpr ReflectionObjKind value = RK_clang__MSInheritanceAttr; };
template<> struct GetTypeFromRK<RK_clang__MSInheritanceAttr>
{ using type = clang::MSInheritanceAttr; };
template<> struct GetReflectionObjKind<clang::MSVtorDispAttr>
{ static constexpr ReflectionObjKind value = RK_clang__MSVtorDispAttr; };
template<> struct GetTypeFromRK<RK_clang__MSVtorDispAttr>
{ using type = clang::MSVtorDispAttr; };
template<> struct GetReflectionObjKind<clang::LambdaCapture>
{ static constexpr ReflectionObjKind value = RK_clang__LambdaCapture; };
template<> struct GetTypeFromRK<RK_clang__LambdaCapture>
{ using type = clang::LambdaCapture; };
template<> struct GetReflectionObjKind<clang::AccessSpecDecl>
{ static constexpr ReflectionObjKind value = RK_clang__AccessSpecDecl; };
template<> struct GetTypeFromRK<RK_clang__AccessSpecDecl>
{ using type = clang::AccessSpecDecl; };
template<> struct GetReflectionObjKind<clang::CXXBaseSpecifier>
{ static constexpr ReflectionObjKind value = RK_clang__CXXBaseSpecifier; };
template<> struct GetTypeFromRK<RK_clang__CXXBaseSpecifier>
{ using type = clang::CXXBaseSpecifier; };
template<> struct GetReflectionObjKind<clang::CXXRecordDecl>
{ static constexpr ReflectionObjKind value = RK_clang__CXXRecordDecl; };
template<> struct GetTypeFromRK<RK_clang__CXXRecordDecl>
{ using type = clang::CXXRecordDecl; };
template<> struct GetReflectionObjKind<clang::CXXDeductionGuideDecl>
{ static constexpr ReflectionObjKind value = RK_clang__CXXDeductionGuideDecl; };
template<> struct GetTypeFromRK<RK_clang__CXXDeductionGuideDecl>
{ using type = clang::CXXDeductionGuideDecl; };
template<> struct GetReflectionObjKind<clang::CXXMethodDecl>
{ static constexpr ReflectionObjKind value = RK_clang__CXXMethodDecl; };
template<> struct GetTypeFromRK<RK_clang__CXXMethodDecl>
{ using type = clang::CXXMethodDecl; };
template<> struct GetReflectionObjKind<clang::CXXCtorInitializer>
{ static constexpr ReflectionObjKind value = RK_clang__CXXCtorInitializer; };
template<> struct GetTypeFromRK<RK_clang__CXXCtorInitializer>
{ using type = clang::CXXCtorInitializer; };
template<> struct GetReflectionObjKind<clang::InheritedConstructor>
{ static constexpr ReflectionObjKind value = RK_clang__InheritedConstructor; };
template<> struct GetTypeFromRK<RK_clang__InheritedConstructor>
{ using type = clang::InheritedConstructor; };
template<> struct GetReflectionObjKind<clang::CXXConstructorDecl>
{ static constexpr ReflectionObjKind value = RK_clang__CXXConstructorDecl; };
template<> struct GetTypeFromRK<RK_clang__CXXConstructorDecl>
{ using type = clang::CXXConstructorDecl; };
template<> struct GetReflectionObjKind<clang::CXXDestructorDecl>
{ static constexpr ReflectionObjKind value = RK_clang__CXXDestructorDecl; };
template<> struct GetTypeFromRK<RK_clang__CXXDestructorDecl>
{ using type = clang::CXXDestructorDecl; };
template<> struct GetReflectionObjKind<clang::CXXConversionDecl>
{ static constexpr ReflectionObjKind value = RK_clang__CXXConversionDecl; };
template<> struct GetTypeFromRK<RK_clang__CXXConversionDecl>
{ using type = clang::CXXConversionDecl; };
template<> struct GetReflectionObjKind<clang::LinkageSpecDecl>
{ static constexpr ReflectionObjKind value = RK_clang__LinkageSpecDecl; };
template<> struct GetTypeFromRK<RK_clang__LinkageSpecDecl>
{ using type = clang::LinkageSpecDecl; };
template<> struct GetReflectionObjKind<clang::UsingDirectiveDecl>
{ static constexpr ReflectionObjKind value = RK_clang__UsingDirectiveDecl; };
template<> struct GetTypeFromRK<RK_clang__UsingDirectiveDecl>
{ using type = clang::UsingDirectiveDecl; };
template<> struct GetReflectionObjKind<clang::NamespaceAliasDecl>
{ static constexpr ReflectionObjKind value = RK_clang__NamespaceAliasDecl; };
template<> struct GetTypeFromRK<RK_clang__NamespaceAliasDecl>
{ using type = clang::NamespaceAliasDecl; };
template<> struct GetReflectionObjKind<clang::UsingShadowDecl>
{ static constexpr ReflectionObjKind value = RK_clang__UsingShadowDecl; };
template<> struct GetTypeFromRK<RK_clang__UsingShadowDecl>
{ using type = clang::UsingShadowDecl; };
template<> struct GetReflectionObjKind<clang::ConstructorUsingShadowDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ConstructorUsingShadowDecl; };
template<> struct GetTypeFromRK<RK_clang__ConstructorUsingShadowDecl>
{ using type = clang::ConstructorUsingShadowDecl; };
template<> struct GetReflectionObjKind<clang::UsingDecl>
{ static constexpr ReflectionObjKind value = RK_clang__UsingDecl; };
template<> struct GetTypeFromRK<RK_clang__UsingDecl>
{ using type = clang::UsingDecl; };
template<> struct GetReflectionObjKind<clang::UsingPackDecl>
{ static constexpr ReflectionObjKind value = RK_clang__UsingPackDecl; };
template<> struct GetTypeFromRK<RK_clang__UsingPackDecl>
{ using type = clang::UsingPackDecl; };
template<> struct GetReflectionObjKind<clang::UnresolvedUsingValueDecl>
{ static constexpr ReflectionObjKind value = RK_clang__UnresolvedUsingValueDecl; };
template<> struct GetTypeFromRK<RK_clang__UnresolvedUsingValueDecl>
{ using type = clang::UnresolvedUsingValueDecl; };
template<> struct GetReflectionObjKind<clang::UnresolvedUsingTypenameDecl>
{ static constexpr ReflectionObjKind value = RK_clang__UnresolvedUsingTypenameDecl; };
template<> struct GetTypeFromRK<RK_clang__UnresolvedUsingTypenameDecl>
{ using type = clang::UnresolvedUsingTypenameDecl; };
template<> struct GetReflectionObjKind<clang::StaticAssertDecl>
{ static constexpr ReflectionObjKind value = RK_clang__StaticAssertDecl; };
template<> struct GetTypeFromRK<RK_clang__StaticAssertDecl>
{ using type = clang::StaticAssertDecl; };
template<> struct GetReflectionObjKind<clang::BindingDecl>
{ static constexpr ReflectionObjKind value = RK_clang__BindingDecl; };
template<> struct GetTypeFromRK<RK_clang__BindingDecl>
{ using type = clang::BindingDecl; };
template<> struct GetReflectionObjKind<clang::DecompositionDecl>
{ static constexpr ReflectionObjKind value = RK_clang__DecompositionDecl; };
template<> struct GetTypeFromRK<RK_clang__DecompositionDecl>
{ using type = clang::DecompositionDecl; };
template<> struct GetReflectionObjKind<clang::MSPropertyDecl>
{ static constexpr ReflectionObjKind value = RK_clang__MSPropertyDecl; };
template<> struct GetTypeFromRK<RK_clang__MSPropertyDecl>
{ using type = clang::MSPropertyDecl; };
template<> struct GetReflectionObjKind<clang::ConstexprDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ConstexprDecl; };
template<> struct GetTypeFromRK<RK_clang__ConstexprDecl>
{ using type = clang::ConstexprDecl; };
template<> struct GetReflectionObjKind<clang::TemplateParameterList>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateParameterList; };
template<> struct GetTypeFromRK<RK_clang__TemplateParameterList>
{ using type = clang::TemplateParameterList; };
template<> struct GetReflectionObjKind<clang::TemplateArgumentList>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateArgumentList; };
template<> struct GetTypeFromRK<RK_clang__TemplateArgumentList>
{ using type = clang::TemplateArgumentList; };
template<> struct GetReflectionObjKind<clang::TemplateDecl>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateDecl; };
template<> struct GetTypeFromRK<RK_clang__TemplateDecl>
{ using type = clang::TemplateDecl; };
template<> struct GetReflectionObjKind<clang::FunctionTemplateSpecializationInfo>
{ static constexpr ReflectionObjKind value = RK_clang__FunctionTemplateSpecializationInfo; };
template<> struct GetTypeFromRK<RK_clang__FunctionTemplateSpecializationInfo>
{ using type = clang::FunctionTemplateSpecializationInfo; };
template<> struct GetReflectionObjKind<clang::MemberSpecializationInfo>
{ static constexpr ReflectionObjKind value = RK_clang__MemberSpecializationInfo; };
template<> struct GetTypeFromRK<RK_clang__MemberSpecializationInfo>
{ using type = clang::MemberSpecializationInfo; };
template<> struct GetReflectionObjKind<clang::DependentFunctionTemplateSpecializationInfo>
{ static constexpr ReflectionObjKind value = RK_clang__DependentFunctionTemplateSpecializationInfo; };
template<> struct GetTypeFromRK<RK_clang__DependentFunctionTemplateSpecializationInfo>
{ using type = clang::DependentFunctionTemplateSpecializationInfo; };
template<> struct GetReflectionObjKind<clang::RedeclarableTemplateDecl>
{ static constexpr ReflectionObjKind value = RK_clang__RedeclarableTemplateDecl; };
template<> struct GetTypeFromRK<RK_clang__RedeclarableTemplateDecl>
{ using type = clang::RedeclarableTemplateDecl; };
template<> struct GetReflectionObjKind<clang::FunctionTemplateDecl>
{ static constexpr ReflectionObjKind value = RK_clang__FunctionTemplateDecl; };
template<> struct GetTypeFromRK<RK_clang__FunctionTemplateDecl>
{ using type = clang::FunctionTemplateDecl; };
template<> struct GetReflectionObjKind<clang::TemplateTypeParmDecl>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateTypeParmDecl; };
template<> struct GetTypeFromRK<RK_clang__TemplateTypeParmDecl>
{ using type = clang::TemplateTypeParmDecl; };
template<> struct GetReflectionObjKind<clang::NonTypeTemplateParmDecl>
{ static constexpr ReflectionObjKind value = RK_clang__NonTypeTemplateParmDecl; };
template<> struct GetTypeFromRK<RK_clang__NonTypeTemplateParmDecl>
{ using type = clang::NonTypeTemplateParmDecl; };
template<> struct GetReflectionObjKind<clang::TemplateTemplateParmDecl>
{ static constexpr ReflectionObjKind value = RK_clang__TemplateTemplateParmDecl; };
template<> struct GetTypeFromRK<RK_clang__TemplateTemplateParmDecl>
{ using type = clang::TemplateTemplateParmDecl; };
template<> struct GetReflectionObjKind<clang::BuiltinTemplateDecl>
{ static constexpr ReflectionObjKind value = RK_clang__BuiltinTemplateDecl; };
template<> struct GetTypeFromRK<RK_clang__BuiltinTemplateDecl>
{ using type = clang::BuiltinTemplateDecl; };
template<> struct GetReflectionObjKind<clang::ClassTemplateSpecializationDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ClassTemplateSpecializationDecl; };
template<> struct GetTypeFromRK<RK_clang__ClassTemplateSpecializationDecl>
{ using type = clang::ClassTemplateSpecializationDecl; };
template<> struct GetReflectionObjKind<clang::ClassTemplatePartialSpecializationDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ClassTemplatePartialSpecializationDecl; };
template<> struct GetTypeFromRK<RK_clang__ClassTemplatePartialSpecializationDecl>
{ using type = clang::ClassTemplatePartialSpecializationDecl; };
template<> struct GetReflectionObjKind<clang::ClassTemplateDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ClassTemplateDecl; };
template<> struct GetTypeFromRK<RK_clang__ClassTemplateDecl>
{ using type = clang::ClassTemplateDecl; };
template<> struct GetReflectionObjKind<clang::FriendTemplateDecl>
{ static constexpr ReflectionObjKind value = RK_clang__FriendTemplateDecl; };
template<> struct GetTypeFromRK<RK_clang__FriendTemplateDecl>
{ using type = clang::FriendTemplateDecl; };
template<> struct GetReflectionObjKind<clang::TypeAliasTemplateDecl>
{ static constexpr ReflectionObjKind value = RK_clang__TypeAliasTemplateDecl; };
template<> struct GetTypeFromRK<RK_clang__TypeAliasTemplateDecl>
{ using type = clang::TypeAliasTemplateDecl; };
template<> struct GetReflectionObjKind<clang::ClassScopeFunctionSpecializationDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ClassScopeFunctionSpecializationDecl; };
template<> struct GetTypeFromRK<RK_clang__ClassScopeFunctionSpecializationDecl>
{ using type = clang::ClassScopeFunctionSpecializationDecl; };
template<> struct GetReflectionObjKind<clang::VarTemplateSpecializationDecl>
{ static constexpr ReflectionObjKind value = RK_clang__VarTemplateSpecializationDecl; };
template<> struct GetTypeFromRK<RK_clang__VarTemplateSpecializationDecl>
{ using type = clang::VarTemplateSpecializationDecl; };
template<> struct GetReflectionObjKind<clang::VarTemplatePartialSpecializationDecl>
{ static constexpr ReflectionObjKind value = RK_clang__VarTemplatePartialSpecializationDecl; };
template<> struct GetTypeFromRK<RK_clang__VarTemplatePartialSpecializationDecl>
{ using type = clang::VarTemplatePartialSpecializationDecl; };
template<> struct GetReflectionObjKind<clang::VarTemplateDecl>
{ static constexpr ReflectionObjKind value = RK_clang__VarTemplateDecl; };
template<> struct GetTypeFromRK<RK_clang__VarTemplateDecl>
{ using type = clang::VarTemplateDecl; };
template<> struct GetReflectionObjKind<clang::FriendDecl>
{ static constexpr ReflectionObjKind value = RK_clang__FriendDecl; };
template<> struct GetTypeFromRK<RK_clang__FriendDecl>
{ using type = clang::FriendDecl; };
template<> struct GetReflectionObjKind<clang::ObjCListBase>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCListBase; };
template<> struct GetTypeFromRK<RK_clang__ObjCListBase>
{ using type = clang::ObjCListBase; };
template<> struct GetReflectionObjKind<clang::ObjCProtocolList>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCProtocolList; };
template<> struct GetTypeFromRK<RK_clang__ObjCProtocolList>
{ using type = clang::ObjCProtocolList; };
template<> struct GetReflectionObjKind<clang::ObjCMethodDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCMethodDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCMethodDecl>
{ using type = clang::ObjCMethodDecl; };
template<> struct GetReflectionObjKind<clang::ObjCTypeParamDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCTypeParamDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCTypeParamDecl>
{ using type = clang::ObjCTypeParamDecl; };
template<> struct GetReflectionObjKind<clang::ObjCTypeParamList>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCTypeParamList; };
template<> struct GetTypeFromRK<RK_clang__ObjCTypeParamList>
{ using type = clang::ObjCTypeParamList; };
template<> struct GetReflectionObjKind<clang::ObjCPropertyDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCPropertyDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCPropertyDecl>
{ using type = clang::ObjCPropertyDecl; };
template<> struct GetReflectionObjKind<clang::ObjCContainerDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCContainerDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCContainerDecl>
{ using type = clang::ObjCContainerDecl; };
template<> struct GetReflectionObjKind<clang::ObjCInterfaceDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCInterfaceDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCInterfaceDecl>
{ using type = clang::ObjCInterfaceDecl; };
template<> struct GetReflectionObjKind<clang::ObjCIvarDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCIvarDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCIvarDecl>
{ using type = clang::ObjCIvarDecl; };
template<> struct GetReflectionObjKind<clang::ObjCAtDefsFieldDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCAtDefsFieldDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCAtDefsFieldDecl>
{ using type = clang::ObjCAtDefsFieldDecl; };
template<> struct GetReflectionObjKind<clang::ObjCProtocolDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCProtocolDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCProtocolDecl>
{ using type = clang::ObjCProtocolDecl; };
template<> struct GetReflectionObjKind<clang::ObjCCategoryDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCCategoryDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCCategoryDecl>
{ using type = clang::ObjCCategoryDecl; };
template<> struct GetReflectionObjKind<clang::ObjCImplDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCImplDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCImplDecl>
{ using type = clang::ObjCImplDecl; };
template<> struct GetReflectionObjKind<clang::ObjCCategoryImplDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCCategoryImplDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCCategoryImplDecl>
{ using type = clang::ObjCCategoryImplDecl; };
template<> struct GetReflectionObjKind<clang::ObjCImplementationDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCImplementationDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCImplementationDecl>
{ using type = clang::ObjCImplementationDecl; };
template<> struct GetReflectionObjKind<clang::ObjCCompatibleAliasDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCCompatibleAliasDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCCompatibleAliasDecl>
{ using type = clang::ObjCCompatibleAliasDecl; };
template<> struct GetReflectionObjKind<clang::ObjCPropertyImplDecl>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCPropertyImplDecl; };
template<> struct GetTypeFromRK<RK_clang__ObjCPropertyImplDecl>
{ using type = clang::ObjCPropertyImplDecl; };
template<> struct GetReflectionObjKind<clang::OMPThreadPrivateDecl>
{ static constexpr ReflectionObjKind value = RK_clang__OMPThreadPrivateDecl; };
template<> struct GetTypeFromRK<RK_clang__OMPThreadPrivateDecl>
{ using type = clang::OMPThreadPrivateDecl; };
template<> struct GetReflectionObjKind<clang::OMPDeclareReductionDecl>
{ static constexpr ReflectionObjKind value = RK_clang__OMPDeclareReductionDecl; };
template<> struct GetTypeFromRK<RK_clang__OMPDeclareReductionDecl>
{ using type = clang::OMPDeclareReductionDecl; };
template<> struct GetReflectionObjKind<clang::OMPCapturedExprDecl>
{ static constexpr ReflectionObjKind value = RK_clang__OMPCapturedExprDecl; };
template<> struct GetTypeFromRK<RK_clang__OMPCapturedExprDecl>
{ using type = clang::OMPCapturedExprDecl; };
template<> struct GetReflectionObjKind<clang::CXXOperatorCallExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXOperatorCallExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXOperatorCallExpr>
{ using type = clang::CXXOperatorCallExpr; };
template<> struct GetReflectionObjKind<clang::CXXMemberCallExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXMemberCallExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXMemberCallExpr>
{ using type = clang::CXXMemberCallExpr; };
template<> struct GetReflectionObjKind<clang::CUDAKernelCallExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CUDAKernelCallExpr; };
template<> struct GetTypeFromRK<RK_clang__CUDAKernelCallExpr>
{ using type = clang::CUDAKernelCallExpr; };
template<> struct GetReflectionObjKind<clang::CXXNamedCastExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXNamedCastExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXNamedCastExpr>
{ using type = clang::CXXNamedCastExpr; };
template<> struct GetReflectionObjKind<clang::CXXStaticCastExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXStaticCastExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXStaticCastExpr>
{ using type = clang::CXXStaticCastExpr; };
template<> struct GetReflectionObjKind<clang::CXXDynamicCastExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXDynamicCastExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXDynamicCastExpr>
{ using type = clang::CXXDynamicCastExpr; };
template<> struct GetReflectionObjKind<clang::CXXReinterpretCastExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXReinterpretCastExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXReinterpretCastExpr>
{ using type = clang::CXXReinterpretCastExpr; };
template<> struct GetReflectionObjKind<clang::CXXConstCastExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXConstCastExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXConstCastExpr>
{ using type = clang::CXXConstCastExpr; };
template<> struct GetReflectionObjKind<clang::UserDefinedLiteral>
{ static constexpr ReflectionObjKind value = RK_clang__UserDefinedLiteral; };
template<> struct GetTypeFromRK<RK_clang__UserDefinedLiteral>
{ using type = clang::UserDefinedLiteral; };
template<> struct GetReflectionObjKind<clang::CXXBoolLiteralExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXBoolLiteralExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXBoolLiteralExpr>
{ using type = clang::CXXBoolLiteralExpr; };
template<> struct GetReflectionObjKind<clang::CXXNullPtrLiteralExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXNullPtrLiteralExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXNullPtrLiteralExpr>
{ using type = clang::CXXNullPtrLiteralExpr; };
template<> struct GetReflectionObjKind<clang::CXXStdInitializerListExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXStdInitializerListExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXStdInitializerListExpr>
{ using type = clang::CXXStdInitializerListExpr; };
template<> struct GetReflectionObjKind<clang::CXXTypeidExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXTypeidExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXTypeidExpr>
{ using type = clang::CXXTypeidExpr; };
template<> struct GetReflectionObjKind<clang::MSPropertyRefExpr>
{ static constexpr ReflectionObjKind value = RK_clang__MSPropertyRefExpr; };
template<> struct GetTypeFromRK<RK_clang__MSPropertyRefExpr>
{ using type = clang::MSPropertyRefExpr; };
template<> struct GetReflectionObjKind<clang::MSPropertySubscriptExpr>
{ static constexpr ReflectionObjKind value = RK_clang__MSPropertySubscriptExpr; };
template<> struct GetTypeFromRK<RK_clang__MSPropertySubscriptExpr>
{ using type = clang::MSPropertySubscriptExpr; };
template<> struct GetReflectionObjKind<clang::CXXUuidofExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXUuidofExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXUuidofExpr>
{ using type = clang::CXXUuidofExpr; };
template<> struct GetReflectionObjKind<clang::CXXThisExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXThisExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXThisExpr>
{ using type = clang::CXXThisExpr; };
template<> struct GetReflectionObjKind<clang::CXXThrowExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXThrowExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXThrowExpr>
{ using type = clang::CXXThrowExpr; };
template<> struct GetReflectionObjKind<clang::CXXDefaultArgExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXDefaultArgExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXDefaultArgExpr>
{ using type = clang::CXXDefaultArgExpr; };
template<> struct GetReflectionObjKind<clang::CXXDefaultInitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXDefaultInitExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXDefaultInitExpr>
{ using type = clang::CXXDefaultInitExpr; };
template<> struct GetReflectionObjKind<clang::CXXTemporary>
{ static constexpr ReflectionObjKind value = RK_clang__CXXTemporary; };
template<> struct GetTypeFromRK<RK_clang__CXXTemporary>
{ using type = clang::CXXTemporary; };
template<> struct GetReflectionObjKind<clang::CXXBindTemporaryExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXBindTemporaryExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXBindTemporaryExpr>
{ using type = clang::CXXBindTemporaryExpr; };
template<> struct GetReflectionObjKind<clang::CXXConstructExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXConstructExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXConstructExpr>
{ using type = clang::CXXConstructExpr; };
template<> struct GetReflectionObjKind<clang::CXXInheritedCtorInitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXInheritedCtorInitExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXInheritedCtorInitExpr>
{ using type = clang::CXXInheritedCtorInitExpr; };
template<> struct GetReflectionObjKind<clang::CXXFunctionalCastExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXFunctionalCastExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXFunctionalCastExpr>
{ using type = clang::CXXFunctionalCastExpr; };
template<> struct GetReflectionObjKind<clang::CXXTemporaryObjectExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXTemporaryObjectExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXTemporaryObjectExpr>
{ using type = clang::CXXTemporaryObjectExpr; };
template<> struct GetReflectionObjKind<clang::LambdaExpr>
{ static constexpr ReflectionObjKind value = RK_clang__LambdaExpr; };
template<> struct GetTypeFromRK<RK_clang__LambdaExpr>
{ using type = clang::LambdaExpr; };
template<> struct GetReflectionObjKind<clang::CXXScalarValueInitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXScalarValueInitExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXScalarValueInitExpr>
{ using type = clang::CXXScalarValueInitExpr; };
template<> struct GetReflectionObjKind<clang::CXXNewExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXNewExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXNewExpr>
{ using type = clang::CXXNewExpr; };
template<> struct GetReflectionObjKind<clang::CXXDeleteExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXDeleteExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXDeleteExpr>
{ using type = clang::CXXDeleteExpr; };
template<> struct GetReflectionObjKind<clang::CXXPseudoDestructorExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXPseudoDestructorExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXPseudoDestructorExpr>
{ using type = clang::CXXPseudoDestructorExpr; };
template<> struct GetReflectionObjKind<clang::TypeTraitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__TypeTraitExpr; };
template<> struct GetTypeFromRK<RK_clang__TypeTraitExpr>
{ using type = clang::TypeTraitExpr; };
template<> struct GetReflectionObjKind<clang::ArrayTypeTraitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ArrayTypeTraitExpr; };
template<> struct GetTypeFromRK<RK_clang__ArrayTypeTraitExpr>
{ using type = clang::ArrayTypeTraitExpr; };
template<> struct GetReflectionObjKind<clang::ExpressionTraitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ExpressionTraitExpr; };
template<> struct GetTypeFromRK<RK_clang__ExpressionTraitExpr>
{ using type = clang::ExpressionTraitExpr; };
template<> struct GetReflectionObjKind<clang::OverloadExpr>
{ static constexpr ReflectionObjKind value = RK_clang__OverloadExpr; };
template<> struct GetTypeFromRK<RK_clang__OverloadExpr>
{ using type = clang::OverloadExpr; };
template<> struct GetReflectionObjKind<clang::OverloadExpr::FindResult>
{ static constexpr ReflectionObjKind value = RK_clang__OverloadExpr__FindResult; };
template<> struct GetTypeFromRK<RK_clang__OverloadExpr__FindResult>
{ using type = clang::OverloadExpr::FindResult; };
template<> struct GetReflectionObjKind<clang::UnresolvedLookupExpr>
{ static constexpr ReflectionObjKind value = RK_clang__UnresolvedLookupExpr; };
template<> struct GetTypeFromRK<RK_clang__UnresolvedLookupExpr>
{ using type = clang::UnresolvedLookupExpr; };
template<> struct GetReflectionObjKind<clang::DependentScopeDeclRefExpr>
{ static constexpr ReflectionObjKind value = RK_clang__DependentScopeDeclRefExpr; };
template<> struct GetTypeFromRK<RK_clang__DependentScopeDeclRefExpr>
{ using type = clang::DependentScopeDeclRefExpr; };
template<> struct GetReflectionObjKind<clang::ExprWithCleanups>
{ static constexpr ReflectionObjKind value = RK_clang__ExprWithCleanups; };
template<> struct GetTypeFromRK<RK_clang__ExprWithCleanups>
{ using type = clang::ExprWithCleanups; };
template<> struct GetReflectionObjKind<clang::CXXUnresolvedConstructExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXUnresolvedConstructExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXUnresolvedConstructExpr>
{ using type = clang::CXXUnresolvedConstructExpr; };
template<> struct GetReflectionObjKind<clang::CXXDependentScopeMemberExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXDependentScopeMemberExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXDependentScopeMemberExpr>
{ using type = clang::CXXDependentScopeMemberExpr; };
template<> struct GetReflectionObjKind<clang::UnresolvedMemberExpr>
{ static constexpr ReflectionObjKind value = RK_clang__UnresolvedMemberExpr; };
template<> struct GetTypeFromRK<RK_clang__UnresolvedMemberExpr>
{ using type = clang::UnresolvedMemberExpr; };
template<> struct GetReflectionObjKind<clang::CXXNoexceptExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXNoexceptExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXNoexceptExpr>
{ using type = clang::CXXNoexceptExpr; };
template<> struct GetReflectionObjKind<clang::PackExpansionExpr>
{ static constexpr ReflectionObjKind value = RK_clang__PackExpansionExpr; };
template<> struct GetTypeFromRK<RK_clang__PackExpansionExpr>
{ using type = clang::PackExpansionExpr; };
template<> struct GetReflectionObjKind<clang::SizeOfPackExpr>
{ static constexpr ReflectionObjKind value = RK_clang__SizeOfPackExpr; };
template<> struct GetTypeFromRK<RK_clang__SizeOfPackExpr>
{ using type = clang::SizeOfPackExpr; };
template<> struct GetReflectionObjKind<clang::SubstNonTypeTemplateParmExpr>
{ static constexpr ReflectionObjKind value = RK_clang__SubstNonTypeTemplateParmExpr; };
template<> struct GetTypeFromRK<RK_clang__SubstNonTypeTemplateParmExpr>
{ using type = clang::SubstNonTypeTemplateParmExpr; };
template<> struct GetReflectionObjKind<clang::SubstNonTypeTemplateParmPackExpr>
{ static constexpr ReflectionObjKind value = RK_clang__SubstNonTypeTemplateParmPackExpr; };
template<> struct GetTypeFromRK<RK_clang__SubstNonTypeTemplateParmPackExpr>
{ using type = clang::SubstNonTypeTemplateParmPackExpr; };
template<> struct GetReflectionObjKind<clang::FunctionParmPackExpr>
{ static constexpr ReflectionObjKind value = RK_clang__FunctionParmPackExpr; };
template<> struct GetTypeFromRK<RK_clang__FunctionParmPackExpr>
{ using type = clang::FunctionParmPackExpr; };
template<> struct GetReflectionObjKind<clang::MaterializeTemporaryExpr>
{ static constexpr ReflectionObjKind value = RK_clang__MaterializeTemporaryExpr; };
template<> struct GetTypeFromRK<RK_clang__MaterializeTemporaryExpr>
{ using type = clang::MaterializeTemporaryExpr; };
template<> struct GetReflectionObjKind<clang::CXXFoldExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXFoldExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXFoldExpr>
{ using type = clang::CXXFoldExpr; };
template<> struct GetReflectionObjKind<clang::CoroutineSuspendExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CoroutineSuspendExpr; };
template<> struct GetTypeFromRK<RK_clang__CoroutineSuspendExpr>
{ using type = clang::CoroutineSuspendExpr; };
template<> struct GetReflectionObjKind<clang::CoawaitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CoawaitExpr; };
template<> struct GetTypeFromRK<RK_clang__CoawaitExpr>
{ using type = clang::CoawaitExpr; };
template<> struct GetReflectionObjKind<clang::DependentCoawaitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__DependentCoawaitExpr; };
template<> struct GetTypeFromRK<RK_clang__DependentCoawaitExpr>
{ using type = clang::DependentCoawaitExpr; };
template<> struct GetReflectionObjKind<clang::CoyieldExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CoyieldExpr; };
template<> struct GetTypeFromRK<RK_clang__CoyieldExpr>
{ using type = clang::CoyieldExpr; };
template<> struct GetReflectionObjKind<clang::ReflectionExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ReflectionExpr; };
template<> struct GetTypeFromRK<RK_clang__ReflectionExpr>
{ using type = clang::ReflectionExpr; };
template<> struct GetReflectionObjKind<clang::CompilerMessageExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CompilerMessageExpr; };
template<> struct GetTypeFromRK<RK_clang__CompilerMessageExpr>
{ using type = clang::CompilerMessageExpr; };
template<> struct GetReflectionObjKind<clang::CompilerDiagnosticExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CompilerDiagnosticExpr; };
template<> struct GetTypeFromRK<RK_clang__CompilerDiagnosticExpr>
{ using type = clang::CompilerDiagnosticExpr; };
template<> struct GetReflectionObjKind<clang::CXXMetaparseExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXMetaparseExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXMetaparseExpr>
{ using type = clang::CXXMetaparseExpr; };
template<> struct GetReflectionObjKind<clang::ReflectionTraitExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ReflectionTraitExpr; };
template<> struct GetTypeFromRK<RK_clang__ReflectionTraitExpr>
{ using type = clang::ReflectionTraitExpr; };
template<> struct GetReflectionObjKind<clang::ReflectNewExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ReflectNewExpr; };
template<> struct GetTypeFromRK<RK_clang__ReflectNewExpr>
{ using type = clang::ReflectNewExpr; };
template<> struct GetReflectionObjKind<clang::ReflectDeleteExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ReflectDeleteExpr; };
template<> struct GetTypeFromRK<RK_clang__ReflectDeleteExpr>
{ using type = clang::ReflectDeleteExpr; };
template<> struct GetReflectionObjKind<clang::CXXConstantExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXConstantExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXConstantExpr>
{ using type = clang::CXXConstantExpr; };
template<> struct GetReflectionObjKind<clang::CXXDependentIdExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXDependentIdExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXDependentIdExpr>
{ using type = clang::CXXDependentIdExpr; };
template<> struct GetReflectionObjKind<clang::CXXConcatenateExpr>
{ static constexpr ReflectionObjKind value = RK_clang__CXXConcatenateExpr; };
template<> struct GetTypeFromRK<RK_clang__CXXConcatenateExpr>
{ using type = clang::CXXConcatenateExpr; };
template<> struct GetReflectionObjKind<clang::ObjCStringLiteral>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCStringLiteral; };
template<> struct GetTypeFromRK<RK_clang__ObjCStringLiteral>
{ using type = clang::ObjCStringLiteral; };
template<> struct GetReflectionObjKind<clang::ObjCBoolLiteralExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCBoolLiteralExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCBoolLiteralExpr>
{ using type = clang::ObjCBoolLiteralExpr; };
template<> struct GetReflectionObjKind<clang::ObjCBoxedExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCBoxedExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCBoxedExpr>
{ using type = clang::ObjCBoxedExpr; };
template<> struct GetReflectionObjKind<clang::ObjCArrayLiteral>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCArrayLiteral; };
template<> struct GetTypeFromRK<RK_clang__ObjCArrayLiteral>
{ using type = clang::ObjCArrayLiteral; };
template<> struct GetReflectionObjKind<clang::ObjCDictionaryElement>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCDictionaryElement; };
template<> struct GetTypeFromRK<RK_clang__ObjCDictionaryElement>
{ using type = clang::ObjCDictionaryElement; };
template<> struct GetReflectionObjKind<clang::ObjCDictionaryLiteral>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCDictionaryLiteral; };
template<> struct GetTypeFromRK<RK_clang__ObjCDictionaryLiteral>
{ using type = clang::ObjCDictionaryLiteral; };
template<> struct GetReflectionObjKind<clang::ObjCEncodeExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCEncodeExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCEncodeExpr>
{ using type = clang::ObjCEncodeExpr; };
template<> struct GetReflectionObjKind<clang::ObjCSelectorExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCSelectorExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCSelectorExpr>
{ using type = clang::ObjCSelectorExpr; };
template<> struct GetReflectionObjKind<clang::ObjCProtocolExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCProtocolExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCProtocolExpr>
{ using type = clang::ObjCProtocolExpr; };
template<> struct GetReflectionObjKind<clang::ObjCIvarRefExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCIvarRefExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCIvarRefExpr>
{ using type = clang::ObjCIvarRefExpr; };
template<> struct GetReflectionObjKind<clang::ObjCPropertyRefExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCPropertyRefExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCPropertyRefExpr>
{ using type = clang::ObjCPropertyRefExpr; };
template<> struct GetReflectionObjKind<clang::ObjCSubscriptRefExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCSubscriptRefExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCSubscriptRefExpr>
{ using type = clang::ObjCSubscriptRefExpr; };
template<> struct GetReflectionObjKind<clang::ObjCMessageExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCMessageExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCMessageExpr>
{ using type = clang::ObjCMessageExpr; };
template<> struct GetReflectionObjKind<clang::ObjCIsaExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCIsaExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCIsaExpr>
{ using type = clang::ObjCIsaExpr; };
template<> struct GetReflectionObjKind<clang::ObjCIndirectCopyRestoreExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCIndirectCopyRestoreExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCIndirectCopyRestoreExpr>
{ using type = clang::ObjCIndirectCopyRestoreExpr; };
template<> struct GetReflectionObjKind<clang::ObjCBridgedCastExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCBridgedCastExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCBridgedCastExpr>
{ using type = clang::ObjCBridgedCastExpr; };
template<> struct GetReflectionObjKind<clang::ObjCAvailabilityCheckExpr>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCAvailabilityCheckExpr; };
template<> struct GetTypeFromRK<RK_clang__ObjCAvailabilityCheckExpr>
{ using type = clang::ObjCAvailabilityCheckExpr; };
template<> struct GetReflectionObjKind<clang::OMPArraySectionExpr>
{ static constexpr ReflectionObjKind value = RK_clang__OMPArraySectionExpr; };
template<> struct GetTypeFromRK<RK_clang__OMPArraySectionExpr>
{ using type = clang::OMPArraySectionExpr; };
template<> struct GetReflectionObjKind<clang::CXXCatchStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CXXCatchStmt; };
template<> struct GetTypeFromRK<RK_clang__CXXCatchStmt>
{ using type = clang::CXXCatchStmt; };
template<> struct GetReflectionObjKind<clang::CXXTryStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CXXTryStmt; };
template<> struct GetTypeFromRK<RK_clang__CXXTryStmt>
{ using type = clang::CXXTryStmt; };
template<> struct GetReflectionObjKind<clang::CXXForRangeStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CXXForRangeStmt; };
template<> struct GetTypeFromRK<RK_clang__CXXForRangeStmt>
{ using type = clang::CXXForRangeStmt; };
template<> struct GetReflectionObjKind<clang::CXXExpansionStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CXXExpansionStmt; };
template<> struct GetTypeFromRK<RK_clang__CXXExpansionStmt>
{ using type = clang::CXXExpansionStmt; };
template<> struct GetReflectionObjKind<clang::CXXTupleExpansionStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CXXTupleExpansionStmt; };
template<> struct GetTypeFromRK<RK_clang__CXXTupleExpansionStmt>
{ using type = clang::CXXTupleExpansionStmt; };
template<> struct GetReflectionObjKind<clang::CXXPackExpansionStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CXXPackExpansionStmt; };
template<> struct GetTypeFromRK<RK_clang__CXXPackExpansionStmt>
{ using type = clang::CXXPackExpansionStmt; };
template<> struct GetReflectionObjKind<clang::MSDependentExistsStmt>
{ static constexpr ReflectionObjKind value = RK_clang__MSDependentExistsStmt; };
template<> struct GetTypeFromRK<RK_clang__MSDependentExistsStmt>
{ using type = clang::MSDependentExistsStmt; };
template<> struct GetReflectionObjKind<clang::CoroutineBodyStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CoroutineBodyStmt; };
template<> struct GetTypeFromRK<RK_clang__CoroutineBodyStmt>
{ using type = clang::CoroutineBodyStmt; };
template<> struct GetReflectionObjKind<clang::CoroutineBodyStmt::CtorArgs>
{ static constexpr ReflectionObjKind value = RK_clang__CoroutineBodyStmt__CtorArgs; };
template<> struct GetTypeFromRK<RK_clang__CoroutineBodyStmt__CtorArgs>
{ using type = clang::CoroutineBodyStmt::CtorArgs; };
template<> struct GetReflectionObjKind<clang::CoreturnStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CoreturnStmt; };
template<> struct GetTypeFromRK<RK_clang__CoreturnStmt>
{ using type = clang::CoreturnStmt; };
template<> struct GetReflectionObjKind<clang::CXXQueueMetaparseStmt>
{ static constexpr ReflectionObjKind value = RK_clang__CXXQueueMetaparseStmt; };
template<> struct GetTypeFromRK<RK_clang__CXXQueueMetaparseStmt>
{ using type = clang::CXXQueueMetaparseStmt; };
template<> struct GetReflectionObjKind<clang::ObjCForCollectionStmt>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCForCollectionStmt; };
template<> struct GetTypeFromRK<RK_clang__ObjCForCollectionStmt>
{ using type = clang::ObjCForCollectionStmt; };
template<> struct GetReflectionObjKind<clang::ObjCAtCatchStmt>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCAtCatchStmt; };
template<> struct GetTypeFromRK<RK_clang__ObjCAtCatchStmt>
{ using type = clang::ObjCAtCatchStmt; };
template<> struct GetReflectionObjKind<clang::ObjCAtFinallyStmt>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCAtFinallyStmt; };
template<> struct GetTypeFromRK<RK_clang__ObjCAtFinallyStmt>
{ using type = clang::ObjCAtFinallyStmt; };
template<> struct GetReflectionObjKind<clang::ObjCAtTryStmt>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCAtTryStmt; };
template<> struct GetTypeFromRK<RK_clang__ObjCAtTryStmt>
{ using type = clang::ObjCAtTryStmt; };
template<> struct GetReflectionObjKind<clang::ObjCAtSynchronizedStmt>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCAtSynchronizedStmt; };
template<> struct GetTypeFromRK<RK_clang__ObjCAtSynchronizedStmt>
{ using type = clang::ObjCAtSynchronizedStmt; };
template<> struct GetReflectionObjKind<clang::ObjCAtThrowStmt>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCAtThrowStmt; };
template<> struct GetTypeFromRK<RK_clang__ObjCAtThrowStmt>
{ using type = clang::ObjCAtThrowStmt; };
template<> struct GetReflectionObjKind<clang::ObjCAutoreleasePoolStmt>
{ static constexpr ReflectionObjKind value = RK_clang__ObjCAutoreleasePoolStmt; };
template<> struct GetTypeFromRK<RK_clang__ObjCAutoreleasePoolStmt>
{ using type = clang::ObjCAutoreleasePoolStmt; };
template<> struct GetReflectionObjKind<clang::OMPClause>
{ static constexpr ReflectionObjKind value = RK_clang__OMPClause; };
template<> struct GetTypeFromRK<RK_clang__OMPClause>
{ using type = clang::OMPClause; };
template<> struct GetReflectionObjKind<clang::OMPExecutableDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPExecutableDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPExecutableDirective>
{ using type = clang::OMPExecutableDirective; };
template<> struct GetReflectionObjKind<clang::OMPParallelDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPParallelDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPParallelDirective>
{ using type = clang::OMPParallelDirective; };
template<> struct GetReflectionObjKind<clang::OMPLoopDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPLoopDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPLoopDirective>
{ using type = clang::OMPLoopDirective; };
template<> struct GetReflectionObjKind<clang::OMPSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPSimdDirective>
{ using type = clang::OMPSimdDirective; };
template<> struct GetReflectionObjKind<clang::OMPForDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPForDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPForDirective>
{ using type = clang::OMPForDirective; };
template<> struct GetReflectionObjKind<clang::OMPForSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPForSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPForSimdDirective>
{ using type = clang::OMPForSimdDirective; };
template<> struct GetReflectionObjKind<clang::OMPSectionsDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPSectionsDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPSectionsDirective>
{ using type = clang::OMPSectionsDirective; };
template<> struct GetReflectionObjKind<clang::OMPSectionDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPSectionDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPSectionDirective>
{ using type = clang::OMPSectionDirective; };
template<> struct GetReflectionObjKind<clang::OMPSingleDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPSingleDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPSingleDirective>
{ using type = clang::OMPSingleDirective; };
template<> struct GetReflectionObjKind<clang::OMPMasterDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPMasterDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPMasterDirective>
{ using type = clang::OMPMasterDirective; };
template<> struct GetReflectionObjKind<clang::OMPCriticalDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPCriticalDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPCriticalDirective>
{ using type = clang::OMPCriticalDirective; };
template<> struct GetReflectionObjKind<clang::OMPParallelForDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPParallelForDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPParallelForDirective>
{ using type = clang::OMPParallelForDirective; };
template<> struct GetReflectionObjKind<clang::OMPParallelForSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPParallelForSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPParallelForSimdDirective>
{ using type = clang::OMPParallelForSimdDirective; };
template<> struct GetReflectionObjKind<clang::OMPParallelSectionsDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPParallelSectionsDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPParallelSectionsDirective>
{ using type = clang::OMPParallelSectionsDirective; };
template<> struct GetReflectionObjKind<clang::OMPTaskDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTaskDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTaskDirective>
{ using type = clang::OMPTaskDirective; };
template<> struct GetReflectionObjKind<clang::OMPTaskyieldDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTaskyieldDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTaskyieldDirective>
{ using type = clang::OMPTaskyieldDirective; };
template<> struct GetReflectionObjKind<clang::OMPBarrierDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPBarrierDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPBarrierDirective>
{ using type = clang::OMPBarrierDirective; };
template<> struct GetReflectionObjKind<clang::OMPTaskwaitDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTaskwaitDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTaskwaitDirective>
{ using type = clang::OMPTaskwaitDirective; };
template<> struct GetReflectionObjKind<clang::OMPTaskgroupDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTaskgroupDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTaskgroupDirective>
{ using type = clang::OMPTaskgroupDirective; };
template<> struct GetReflectionObjKind<clang::OMPFlushDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPFlushDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPFlushDirective>
{ using type = clang::OMPFlushDirective; };
template<> struct GetReflectionObjKind<clang::OMPOrderedDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPOrderedDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPOrderedDirective>
{ using type = clang::OMPOrderedDirective; };
template<> struct GetReflectionObjKind<clang::OMPAtomicDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPAtomicDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPAtomicDirective>
{ using type = clang::OMPAtomicDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetDirective>
{ using type = clang::OMPTargetDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetDataDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetDataDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetDataDirective>
{ using type = clang::OMPTargetDataDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetEnterDataDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetEnterDataDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetEnterDataDirective>
{ using type = clang::OMPTargetEnterDataDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetExitDataDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetExitDataDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetExitDataDirective>
{ using type = clang::OMPTargetExitDataDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetParallelDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetParallelDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetParallelDirective>
{ using type = clang::OMPTargetParallelDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetParallelForDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetParallelForDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetParallelForDirective>
{ using type = clang::OMPTargetParallelForDirective; };
template<> struct GetReflectionObjKind<clang::OMPTeamsDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTeamsDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTeamsDirective>
{ using type = clang::OMPTeamsDirective; };
template<> struct GetReflectionObjKind<clang::OMPCancellationPointDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPCancellationPointDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPCancellationPointDirective>
{ using type = clang::OMPCancellationPointDirective; };
template<> struct GetReflectionObjKind<clang::OMPCancelDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPCancelDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPCancelDirective>
{ using type = clang::OMPCancelDirective; };
template<> struct GetReflectionObjKind<clang::OMPTaskLoopDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTaskLoopDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTaskLoopDirective>
{ using type = clang::OMPTaskLoopDirective; };
template<> struct GetReflectionObjKind<clang::OMPTaskLoopSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTaskLoopSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTaskLoopSimdDirective>
{ using type = clang::OMPTaskLoopSimdDirective; };
template<> struct GetReflectionObjKind<clang::OMPDistributeDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPDistributeDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPDistributeDirective>
{ using type = clang::OMPDistributeDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetUpdateDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetUpdateDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetUpdateDirective>
{ using type = clang::OMPTargetUpdateDirective; };
template<> struct GetReflectionObjKind<clang::OMPDistributeParallelForDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPDistributeParallelForDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPDistributeParallelForDirective>
{ using type = clang::OMPDistributeParallelForDirective; };
template<> struct GetReflectionObjKind<clang::OMPDistributeParallelForSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPDistributeParallelForSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPDistributeParallelForSimdDirective>
{ using type = clang::OMPDistributeParallelForSimdDirective; };
template<> struct GetReflectionObjKind<clang::OMPDistributeSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPDistributeSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPDistributeSimdDirective>
{ using type = clang::OMPDistributeSimdDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetParallelForSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetParallelForSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetParallelForSimdDirective>
{ using type = clang::OMPTargetParallelForSimdDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetSimdDirective>
{ using type = clang::OMPTargetSimdDirective; };
template<> struct GetReflectionObjKind<clang::OMPTeamsDistributeDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTeamsDistributeDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTeamsDistributeDirective>
{ using type = clang::OMPTeamsDistributeDirective; };
template<> struct GetReflectionObjKind<clang::OMPTeamsDistributeSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTeamsDistributeSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTeamsDistributeSimdDirective>
{ using type = clang::OMPTeamsDistributeSimdDirective; };
template<> struct GetReflectionObjKind<clang::OMPTeamsDistributeParallelForSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTeamsDistributeParallelForSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTeamsDistributeParallelForSimdDirective>
{ using type = clang::OMPTeamsDistributeParallelForSimdDirective; };
template<> struct GetReflectionObjKind<clang::OMPTeamsDistributeParallelForDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTeamsDistributeParallelForDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTeamsDistributeParallelForDirective>
{ using type = clang::OMPTeamsDistributeParallelForDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetTeamsDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetTeamsDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetTeamsDirective>
{ using type = clang::OMPTargetTeamsDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetTeamsDistributeDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetTeamsDistributeDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetTeamsDistributeDirective>
{ using type = clang::OMPTargetTeamsDistributeDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetTeamsDistributeParallelForDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetTeamsDistributeParallelForDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetTeamsDistributeParallelForDirective>
{ using type = clang::OMPTargetTeamsDistributeParallelForDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetTeamsDistributeParallelForSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetTeamsDistributeParallelForSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetTeamsDistributeParallelForSimdDirective>
{ using type = clang::OMPTargetTeamsDistributeParallelForSimdDirective; };
template<> struct GetReflectionObjKind<clang::OMPTargetTeamsDistributeSimdDirective>
{ static constexpr ReflectionObjKind value = RK_clang__OMPTargetTeamsDistributeSimdDirective; };
template<> struct GetTypeFromRK<RK_clang__OMPTargetTeamsDistributeSimdDirective>
{ using type = clang::OMPTargetTeamsDistributeSimdDirective; };
template<> struct GetReflectionObjKind<clang::StoredDeclsMap>
{ static constexpr ReflectionObjKind value = RK_clang__StoredDeclsMap; };
template<> struct GetTypeFromRK<RK_clang__StoredDeclsMap>
{ using type = clang::StoredDeclsMap; };
template<> struct GetReflectionObjKind<clang::DependentDiagnostic>
{ static constexpr ReflectionObjKind value = RK_clang__DependentDiagnostic; };
template<> struct GetTypeFromRK<RK_clang__DependentDiagnostic>
{ using type = clang::DependentDiagnostic; };
template<> struct GetReflectionObjKind<reflcontainers::VectorStr>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__VectorStr; };
template<> struct GetTypeFromRK<RK_reflcontainers__VectorStr>
{ using type = reflcontainers::VectorStr; };
template<> struct GetReflectionObjKind<reflcontainers::VectorInt>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__VectorInt; };
template<> struct GetTypeFromRK<RK_reflcontainers__VectorInt>
{ using type = reflcontainers::VectorInt; };
template<> struct GetReflectionObjKind<reflcontainers::SetInt>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__SetInt; };
template<> struct GetTypeFromRK<RK_reflcontainers__SetInt>
{ using type = reflcontainers::SetInt; };
template<> struct GetReflectionObjKind<reflcontainers::SetStr>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__SetStr; };
template<> struct GetTypeFromRK<RK_reflcontainers__SetStr>
{ using type = reflcontainers::SetStr; };
template<> struct GetReflectionObjKind<reflcontainers::IntIntPair>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__IntIntPair; };
template<> struct GetTypeFromRK<RK_reflcontainers__IntIntPair>
{ using type = reflcontainers::IntIntPair; };
template<> struct GetReflectionObjKind<reflcontainers::IntStrPair>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__IntStrPair; };
template<> struct GetTypeFromRK<RK_reflcontainers__IntStrPair>
{ using type = reflcontainers::IntStrPair; };
template<> struct GetReflectionObjKind<reflcontainers::StrIntPair>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__StrIntPair; };
template<> struct GetTypeFromRK<RK_reflcontainers__StrIntPair>
{ using type = reflcontainers::StrIntPair; };
template<> struct GetReflectionObjKind<reflcontainers::StrStrPair>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__StrStrPair; };
template<> struct GetTypeFromRK<RK_reflcontainers__StrStrPair>
{ using type = reflcontainers::StrStrPair; };
template<> struct GetReflectionObjKind<reflcontainers::MapIntStr>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__MapIntStr; };
template<> struct GetTypeFromRK<RK_reflcontainers__MapIntStr>
{ using type = reflcontainers::MapIntStr; };
template<> struct GetReflectionObjKind<reflcontainers::MapStrInt>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__MapStrInt; };
template<> struct GetTypeFromRK<RK_reflcontainers__MapStrInt>
{ using type = reflcontainers::MapStrInt; };
template<> struct GetReflectionObjKind<reflcontainers::MapStrStr>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__MapStrStr; };
template<> struct GetTypeFromRK<RK_reflcontainers__MapStrStr>
{ using type = reflcontainers::MapStrStr; };
template<> struct GetReflectionObjKind<reflcontainers::MapIntInt>
{ static constexpr ReflectionObjKind value = RK_reflcontainers__MapIntInt; };
template<> struct GetTypeFromRK<RK_reflcontainers__MapIntInt>
{ using type = reflcontainers::MapIntInt; };
