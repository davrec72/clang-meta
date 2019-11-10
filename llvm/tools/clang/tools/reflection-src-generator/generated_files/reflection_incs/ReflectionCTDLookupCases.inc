    case RK_llvm__APInt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "APInt"), "impl");
    case RK_llvm__APInt__ms:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "APInt"), "ms"), "impl");
    case RK_llvm__APInt__mu:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "APInt"), "mu"), "impl");
    case RK_llvm__APFloatBase:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "APFloatBase"), "impl");
    case RK_llvm__APFloat:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "APFloat"), "impl");
    case RK_llvm__APSInt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "APSInt"), "impl");
    case RK_clang__APValue:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "APValue"), "impl");
    case RK_clang__APValue__LValueBase:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "APValue"), "LValueBase"), "impl");
    case RK_clang__DiagnosticOptions:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DiagnosticOptions"), "impl");
    case RK_clang__FileID:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FileID"), "impl");
    case RK_clang__SourceLocation:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SourceLocation"), "impl");
    case RK_clang__SourceRange:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SourceRange"), "impl");
    case RK_clang__CharSourceRange:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CharSourceRange"), "impl");
    case RK_clang__PresumedLoc:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PresumedLoc"), "impl");
    case RK_clang__FullSourceLoc:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FullSourceLoc"), "impl");
    case RK_llvm__DebugEpochBase:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "DebugEpochBase"), "impl");
    case RK_clang__FixItHint:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FixItHint"), "impl");
    case RK_clang__DiagnosticsEngine:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DiagnosticsEngine"), "impl");
    case RK_clang__DiagnosticBuilder:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DiagnosticBuilder"), "impl");
    case RK_clang__DiagnosticConsumer:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DiagnosticConsumer"), "impl");
    case RK_clang__IdentifierInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "IdentifierInfo"), "impl");
    case RK_clang__IdentifierInfoLookup:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "IdentifierInfoLookup"), "impl");
    case RK_clang__IdentifierTable:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "IdentifierTable"), "impl");
    case RK_clang__Selector:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Selector"), "impl");
    case RK_clang__SelectorTable:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SelectorTable"), "impl");
    case RK_clang__PartialDiagnostic:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PartialDiagnostic"), "impl");
    case RK_clang__DeclarationName:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeclarationName"), "impl");
    case RK_clang__DeclarationNameLoc:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeclarationNameLoc"), "impl");
    case RK_clang__DeclarationNameInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeclarationNameInfo"), "impl");
    case RK_llvm__raw_ostream:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "raw_ostream"), "impl");
    case RK_llvm__VersionTuple:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "VersionTuple"), "impl");
    case RK_clang__Decl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Decl"), "impl");
    case RK_clang__DeclContextLookupResult:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeclContextLookupResult"), "impl");
    case RK_clang__DeclContext:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeclContext"), "impl");
    case RK_clang__CharUnits:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CharUnits"), "impl");
    case RK_clang__FileSystemOptions:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FileSystemOptions"), "impl");
    case RK_llvm__Twine:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "Twine"), "impl");
    case RK_llvm__sys__fs__UniqueID:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "sys"), "fs"), "UniqueID"), "impl");
    case RK_llvm__MemoryBuffer:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "MemoryBuffer"), "impl");
    case RK_llvm__MemoryBufferRef:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "MemoryBufferRef"), "impl");
    case RK_clang__DirectoryEntry:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DirectoryEntry"), "impl");
    case RK_clang__FileEntry:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FileEntry"), "impl");
    case RK_clang__FileManager:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FileManager"), "impl");
    case RK_clang__ASTFileSignature:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ASTFileSignature"), "impl");
    case RK_clang__Module:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Module"), "impl");
    case RK_clang__Module__Header:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Module"), "Header"), "impl");
    case RK_clang__Module__DirectoryName:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Module"), "DirectoryName"), "impl");
    case RK_clang__ExternalASTSource:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ExternalASTSource"), "impl");
    case RK_clang__ExternalASTSource__MemoryBufferSizes:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ExternalASTSource"), "MemoryBufferSizes"), "impl");
    case RK_llvm__FoldingSetBase:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "FoldingSetBase"), "impl");
    case RK_llvm__FoldingSetBase__Node:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "FoldingSetBase"), "Node"), "impl");
    case RK_llvm__FoldingSetNodeIDRef:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "FoldingSetNodeIDRef"), "impl");
    case RK_llvm__FoldingSetNodeID:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "FoldingSetNodeID"), "impl");
    case RK_clang__NestedNameSpecifier:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "NestedNameSpecifier"), "impl");
    case RK_clang__NestedNameSpecifierLoc:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "NestedNameSpecifierLoc"), "impl");
    case RK_clang__UncommonTemplateNameStorage:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UncommonTemplateNameStorage"), "impl");
    case RK_clang__OverloadedTemplateStorage:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OverloadedTemplateStorage"), "impl");
    case RK_clang__SubstTemplateTemplateParmPackStorage:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SubstTemplateTemplateParmPackStorage"), "impl");
    case RK_clang__TemplateName:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateName"), "impl");
    case RK_clang__SubstTemplateTemplateParmStorage:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SubstTemplateTemplateParmStorage"), "impl");
    case RK_clang__QualifiedTemplateName:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "QualifiedTemplateName"), "impl");
    case RK_clang__DependentTemplateName:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DependentTemplateName"), "impl");
    case RK_clang__LinkageInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "LinkageInfo"), "impl");
    case RK_clang__Qualifiers:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Qualifiers"), "impl");
    case RK_clang__SplitQualType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SplitQualType"), "impl");
    case RK_clang__QualType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "QualType"), "impl");
    case RK_clang__ExtQualsTypeCommonBase:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ExtQualsTypeCommonBase"), "impl");
    case RK_clang__Type:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Type"), "impl");
    case RK_clang__BuiltinType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "BuiltinType"), "impl");
    case RK_clang__ComplexType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ComplexType"), "impl");
    case RK_clang__ParenType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ParenType"), "impl");
    case RK_clang__PointerType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PointerType"), "impl");
    case RK_clang__AdjustedType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "AdjustedType"), "impl");
    case RK_clang__DecayedType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DecayedType"), "impl");
    case RK_clang__BlockPointerType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "BlockPointerType"), "impl");
    case RK_clang__ReferenceType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ReferenceType"), "impl");
    case RK_clang__LValueReferenceType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "LValueReferenceType"), "impl");
    case RK_clang__RValueReferenceType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "RValueReferenceType"), "impl");
    case RK_clang__MemberPointerType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "MemberPointerType"), "impl");
    case RK_clang__ArrayType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ArrayType"), "impl");
    case RK_clang__ConstantArrayType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ConstantArrayType"), "impl");
    case RK_clang__IncompleteArrayType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "IncompleteArrayType"), "impl");
    case RK_clang__VariableArrayType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "VariableArrayType"), "impl");
    case RK_clang__DependentSizedArrayType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DependentSizedArrayType"), "impl");
    case RK_clang__DependentAddressSpaceType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DependentAddressSpaceType"), "impl");
    case RK_clang__DependentSizedExtVectorType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DependentSizedExtVectorType"), "impl");
    case RK_clang__VectorType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "VectorType"), "impl");
    case RK_clang__DependentVectorType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DependentVectorType"), "impl");
    case RK_clang__ExtVectorType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ExtVectorType"), "impl");
    case RK_clang__FunctionType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FunctionType"), "impl");
    case RK_clang__FunctionType__ExtInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FunctionType"), "ExtInfo"), "impl");
    case RK_clang__FunctionNoProtoType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FunctionNoProtoType"), "impl");
    case RK_clang__FunctionProtoType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FunctionProtoType"), "impl");
    case RK_clang__FunctionProtoType__ExtParameterInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FunctionProtoType"), "ExtParameterInfo"), "impl");
    case RK_clang__FunctionProtoType__ExceptionSpecInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FunctionProtoType"), "ExceptionSpecInfo"), "impl");
    case RK_clang__FunctionProtoType__ExtProtoInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FunctionProtoType"), "ExtProtoInfo"), "impl");
    case RK_clang__UnresolvedUsingType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UnresolvedUsingType"), "impl");
    case RK_clang__TypedefType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypedefType"), "impl");
    case RK_clang__TypeOfExprType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypeOfExprType"), "impl");
    case RK_clang__TypeOfType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypeOfType"), "impl");
    case RK_clang__DecltypeType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DecltypeType"), "impl");
    case RK_clang__ReflectedType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ReflectedType"), "impl");
    case RK_clang__UnaryTransformType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UnaryTransformType"), "impl");
    case RK_clang__TagType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TagType"), "impl");
    case RK_clang__RecordType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "RecordType"), "impl");
    case RK_clang__EnumType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "EnumType"), "impl");
    case RK_clang__AttributedType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "AttributedType"), "impl");
    case RK_clang__TemplateTypeParmType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateTypeParmType"), "impl");
    case RK_clang__SubstTemplateTypeParmType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SubstTemplateTypeParmType"), "impl");
    case RK_clang__SubstTemplateTypeParmPackType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SubstTemplateTypeParmPackType"), "impl");
    case RK_clang__DeducedType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeducedType"), "impl");
    case RK_clang__AutoType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "AutoType"), "impl");
    case RK_clang__DeducedTemplateSpecializationType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeducedTemplateSpecializationType"), "impl");
    case RK_clang__TemplateSpecializationType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateSpecializationType"), "impl");
    case RK_clang__InjectedClassNameType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "InjectedClassNameType"), "impl");
    case RK_clang__TypeWithKeyword:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypeWithKeyword"), "impl");
    case RK_clang__ElaboratedType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ElaboratedType"), "impl");
    case RK_clang__DependentNameType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DependentNameType"), "impl");
    case RK_clang__DependentTemplateSpecializationType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DependentTemplateSpecializationType"), "impl");
    case RK_clang__PackExpansionType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PackExpansionType"), "impl");
    case RK_clang__ObjCTypeParamType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCTypeParamType"), "impl");
    case RK_clang__ObjCObjectType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCObjectType"), "impl");
    case RK_clang__ObjCInterfaceType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCInterfaceType"), "impl");
    case RK_clang__ObjCObjectPointerType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCObjectPointerType"), "impl");
    case RK_clang__AtomicType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "AtomicType"), "impl");
    case RK_clang__PipeType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PipeType"), "impl");
    case RK_clang__TypeSourceInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypeSourceInfo"), "impl");
    case RK_clang__TranslationUnitDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TranslationUnitDecl"), "impl");
    case RK_clang__PragmaCommentDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PragmaCommentDecl"), "impl");
    case RK_clang__PragmaDetectMismatchDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PragmaDetectMismatchDecl"), "impl");
    case RK_clang__ExternCContextDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ExternCContextDecl"), "impl");
    case RK_clang__NamedDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "NamedDecl"), "impl");
    case RK_clang__LabelDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "LabelDecl"), "impl");
    case RK_clang__NamespaceDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "NamespaceDecl"), "impl");
    case RK_clang__ValueDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ValueDecl"), "impl");
    case RK_clang__DeclaratorDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeclaratorDecl"), "impl");
    case RK_clang__EvaluatedStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "EvaluatedStmt"), "impl");
    case RK_clang__VarDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "VarDecl"), "impl");
    case RK_clang__ImplicitParamDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ImplicitParamDecl"), "impl");
    case RK_clang__ParmVarDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ParmVarDecl"), "impl");
    case RK_clang__FunctionDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FunctionDecl"), "impl");
    case RK_clang__FieldDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FieldDecl"), "impl");
    case RK_clang__EnumConstantDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "EnumConstantDecl"), "impl");
    case RK_clang__IndirectFieldDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "IndirectFieldDecl"), "impl");
    case RK_clang__TypeDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypeDecl"), "impl");
    case RK_clang__TypedefNameDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypedefNameDecl"), "impl");
    case RK_clang__TypedefDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypedefDecl"), "impl");
    case RK_clang__TypeAliasDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypeAliasDecl"), "impl");
    case RK_clang__TagDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TagDecl"), "impl");
    case RK_clang__EnumDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "EnumDecl"), "impl");
    case RK_clang__RecordDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "RecordDecl"), "impl");
    case RK_clang__FileScopeAsmDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FileScopeAsmDecl"), "impl");
    case RK_clang__BlockDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "BlockDecl"), "impl");
    case RK_clang__BlockDecl__Capture:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "BlockDecl"), "Capture"), "impl");
    case RK_clang__CapturedDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CapturedDecl"), "impl");
    case RK_clang__ImportDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ImportDecl"), "impl");
    case RK_clang__ExportDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ExportDecl"), "impl");
    case RK_clang__EmptyDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "EmptyDecl"), "impl");
    case RK_clang__DeclGroup:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeclGroup"), "impl");
    case RK_clang__DeclGroupRef:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeclGroupRef"), "impl");
    case RK_clang__Stmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Stmt"), "impl");
    case RK_clang__Stmt__EmptyShell:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Stmt"), "EmptyShell"), "impl");
    case RK_clang__DeclStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeclStmt"), "impl");
    case RK_clang__NullStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "NullStmt"), "impl");
    case RK_clang__CompoundStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CompoundStmt"), "impl");
    case RK_clang__SwitchCase:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SwitchCase"), "impl");
    case RK_clang__CaseStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CaseStmt"), "impl");
    case RK_clang__DefaultStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DefaultStmt"), "impl");
    case RK_clang__LabelStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "LabelStmt"), "impl");
    case RK_clang__AttributedStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "AttributedStmt"), "impl");
    case RK_clang__IfStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "IfStmt"), "impl");
    case RK_clang__SwitchStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SwitchStmt"), "impl");
    case RK_clang__WhileStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "WhileStmt"), "impl");
    case RK_clang__DoStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DoStmt"), "impl");
    case RK_clang__ForStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ForStmt"), "impl");
    case RK_clang__GotoStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "GotoStmt"), "impl");
    case RK_clang__IndirectGotoStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "IndirectGotoStmt"), "impl");
    case RK_clang__ContinueStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ContinueStmt"), "impl");
    case RK_clang__BreakStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "BreakStmt"), "impl");
    case RK_clang__ReturnStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ReturnStmt"), "impl");
    case RK_clang__AsmStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "AsmStmt"), "impl");
    case RK_clang__GCCAsmStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "GCCAsmStmt"), "impl");
    case RK_clang__MSAsmStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "MSAsmStmt"), "impl");
    case RK_clang__SEHExceptStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SEHExceptStmt"), "impl");
    case RK_clang__SEHFinallyStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SEHFinallyStmt"), "impl");
    case RK_clang__SEHTryStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SEHTryStmt"), "impl");
    case RK_clang__SEHLeaveStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SEHLeaveStmt"), "impl");
    case RK_clang__CapturedStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CapturedStmt"), "impl");
    case RK_clang__CapturedStmt__Capture:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CapturedStmt"), "Capture"), "impl");
    case RK_clang__TemplateArgument:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateArgument"), "impl");
    case RK_clang__TemplateArgumentLocInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateArgumentLocInfo"), "impl");
    case RK_clang__TemplateArgumentLoc:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateArgumentLoc"), "impl");
    case RK_clang__TemplateArgumentListInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateArgumentListInfo"), "impl");
    case RK_clang__ASTTemplateArgumentListInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ASTTemplateArgumentListInfo"), "impl");
    case RK_clang__TypeLoc:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypeLoc"), "impl");
    case RK_clang__UnqualTypeLoc:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UnqualTypeLoc"), "impl");
    case RK_clang__CommentOptions:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CommentOptions"), "impl");
    case RK_clang__comments__CommandInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "comments"), "CommandInfo"), "impl");
    case RK_clang__comments__CommandTraits:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "comments"), "CommandTraits"), "impl");
    case RK_llvm__Triple:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "Triple"), "impl");
    case RK_clang__ObjCRuntime:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCRuntime"), "impl");
    case RK_clang__SanitizerSet:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SanitizerSet"), "impl");
    case RK_clang__LangOptionsBase:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "LangOptionsBase"), "impl");
    case RK_clang__LangOptions:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "LangOptions"), "impl");
    case RK_clang__FPOptions:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FPOptions"), "impl");
    case RK_clang__PrinterHelper:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PrinterHelper"), "impl");
    case RK_clang__PrintingPolicy:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PrintingPolicy"), "impl");
    case RK_clang__SrcMgr__ContentCache:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SrcMgr"), "ContentCache"), "impl");
    case RK_clang__SrcMgr__FileInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SrcMgr"), "FileInfo"), "impl");
    case RK_clang__SrcMgr__ExpansionInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SrcMgr"), "ExpansionInfo"), "impl");
    case RK_clang__SrcMgr__SLocEntry:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SrcMgr"), "SLocEntry"), "impl");
    case RK_clang__SourceManager:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SourceManager"), "impl");
    case RK_clang__RawComment:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "RawComment"), "impl");
    case RK_clang__SanitizerBlacklist:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SanitizerBlacklist"), "impl");
    case RK_clang__TargetCXXABI:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TargetCXXABI"), "impl");
    case RK_clang__OpenCLOptions:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OpenCLOptions"), "impl");
    case RK_clang__TargetOptions:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TargetOptions"), "impl");
    case RK_llvm__Type:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "Type"), "impl");
    case RK_llvm__IntegerType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "IntegerType"), "impl");
    case RK_llvm__CompositeType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "CompositeType"), "impl");
    case RK_llvm__StructType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "StructType"), "impl");
    case RK_llvm__PointerType:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "PointerType"), "impl");
    case RK_llvm__DataLayout:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "DataLayout"), "impl");
    case RK_llvm__StructLayout:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "llvm"), "StructLayout"), "impl");
    case RK_clang__TargetInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TargetInfo"), "impl");
    case RK_clang__TargetInfo__ConstraintInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TargetInfo"), "ConstraintInfo"), "impl");
    case RK_clang__XRayFunctionFilter:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "XRayFunctionFilter"), "impl");
    case RK_clang__TypeInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypeInfo"), "impl");
    case RK_clang__ASTContext:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ASTContext"), "impl");
    case RK_clang__DeclAccessPair:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeclAccessPair"), "impl");
    case RK_clang__UnresolvedSetIterator:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UnresolvedSetIterator"), "impl");
    case RK_clang__UnresolvedSetImpl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UnresolvedSetImpl"), "impl");
    case RK_clang__Expr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Expr"), "impl");
    case RK_clang__Expr__Classification:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Expr"), "Classification"), "impl");
    case RK_clang__Expr__EvalStatus:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Expr"), "EvalStatus"), "impl");
    case RK_clang__Expr__EvalResult:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Expr"), "EvalResult"), "impl");
    case RK_clang__OpaqueValueExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OpaqueValueExpr"), "impl");
    case RK_clang__DeclRefExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DeclRefExpr"), "impl");
    case RK_clang__PredefinedExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PredefinedExpr"), "impl");
    case RK_clang__APIntStorage:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "APIntStorage"), "impl");
    case RK_clang__IntegerLiteral:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "IntegerLiteral"), "impl");
    case RK_clang__FixedPointLiteral:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FixedPointLiteral"), "impl");
    case RK_clang__CharacterLiteral:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CharacterLiteral"), "impl");
    case RK_clang__FloatingLiteral:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FloatingLiteral"), "impl");
    case RK_clang__ImaginaryLiteral:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ImaginaryLiteral"), "impl");
    case RK_clang__StringLiteral:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "StringLiteral"), "impl");
    case RK_clang__ParenExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ParenExpr"), "impl");
    case RK_clang__UnaryOperator:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UnaryOperator"), "impl");
    case RK_clang__OffsetOfNode:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OffsetOfNode"), "impl");
    case RK_clang__OffsetOfExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OffsetOfExpr"), "impl");
    case RK_clang__UnaryExprOrTypeTraitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UnaryExprOrTypeTraitExpr"), "impl");
    case RK_clang__ArraySubscriptExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ArraySubscriptExpr"), "impl");
    case RK_clang__CallExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CallExpr"), "impl");
    case RK_clang__MemberExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "MemberExpr"), "impl");
    case RK_clang__CompoundLiteralExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CompoundLiteralExpr"), "impl");
    case RK_clang__CastExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CastExpr"), "impl");
    case RK_clang__ImplicitCastExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ImplicitCastExpr"), "impl");
    case RK_clang__ExplicitCastExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ExplicitCastExpr"), "impl");
    case RK_clang__CStyleCastExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CStyleCastExpr"), "impl");
    case RK_clang__BinaryOperator:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "BinaryOperator"), "impl");
    case RK_clang__CompoundAssignOperator:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CompoundAssignOperator"), "impl");
    case RK_clang__AbstractConditionalOperator:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "AbstractConditionalOperator"), "impl");
    case RK_clang__ConditionalOperator:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ConditionalOperator"), "impl");
    case RK_clang__BinaryConditionalOperator:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "BinaryConditionalOperator"), "impl");
    case RK_clang__AddrLabelExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "AddrLabelExpr"), "impl");
    case RK_clang__StmtExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "StmtExpr"), "impl");
    case RK_clang__ShuffleVectorExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ShuffleVectorExpr"), "impl");
    case RK_clang__ConvertVectorExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ConvertVectorExpr"), "impl");
    case RK_clang__ChooseExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ChooseExpr"), "impl");
    case RK_clang__GNUNullExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "GNUNullExpr"), "impl");
    case RK_clang__VAArgExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "VAArgExpr"), "impl");
    case RK_clang__InitListExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "InitListExpr"), "impl");
    case RK_clang__DesignatedInitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DesignatedInitExpr"), "impl");
    case RK_clang__DesignatedInitExpr__Designator:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DesignatedInitExpr"), "Designator"), "impl");
    case RK_clang__NoInitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "NoInitExpr"), "impl");
    case RK_clang__DesignatedInitUpdateExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DesignatedInitUpdateExpr"), "impl");
    case RK_clang__ArrayInitLoopExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ArrayInitLoopExpr"), "impl");
    case RK_clang__ArrayInitIndexExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ArrayInitIndexExpr"), "impl");
    case RK_clang__ImplicitValueInitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ImplicitValueInitExpr"), "impl");
    case RK_clang__ParenListExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ParenListExpr"), "impl");
    case RK_clang__GenericSelectionExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "GenericSelectionExpr"), "impl");
    case RK_clang__ExtVectorElementExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ExtVectorElementExpr"), "impl");
    case RK_clang__BlockExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "BlockExpr"), "impl");
    case RK_clang__AsTypeExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "AsTypeExpr"), "impl");
    case RK_clang__PseudoObjectExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PseudoObjectExpr"), "impl");
    case RK_clang__AtomicExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "AtomicExpr"), "impl");
    case RK_clang__TypoExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypoExpr"), "impl");
    case RK_clang__Attr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "Attr"), "impl");
    case RK_clang__InheritableAttr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "InheritableAttr"), "impl");
    case RK_clang__ExternalSourceSymbolAttr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ExternalSourceSymbolAttr"), "impl");
    case RK_clang__MSInheritanceAttr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "MSInheritanceAttr"), "impl");
    case RK_clang__MSVtorDispAttr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "MSVtorDispAttr"), "impl");
    case RK_clang__LambdaCapture:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "LambdaCapture"), "impl");
    case RK_clang__AccessSpecDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "AccessSpecDecl"), "impl");
    case RK_clang__CXXBaseSpecifier:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXBaseSpecifier"), "impl");
    case RK_clang__CXXRecordDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXRecordDecl"), "impl");
    case RK_clang__CXXDeductionGuideDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXDeductionGuideDecl"), "impl");
    case RK_clang__CXXMethodDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXMethodDecl"), "impl");
    case RK_clang__CXXCtorInitializer:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXCtorInitializer"), "impl");
    case RK_clang__InheritedConstructor:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "InheritedConstructor"), "impl");
    case RK_clang__CXXConstructorDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXConstructorDecl"), "impl");
    case RK_clang__CXXDestructorDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXDestructorDecl"), "impl");
    case RK_clang__CXXConversionDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXConversionDecl"), "impl");
    case RK_clang__LinkageSpecDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "LinkageSpecDecl"), "impl");
    case RK_clang__UsingDirectiveDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UsingDirectiveDecl"), "impl");
    case RK_clang__NamespaceAliasDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "NamespaceAliasDecl"), "impl");
    case RK_clang__UsingShadowDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UsingShadowDecl"), "impl");
    case RK_clang__ConstructorUsingShadowDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ConstructorUsingShadowDecl"), "impl");
    case RK_clang__UsingDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UsingDecl"), "impl");
    case RK_clang__UsingPackDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UsingPackDecl"), "impl");
    case RK_clang__UnresolvedUsingValueDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UnresolvedUsingValueDecl"), "impl");
    case RK_clang__UnresolvedUsingTypenameDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UnresolvedUsingTypenameDecl"), "impl");
    case RK_clang__StaticAssertDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "StaticAssertDecl"), "impl");
    case RK_clang__BindingDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "BindingDecl"), "impl");
    case RK_clang__DecompositionDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DecompositionDecl"), "impl");
    case RK_clang__MSPropertyDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "MSPropertyDecl"), "impl");
    case RK_clang__ConstexprDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ConstexprDecl"), "impl");
    case RK_clang__TemplateParameterList:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateParameterList"), "impl");
    case RK_clang__TemplateArgumentList:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateArgumentList"), "impl");
    case RK_clang__TemplateDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateDecl"), "impl");
    case RK_clang__FunctionTemplateSpecializationInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FunctionTemplateSpecializationInfo"), "impl");
    case RK_clang__MemberSpecializationInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "MemberSpecializationInfo"), "impl");
    case RK_clang__DependentFunctionTemplateSpecializationInfo:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DependentFunctionTemplateSpecializationInfo"), "impl");
    case RK_clang__RedeclarableTemplateDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "RedeclarableTemplateDecl"), "impl");
    case RK_clang__FunctionTemplateDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FunctionTemplateDecl"), "impl");
    case RK_clang__TemplateTypeParmDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateTypeParmDecl"), "impl");
    case RK_clang__NonTypeTemplateParmDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "NonTypeTemplateParmDecl"), "impl");
    case RK_clang__TemplateTemplateParmDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TemplateTemplateParmDecl"), "impl");
    case RK_clang__BuiltinTemplateDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "BuiltinTemplateDecl"), "impl");
    case RK_clang__ClassTemplateSpecializationDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ClassTemplateSpecializationDecl"), "impl");
    case RK_clang__ClassTemplatePartialSpecializationDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ClassTemplatePartialSpecializationDecl"), "impl");
    case RK_clang__ClassTemplateDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ClassTemplateDecl"), "impl");
    case RK_clang__FriendTemplateDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FriendTemplateDecl"), "impl");
    case RK_clang__TypeAliasTemplateDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypeAliasTemplateDecl"), "impl");
    case RK_clang__ClassScopeFunctionSpecializationDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ClassScopeFunctionSpecializationDecl"), "impl");
    case RK_clang__VarTemplateSpecializationDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "VarTemplateSpecializationDecl"), "impl");
    case RK_clang__VarTemplatePartialSpecializationDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "VarTemplatePartialSpecializationDecl"), "impl");
    case RK_clang__VarTemplateDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "VarTemplateDecl"), "impl");
    case RK_clang__FriendDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FriendDecl"), "impl");
    case RK_clang__ObjCListBase:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCListBase"), "impl");
    case RK_clang__ObjCProtocolList:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCProtocolList"), "impl");
    case RK_clang__ObjCMethodDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCMethodDecl"), "impl");
    case RK_clang__ObjCTypeParamDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCTypeParamDecl"), "impl");
    case RK_clang__ObjCTypeParamList:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCTypeParamList"), "impl");
    case RK_clang__ObjCPropertyDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCPropertyDecl"), "impl");
    case RK_clang__ObjCContainerDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCContainerDecl"), "impl");
    case RK_clang__ObjCInterfaceDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCInterfaceDecl"), "impl");
    case RK_clang__ObjCIvarDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCIvarDecl"), "impl");
    case RK_clang__ObjCAtDefsFieldDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCAtDefsFieldDecl"), "impl");
    case RK_clang__ObjCProtocolDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCProtocolDecl"), "impl");
    case RK_clang__ObjCCategoryDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCCategoryDecl"), "impl");
    case RK_clang__ObjCImplDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCImplDecl"), "impl");
    case RK_clang__ObjCCategoryImplDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCCategoryImplDecl"), "impl");
    case RK_clang__ObjCImplementationDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCImplementationDecl"), "impl");
    case RK_clang__ObjCCompatibleAliasDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCCompatibleAliasDecl"), "impl");
    case RK_clang__ObjCPropertyImplDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCPropertyImplDecl"), "impl");
    case RK_clang__OMPThreadPrivateDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPThreadPrivateDecl"), "impl");
    case RK_clang__OMPDeclareReductionDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPDeclareReductionDecl"), "impl");
    case RK_clang__OMPCapturedExprDecl:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPCapturedExprDecl"), "impl");
    case RK_clang__CXXOperatorCallExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXOperatorCallExpr"), "impl");
    case RK_clang__CXXMemberCallExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXMemberCallExpr"), "impl");
    case RK_clang__CUDAKernelCallExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CUDAKernelCallExpr"), "impl");
    case RK_clang__CXXNamedCastExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXNamedCastExpr"), "impl");
    case RK_clang__CXXStaticCastExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXStaticCastExpr"), "impl");
    case RK_clang__CXXDynamicCastExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXDynamicCastExpr"), "impl");
    case RK_clang__CXXReinterpretCastExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXReinterpretCastExpr"), "impl");
    case RK_clang__CXXConstCastExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXConstCastExpr"), "impl");
    case RK_clang__UserDefinedLiteral:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UserDefinedLiteral"), "impl");
    case RK_clang__CXXBoolLiteralExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXBoolLiteralExpr"), "impl");
    case RK_clang__CXXNullPtrLiteralExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXNullPtrLiteralExpr"), "impl");
    case RK_clang__CXXStdInitializerListExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXStdInitializerListExpr"), "impl");
    case RK_clang__CXXTypeidExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXTypeidExpr"), "impl");
    case RK_clang__MSPropertyRefExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "MSPropertyRefExpr"), "impl");
    case RK_clang__MSPropertySubscriptExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "MSPropertySubscriptExpr"), "impl");
    case RK_clang__CXXUuidofExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXUuidofExpr"), "impl");
    case RK_clang__CXXThisExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXThisExpr"), "impl");
    case RK_clang__CXXThrowExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXThrowExpr"), "impl");
    case RK_clang__CXXDefaultArgExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXDefaultArgExpr"), "impl");
    case RK_clang__CXXDefaultInitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXDefaultInitExpr"), "impl");
    case RK_clang__CXXTemporary:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXTemporary"), "impl");
    case RK_clang__CXXBindTemporaryExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXBindTemporaryExpr"), "impl");
    case RK_clang__CXXConstructExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXConstructExpr"), "impl");
    case RK_clang__CXXInheritedCtorInitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXInheritedCtorInitExpr"), "impl");
    case RK_clang__CXXFunctionalCastExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXFunctionalCastExpr"), "impl");
    case RK_clang__CXXTemporaryObjectExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXTemporaryObjectExpr"), "impl");
    case RK_clang__LambdaExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "LambdaExpr"), "impl");
    case RK_clang__CXXScalarValueInitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXScalarValueInitExpr"), "impl");
    case RK_clang__CXXNewExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXNewExpr"), "impl");
    case RK_clang__CXXDeleteExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXDeleteExpr"), "impl");
    case RK_clang__CXXPseudoDestructorExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXPseudoDestructorExpr"), "impl");
    case RK_clang__TypeTraitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "TypeTraitExpr"), "impl");
    case RK_clang__ArrayTypeTraitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ArrayTypeTraitExpr"), "impl");
    case RK_clang__ExpressionTraitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ExpressionTraitExpr"), "impl");
    case RK_clang__OverloadExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OverloadExpr"), "impl");
    case RK_clang__OverloadExpr__FindResult:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OverloadExpr"), "FindResult"), "impl");
    case RK_clang__UnresolvedLookupExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UnresolvedLookupExpr"), "impl");
    case RK_clang__DependentScopeDeclRefExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DependentScopeDeclRefExpr"), "impl");
    case RK_clang__ExprWithCleanups:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ExprWithCleanups"), "impl");
    case RK_clang__CXXUnresolvedConstructExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXUnresolvedConstructExpr"), "impl");
    case RK_clang__CXXDependentScopeMemberExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXDependentScopeMemberExpr"), "impl");
    case RK_clang__UnresolvedMemberExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "UnresolvedMemberExpr"), "impl");
    case RK_clang__CXXNoexceptExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXNoexceptExpr"), "impl");
    case RK_clang__PackExpansionExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "PackExpansionExpr"), "impl");
    case RK_clang__SizeOfPackExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SizeOfPackExpr"), "impl");
    case RK_clang__SubstNonTypeTemplateParmExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SubstNonTypeTemplateParmExpr"), "impl");
    case RK_clang__SubstNonTypeTemplateParmPackExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "SubstNonTypeTemplateParmPackExpr"), "impl");
    case RK_clang__FunctionParmPackExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "FunctionParmPackExpr"), "impl");
    case RK_clang__MaterializeTemporaryExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "MaterializeTemporaryExpr"), "impl");
    case RK_clang__CXXFoldExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXFoldExpr"), "impl");
    case RK_clang__CoroutineSuspendExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CoroutineSuspendExpr"), "impl");
    case RK_clang__CoawaitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CoawaitExpr"), "impl");
    case RK_clang__DependentCoawaitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DependentCoawaitExpr"), "impl");
    case RK_clang__CoyieldExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CoyieldExpr"), "impl");
    case RK_clang__ReflectionExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ReflectionExpr"), "impl");
    case RK_clang__CompilerMessageExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CompilerMessageExpr"), "impl");
    case RK_clang__CompilerDiagnosticExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CompilerDiagnosticExpr"), "impl");
    case RK_clang__CXXMetaparseExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXMetaparseExpr"), "impl");
    case RK_clang__ReflectionTraitExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ReflectionTraitExpr"), "impl");
    case RK_clang__ReflectNewExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ReflectNewExpr"), "impl");
    case RK_clang__ReflectDeleteExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ReflectDeleteExpr"), "impl");
    case RK_clang__CXXConstantExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXConstantExpr"), "impl");
    case RK_clang__CXXDependentIdExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXDependentIdExpr"), "impl");
    case RK_clang__CXXConcatenateExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXConcatenateExpr"), "impl");
    case RK_clang__ObjCStringLiteral:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCStringLiteral"), "impl");
    case RK_clang__ObjCBoolLiteralExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCBoolLiteralExpr"), "impl");
    case RK_clang__ObjCBoxedExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCBoxedExpr"), "impl");
    case RK_clang__ObjCArrayLiteral:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCArrayLiteral"), "impl");
    case RK_clang__ObjCDictionaryElement:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCDictionaryElement"), "impl");
    case RK_clang__ObjCDictionaryLiteral:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCDictionaryLiteral"), "impl");
    case RK_clang__ObjCEncodeExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCEncodeExpr"), "impl");
    case RK_clang__ObjCSelectorExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCSelectorExpr"), "impl");
    case RK_clang__ObjCProtocolExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCProtocolExpr"), "impl");
    case RK_clang__ObjCIvarRefExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCIvarRefExpr"), "impl");
    case RK_clang__ObjCPropertyRefExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCPropertyRefExpr"), "impl");
    case RK_clang__ObjCSubscriptRefExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCSubscriptRefExpr"), "impl");
    case RK_clang__ObjCMessageExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCMessageExpr"), "impl");
    case RK_clang__ObjCIsaExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCIsaExpr"), "impl");
    case RK_clang__ObjCIndirectCopyRestoreExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCIndirectCopyRestoreExpr"), "impl");
    case RK_clang__ObjCBridgedCastExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCBridgedCastExpr"), "impl");
    case RK_clang__ObjCAvailabilityCheckExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCAvailabilityCheckExpr"), "impl");
    case RK_clang__OMPArraySectionExpr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPArraySectionExpr"), "impl");
    case RK_clang__CXXCatchStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXCatchStmt"), "impl");
    case RK_clang__CXXTryStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXTryStmt"), "impl");
    case RK_clang__CXXForRangeStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXForRangeStmt"), "impl");
    case RK_clang__CXXExpansionStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXExpansionStmt"), "impl");
    case RK_clang__CXXTupleExpansionStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXTupleExpansionStmt"), "impl");
    case RK_clang__CXXPackExpansionStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXPackExpansionStmt"), "impl");
    case RK_clang__MSDependentExistsStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "MSDependentExistsStmt"), "impl");
    case RK_clang__CoroutineBodyStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CoroutineBodyStmt"), "impl");
    case RK_clang__CoroutineBodyStmt__CtorArgs:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CoroutineBodyStmt"), "CtorArgs"), "impl");
    case RK_clang__CoreturnStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CoreturnStmt"), "impl");
    case RK_clang__CXXQueueMetaparseStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "CXXQueueMetaparseStmt"), "impl");
    case RK_clang__ObjCForCollectionStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCForCollectionStmt"), "impl");
    case RK_clang__ObjCAtCatchStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCAtCatchStmt"), "impl");
    case RK_clang__ObjCAtFinallyStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCAtFinallyStmt"), "impl");
    case RK_clang__ObjCAtTryStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCAtTryStmt"), "impl");
    case RK_clang__ObjCAtSynchronizedStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCAtSynchronizedStmt"), "impl");
    case RK_clang__ObjCAtThrowStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCAtThrowStmt"), "impl");
    case RK_clang__ObjCAutoreleasePoolStmt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "ObjCAutoreleasePoolStmt"), "impl");
    case RK_clang__OMPClause:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPClause"), "impl");
    case RK_clang__OMPExecutableDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPExecutableDirective"), "impl");
    case RK_clang__OMPParallelDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPParallelDirective"), "impl");
    case RK_clang__OMPLoopDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPLoopDirective"), "impl");
    case RK_clang__OMPSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPSimdDirective"), "impl");
    case RK_clang__OMPForDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPForDirective"), "impl");
    case RK_clang__OMPForSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPForSimdDirective"), "impl");
    case RK_clang__OMPSectionsDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPSectionsDirective"), "impl");
    case RK_clang__OMPSectionDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPSectionDirective"), "impl");
    case RK_clang__OMPSingleDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPSingleDirective"), "impl");
    case RK_clang__OMPMasterDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPMasterDirective"), "impl");
    case RK_clang__OMPCriticalDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPCriticalDirective"), "impl");
    case RK_clang__OMPParallelForDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPParallelForDirective"), "impl");
    case RK_clang__OMPParallelForSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPParallelForSimdDirective"), "impl");
    case RK_clang__OMPParallelSectionsDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPParallelSectionsDirective"), "impl");
    case RK_clang__OMPTaskDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTaskDirective"), "impl");
    case RK_clang__OMPTaskyieldDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTaskyieldDirective"), "impl");
    case RK_clang__OMPBarrierDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPBarrierDirective"), "impl");
    case RK_clang__OMPTaskwaitDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTaskwaitDirective"), "impl");
    case RK_clang__OMPTaskgroupDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTaskgroupDirective"), "impl");
    case RK_clang__OMPFlushDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPFlushDirective"), "impl");
    case RK_clang__OMPOrderedDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPOrderedDirective"), "impl");
    case RK_clang__OMPAtomicDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPAtomicDirective"), "impl");
    case RK_clang__OMPTargetDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetDirective"), "impl");
    case RK_clang__OMPTargetDataDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetDataDirective"), "impl");
    case RK_clang__OMPTargetEnterDataDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetEnterDataDirective"), "impl");
    case RK_clang__OMPTargetExitDataDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetExitDataDirective"), "impl");
    case RK_clang__OMPTargetParallelDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetParallelDirective"), "impl");
    case RK_clang__OMPTargetParallelForDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetParallelForDirective"), "impl");
    case RK_clang__OMPTeamsDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTeamsDirective"), "impl");
    case RK_clang__OMPCancellationPointDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPCancellationPointDirective"), "impl");
    case RK_clang__OMPCancelDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPCancelDirective"), "impl");
    case RK_clang__OMPTaskLoopDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTaskLoopDirective"), "impl");
    case RK_clang__OMPTaskLoopSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTaskLoopSimdDirective"), "impl");
    case RK_clang__OMPDistributeDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPDistributeDirective"), "impl");
    case RK_clang__OMPTargetUpdateDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetUpdateDirective"), "impl");
    case RK_clang__OMPDistributeParallelForDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPDistributeParallelForDirective"), "impl");
    case RK_clang__OMPDistributeParallelForSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPDistributeParallelForSimdDirective"), "impl");
    case RK_clang__OMPDistributeSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPDistributeSimdDirective"), "impl");
    case RK_clang__OMPTargetParallelForSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetParallelForSimdDirective"), "impl");
    case RK_clang__OMPTargetSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetSimdDirective"), "impl");
    case RK_clang__OMPTeamsDistributeDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTeamsDistributeDirective"), "impl");
    case RK_clang__OMPTeamsDistributeSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTeamsDistributeSimdDirective"), "impl");
    case RK_clang__OMPTeamsDistributeParallelForSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTeamsDistributeParallelForSimdDirective"), "impl");
    case RK_clang__OMPTeamsDistributeParallelForDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTeamsDistributeParallelForDirective"), "impl");
    case RK_clang__OMPTargetTeamsDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetTeamsDirective"), "impl");
    case RK_clang__OMPTargetTeamsDistributeDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetTeamsDistributeDirective"), "impl");
    case RK_clang__OMPTargetTeamsDistributeParallelForDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetTeamsDistributeParallelForDirective"), "impl");
    case RK_clang__OMPTargetTeamsDistributeParallelForSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetTeamsDistributeParallelForSimdDirective"), "impl");
    case RK_clang__OMPTargetTeamsDistributeSimdDirective:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "OMPTargetTeamsDistributeSimdDirective"), "impl");
    case RK_clang__StoredDeclsMap:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "StoredDeclsMap"), "impl");
    case RK_clang__DependentDiagnostic:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "clang"), "DependentDiagnostic"), "impl");
    case RK_reflcontainers__VectorStr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "VectorStr"), "impl");
    case RK_reflcontainers__VectorInt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "VectorInt"), "impl");
    case RK_reflcontainers__SetInt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "SetInt"), "impl");
    case RK_reflcontainers__SetStr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "SetStr"), "impl");
    case RK_reflcontainers__IntIntPair:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "IntIntPair"), "impl");
    case RK_reflcontainers__IntStrPair:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "IntStrPair"), "impl");
    case RK_reflcontainers__StrIntPair:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "StrIntPair"), "impl");
    case RK_reflcontainers__StrStrPair:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "StrStrPair"), "impl");
    case RK_reflcontainers__MapIntStr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "MapIntStr"), "impl");
    case RK_reflcontainers__MapStrInt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "MapStrInt"), "impl");
    case RK_reflcontainers__MapStrStr:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "MapStrStr"), "impl");
    case RK_reflcontainers__MapIntInt:
      return LookupCTD(S, Loc, LookupDC(S, Loc, LookupDC(S, Loc, ReflNs, "reflcontainers"), "MapIntInt"), "impl");
