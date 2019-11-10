namespace llvm__APInt {
  enum memnames {
    needsCleanup,
    Profile,
    isNegative,
    isNonNegative,
    isSignBitSet,
    isSignBitClear,
    isStrictlyPositive,
    isAllOnesValue,
    isNullValue,
    isOneValue,
    isMaxValue,
    isMaxSignedValue,
    isMinValue,
    isMinSignedValue,
    isIntN,
    isSignedIntN,
    isPowerOf2,
    isSignMask,
    getBoolValue,
    getLimitedValue,
    isSplat,
    isMask,
    isMask1,
    isShiftedMask,
    isSameValue,
    getRawData,
    operator_not,
    urem,
    srem,
    udivrem,
    sdivrem,
    operator_sub,
    operator_eq_eq,
    operator_eq_eq1,
    eq,
    operator_not_eq,
    operator_not_eq1,
    ne,
    ult,
    ult1,
    slt,
    slt1,
    ule,
    ule1,
    sle,
    sle1,
    ugt,
    ugt1,
    sgt,
    sgt1,
    uge,
    uge1,
    sge,
    sge1,
    intersects,
    isSubsetOf,
    getBitWidth,
    getNumWords,
    getNumWords1,
    getActiveBits,
    getActiveWords,
    getMinSignedBits,
    getZExtValue,
    getSExtValue,
    getBitsNeeded,
    countLeadingZeros,
    countLeadingOnes,
    getNumSignBits,
    countTrailingZeros,
    countTrailingOnes,
    countPopulation,
    print,
    toString,
    roundToDouble,
    roundToDouble1,
    signedRoundToDouble,
    bitsToDouble,
    bitsToFloat,
    logBase2,
    ceilLogBase2,
    nearestLogBase2,
    exactLogBase2,
    magic,
    magicu,
    tcSet,
    tcAssign,
    tcIsZero,
    tcExtractBit,
    tcExtract,
    tcSetBit,
    tcClearBit,
    tcLSB,
    tcMSB,
    tcNegate,
    tcAdd,
    tcAddPart,
    tcSubtract,
    tcSubtractPart,
    tcMultiplyPart,
    tcMultiply,
    tcFullMultiply,
    tcDivide,
    tcShiftLeft,
    tcShiftRight,
    tcAnd,
    tcOr,
    tcXor,
    tcComplement,
    tcCompare,
    tcIncrement,
    tcDecrement,
    tcSetLeastSignificantBits,
    dump,
  };
  static const unsigned _total_num_mems_ = dump + 1;
}
namespace llvm__APInt__ms {
  enum memnames {
    s,
  };
  static const unsigned _total_num_mems_ = s + 1;
}
namespace llvm__APInt__mu {
  enum memnames {
    a,
    s,
  };
  static const unsigned _total_num_mems_ = s + 1;
}
namespace llvm__APFloatBase {
  enum memnames {
  };
  static const unsigned _total_num_mems_ = 0;
}
namespace llvm__APFloat {
  enum memnames {
    needsCleanup,
    Profile,
    convertToInteger,
    convertToDouble,
    convertToFloat,
    compare,
    bitwiseIsEqual,
    isExactlyValue,
    convertToHexString,
    isZero,
    isInfinity,
    isNaN,
    isNegative,
    isDenormal,
    isSignaling,
    isNormal,
    isFinite,
    getCategory,
    isNonZero,
    isFiniteNonZero,
    isPosZero,
    isNegZero,
    isSmallest,
    isLargest,
    isInteger,
    print,
    dump,
    getExactInverse,
  };
  static const unsigned _total_num_mems_ = getExactInverse + 1;
}
namespace llvm__APSInt {
  enum memnames {
    isSigned,
    isUnsigned,
    toString,
    getExtValue,
    trunc,
    extend,
    extOrTrunc,
    operator_mod,
    operator_div,
    operator_gr_gr,
    operator_less,
    operator_gr,
    operator_less_eq,
    operator_gr_eq,
    operator_eq_eq,
    operator_not_eq,
    operator_eq_eq1,
    operator_not_eq1,
    operator_less_eq1,
    operator_gr_eq1,
    operator_less1,
    operator_gr1,
    operator_less_less,
    operator_minus,
    operator_and,
    operator_or,
    operator_exp,
    operator_star,
    operator_plus,
    operator_minus1,
    operator_tilde,
    getMaxValue,
    getMinValue,
    isSameValue,
    compareValues,
    get,
    getUnsigned,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__APValue {
  enum memnames {
    needsCleanup,
    getKind,
    isUninit,
    isInt,
    isFloat,
    isComplexInt,
    isComplexFloat,
    isLValue,
    isVector,
    isArray,
    isStruct,
    isUnion,
    isMemberPointer,
    isAddrLabelDiff,
    dump,
    dump1,
    printPretty,
    getAsString,
    getInt,
    getFloat,
    getComplexIntReal,
    getComplexIntImag,
    getComplexFloatReal,
    getComplexFloatImag,
    getLValueBase,
    getLValueOffset,
    isLValueOnePastTheEnd,
    hasLValuePath,
    getLValueCallIndex,
    getLValueVersion,
    isNullPointer,
    getVectorElt,
    getVectorLength,
    getArrayInitializedElt,
    hasArrayFiller,
    getArrayFiller,
    getArrayInitializedElts,
    getArraySize,
    getStructNumBases,
    getStructNumFields,
    getStructBase,
    getStructField,
    getUnionField,
    getUnionValue,
    getMemberPointerDecl,
    isMemberPointerToDerivedMember,
    getMemberPointerPath,
    getAddrLabelDiffLHS,
    getAddrLabelDiffRHS,
  };
  static const unsigned _total_num_mems_ = getAddrLabelDiffRHS + 1;
}
namespace clang__APValue__LValueBase {
  enum memnames {
    getOpaqueValue,
    isNull,
    operator_bool,
    getCallIndex,
    getVersion,
    operator_eq_eq,
  };
  static const unsigned _total_num_mems_ = operator_eq_eq + 1;
}
namespace clang__DiagnosticOptions {
  enum memnames {
    Retain,
    Release,
    IgnoreWarnings,
    NoRewriteMacros,
    Pedantic,
    PedanticErrors,
    ShowColumn,
    ShowLocation,
    AbsolutePath,
    ShowCarets,
    ShowFixits,
    ShowSourceRanges,
    ShowParseableFixits,
    ShowPresumedLoc,
    ShowOptionNames,
    ShowNoteIncludeStack,
    ShowCategories,
    ShowColors,
    VerifyDiagnostics,
    ElideType,
    ShowTemplateTree,
    CLFallbackMode,
    ErrorLimit,
    MacroBacktraceLimit,
    TemplateBacktraceLimit,
    ConstexprBacktraceLimit,
    SpellCheckingLimit,
    SnippetLineLimit,
    TabStop,
    MessageLength,
    getFormat,
    getShowOverloads,
    getVerifyIgnoreUnexpected,
  };
  static const unsigned _total_num_mems_ = getVerifyIgnoreUnexpected + 1;
}
namespace clang__FileID {
  enum memnames {
    isValid,
    isInvalid,
    operator_eq_eq,
    operator_less,
    operator_less_eq,
    operator_not_eq,
    operator_gr,
    operator_gr_eq,
    getSentinel,
    getHashValue,
  };
  static const unsigned _total_num_mems_ = getHashValue + 1;
}
namespace clang__SourceLocation {
  enum memnames {
    isFileID,
    isMacroID,
    isValid,
    isInvalid,
    getLocWithOffset,
    getRawEncoding,
    getFromRawEncoding,
    getPtrEncoding,
    getFromPtrEncoding,
    isPairOfFileLocations,
    print,
    printToString,
    dump,
  };
  static const unsigned _total_num_mems_ = dump + 1;
}
namespace clang__SourceRange {
  enum memnames {
    getBegin,
    getEnd,
    isValid,
    isInvalid,
    operator_eq_eq,
    operator_not_eq,
  };
  static const unsigned _total_num_mems_ = operator_not_eq + 1;
}
namespace clang__CharSourceRange {
  enum memnames {
    getTokenRange,
    getCharRange,
    getTokenRange1,
    getCharRange1,
    isTokenRange,
    isCharRange,
    getBegin,
    getEnd,
    getAsRange,
    isValid,
    isInvalid,
  };
  static const unsigned _total_num_mems_ = isInvalid + 1;
}
namespace clang__PresumedLoc {
  enum memnames {
    isInvalid,
    isValid,
    getFilename,
    getLine,
    getColumn,
    getIncludeLoc,
  };
  static const unsigned _total_num_mems_ = getIncludeLoc + 1;
}
namespace clang__FullSourceLoc {
  enum memnames {
    hasManager,
    getManager,
    getFileID,
    getExpansionLoc,
    getSpellingLoc,
    getFileLoc,
    getPresumedLoc,
    isMacroArgExpansion,
    getImmediateMacroCallerLoc,
    getFileOffset,
    getExpansionLineNumber,
    getExpansionColumnNumber,
    getSpellingLineNumber,
    getSpellingColumnNumber,
    getCharacterData,
    getLineNumber,
    getColumnNumber,
    getFileEntry,
    getBufferData,
    isInSystemHeader,
    isBeforeInTranslationUnitThan,
    isBeforeInTranslationUnitThan1,
    dump,
    operator_eq_eq,
    operator_not_eq,
  };
  static const unsigned _total_num_mems_ = operator_not_eq + 1;
}
namespace llvm__DebugEpochBase {
  enum memnames {
  };
  static const unsigned _total_num_mems_ = 0;
}
namespace clang__FixItHint {
  enum memnames {
    RemoveRange,
    InsertFromRange,
    BeforePreviousInsertions,
    isNull,
    CreateInsertionFromRange,
    CreateRemoval,
    CreateRemoval1,
  };
  static const unsigned _total_num_mems_ = CreateRemoval1 + 1;
}
namespace clang__DiagnosticsEngine {
  enum memnames {
    Retain,
    Release,
    dump,
    dump1,
    getDiagnosticOptions,
    getClient,
    ownsClient,
    hasSourceManager,
    getSourceManager,
    getTemplateBacktraceLimit,
    getConstexprBacktraceLimit,
    getIgnoreAllWarnings,
    getEnableAllWarnings,
    getWarningsAsErrors,
    getErrorsAsFatal,
    getSuppressSystemWarnings,
    getSuppressAllDiagnostics,
    getShowOverloads,
    isLastDiagnosticIgnored,
    getExtensionHandlingBehavior,
    hasErrorOccurred,
    hasUncompilableErrorOccurred,
    hasFatalErrorOccurred,
    hasUnrecoverableErrorOccurred,
    getNumWarnings,
    isIgnored,
    getDiagnosticLevel,
    isDiagnosticInFlight,
    getFlagValue,
  };
  static const unsigned _total_num_mems_ = getFlagValue + 1;
}
namespace clang__DiagnosticBuilder {
  enum memnames {
    setForceEmit,
    operator_bool,
    AddString,
    AddTaggedVal,
    AddSourceRange,
    AddFixItHint,
    addFlagValue,
  };
  static const unsigned _total_num_mems_ = addFlagValue + 1;
}
namespace clang__DiagnosticConsumer {
  enum memnames {
    getNumErrors,
    getNumWarnings,
    IncludeInDiagnosticCounts,
  };
  static const unsigned _total_num_mems_ = IncludeInDiagnosticCounts + 1;
}
namespace clang__IdentifierInfo {
  enum memnames {
    isStr,
    getNameStart,
    getLength,
    getName,
    hasMacroDefinition,
    hadMacroDefinition,
    getTokenID,
    hasRevertedTokenIDToIdentifier,
    getPPKeywordID,
    getObjCKeywordID,
    hasRevertedBuiltin,
    getBuiltinID,
    getObjCOrBuiltinID,
    isExtensionToken,
    isFutureCompatKeyword,
    isPoisoned,
    isCPlusPlusOperatorKeyword,
    isKeyword,
    isCPlusPlusKeyword,
    isHandleIdentifierCase,
    isFromAST,
    hasChangedSinceDeserialization,
    hasFETokenInfoChangedSinceDeserialization,
    isOutOfDate,
    isModulesImport,
    isEditorPlaceholder,
    operator_less,
  };
  static const unsigned _total_num_mems_ = operator_less + 1;
}
namespace clang__IdentifierInfoLookup {
  enum memnames {
  };
  static const unsigned _total_num_mems_ = 0;
}
namespace clang__IdentifierTable {
  enum memnames {
    getExternalIdentifierLookup,
    size,
    PrintStats,
  };
  static const unsigned _total_num_mems_ = PrintStats + 1;
}
namespace clang__Selector {
  enum memnames {
    operator_eq_eq,
    operator_not_eq,
    getAsOpaquePtr,
    isNull,
    isKeywordSelector,
    isUnarySelector,
    getNumArgs,
    getIdentifierInfoForSlot,
    getNameForSlot,
    getAsString,
    print,
    dump,
    getMethodFamily,
    getStringFormatFamily,
    getEmptyMarker,
    getTombstoneMarker,
    getInstTypeMethodFamily,
  };
  static const unsigned _total_num_mems_ = getInstTypeMethodFamily + 1;
}
namespace clang__SelectorTable {
  enum memnames {
    getTotalMemory,
    constructSetterSelector,
    getPropertyNameFromSetterSelector,
  };
  static const unsigned _total_num_mems_ = getPropertyNameFromSetterSelector + 1;
}
namespace clang__PartialDiagnostic {
  enum memnames {
    getDiagID,
    AddTaggedVal,
    AddString,
    Emit,
    hasStorage,
    getNumArgs,
    operator_less_less,
    operator_less_less1,
    operator_less_less2,
    operator_less_less3,
    operator_less_less4,
    operator_less_less5,
    operator_less_less6,
    operator_less_less7,
  };
  static const unsigned _total_num_mems_ = operator_less_less7 + 1;
}
namespace clang__DeclarationName {
  enum memnames {
    getUsingDirectiveName,
    operator_bool,
    isEmpty,
    isIdentifier,
    isObjCZeroArgSelector,
    isObjCOneArgSelector,
    getNameKind,
    isDependentName,
    getAsString,
    getAsIdentifierInfo,
    getAsOpaqueInteger,
    getAsOpaquePtr,
    getFromOpaquePtr,
    getFromOpaqueInteger,
    getCXXNameType,
    getCXXDeductionGuideTemplate,
    getCXXOverloadedOperator,
    getCXXLiteralIdentifier,
    getCXXIdExprArguments,
    getObjCSelector,
    operator_eq_eq,
    operator_not_eq,
    getEmptyMarker,
    getTombstoneMarker,
    compare,
    dump,
  };
  static const unsigned _total_num_mems_ = dump + 1;
}
namespace clang__DeclarationNameLoc {
  enum memnames {
  };
  static const unsigned _total_num_mems_ = 0;
}
namespace clang__DeclarationNameInfo {
  enum memnames {
    getName,
    getLoc,
    getInfo,
    getNamedTypeInfo,
    getCXXOperatorNameRange,
    getCXXLiteralOperatorNameLoc,
    getCXXIdExprNameRange,
    isInstantiationDependent,
    containsUnexpandedParameterPack,
    getAsString,
    printName,
    getBeginLoc,
    getSourceRange,
    getLocStart,
    getLocEnd,
    getEndLoc,
  };
  static const unsigned _total_num_mems_ = getEndLoc + 1;
}
namespace llvm__raw_ostream {
  enum memnames {
    tell,
    GetBufferSize,
    GetNumBytesInBuffer,
    is_displayed,
    has_colors,
  };
  static const unsigned _total_num_mems_ = has_colors + 1;
}
namespace llvm__VersionTuple {
  enum memnames {
    empty,
    getMajor,
    operator_eq_eq,
    operator_not_eq,
    operator_less,
    operator_gr,
    operator_less_eq,
    operator_gr_eq,
    getAsString,
  };
  static const unsigned _total_num_mems_ = getAsString + 1;
}
namespace clang__Decl {
  enum memnames {
    getSourceRange,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getLocation,
    getKind,
    getDeclKindName,
    getNextDeclInContext,
    getDeclContext,
    getNonClosureContext,
    getTranslationUnitDecl,
    isInAnonymousNamespace,
    isInStdNamespace,
    getASTContext,
    getAccess,
    getAccessUnsafe,
    hasAttrs,
    attrs,
    getMaxAlignment,
    isInvalidDecl,
    isImplicit,
    isUsed,
    isReferenced,
    isThisDeclarationReferenced,
    instantiationsWillNeedParsing,
    isTopLevelDeclInObjCContainer,
    getExternalSourceSymbolAttr,
    isModulePrivate,
    isExported,
    hasDefiningAttr,
    getDefiningAttr,
    getVersionIntroduced,
    isWeakImported,
    isFromASTFile,
    getGlobalID,
    getOwningModuleID,
    getImportedOwningModule,
    getLocalOwningModule,
    hasOwningModule,
    getOwningModule,
    getOwningModuleForLinkage,
    isHidden,
    getModuleOwnershipKind,
    getIdentifierNamespace,
    isInIdentifierNamespace,
    getIdentifierNamespaceForKind,
    hasTagIdentifierNamespace,
    isTagIdentifierNamespace,
    getLexicalDeclContext,
    isOutOfLine,
    isTemplated,
    isDefinedOutsideFunctionOrMethod,
    isLexicallyWithinFunctionOrMethod,
    getParentFunctionOrMethod,
    getCanonicalDecl,
    isCanonicalDecl,
    redecls,
    getPreviousDecl,
    isFirstDecl,
    getMostRecentDecl,
    getBody,
    hasBody,
    getBodyRBrace,
    add,
    EnableStatistics,
    PrintStats,
    isTemplateParameter,
    isTemplateParameterPack,
    isParameterPack,
    isTemplateDecl,
    isFunctionOrFunctionTemplate,
    getDescribedTemplate,
    getAsFunction,
    getFriendObjectKind,
    classofKind,
    print,
    print1,
    printGroup,
    dump,
    dumpColor,
    dump1,
    getFunctionType,
  };
  static const unsigned _total_num_mems_ = getFunctionType + 1;
}
namespace clang__DeclContextLookupResult {
  enum memnames {
    _this_,
    empty,
    data,
    size,
    front,
    back,
    operator_sub,
    slice,
  };
  static const unsigned _total_num_mems_ = slice + 1;
}
namespace clang__DeclContext {
  enum memnames {
    getDeclKind,
    getDeclKindName,
    getParent,
    getLexicalParent,
    getLookupParent,
    getParentASTContext,
    isClosure,
    isObjCContainer,
    isFunctionOrMethod,
    isLookupContext,
    isFileContext,
    isTranslationUnit,
    isRecord,
    isNamespace,
    isStdNamespace,
    isInlineNamespace,
    isDependentContext,
    isTransparentContext,
    isExternCContext,
    getExternCContext,
    isExternCXXContext,
    Equals,
    Encloses,
    getNonClosureAncestor,
    getPrimaryContext,
    getRedeclContext,
    getEnclosingNamespaceContext,
    getOuterLexicalRecordContext,
    InEnclosingNamespaceSetOf,
    decls,
    decls_empty,
    noload_decls,
    containsDecl,
    containsDeclAndLoad,
    lookup,
    lookups,
    ddiags,
    getLookupPtr,
    hasExternalLexicalStorage,
    hasExternalVisibleStorage,
    isDeclInLexicalTraversal,
    shouldUseQualifiedLookup,
    classof,
    classof1,
    dumpDeclContext,
    dumpLookups,
    dumpLookups1,
  };
  static const unsigned _total_num_mems_ = dumpLookups1 + 1;
}
namespace clang__CharUnits {
  enum memnames {
    Zero,
    One,
    fromQuantity,
    operator_eq_eq,
    operator_not_eq,
    operator_less,
    operator_less_eq,
    operator_gr,
    operator_gr_eq,
    isZero,
    isOne,
    isPositive,
    isNegative,
    isPowerOfTwo,
    isMultipleOf,
    operator_star,
    operator_div,
    operator_div1,
    operator_mod,
    operator_mod1,
    operator_plus,
    operator_minus,
    operator_minus1,
    getQuantity,
    alignTo,
    alignmentAtOffset,
    alignmentOfArrayElement,
  };
  static const unsigned _total_num_mems_ = alignmentOfArrayElement + 1;
}
namespace clang__FileSystemOptions {
  enum memnames {
  };
  static const unsigned _total_num_mems_ = 0;
}
namespace llvm__Twine {
  enum memnames {
    isTriviallyEmpty,
    isSingleStringRef,
    str,
    getSingleStringRef,
    print,
    dump,
    printRepr,
    dumpRepr,
  };
  static const unsigned _total_num_mems_ = dumpRepr + 1;
}
namespace llvm__sys__fs__UniqueID {
  enum memnames {
    operator_eq_eq,
    operator_not_eq,
    operator_less,
    getDevice,
    getFile,
  };
  static const unsigned _total_num_mems_ = getFile + 1;
}
namespace llvm__MemoryBuffer {
  enum memnames {
    getBufferStart,
    getBufferEnd,
    getBufferSize,
    getBuffer,
    getBufferIdentifier,
    getBufferKind,
    getMemBufferRef,
  };
  static const unsigned _total_num_mems_ = getMemBufferRef + 1;
}
namespace llvm__MemoryBufferRef {
  enum memnames {
    getBuffer,
    getBufferIdentifier,
    getBufferStart,
    getBufferEnd,
    getBufferSize,
  };
  static const unsigned _total_num_mems_ = getBufferSize + 1;
}
namespace clang__DirectoryEntry {
  enum memnames {
    getName,
  };
  static const unsigned _total_num_mems_ = getName + 1;
}
namespace clang__FileEntry {
  enum memnames {
    getName,
    tryGetRealPathName,
    isValid,
    getSize,
    getUID,
    getUniqueID,
    isInPCH,
    getModificationTime,
    getDir,
    operator_less,
    isNamedPipe,
    closeFile,
  };
  static const unsigned _total_num_mems_ = closeFile + 1;
}
namespace clang__FileManager {
  enum memnames {
    Retain,
    Release,
    getFileSystemOpts,
    modifyFileEntry,
    PrintStats,
  };
  static const unsigned _total_num_mems_ = PrintStats + 1;
}
namespace clang__ASTFileSignature {
  enum memnames {
    _this_,
    cbegin,
    cend,
    size,
    max_size,
    empty,
    operator_sub,
    at,
    front,
    back,
    data,
    operator_bool,
  };
  static const unsigned _total_num_mems_ = operator_bool + 1;
}
namespace clang__Module {
  enum memnames {
    DefinitionLoc,
    Kind,
    Parent,
    Directory,
    Signature,
    ShadowingModule,
    IsMissingRequirement,
    HasIncompatibleModuleFile,
    IsAvailable,
    IsFromModuleFile,
    IsFramework,
    IsExplicit,
    IsSystem,
    IsExternC,
    IsInferred,
    InferSubmodules,
    InferExplicitSubmodules,
    InferExportWildcard,
    ConfigMacrosExhaustive,
    NoUndeclaredIncludes,
    ModuleMapIsPrivate,
    NameVisibility,
    InferredSubmoduleLoc,
    UseExportAsModuleLinkName,
    isAvailable,
    isSubModule,
    isSubModuleOf,
    isPartOfFramework,
    isSubFramework,
    getFullModuleName,
    getTopLevelModule,
    getTopLevelModuleName,
    getASTFile,
    getUmbrellaDir,
    getUmbrellaHeader,
    hasUmbrellaDir,
    directlyUses,
    isModuleVisible,
    getVisibilityID,
    submodules,
    getModuleInputBufferName,
    print,
    dump,
  };
  static const unsigned _total_num_mems_ = dump + 1;
}
namespace clang__Module__Header {
  enum memnames {
    Entry,
  };
  static const unsigned _total_num_mems_ = Entry + 1;
}
namespace clang__Module__DirectoryName {
  enum memnames {
    Entry,
  };
  static const unsigned _total_num_mems_ = Entry + 1;
}
namespace clang__ExternalASTSource {
  enum memnames {
    Retain,
    Release,
    getGeneration,
    getMemoryBufferSizes,
    getMemoryBufferSizes1,
  };
  static const unsigned _total_num_mems_ = getMemoryBufferSizes1 + 1;
}
namespace clang__ExternalASTSource__MemoryBufferSizes {
  enum memnames {
    malloc_bytes,
    mmap_bytes,
  };
  static const unsigned _total_num_mems_ = mmap_bytes + 1;
}
namespace llvm__FoldingSetBase {
  enum memnames {
    size,
    empty,
  };
  static const unsigned _total_num_mems_ = empty + 1;
}
namespace llvm__FoldingSetBase__Node {
  enum memnames {
    getNextInBucket,
  };
  static const unsigned _total_num_mems_ = getNextInBucket + 1;
}
namespace llvm__FoldingSetNodeIDRef {
  enum memnames {
    ComputeHash,
    operator_eq_eq,
    operator_not_eq,
    operator_less,
    getData,
    getSize,
  };
  static const unsigned _total_num_mems_ = getSize + 1;
}
namespace llvm__FoldingSetNodeID {
  enum memnames {
    ComputeHash,
    operator_eq_eq,
    operator_eq_eq1,
    operator_not_eq,
    operator_not_eq1,
    operator_less,
    operator_less1,
  };
  static const unsigned _total_num_mems_ = operator_less1 + 1;
}
namespace clang__NestedNameSpecifier {
  enum memnames {
    Create,
    Create1,
    Create2,
    Create3,
    Create4,
    GlobalSpecifier,
    SuperSpecifier,
    getPrefix,
    getKind,
    getAsIdentifier,
    getAsNamespace,
    getAsNamespaceAlias,
    getAsRecordDecl,
    getAsType,
    isDependent,
    isInstantiationDependent,
    containsUnexpandedParameterPack,
    print,
    Profile,
    dump,
    dump1,
  };
  static const unsigned _total_num_mems_ = dump1 + 1;
}
namespace clang__NestedNameSpecifierLoc {
  enum memnames {
    operator_bool,
    hasQualifier,
    getNestedNameSpecifier,
    getOpaqueData,
    getSourceRange,
    getLocalSourceRange,
    getBeginLoc,
    getEndLoc,
    getLocalBeginLoc,
    getLocalEndLoc,
    getPrefix,
    getTypeLoc,
    getDataLength,
    operator_eq_eq,
    operator_not_eq,
  };
  static const unsigned _total_num_mems_ = operator_not_eq + 1;
}
namespace clang__UncommonTemplateNameStorage {
  enum memnames {
    size,
  };
  static const unsigned _total_num_mems_ = size + 1;
}
namespace clang__OverloadedTemplateStorage {
  enum memnames {
    _this_,
  };
  static const unsigned _total_num_mems_ = 0;
}
namespace clang__SubstTemplateTemplateParmPackStorage {
  enum memnames {
    getParameterPack,
    getArgumentPack,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__TemplateName {
  enum memnames {
    isNull,
    getKind,
    getAsTemplateDecl,
    getAsOverloadedTemplate,
    getAsSubstTemplateTemplateParm,
    getAsSubstTemplateTemplateParmPack,
    getAsQualifiedTemplateName,
    getAsDependentTemplateName,
    getUnderlying,
    getNameToSubstitute,
    isDependent,
    isInstantiationDependent,
    containsUnexpandedParameterPack,
    print,
    dump,
    dump1,
    getAsVoidPointer,
    getFromVoidPointer,
  };
  static const unsigned _total_num_mems_ = getFromVoidPointer + 1;
}
namespace clang__SubstTemplateTemplateParmStorage {
  enum memnames {
    getParameter,
    getReplacement,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__QualifiedTemplateName {
  enum memnames {
    getQualifier,
    hasTemplateKeyword,
    getDecl,
    getTemplateDecl,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__DependentTemplateName {
  enum memnames {
    getQualifier,
    isIdentifier,
    getIdentifier,
    isOverloadedOperator,
    getOperator,
    Profile,
    Profile1,
  };
  static const unsigned _total_num_mems_ = Profile1 + 1;
}
namespace clang__LinkageInfo {
  enum memnames {
    external,
    internal,
    uniqueExternal,
    none,
    visible_none,
    getLinkage,
    getVisibility,
    isVisibilityExplicit,
  };
  static const unsigned _total_num_mems_ = isVisibilityExplicit + 1;
}
namespace clang__Qualifiers {
  enum memnames {
    removeCommonQualifiers,
    fromFastMask,
    fromCVRMask,
    fromCVRUMask,
    fromOpaqueValue,
    getAsOpaqueValue,
    hasConst,
    hasVolatile,
    hasRestrict,
    hasCVRQualifiers,
    getCVRQualifiers,
    hasUnaligned,
    hasObjCGCAttr,
    getObjCGCAttr,
    withoutObjCGCAttr,
    withoutObjCLifetime,
    hasObjCLifetime,
    getObjCLifetime,
    hasNonTrivialObjCLifetime,
    hasStrongOrWeakObjCLifetime,
    hasAddressSpace,
    getAddressSpace,
    hasTargetSpecificAddressSpace,
    getAddressSpaceAttributePrintValue,
    hasFastQualifiers,
    getFastQualifiers,
    hasNonFastQualifiers,
    getNonFastQualifiers,
    hasQualifiers,
    empty,
    isAddressSpaceSupersetOf,
    compatiblyIncludes,
    compatiblyIncludesObjCLifetime,
    isStrictSupersetOf,
    operator_eq_eq,
    operator_not_eq,
    operator_bool,
    operator_plus,
    operator_minus,
    getAsString,
    getAsString1,
    isEmptyWhenPrinted,
    print,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__SplitQualType {
  enum memnames {
    Ty,
    Quals,
    getSingleStepDesugaredType,
    operator_eq_eq,
    operator_not_eq,
  };
  static const unsigned _total_num_mems_ = operator_not_eq + 1;
}
namespace clang__QualType {
  enum memnames {
    getLocalFastQualifiers,
    getTypePtr,
    getTypePtrOrNull,
    getBaseTypeIdentifier,
    split,
    getAsOpaquePtr,
    getFromOpaquePtr,
    operator_star,
    operator_arrow,
    isCanonical,
    isCanonicalAsParam,
    isNull,
    isLocalConstQualified,
    isConstQualified,
    isLocalRestrictQualified,
    isRestrictQualified,
    isLocalVolatileQualified,
    isVolatileQualified,
    hasLocalQualifiers,
    hasQualifiers,
    hasLocalNonFastQualifiers,
    getLocalQualifiers,
    getQualifiers,
    getLocalCVRQualifiers,
    getCVRQualifiers,
    isConstant,
    isPODType,
    isCXX98PODType,
    isCXX11PODType,
    isTrivialType,
    isTriviallyCopyableType,
    mayBeDynamicClass,
    mayBeNotDynamicClass,
    withConst,
    withVolatile,
    withRestrict,
    withCVRQualifiers,
    withFastQualifiers,
    withExactLocalFastQualifiers,
    withoutLocalFastQualifiers,
    getCanonicalType,
    getLocalUnqualifiedType,
    getUnqualifiedType,
    getSplitUnqualifiedType,
    isMoreQualifiedThan,
    isAtLeastAsQualifiedAs,
    getNonReferenceType,
    getNonLValueExprType,
    getDesugaredType,
    getSplitDesugaredType,
    getSingleStepDesugaredType,
    IgnoreParens,
    operator_eq_eq,
    operator_not_eq,
    getAsString,
    getAsString1,
    getAsString2,
    getAsString3,
    getAsString_NoPrependScope,
    print,
    print1,
    print2,
    dump,
    dump1,
    dump2,
    Profile,
    getAddressSpace,
    getObjCGCAttr,
    isObjCGCWeak,
    isObjCGCStrong,
    getObjCLifetime,
    hasNonTrivialObjCLifetime,
    hasStrongOrWeakObjCLifetime,
    isNonWeakInMRRWithObjCWeak,
    isNonTrivialToPrimitiveDefaultInitialize,
    isNonTrivialToPrimitiveCopy,
    isNonTrivialToPrimitiveDestructiveMove,
    isDestructedType,
    isCForbiddenLValueType,
    substObjCMemberType,
    stripObjCKindOfType,
    getAtomicUnqualifiedType,
  };
  static const unsigned _total_num_mems_ = getAtomicUnqualifiedType + 1;
}
namespace clang__ExtQualsTypeCommonBase {
  enum memnames {
  };
  static const unsigned _total_num_mems_ = 0;
}
namespace clang__Type {
  enum memnames {
    getTypeClass,
    isFromAST,
    containsUnexpandedParameterPack,
    isCanonicalUnqualified,
    getLocallyUnqualifiedSingleStepDesugaredType,
    isIncompleteType,
    isIncompleteOrObjectType,
    isObjectType,
    isLiteralType,
    isStandardLayoutType,
    isBuiltinType,
    isSpecificBuiltinType,
    isPlaceholderType,
    getAsPlaceholderType,
    isSpecificPlaceholderType,
    isNonOverloadPlaceholderType,
    isIntegerType,
    isEnumeralType,
    isScopedEnumeralType,
    isBooleanType,
    isCharType,
    isWideCharType,
    isChar8Type,
    isChar16Type,
    isChar32Type,
    isAnyCharacterType,
    isIntegralType,
    isIntegralOrEnumerationType,
    isIntegralOrUnscopedEnumerationType,
    isRealFloatingType,
    isComplexType,
    isAnyComplexType,
    isFloatingType,
    isHalfType,
    isFloat16Type,
    isFloat128Type,
    isRealType,
    isArithmeticType,
    isVoidType,
    isScalarType,
    isAggregateType,
    isFundamentalType,
    isCompoundType,
    isFunctionType,
    isFunctionNoProtoType,
    isFunctionProtoType,
    isPointerType,
    isAnyPointerType,
    isBlockPointerType,
    isVoidPointerType,
    isReferenceType,
    isLValueReferenceType,
    isRValueReferenceType,
    isFunctionPointerType,
    isMemberPointerType,
    isMemberFunctionPointerType,
    isMemberDataPointerType,
    isArrayType,
    isConstantArrayType,
    isIncompleteArrayType,
    isVariableArrayType,
    isDependentSizedArrayType,
    isRecordType,
    isClassType,
    isStructureType,
    isObjCBoxableRecordType,
    isInterfaceType,
    isStructureOrClassType,
    isUnionType,
    isReflectedType,
    isComplexIntegerType,
    isVectorType,
    isExtVectorType,
    isDependentAddressSpaceType,
    isObjCObjectPointerType,
    isObjCRetainableType,
    isObjCLifetimeType,
    isObjCIndirectLifetimeType,
    isObjCNSObjectType,
    isObjCIndependentClassType,
    isObjCObjectType,
    isObjCQualifiedInterfaceType,
    isObjCQualifiedIdType,
    isObjCQualifiedClassType,
    isObjCObjectOrInterfaceType,
    isObjCIdType,
    isObjCInertUnsafeUnretainedType,
    isObjCIdOrObjectKindOfType,
    isObjCClassType,
    isObjCClassOrClassKindOfType,
    isBlockCompatibleObjCPointerType,
    isObjCSelType,
    isObjCBuiltinType,
    isObjCARCBridgableType,
    isCARCBridgableType,
    isTemplateTypeParmType,
    isNullPtrType,
    isAlignValT,
    isStdByteType,
    isAtomicType,
    isOCLImage1dROType,
    isOCLImage1dArrayROType,
    isOCLImage1dBufferROType,
    isOCLImage2dROType,
    isOCLImage2dArrayROType,
    isOCLImage2dDepthROType,
    isOCLImage2dArrayDepthROType,
    isOCLImage2dMSAAROType,
    isOCLImage2dArrayMSAAROType,
    isOCLImage2dMSAADepthROType,
    isOCLImage2dArrayMSAADepthROType,
    isOCLImage3dROType,
    isOCLImage1dWOType,
    isOCLImage1dArrayWOType,
    isOCLImage1dBufferWOType,
    isOCLImage2dWOType,
    isOCLImage2dArrayWOType,
    isOCLImage2dDepthWOType,
    isOCLImage2dArrayDepthWOType,
    isOCLImage2dMSAAWOType,
    isOCLImage2dArrayMSAAWOType,
    isOCLImage2dMSAADepthWOType,
    isOCLImage2dArrayMSAADepthWOType,
    isOCLImage3dWOType,
    isOCLImage1dRWType,
    isOCLImage1dArrayRWType,
    isOCLImage1dBufferRWType,
    isOCLImage2dRWType,
    isOCLImage2dArrayRWType,
    isOCLImage2dDepthRWType,
    isOCLImage2dArrayDepthRWType,
    isOCLImage2dMSAARWType,
    isOCLImage2dArrayMSAARWType,
    isOCLImage2dMSAADepthRWType,
    isOCLImage2dArrayMSAADepthRWType,
    isOCLImage3dRWType,
    isImageType,
    isSamplerT,
    isEventT,
    isClkEventT,
    isQueueT,
    isReserveIDT,
    isPipeType,
    isOpenCLSpecificType,
    isObjCARCImplicitlyUnretainedType,
    getObjCARCImplicitLifetime,
    getScalarTypeKind,
    isDependentType,
    isInstantiationDependentType,
    isUndeducedType,
    isVariablyModifiedType,
    hasSizedVLAType,
    hasUnnamedOrLocalType,
    isOverloadableType,
    isElaboratedTypeSpecifier,
    canDecayToPointerType,
    hasPointerRepresentation,
    hasObjCPointerRepresentation,
    hasIntegerRepresentation,
    hasSignedIntegerRepresentation,
    hasUnsignedIntegerRepresentation,
    hasFloatingRepresentation,
    getAsStructureType,
    getAsUnionType,
    getAsComplexIntegerType,
    getAsObjCInterfaceType,
    getAsObjCInterfacePointerType,
    getAsObjCQualifiedIdType,
    getAsObjCQualifiedClassType,
    getAsObjCQualifiedInterfaceType,
    getAsCXXRecordDecl,
    getAsRecordDecl,
    getAsTagDecl,
    getPointeeCXXRecordDecl,
    getContainedDeducedType,
    getContainedAutoType,
    hasAutoForTrailingReturnType,
    getAsArrayTypeUnsafe,
    castAsArrayTypeUnsafe,
    getBaseElementTypeUnsafe,
    getArrayElementTypeNoTypeQual,
    getPointeeOrArrayElementType,
    getPointeeType,
    getUnqualifiedDesugaredType,
    isPromotableIntegerType,
    isSignedIntegerType,
    isUnsignedIntegerType,
    isSignedIntegerOrEnumerationType,
    isUnsignedIntegerOrEnumerationType,
    isFixedPointType,
    isSaturatedFixedPointType,
    isUnsaturatedFixedPointType,
    isSignedFixedPointType,
    isUnsignedFixedPointType,
    isConstantSizeType,
    isSpecifierType,
    getLinkage,
    getVisibility,
    isVisibilityExplicit,
    getLinkageAndVisibility,
    isLinkageValid,
    canHaveNullability,
    acceptsObjCTypeParams,
    getTypeClassName,
    getCanonicalTypeInternal,
    dump,
    dump1,
  };
  static const unsigned _total_num_mems_ = dump1 + 1;
}
namespace clang__BuiltinType {
  enum memnames {
    getKind,
    getName,
    getNameAsCString,
    isSugared,
    desugar,
    isInteger,
    isSignedInteger,
    isUnsignedInteger,
    isFloatingPoint,
    isPlaceholderTypeKind,
    isPlaceholderType,
    isNonOverloadPlaceholderType,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ComplexType {
  enum memnames {
    getElementType,
    isSugared,
    desugar,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ParenType {
  enum memnames {
    getInnerType,
    isSugared,
    desugar,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__PointerType {
  enum memnames {
    getPointeeType,
    isAddressSpaceOverlapping,
    isSugared,
    desugar,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__AdjustedType {
  enum memnames {
    getOriginalType,
    getAdjustedType,
    isSugared,
    desugar,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DecayedType {
  enum memnames {
    getDecayedType,
    getPointeeType,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__BlockPointerType {
  enum memnames {
    getPointeeType,
    isSugared,
    desugar,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ReferenceType {
  enum memnames {
    isSpelledAsLValue,
    isInnerRef,
    getPointeeTypeAsWritten,
    getPointeeType,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__LValueReferenceType {
  enum memnames {
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__RValueReferenceType {
  enum memnames {
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__MemberPointerType {
  enum memnames {
    getPointeeType,
    isMemberFunctionPointer,
    isMemberDataPointer,
    getClass,
    getMostRecentCXXRecordDecl,
    isSugared,
    desugar,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ArrayType {
  enum memnames {
    getElementType,
    getSizeModifier,
    getIndexTypeQualifiers,
    getIndexTypeCVRQualifiers,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ConstantArrayType {
  enum memnames {
    getSize,
    isSugared,
    desugar,
    getNumAddressingBits,
    getMaxSizeBits,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__IncompleteArrayType {
  enum memnames {
    isSugared,
    desugar,
    classof,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__VariableArrayType {
  enum memnames {
    getSizeExpr,
    getBracketsRange,
    getLBracketLoc,
    getRBracketLoc,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DependentSizedArrayType {
  enum memnames {
    getSizeExpr,
    getBracketsRange,
    getLBracketLoc,
    getRBracketLoc,
    isSugared,
    desugar,
    classof,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__DependentAddressSpaceType {
  enum memnames {
    getAddrSpaceExpr,
    getPointeeType,
    getAttributeLoc,
    isSugared,
    desugar,
    classof,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__DependentSizedExtVectorType {
  enum memnames {
    getSizeExpr,
    getElementType,
    getAttributeLoc,
    isSugared,
    desugar,
    classof,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__VectorType {
  enum memnames {
    getElementType,
    getNumElements,
    isVectorSizeTooLarge,
    isSugared,
    desugar,
    getVectorKind,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DependentVectorType {
  enum memnames {
    getSizeExpr,
    getElementType,
    getAttributeLoc,
    getVectorKind,
    isSugared,
    desugar,
    classof,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__ExtVectorType {
  enum memnames {
    getPointAccessorIdx,
    getNumericAccessorIdx,
    getAccessorIdx,
    isAccessorWithinNumElements,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__FunctionType {
  enum memnames {
    getReturnType,
    getHasRegParm,
    getRegParmType,
    getNoReturnAttr,
    getCallConv,
    getExtInfo,
    isConst,
    isVolatile,
    isRestrict,
    getCallResultType,
    getNameForCallConv,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__FunctionType__ExtInfo {
  enum memnames {
    getNoReturn,
    getProducesResult,
    getNoCallerSavedRegs,
    getNoCfCheck,
    getHasRegParm,
    getRegParm,
    getCC,
    operator_eq_eq,
    operator_not_eq,
    withNoReturn,
    withProducesResult,
    withNoCallerSavedRegs,
    withNoCfCheck,
    withRegParm,
    withCallingConv,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__FunctionNoProtoType {
  enum memnames {
    isSugared,
    desugar,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__FunctionProtoType {
  enum memnames {
    getNumParams,
    getParamType,
    getParamTypes,
    getExtProtoInfo,
    getExceptionSpecType,
    hasExceptionSpec,
    hasDynamicExceptionSpec,
    hasNoexceptExceptionSpec,
    hasDependentExceptionSpec,
    hasInstantiationDependentExceptionSpec,
    getNumExceptions,
    getExceptionType,
    getNoexceptExpr,
    getExceptionSpecDecl,
    getExceptionSpecTemplate,
    canThrow,
    isNothrow,
    isVariadic,
    isTemplateVariadic,
    hasTrailingReturn,
    getTypeQuals,
    getRefQualifier,
    param_types,
    exceptions,
    hasExtParameterInfos,
    getExtParameterInfos,
    getExtParameterInfosOrNull,
    getExtParameterInfo,
    getParameterABI,
    isParamConsumed,
    isSugared,
    desugar,
    printExceptionSpecification,
    classof,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__FunctionProtoType__ExtParameterInfo {
  enum memnames {
    getABI,
    withABI,
    isConsumed,
    withIsConsumed,
    hasPassObjectSize,
    withHasPassObjectSize,
    isNoEscape,
    withIsNoEscape,
    getOpaqueValue,
    getFromOpaqueValue,
    operator_eq_eq,
    operator_not_eq,
  };
  static const unsigned _total_num_mems_ = operator_not_eq + 1;
}
namespace clang__FunctionProtoType__ExceptionSpecInfo {
  enum memnames {
    Type,
    NoexceptExpr,
    SourceDecl,
    SourceTemplate,
  };
  static const unsigned _total_num_mems_ = SourceTemplate + 1;
}
namespace clang__FunctionProtoType__ExtProtoInfo {
  enum memnames {
    ExtInfo,
    Variadic,
    HasTrailingReturn,
    TypeQuals,
    RefQualifier,
    ExceptionSpec,
    ExtParameterInfos,
  };
  static const unsigned _total_num_mems_ = ExtParameterInfos + 1;
}
namespace clang__UnresolvedUsingType {
  enum memnames {
    getDecl,
    isSugared,
    desugar,
    classof,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__TypedefType {
  enum memnames {
    getDecl,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__TypeOfExprType {
  enum memnames {
    getUnderlyingExpr,
    desugar,
    isSugared,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__TypeOfType {
  enum memnames {
    getUnderlyingType,
    desugar,
    isSugared,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DecltypeType {
  enum memnames {
    getUnderlyingExpr,
    getUnderlyingType,
    desugar,
    isSugared,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ReflectedType {
  enum memnames {
    getTypeReflection,
    getUnderlyingType,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__UnaryTransformType {
  enum memnames {
    isSugared,
    desugar,
    getUnderlyingType,
    getBaseType,
    getUTTKind,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__TagType {
  enum memnames {
    getDecl,
    isBeingDefined,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__RecordType {
  enum memnames {
    getDecl,
    hasConstFields,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__EnumType {
  enum memnames {
    getDecl,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__AttributedType {
  enum memnames {
    getAttrKind,
    getModifiedType,
    getEquivalentType,
    isSugared,
    desugar,
    isQualifier,
    isMSTypeSpec,
    isCallingConv,
    getNullabilityAttrKind,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__TemplateTypeParmType {
  enum memnames {
    getDepth,
    getIndex,
    isParameterPack,
    getDecl,
    getIdentifier,
    isSugared,
    desugar,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__SubstTemplateTypeParmType {
  enum memnames {
    getReplacedParameter,
    getReplacementType,
    isSugared,
    desugar,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__SubstTemplateTypeParmPackType {
  enum memnames {
    getIdentifier,
    getReplacedParameter,
    isSugared,
    desugar,
    getArgumentPack,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DeducedType {
  enum memnames {
    isSugared,
    desugar,
    getDeducedType,
    isDeduced,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__AutoType {
  enum memnames {
    isDecltypeAuto,
    getKeyword,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DeducedTemplateSpecializationType {
  enum memnames {
    getTemplateName,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__TemplateSpecializationType {
  enum memnames {
    _this_,
    isCurrentInstantiation,
    isTypeAlias,
    getAliasedType,
    getTemplateName,
    getArgs,
    getNumArgs,
    getArg,
    template_arguments,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__InjectedClassNameType {
  enum memnames {
    getInjectedSpecializationType,
    getInjectedTST,
    getTemplateName,
    getDecl,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__TypeWithKeyword {
  enum memnames {
    getKeyword,
    getKeywordForTypeSpec,
    getTagTypeKindForTypeSpec,
    getKeywordForTagTypeKind,
    getTagTypeKindForKeyword,
    KeywordIsTagTypeKind,
    getKeywordName,
    getTagTypeKindName,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ElaboratedType {
  enum memnames {
    getQualifier,
    getNamedType,
    desugar,
    isSugared,
    getOwnedTagDecl,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DependentNameType {
  enum memnames {
    getQualifier,
    getIdentifier,
    isSugared,
    desugar,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DependentTemplateSpecializationType {
  enum memnames {
    _this_,
    getQualifier,
    getIdentifier,
    getArgs,
    getNumArgs,
    getArg,
    template_arguments,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__PackExpansionType {
  enum memnames {
    getPattern,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCTypeParamType {
  enum memnames {
    quals,
    qual_empty,
    getNumProtocols,
    getProtocol,
    getProtocols,
    isSugared,
    desugar,
    classof,
    getDecl,
  };
  static const unsigned _total_num_mems_ = getDecl + 1;
}
namespace clang__ObjCObjectType {
  enum memnames {
    quals,
    qual_empty,
    getNumProtocols,
    getProtocol,
    getProtocols,
    getBaseType,
    isObjCId,
    isObjCClass,
    isObjCUnqualifiedId,
    isObjCUnqualifiedClass,
    isObjCUnqualifiedIdOrClass,
    isObjCQualifiedId,
    isObjCQualifiedClass,
    getInterface,
    isSpecialized,
    isSpecializedAsWritten,
    isUnspecialized,
    isUnspecializedAsWritten,
    getTypeArgs,
    getTypeArgsAsWritten,
    isKindOfTypeAsWritten,
    isKindOfType,
    getSuperClassType,
    stripObjCKindOfTypeAndQuals,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCInterfaceType {
  enum memnames {
    getDecl,
    isSugared,
    desugar,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCObjectPointerType {
  enum memnames {
    getPointeeType,
    getObjectType,
    getInterfaceType,
    getInterfaceDecl,
    isObjCIdType,
    isObjCClassType,
    isObjCIdOrClassType,
    isObjCQualifiedIdType,
    isObjCQualifiedClassType,
    isKindOfType,
    isSpecialized,
    isSpecializedAsWritten,
    isUnspecialized,
    isUnspecializedAsWritten,
    getTypeArgs,
    getTypeArgsAsWritten,
    quals,
    qual_empty,
    getNumProtocols,
    getProtocol,
    isSugared,
    desugar,
    getSuperClassType,
    stripObjCKindOfTypeAndQuals,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__AtomicType {
  enum memnames {
    getValueType,
    isSugared,
    desugar,
    Profile,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__PipeType {
  enum memnames {
    getElementType,
    isSugared,
    desugar,
    Profile,
    classof,
    isReadOnly,
  };
  static const unsigned _total_num_mems_ = isReadOnly + 1;
}
namespace clang__TypeSourceInfo {
  enum memnames {
    getType,
    getTypeLoc,
  };
  static const unsigned _total_num_mems_ = getTypeLoc + 1;
}
namespace clang__TranslationUnitDecl {
  enum memnames {
    getASTContext,
    getAnonymousNamespace,
    Create,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__PragmaCommentDecl {
  enum memnames {
    CreateDeserialized,
    getCommentKind,
    getArg,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__PragmaDetectMismatchDecl {
  enum memnames {
    CreateDeserialized,
    getName,
    getValue,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ExternCContextDecl {
  enum memnames {
    Create,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__NamedDecl {
  enum memnames {
    getIdentifier,
    getName,
    getNameAsString,
    printName,
    getDeclName,
    printQualifiedName,
    printQualifiedName1,
    getQualifiedNameAsString,
    getNameForDiagnostic,
    declarationReplaces,
    hasLinkage,
    isCXXClassMember,
    isCXXInstanceMember,
    getLinkageInternal,
    getFormalLinkage,
    hasExternalFormalLinkage,
    isExternallyVisible,
    isExternallyDeclarable,
    getVisibility,
    getLinkageAndVisibility,
    isLinkageValid,
    hasLinkageBeenComputed,
    getUnderlyingDecl,
    getMostRecentDecl,
    getObjCFStringFormattingFamily,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__LabelDecl {
  enum memnames {
    Create,
    Create1,
    CreateDeserialized,
    getStmt,
    isGnuLocal,
    getSourceRange,
    isMSAsmLabel,
    isResolvedMSAsmLabel,
    getMSAsmLabel,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__NamespaceDecl {
  enum memnames {
    getPreviousDecl,
    getFirstDecl,
    getMostRecentDecl,
    Create,
    CreateDeserialized,
    isAnonymousNamespace,
    isInline,
    getOriginalNamespace,
    isOriginalNamespace,
    getAnonymousNamespace,
    getCanonicalDecl,
    getSourceRange,
    getLocStart,
    getBeginLoc,
    getRBraceLoc,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ValueDecl {
  enum memnames {
    getType,
    isWeak,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__DeclaratorDecl {
  enum memnames {
    getTypeSourceInfo,
    getInnerLocStart,
    getOuterLocStart,
    getSourceRange,
    getLocStart,
    getBeginLoc,
    getQualifier,
    getQualifierLoc,
    getNumTemplateParameterLists,
    getTemplateParameterList,
    getTypeSpecStartLoc,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__EvaluatedStmt {
  enum memnames {
    WasEvaluated,
    IsEvaluating,
    CheckedICE,
    CheckingICE,
    IsICE,
    Value,
  };
  static const unsigned _total_num_mems_ = Value + 1;
}
namespace clang__VarDecl {
  enum memnames {
    getPreviousDecl,
    getFirstDecl,
    getMostRecentDecl,
    getStorageClassSpecifierString,
    Create,
    Create1,
    CreateDeserialized,
    getSourceRange,
    getStorageClass,
    getTSCSpec,
    getTLSKind,
    hasLocalStorage,
    isStaticLocal,
    hasExternalStorage,
    hasGlobalStorage,
    getStorageDuration,
    getLanguageLinkage,
    isExternC,
    isInExternCContext,
    isInExternCXXContext,
    isLocalVarDecl,
    isLocalVarDeclOrParm,
    isFunctionOrMethodVarDecl,
    isStaticDataMember,
    getCanonicalDecl,
    isThisDeclarationADefinition,
    isThisDeclarationADefinition1,
    hasDefinition,
    hasDefinition1,
    getActingDefinition,
    getDefinition,
    getDefinition1,
    isOutOfLine,
    isFileVarDecl,
    getAnyInitializer,
    getAnyInitializer1,
    hasInit,
    getInit,
    isUsableInConstantExpressions,
    ensureEvaluatedStmt,
    evaluateValue,
    getEvaluatedValue,
    isInitKnownICE,
    isInitICE,
    checkInitIsICE,
    getInitStyle,
    isDirectInit,
    isThisDeclarationADemotedDefinition,
    isExceptionVariable,
    isNRVOVariable,
    isCXXForRangeDecl,
    isObjCForDecl,
    isARCPseudoStrong,
    isInline,
    isInlineSpecified,
    isConstexpr,
    isInitCapture,
    isPreviousDeclInSameBlockScope,
    getTemplateInstantiationPattern,
    getInstantiatedFromStaticDataMember,
    getTemplateSpecializationKind,
    getPointOfInstantiation,
    getMemberSpecializationInfo,
    getDescribedVarTemplate,
    isKnownToBeDefined,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ImplicitParamDecl {
  enum memnames {
    Create,
    Create1,
    CreateDeserialized,
    getParameterKind,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ParmVarDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getSourceRange,
    isObjCMethodParameter,
    getFunctionScopeDepth,
    getFunctionScopeIndex,
    getObjCDeclQualifier,
    isKNRPromoted,
    getDefaultArg,
    getDefaultArgRange,
    getUninstantiatedDefaultArg,
    hasDefaultArg,
    hasUnparsedDefaultArg,
    hasUninstantiatedDefaultArg,
    hasInheritedDefaultArg,
    getOriginalType,
    isParameterPack,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__FunctionDecl {
  enum memnames {
    getPreviousDecl,
    getFirstDecl,
    getMostRecentDecl,
    Create,
    Create1,
    CreateDeserialized,
    getNameInfo,
    getNameForDiagnostic,
    getSourceRange,
    hasBody,
    hasBody1,
    hasTrivialBody,
    isDefined,
    isDefined1,
    getDefinition,
    getBody,
    getBody1,
    isThisDeclarationADefinition,
    doesThisDeclarationHaveABody,
    isVariadic,
    isVirtualAsWritten,
    isPure,
    isLateTemplateParsed,
    isTrivial,
    isTrivialForCall,
    isDefaulted,
    isExplicitlyDefaulted,
    hasImplicitReturnZero,
    hasPrototype,
    hasWrittenPrototype,
    hasInheritedPrototype,
    isConstexpr,
    instantiationIsPending,
    usesSEHTry,
    isDeleted,
    isDeletedAsWritten,
    isMain,
    isMSVCRTEntryPoint,
    isReservedGlobalPlacementOperator,
    isReplaceableGlobalAllocationFunction,
    isDestroyingOperatorDelete,
    getLanguageLinkage,
    isExternC,
    isInExternCContext,
    isInExternCXXContext,
    isGlobal,
    isNoReturn,
    hasSkippedBody,
    willHaveBody,
    isMetaprogram,
    isMultiVersion,
    isCPUDispatchMultiVersion,
    isCPUSpecificMultiVersion,
    getCanonicalDecl,
    getBuiltinID,
    parameters,
    param_empty,
    param_size,
    getNumParams,
    getParamDecl,
    getMinRequiredArguments,
    getReturnType,
    getReturnTypeSourceRange,
    getExceptionSpecSourceRange,
    getCallResultType,
    getUnusedResultAttr,
    hasUnusedResultAttr,
    getStorageClass,
    isInlineSpecified,
    isInlined,
    isInlineDefinitionExternallyVisible,
    isMSExternInline,
    doesDeclarationForceExternallyVisibleDefinition,
    isOverloadedOperator,
    getOverloadedOperator,
    getLiteralIdentifier,
    getInstantiatedFromMemberFunction,
    getTemplatedKind,
    getMemberSpecializationInfo,
    getDescribedFunctionTemplate,
    isFunctionTemplateSpecialization,
    getClassScopeSpecializationPattern,
    getTemplateSpecializationInfo,
    isImplicitlyInstantiable,
    isTemplateInstantiation,
    getTemplateInstantiationPattern,
    getPrimaryTemplate,
    getTemplateSpecializationArgs,
    getTemplateSpecializationArgsAsWritten,
    getDependentSpecializationInfo,
    getTemplateSpecializationKind,
    getPointOfInstantiation,
    isOutOfLine,
    getMemoryFunctionKind,
    getODRHash,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__FieldDecl {
  enum memnames {
    getFirstDecl,
    Create,
    Create1,
    CreateDeserialized,
    getFieldIndex,
    isMutable,
    isBitField,
    isUnnamedBitfield,
    isAnonymousStructOrUnion,
    getBitWidth,
    getBitWidthValue,
    isZeroLengthBitField,
    getInClassInitStyle,
    hasInClassInitializer,
    getInClassInitializer,
    hasCapturedVLAType,
    getCapturedVLAType,
    getParent,
    getSourceRange,
    getCanonicalDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__EnumConstantDecl {
  enum memnames {
    getFirstDecl,
    Create,
    CreateDeserialized,
    getInitExpr,
    getInitVal,
    getSourceRange,
    getCanonicalDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__IndirectFieldDecl {
  enum memnames {
    getFirstDecl,
    CreateDeserialized,
    chain,
    getChainingSize,
    getAnonField,
    getVarDecl,
    getCanonicalDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__TypeDecl {
  enum memnames {
    getTypeForDecl,
    getLocStart,
    getBeginLoc,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__TypedefNameDecl {
  enum memnames {
    getPreviousDecl,
    getFirstDecl,
    getMostRecentDecl,
    isModed,
    getTypeSourceInfo,
    getUnderlyingType,
    getCanonicalDecl,
    getAnonDeclWithTypedefName,
    isTransparentTag,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__TypedefDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__TypeAliasDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getSourceRange,
    getDescribedAliasTemplate,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__TagDecl {
  enum memnames {
    getPreviousDecl,
    getFirstDecl,
    getMostRecentDecl,
    getBraceRange,
    getInnerLocStart,
    getOuterLocStart,
    getSourceRange,
    getCanonicalDecl,
    isThisDeclarationADefinition,
    isCompleteDefinition,
    isCompleteDefinitionRequired,
    isBeingDefined,
    isEmbeddedInDeclarator,
    isFreeStanding,
    isDependentType,
    getDefinition,
    getKindName,
    getTagKind,
    isStruct,
    isInterface,
    isClass,
    isUnion,
    isEnum,
    hasNameForLinkage,
    getTypedefNameForAnonDecl,
    getQualifier,
    getQualifierLoc,
    getNumTemplateParameterLists,
    getTemplateParameterList,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__EnumDecl {
  enum memnames {
    getCanonicalDecl,
    getPreviousDecl,
    getMostRecentDecl,
    getDefinition,
    Create,
    CreateDeserialized,
    enumerators,
    getPromotionType,
    getIntegerType,
    getIntegerTypeSourceInfo,
    getIntegerTypeRange,
    getNumPositiveBits,
    getNumNegativeBits,
    isScoped,
    isScopedUsingClassTag,
    isFixed,
    isComplete,
    isClosed,
    isClosedFlag,
    isClosedNonFlag,
    getTemplateInstantiationPattern,
    getInstantiatedFromMemberEnum,
    getTemplateSpecializationKind,
    getMemberSpecializationInfo,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__RecordDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getPreviousDecl,
    getMostRecentDecl,
    hasFlexibleArrayMember,
    isAnonymousStructOrUnion,
    hasObjectMember,
    hasVolatileMember,
    hasLoadedFieldsFromExternalStorage,
    isNonTrivialToPrimitiveDefaultInitialize,
    isNonTrivialToPrimitiveCopy,
    isNonTrivialToPrimitiveDestroy,
    canPassInRegisters,
    getArgPassingRestrictions,
    isParamDestroyedInCallee,
    isInjectedClassName,
    isLambda,
    isCapturedRecord,
    getDefinition,
    fields,
    field_empty,
    classof,
    classofKind,
    isMsStruct,
    mayInsertExtraPadding,
    findFirstNamedDataMember,
  };
  static const unsigned _total_num_mems_ = findFirstNamedDataMember + 1;
}
namespace clang__FileScopeAsmDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getAsmLoc,
    getRParenLoc,
    getSourceRange,
    getAsmString,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__BlockDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getCaretLocation,
    isVariadic,
    getCompoundBody,
    getBody,
    getSignatureAsWritten,
    parameters,
    param_empty,
    param_size,
    getNumParams,
    getParamDecl,
    hasCaptures,
    getNumCaptures,
    captures,
    capturesCXXThis,
    blockMissingReturnType,
    isConversionFromLambda,
    doesNotEscape,
    capturesVariable,
    getBlockManglingNumber,
    getBlockManglingContextDecl,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__BlockDecl__Capture {
  enum memnames {
    getVariable,
    isByRef,
    isNested,
    hasCopyExpr,
    getCopyExpr,
  };
  static const unsigned _total_num_mems_ = getCopyExpr + 1;
}
namespace clang__CapturedDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getBody,
    isNothrow,
    getNumParams,
    getParam,
    parameters,
    getContextParam,
    getContextParamPosition,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ImportDecl {
  enum memnames {
    CreateImplicit,
    CreateDeserialized,
    getImportedModule,
    getIdentifierLocs,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ExportDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getExportLoc,
    getRBraceLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__EmptyDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__DeclGroup {
  enum memnames {
    Create,
    size,
    operator_sub,
  };
  static const unsigned _total_num_mems_ = operator_sub + 1;
}
namespace clang__DeclGroupRef {
  enum memnames {
    _this_,
    Create,
    isNull,
    isSingleDecl,
    isDeclGroup,
    getSingleDecl,
    getDeclGroup,
    getAsOpaquePtr,
    getFromOpaquePtr,
  };
  static const unsigned _total_num_mems_ = getFromOpaquePtr + 1;
}
namespace clang__Stmt {
  enum memnames {
    operator_new,
    operator_new1,
    operator_new2,
    operator_delete,
    operator_delete1,
    operator_delete2,
    operator_delete3,
    getStmtClass,
    getStmtClassName,
    getSourceRange,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    addStmtClass,
    EnableStatistics,
    PrintStats,
    dump,
    dump1,
    dump2,
    dump3,
    dumpColor,
    dumpPretty,
    printPretty,
    viewAST,
    IgnoreImplicit,
    IgnoreContainers,
    stripLabelLikeStatements,
    children,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__Stmt__EmptyShell {
  enum memnames {
  };
  static const unsigned _total_num_mems_ = 0;
}
namespace clang__DeclStmt {
  enum memnames {
    isSingleDecl,
    getSingleDecl,
    getDeclGroup,
    getStartLoc,
    getEndLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    classof,
    decls,
  };
  static const unsigned _total_num_mems_ = decls + 1;
}
namespace clang__NullStmt {
  enum memnames {
    getSemiLoc,
    hasLeadingEmptyMacro,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CompoundStmt {
  enum memnames {
    CreateEmpty,
    body_empty,
    size,
    body,
    body_front,
    body_back,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getLBracLoc,
    getRBracLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__SwitchCase {
  enum memnames {
    getNextSwitchCase,
    getKeywordLoc,
    getColonLoc,
    getSubStmt,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CaseStmt {
  enum memnames {
    getCaseLoc,
    getEllipsisLoc,
    getColonLoc,
    getLHS,
    getRHS,
    getSubStmt,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DefaultStmt {
  enum memnames {
    getSubStmt,
    getDefaultLoc,
    getColonLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__LabelStmt {
  enum memnames {
    getIdentLoc,
    getDecl,
    getName,
    getSubStmt,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__AttributedStmt {
  enum memnames {
    CreateEmpty,
    getAttrLoc,
    getAttrs,
    getSubStmt,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__IfStmt {
  enum memnames {
    getConditionVariable,
    getConditionVariableDeclStmt,
    getInit,
    getCond,
    getThen,
    getElse,
    getIfLoc,
    getElseLoc,
    isConstexpr,
    isObjCAvailabilityCheck,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__SwitchStmt {
  enum memnames {
    getConditionVariable,
    getConditionVariableDeclStmt,
    getInit,
    getCond,
    getBody,
    getSwitchCaseList,
    getSwitchLoc,
    isAllEnumCasesCovered,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__WhileStmt {
  enum memnames {
    getConditionVariable,
    getConditionVariableDeclStmt,
    getCond,
    getBody,
    getWhileLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DoStmt {
  enum memnames {
    getCond,
    getBody,
    getDoLoc,
    getWhileLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ForStmt {
  enum memnames {
    getConditionVariable,
    getConditionVariableDeclStmt,
    getInit,
    getCond,
    getInc,
    getBody,
    getForLoc,
    getLParenLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__GotoStmt {
  enum memnames {
    getLabel,
    getGotoLoc,
    getLabelLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__IndirectGotoStmt {
  enum memnames {
    getGotoLoc,
    getStarLoc,
    getTarget,
    getConstantTarget,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ContinueStmt {
  enum memnames {
    getContinueLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__BreakStmt {
  enum memnames {
    getBreakLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ReturnStmt {
  enum memnames {
    getRetValue,
    getReturnLoc,
    getNRVOCandidate,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__AsmStmt {
  enum memnames {
    getAsmLoc,
    isSimple,
    isVolatile,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    generateAsmString,
    getNumOutputs,
    getOutputConstraint,
    isOutputPlusConstraint,
    getOutputExpr,
    getNumPlusOperands,
    getNumInputs,
    getInputConstraint,
    getInputExpr,
    getNumClobbers,
    getClobber,
    classof,
    inputs,
    outputs,
  };
  static const unsigned _total_num_mems_ = outputs + 1;
}
namespace clang__GCCAsmStmt {
  enum memnames {
    getRParenLoc,
    getAsmString,
    generateAsmString,
    getOutputIdentifier,
    getOutputName,
    getOutputConstraint,
    getOutputConstraintLiteral,
    getOutputExpr,
    getInputIdentifier,
    getInputName,
    getInputConstraint,
    getInputConstraintLiteral,
    getInputExpr,
    getNamedOperand,
    getClobber,
    getClobberStringLiteral,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__MSAsmStmt {
  enum memnames {
    getLBraceLoc,
    getEndLoc,
    hasBraces,
    getAsmString,
    generateAsmString,
    getOutputConstraint,
    getOutputExpr,
    getInputConstraint,
    getInputExpr,
    getAllConstraints,
    getClobbers,
    getAllExprs,
    getClobber,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__SEHExceptStmt {
  enum memnames {
    Create,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getExceptLoc,
    getEndLoc,
    getFilterExpr,
    getBlock,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__SEHFinallyStmt {
  enum memnames {
    Create,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getFinallyLoc,
    getEndLoc,
    getBlock,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__SEHTryStmt {
  enum memnames {
    Create,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getTryLoc,
    getEndLoc,
    getIsCXXTry,
    getTryBlock,
    getHandler,
    getExceptHandler,
    getFinallyHandler,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__SEHLeaveStmt {
  enum memnames {
    getLeaveLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CapturedStmt {
  enum memnames {
    CreateDeserialized,
    getCapturedStmt,
    getCapturedDecl,
    getCapturedRegionKind,
    getCapturedRecordDecl,
    capturesVariable,
    captures,
    capture_size,
    capture_inits,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CapturedStmt__Capture {
  enum memnames {
    getCaptureKind,
    getLocation,
    capturesThis,
    capturesVariable,
    capturesVariableByCopy,
    capturesVariableArrayType,
    getCapturedVar,
  };
  static const unsigned _total_num_mems_ = getCapturedVar + 1;
}
namespace clang__TemplateArgument {
  enum memnames {
    getEmptyPack,
    getKind,
    isNull,
    isDependent,
    isInstantiationDependent,
    containsUnexpandedParameterPack,
    isPackExpansion,
    getAsType,
    getAsDecl,
    getParamTypeForDecl,
    getNullPtrType,
    getAsTemplate,
    getAsTemplateOrTemplatePattern,
    getAsIntegral,
    getIntegralType,
    getNonTypeTemplateArgumentType,
    getAsExpr,
    pack_elements,
    pack_size,
    getPackAsArray,
    structurallyEquals,
    getPackExpansionPattern,
    print,
    dump,
    dump1,
    Profile,
  };
  static const unsigned _total_num_mems_ = Profile + 1;
}
namespace clang__TemplateArgumentLocInfo {
  enum memnames {
    getAsTypeSourceInfo,
    getAsExpr,
    getTemplateQualifierLoc,
    getTemplateNameLoc,
    getTemplateEllipsisLoc,
  };
  static const unsigned _total_num_mems_ = getTemplateEllipsisLoc + 1;
}
namespace clang__TemplateArgumentLoc {
  enum memnames {
    getLocation,
    getSourceRange,
    getArgument,
    getLocInfo,
    getTypeSourceInfo,
    getSourceExpression,
    getSourceDeclExpression,
    getSourceNullPtrExpression,
    getSourceIntegralExpression,
    getTemplateQualifierLoc,
    getTemplateNameLoc,
    getTemplateEllipsisLoc,
  };
  static const unsigned _total_num_mems_ = getTemplateEllipsisLoc + 1;
}
namespace clang__TemplateArgumentListInfo {
  enum memnames {
    getLAngleLoc,
    getRAngleLoc,
    size,
    getArgumentArray,
    arguments,
    operator_sub,
  };
  static const unsigned _total_num_mems_ = operator_sub + 1;
}
namespace clang__ASTTemplateArgumentListInfo {
  enum memnames {
    LAngleLoc,
    RAngleLoc,
    NumTemplateArgs,
    getTemplateArgs,
    arguments,
    operator_sub,
    Create,
  };
  static const unsigned _total_num_mems_ = Create + 1;
}
namespace clang__TypeLoc {
  enum memnames {
    getTypeLocClass,
    isNull,
    operator_bool,
    getFullDataSizeForType,
    getLocalAlignmentForType,
    getType,
    getTypePtr,
    getOpaqueData,
    getBeginLoc,
    getEndLoc,
    getSourceRange,
    getLocStart,
    getLocEnd,
    getLocalSourceRange,
    getFullDataSize,
    getNextTypeLoc,
    getUnqualifiedLoc,
    IgnoreParens,
    findExplicitQualifierLoc,
    initialize,
    operator_eq_eq,
    operator_not_eq,
    findNullabilityLoc,
  };
  static const unsigned _total_num_mems_ = findNullabilityLoc + 1;
}
namespace clang__UnqualTypeLoc {
  enum memnames {
    getTypePtr,
    getTypeLocClass,
  };
  static const unsigned _total_num_mems_ = getTypeLocClass + 1;
}
namespace clang__CommentOptions {
  enum memnames {
    ParseAllComments,
  };
  static const unsigned _total_num_mems_ = ParseAllComments + 1;
}
namespace clang__comments__CommandInfo {
  enum memnames {
    getID,
    Name,
    EndCommandName,
    ID,
    NumArgs,
    IsInlineCommand,
    IsBlockCommand,
    IsBriefCommand,
    IsReturnsCommand,
    IsParamCommand,
    IsTParamCommand,
    IsThrowsCommand,
    IsDeprecatedCommand,
    IsHeaderfileCommand,
    IsEmptyParagraphAllowed,
    IsVerbatimBlockCommand,
    IsVerbatimBlockEndCommand,
    IsVerbatimLineCommand,
    IsDeclarationCommand,
    IsFunctionDeclarationCommand,
    IsRecordLikeDetailCommand,
    IsRecordLikeDeclarationCommand,
    IsUnknownCommand,
  };
  static const unsigned _total_num_mems_ = IsUnknownCommand + 1;
}
namespace clang__comments__CommandTraits {
  enum memnames {
    getCommandInfo,
    getBuiltinCommandInfo,
  };
  static const unsigned _total_num_mems_ = getBuiltinCommandInfo + 1;
}
namespace llvm__Triple {
  enum memnames {
    operator_eq_eq,
    operator_not_eq,
    normalize,
    normalize1,
    getArch,
    getSubArch,
    getVendor,
    getOS,
    hasEnvironment,
    getEnvironment,
    getObjectFormat,
    getOSMajorVersion,
    getArchName,
    getVendorName,
    getOSName,
    getEnvironmentName,
    getOSAndEnvironmentName,
    isArch64Bit,
    isArch32Bit,
    isArch16Bit,
    isOSVersionLT,
    isOSVersionLT1,
    isMacOSXVersionLT,
    isMacOSX,
    isiOS,
    isTvOS,
    isWatchOS,
    isWatchABI,
    isOSDarwin,
    isSimulatorEnvironment,
    isOSNetBSD,
    isOSOpenBSD,
    isOSFreeBSD,
    isOSFuchsia,
    isOSDragonFly,
    isOSSolaris,
    isOSIAMCU,
    isOSUnknown,
    isGNUEnvironment,
    isOSContiki,
    isOSHaiku,
    isWindowsMSVCEnvironment,
    isKnownWindowsMSVCEnvironment,
    isWindowsCoreCLREnvironment,
    isWindowsItaniumEnvironment,
    isWindowsCygwinEnvironment,
    isWindowsGNUEnvironment,
    isOSCygMing,
    isOSMSVCRT,
    isOSWindows,
    isOSNaCl,
    isOSLinux,
    isOSKFreeBSD,
    isOSGlibc,
    isOSBinFormatELF,
    isOSBinFormatCOFF,
    isOSBinFormatMachO,
    isOSBinFormatWasm,
    isPS4CPU,
    isPS4,
    isAndroid,
    isAndroidVersionLT,
    isMusl,
    isNVPTX,
    isThumb,
    isARM,
    isAArch64,
    isMIPS32,
    isMIPS64,
    isMIPS,
    supportsCOMDAT,
    hasDefaultEmulatedTLS,
    get32BitArchVariant,
    get64BitArchVariant,
    getBigEndianArchVariant,
    getLittleEndianArchVariant,
    getARMCPUForArch,
    isLittleEndian,
    isCompatibleWith,
    merge,
    getArchTypeName,
    getArchTypePrefix,
    getVendorTypeName,
    getOSTypeName,
    getEnvironmentTypeName,
    getArchTypeForLLVMName,
  };
  static const unsigned _total_num_mems_ = getArchTypeForLLVMName + 1;
}
namespace clang__ObjCRuntime {
  enum memnames {
    getKind,
    getVersion,
    isNonFragile,
    isFragile,
    isGNUFamily,
    isNeXTFamily,
    allowsARC,
    hasNativeARC,
    hasOptimizedSetter,
    allowsWeak,
    hasNativeWeak,
    hasSubscripting,
    allowsSizeofAlignof,
    allowsPointerArithmetic,
    isSubscriptPointerArithmetic,
    hasTerminate,
    hasWeakClassImport,
    hasUnwindExceptions,
    hasAtomicCopyHelper,
    hasARCUnsafeClaimAutoreleasedReturnValue,
    hasEmptyCollections,
    getAsString,
    operator_eq_eq,
    operator_not_eq,
  };
  static const unsigned _total_num_mems_ = operator_not_eq + 1;
}
namespace clang__SanitizerSet {
  enum memnames {
    has,
    hasOneOf,
    empty,
    Mask,
  };
  static const unsigned _total_num_mems_ = Mask + 1;
}
namespace clang__LangOptionsBase {
  enum memnames {
    C99,
    C11,
    C17,
    MSVCCompat,
    MicrosoftExt,
    AsmBlocks,
    Borland,
    CPlusPlus,
    CPlusPlus11,
    CPlusPlus14,
    CPlusPlus17,
    CPlusPlus2a,
    ObjC1,
    ObjC2,
    ObjCDefaultSynthProperties,
    EncodeExtendedBlockSig,
    ObjCInferRelatedResultType,
    AppExt,
    Trigraphs,
    LineComment,
    Bool,
    Half,
    WChar,
    Char8,
    DeclSpecKeyword,
    DollarIdents,
    AsmPreprocessor,
    GNUMode,
    GNUKeywords,
    ImplicitInt,
    Digraphs,
    HexFloats,
    CXXOperatorNames,
    AppleKext,
    PascalStrings,
    WritableStrings,
    ConstStrings,
    LaxVectorConversions,
    AltiVec,
    ZVector,
    Exceptions,
    ObjCExceptions,
    CXXExceptions,
    DWARFExceptions,
    SjLjExceptions,
    SEHExceptions,
    ExternCNoUnwind,
    TraditionalCPP,
    RTTI,
    RTTIData,
    MSBitfields,
    Freestanding,
    NoBuiltin,
    NoMathBuiltin,
    GNUAsm,
    CoroutinesTS,
    RelaxedTemplateTemplateArgs,
    DoubleSquareBracketAttributes,
    ThreadsafeStatics,
    POSIXThreads,
    Blocks,
    EmitAllDecls,
    MathErrno,
    HeinousExtensions,
    Modules,
    ModulesTS,
    CompilingPCH,
    BuildingPCHWithObjectFile,
    ModulesDeclUse,
    ModulesSearchAll,
    ModulesStrictDeclUse,
    ModulesErrorRecovery,
    ImplicitModules,
    ModulesLocalVisibility,
    Optimize,
    OptimizeSize,
    Static,
    PackStruct,
    MaxTypeAlign,
    AlignDouble,
    PICLevel,
    PIE,
    GNUInline,
    NoInlineDefine,
    Deprecated,
    FastMath,
    FiniteMathOnly,
    UnsafeFPMath,
    ObjCGCBitmapPrint,
    AccessControl,
    CharIsSigned,
    WCharSize,
    WCharIsSigned,
    ShortEnums,
    OpenCL,
    OpenCLVersion,
    OpenCLCPlusPlus,
    OpenCLCPlusPlusVersion,
    NativeHalfType,
    NativeHalfArgsAndReturns,
    HalfArgsAndReturns,
    CUDA,
    HIP,
    OpenMP,
    OpenMPSimd,
    OpenMPUseTLS,
    OpenMPIsDevice,
    OpenMPCUDAMode,
    OpenMPHostCXXExceptions,
    RenderScript,
    CUDAIsDevice,
    CUDAAllowVariadicFunctions,
    CUDAHostDeviceConstexpr,
    CUDADeviceApproxTranscendentals,
    CUDARelocatableDeviceCode,
    SizedDeallocation,
    AlignedAllocation,
    AlignedAllocationUnavailable,
    NewAlignOverride,
    ConceptsTS,
    Reflection,
    ModulesCodegen,
    ModulesDebugInfo,
    ElideConstructors,
    DumpRecordLayouts,
    DumpRecordLayoutsSimple,
    DumpVTableLayouts,
    NoConstantCFStrings,
    InlineVisibilityHidden,
    ParseUnknownAnytype,
    DebuggerSupport,
    DebuggerCastResultToId,
    DebuggerObjCLiteral,
    SpellChecking,
    SinglePrecisionConstants,
    FastRelaxedMath,
    NoBitFieldTypeAlign,
    HexagonQdsp6Compat,
    ObjCAutoRefCount,
    ObjCWeakRuntime,
    ObjCWeak,
    ObjCSubscriptingLegacyRuntime,
    CFProtectionBranch,
    FakeAddressSpaceMap,
    IncludeDefaultHeader,
    DelayedTemplateParsing,
    BlocksRuntimeOptional,
    CompleteMemberPointers,
    ArrowDepth,
    InstantiationDepth,
    ConstexprCallDepth,
    ConstexprStepLimit,
    BracketDepth,
    NumLargeByValueCopy,
    MSCompatibilityVersion,
    VtorDispMode,
    ApplePragmaPack,
    RetainCommentsFromSystemHeaders,
    SanitizeAddressFieldPadding,
    XRayInstrument,
    XRayAlwaysEmitCustomEvents,
    XRayAlwaysEmitTypedEvents,
    ForceEmitVTables,
    AllowEditorPlaceholders,
    FunctionAlignment,
    FixedPoint,
    PaddingOnUnsignedFixedPoint,
  };
  static const unsigned _total_num_mems_ = PaddingOnUnsignedFixedPoint + 1;
}
namespace clang__LangOptions {
  enum memnames {
    Sanitize,
    ObjCRuntime,
    CommentOpts,
    IsHeaderFile,
    getCompilingModule,
    getMSPointerToMemberRepresentationMethod,
    getDefaultCallingConv,
    getDefaultFPContractMode,
    getAddressSpaceMapMangling,
    getGC,
    getValueVisibilityMode,
    getTypeVisibilityMode,
    getStackProtector,
    getSignedOverflowBehavior,
    getClangABICompat,
    isCompilingModule,
    trackLocalOwningModule,
    isSignedOverflowDefined,
    isSubscriptPointerArithmetic,
    isCompatibleWithMSVC,
    isNoBuiltinFunc,
    allowsNonTrivialObjCLifetimeQualifiers,
    assumeFunctionsAreConvergent,
    getOpenCLVersionTuple,
  };
  static const unsigned _total_num_mems_ = getOpenCLVersionTuple + 1;
}
namespace clang__FPOptions {
  enum memnames {
    allowFPContractWithinStatement,
    allowFPContractAcrossStatement,
    getInt,
  };
  static const unsigned _total_num_mems_ = getInt + 1;
}
namespace clang__PrinterHelper {
  enum memnames {
  };
  static const unsigned _total_num_mems_ = 0;
}
namespace clang__PrintingPolicy {
  enum memnames {
    Indentation,
    SuppressSpecifiers,
    SuppressTagKeyword,
    IncludeTagDefinition,
    SuppressScope,
    SuppressUnwrittenScope,
    SuppressInitializers,
    ConstantArraySizeAsWritten,
    AnonymousTagLocations,
    SuppressStrongLifetime,
    SuppressLifetimeQualifiers,
    SuppressTemplateArgsInCXXConstructors,
    Bool,
    Restrict,
    Alignof,
    UnderscoreAlignof,
    UseVoidForZeroParams,
    TerseOutput,
    PolishForDeclaration,
    Half,
    MSWChar,
    IncludeNewlines,
    MSVCFormatting,
    ConstantsAsWritten,
    SuppressImplicitBase,
    FullyQualifiedName,
  };
  static const unsigned _total_num_mems_ = FullyQualifiedName + 1;
}
namespace clang__SrcMgr__ContentCache {
  enum memnames {
    OrigEntry,
    ContentsEntry,
    SourceLineCache,
    NumLines,
    BufferOverridden,
    IsSystemFile,
    IsTransient,
    getBuffer,
    getSize,
    getSizeBytesMapped,
    getMemoryBufferKind,
    getRawBuffer,
    isBufferInvalid,
    shouldFreeBuffer,
  };
  static const unsigned _total_num_mems_ = shouldFreeBuffer + 1;
}
namespace clang__SrcMgr__FileInfo {
  enum memnames {
    get,
    getIncludeLoc,
    getContentCache,
    getFileCharacteristic,
    hasLineDirectives,
  };
  static const unsigned _total_num_mems_ = hasLineDirectives + 1;
}
namespace clang__SrcMgr__ExpansionInfo {
  enum memnames {
    getSpellingLoc,
    getExpansionLocStart,
    getExpansionLocEnd,
    isExpansionTokenRange,
    getExpansionLocRange,
    isMacroArgExpansion,
    isMacroBodyExpansion,
    isFunctionMacroExpansion,
    create,
    createForMacroArg,
    createForTokenSplit,
  };
  static const unsigned _total_num_mems_ = createForTokenSplit + 1;
}
namespace clang__SrcMgr__SLocEntry {
  enum memnames {
    getOffset,
    isExpansion,
    isFile,
    getFile,
    getExpansion,
    get,
    get1,
  };
  static const unsigned _total_num_mems_ = get1 + 1;
}
namespace clang__SourceManager {
  enum memnames {
    Retain,
    Release,
    getDiagnostics,
    getFileManager,
    userFilesAreVolatile,
    getMainFileID,
    getPreambleFileID,
    isFileOverridden,
    getBuffer,
    getBuffer1,
    getFileEntryForID,
    getFileEntryForSLocEntry,
    getBufferData,
    getNumCreatedFIDsForFileID,
    setNumCreatedFIDsForFileID,
    getFileID,
    getFilename,
    getLocForStartOfFile,
    getLocForEndOfFile,
    getIncludeLoc,
    getExpansionLoc,
    getFileLoc,
    getImmediateExpansionRange,
    getExpansionRange,
    getExpansionRange1,
    getExpansionRange2,
    getSpellingLoc,
    getImmediateSpellingLoc,
    getComposedLoc,
    getFileOffset,
    isMacroArgExpansion,
    isMacroBodyExpansion,
    isAtStartOfImmediateMacroExpansion,
    isAtEndOfImmediateMacroExpansion,
    isInSLocAddrSpace,
    isInSameSLocAddrSpace,
    getCharacterData,
    getColumnNumber,
    getSpellingColumnNumber,
    getExpansionColumnNumber,
    getPresumedColumnNumber,
    getLineNumber,
    getSpellingLineNumber,
    getExpansionLineNumber,
    getPresumedLineNumber,
    getBufferName,
    getFileCharacteristic,
    getPresumedLoc,
    isInMainFile,
    isWrittenInSameFile,
    isWrittenInMainFile,
    isInSystemHeader,
    isInExternCSystemHeader,
    isInSystemMacro,
    getFileIDSize,
    isInFileID,
    hasLineTable,
    getContentCacheSize,
    getDataStructureSizes,
    translateFileLineCol,
    translateFile,
    translateLineCol,
    getMacroArgExpandedLocation,
    isBeforeInTranslationUnit,
    isBeforeInSLocAddrSpace,
    isBeforeInSLocAddrSpace1,
    isPointWithin,
    hasFileInfo,
    PrintStats,
    dump,
    local_sloc_entry_size,
    getLocalSLocEntry,
    loaded_sloc_entry_size,
    getLoadedSLocEntry,
    getSLocEntry,
    getNextLocalOffset,
    isLoadedSourceLocation,
    isLocalSourceLocation,
    isLoadedFileID,
    isLocalFileID,
    getImmediateMacroCallerLoc,
    getTopMacroCallerLoc,
  };
  static const unsigned _total_num_mems_ = getTopMacroCallerLoc + 1;
}
namespace clang__RawComment {
  enum memnames {
    getKind,
    isInvalid,
    isMerged,
    isAttached,
    isTrailingComment,
    isAlmostTrailingComment,
    isOrdinary,
    isDocumentation,
    getRawText,
    getSourceRange,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getBriefText,
    getFormattedText,
  };
  static const unsigned _total_num_mems_ = getFormattedText + 1;
}
namespace clang__SanitizerBlacklist {
  enum memnames {
    isBlacklistedGlobal,
    isBlacklistedType,
    isBlacklistedFunction,
    isBlacklistedFile,
    isBlacklistedLocation,
  };
  static const unsigned _total_num_mems_ = isBlacklistedLocation + 1;
}
namespace clang__TargetCXXABI {
  enum memnames {
    getKind,
    isItaniumFamily,
    isMicrosoft,
    areMemberFunctionsAligned,
    areArgsDestroyedLeftToRightInCallee,
    hasConstructorVariants,
    hasPrimaryVBases,
    hasKeyFunctions,
    canKeyFunctionBeInline,
    getTailPaddingUseRules,
    operator_eq_eq,
    operator_not_eq,
  };
  static const unsigned _total_num_mems_ = operator_not_eq + 1;
}
namespace clang__OpenCLOptions {
  enum memnames {
    isKnown,
    isEnabled,
    isSupported,
    isSupportedCore,
    isSupportedExtension,
  };
  static const unsigned _total_num_mems_ = isSupportedExtension + 1;
}
namespace clang__TargetOptions {
  enum memnames {
    EABIVersion,
    SupportedOpenCLOptions,
    ForceEnableInt128,
    NVPTXUseShortPointers,
  };
  static const unsigned _total_num_mems_ = NVPTXUseShortPointers + 1;
}
namespace llvm__Type {
  enum memnames {
    print,
    dump,
    getTypeID,
    isVoidTy,
    isHalfTy,
    isFloatTy,
    isDoubleTy,
    isX86_FP80Ty,
    isFP128Ty,
    isPPC_FP128Ty,
    isFloatingPointTy,
    isX86_MMXTy,
    isFPOrFPVectorTy,
    isLabelTy,
    isMetadataTy,
    isTokenTy,
    isIntegerTy,
    isIntegerTy1,
    isIntOrIntVectorTy,
    isIntOrIntVectorTy1,
    isIntOrPtrTy,
    isFunctionTy,
    isStructTy,
    isArrayTy,
    isPointerTy,
    isPtrOrPtrVectorTy,
    isVectorTy,
    canLosslesslyBitCastTo,
    isEmptyTy,
    isFirstClassType,
    isSingleValueType,
    isAggregateType,
    getPrimitiveSizeInBits,
    getScalarSizeInBits,
    getFPMantissaWidth,
    getScalarType,
    subtypes,
    getContainedType,
    getNumContainedTypes,
    getIntegerBitWidth,
    getFunctionParamType,
    getFunctionNumParams,
    isFunctionVarArg,
    getStructName,
    getStructNumElements,
    getStructElementType,
    getSequentialElementType,
    getArrayNumElements,
    getArrayElementType,
    getVectorNumElements,
    getVectorElementType,
    getPointerElementType,
    getPointerAddressSpace,
    getPointerTo,
  };
  static const unsigned _total_num_mems_ = getPointerTo + 1;
}
namespace llvm__IntegerType {
  enum memnames {
    getBitWidth,
    getBitMask,
    getSignBit,
    isPowerOf2ByteWidth,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace llvm__CompositeType {
  enum memnames {
    getTypeAtIndex,
    indexValid,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace llvm__StructType {
  enum memnames {
    isPacked,
    isLiteral,
    isOpaque,
    hasName,
    getName,
    isValidElementType,
    elements,
    isLayoutIdentical,
    getNumElements,
    getElementType,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace llvm__PointerType {
  enum memnames {
    get,
    getUnqual,
    getElementType,
    isValidElementType,
    isLoadableOrStorableType,
    getAddressSpace,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace llvm__DataLayout {
  enum memnames {
    operator_eq_eq,
    operator_not_eq,
    isLittleEndian,
    isBigEndian,
    isDefault,
    isLegalInteger,
    isIllegalInteger,
    exceedsNaturalStackAlignment,
    getStackAlignment,
    getAllocaAddrSpace,
    getProgramAddressSpace,
    hasMicrosoftFastStdCallMangling,
    doNotMangleLeadingQuestionMark,
    hasLinkerPrivateGlobalPrefix,
    getLinkerPrivateGlobalPrefix,
    getGlobalPrefix,
    getPrivateGlobalPrefix,
    getManglingComponent,
    fitsInLegalInteger,
    getPointerABIAlignment,
    getPointerPrefAlignment,
    getPointerSize,
    getIndexSize,
    getNonIntegralAddressSpaces,
    isNonIntegralPointerType,
    isNonIntegralPointerType1,
    getPointerSizeInBits,
    getIndexSizeInBits,
    getPointerTypeSizeInBits,
    getIndexTypeSizeInBits,
    getPointerTypeSize,
    getTypeSizeInBits,
    getTypeStoreSize,
    getTypeStoreSizeInBits,
    getTypeAllocSize,
    getTypeAllocSizeInBits,
    getABITypeAlignment,
    getABIIntegerTypeAlignment,
    getPrefTypeAlignment,
    getPreferredTypeAlignmentShift,
    getIntPtrType,
    getLargestLegalIntTypeSizeInBits,
    getIndexType,
    getStructLayout,
  };
  static const unsigned _total_num_mems_ = getStructLayout + 1;
}
namespace llvm__StructLayout {
  enum memnames {
    getSizeInBytes,
    getSizeInBits,
    getAlignment,
    hasPadding,
    getElementContainingOffset,
    getElementOffset,
    getElementOffsetInBits,
  };
  static const unsigned _total_num_mems_ = getElementOffsetInBits + 1;
}
namespace clang__TargetInfo {
  enum memnames {
    Retain,
    Release,
    getTargetOpts,
    getSizeType,
    getSignedSizeType,
    getIntMaxType,
    getUIntMaxType,
    getPtrDiffType,
    getUnsignedPtrDiffType,
    getIntPtrType,
    getUIntPtrType,
    getWCharType,
    getWIntType,
    getChar16Type,
    getChar32Type,
    getInt64Type,
    getUInt64Type,
    getSigAtomicType,
    getProcessIDType,
    getCorrespondingUnsignedType,
    getTypeWidth,
    getIntTypeByWidth,
    getLeastIntTypeByWidth,
    getRealTypeByWidth,
    getTypeAlign,
    isTypeSigned,
    getPointerWidth,
    getPointerAlign,
    getMaxPointerWidth,
    getNullPointerValue,
    getBoolWidth,
    getBoolAlign,
    getCharWidth,
    getCharAlign,
    getShortWidth,
    getShortAlign,
    getIntWidth,
    getIntAlign,
    getLongWidth,
    getLongAlign,
    getLongLongWidth,
    getLongLongAlign,
    getShortAccumWidth,
    getShortAccumAlign,
    getAccumWidth,
    getAccumAlign,
    getLongAccumWidth,
    getLongAccumAlign,
    getShortFractWidth,
    getShortFractAlign,
    getFractWidth,
    getFractAlign,
    getLongFractWidth,
    getLongFractAlign,
    getShortAccumScale,
    getShortAccumIBits,
    getAccumScale,
    getAccumIBits,
    getLongAccumScale,
    getLongAccumIBits,
    getUnsignedShortAccumScale,
    getUnsignedShortAccumIBits,
    getUnsignedAccumScale,
    getUnsignedAccumIBits,
    getUnsignedLongAccumScale,
    getUnsignedLongAccumIBits,
    getShortFractScale,
    getFractScale,
    getLongFractScale,
    getUnsignedShortFractScale,
    getUnsignedFractScale,
    getUnsignedLongFractScale,
    hasInt128Type,
    hasLegalHalfType,
    hasFloat128Type,
    getSuitableAlign,
    getDefaultAlignForAttributeAligned,
    getMinGlobalAlign,
    getNewAlign,
    getWCharWidth,
    getWCharAlign,
    getChar16Width,
    getChar16Align,
    getChar32Width,
    getChar32Align,
    getHalfWidth,
    getHalfAlign,
    getFloatWidth,
    getFloatAlign,
    getDoubleWidth,
    getDoubleAlign,
    getLongDoubleWidth,
    getLongDoubleAlign,
    getFloat128Width,
    getFloat128Align,
    useFloat128ManglingForLongDouble,
    getFloatEvalMethod,
    getLargeArrayMinWidth,
    getLargeArrayAlign,
    getMaxAtomicPromoteWidth,
    getMaxAtomicInlineWidth,
    hasBuiltinAtomic,
    getMaxVectorAlign,
    getSimdDefaultAlign,
    getIntMaxTWidth,
    getUnwindWordWidth,
    getRegisterWidth,
    getMCountName,
    useSignedCharForObjCBool,
    useBitFieldTypeAlignment,
    useZeroLengthBitfieldAlignment,
    getZeroLengthBitfieldBoundary,
    useExplicitBitFieldAlignment,
    hasAlignMac68kSupport,
    getTypeName,
    getTypeConstantSuffix,
    getTypeFormatModifier,
    useObjCFPRetForRealType,
    useObjCFP2RetForComplexLongDouble,
    useFP16ConversionIntrinsics,
    useAddressSpaceMapMangling,
    isCLZForZeroUndef,
    getBuiltinVaListKind,
    hasBuiltinMSVaList,
    isRenderScriptTarget,
    isValidClobber,
    isValidGCCRegisterName,
    getNormalizedGCCRegisterName,
    getConstraintRegister,
    validateOutputConstraint,
    validateOutputSize,
    validateInputSize,
    getClobbers,
    isNan2008,
    getTriple,
    getDataLayout,
    hasProtectedVisibility,
    isValidSectionSpecifier,
    getABI,
    getCXXABI,
    isValidCPUName,
    isValidFeatureName,
    hasFeature,
    supportsMultiVersioning,
    validateCpuSupports,
    multiVersionSortPriority,
    validateCpuIs,
    validateCPUSpecificCPUDispatch,
    CPUSpecificManglingCharacter,
    getRegParmMax,
    isTLSSupported,
    getMaxTLSAlign,
    isVLASupported,
    isSEHTrySupported,
    hasNoAsmVariants,
    getEHDataRegisterNumber,
    getStaticInitSectionSpecifier,
    getPlatformName,
    getPlatformMinVersion,
    isBigEndian,
    isLittleEndian,
    getDefaultCallingConv,
    checkCallingConvention,
    getCallingConvKind,
    hasSjLjLowering,
    checkCFProtectionBranchSupported,
    checkCFProtectionReturnSupported,
    allowsLargerPreferedTypeAlignment,
    getSupportedOpenCLOpts,
    getOpenCLTypeAddrSpace,
    getVtblPtrAddressSpace,
    validateTarget,
  };
  static const unsigned _total_num_mems_ = validateTarget + 1;
}
namespace clang__TargetInfo__ConstraintInfo {
  enum memnames {
    Flags,
    TiedOperand,
    isReadWrite,
    allowsRegister,
    allowsMemory,
    hasMatchingInput,
    hasTiedOperand,
    getTiedOperand,
    requiresImmediateConstant,
    isValidAsmImmediate,
  };
  static const unsigned _total_num_mems_ = isValidAsmImmediate + 1;
}
namespace clang__XRayFunctionFilter {
  enum memnames {
    shouldImbueFunction,
    shouldImbueFunctionsInFile,
    shouldImbueLocation,
  };
  static const unsigned _total_num_mems_ = shouldImbueLocation + 1;
}
namespace clang__TypeInfo {
  enum memnames {
    Width,
    Align,
    AlignIsRequired,
  };
  static const unsigned _total_num_mems_ = AlignIsRequired + 1;
}
namespace clang__ASTContext {
  enum memnames {
    Retain,
    Release,
    Idents,
    Selectors,
    getPrintingPolicy,
    getSourceManager,
    Allocate,
    Deallocate,
    getASTAllocatedMemory,
    getSideTableAllocatedMemory,
    getTargetInfo,
    getAuxTargetInfo,
    getIntTypeForBitwidth,
    getRealTypeForBitwidth,
    AtomicUsesUnsupportedLibcall,
    getLangOpts,
    getSanitizerBlacklist,
    getXRayFilter,
    getDiagnostics,
    getFullLoc,
    CommentsLoaded,
    getRawCommentForDeclNoCache,
    getRawCommentForAnyRedecl,
    getCommentCommandTraits,
    overridden_methods_size,
    getNextLocalImport,
    local_imports,
    getTranslationUnitDecl,
    getExternCContextDecl,
    getMakeIntegerSeqDecl,
    getTypePackElementDecl,
    AutoDeductTy,
    AutoRRefDeductTy,
    VaListTagDecl,
    getExternalSource,
    PrintStats,
    getInt128Decl,
    getUInt128Decl,
    getAddrSpaceQualType,
    removeAddrSpaceQualType,
    getObjCGCQualType,
    getRestrictType,
    getVolatileType,
    getConstType,
    getComplexType,
    getPointerType,
    getAdjustedType,
    getDecayedType,
    getAtomicType,
    getBlockPointerType,
    getBlockDescriptorType,
    getReadPipeType,
    getWritePipeType,
    getBlockDescriptorExtendedType,
    getOpenCLTypeKind,
    getOpenCLTypeAddrSpace,
    getLValueReferenceType,
    getRValueReferenceType,
    getMemberPointerType,
    getVariableArrayType,
    getDependentSizedArrayType,
    getIncompleteArrayType,
    getConstantArrayType,
    getVariableArrayDecayedType,
    getVectorType,
    getDependentVectorType,
    getExtVectorType,
    getDependentSizedExtVectorType,
    getDependentAddressSpaceType,
    getFunctionNoProtoType,
    getFunctionNoProtoType1,
    adjustStringLiteralBaseType,
    getTypeDeclType,
    getTypedefType,
    getRecordType,
    getEnumType,
    getInjectedClassNameType,
    getSubstTemplateTypeParmType,
    getTemplateTypeParmType,
    getTemplateSpecializationType,
    getTemplateSpecializationTypeInfo,
    getParenType,
    getElaboratedType,
    getDependentNameType,
    getDependentTemplateSpecializationType,
    getObjCInterfaceType,
    getObjCObjectType,
    getObjCObjectPointerType,
    getTypeOfExprType,
    getTypeOfType,
    getDecltypeType,
    getReflectedType,
    getUnaryTransformType,
    getAutoType,
    getAutoDeductType,
    getAutoRRefDeductType,
    getDeducedTemplateSpecializationType,
    getTagDeclType,
    getWCharType,
    getWideCharType,
    getSignedWCharType,
    getUnsignedWCharType,
    getWIntType,
    getIntPtrType,
    getUIntPtrType,
    getPointerDiffType,
    getUnsignedPointerDiffType,
    getProcessIDType,
    getCFConstantStringType,
    getObjCSuperType,
    getRawCFConstantStringType,
    getCFConstantStringDecl,
    getCFConstantStringTagDecl,
    getObjCConstantStringInterface,
    getObjCNSStringType,
    getObjCIdRedefinitionType,
    getObjCClassRedefinitionType,
    getObjCSelRedefinitionType,
    getBoolName,
    getMakeIntegerSeqName,
    getTypePackElementName,
    getFILEType,
    getjmp_bufType,
    getsigjmp_bufType,
    getucontext_tType,
    getLogicalOperationType,
    getLegacyIntegralTypeEncoding,
    getObjCEncodingForFunctionDecl,
    getObjCEncodingForMethodDecl,
    getObjCEncodingForBlock,
    getObjCEncodingForPropertyDecl,
    ProtocolCompatibleWithProtocol,
    getObjCPropertyImplDeclForPropertyDecl,
    getObjCEncodingTypeSize,
    getObjCIdDecl,
    getObjCIdType,
    getObjCSelDecl,
    getObjCSelType,
    getObjCClassDecl,
    getObjCClassType,
    getObjCProtocolDecl,
    getBOOLDecl,
    getBOOLType,
    getObjCProtoType,
    getBuiltinVaListDecl,
    getBuiltinVaListType,
    getVaListTagDecl,
    getBuiltinMSVaListDecl,
    getBuiltinMSVaListType,
    canBuiltinBeRedeclared,
    getCVRQualifiedType,
    getQualifiedType,
    getQualifiedType1,
    getQualifiedType2,
    getUnqualifiedObjCPointerType,
    getFixedPointScale,
    getFixedPointIBits,
    getNameForTemplate,
    getOverloadedTemplateName,
    getQualifiedTemplateName,
    getDependentTemplateName,
    getDependentTemplateName1,
    getSubstTemplateTemplateParm,
    getSubstTemplateTemplateParmPack,
    getObjCGCAttrKind,
    isObjCNSObjectType,
    getTypeInfo,
    getTypeInfo1,
    getOpenMPDefaultSimdAlign,
    getTypeSize,
    getTypeSize1,
    getCharWidth,
    toCharUnitsFromBits,
    toBits,
    getTypeSizeInChars,
    getTypeSizeInChars1,
    getTypeAlign,
    getTypeAlign1,
    getTypeUnadjustedAlign,
    getTypeUnadjustedAlign1,
    getTypeAlignIfKnown,
    getTypeAlignInChars,
    getTypeAlignInChars1,
    getTypeUnadjustedAlignInChars,
    getTypeUnadjustedAlignInChars1,
    isAlignmentRequired,
    isAlignmentRequired1,
    getPreferredTypeAlign,
    getTargetDefaultAlignForAttributeAligned,
    getAlignOfGlobalVar,
    getAlignOfGlobalVarInChars,
    getDeclAlign,
    DumpRecordLayout,
    getOffsetOfBaseWithVBPtr,
    getFieldOffset,
    lookupFieldBitOffset,
    isNearlyEmpty,
    CountNonClassIvars,
    hasUniqueObjectRepresentations,
    getCanonicalType,
    hasSameType,
    hasSameType1,
    hasSameUnqualifiedType,
    hasSameNullabilityTypeQualifier,
    getCanonicalNestedNameSpecifier,
    getDefaultCallingConvention,
    getCanonicalTemplateName,
    getCanonicalTemplateArgument,
    getAsArrayType,
    getAsConstantArrayType,
    getAsVariableArrayType,
    getAsIncompleteArrayType,
    getAsDependentSizedArrayType,
    getBaseElementType,
    getBaseElementType1,
    getConstantArrayElementCount,
    getAdjustedParameterType,
    getSignatureParameterType,
    getExceptionObjectType,
    getArrayDecayedType,
    getPromotedIntegerType,
    getInnerObjCOwnership,
    isPromotableBitField,
    getIntegerTypeOrder,
    getFloatingTypeOrder,
    getFloatingTypeOfSizeWithinDomain,
    getTargetAddressSpace,
    getTargetAddressSpace1,
    getTargetAddressSpace2,
    getTargetNullPointerValue,
    addressSpaceMapManglingFor,
    isObjCIdType,
    isObjCClassType,
    isObjCSelType,
    getIntWidth,
    getCorrespondingUnsignedType,
    getCorrespondingSaturatedType,
    MakeIntValue,
    getObjCMethodRedeclaration,
    getObjContainingInterface,
    CreateTypeSourceInfo,
    getTrivialTypeSourceInfo,
    GetGVALinkageForFunction,
    getManglingNumber,
    getStaticLocalNumber,
    getParameterIndex,
    isMSStaticDataMemberInlineDefinition,
    getInlineVariableDefinitionKind,
  };
  static const unsigned _total_num_mems_ = getInlineVariableDefinitionKind + 1;
}
namespace clang__DeclAccessPair {
  enum memnames {
    make,
    getDecl,
    getAccess,
    operator_arrow,
  };
  static const unsigned _total_num_mems_ = operator_arrow + 1;
}
namespace clang__UnresolvedSetIterator {
  enum memnames {
    operator_plus,
    operator_plus1,
    operator_minus,
    operator_not_eq,
    operator_gr,
    operator_less_eq,
    operator_gr_eq,
    operator_arrow,
    operator_minus1,
    operator_eq_eq,
    operator_less,
    operator_star,
    getDecl,
    setDecl,
    getAccess,
    getPair,
  };
  static const unsigned _total_num_mems_ = getPair + 1;
}
namespace clang__UnresolvedSetImpl {
  enum memnames {
    _this_,
    empty,
    size,
    operator_sub,
  };
  static const unsigned _total_num_mems_ = operator_sub + 1;
}
namespace clang__Expr {
  enum memnames {
    getType,
    isValueDependent,
    isTypeDependent,
    isInstantiationDependent,
    containsUnexpandedParameterPack,
    getExprLoc,
    isUnusedResultAWarning,
    isLValue,
    isRValue,
    isXValue,
    isGLValue,
    ClassifyLValue,
    isModifiableLvalue,
    Classify,
    ClassifyModifiable,
    getValueKindForType,
    getValueKind,
    getObjectKind,
    isOrdinaryOrBitFieldObject,
    refersToBitField,
    getSourceBitField,
    getReferencedDeclOfCallee,
    getObjCProperty,
    isObjCSelfExpr,
    refersToVectorElement,
    refersToGlobalRegisterVar,
    hasPlaceholderType,
    hasPlaceholderType1,
    isKnownToHaveBooleanValue,
    isIntegerConstantExpr,
    isIntegerConstantExpr1,
    isCXX98IntegralConstantExpr,
    isCXX11ConstantExpr,
    isConstantInitializer,
    EvaluateAsRValue,
    EvaluateAsInt,
    EvaluateAsString,
    EvaluateAsVoid,
    EvaluateAsFloat,
    isEvaluatable,
    HasSideEffects,
    hasNonTrivialCall,
    EvaluateForOverflow,
    EvaluateAsLValue,
    EvaluateAsConstantExpr,
    isNullPointerConstant,
    isOBJCGCCandidate,
    isBoundMemberFunction,
    findBoundMemberType,
    IgnoreImplicit,
    IgnoreConversionOperator,
    IgnoreParenImpCasts,
    IgnoreParenLValueCasts,
    ignoreParenBaseCasts,
    isDefaultArgument,
    isTemporaryObject,
    isImplicitCXXThis,
    IgnoreImpCasts,
    IgnoreParens,
    IgnoreParenCasts,
    IgnoreCasts,
    IgnoreParenNoopCasts,
    getBestDynamicClassType,
    getBestDynamicClassTypeExpr,
    skipRValueSubobjectAdjustments,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__Expr__Classification {
  enum memnames {
    getKind,
    getModifiable,
    isLValue,
    isXValue,
    isGLValue,
    isPRValue,
    isRValue,
    isModifiable,
    makeSimpleLValue,
  };
  static const unsigned _total_num_mems_ = makeSimpleLValue + 1;
}
namespace clang__Expr__EvalStatus {
  enum memnames {
    HasSideEffects,
    HasUndefinedBehavior,
    hasSideEffects,
  };
  static const unsigned _total_num_mems_ = hasSideEffects + 1;
}
namespace clang__Expr__EvalResult {
  enum memnames {
    isGlobalLValue,
  };
  static const unsigned _total_num_mems_ = isGlobalLValue + 1;
}
namespace clang__OpaqueValueExpr {
  enum memnames {
    findInCopyConstruct,
    getLocation,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getExprLoc,
    children,
    getSourceExpr,
    isUnique,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DeclRefExpr {
  enum memnames {
    Create,
    Create1,
    CreateEmpty,
    getDecl,
    getNameInfo,
    getLocation,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    hasQualifier,
    getQualifierLoc,
    getQualifier,
    getFoundDecl,
    hasTemplateKWAndArgsInfo,
    getTemplateKeywordLoc,
    getLAngleLoc,
    getRAngleLoc,
    hasTemplateKeyword,
    hasExplicitTemplateArgs,
    copyTemplateArgumentsInto,
    getTemplateArgs,
    getNumTemplateArgs,
    template_arguments,
    hadMultipleCandidates,
    refersToEnclosingVariableOrCapture,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__PredefinedExpr {
  enum memnames {
    getIdentType,
    getLocation,
    getFunctionName,
    getIdentTypeName,
    ComputeName,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__APIntStorage {
  enum memnames {
  };
  static const unsigned _total_num_mems_ = 0;
}
namespace clang__IntegerLiteral {
  enum memnames {
    Create,
    Create1,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getLocation,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__FixedPointLiteral {
  enum memnames {
    CreateFromRawInt,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getLocation,
    classof,
    getValueAsString,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__CharacterLiteral {
  enum memnames {
    getLocation,
    getKind,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getValue,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__FloatingLiteral {
  enum memnames {
    Create,
    Create1,
    isExact,
    getValueAsApproximateDouble,
    getLocation,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ImaginaryLiteral {
  enum memnames {
    getSubExpr,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__StringLiteral {
  enum memnames {
    CreateEmpty,
    getString,
    getBytes,
    outputString,
    getCodeUnit,
    getByteLength,
    getLength,
    getCharByteWidth,
    getKind,
    isAscii,
    isWide,
    isUTF8,
    isUTF16,
    isUTF32,
    isPascal,
    containsNonAscii,
    containsNonAsciiOrNull,
    getNumConcatenated,
    getStrTokenLoc,
    getLocationOfByte,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ParenExpr {
  enum memnames {
    getSubExpr,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getLParen,
    getRParen,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__UnaryOperator {
  enum memnames {
    getOpcode,
    getSubExpr,
    getOperatorLoc,
    canOverflow,
    isPostfix,
    isPrefix,
    isPrefix1,
    isPostfix1,
    isIncrementOp,
    isIncrementOp1,
    isDecrementOp,
    isDecrementOp1,
    isIncrementDecrementOp,
    isIncrementDecrementOp1,
    isArithmeticOp,
    isArithmeticOp1,
    getOpcodeStr,
    getOverloadedOpcode,
    getOverloadedOperator,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getExprLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__OffsetOfNode {
  enum memnames {
    getKind,
    getArrayExprIndex,
    getField,
    getFieldName,
    getBase,
    getSourceRange,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
  };
  static const unsigned _total_num_mems_ = getEndLoc + 1;
}
namespace clang__OffsetOfExpr {
  enum memnames {
    CreateEmpty,
    getOperatorLoc,
    getRParenLoc,
    getTypeSourceInfo,
    getComponent,
    getNumComponents,
    getIndexExpr,
    getNumExpressions,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__UnaryExprOrTypeTraitExpr {
  enum memnames {
    getKind,
    isArgumentType,
    getArgumentType,
    getArgumentTypeInfo,
    getArgumentExpr,
    getTypeOfArgument,
    getOperatorLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ArraySubscriptExpr {
  enum memnames {
    getLHS,
    getRHS,
    getBase,
    getIdx,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getRBracketLoc,
    getExprLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__CallExpr {
  enum memnames {
    getCallee,
    getCalleeDecl,
    getDirectCallee,
    getNumArgs,
    getArgs,
    getArg,
    arguments,
    getNumCommas,
    getBuiltinCallee,
    isUnevaluatedBuiltinCall,
    getCallReturnType,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    isBuiltinAssumeFalse,
    isCallToStdMove,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__MemberExpr {
  enum memnames {
    Create,
    getBase,
    getMemberDecl,
    getFoundDecl,
    hasQualifier,
    getQualifierLoc,
    getQualifier,
    getTemplateKeywordLoc,
    getLAngleLoc,
    getRAngleLoc,
    hasTemplateKeyword,
    hasExplicitTemplateArgs,
    copyTemplateArgumentsInto,
    getTemplateArgs,
    getNumTemplateArgs,
    template_arguments,
    getMemberNameInfo,
    getOperatorLoc,
    isArrow,
    getMemberLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getExprLoc,
    isImplicitAccess,
    hadMultipleCandidates,
    performsVirtualDispatch,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__CompoundLiteralExpr {
  enum memnames {
    getInitializer,
    isFileScope,
    getLParenLoc,
    getTypeSourceInfo,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__CastExpr {
  enum memnames {
    getCastKind,
    getCastKindName,
    getCastKindName1,
    getSubExpr,
    getSubExprAsWritten,
    getConversionFunction,
    path_empty,
    path_size,
    getTargetUnionField,
    getTargetFieldForToUnionCast,
    getTargetFieldForToUnionCast1,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ImplicitCastExpr {
  enum memnames {
    isPartOfExplicitCast,
    CreateEmpty,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ExplicitCastExpr {
  enum memnames {
    getTypeInfoAsWritten,
    getTypeAsWritten,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CStyleCastExpr {
  enum memnames {
    CreateEmpty,
    getLParenLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__BinaryOperator {
  enum memnames {
    getExprLoc,
    getOperatorLoc,
    getOpcode,
    getLHS,
    getRHS,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getOpcodeStr,
    getOpcodeStr1,
    getOverloadedOpcode,
    getOverloadedOperator,
    isPtrMemOp,
    isMultiplicativeOp,
    isMultiplicativeOp1,
    isAdditiveOp,
    isAdditiveOp1,
    isShiftOp,
    isShiftOp1,
    isBitwiseOp,
    isBitwiseOp1,
    isRelationalOp,
    isRelationalOp1,
    isEqualityOp,
    isEqualityOp1,
    isComparisonOp,
    isComparisonOp1,
    negateComparisonOp,
    reverseComparisonOp,
    isLogicalOp,
    isLogicalOp1,
    isAssignmentOp,
    isAssignmentOp1,
    isCompoundAssignmentOp,
    isCompoundAssignmentOp1,
    getOpForCompoundAssignment,
    isShiftAssignOp,
    isShiftAssignOp1,
    isNullPointerArithmeticExtension,
    classof,
    children,
    getFPFeatures,
    isFPContractableWithinStatement,
  };
  static const unsigned _total_num_mems_ = isFPContractableWithinStatement + 1;
}
namespace clang__CompoundAssignOperator {
  enum memnames {
    getComputationLHSType,
    getComputationResultType,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__AbstractConditionalOperator {
  enum memnames {
    getCond,
    getTrueExpr,
    getFalseExpr,
    getQuestionLoc,
    getColonLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ConditionalOperator {
  enum memnames {
    getCond,
    getTrueExpr,
    getFalseExpr,
    getLHS,
    getRHS,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__BinaryConditionalOperator {
  enum memnames {
    getCommon,
    getOpaqueValue,
    getCond,
    getTrueExpr,
    getFalseExpr,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__AddrLabelExpr {
  enum memnames {
    getAmpAmpLoc,
    getLabelLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getLabel,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__StmtExpr {
  enum memnames {
    getSubStmt,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getLParenLoc,
    getRParenLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ShuffleVectorExpr {
  enum memnames {
    getBuiltinLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    getNumSubExprs,
    getExpr,
    getShuffleMaskIdx,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ConvertVectorExpr {
  enum memnames {
    getSrcExpr,
    getTypeSourceInfo,
    getBuiltinLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ChooseExpr {
  enum memnames {
    isConditionTrue,
    isConditionDependent,
    getChosenSubExpr,
    getCond,
    getLHS,
    getRHS,
    getBuiltinLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__GNUNullExpr {
  enum memnames {
    getTokenLocation,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__VAArgExpr {
  enum memnames {
    getSubExpr,
    isMicrosoftABI,
    getWrittenTypeInfo,
    getBuiltinLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__InitListExpr {
  enum memnames {
    _this_,
    getNumInits,
    getInits,
    inits,
    getInit,
    getArrayFiller,
    hasArrayFiller,
    getInitializedFieldInUnion,
    isExplicit,
    isStringLiteralInit,
    isTransparent,
    isIdiomaticZeroInitializer,
    getLBraceLoc,
    getRBraceLoc,
    isSemanticForm,
    getSemanticForm,
    isSyntacticForm,
    getSyntacticForm,
    hadArrayRangeDesignator,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__DesignatedInitExpr {
  enum memnames {
    CreateEmpty,
    size,
    designators,
    getDesignator,
    getArrayIndex,
    getArrayRangeStart,
    getArrayRangeEnd,
    getEqualOrColonLoc,
    usesGNUSyntax,
    getInit,
    getNumSubExprs,
    getSubExpr,
    getDesignatorsSourceRange,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__DesignatedInitExpr__Designator {
  enum memnames {
    isFieldDesignator,
    isArrayDesignator,
    isArrayRangeDesignator,
    getFieldName,
    getField,
    getDotLoc,
    getFieldLoc,
    getLBracketLoc,
    getRBracketLoc,
    getEllipsisLoc,
    getFirstExprIndex,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
  };
  static const unsigned _total_num_mems_ = getSourceRange + 1;
}
namespace clang__NoInitExpr {
  enum memnames {
    classof,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__DesignatedInitUpdateExpr {
  enum memnames {
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    getBase,
    getUpdater,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ArrayInitLoopExpr {
  enum memnames {
    getCommonExpr,
    getSubExpr,
    classof,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ArrayInitIndexExpr {
  enum memnames {
    classof,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ImplicitValueInitExpr {
  enum memnames {
    classof,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ParenListExpr {
  enum memnames {
    getNumExprs,
    getExpr,
    getLParenLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__GenericSelectionExpr {
  enum memnames {
    getNumAssocs,
    getGenericLoc,
    getDefaultLoc,
    getRParenLoc,
    getAssocExpr,
    getAssocExprs,
    getAssocTypeSourceInfo,
    getAssocTypeSourceInfos,
    getAssocType,
    getControllingExpr,
    isResultDependent,
    getResultIndex,
    getResultExpr,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__ExtVectorElementExpr {
  enum memnames {
    getBase,
    getAccessor,
    getAccessorLoc,
    getNumElements,
    containsDuplicateElements,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    isArrow,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__BlockExpr {
  enum memnames {
    getBlockDecl,
    getCaretLocation,
    getBody,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getFunctionType,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__AsTypeExpr {
  enum memnames {
    getSrcExpr,
    getBuiltinLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__PseudoObjectExpr {
  enum memnames {
    Create,
    getSyntacticForm,
    getResultExprIndex,
    getResultExpr,
    getNumSemanticExprs,
    semantics,
    getSemanticExpr,
    getExprLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    children,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__AtomicExpr {
  enum memnames {
    getNumSubExprs,
    getPtr,
    getOrder,
    getScope,
    getVal1,
    getOrderFail,
    getVal2,
    getWeak,
    getValueType,
    getOp,
    getNumSubExprs1,
    getSubExprs,
    isVolatile,
    isCmpXChg,
    isOpenCL,
    getBuiltinLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    children,
  };
  static const unsigned _total_num_mems_ = children + 1;
}
namespace clang__TypoExpr {
  enum memnames {
    children,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__Attr {
  enum memnames {
    operator_new,
    operator_delete,
    getKind,
    getSpellingListIndex,
    getSpelling,
    getLocation,
    getRange,
    isInherited,
    isImplicit,
    isPackExpansion,
    clone,
    isLateParsed,
    printPretty,
  };
  static const unsigned _total_num_mems_ = printPretty + 1;
}
namespace clang__InheritableAttr {
  enum memnames {
    shouldInheritEvenIfAlreadyPresent,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ExternalSourceSymbolAttr {
  enum memnames {
    clone,
    printPretty,
    getSpelling,
    getLanguage,
    getLanguageLength,
    getDefinedIn,
    getDefinedInLength,
    getGeneratedDeclaration,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__MSInheritanceAttr {
  enum memnames {
    CreateImplicit,
    CreateImplicit1,
    clone,
    printPretty,
    getSpelling,
    getSemanticSpelling,
    getBestCase,
    hasVBPtrOffsetField,
    hasNVOffsetField,
    hasVBTableOffsetField,
    hasOnlyOneField,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__MSVtorDispAttr {
  enum memnames {
    CreateImplicit,
    clone,
    printPretty,
    getSpelling,
    getVdm,
    getVtorDispMode,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__LambdaCapture {
  enum memnames {
    getCaptureKind,
    capturesThis,
    capturesVariable,
    capturesVLAType,
    getCapturedVar,
    isImplicit,
    isExplicit,
    getLocation,
    isPackExpansion,
    getEllipsisLoc,
  };
  static const unsigned _total_num_mems_ = getEllipsisLoc + 1;
}
namespace clang__AccessSpecDecl {
  enum memnames {
    getAccessSpecifierLoc,
    getColonLoc,
    getSourceRange,
    Create,
    CreateDeserialized,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__CXXBaseSpecifier {
  enum memnames {
    getSourceRange,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getBaseTypeLoc,
    isVirtual,
    isBaseOfClass,
    isPackExpansion,
    getInheritConstructors,
    getEllipsisLoc,
    getAccessSpecifier,
    getAccessSpecifierAsWritten,
    getType,
    getTypeSourceInfo,
  };
  static const unsigned _total_num_mems_ = getTypeSourceInfo + 1;
}
namespace clang__CXXRecordDecl {
  enum memnames {
    getCanonicalDecl,
    getPreviousDecl,
    getMostRecentDecl,
    getMostRecentNonInjectedDecl,
    getDefinition,
    hasDefinition,
    Create,
    CreateLambda,
    CreateDeserialized,
    isDynamicClass,
    mayBeDynamicClass,
    mayBeNonDynamicClass,
    isParsingBaseSpecifiers,
    isInstantiation,
    getODRHash,
    getNumBases,
    bases,
    getNumVBases,
    vbases,
    hasAnyDependentBases,
    methods,
    ctors,
    friends,
    hasFriends,
    defaultedCopyConstructorIsDeleted,
    defaultedMoveConstructorIsDeleted,
    defaultedDestructorIsDeleted,
    hasSimpleCopyConstructor,
    hasSimpleMoveConstructor,
    hasSimpleMoveAssignment,
    hasSimpleDestructor,
    hasDefaultConstructor,
    needsImplicitDefaultConstructor,
    hasUserDeclaredConstructor,
    hasUserProvidedDefaultConstructor,
    hasUserDeclaredCopyConstructor,
    needsImplicitCopyConstructor,
    needsOverloadResolutionForCopyConstructor,
    implicitCopyConstructorHasConstParam,
    hasCopyConstructorWithConstParam,
    hasUserDeclaredMoveOperation,
    hasUserDeclaredMoveConstructor,
    hasMoveConstructor,
    needsImplicitMoveConstructor,
    needsOverloadResolutionForMoveConstructor,
    hasUserDeclaredCopyAssignment,
    needsImplicitCopyAssignment,
    needsOverloadResolutionForCopyAssignment,
    implicitCopyAssignmentHasConstParam,
    hasCopyAssignmentWithConstParam,
    hasUserDeclaredMoveAssignment,
    hasMoveAssignment,
    needsImplicitMoveAssignment,
    needsOverloadResolutionForMoveAssignment,
    hasUserDeclaredDestructor,
    needsImplicitDestructor,
    needsOverloadResolutionForDestructor,
    isLambda,
    isGenericLambda,
    getLambdaCallOperator,
    getLambdaStaticInvoker,
    getGenericLambdaTemplateParameterList,
    getLambdaCaptureDefault,
    captures,
    isAggregate,
    hasInClassInitializer,
    hasUninitializedReferenceMember,
    isPOD,
    isCLike,
    isEmpty,
    hasDirectFields,
    isPolymorphic,
    isAbstract,
    isStandardLayout,
    isCXX11StandardLayout,
    hasMutableFields,
    hasVariantMembers,
    hasTrivialDefaultConstructor,
    hasNonTrivialDefaultConstructor,
    hasConstexprNonCopyMoveConstructor,
    defaultedDefaultConstructorIsConstexpr,
    hasConstexprDefaultConstructor,
    hasTrivialCopyConstructor,
    hasTrivialCopyConstructorForCall,
    hasNonTrivialCopyConstructor,
    hasNonTrivialCopyConstructorForCall,
    hasTrivialMoveConstructor,
    hasTrivialMoveConstructorForCall,
    hasNonTrivialMoveConstructor,
    hasNonTrivialMoveConstructorForCall,
    hasTrivialCopyAssignment,
    hasNonTrivialCopyAssignment,
    hasTrivialMoveAssignment,
    hasNonTrivialMoveAssignment,
    hasTrivialDestructor,
    hasTrivialDestructorForCall,
    hasNonTrivialDestructor,
    hasNonTrivialDestructorForCall,
    allowConstDefaultInit,
    hasIrrelevantDestructor,
    hasNonLiteralTypeFieldsOrBases,
    hasInheritedConstructor,
    hasInheritedAssignment,
    isTriviallyCopyable,
    isTrivial,
    isLiteral,
    getInstantiatedFromMemberClass,
    getMemberSpecializationInfo,
    getDescribedClassTemplate,
    getTemplateSpecializationKind,
    getTemplateInstantiationPattern,
    getDestructor,
    isAnyDestructorNoReturn,
    isLocalClass,
    isCurrentInstantiation,
    isDerivedFrom,
    isVirtuallyDerivedFrom,
    isProvablyNotDerivedFrom,
    viewInheritance,
    MergeAccess,
    mayBeAbstract,
    getLambdaManglingNumber,
    getLambdaContextDecl,
    getMSInheritanceModel,
    calculateInheritanceModel,
    nullFieldOffsetIsZero,
    getMSVtorDispMode,
    isDependentLambda,
    getLambdaTypeInfo,
    isInterfaceLike,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__CXXDeductionGuideDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    isExplicit,
    isExplicitSpecified,
    getDeducedTemplate,
    isCopyDeductionCandidate,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__CXXMethodDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    isStatic,
    isInstance,
    isStaticOverloadedOperator,
    isConst,
    isVolatile,
    isVirtual,
    getDevirtualizedMethod,
    isUsualDeallocationFunction,
    isCopyAssignmentOperator,
    isMoveAssignmentOperator,
    getCanonicalDecl,
    getMostRecentDecl,
    isUserProvided,
    size_overridden_methods,
    getParent,
    getThisType,
    getTypeQualifiers,
    getRefQualifier,
    hasInlineBody,
    isLambdaStaticInvoker,
    getCorrespondingMethodInClass,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__CXXCtorInitializer {
  enum memnames {
    isBaseInitializer,
    isMemberInitializer,
    isAnyMemberInitializer,
    isIndirectMemberInitializer,
    isInClassMemberInitializer,
    isDelegatingInitializer,
    isPackExpansion,
    getEllipsisLoc,
    getBaseClassLoc,
    getBaseClass,
    isBaseVirtual,
    getTypeSourceInfo,
    getMember,
    getAnyMember,
    getIndirectMember,
    getMemberLocation,
    getSourceLocation,
    getSourceRange,
    isWritten,
    getSourceOrder,
    getLParenLoc,
    getRParenLoc,
    getInit,
  };
  static const unsigned _total_num_mems_ = getInit + 1;
}
namespace clang__InheritedConstructor {
  enum memnames {
    operator_bool,
    getShadowDecl,
    getConstructor,
  };
  static const unsigned _total_num_mems_ = getConstructor + 1;
}
namespace clang__CXXConstructorDecl {
  enum memnames {
    CreateDeserialized,
    Create,
    inits,
    getNumCtorInitializers,
    isExplicitSpecified,
    isExplicit,
    isDelegatingConstructor,
    getTargetConstructor,
    isDefaultConstructor,
    isCopyConstructor,
    isMoveConstructor,
    isCopyOrMoveConstructor,
    isConvertingConstructor,
    isSpecializationCopyingObject,
    isInheritingConstructor,
    getInheritedConstructor,
    getCanonicalDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__CXXDestructorDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getOperatorDelete,
    getOperatorDeleteThisArg,
    getCanonicalDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__CXXConversionDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    isExplicitSpecified,
    isExplicit,
    getConversionType,
    isLambdaToBlockPointerConversion,
    getCanonicalDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__LinkageSpecDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getLanguage,
    hasBraces,
    getExternLoc,
    getRBraceLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__UsingDirectiveDecl {
  enum memnames {
    getQualifierLoc,
    getQualifier,
    getNominatedNamespaceAsWritten,
    getNominatedNamespace,
    getCommonAncestor,
    getUsingLoc,
    getNamespaceKeyLocation,
    getIdentLocation,
    Create,
    CreateDeserialized,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__NamespaceAliasDecl {
  enum memnames {
    getPreviousDecl,
    getFirstDecl,
    getMostRecentDecl,
    Create,
    CreateDeserialized,
    getCanonicalDecl,
    getQualifierLoc,
    getQualifier,
    getNamespace,
    getAliasLoc,
    getNamespaceLoc,
    getTargetNameLoc,
    getAliasedNamespace,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__UsingShadowDecl {
  enum memnames {
    getPreviousDecl,
    getFirstDecl,
    getMostRecentDecl,
    Create,
    CreateDeserialized,
    getCanonicalDecl,
    getTargetDecl,
    getUsingDecl,
    getNextUsingShadowDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ConstructorUsingShadowDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getParent,
    getNominatedBaseClassShadowDecl,
    getConstructedBaseClassShadowDecl,
    getNominatedBaseClass,
    getConstructedBaseClass,
    constructsVirtualBase,
    getConstructor,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__UsingDecl {
  enum memnames {
    getFirstDecl,
    getUsingLoc,
    getQualifierLoc,
    getQualifier,
    getNameInfo,
    isAccessDeclaration,
    hasTypename,
    shadows,
    shadow_size,
    Create,
    CreateDeserialized,
    getSourceRange,
    getCanonicalDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__UsingPackDecl {
  enum memnames {
    getFirstDecl,
    getInstantiatedFromUsingDecl,
    expansions,
    CreateDeserialized,
    getSourceRange,
    getCanonicalDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__UnresolvedUsingValueDecl {
  enum memnames {
    getFirstDecl,
    getUsingLoc,
    isAccessDeclaration,
    getQualifierLoc,
    getQualifier,
    getNameInfo,
    isPackExpansion,
    getEllipsisLoc,
    Create,
    CreateDeserialized,
    getSourceRange,
    getCanonicalDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__UnresolvedUsingTypenameDecl {
  enum memnames {
    getFirstDecl,
    getUsingLoc,
    getTypenameLoc,
    getQualifierLoc,
    getQualifier,
    getNameInfo,
    isPackExpansion,
    getEllipsisLoc,
    Create,
    CreateDeserialized,
    getCanonicalDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__StaticAssertDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getAssertExpr,
    getMessage,
    isFailed,
    getRParenLoc,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__BindingDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getBinding,
    getHoldingVar,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__DecompositionDecl {
  enum memnames {
    CreateDeserialized,
    bindings,
    printName,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__MSPropertyDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    classof,
    hasGetter,
    getGetterId,
    hasSetter,
    getSetterId,
  };
  static const unsigned _total_num_mems_ = getSetterId + 1;
}
namespace clang__ConstexprDecl {
  enum memnames {
    getLambdaExpr,
    isDependent,
    CreateDeserialized,
    hasFunctionRepresentation,
    hasLambdaRepresentation,
    getFunctionDecl,
    getClosureDecl,
    getClosureCallOperator,
    hasBody,
    getBody,
    getCallExpr,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__TemplateParameterList {
  enum memnames {
    _this_,
    size,
    asArray,
    getParam,
    getMinRequiredArguments,
    getDepth,
    containsUnexpandedParameterPack,
    getRequiresClause,
    getTemplateLoc,
    getLAngleLoc,
    getRAngleLoc,
    getSourceRange,
  };
  static const unsigned _total_num_mems_ = getSourceRange + 1;
}
namespace clang__TemplateArgumentList {
  enum memnames {
    get,
    operator_sub,
    asArray,
    size,
    data,
  };
  static const unsigned _total_num_mems_ = data + 1;
}
namespace clang__TemplateDecl {
  enum memnames {
    getTemplateParameters,
    getRequiresClause,
    getAssociatedConstraints,
    getTemplatedDecl,
    classof,
    classofKind,
    getSourceRange,
  };
  static const unsigned _total_num_mems_ = getSourceRange + 1;
}
namespace clang__FunctionTemplateSpecializationInfo {
  enum memnames {
    Create,
    Function,
    TemplateArguments,
    TemplateArgumentsAsWritten,
    PointOfInstantiation,
    getTemplate,
    getTemplateSpecializationKind,
    isExplicitSpecialization,
    isExplicitInstantiationOrSpecialization,
    getPointOfInstantiation,
  };
  static const unsigned _total_num_mems_ = getPointOfInstantiation + 1;
}
namespace clang__MemberSpecializationInfo {
  enum memnames {
    getInstantiatedFrom,
    getTemplateSpecializationKind,
    isExplicitSpecialization,
    getPointOfInstantiation,
  };
  static const unsigned _total_num_mems_ = getPointOfInstantiation + 1;
}
namespace clang__DependentFunctionTemplateSpecializationInfo {
  enum memnames {
    Create,
    getNumTemplates,
    getTemplate,
    getTemplateArgs,
    getNumTemplateArgs,
    getTemplateArg,
    getLAngleLoc,
    getRAngleLoc,
  };
  static const unsigned _total_num_mems_ = getRAngleLoc + 1;
}
namespace clang__RedeclarableTemplateDecl {
  enum memnames {
    getPreviousDecl,
    getFirstDecl,
    getMostRecentDecl,
    getCanonicalDecl,
    isMemberSpecialization,
    getInstantiatedFromMemberTemplate,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__FunctionTemplateDecl {
  enum memnames {
    LoadLazySpecializations,
    getTemplatedDecl,
    isThisDeclarationADefinition,
    getCanonicalDecl,
    getPreviousDecl,
    getMostRecentDecl,
    getInstantiatedFromMemberTemplate,
    specializations,
    Create,
    CreateDeserialized,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__TemplateTypeParmDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    wasDeclaredWithTypename,
    hasDefaultArgument,
    getDefaultArgument,
    getDefaultArgumentInfo,
    getDefaultArgumentLoc,
    defaultArgumentWasInherited,
    getDepth,
    getIndex,
    isParameterPack,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__NonTypeTemplateParmDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    CreateDeserialized1,
    getSourceRange,
    hasDefaultArgument,
    getDefaultArgument,
    getDefaultArgumentLoc,
    defaultArgumentWasInherited,
    isParameterPack,
    isPackExpansion,
    isExpandedParameterPack,
    getNumExpansionTypes,
    getExpansionType,
    getExpansionTypeSourceInfo,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__TemplateTemplateParmDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    CreateDeserialized1,
    isParameterPack,
    isPackExpansion,
    isExpandedParameterPack,
    getNumExpansionTemplateParameters,
    getExpansionTemplateParameters,
    hasDefaultArgument,
    getDefaultArgument,
    getDefaultArgumentLoc,
    defaultArgumentWasInherited,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__BuiltinTemplateDecl {
  enum memnames {
    classof,
    classofKind,
    getSourceRange,
  };
  static const unsigned _total_num_mems_ = getSourceRange + 1;
}
namespace clang__ClassTemplateSpecializationDecl {
  enum memnames {
    CreateDeserialized,
    getNameForDiagnostic,
    getSpecializedTemplate,
    getTemplateArgs,
    getSpecializationKind,
    isExplicitSpecialization,
    isExplicitInstantiationOrSpecialization,
    getPointOfInstantiation,
    getTemplateInstantiationArgs,
    getTypeAsWritten,
    getExternLoc,
    getTemplateKeywordLoc,
    getSourceRange,
    Profile,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ClassTemplatePartialSpecializationDecl {
  enum memnames {
    CreateDeserialized,
    getTemplateParameters,
    getTemplateArgsAsWritten,
    getInstantiatedFromMember,
    getInstantiatedFromMemberTemplate,
    getInjectedSpecializationType,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ClassTemplateDecl {
  enum memnames {
    LoadLazySpecializations,
    getTemplatedDecl,
    isThisDeclarationADefinition,
    Create,
    CreateDeserialized,
    getCanonicalDecl,
    getPreviousDecl,
    getMostRecentDecl,
    getInstantiatedFromMemberTemplate,
    specializations,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__FriendTemplateDecl {
  enum memnames {
    CreateDeserialized,
    getFriendType,
    getFriendDecl,
    getFriendLoc,
    getTemplateParameterList,
    getNumTemplateParameters,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__TypeAliasTemplateDecl {
  enum memnames {
    getTemplatedDecl,
    getCanonicalDecl,
    getPreviousDecl,
    getInstantiatedFromMemberTemplate,
    Create,
    CreateDeserialized,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ClassScopeFunctionSpecializationDecl {
  enum memnames {
    getSpecialization,
    hasExplicitTemplateArgs,
    templateArgs,
    Create,
    CreateDeserialized,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__VarTemplateSpecializationDecl {
  enum memnames {
    CreateDeserialized,
    getNameForDiagnostic,
    getSpecializedTemplate,
    getTemplateArgs,
    getTemplateArgsInfo,
    getSpecializationKind,
    isExplicitSpecialization,
    isExplicitInstantiationOrSpecialization,
    getPointOfInstantiation,
    getTemplateInstantiationArgs,
    getTypeAsWritten,
    getExternLoc,
    getTemplateKeywordLoc,
    Profile,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__VarTemplatePartialSpecializationDecl {
  enum memnames {
    CreateDeserialized,
    getTemplateParameters,
    getTemplateArgsAsWritten,
    getInstantiatedFromMember,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__VarTemplateDecl {
  enum memnames {
    LoadLazySpecializations,
    getTemplatedDecl,
    isThisDeclarationADefinition,
    Create,
    CreateDeserialized,
    getCanonicalDecl,
    getPreviousDecl,
    getMostRecentDecl,
    getInstantiatedFromMemberTemplate,
    specializations,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__FriendDecl {
  enum memnames {
    CreateDeserialized,
    getFriendType,
    getFriendTypeNumTemplateParameterLists,
    getFriendTypeTemplateParameterList,
    getFriendDecl,
    getFriendLoc,
    getSourceRange,
    isUnsupportedFriend,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCListBase {
  enum memnames {
    size,
    empty,
  };
  static const unsigned _total_num_mems_ = empty + 1;
}
namespace clang__ObjCProtocolList {
  enum memnames {
    _this_,
    operator_sub,
  };
  static const unsigned _total_num_mems_ = operator_sub + 1;
}
namespace clang__ObjCMethodDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getCanonicalDecl,
    getObjCDeclQualifier,
    hasRelatedResultType,
    isRedeclaration,
    getDeclaratorEndLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    getSelectorStartLoc,
    getSelectorLoc,
    getNumSelectorLocs,
    getClassInterface,
    getSelector,
    getReturnType,
    getReturnTypeSourceRange,
    getSendResultType,
    getSendResultType1,
    getReturnTypeSourceInfo,
    param_size,
    parameters,
    getSelfDecl,
    getCmdDecl,
    getMethodFamily,
    isInstanceMethod,
    isVariadic,
    isClassMethod,
    isPropertyAccessor,
    isDefined,
    isOverriding,
    hasSkippedBody,
    findPropertyDecl,
    getImplementationControl,
    isOptional,
    isThisDeclarationADesignatedInitializer,
    isDesignatedInitializerForTheInterface,
    hasBody,
    getBody,
    isThisDeclarationADefinition,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCTypeParamDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getSourceRange,
    getVariance,
    getVarianceLoc,
    getIndex,
    hasExplicitBound,
    getColonLoc,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCTypeParamList {
  enum memnames {
    _this_,
    size,
    front,
    back,
    getLAngleLoc,
    getRAngleLoc,
    getSourceRange,
  };
  static const unsigned _total_num_mems_ = getSourceRange + 1;
}
namespace clang__ObjCPropertyDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getAtLoc,
    getLParenLoc,
    getTypeSourceInfo,
    getType,
    getUsageType,
    getPropertyAttributes,
    getPropertyAttributesAsWritten,
    isReadOnly,
    isAtomic,
    isRetaining,
    isInstanceProperty,
    isClassProperty,
    getQueryKind,
    getQueryKind1,
    getSetterKind,
    getGetterName,
    getGetterNameLoc,
    getSetterName,
    getSetterNameLoc,
    getGetterMethodDecl,
    getSetterMethodDecl,
    getPropertyImplementation,
    isOptional,
    getPropertyIvarDecl,
    getSourceRange,
    getDefaultSynthIvarName,
    findPropertyDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCContainerDecl {
  enum memnames {
    properties,
    instance_properties,
    class_properties,
    methods,
    instance_methods,
    class_methods,
    getMethod,
    getInstanceMethod,
    getClassMethod,
    HasUserDeclaredSetterMethod,
    getIvarDecl,
    FindPropertyDeclaration,
    getAtStartLoc,
    getAtEndRange,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCInterfaceDecl {
  enum memnames {
    getPreviousDecl,
    getFirstDecl,
    getMostRecentDecl,
    Create,
    CreateDeserialized,
    getTypeParamList,
    getTypeParamListAsWritten,
    getSourceRange,
    hasDesignatedInitializers,
    declaresOrInheritsDesignatedInitializers,
    getReferencedProtocols,
    getImplementation,
    FindCategoryDeclaration,
    getCategoryInstanceMethod,
    getCategoryClassMethod,
    getCategoryMethod,
    protocols,
    protocol_locs,
    all_referenced_protocols,
    ivars,
    ivar_size,
    ivar_empty,
    getObjCRuntimeNameAsString,
    isDesignatedInitializer,
    isThisDeclarationADefinition,
    hasDefinition,
    getDefinition,
    getSuperClassType,
    getSuperClassTInfo,
    getSuperClass,
    visible_categories,
    visible_categories_empty,
    known_categories,
    known_categories_empty,
    visible_extensions,
    visible_extensions_empty,
    known_extensions,
    known_extensions_empty,
    getCategoryListRaw,
    FindPropertyVisibleInPrimaryClass,
    isSuperClassOf,
    isArcWeakrefUnavailable,
    isObjCRequiresPropertyDefs,
    lookupMethod,
    lookupInstanceMethod,
    lookupClassMethod,
    lookupPrivateMethod,
    lookupPropertyAccessor,
    getEndOfDefinitionLoc,
    getSuperClassLoc,
    isImplicitInterfaceDecl,
    getCanonicalDecl,
    getTypeForDecl,
    setTypeForDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCIvarDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getContainingInterface,
    getNextIvar,
    getAccessControl,
    getCanonicalAccessControl,
    getSynthesize,
    getUsageType,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCAtDefsFieldDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCProtocolDecl {
  enum memnames {
    getPreviousDecl,
    getFirstDecl,
    getMostRecentDecl,
    Create,
    CreateDeserialized,
    getReferencedProtocols,
    protocols,
    protocol_locs,
    protocol_size,
    lookupMethod,
    lookupInstanceMethod,
    lookupClassMethod,
    hasDefinition,
    getDefinition,
    isThisDeclarationADefinition,
    getObjCRuntimeNameAsString,
    getSourceRange,
    getCanonicalDecl,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCCategoryDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getClassInterface,
    getTypeParamList,
    getImplementation,
    getReferencedProtocols,
    protocols,
    protocol_size,
    protocol_locs,
    getNextClassCategory,
    getNextClassCategoryRaw,
    IsClassExtension,
    ivars,
    ivar_size,
    ivar_empty,
    getCategoryNameLoc,
    getIvarLBraceLoc,
    getIvarRBraceLoc,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCImplDecl {
  enum memnames {
    getClassInterface,
    FindPropertyImplDecl,
    FindPropertyImplIvarDecl,
    property_impls,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCCategoryImplDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getCategoryDecl,
    getCategoryNameLoc,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCImplementationDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    inits,
    getNumIvarInitializers,
    hasNonZeroConstructors,
    hasDestructors,
    getIdentifier,
    getName,
    getNameAsString,
    getObjCRuntimeNameAsString,
    getSuperClass,
    getSuperClassLoc,
    getIvarLBraceLoc,
    getIvarRBraceLoc,
    ivars,
    ivar_size,
    ivar_empty,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCCompatibleAliasDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getClassInterface,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__ObjCPropertyImplDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getSourceRange,
    getLocStart,
    getBeginLoc,
    getPropertyDecl,
    getPropertyImplementation,
    getPropertyIvarDecl,
    getPropertyIvarDeclLoc,
    isIvarNameSpecified,
    getGetterCXXConstructor,
    getSetterCXXAssignment,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__OMPThreadPrivateDecl {
  enum memnames {
    CreateDeserialized,
    varlist_size,
    varlist_empty,
    varlists,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__OMPDeclareReductionDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getCombiner,
    getInitializer,
    getInitializerKind,
    getPrevDeclInScope,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__OMPCapturedExprDecl {
  enum memnames {
    Create,
    CreateDeserialized,
    getSourceRange,
    classof,
    classofKind,
  };
  static const unsigned _total_num_mems_ = classofKind + 1;
}
namespace clang__CXXOperatorCallExpr {
  enum memnames {
    getOperator,
    isAssignmentOp,
    isAssignmentOp1,
    isInfixBinaryOp,
    getOperatorLoc,
    getExprLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    classof,
    getFPFeatures,
    isFPContractableWithinStatement,
  };
  static const unsigned _total_num_mems_ = isFPContractableWithinStatement + 1;
}
namespace clang__CXXMemberCallExpr {
  enum memnames {
    getImplicitObjectArgument,
    getMethodDecl,
    getRecordDecl,
    getExprLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CUDAKernelCallExpr {
  enum memnames {
    getConfig,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXNamedCastExpr {
  enum memnames {
    getCastName,
    getOperatorLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getAngleBrackets,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXStaticCastExpr {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXDynamicCastExpr {
  enum memnames {
    CreateEmpty,
    isAlwaysNull,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXReinterpretCastExpr {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXConstCastExpr {
  enum memnames {
    Create,
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__UserDefinedLiteral {
  enum memnames {
    getLiteralOperatorKind,
    getCookedLiteral,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getUDSuffixLoc,
    getUDSuffix,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXBoolLiteralExpr {
  enum memnames {
    getValue,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getLocation,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXNullPtrLiteralExpr {
  enum memnames {
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getLocation,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXStdInitializerListExpr {
  enum memnames {
    getSubExpr,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXTypeidExpr {
  enum memnames {
    isPotentiallyEvaluated,
    isTypeOperand,
    getTypeOperand,
    getTypeOperandSourceInfo,
    getExprOperand,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__MSPropertyRefExpr {
  enum memnames {
    getSourceRange,
    isImplicitAccess,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
    getBaseExpr,
    getPropertyDecl,
    isArrow,
    getMemberLoc,
    getQualifierLoc,
  };
  static const unsigned _total_num_mems_ = getQualifierLoc + 1;
}
namespace clang__MSPropertySubscriptExpr {
  enum memnames {
    getBase,
    getIdx,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getRBracketLoc,
    getExprLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXUuidofExpr {
  enum memnames {
    isTypeOperand,
    getTypeOperand,
    getTypeOperandSourceInfo,
    getExprOperand,
    getUuidStr,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXThisExpr {
  enum memnames {
    getLocation,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    isImplicit,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXThrowExpr {
  enum memnames {
    getSubExpr,
    getThrowLoc,
    isThrownVariableInScope,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXDefaultArgExpr {
  enum memnames {
    Create,
    getParam,
    getExpr,
    getUsedLocation,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getExprLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXDefaultInitExpr {
  enum memnames {
    Create,
    getField,
    getExpr,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXTemporary {
  enum memnames {
    Create,
    getDestructor,
  };
  static const unsigned _total_num_mems_ = getDestructor + 1;
}
namespace clang__CXXBindTemporaryExpr {
  enum memnames {
    Create,
    getTemporary,
    getSubExpr,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXConstructExpr {
  enum memnames {
    getConstructor,
    getLocation,
    isElidable,
    hadMultipleCandidates,
    isListInitialization,
    isStdInitListInitialization,
    requiresZeroInitialization,
    getConstructionKind,
    arguments,
    getArgs,
    getNumArgs,
    getArg,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getParenOrBraceRange,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXInheritedCtorInitExpr {
  enum memnames {
    getConstructor,
    constructsVBase,
    getConstructionKind,
    inheritedFromVBase,
    getLocation,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXFunctionalCastExpr {
  enum memnames {
    CreateEmpty,
    getLParenLoc,
    getRParenLoc,
    isListInitialization,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXTemporaryObjectExpr {
  enum memnames {
    getTypeSourceInfo,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__LambdaExpr {
  enum memnames {
    CreateDeserialized,
    getCaptureDefault,
    getCaptureDefaultLoc,
    isInitCapture,
    captures,
    capture_size,
    explicit_captures,
    implicit_captures,
    capture_inits,
    getIntroducerRange,
    getLambdaClass,
    getCallOperator,
    getTemplateParameterList,
    isGenericLambda,
    getBody,
    isMutable,
    hasExplicitParameters,
    hasExplicitResultType,
    classof,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
  };
  static const unsigned _total_num_mems_ = getEndLoc + 1;
}
namespace clang__CXXScalarValueInitExpr {
  enum memnames {
    getTypeSourceInfo,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXNewExpr {
  enum memnames {
    getAllocatedType,
    getAllocatedTypeSourceInfo,
    shouldNullCheckAllocation,
    getOperatorNew,
    getOperatorDelete,
    isArray,
    getArraySize,
    getNumPlacementArgs,
    getPlacementArg,
    isParenTypeId,
    getTypeIdParens,
    isGlobalNew,
    hasInitializer,
    getInitializationStyle,
    getInitializer,
    getConstructExpr,
    passAlignment,
    doesUsualArrayDeleteWantSize,
    placement_arguments,
    getStartLoc,
    getBeginLoc,
    getEndLoc,
    getDirectInitRange,
    getSourceRange,
    getLocStart,
    getLocEnd,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXDeleteExpr {
  enum memnames {
    isGlobalDelete,
    isArrayForm,
    isArrayFormAsWritten,
    doesUsualArrayDeleteWantSize,
    getOperatorDelete,
    getArgument,
    getDestroyedType,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXPseudoDestructorExpr {
  enum memnames {
    getBase,
    hasQualifier,
    getQualifierLoc,
    getQualifier,
    isArrow,
    getOperatorLoc,
    getScopeTypeInfo,
    getColonColonLoc,
    getTildeLoc,
    getDestroyedTypeInfo,
    getDestroyedTypeIdentifier,
    getDestroyedType,
    getDestroyedTypeLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__TypeTraitExpr {
  enum memnames {
    CreateDeserialized,
    getTrait,
    getValue,
    getNumArgs,
    getArg,
    getArgs,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ArrayTypeTraitExpr {
  enum memnames {
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getTrait,
    getQueriedType,
    getQueriedTypeSourceInfo,
    getValue,
    getDimensionExpression,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ExpressionTraitExpr {
  enum memnames {
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getTrait,
    getQueriedExpression,
    getValue,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OverloadExpr {
  enum memnames {
    find,
    getNamingClass,
    decls,
    getNumDecls,
    getNameInfo,
    getName,
    getNameLoc,
    getQualifier,
    getQualifierLoc,
    getTemplateKeywordLoc,
    getLAngleLoc,
    getRAngleLoc,
    hasTemplateKeyword,
    hasExplicitTemplateArgs,
    getTemplateArgs,
    getNumTemplateArgs,
    template_arguments,
    copyTemplateArgumentsInto,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OverloadExpr__FindResult {
  enum memnames {
    Expression,
    IsAddressOfOperand,
    HasFormOfMemberPointer,
  };
  static const unsigned _total_num_mems_ = HasFormOfMemberPointer + 1;
}
namespace clang__UnresolvedLookupExpr {
  enum memnames {
    Create,
    Create1,
    CreateEmpty,
    requiresADL,
    isOverloaded,
    getNamingClass,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DependentScopeDeclRefExpr {
  enum memnames {
    Create,
    CreateEmpty,
    getNameInfo,
    getDeclName,
    getLocation,
    getQualifierLoc,
    getQualifier,
    getTemplateKeywordLoc,
    getLAngleLoc,
    getRAngleLoc,
    hasTemplateKeyword,
    hasExplicitTemplateArgs,
    copyTemplateArgumentsInto,
    getTemplateArgs,
    getNumTemplateArgs,
    template_arguments,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ExprWithCleanups {
  enum memnames {
    Create,
    getObjects,
    getNumObjects,
    getObject,
    getSubExpr,
    cleanupsHaveSideEffects,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXUnresolvedConstructExpr {
  enum memnames {
    CreateEmpty,
    getTypeAsWritten,
    getTypeSourceInfo,
    getLParenLoc,
    getRParenLoc,
    isListInitialization,
    arg_size,
    getArg,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXDependentScopeMemberExpr {
  enum memnames {
    Create,
    CreateEmpty,
    isImplicitAccess,
    getBase,
    getBaseType,
    isArrow,
    getOperatorLoc,
    getQualifier,
    getQualifierLoc,
    getFirstQualifierFoundInScope,
    getMemberNameInfo,
    getMember,
    getMemberLoc,
    getTemplateKeywordLoc,
    getLAngleLoc,
    getRAngleLoc,
    hasTemplateKeyword,
    hasExplicitTemplateArgs,
    copyTemplateArgumentsInto,
    getTemplateArgs,
    getNumTemplateArgs,
    template_arguments,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__UnresolvedMemberExpr {
  enum memnames {
    Create,
    CreateEmpty,
    isImplicitAccess,
    getBase,
    getBaseType,
    hasUnresolvedUsing,
    isArrow,
    getOperatorLoc,
    getNamingClass,
    getMemberNameInfo,
    getMemberName,
    getMemberLoc,
    getExprLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXNoexceptExpr {
  enum memnames {
    getOperand,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    getValue,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__PackExpansionExpr {
  enum memnames {
    getPattern,
    getEllipsisLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__SizeOfPackExpr {
  enum memnames {
    CreateDeserialized,
    getOperatorLoc,
    getPackLoc,
    getRParenLoc,
    getPack,
    getPackLength,
    isPartiallySubstituted,
    getPartialArguments,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__SubstNonTypeTemplateParmExpr {
  enum memnames {
    getNameLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getReplacement,
    getParameter,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__SubstNonTypeTemplateParmPackExpr {
  enum memnames {
    getParameterPack,
    getParameterPackLocation,
    getArgumentPack,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__FunctionParmPackExpr {
  enum memnames {
    _this_,
    CreateEmpty,
    getParameterPack,
    getParameterPackLocation,
    getNumExpansions,
    getExpansion,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__MaterializeTemporaryExpr {
  enum memnames {
    getTemporary,
    GetTemporaryExpr,
    getStorageDuration,
    getExtendingDecl,
    getManglingNumber,
    isBoundToLvalueReference,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXFoldExpr {
  enum memnames {
    getLHS,
    getRHS,
    isRightFold,
    isLeftFold,
    getPattern,
    getInit,
    getEllipsisLoc,
    getOperator,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CoroutineSuspendExpr {
  enum memnames {
    getKeywordLoc,
    getCommonExpr,
    getOpaqueValue,
    getReadyExpr,
    getSuspendExpr,
    getResumeExpr,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CoawaitExpr {
  enum memnames {
    getOperand,
    isImplicit,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__DependentCoawaitExpr {
  enum memnames {
    getOperand,
    getOperatorCoawaitLookup,
    getKeywordLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CoyieldExpr {
  enum memnames {
    getOperand,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ReflectionExpr {
  enum memnames {
    isReflexpr,
    hasTypeOperand,
    hasExpressionOperand,
    getTypeOperand,
    getExpressionOperand,
    getOperatorLoc,
    getLocStart,
    getLocEnd,
  };
  static const unsigned _total_num_mems_ = getLocEnd + 1;
}
namespace clang__CompilerMessageExpr {
  enum memnames {
    Create,
    CreateEmpty,
    getMessage,
    getKWLoc,
    getRParenLoc,
    getLocStart,
    getLocEnd,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CompilerDiagnosticExpr {
  enum memnames {
    CreateEmpty,
    getKWLoc,
    getRParenLoc,
    arg_size,
    getArgs,
    getArg,
    isError,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXMetaparseExpr {
  enum memnames {
    getExprSrcCode,
    getLocStart,
    getLocEnd,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ReflectionTraitExpr {
  enum memnames {
    getTraitKind,
    getNumArgs,
    getArg,
    getArgs,
    getKWLoc,
    getRParenLoc,
    getLocStart,
    getLocEnd,
    getASTContext_p,
    getObjKind,
    callbacksAreSet,
    readyToConvertToStringLiteral,
    getMemNum,
    isPtr,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ReflectNewExpr {
  enum memnames {
    getNumArgs,
    getArg,
    getArgs,
    getObjKind,
    getTraitLoc,
    getRParenLoc,
    getLocStart,
    getLocEnd,
  };
  static const unsigned _total_num_mems_ = getLocEnd + 1;
}
namespace clang__ReflectDeleteExpr {
  enum memnames {
    getNumArgs,
    getArg,
    getArgs,
    getObjKind,
    getTraitLoc,
    getRParenLoc,
    getLocStart,
    getLocEnd,
  };
  static const unsigned _total_num_mems_ = getLocEnd + 1;
}
namespace clang__CXXConstantExpr {
  enum memnames {
    getExpression,
    getValue,
    getLocStart,
    getLocEnd,
    children,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXDependentIdExpr {
  enum memnames {
    getNameInfo,
    getLocStart,
    getLocEnd,
    getSourceRange,
    children,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXConcatenateExpr {
  enum memnames {
    getNumOperands,
    getOperand,
    getOperands,
    children,
    getIntroLoc,
    getLocStart,
    getLocEnd,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCStringLiteral {
  enum memnames {
    getString,
    getAtLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCBoolLiteralExpr {
  enum memnames {
    getValue,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getLocation,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCBoxedExpr {
  enum memnames {
    getSubExpr,
    getBoxingMethod,
    getAtLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCArrayLiteral {
  enum memnames {
    CreateEmpty,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    getElements,
    getNumElements,
    getElement,
    getArrayWithObjectsMethod,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCDictionaryElement {
  enum memnames {
    Key,
    Value,
    EllipsisLoc,
    isPackExpansion,
  };
  static const unsigned _total_num_mems_ = isPackExpansion + 1;
}
namespace clang__ObjCDictionaryLiteral {
  enum memnames {
    CreateEmpty,
    getNumElements,
    getKeyValueElement,
    getDictWithObjectsMethod,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCEncodeExpr {
  enum memnames {
    getAtLoc,
    getRParenLoc,
    getEncodedType,
    getEncodedTypeSourceInfo,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCSelectorExpr {
  enum memnames {
    getSelector,
    getAtLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getNumArgs,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCProtocolExpr {
  enum memnames {
    getProtocol,
    getProtocolIdLoc,
    getAtLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCIvarRefExpr {
  enum memnames {
    getDecl,
    getBase,
    isArrow,
    isFreeIvar,
    getLocation,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getOpLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCPropertyRefExpr {
  enum memnames {
    isImplicitProperty,
    isExplicitProperty,
    getExplicitProperty,
    getImplicitPropertyGetter,
    getImplicitPropertySetter,
    getGetterSelector,
    getSetterSelector,
    isMessagingGetter,
    isMessagingSetter,
    getBase,
    getLocation,
    getReceiverLocation,
    getSuperReceiverType,
    getClassReceiver,
    isObjectReceiver,
    isSuperReceiver,
    isClassReceiver,
    getReceiverType,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCSubscriptRefExpr {
  enum memnames {
    getRBracket,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getBaseExpr,
    getKeyExpr,
    getAtIndexMethodDecl,
    setAtIndexMethodDecl,
    isArraySubscriptRefExpr,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCMessageExpr {
  enum memnames {
    CreateEmpty,
    isImplicit,
    getReceiverKind,
    getReceiverRange,
    isInstanceMessage,
    isClassMessage,
    getInstanceReceiver,
    getClassReceiver,
    getClassReceiverTypeInfo,
    getSuperLoc,
    getReceiverType,
    getReceiverInterface,
    getSuperType,
    getSelector,
    getMethodDecl,
    getMethodFamily,
    getNumArgs,
    getArgs,
    getArg,
    isDelegateInitCall,
    getLeftLoc,
    getRightLoc,
    getSelectorStartLoc,
    getSelectorLoc,
    getNumSelectorLocs,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    arguments,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCIsaExpr {
  enum memnames {
    getBase,
    isArrow,
    getIsaMemberLoc,
    getOpLoc,
    getLocStart,
    getBeginLoc,
    getBaseLocEnd,
    getLocEnd,
    getEndLoc,
    getExprLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCIndirectCopyRestoreExpr {
  enum memnames {
    getSubExpr,
    shouldCopy,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getExprLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCBridgedCastExpr {
  enum memnames {
    getLParenLoc,
    getBridgeKind,
    getBridgeKindName,
    getBridgeKeywordLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCAvailabilityCheckExpr {
  enum memnames {
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getSourceRange,
    hasVersion,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPArraySectionExpr {
  enum memnames {
    getBase,
    getBaseOriginalType,
    getLowerBound,
    getLength,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getColonLoc,
    getRBracketLoc,
    getExprLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXCatchStmt {
  enum memnames {
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getCatchLoc,
    getExceptionDecl,
    getCaughtType,
    getHandlerBlock,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXTryStmt {
  enum memnames {
    Create,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getTryLoc,
    getEndLoc,
    getTryBlock,
    getNumHandlers,
    getHandler,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXForRangeStmt {
  enum memnames {
    getLoopVariable,
    getRangeInit,
    getRangeStmt,
    getBeginStmt,
    getEndStmt,
    getCond,
    getInc,
    getLoopVarStmt,
    getBody,
    getForLoc,
    getCoawaitLoc,
    getColonLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXExpansionStmt {
  enum memnames {
    getRangeVarStmt,
    getRangeVariable,
    getLoopVarStmt,
    getLoopVariable,
    getBody,
    getSize,
    getInstantiatedStatements,
    getForLoc,
    getEllipsisLoc,
    getColonLoc,
    getRParenLoc,
    getLocStart,
    getLocEnd,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXTupleExpansionStmt {
  enum memnames {
    getRangeVarStmt,
    getRangeInit,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXPackExpansionStmt {
  enum memnames {
    getUnexpandedPack,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__MSDependentExistsStmt {
  enum memnames {
    getKeywordLoc,
    isIfExists,
    isIfNotExists,
    getQualifierLoc,
    getNameInfo,
    getSubStmt,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CoroutineBodyStmt {
  enum memnames {
    Create,
    Create1,
    hasDependentPromiseType,
    getBody,
    getPromiseDeclStmt,
    getPromiseDecl,
    getInitSuspendStmt,
    getFinalSuspendStmt,
    getExceptionHandler,
    getFallthroughHandler,
    getAllocate,
    getDeallocate,
    getReturnValueInit,
    getResultDecl,
    getReturnStmt,
    getReturnStmtOnAllocFailure,
    getParamMoves,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CoroutineBodyStmt__CtorArgs {
  enum memnames {
    Body,
    Promise,
    InitialSuspend,
    FinalSuspend,
    OnException,
    OnFallthrough,
    Allocate,
    Deallocate,
    ReturnValue,
    ResultDecl,
    ReturnStmt,
    ReturnStmtOnAllocFailure,
  };
  static const unsigned _total_num_mems_ = ReturnStmtOnAllocFailure + 1;
}
namespace clang__CoreturnStmt {
  enum memnames {
    getKeywordLoc,
    getOperand,
    getPromiseCall,
    isImplicit,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__CXXQueueMetaparseStmt {
  enum memnames {
    getNewSrcCode,
    getIntroLoc,
    getLocStart,
    getLocEnd,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCForCollectionStmt {
  enum memnames {
    getElement,
    getCollection,
    getBody,
    getForLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCAtCatchStmt {
  enum memnames {
    getCatchBody,
    getCatchParamDecl,
    getAtCatchLoc,
    getRParenLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    hasEllipsis,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCAtFinallyStmt {
  enum memnames {
    getFinallyBody,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getAtFinallyLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCAtTryStmt {
  enum memnames {
    Create,
    CreateEmpty,
    getAtTryLoc,
    getTryBody,
    getNumCatchStmts,
    getCatchStmt,
    getFinallyStmt,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCAtSynchronizedStmt {
  enum memnames {
    getAtSynchronizedLoc,
    getSynchBody,
    getSynchExpr,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCAtThrowStmt {
  enum memnames {
    getThrowExpr,
    getThrowLoc,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__ObjCAutoreleasePoolStmt {
  enum memnames {
    getSubStmt,
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getAtLoc,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPClause {
  enum memnames {
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getClauseKind,
    isImplicit,
    children,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPExecutableDirective {
  enum memnames {
    getLocStart,
    getBeginLoc,
    getLocEnd,
    getEndLoc,
    getNumClauses,
    getClause,
    hasAssociatedStmt,
    getAssociatedStmt,
    getCapturedStmt,
    getInnermostCapturedStmt,
    getDirectiveKind,
    classof,
    clauses,
  };
  static const unsigned _total_num_mems_ = clauses + 1;
}
namespace clang__OMPParallelDirective {
  enum memnames {
    CreateEmpty,
    hasCancel,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPLoopDirective {
  enum memnames {
    getCollapsedNumber,
    getIterationVariable,
    getLastIteration,
    getCalcLastIteration,
    getPreCond,
    getCond,
    getInit,
    getInc,
    getPreInits,
    getIsLastIterVariable,
    getLowerBoundVariable,
    getUpperBoundVariable,
    getStrideVariable,
    getEnsureUpperBound,
    getNextLowerBound,
    getNextUpperBound,
    getNumIterations,
    getPrevLowerBoundVariable,
    getPrevUpperBoundVariable,
    getDistInc,
    getPrevEnsureUpperBound,
    getCombinedLowerBoundVariable,
    getCombinedUpperBoundVariable,
    getCombinedEnsureUpperBound,
    getCombinedInit,
    getCombinedCond,
    getCombinedNextLowerBound,
    getCombinedNextUpperBound,
    getBody,
    counters,
    private_counters,
    inits,
    updates,
    finals,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPForDirective {
  enum memnames {
    CreateEmpty,
    hasCancel,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPForSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPSectionsDirective {
  enum memnames {
    CreateEmpty,
    hasCancel,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPSectionDirective {
  enum memnames {
    Create,
    CreateEmpty,
    hasCancel,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPSingleDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPMasterDirective {
  enum memnames {
    Create,
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPCriticalDirective {
  enum memnames {
    CreateEmpty,
    getDirectiveName,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPParallelForDirective {
  enum memnames {
    CreateEmpty,
    hasCancel,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPParallelForSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPParallelSectionsDirective {
  enum memnames {
    CreateEmpty,
    hasCancel,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTaskDirective {
  enum memnames {
    CreateEmpty,
    hasCancel,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTaskyieldDirective {
  enum memnames {
    Create,
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPBarrierDirective {
  enum memnames {
    Create,
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTaskwaitDirective {
  enum memnames {
    Create,
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTaskgroupDirective {
  enum memnames {
    CreateEmpty,
    getReductionRef,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPFlushDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPOrderedDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPAtomicDirective {
  enum memnames {
    CreateEmpty,
    getX,
    getUpdateExpr,
    isXLHSInRHSPart,
    isPostfixUpdate,
    getV,
    getExpr,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetDataDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetEnterDataDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetExitDataDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetParallelDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetParallelForDirective {
  enum memnames {
    CreateEmpty,
    hasCancel,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTeamsDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPCancellationPointDirective {
  enum memnames {
    Create,
    CreateEmpty,
    getCancelRegion,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPCancelDirective {
  enum memnames {
    CreateEmpty,
    getCancelRegion,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTaskLoopDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTaskLoopSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPDistributeDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetUpdateDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPDistributeParallelForDirective {
  enum memnames {
    CreateEmpty,
    hasCancel,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPDistributeParallelForSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPDistributeSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetParallelForSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTeamsDistributeDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTeamsDistributeSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTeamsDistributeParallelForSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTeamsDistributeParallelForDirective {
  enum memnames {
    CreateEmpty,
    hasCancel,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetTeamsDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetTeamsDistributeDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetTeamsDistributeParallelForDirective {
  enum memnames {
    CreateEmpty,
    hasCancel,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetTeamsDistributeParallelForSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__OMPTargetTeamsDistributeSimdDirective {
  enum memnames {
    CreateEmpty,
    classof,
  };
  static const unsigned _total_num_mems_ = classof + 1;
}
namespace clang__StoredDeclsMap {
  enum memnames {
    empty,
    size,
    count,
    isPointerIntoBucketsArray,
    getPointerIntoBucketsArray,
    getMemorySize,
    DestroyAll,
  };
  static const unsigned _total_num_mems_ = DestroyAll + 1;
}
namespace clang__DependentDiagnostic {
  enum memnames {
    Create,
    getKind,
    isAccessToMember,
    getAccess,
    getAccessLoc,
    getAccessTarget,
    getAccessNamingClass,
    getAccessBaseObjectType,
    getDiagnostic,
  };
  static const unsigned _total_num_mems_ = getDiagnostic + 1;
}
namespace reflcontainers__VectorStr {
  enum memnames {
    _this_,
    clone,
    size,
    empty,
    clear,
    resize,
    capacity,
    reserve,
    shrink_to_fit,
    push_back,
    pop_back,
    dealloc,
    at,
    assign,
    front,
    back,
  };
  static const unsigned _total_num_mems_ = back + 1;
}
namespace reflcontainers__VectorInt {
  enum memnames {
    _this_,
    clone,
    size,
    empty,
    clear,
    resize,
    capacity,
    reserve,
    shrink_to_fit,
    push_back,
    pop_back,
    dealloc,
    at,
    assign,
    front,
    back,
    assign1,
    push_back1,
  };
  static const unsigned _total_num_mems_ = push_back1 + 1;
}
namespace reflcontainers__SetInt {
  enum memnames {
    _this_,
    clone,
    size,
    empty,
    clear,
    contains,
    insert,
    erase,
    at_ith,
    dealloc,
    contains1,
    insert1,
    erase1,
  };
  static const unsigned _total_num_mems_ = erase1 + 1;
}
namespace reflcontainers__SetStr {
  enum memnames {
    _this_,
    clone,
    size,
    empty,
    clear,
    contains,
    insert,
    erase,
    at_ith,
    dealloc,
  };
  static const unsigned _total_num_mems_ = dealloc + 1;
}
namespace reflcontainers__IntIntPair {
  enum memnames {
    first,
    second,
  };
  static const unsigned _total_num_mems_ = second + 1;
}
namespace reflcontainers__IntStrPair {
  enum memnames {
    first,
    second,
  };
  static const unsigned _total_num_mems_ = second + 1;
}
namespace reflcontainers__StrIntPair {
  enum memnames {
    first,
    second,
  };
  static const unsigned _total_num_mems_ = second + 1;
}
namespace reflcontainers__StrStrPair {
  enum memnames {
    first,
    second,
  };
  static const unsigned _total_num_mems_ = second + 1;
}
namespace reflcontainers__MapIntStr {
  enum memnames {
    _this_,
    clone,
    size,
    empty,
    clear,
    at_ith,
    at_ith_key,
    at_ith_val,
    dealloc,
    contains,
    at,
    assign,
    erase,
    erase1,
    contains1,
    at1,
    assign1,
  };
  static const unsigned _total_num_mems_ = assign1 + 1;
}
namespace reflcontainers__MapStrInt {
  enum memnames {
    _this_,
    clone,
    size,
    empty,
    clear,
    at_ith,
    at_ith_key,
    at_ith_val,
    dealloc,
    contains,
    at,
    assign,
    erase,
    assign1,
  };
  static const unsigned _total_num_mems_ = assign1 + 1;
}
namespace reflcontainers__MapStrStr {
  enum memnames {
    _this_,
    clone,
    size,
    empty,
    clear,
    at_ith,
    at_ith_key,
    at_ith_val,
    dealloc,
    contains,
    at,
    assign,
    erase,
  };
  static const unsigned _total_num_mems_ = erase + 1;
}
namespace reflcontainers__MapIntInt {
  enum memnames {
    _this_,
    clone,
    size,
    empty,
    clear,
    at_ith,
    at_ith_key,
    at_ith_val,
    dealloc,
    contains,
    at,
    assign,
    erase,
    erase1,
    contains1,
    at1,
    assign1,
    assign2,
    assign3,
  };
  static const unsigned _total_num_mems_ = assign3 + 1;
}
