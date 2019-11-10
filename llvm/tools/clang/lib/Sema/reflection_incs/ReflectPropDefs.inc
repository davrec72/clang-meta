ExprResult Reflector::ReflectProp(unsigned N , llvm::APInt *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__APInt;
# define CLASSNAME llvm::APInt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(needsCleanup, PRIM, P, needsCleanup)
    case_METHOD_NOARGS(isNegative, PRIM, P, isNegative)
    case_METHOD_NOARGS(isNonNegative, PRIM, P, isNonNegative)
    case_METHOD_NOARGS(isSignBitSet, PRIM, P, isSignBitSet)
    case_METHOD_NOARGS(isSignBitClear, PRIM, P, isSignBitClear)
    case_METHOD_NOARGS(isStrictlyPositive, PRIM, P, isStrictlyPositive)
    case_METHOD_NOARGS(isAllOnesValue, PRIM, P, isAllOnesValue)
    case_METHOD_NOARGS(isNullValue, PRIM, P, isNullValue)
    case_METHOD_NOARGS(isOneValue, PRIM, P, isOneValue)
    case_METHOD_NOARGS(isMaxValue, PRIM, P, isMaxValue)
    case_METHOD_NOARGS(isMaxSignedValue, PRIM, P, isMaxSignedValue)
    case_METHOD_NOARGS(isMinValue, PRIM, P, isMinValue)
    case_METHOD_NOARGS(isMinSignedValue, PRIM, P, isMinSignedValue)
    case currefl::isIntN: {
      CHECK_TRAIT_IS_REFLPROP(isIntN)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isIntN, p0)
      TOO_MANY;
    }
    case currefl::isSignedIntN: {
      CHECK_TRAIT_IS_REFLPROP(isSignedIntN)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isSignedIntN, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isPowerOf2, PRIM, P, isPowerOf2)
    case_METHOD_NOARGS(isSignMask, PRIM, P, isSignMask)
    case_METHOD_NOARGS(getBoolValue, PRIM, P, getBoolValue)
    case currefl::getLimitedValue: {
      CHECK_TRAIT_IS_REFLPROP(getLimitedValue)
      if (no_more) PRIM_REFLECTION_0(P, getLimitedValue)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, getLimitedValue, p0)
      TOO_MANY;
    }
    case currefl::isSplat: {
      CHECK_TRAIT_IS_REFLPROP(isSplat)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isSplat, p0)
      TOO_MANY;
    }
    case currefl::isMask: {
      CHECK_TRAIT_IS_REFLPROP(isMask)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isMask, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isMask1, PRIM, P, isMask)
    case_METHOD_NOARGS(isShiftedMask, PRIM, P, isShiftedMask)
    case currefl::getMaxValue: {
      CHECK_TRAIT_IS_REFLPROP(getMaxValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getMaxValue, p0)
      TOO_MANY;
    }
    case currefl::getSignedMaxValue: {
      CHECK_TRAIT_IS_REFLPROP(getSignedMaxValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getSignedMaxValue, p0)
      TOO_MANY;
    }
    case currefl::getMinValue: {
      CHECK_TRAIT_IS_REFLPROP(getMinValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getMinValue, p0)
      TOO_MANY;
    }
    case currefl::getSignedMinValue: {
      CHECK_TRAIT_IS_REFLPROP(getSignedMinValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getSignedMinValue, p0)
      TOO_MANY;
    }
    case currefl::getSignMask: {
      CHECK_TRAIT_IS_REFLPROP(getSignMask)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getSignMask, p0)
      TOO_MANY;
    }
    case currefl::getAllOnesValue: {
      CHECK_TRAIT_IS_REFLPROP(getAllOnesValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getAllOnesValue, p0)
      TOO_MANY;
    }
    case currefl::getNullValue: {
      CHECK_TRAIT_IS_REFLPROP(getNullValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getNullValue, p0)
      TOO_MANY;
    }
    case currefl::getHiBits: {
      CHECK_TRAIT_IS_REFLPROP(getHiBits)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getHiBits, p0)
      TOO_MANY;
    }
    case currefl::getLoBits: {
      CHECK_TRAIT_IS_REFLPROP(getLoBits)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getLoBits, p0)
      TOO_MANY;
    }
    case currefl::getOneBitSet: {
      CHECK_TRAIT_IS_REFLPROP(getOneBitSet)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(S, getOneBitSet, p0, p1)
      TOO_MANY;
    }
    case currefl::getBitsSet: {
      CHECK_TRAIT_IS_REFLPROP(getBitsSet)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) CLASS_REFLECTION(S, getBitsSet, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getBitsSetFrom: {
      CHECK_TRAIT_IS_REFLPROP(getBitsSetFrom)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(S, getBitsSetFrom, p0, p1)
      TOO_MANY;
    }
    case currefl::getHighBitsSet: {
      CHECK_TRAIT_IS_REFLPROP(getHighBitsSet)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(S, getHighBitsSet, p0, p1)
      TOO_MANY;
    }
    case currefl::getLowBitsSet: {
      CHECK_TRAIT_IS_REFLPROP(getLowBitsSet)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(S, getLowBitsSet, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getRawData, PRIM, P, getRawData)
    case_METHOD_NOARGS(operator_not, PRIM, P, operator!)
    case currefl::operator_less_less: {
      CHECK_TRAIT_IS_REFLPROP(operator_less_less)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, operator<<, p0)
      TOO_MANY;
    }
    case currefl::ashr: {
      CHECK_TRAIT_IS_REFLPROP(ashr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, ashr, p0)
      TOO_MANY;
    }
    case currefl::lshr: {
      CHECK_TRAIT_IS_REFLPROP(lshr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, lshr, p0)
      TOO_MANY;
    }
    case currefl::shl: {
      CHECK_TRAIT_IS_REFLPROP(shl)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, shl, p0)
      TOO_MANY;
    }
    case currefl::rotl: {
      CHECK_TRAIT_IS_REFLPROP(rotl)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, rotl, p0)
      TOO_MANY;
    }
    case currefl::rotr: {
      CHECK_TRAIT_IS_REFLPROP(rotr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, rotr, p0)
      TOO_MANY;
    }
    case currefl::udiv: {
      CHECK_TRAIT_IS_REFLPROP(udiv)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) CLASS_REFLECTION(P, udiv, p0)
      TOO_MANY;
    }
    case currefl::sdiv: {
      CHECK_TRAIT_IS_REFLPROP(sdiv)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(P, sdiv, p0)
      TOO_MANY;
    }
    case currefl::urem: {
      CHECK_TRAIT_IS_REFLPROP(urem)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, urem, p0)
      TOO_MANY;
    }
    case currefl::srem: {
      CHECK_TRAIT_IS_REFLPROP(srem)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(P, srem, p0)
      TOO_MANY;
    }
    case currefl::operator_sub: {
      CHECK_TRAIT_IS_REFLPROP(operator_sub)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, operator[], p0)
      TOO_MANY;
    }
    case currefl::operator_eq_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_eq_eq)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, operator==, p0)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_not_eq)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, operator!=, p0)
      TOO_MANY;
    }
    case currefl::ult: {
      CHECK_TRAIT_IS_REFLPROP(ult)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, ult, p0)
      TOO_MANY;
    }
    case currefl::slt: {
      CHECK_TRAIT_IS_REFLPROP(slt)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(P, slt, p0)
      TOO_MANY;
    }
    case currefl::ule: {
      CHECK_TRAIT_IS_REFLPROP(ule)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, ule, p0)
      TOO_MANY;
    }
    case currefl::sle: {
      CHECK_TRAIT_IS_REFLPROP(sle)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, sle, p0)
      TOO_MANY;
    }
    case currefl::ugt: {
      CHECK_TRAIT_IS_REFLPROP(ugt)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, ugt, p0)
      TOO_MANY;
    }
    case currefl::sgt: {
      CHECK_TRAIT_IS_REFLPROP(sgt)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(P, sgt, p0)
      TOO_MANY;
    }
    case currefl::uge: {
      CHECK_TRAIT_IS_REFLPROP(uge)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, uge, p0)
      TOO_MANY;
    }
    case currefl::sge: {
      CHECK_TRAIT_IS_REFLPROP(sge)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(P, sge, p0)
      TOO_MANY;
    }
    case currefl::trunc: {
      CHECK_TRAIT_IS_REFLPROP(trunc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, trunc, p0)
      TOO_MANY;
    }
    case currefl::sext: {
      CHECK_TRAIT_IS_REFLPROP(sext)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, sext, p0)
      TOO_MANY;
    }
    case currefl::zext: {
      CHECK_TRAIT_IS_REFLPROP(zext)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, zext, p0)
      TOO_MANY;
    }
    case currefl::sextOrTrunc: {
      CHECK_TRAIT_IS_REFLPROP(sextOrTrunc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, sextOrTrunc, p0)
      TOO_MANY;
    }
    case currefl::zextOrTrunc: {
      CHECK_TRAIT_IS_REFLPROP(zextOrTrunc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, zextOrTrunc, p0)
      TOO_MANY;
    }
    case currefl::sextOrSelf: {
      CHECK_TRAIT_IS_REFLPROP(sextOrSelf)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, sextOrSelf, p0)
      TOO_MANY;
    }
    case currefl::zextOrSelf: {
      CHECK_TRAIT_IS_REFLPROP(zextOrSelf)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, zextOrSelf, p0)
      TOO_MANY;
    }
    case currefl::extractBits: {
      CHECK_TRAIT_IS_REFLPROP(extractBits)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(P, extractBits, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getBitWidth, PRIM, P, getBitWidth)
    case_METHOD_NOARGS(getNumWords, PRIM, P, getNumWords)
    case currefl::getNumWords1: {
      CHECK_TRAIT_IS_REFLPROP(getNumWords1)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(S, getNumWords, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getActiveBits, PRIM, P, getActiveBits)
    case_METHOD_NOARGS(getActiveWords, PRIM, P, getActiveWords)
    case_METHOD_NOARGS(getMinSignedBits, PRIM, P, getMinSignedBits)
    case_METHOD_NOARGS(getZExtValue, PRIM, P, getZExtValue)
    case_METHOD_NOARGS(getSExtValue, PRIM, P, getSExtValue)
    case currefl::getBitsNeeded: {
      CHECK_TRAIT_IS_REFLPROP(getBitsNeeded)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      TRY_LOAD_VAL(unsigned char, p1)
      if (no_more) PRIM_REFLECTION(S, getBitsNeeded, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(countLeadingZeros, PRIM, P, countLeadingZeros)
    case_METHOD_NOARGS(countLeadingOnes, PRIM, P, countLeadingOnes)
    case_METHOD_NOARGS(getNumSignBits, PRIM, P, getNumSignBits)
    case_METHOD_NOARGS(countTrailingZeros, PRIM, P, countTrailingZeros)
    case_METHOD_NOARGS(countTrailingOnes, PRIM, P, countTrailingOnes)
    case_METHOD_NOARGS(countPopulation, PRIM, P, countPopulation)
    case currefl::toString: {
      CHECK_TRAIT_IS_REFLPROP(toString)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) PRIM_REFLECTION(P, toString, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(byteSwap, CLASS, P, byteSwap)
    case_METHOD_NOARGS(reverseBits, CLASS, P, reverseBits)
    case currefl::roundToDouble: {
      CHECK_TRAIT_IS_REFLPROP(roundToDouble)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) PRIM_REFLECTION(P, roundToDouble, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(roundToDouble1, PRIM, P, roundToDouble)
    case_METHOD_NOARGS(signedRoundToDouble, PRIM, P, signedRoundToDouble)
    case_METHOD_NOARGS(bitsToDouble, PRIM, P, bitsToDouble)
    case_METHOD_NOARGS(bitsToFloat, PRIM, P, bitsToFloat)
    case currefl::doubleToBits: {
      CHECK_TRAIT_IS_REFLPROP(doubleToBits)
      TRY_LOAD_VAL(double, p0)
      if (no_more) CLASS_REFLECTION(S, doubleToBits, p0)
      TOO_MANY;
    }
    case currefl::floatToBits: {
      CHECK_TRAIT_IS_REFLPROP(floatToBits)
      TRY_LOAD_VAL(float, p0)
      if (no_more) CLASS_REFLECTION(S, floatToBits, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(logBase2, PRIM, P, logBase2)
    case_METHOD_NOARGS(ceilLogBase2, PRIM, P, ceilLogBase2)
    case_METHOD_NOARGS(nearestLogBase2, PRIM, P, nearestLogBase2)
    case_METHOD_NOARGS(exactLogBase2, PRIM, P, exactLogBase2)
    case_METHOD_NOARGS(sqrt, CLASS, P, sqrt)
    case_METHOD_NOARGS(abs, CLASS, P, abs)
    case_METHOD_NOARGS(magic, CLASS, P, magic)
    case currefl::magicu: {
      CHECK_TRAIT_IS_REFLPROP(magicu)
      if (no_more) CLASS_REFLECTION_0(P, magicu)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, magicu, p0)
      TOO_MANY;
    }
    case currefl::tcSet: {
      CHECK_TRAIT_IS_REFLPROP(tcSet)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned long long, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcSet, (llvm::APInt::WordType *)p0, (llvm::APInt::WordType)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcAssign: {
      CHECK_TRAIT_IS_REFLPROP(tcAssign)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcAssign, (llvm::APInt::WordType *)p0, (const llvm::APInt::WordType *)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcIsZero: {
      CHECK_TRAIT_IS_REFLPROP(tcIsZero)
      TRY_LOAD_VAL(const unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcIsZero, p0, p1)
      TOO_MANY;
    }
    case currefl::tcExtractBit: {
      CHECK_TRAIT_IS_REFLPROP(tcExtractBit)
      TRY_LOAD_VAL(const unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcExtractBit, p0, p1)
      TOO_MANY;
    }
    case currefl::tcExtract: {
      CHECK_TRAIT_IS_REFLPROP(tcExtract)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(const unsigned long long *, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      TRY_LOAD_VAL(unsigned int, p4)
      if (no_more) VOID_REFLECTION(S, tcExtract, (llvm::APInt::WordType *)p0, (unsigned int)p1, (const llvm::APInt::WordType *)p2, (unsigned int)p3, (unsigned int)p4)
      TOO_MANY;
    }
    case currefl::tcSetBit: {
      CHECK_TRAIT_IS_REFLPROP(tcSetBit)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) VOID_REFLECTION(S, tcSetBit, (llvm::APInt::WordType *)p0, (unsigned int)p1)
      TOO_MANY;
    }
    case currefl::tcClearBit: {
      CHECK_TRAIT_IS_REFLPROP(tcClearBit)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) VOID_REFLECTION(S, tcClearBit, (llvm::APInt::WordType *)p0, (unsigned int)p1)
      TOO_MANY;
    }
    case currefl::tcLSB: {
      CHECK_TRAIT_IS_REFLPROP(tcLSB)
      TRY_LOAD_VAL(const unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcLSB, p0, p1)
      TOO_MANY;
    }
    case currefl::tcMSB: {
      CHECK_TRAIT_IS_REFLPROP(tcMSB)
      TRY_LOAD_VAL(const unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcMSB, p0, p1)
      TOO_MANY;
    }
    case currefl::tcNegate: {
      CHECK_TRAIT_IS_REFLPROP(tcNegate)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) VOID_REFLECTION(S, tcNegate, (llvm::APInt::WordType *)p0, (unsigned int)p1)
      TOO_MANY;
    }
    case currefl::tcAdd: {
      CHECK_TRAIT_IS_REFLPROP(tcAdd)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned long long, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      if (no_more) PRIM_REFLECTION(S, tcAdd, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::tcAddPart: {
      CHECK_TRAIT_IS_REFLPROP(tcAddPart)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned long long, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(S, tcAddPart, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::tcSubtract: {
      CHECK_TRAIT_IS_REFLPROP(tcSubtract)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned long long, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      if (no_more) PRIM_REFLECTION(S, tcSubtract, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::tcSubtractPart: {
      CHECK_TRAIT_IS_REFLPROP(tcSubtractPart)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned long long, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(S, tcSubtractPart, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::tcMultiplyPart: {
      CHECK_TRAIT_IS_REFLPROP(tcMultiplyPart)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned long long, p2)
      TRY_LOAD_VAL(unsigned long long, p3)
      TRY_LOAD_VAL(unsigned int, p4)
      TRY_LOAD_VAL(unsigned int, p5)
      TRY_LOAD_VAL(bool, p6)
      if (no_more) PRIM_REFLECTION(S, tcMultiplyPart, p0, p1, p2, p3, p4, p5, p6)
      TOO_MANY;
    }
    case currefl::tcMultiply: {
      CHECK_TRAIT_IS_REFLPROP(tcMultiply)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(const unsigned long long *, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      if (no_more) PRIM_REFLECTION(S, tcMultiply, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::tcFullMultiply: {
      CHECK_TRAIT_IS_REFLPROP(tcFullMultiply)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(const unsigned long long *, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      TRY_LOAD_VAL(unsigned int, p4)
      if (no_more) VOID_REFLECTION(S, tcFullMultiply, (llvm::APInt::WordType *)p0, (const llvm::APInt::WordType *)p1, (const llvm::APInt::WordType *)p2, (unsigned int)p3, (unsigned int)p4)
      TOO_MANY;
    }
    case currefl::tcDivide: {
      CHECK_TRAIT_IS_REFLPROP(tcDivide)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned long long *, p2)
      TRY_LOAD_VAL(unsigned long long *, p3)
      TRY_LOAD_VAL(unsigned int, p4)
      if (no_more) PRIM_REFLECTION(S, tcDivide, p0, p1, p2, p3, p4)
      TOO_MANY;
    }
    case currefl::tcShiftLeft: {
      CHECK_TRAIT_IS_REFLPROP(tcShiftLeft)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcShiftLeft, (llvm::APInt::WordType *)p0, (unsigned int)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcShiftRight: {
      CHECK_TRAIT_IS_REFLPROP(tcShiftRight)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcShiftRight, (llvm::APInt::WordType *)p0, (unsigned int)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcAnd: {
      CHECK_TRAIT_IS_REFLPROP(tcAnd)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcAnd, (llvm::APInt::WordType *)p0, (const llvm::APInt::WordType *)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcOr: {
      CHECK_TRAIT_IS_REFLPROP(tcOr)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcOr, (llvm::APInt::WordType *)p0, (const llvm::APInt::WordType *)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcXor: {
      CHECK_TRAIT_IS_REFLPROP(tcXor)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcXor, (llvm::APInt::WordType *)p0, (const llvm::APInt::WordType *)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcComplement: {
      CHECK_TRAIT_IS_REFLPROP(tcComplement)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) VOID_REFLECTION(S, tcComplement, (llvm::APInt::WordType *)p0, (unsigned int)p1)
      TOO_MANY;
    }
    case currefl::tcCompare: {
      CHECK_TRAIT_IS_REFLPROP(tcCompare)
      TRY_LOAD_VAL(const unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(S, tcCompare, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::tcIncrement: {
      CHECK_TRAIT_IS_REFLPROP(tcIncrement)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcIncrement, p0, p1)
      TOO_MANY;
    }
    case currefl::tcDecrement: {
      CHECK_TRAIT_IS_REFLPROP(tcDecrement)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcDecrement, p0, p1)
      TOO_MANY;
    }
    case currefl::tcSetLeastSignificantBits: {
      CHECK_TRAIT_IS_REFLPROP(tcSetLeastSignificantBits)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcSetLeastSignificantBits, (llvm::APInt::WordType *)p0, (unsigned int)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(dump, VOID, P, dump)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::APInt const &X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__APInt;
# define CLASSNAME llvm::APInt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(needsCleanup, PRIM, V, needsCleanup)
    case_METHOD_NOARGS(isNegative, PRIM, V, isNegative)
    case_METHOD_NOARGS(isNonNegative, PRIM, V, isNonNegative)
    case_METHOD_NOARGS(isSignBitSet, PRIM, V, isSignBitSet)
    case_METHOD_NOARGS(isSignBitClear, PRIM, V, isSignBitClear)
    case_METHOD_NOARGS(isStrictlyPositive, PRIM, V, isStrictlyPositive)
    case_METHOD_NOARGS(isAllOnesValue, PRIM, V, isAllOnesValue)
    case_METHOD_NOARGS(isNullValue, PRIM, V, isNullValue)
    case_METHOD_NOARGS(isOneValue, PRIM, V, isOneValue)
    case_METHOD_NOARGS(isMaxValue, PRIM, V, isMaxValue)
    case_METHOD_NOARGS(isMaxSignedValue, PRIM, V, isMaxSignedValue)
    case_METHOD_NOARGS(isMinValue, PRIM, V, isMinValue)
    case_METHOD_NOARGS(isMinSignedValue, PRIM, V, isMinSignedValue)
    case currefl::isIntN: {
      CHECK_TRAIT_IS_REFLPROP(isIntN)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(V, isIntN, p0)
      TOO_MANY;
    }
    case currefl::isSignedIntN: {
      CHECK_TRAIT_IS_REFLPROP(isSignedIntN)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(V, isSignedIntN, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isPowerOf2, PRIM, V, isPowerOf2)
    case_METHOD_NOARGS(isSignMask, PRIM, V, isSignMask)
    case_METHOD_NOARGS(getBoolValue, PRIM, V, getBoolValue)
    case currefl::getLimitedValue: {
      CHECK_TRAIT_IS_REFLPROP(getLimitedValue)
      if (no_more) PRIM_REFLECTION_0(V, getLimitedValue)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(V, getLimitedValue, p0)
      TOO_MANY;
    }
    case currefl::isSplat: {
      CHECK_TRAIT_IS_REFLPROP(isSplat)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(V, isSplat, p0)
      TOO_MANY;
    }
    case currefl::isMask: {
      CHECK_TRAIT_IS_REFLPROP(isMask)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(V, isMask, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isMask1, PRIM, V, isMask)
    case_METHOD_NOARGS(isShiftedMask, PRIM, V, isShiftedMask)
    case currefl::getMaxValue: {
      CHECK_TRAIT_IS_REFLPROP(getMaxValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getMaxValue, p0)
      TOO_MANY;
    }
    case currefl::getSignedMaxValue: {
      CHECK_TRAIT_IS_REFLPROP(getSignedMaxValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getSignedMaxValue, p0)
      TOO_MANY;
    }
    case currefl::getMinValue: {
      CHECK_TRAIT_IS_REFLPROP(getMinValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getMinValue, p0)
      TOO_MANY;
    }
    case currefl::getSignedMinValue: {
      CHECK_TRAIT_IS_REFLPROP(getSignedMinValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getSignedMinValue, p0)
      TOO_MANY;
    }
    case currefl::getSignMask: {
      CHECK_TRAIT_IS_REFLPROP(getSignMask)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getSignMask, p0)
      TOO_MANY;
    }
    case currefl::getAllOnesValue: {
      CHECK_TRAIT_IS_REFLPROP(getAllOnesValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getAllOnesValue, p0)
      TOO_MANY;
    }
    case currefl::getNullValue: {
      CHECK_TRAIT_IS_REFLPROP(getNullValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getNullValue, p0)
      TOO_MANY;
    }
    case currefl::getHiBits: {
      CHECK_TRAIT_IS_REFLPROP(getHiBits)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, getHiBits, p0)
      TOO_MANY;
    }
    case currefl::getLoBits: {
      CHECK_TRAIT_IS_REFLPROP(getLoBits)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, getLoBits, p0)
      TOO_MANY;
    }
    case currefl::getOneBitSet: {
      CHECK_TRAIT_IS_REFLPROP(getOneBitSet)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(S, getOneBitSet, p0, p1)
      TOO_MANY;
    }
    case currefl::getBitsSet: {
      CHECK_TRAIT_IS_REFLPROP(getBitsSet)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) CLASS_REFLECTION(S, getBitsSet, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getBitsSetFrom: {
      CHECK_TRAIT_IS_REFLPROP(getBitsSetFrom)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(S, getBitsSetFrom, p0, p1)
      TOO_MANY;
    }
    case currefl::getHighBitsSet: {
      CHECK_TRAIT_IS_REFLPROP(getHighBitsSet)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(S, getHighBitsSet, p0, p1)
      TOO_MANY;
    }
    case currefl::getLowBitsSet: {
      CHECK_TRAIT_IS_REFLPROP(getLowBitsSet)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(S, getLowBitsSet, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getRawData, PRIM, V, getRawData)
    case_METHOD_NOARGS(operator_not, PRIM, V, operator!)
    case currefl::operator_less_less: {
      CHECK_TRAIT_IS_REFLPROP(operator_less_less)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, operator<<, p0)
      TOO_MANY;
    }
    case currefl::ashr: {
      CHECK_TRAIT_IS_REFLPROP(ashr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, ashr, p0)
      TOO_MANY;
    }
    case currefl::lshr: {
      CHECK_TRAIT_IS_REFLPROP(lshr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, lshr, p0)
      TOO_MANY;
    }
    case currefl::shl: {
      CHECK_TRAIT_IS_REFLPROP(shl)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, shl, p0)
      TOO_MANY;
    }
    case currefl::rotl: {
      CHECK_TRAIT_IS_REFLPROP(rotl)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, rotl, p0)
      TOO_MANY;
    }
    case currefl::rotr: {
      CHECK_TRAIT_IS_REFLPROP(rotr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, rotr, p0)
      TOO_MANY;
    }
    case currefl::udiv: {
      CHECK_TRAIT_IS_REFLPROP(udiv)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) CLASS_REFLECTION(V, udiv, p0)
      TOO_MANY;
    }
    case currefl::sdiv: {
      CHECK_TRAIT_IS_REFLPROP(sdiv)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(V, sdiv, p0)
      TOO_MANY;
    }
    case currefl::urem: {
      CHECK_TRAIT_IS_REFLPROP(urem)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(V, urem, p0)
      TOO_MANY;
    }
    case currefl::srem: {
      CHECK_TRAIT_IS_REFLPROP(srem)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(V, srem, p0)
      TOO_MANY;
    }
    case currefl::operator_sub: {
      CHECK_TRAIT_IS_REFLPROP(operator_sub)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(V, operator[], p0)
      TOO_MANY;
    }
    case currefl::operator_eq_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_eq_eq)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(V, operator==, p0)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_not_eq)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(V, operator!=, p0)
      TOO_MANY;
    }
    case currefl::ult: {
      CHECK_TRAIT_IS_REFLPROP(ult)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(V, ult, p0)
      TOO_MANY;
    }
    case currefl::slt: {
      CHECK_TRAIT_IS_REFLPROP(slt)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(V, slt, p0)
      TOO_MANY;
    }
    case currefl::ule: {
      CHECK_TRAIT_IS_REFLPROP(ule)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(V, ule, p0)
      TOO_MANY;
    }
    case currefl::sle: {
      CHECK_TRAIT_IS_REFLPROP(sle)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(V, sle, p0)
      TOO_MANY;
    }
    case currefl::ugt: {
      CHECK_TRAIT_IS_REFLPROP(ugt)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(V, ugt, p0)
      TOO_MANY;
    }
    case currefl::sgt: {
      CHECK_TRAIT_IS_REFLPROP(sgt)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(V, sgt, p0)
      TOO_MANY;
    }
    case currefl::uge: {
      CHECK_TRAIT_IS_REFLPROP(uge)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(V, uge, p0)
      TOO_MANY;
    }
    case currefl::sge: {
      CHECK_TRAIT_IS_REFLPROP(sge)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(V, sge, p0)
      TOO_MANY;
    }
    case currefl::trunc: {
      CHECK_TRAIT_IS_REFLPROP(trunc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, trunc, p0)
      TOO_MANY;
    }
    case currefl::sext: {
      CHECK_TRAIT_IS_REFLPROP(sext)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, sext, p0)
      TOO_MANY;
    }
    case currefl::zext: {
      CHECK_TRAIT_IS_REFLPROP(zext)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, zext, p0)
      TOO_MANY;
    }
    case currefl::sextOrTrunc: {
      CHECK_TRAIT_IS_REFLPROP(sextOrTrunc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, sextOrTrunc, p0)
      TOO_MANY;
    }
    case currefl::zextOrTrunc: {
      CHECK_TRAIT_IS_REFLPROP(zextOrTrunc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, zextOrTrunc, p0)
      TOO_MANY;
    }
    case currefl::sextOrSelf: {
      CHECK_TRAIT_IS_REFLPROP(sextOrSelf)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, sextOrSelf, p0)
      TOO_MANY;
    }
    case currefl::zextOrSelf: {
      CHECK_TRAIT_IS_REFLPROP(zextOrSelf)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, zextOrSelf, p0)
      TOO_MANY;
    }
    case currefl::extractBits: {
      CHECK_TRAIT_IS_REFLPROP(extractBits)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(V, extractBits, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getBitWidth, PRIM, V, getBitWidth)
    case_METHOD_NOARGS(getNumWords, PRIM, V, getNumWords)
    case currefl::getNumWords1: {
      CHECK_TRAIT_IS_REFLPROP(getNumWords1)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(S, getNumWords, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getActiveBits, PRIM, V, getActiveBits)
    case_METHOD_NOARGS(getActiveWords, PRIM, V, getActiveWords)
    case_METHOD_NOARGS(getMinSignedBits, PRIM, V, getMinSignedBits)
    case_METHOD_NOARGS(getZExtValue, PRIM, V, getZExtValue)
    case_METHOD_NOARGS(getSExtValue, PRIM, V, getSExtValue)
    case currefl::getBitsNeeded: {
      CHECK_TRAIT_IS_REFLPROP(getBitsNeeded)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      TRY_LOAD_VAL(unsigned char, p1)
      if (no_more) PRIM_REFLECTION(S, getBitsNeeded, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(countLeadingZeros, PRIM, V, countLeadingZeros)
    case_METHOD_NOARGS(countLeadingOnes, PRIM, V, countLeadingOnes)
    case_METHOD_NOARGS(getNumSignBits, PRIM, V, getNumSignBits)
    case_METHOD_NOARGS(countTrailingZeros, PRIM, V, countTrailingZeros)
    case_METHOD_NOARGS(countTrailingOnes, PRIM, V, countTrailingOnes)
    case_METHOD_NOARGS(countPopulation, PRIM, V, countPopulation)
    case currefl::toString: {
      CHECK_TRAIT_IS_REFLPROP(toString)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) PRIM_REFLECTION(V, toString, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(byteSwap, CLASS, V, byteSwap)
    case_METHOD_NOARGS(reverseBits, CLASS, V, reverseBits)
    case currefl::roundToDouble: {
      CHECK_TRAIT_IS_REFLPROP(roundToDouble)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) PRIM_REFLECTION(V, roundToDouble, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(roundToDouble1, PRIM, V, roundToDouble)
    case_METHOD_NOARGS(signedRoundToDouble, PRIM, V, signedRoundToDouble)
    case_METHOD_NOARGS(bitsToDouble, PRIM, V, bitsToDouble)
    case_METHOD_NOARGS(bitsToFloat, PRIM, V, bitsToFloat)
    case currefl::doubleToBits: {
      CHECK_TRAIT_IS_REFLPROP(doubleToBits)
      TRY_LOAD_VAL(double, p0)
      if (no_more) CLASS_REFLECTION(S, doubleToBits, p0)
      TOO_MANY;
    }
    case currefl::floatToBits: {
      CHECK_TRAIT_IS_REFLPROP(floatToBits)
      TRY_LOAD_VAL(float, p0)
      if (no_more) CLASS_REFLECTION(S, floatToBits, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(logBase2, PRIM, V, logBase2)
    case_METHOD_NOARGS(ceilLogBase2, PRIM, V, ceilLogBase2)
    case_METHOD_NOARGS(nearestLogBase2, PRIM, V, nearestLogBase2)
    case_METHOD_NOARGS(exactLogBase2, PRIM, V, exactLogBase2)
    case_METHOD_NOARGS(sqrt, CLASS, V, sqrt)
    case_METHOD_NOARGS(abs, CLASS, V, abs)
    case_METHOD_NOARGS(magic, CLASS, V, magic)
    case currefl::magicu: {
      CHECK_TRAIT_IS_REFLPROP(magicu)
      if (no_more) CLASS_REFLECTION_0(V, magicu)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, magicu, p0)
      TOO_MANY;
    }
    case currefl::tcSet: {
      CHECK_TRAIT_IS_REFLPROP(tcSet)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned long long, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcSet, (llvm::APInt::WordType *)p0, (llvm::APInt::WordType)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcAssign: {
      CHECK_TRAIT_IS_REFLPROP(tcAssign)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcAssign, (llvm::APInt::WordType *)p0, (const llvm::APInt::WordType *)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcIsZero: {
      CHECK_TRAIT_IS_REFLPROP(tcIsZero)
      TRY_LOAD_VAL(const unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcIsZero, p0, p1)
      TOO_MANY;
    }
    case currefl::tcExtractBit: {
      CHECK_TRAIT_IS_REFLPROP(tcExtractBit)
      TRY_LOAD_VAL(const unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcExtractBit, p0, p1)
      TOO_MANY;
    }
    case currefl::tcExtract: {
      CHECK_TRAIT_IS_REFLPROP(tcExtract)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(const unsigned long long *, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      TRY_LOAD_VAL(unsigned int, p4)
      if (no_more) VOID_REFLECTION(S, tcExtract, (llvm::APInt::WordType *)p0, (unsigned int)p1, (const llvm::APInt::WordType *)p2, (unsigned int)p3, (unsigned int)p4)
      TOO_MANY;
    }
    case currefl::tcSetBit: {
      CHECK_TRAIT_IS_REFLPROP(tcSetBit)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) VOID_REFLECTION(S, tcSetBit, (llvm::APInt::WordType *)p0, (unsigned int)p1)
      TOO_MANY;
    }
    case currefl::tcClearBit: {
      CHECK_TRAIT_IS_REFLPROP(tcClearBit)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) VOID_REFLECTION(S, tcClearBit, (llvm::APInt::WordType *)p0, (unsigned int)p1)
      TOO_MANY;
    }
    case currefl::tcLSB: {
      CHECK_TRAIT_IS_REFLPROP(tcLSB)
      TRY_LOAD_VAL(const unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcLSB, p0, p1)
      TOO_MANY;
    }
    case currefl::tcMSB: {
      CHECK_TRAIT_IS_REFLPROP(tcMSB)
      TRY_LOAD_VAL(const unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcMSB, p0, p1)
      TOO_MANY;
    }
    case currefl::tcNegate: {
      CHECK_TRAIT_IS_REFLPROP(tcNegate)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) VOID_REFLECTION(S, tcNegate, (llvm::APInt::WordType *)p0, (unsigned int)p1)
      TOO_MANY;
    }
    case currefl::tcAdd: {
      CHECK_TRAIT_IS_REFLPROP(tcAdd)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned long long, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      if (no_more) PRIM_REFLECTION(S, tcAdd, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::tcAddPart: {
      CHECK_TRAIT_IS_REFLPROP(tcAddPart)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned long long, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(S, tcAddPart, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::tcSubtract: {
      CHECK_TRAIT_IS_REFLPROP(tcSubtract)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned long long, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      if (no_more) PRIM_REFLECTION(S, tcSubtract, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::tcSubtractPart: {
      CHECK_TRAIT_IS_REFLPROP(tcSubtractPart)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned long long, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(S, tcSubtractPart, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::tcMultiplyPart: {
      CHECK_TRAIT_IS_REFLPROP(tcMultiplyPart)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned long long, p2)
      TRY_LOAD_VAL(unsigned long long, p3)
      TRY_LOAD_VAL(unsigned int, p4)
      TRY_LOAD_VAL(unsigned int, p5)
      TRY_LOAD_VAL(bool, p6)
      if (no_more) PRIM_REFLECTION(S, tcMultiplyPart, p0, p1, p2, p3, p4, p5, p6)
      TOO_MANY;
    }
    case currefl::tcMultiply: {
      CHECK_TRAIT_IS_REFLPROP(tcMultiply)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(const unsigned long long *, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      if (no_more) PRIM_REFLECTION(S, tcMultiply, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::tcFullMultiply: {
      CHECK_TRAIT_IS_REFLPROP(tcFullMultiply)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(const unsigned long long *, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      TRY_LOAD_VAL(unsigned int, p4)
      if (no_more) VOID_REFLECTION(S, tcFullMultiply, (llvm::APInt::WordType *)p0, (const llvm::APInt::WordType *)p1, (const llvm::APInt::WordType *)p2, (unsigned int)p3, (unsigned int)p4)
      TOO_MANY;
    }
    case currefl::tcDivide: {
      CHECK_TRAIT_IS_REFLPROP(tcDivide)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned long long *, p2)
      TRY_LOAD_VAL(unsigned long long *, p3)
      TRY_LOAD_VAL(unsigned int, p4)
      if (no_more) PRIM_REFLECTION(S, tcDivide, p0, p1, p2, p3, p4)
      TOO_MANY;
    }
    case currefl::tcShiftLeft: {
      CHECK_TRAIT_IS_REFLPROP(tcShiftLeft)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcShiftLeft, (llvm::APInt::WordType *)p0, (unsigned int)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcShiftRight: {
      CHECK_TRAIT_IS_REFLPROP(tcShiftRight)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcShiftRight, (llvm::APInt::WordType *)p0, (unsigned int)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcAnd: {
      CHECK_TRAIT_IS_REFLPROP(tcAnd)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcAnd, (llvm::APInt::WordType *)p0, (const llvm::APInt::WordType *)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcOr: {
      CHECK_TRAIT_IS_REFLPROP(tcOr)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcOr, (llvm::APInt::WordType *)p0, (const llvm::APInt::WordType *)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcXor: {
      CHECK_TRAIT_IS_REFLPROP(tcXor)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcXor, (llvm::APInt::WordType *)p0, (const llvm::APInt::WordType *)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::tcComplement: {
      CHECK_TRAIT_IS_REFLPROP(tcComplement)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) VOID_REFLECTION(S, tcComplement, (llvm::APInt::WordType *)p0, (unsigned int)p1)
      TOO_MANY;
    }
    case currefl::tcCompare: {
      CHECK_TRAIT_IS_REFLPROP(tcCompare)
      TRY_LOAD_VAL(const unsigned long long *, p0)
      TRY_LOAD_VAL(const unsigned long long *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(S, tcCompare, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::tcIncrement: {
      CHECK_TRAIT_IS_REFLPROP(tcIncrement)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcIncrement, p0, p1)
      TOO_MANY;
    }
    case currefl::tcDecrement: {
      CHECK_TRAIT_IS_REFLPROP(tcDecrement)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(S, tcDecrement, p0, p1)
      TOO_MANY;
    }
    case currefl::tcSetLeastSignificantBits: {
      CHECK_TRAIT_IS_REFLPROP(tcSetLeastSignificantBits)
      TRY_LOAD_VAL(unsigned long long *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, tcSetLeastSignificantBits, (llvm::APInt::WordType *)p0, (unsigned int)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(dump, VOID, V, dump)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::APInt::ms X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__APInt__ms;
# define CLASSNAME llvm::APInt::ms
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, V, s)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::APInt::mu X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__APInt__mu;
# define CLASSNAME llvm::APInt::mu
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, V, a)
    case_FIELD(PRIM, V, s)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::StringRef X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__StringRef;
# define CLASSNAME llvm::StringRef
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(V)
      TOO_MANY;
};
    case currefl::withNullAsEmpty: {
      CHECK_TRAIT_IS_REFLPROP(withNullAsEmpty)
      TRY_LOAD_VAL(const char *, p0)
      if (no_more) CLASS_REFLECTION(S, withNullAsEmpty, p0)
      TOO_MANY;
    }
    case currefl::bytes: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::bytes))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(V, bytes)
      TOO_MANY;
    }
    case_METHOD_NOARGS(data, PRIM, V, data)
    case_METHOD_NOARGS(empty, PRIM, V, empty)
    case_METHOD_NOARGS(size, PRIM, V, size)
    case_METHOD_NOARGS(front, PRIM, V, front)
    case_METHOD_NOARGS(back, PRIM, V, back)
    case currefl::equals: {
      CHECK_TRAIT_IS_REFLPROP(equals)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, equals, p0)
      TOO_MANY;
    }
    case currefl::equals_lower: {
      CHECK_TRAIT_IS_REFLPROP(equals_lower)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, equals_lower, p0)
      TOO_MANY;
    }
    case currefl::compare: {
      CHECK_TRAIT_IS_REFLPROP(compare)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, compare, p0)
      TOO_MANY;
    }
    case currefl::compare_lower: {
      CHECK_TRAIT_IS_REFLPROP(compare_lower)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, compare_lower, p0)
      TOO_MANY;
    }
    case currefl::compare_numeric: {
      CHECK_TRAIT_IS_REFLPROP(compare_numeric)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, compare_numeric, p0)
      TOO_MANY;
    }
    case currefl::edit_distance: {
      CHECK_TRAIT_IS_REFLPROP(edit_distance)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, edit_distance, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) PRIM_REFLECTION(V, edit_distance, p0, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(V, edit_distance, p0, p1, p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(str, PRIM, V, str)
    case currefl::operator_sub: {
      CHECK_TRAIT_IS_REFLPROP(operator_sub)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) PRIM_REFLECTION(V, operator[], p0)
      TOO_MANY;
    }
    case currefl::startswith: {
      CHECK_TRAIT_IS_REFLPROP(startswith)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, startswith, p0)
      TOO_MANY;
    }
    case currefl::startswith_lower: {
      CHECK_TRAIT_IS_REFLPROP(startswith_lower)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, startswith_lower, p0)
      TOO_MANY;
    }
    case currefl::endswith: {
      CHECK_TRAIT_IS_REFLPROP(endswith)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, endswith, p0)
      TOO_MANY;
    }
    case currefl::endswith_lower: {
      CHECK_TRAIT_IS_REFLPROP(endswith_lower)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, endswith_lower, p0)
      TOO_MANY;
    }
    case currefl::find: {
      CHECK_TRAIT_IS_REFLPROP(find)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(V, find, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find, p0, p1)
      TOO_MANY;
    }
    case currefl::find_lower: {
      CHECK_TRAIT_IS_REFLPROP(find_lower)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(V, find_lower, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find_lower, p0, p1)
      TOO_MANY;
    }
    case currefl::find1: {
      CHECK_TRAIT_IS_REFLPROP(find1)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, find, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find, p0, p1)
      TOO_MANY;
    }
    case currefl::find_lower1: {
      CHECK_TRAIT_IS_REFLPROP(find_lower1)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, find_lower, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find_lower, p0, p1)
      TOO_MANY;
    }
    case currefl::rfind: {
      CHECK_TRAIT_IS_REFLPROP(rfind)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(V, rfind, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, rfind, p0, p1)
      TOO_MANY;
    }
    case currefl::rfind_lower: {
      CHECK_TRAIT_IS_REFLPROP(rfind_lower)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(V, rfind_lower, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, rfind_lower, p0, p1)
      TOO_MANY;
    }
    case currefl::rfind1: {
      CHECK_TRAIT_IS_REFLPROP(rfind1)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, rfind, p0)
      TOO_MANY;
    }
    case currefl::rfind_lower1: {
      CHECK_TRAIT_IS_REFLPROP(rfind_lower1)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, rfind_lower, p0)
      TOO_MANY;
    }
    case currefl::find_first_of: {
      CHECK_TRAIT_IS_REFLPROP(find_first_of)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(V, find_first_of, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find_first_of, p0, p1)
      TOO_MANY;
    }
    case currefl::find_first_of1: {
      CHECK_TRAIT_IS_REFLPROP(find_first_of1)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, find_first_of, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find_first_of, p0, p1)
      TOO_MANY;
    }
    case currefl::find_first_not_of: {
      CHECK_TRAIT_IS_REFLPROP(find_first_not_of)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(V, find_first_not_of, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find_first_not_of, p0, p1)
      TOO_MANY;
    }
    case currefl::find_first_not_of1: {
      CHECK_TRAIT_IS_REFLPROP(find_first_not_of1)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, find_first_not_of, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find_first_not_of, p0, p1)
      TOO_MANY;
    }
    case currefl::find_last_of: {
      CHECK_TRAIT_IS_REFLPROP(find_last_of)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(V, find_last_of, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find_last_of, p0, p1)
      TOO_MANY;
    }
    case currefl::find_last_of1: {
      CHECK_TRAIT_IS_REFLPROP(find_last_of1)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, find_last_of, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find_last_of, p0, p1)
      TOO_MANY;
    }
    case currefl::find_last_not_of: {
      CHECK_TRAIT_IS_REFLPROP(find_last_not_of)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(V, find_last_not_of, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find_last_not_of, p0, p1)
      TOO_MANY;
    }
    case currefl::find_last_not_of1: {
      CHECK_TRAIT_IS_REFLPROP(find_last_not_of1)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, find_last_not_of, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) PRIM_REFLECTION(V, find_last_not_of, p0, p1)
      TOO_MANY;
    }
    case currefl::contains: {
      CHECK_TRAIT_IS_REFLPROP(contains)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, contains, p0)
      TOO_MANY;
    }
    case currefl::contains1: {
      CHECK_TRAIT_IS_REFLPROP(contains1)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(V, contains, p0)
      TOO_MANY;
    }
    case currefl::contains_lower: {
      CHECK_TRAIT_IS_REFLPROP(contains_lower)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, contains_lower, p0)
      TOO_MANY;
    }
    case currefl::contains_lower1: {
      CHECK_TRAIT_IS_REFLPROP(contains_lower1)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(V, contains_lower, p0)
      TOO_MANY;
    }
    case currefl::count: {
      CHECK_TRAIT_IS_REFLPROP(count)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(V, count, p0)
      TOO_MANY;
    }
    case currefl::count1: {
      CHECK_TRAIT_IS_REFLPROP(count1)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, count, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(lower, PRIM, V, lower)
    case_METHOD_NOARGS(upper, PRIM, V, upper)
    case currefl::substr: {
      CHECK_TRAIT_IS_REFLPROP(substr)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) CLASS_REFLECTION(V, substr, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) CLASS_REFLECTION(V, substr, p0, p1)
      TOO_MANY;
    }
    case currefl::take_front: {
      CHECK_TRAIT_IS_REFLPROP(take_front)
      if (no_more) CLASS_REFLECTION_0(V, take_front)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) CLASS_REFLECTION(V, take_front, p0)
      TOO_MANY;
    }
    case currefl::take_back: {
      CHECK_TRAIT_IS_REFLPROP(take_back)
      if (no_more) CLASS_REFLECTION_0(V, take_back)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) CLASS_REFLECTION(V, take_back, p0)
      TOO_MANY;
    }
    case currefl::drop_front: {
      CHECK_TRAIT_IS_REFLPROP(drop_front)
      if (no_more) CLASS_REFLECTION_0(V, drop_front)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) CLASS_REFLECTION(V, drop_front, p0)
      TOO_MANY;
    }
    case currefl::drop_back: {
      CHECK_TRAIT_IS_REFLPROP(drop_back)
      if (no_more) CLASS_REFLECTION_0(V, drop_back)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) CLASS_REFLECTION(V, drop_back, p0)
      TOO_MANY;
    }
    case currefl::slice: {
      CHECK_TRAIT_IS_REFLPROP(slice)
      TRY_LOAD_VAL(unsigned long, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) CLASS_REFLECTION(V, slice, p0, p1)
      TOO_MANY;
    }
    case currefl::ltrim: {
      CHECK_TRAIT_IS_REFLPROP(ltrim)
      TRY_LOAD_VAL(char, p0)
      if (no_more) CLASS_REFLECTION(V, ltrim, p0)
      TOO_MANY;
    }
    case currefl::ltrim1: {
      CHECK_TRAIT_IS_REFLPROP(ltrim1)
      if (no_more) CLASS_REFLECTION_0(V, ltrim)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(V, ltrim, p0)
      TOO_MANY;
    }
    case currefl::rtrim: {
      CHECK_TRAIT_IS_REFLPROP(rtrim)
      TRY_LOAD_VAL(char, p0)
      if (no_more) CLASS_REFLECTION(V, rtrim, p0)
      TOO_MANY;
    }
    case currefl::rtrim1: {
      CHECK_TRAIT_IS_REFLPROP(rtrim1)
      if (no_more) CLASS_REFLECTION_0(V, rtrim)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(V, rtrim, p0)
      TOO_MANY;
    }
    case currefl::trim: {
      CHECK_TRAIT_IS_REFLPROP(trim)
      TRY_LOAD_VAL(char, p0)
      if (no_more) CLASS_REFLECTION(V, trim, p0)
      TOO_MANY;
    }
    case currefl::trim1: {
      CHECK_TRAIT_IS_REFLPROP(trim1)
      if (no_more) CLASS_REFLECTION_0(V, trim)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(V, trim, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::APFloatBase *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__APFloatBase;
# define CLASSNAME llvm::APFloatBase
  switch( (currefl::memnames)N ) {
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::APFloatBase X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__APFloatBase;
# define CLASSNAME llvm::APFloatBase
  switch( (currefl::memnames)N ) {
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::APFloat *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__APFloat;
# define CLASSNAME llvm::APFloat
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(needsCleanup, PRIM, P, needsCleanup)
    case currefl::getAllOnesValue: {
      CHECK_TRAIT_IS_REFLPROP(getAllOnesValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getAllOnesValue, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(S, getAllOnesValue, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(bitcastToAPInt, CLASS, P, bitcastToAPInt)
    case_METHOD_NOARGS(convertToDouble, PRIM, P, convertToDouble)
    case_METHOD_NOARGS(convertToFloat, PRIM, P, convertToFloat)
    case currefl::isExactlyValue: {
      CHECK_TRAIT_IS_REFLPROP(isExactlyValue)
      TRY_LOAD_VAL(double, p0)
      if (no_more) PRIM_REFLECTION(P, isExactlyValue, p0)
      TOO_MANY;
    }
    case currefl::convertToHexString: {
      CHECK_TRAIT_IS_REFLPROP(convertToHexString)
      TRY_LOAD_VAL(char *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(bool, p2)
      TRY_LOAD_VAL(enum llvm::APFloatBase::roundingMode, p3)
      if (no_more) PRIM_REFLECTION(P, convertToHexString, p0, p1, p2, p3)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isZero, PRIM, P, isZero)
    case_METHOD_NOARGS(isInfinity, PRIM, P, isInfinity)
    case_METHOD_NOARGS(isNaN, PRIM, P, isNaN)
    case_METHOD_NOARGS(isNegative, PRIM, P, isNegative)
    case_METHOD_NOARGS(isDenormal, PRIM, P, isDenormal)
    case_METHOD_NOARGS(isSignaling, PRIM, P, isSignaling)
    case_METHOD_NOARGS(isNormal, PRIM, P, isNormal)
    case_METHOD_NOARGS(isFinite, PRIM, P, isFinite)
    case_METHOD_NOARGS(getCategory, PRIM, P, getCategory)
    case_METHOD_NOARGS(isNonZero, PRIM, P, isNonZero)
    case_METHOD_NOARGS(isFiniteNonZero, PRIM, P, isFiniteNonZero)
    case_METHOD_NOARGS(isPosZero, PRIM, P, isPosZero)
    case_METHOD_NOARGS(isNegZero, PRIM, P, isNegZero)
    case_METHOD_NOARGS(isSmallest, PRIM, P, isSmallest)
    case_METHOD_NOARGS(isLargest, PRIM, P, isLargest)
    case_METHOD_NOARGS(isInteger, PRIM, P, isInteger)
    case_METHOD_NOARGS(dump, VOID, P, dump)
    case currefl::getExactInverse: {
      CHECK_TRAIT_IS_REFLPROP(getExactInverse)
      TRY_LOAD_VAL(class llvm::APFloat *, p0)
      if (no_more) PRIM_REFLECTION(P, getExactInverse, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::APFloat const &X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__APFloat;
# define CLASSNAME llvm::APFloat
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(needsCleanup, PRIM, V, needsCleanup)
    case currefl::getAllOnesValue: {
      CHECK_TRAIT_IS_REFLPROP(getAllOnesValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getAllOnesValue, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(S, getAllOnesValue, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(bitcastToAPInt, CLASS, V, bitcastToAPInt)
    case_METHOD_NOARGS(convertToDouble, PRIM, V, convertToDouble)
    case_METHOD_NOARGS(convertToFloat, PRIM, V, convertToFloat)
    case currefl::isExactlyValue: {
      CHECK_TRAIT_IS_REFLPROP(isExactlyValue)
      TRY_LOAD_VAL(double, p0)
      if (no_more) PRIM_REFLECTION(V, isExactlyValue, p0)
      TOO_MANY;
    }
    case currefl::convertToHexString: {
      CHECK_TRAIT_IS_REFLPROP(convertToHexString)
      TRY_LOAD_VAL(char *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(bool, p2)
      TRY_LOAD_VAL(enum llvm::APFloatBase::roundingMode, p3)
      if (no_more) PRIM_REFLECTION(V, convertToHexString, p0, p1, p2, p3)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isZero, PRIM, V, isZero)
    case_METHOD_NOARGS(isInfinity, PRIM, V, isInfinity)
    case_METHOD_NOARGS(isNaN, PRIM, V, isNaN)
    case_METHOD_NOARGS(isNegative, PRIM, V, isNegative)
    case_METHOD_NOARGS(isDenormal, PRIM, V, isDenormal)
    case_METHOD_NOARGS(isSignaling, PRIM, V, isSignaling)
    case_METHOD_NOARGS(isNormal, PRIM, V, isNormal)
    case_METHOD_NOARGS(isFinite, PRIM, V, isFinite)
    case_METHOD_NOARGS(getCategory, PRIM, V, getCategory)
    case_METHOD_NOARGS(isNonZero, PRIM, V, isNonZero)
    case_METHOD_NOARGS(isFiniteNonZero, PRIM, V, isFiniteNonZero)
    case_METHOD_NOARGS(isPosZero, PRIM, V, isPosZero)
    case_METHOD_NOARGS(isNegZero, PRIM, V, isNegZero)
    case_METHOD_NOARGS(isSmallest, PRIM, V, isSmallest)
    case_METHOD_NOARGS(isLargest, PRIM, V, isLargest)
    case_METHOD_NOARGS(isInteger, PRIM, V, isInteger)
    case_METHOD_NOARGS(dump, VOID, V, dump)
    case currefl::getExactInverse: {
      CHECK_TRAIT_IS_REFLPROP(getExactInverse)
      TRY_LOAD_VAL(class llvm::APFloat *, p0)
      if (no_more) PRIM_REFLECTION(V, getExactInverse, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::APSInt *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__APSInt;
# define CLASSNAME llvm::APSInt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isSigned, PRIM, P, isSigned)
    case_METHOD_NOARGS(isUnsigned, PRIM, P, isUnsigned)
    case currefl::toString: {
      CHECK_TRAIT_IS_REFLPROP(toString)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, toString, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getExtValue, PRIM, P, getExtValue)
    case currefl::trunc: {
      CHECK_TRAIT_IS_REFLPROP(trunc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, trunc, p0)
      TOO_MANY;
    }
    case currefl::extend: {
      CHECK_TRAIT_IS_REFLPROP(extend)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, extend, p0)
      TOO_MANY;
    }
    case currefl::extOrTrunc: {
      CHECK_TRAIT_IS_REFLPROP(extOrTrunc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, extOrTrunc, p0)
      TOO_MANY;
    }
    case currefl::operator_gr_gr: {
      CHECK_TRAIT_IS_REFLPROP(operator_gr_gr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, operator>>, p0)
      TOO_MANY;
    }
    case currefl::operator_eq_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_eq_eq)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(P, operator==, p0)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_not_eq)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(P, operator!=, p0)
      TOO_MANY;
    }
    case currefl::operator_less_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_less_eq)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(P, operator<=, p0)
      TOO_MANY;
    }
    case currefl::operator_gr_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_gr_eq)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(P, operator>=, p0)
      TOO_MANY;
    }
    case currefl::operator_less: {
      CHECK_TRAIT_IS_REFLPROP(operator_less)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(P, operator<, p0)
      TOO_MANY;
    }
    case currefl::operator_gr: {
      CHECK_TRAIT_IS_REFLPROP(operator_gr)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(P, operator>, p0)
      TOO_MANY;
    }
    case currefl::operator_less_less: {
      CHECK_TRAIT_IS_REFLPROP(operator_less_less)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, operator<<, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(operator_minus, CLASS, P, operator-)
    case_METHOD_NOARGS(operator_tilde, CLASS, P, operator~)
    case currefl::getMaxValue: {
      CHECK_TRAIT_IS_REFLPROP(getMaxValue)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(S, getMaxValue, p0, p1)
      TOO_MANY;
    }
    case currefl::getMinValue: {
      CHECK_TRAIT_IS_REFLPROP(getMinValue)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(S, getMinValue, p0, p1)
      TOO_MANY;
    }
    case currefl::get: {
      CHECK_TRAIT_IS_REFLPROP(get)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(S, get, p0)
      TOO_MANY;
    }
    case currefl::getUnsigned: {
      CHECK_TRAIT_IS_REFLPROP(getUnsigned)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) CLASS_REFLECTION(S, getUnsigned, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::APSInt X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__APSInt;
# define CLASSNAME llvm::APSInt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isSigned, PRIM, V, isSigned)
    case_METHOD_NOARGS(isUnsigned, PRIM, V, isUnsigned)
    case currefl::toString: {
      CHECK_TRAIT_IS_REFLPROP(toString)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(V, toString, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getExtValue, PRIM, V, getExtValue)
    case currefl::trunc: {
      CHECK_TRAIT_IS_REFLPROP(trunc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, trunc, p0)
      TOO_MANY;
    }
    case currefl::extend: {
      CHECK_TRAIT_IS_REFLPROP(extend)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, extend, p0)
      TOO_MANY;
    }
    case currefl::extOrTrunc: {
      CHECK_TRAIT_IS_REFLPROP(extOrTrunc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, extOrTrunc, p0)
      TOO_MANY;
    }
    case currefl::operator_gr_gr: {
      CHECK_TRAIT_IS_REFLPROP(operator_gr_gr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, operator>>, p0)
      TOO_MANY;
    }
    case currefl::operator_eq_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_eq_eq)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(V, operator==, p0)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_not_eq)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(V, operator!=, p0)
      TOO_MANY;
    }
    case currefl::operator_less_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_less_eq)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(V, operator<=, p0)
      TOO_MANY;
    }
    case currefl::operator_gr_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_gr_eq)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(V, operator>=, p0)
      TOO_MANY;
    }
    case currefl::operator_less: {
      CHECK_TRAIT_IS_REFLPROP(operator_less)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(V, operator<, p0)
      TOO_MANY;
    }
    case currefl::operator_gr: {
      CHECK_TRAIT_IS_REFLPROP(operator_gr)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) PRIM_REFLECTION(V, operator>, p0)
      TOO_MANY;
    }
    case currefl::operator_less_less: {
      CHECK_TRAIT_IS_REFLPROP(operator_less_less)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, operator<<, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(operator_minus, CLASS, V, operator-)
    case_METHOD_NOARGS(operator_tilde, CLASS, V, operator~)
    case currefl::getMaxValue: {
      CHECK_TRAIT_IS_REFLPROP(getMaxValue)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(S, getMaxValue, p0, p1)
      TOO_MANY;
    }
    case currefl::getMinValue: {
      CHECK_TRAIT_IS_REFLPROP(getMinValue)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(S, getMinValue, p0, p1)
      TOO_MANY;
    }
    case currefl::get: {
      CHECK_TRAIT_IS_REFLPROP(get)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(S, get, p0)
      TOO_MANY;
    }
    case currefl::getUnsigned: {
      CHECK_TRAIT_IS_REFLPROP(getUnsigned)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) CLASS_REFLECTION(S, getUnsigned, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::APValue *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__APValue;
# define CLASSNAME clang::APValue
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(needsCleanup, PRIM, P, needsCleanup)
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(isUninit, PRIM, P, isUninit)
    case_METHOD_NOARGS(isInt, PRIM, P, isInt)
    case_METHOD_NOARGS(isFloat, PRIM, P, isFloat)
    case_METHOD_NOARGS(isComplexInt, PRIM, P, isComplexInt)
    case_METHOD_NOARGS(isComplexFloat, PRIM, P, isComplexFloat)
    case_METHOD_NOARGS(isLValue, PRIM, P, isLValue)
    case_METHOD_NOARGS(isVector, PRIM, P, isVector)
    case_METHOD_NOARGS(isArray, PRIM, P, isArray)
    case_METHOD_NOARGS(isStruct, PRIM, P, isStruct)
    case_METHOD_NOARGS(isUnion, PRIM, P, isUnion)
    case_METHOD_NOARGS(isMemberPointer, PRIM, P, isMemberPointer)
    case_METHOD_NOARGS(isAddrLabelDiff, PRIM, P, isAddrLabelDiff)
    case_METHOD_NOARGS(dump, VOID, P, dump)
    case_METHOD_NOARGS(getInt, CLASS, P, getInt)
    case_METHOD_NOARGS(getFloat, CLASS, P, getFloat)
    case_METHOD_NOARGS(getComplexIntReal, CLASS, P, getComplexIntReal)
    case_METHOD_NOARGS(getComplexIntImag, CLASS, P, getComplexIntImag)
    case_METHOD_NOARGS(getComplexFloatReal, CLASS, P, getComplexFloatReal)
    case_METHOD_NOARGS(getComplexFloatImag, CLASS, P, getComplexFloatImag)
    case_METHOD_NOARGS(getLValueBase, CLASS, P, getLValueBase)
    case_METHOD_NOARGS(getLValueOffset, CLASS, P, getLValueOffset)
    case_METHOD_NOARGS(isLValueOnePastTheEnd, PRIM, P, isLValueOnePastTheEnd)
    case_METHOD_NOARGS(hasLValuePath, PRIM, P, hasLValuePath)
    case_METHOD_NOARGS(getLValueCallIndex, PRIM, P, getLValueCallIndex)
    case_METHOD_NOARGS(getLValueVersion, PRIM, P, getLValueVersion)
    case_METHOD_NOARGS(isNullPointer, PRIM, P, isNullPointer)
    case currefl::getVectorElt: {
      CHECK_TRAIT_IS_REFLPROP(getVectorElt)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getVectorElt, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getVectorLength, PRIM, P, getVectorLength)
    case currefl::getArrayInitializedElt: {
      CHECK_TRAIT_IS_REFLPROP(getArrayInitializedElt)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArrayInitializedElt, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasArrayFiller, PRIM, P, hasArrayFiller)
    case_METHOD_NOARGS(getArrayFiller, CLASS, P, getArrayFiller)
    case_METHOD_NOARGS(getArrayInitializedElts, PRIM, P, getArrayInitializedElts)
    case_METHOD_NOARGS(getArraySize, PRIM, P, getArraySize)
    case_METHOD_NOARGS(getStructNumBases, PRIM, P, getStructNumBases)
    case_METHOD_NOARGS(getStructNumFields, PRIM, P, getStructNumFields)
    case currefl::getStructBase: {
      CHECK_TRAIT_IS_REFLPROP(getStructBase)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getStructBase, p0)
      TOO_MANY;
    }
    case currefl::getStructField: {
      CHECK_TRAIT_IS_REFLPROP(getStructField)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getStructField, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getUnionField, CLASS, P, getUnionField)
    case_METHOD_NOARGS(getUnionValue, CLASS, P, getUnionValue)
    case_METHOD_NOARGS(getMemberPointerDecl, CLASS, P, getMemberPointerDecl)
    case_METHOD_NOARGS(isMemberPointerToDerivedMember, PRIM, P, isMemberPointerToDerivedMember)
    case currefl::getMemberPointerPath: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getMemberPointerPath))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getMemberPointerPath)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAddrLabelDiffLHS, CLASS, P, getAddrLabelDiffLHS)
    case_METHOD_NOARGS(getAddrLabelDiffRHS, CLASS, P, getAddrLabelDiffRHS)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::APValue::LValueBase X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__APValue__LValueBase;
# define CLASSNAME clang::APValue::LValueBase
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOpaqueValue, PRIM, V, getOpaqueValue)
    case_METHOD_NOARGS(isNull, PRIM, V, isNull)
    case_METHOD_NOARGS(operator_bool, PRIM, V, operator bool)
    case_METHOD_NOARGS(getCallIndex, PRIM, V, getCallIndex)
    case_METHOD_NOARGS(getVersion, PRIM, V, getVersion)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DiagnosticOptions *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DiagnosticOptions;
# define CLASSNAME clang::DiagnosticOptions
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, P, IgnoreWarnings)
    case_FIELD(PRIM, P, NoRewriteMacros)
    case_FIELD(PRIM, P, Pedantic)
    case_FIELD(PRIM, P, PedanticErrors)
    case_FIELD(PRIM, P, ShowColumn)
    case_FIELD(PRIM, P, ShowLocation)
    case_FIELD(PRIM, P, AbsolutePath)
    case_FIELD(PRIM, P, ShowCarets)
    case_FIELD(PRIM, P, ShowFixits)
    case_FIELD(PRIM, P, ShowSourceRanges)
    case_FIELD(PRIM, P, ShowParseableFixits)
    case_FIELD(PRIM, P, ShowPresumedLoc)
    case_FIELD(PRIM, P, ShowOptionNames)
    case_FIELD(PRIM, P, ShowNoteIncludeStack)
    case_FIELD(PRIM, P, ShowCategories)
    case_FIELD(PRIM, P, ShowColors)
    case_FIELD(PRIM, P, VerifyDiagnostics)
    case_FIELD(PRIM, P, ElideType)
    case_FIELD(PRIM, P, ShowTemplateTree)
    case_FIELD(PRIM, P, CLFallbackMode)
    case_FIELD(PRIM, P, ErrorLimit)
    case_FIELD(PRIM, P, MacroBacktraceLimit)
    case_FIELD(PRIM, P, TemplateBacktraceLimit)
    case_FIELD(PRIM, P, ConstexprBacktraceLimit)
    case_FIELD(PRIM, P, SpellCheckingLimit)
    case_FIELD(PRIM, P, SnippetLineLimit)
    case_FIELD(PRIM, P, TabStop)
    case_FIELD(PRIM, P, MessageLength)
    case_METHOD_NOARGS(getFormat, PRIM, P, getFormat)
    case_METHOD_NOARGS(getShowOverloads, PRIM, P, getShowOverloads)
    case_METHOD_NOARGS(getVerifyIgnoreUnexpected, PRIM, P, getVerifyIgnoreUnexpected)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FileID X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FileID;
# define CLASSNAME clang::FileID
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isValid, PRIM, V, isValid)
    case_METHOD_NOARGS(isInvalid, PRIM, V, isInvalid)
    case_METHOD_NOARGS(getSentinel, CLASS, S, getSentinel)
    case_METHOD_NOARGS(getHashValue, PRIM, V, getHashValue)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SourceLocation *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SourceLocation;
# define CLASSNAME clang::SourceLocation
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isFileID, PRIM, P, isFileID)
    case_METHOD_NOARGS(isMacroID, PRIM, P, isMacroID)
    case_METHOD_NOARGS(isValid, PRIM, P, isValid)
    case_METHOD_NOARGS(isInvalid, PRIM, P, isInvalid)
    case currefl::getLocWithOffset: {
      CHECK_TRAIT_IS_REFLPROP(getLocWithOffset)
      TRY_LOAD_VAL(int, p0)
      if (no_more) CLASS_REFLECTION(P, getLocWithOffset, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getRawEncoding, PRIM, P, getRawEncoding)
    case currefl::getFromRawEncoding: {
      CHECK_TRAIT_IS_REFLPROP(getFromRawEncoding)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getFromRawEncoding, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getPtrEncoding, PRIM, P, getPtrEncoding)
    case currefl::getFromPtrEncoding: {
      CHECK_TRAIT_IS_REFLPROP(getFromPtrEncoding)
      TRY_LOAD_VAL(const void *, p0)
      if (no_more) CLASS_REFLECTION(S, getFromPtrEncoding, p0)
      TOO_MANY;
    }
    case currefl::isPairOfFileLocations: {
      CHECK_TRAIT_IS_REFLPROP(isPairOfFileLocations)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) PRIM_REFLECTION(S, isPairOfFileLocations, p0, p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SourceLocation X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SourceLocation;
# define CLASSNAME clang::SourceLocation
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isFileID, PRIM, V, isFileID)
    case_METHOD_NOARGS(isMacroID, PRIM, V, isMacroID)
    case_METHOD_NOARGS(isValid, PRIM, V, isValid)
    case_METHOD_NOARGS(isInvalid, PRIM, V, isInvalid)
    case currefl::getLocWithOffset: {
      CHECK_TRAIT_IS_REFLPROP(getLocWithOffset)
      TRY_LOAD_VAL(int, p0)
      if (no_more) CLASS_REFLECTION(V, getLocWithOffset, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getRawEncoding, PRIM, V, getRawEncoding)
    case currefl::getFromRawEncoding: {
      CHECK_TRAIT_IS_REFLPROP(getFromRawEncoding)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getFromRawEncoding, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getPtrEncoding, PRIM, V, getPtrEncoding)
    case currefl::getFromPtrEncoding: {
      CHECK_TRAIT_IS_REFLPROP(getFromPtrEncoding)
      TRY_LOAD_VAL(const void *, p0)
      if (no_more) CLASS_REFLECTION(S, getFromPtrEncoding, p0)
      TOO_MANY;
    }
    case currefl::isPairOfFileLocations: {
      CHECK_TRAIT_IS_REFLPROP(isPairOfFileLocations)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) PRIM_REFLECTION(S, isPairOfFileLocations, p0, p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SourceRange X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SourceRange;
# define CLASSNAME clang::SourceRange
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBegin, CLASS, V, getBegin)
    case_METHOD_NOARGS(getEnd, CLASS, V, getEnd)
    case_METHOD_NOARGS(isValid, PRIM, V, isValid)
    case_METHOD_NOARGS(isInvalid, PRIM, V, isInvalid)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CharSourceRange X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__CharSourceRange;
# define CLASSNAME clang::CharSourceRange
  switch( (currefl::memnames)N ) {
    case currefl::getTokenRange: {
      CHECK_TRAIT_IS_REFLPROP(getTokenRange)
      TRY_LOAD_VAL(class clang::SourceRange, p0)
      if (no_more) CLASS_REFLECTION(S, getTokenRange, p0)
      TOO_MANY;
    }
    case currefl::getCharRange: {
      CHECK_TRAIT_IS_REFLPROP(getCharRange)
      TRY_LOAD_VAL(class clang::SourceRange, p0)
      if (no_more) CLASS_REFLECTION(S, getCharRange, p0)
      TOO_MANY;
    }
    case currefl::getTokenRange1: {
      CHECK_TRAIT_IS_REFLPROP(getTokenRange1)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) CLASS_REFLECTION(S, getTokenRange, p0, p1)
      TOO_MANY;
    }
    case currefl::getCharRange1: {
      CHECK_TRAIT_IS_REFLPROP(getCharRange1)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) CLASS_REFLECTION(S, getCharRange, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isTokenRange, PRIM, V, isTokenRange)
    case_METHOD_NOARGS(isCharRange, PRIM, V, isCharRange)
    case_METHOD_NOARGS(getBegin, CLASS, V, getBegin)
    case_METHOD_NOARGS(getEnd, CLASS, V, getEnd)
    case_METHOD_NOARGS(getAsRange, CLASS, V, getAsRange)
    case_METHOD_NOARGS(isValid, PRIM, V, isValid)
    case_METHOD_NOARGS(isInvalid, PRIM, V, isInvalid)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::PresumedLoc X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__PresumedLoc;
# define CLASSNAME clang::PresumedLoc
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isInvalid, PRIM, V, isInvalid)
    case_METHOD_NOARGS(isValid, PRIM, V, isValid)
    case_METHOD_NOARGS(getFilename, PRIM, V, getFilename)
    case_METHOD_NOARGS(getLine, PRIM, V, getLine)
    case_METHOD_NOARGS(getColumn, PRIM, V, getColumn)
    case_METHOD_NOARGS(getIncludeLoc, CLASS, V, getIncludeLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FullSourceLoc *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FullSourceLoc;
# define CLASSNAME clang::FullSourceLoc
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasManager, PRIM, P, hasManager)
    case_METHOD_NOARGS(getManager, CLASS, P, getManager)
    case_METHOD_NOARGS(getFileID, CLASS, P, getFileID)
    case_METHOD_NOARGS(getExpansionLoc, CLASS, P, getExpansionLoc)
    case_METHOD_NOARGS(getSpellingLoc, CLASS, P, getSpellingLoc)
    case_METHOD_NOARGS(getFileLoc, CLASS, P, getFileLoc)
    case currefl::getPresumedLoc: {
      CHECK_TRAIT_IS_REFLPROP(getPresumedLoc)
      if (no_more) CLASS_REFLECTION_0(P, getPresumedLoc)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(P, getPresumedLoc, p0)
      TOO_MANY;
    }
    case currefl::isMacroArgExpansion: {
      CHECK_TRAIT_IS_REFLPROP(isMacroArgExpansion)
      if (no_more) PRIM_REFLECTION_0(P, isMacroArgExpansion)
      TRY_LOAD_VAL(class clang::FullSourceLoc *, p0)
      if (no_more) PRIM_REFLECTION(P, isMacroArgExpansion, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getImmediateMacroCallerLoc, CLASS, P, getImmediateMacroCallerLoc)
    case_METHOD_NOARGS(getFileOffset, PRIM, P, getFileOffset)
    case currefl::getExpansionLineNumber: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionLineNumber)
      if (no_more) PRIM_REFLECTION_0(P, getExpansionLineNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(P, getExpansionLineNumber, p0)
      TOO_MANY;
    }
    case currefl::getExpansionColumnNumber: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionColumnNumber)
      if (no_more) PRIM_REFLECTION_0(P, getExpansionColumnNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(P, getExpansionColumnNumber, p0)
      TOO_MANY;
    }
    case currefl::getSpellingLineNumber: {
      CHECK_TRAIT_IS_REFLPROP(getSpellingLineNumber)
      if (no_more) PRIM_REFLECTION_0(P, getSpellingLineNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(P, getSpellingLineNumber, p0)
      TOO_MANY;
    }
    case currefl::getSpellingColumnNumber: {
      CHECK_TRAIT_IS_REFLPROP(getSpellingColumnNumber)
      if (no_more) PRIM_REFLECTION_0(P, getSpellingColumnNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(P, getSpellingColumnNumber, p0)
      TOO_MANY;
    }
    case currefl::getCharacterData: {
      CHECK_TRAIT_IS_REFLPROP(getCharacterData)
      if (no_more) PRIM_REFLECTION_0(P, getCharacterData)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(P, getCharacterData, p0)
      TOO_MANY;
    }
    case currefl::getLineNumber: {
      CHECK_TRAIT_IS_REFLPROP(getLineNumber)
      if (no_more) PRIM_REFLECTION_0(P, getLineNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(P, getLineNumber, p0)
      TOO_MANY;
    }
    case currefl::getColumnNumber: {
      CHECK_TRAIT_IS_REFLPROP(getColumnNumber)
      if (no_more) PRIM_REFLECTION_0(P, getColumnNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(P, getColumnNumber, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getFileEntry, CLASS, P, getFileEntry)
    case currefl::getBufferData: {
      CHECK_TRAIT_IS_REFLPROP(getBufferData)
      if (no_more) CLASS_REFLECTION_0(P, getBufferData)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) CLASS_REFLECTION(P, getBufferData, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isInSystemHeader, PRIM, P, isInSystemHeader)
    case currefl::isBeforeInTranslationUnitThan: {
      CHECK_TRAIT_IS_REFLPROP(isBeforeInTranslationUnitThan)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isBeforeInTranslationUnitThan, p0)
      TOO_MANY;
    }
    case currefl::isBeforeInTranslationUnitThan1: {
      CHECK_TRAIT_IS_REFLPROP(isBeforeInTranslationUnitThan1)
      TRY_LOAD_VAL(class clang::FullSourceLoc, p0)
      if (no_more) PRIM_REFLECTION(P, isBeforeInTranslationUnitThan, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(dump, VOID, P, dump)
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(const class clang::FullSourceLoc &, p0 )
      TRY_LOAD_VAL(const class clang::FullSourceLoc &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(const class clang::FullSourceLoc &, p0 )
      TRY_LOAD_VAL(const class clang::FullSourceLoc &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FullSourceLoc X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FullSourceLoc;
# define CLASSNAME clang::FullSourceLoc
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasManager, PRIM, V, hasManager)
    case_METHOD_NOARGS(getManager, CLASS, V, getManager)
    case_METHOD_NOARGS(getFileID, CLASS, V, getFileID)
    case_METHOD_NOARGS(getExpansionLoc, CLASS, V, getExpansionLoc)
    case_METHOD_NOARGS(getSpellingLoc, CLASS, V, getSpellingLoc)
    case_METHOD_NOARGS(getFileLoc, CLASS, V, getFileLoc)
    case currefl::getPresumedLoc: {
      CHECK_TRAIT_IS_REFLPROP(getPresumedLoc)
      if (no_more) CLASS_REFLECTION_0(V, getPresumedLoc)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(V, getPresumedLoc, p0)
      TOO_MANY;
    }
    case currefl::isMacroArgExpansion: {
      CHECK_TRAIT_IS_REFLPROP(isMacroArgExpansion)
      if (no_more) PRIM_REFLECTION_0(V, isMacroArgExpansion)
      TRY_LOAD_VAL(class clang::FullSourceLoc *, p0)
      if (no_more) PRIM_REFLECTION(V, isMacroArgExpansion, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getImmediateMacroCallerLoc, CLASS, V, getImmediateMacroCallerLoc)
    case_METHOD_NOARGS(getFileOffset, PRIM, V, getFileOffset)
    case currefl::getExpansionLineNumber: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionLineNumber)
      if (no_more) PRIM_REFLECTION_0(V, getExpansionLineNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(V, getExpansionLineNumber, p0)
      TOO_MANY;
    }
    case currefl::getExpansionColumnNumber: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionColumnNumber)
      if (no_more) PRIM_REFLECTION_0(V, getExpansionColumnNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(V, getExpansionColumnNumber, p0)
      TOO_MANY;
    }
    case currefl::getSpellingLineNumber: {
      CHECK_TRAIT_IS_REFLPROP(getSpellingLineNumber)
      if (no_more) PRIM_REFLECTION_0(V, getSpellingLineNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(V, getSpellingLineNumber, p0)
      TOO_MANY;
    }
    case currefl::getSpellingColumnNumber: {
      CHECK_TRAIT_IS_REFLPROP(getSpellingColumnNumber)
      if (no_more) PRIM_REFLECTION_0(V, getSpellingColumnNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(V, getSpellingColumnNumber, p0)
      TOO_MANY;
    }
    case currefl::getCharacterData: {
      CHECK_TRAIT_IS_REFLPROP(getCharacterData)
      if (no_more) PRIM_REFLECTION_0(V, getCharacterData)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(V, getCharacterData, p0)
      TOO_MANY;
    }
    case currefl::getLineNumber: {
      CHECK_TRAIT_IS_REFLPROP(getLineNumber)
      if (no_more) PRIM_REFLECTION_0(V, getLineNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(V, getLineNumber, p0)
      TOO_MANY;
    }
    case currefl::getColumnNumber: {
      CHECK_TRAIT_IS_REFLPROP(getColumnNumber)
      if (no_more) PRIM_REFLECTION_0(V, getColumnNumber)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(V, getColumnNumber, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getFileEntry, CLASS, V, getFileEntry)
    case currefl::getBufferData: {
      CHECK_TRAIT_IS_REFLPROP(getBufferData)
      if (no_more) CLASS_REFLECTION_0(V, getBufferData)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) CLASS_REFLECTION(V, getBufferData, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isInSystemHeader, PRIM, V, isInSystemHeader)
    case currefl::isBeforeInTranslationUnitThan: {
      CHECK_TRAIT_IS_REFLPROP(isBeforeInTranslationUnitThan)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(V, isBeforeInTranslationUnitThan, p0)
      TOO_MANY;
    }
    case currefl::isBeforeInTranslationUnitThan1: {
      CHECK_TRAIT_IS_REFLPROP(isBeforeInTranslationUnitThan1)
      TRY_LOAD_VAL(class clang::FullSourceLoc, p0)
      if (no_more) PRIM_REFLECTION(V, isBeforeInTranslationUnitThan, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(dump, VOID, V, dump)
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(const class clang::FullSourceLoc &, p0 )
      TRY_LOAD_VAL(const class clang::FullSourceLoc &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(const class clang::FullSourceLoc &, p0 )
      TRY_LOAD_VAL(const class clang::FullSourceLoc &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::DebugEpochBase *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__DebugEpochBase;
# define CLASSNAME llvm::DebugEpochBase
  switch( (currefl::memnames)N ) {
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DiagnosticsEngine *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DiagnosticsEngine;
# define CLASSNAME clang::DiagnosticsEngine
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(dump, VOID, P, dump)
    case currefl::dump1: {
      CHECK_TRAIT_IS_REFLPROP(dump1)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) VOID_REFLECTION(P, dump, (class llvm::StringRef)p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getDiagnosticOptions, CLASS, P, getDiagnosticOptions)
    case_METHOD_NOARGS(getClient, CLASS, P, getClient)
    case_METHOD_NOARGS(ownsClient, PRIM, P, ownsClient)
    case_METHOD_NOARGS(hasSourceManager, PRIM, P, hasSourceManager)
    case_METHOD_NOARGS(getSourceManager, CLASS, P, getSourceManager)
    case_METHOD_NOARGS(getTemplateBacktraceLimit, PRIM, P, getTemplateBacktraceLimit)
    case_METHOD_NOARGS(getConstexprBacktraceLimit, PRIM, P, getConstexprBacktraceLimit)
    case_METHOD_NOARGS(getIgnoreAllWarnings, PRIM, P, getIgnoreAllWarnings)
    case_METHOD_NOARGS(getEnableAllWarnings, PRIM, P, getEnableAllWarnings)
    case_METHOD_NOARGS(getWarningsAsErrors, PRIM, P, getWarningsAsErrors)
    case_METHOD_NOARGS(getErrorsAsFatal, PRIM, P, getErrorsAsFatal)
    case_METHOD_NOARGS(getSuppressSystemWarnings, PRIM, P, getSuppressSystemWarnings)
    case_METHOD_NOARGS(getSuppressAllDiagnostics, PRIM, P, getSuppressAllDiagnostics)
    case_METHOD_NOARGS(getShowOverloads, PRIM, P, getShowOverloads)
    case_METHOD_NOARGS(isLastDiagnosticIgnored, PRIM, P, isLastDiagnosticIgnored)
    case_METHOD_NOARGS(getExtensionHandlingBehavior, PRIM, P, getExtensionHandlingBehavior)
    case_METHOD_NOARGS(hasErrorOccurred, PRIM, P, hasErrorOccurred)
    case_METHOD_NOARGS(hasUncompilableErrorOccurred, PRIM, P, hasUncompilableErrorOccurred)
    case_METHOD_NOARGS(hasFatalErrorOccurred, PRIM, P, hasFatalErrorOccurred)
    case_METHOD_NOARGS(hasUnrecoverableErrorOccurred, PRIM, P, hasUnrecoverableErrorOccurred)
    case_METHOD_NOARGS(getNumWarnings, PRIM, P, getNumWarnings)
    case currefl::isIgnored: {
      CHECK_TRAIT_IS_REFLPROP(isIgnored)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) PRIM_REFLECTION(P, isIgnored, p0, p1)
      TOO_MANY;
    }
    case currefl::getDiagnosticLevel: {
      CHECK_TRAIT_IS_REFLPROP(getDiagnosticLevel)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) PRIM_REFLECTION(P, getDiagnosticLevel, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isDiagnosticInFlight, PRIM, P, isDiagnosticInFlight)
    case_METHOD_NOARGS(getFlagValue, CLASS, P, getFlagValue)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DiagnosticConsumer *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DiagnosticConsumer;
# define CLASSNAME clang::DiagnosticConsumer
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNumErrors, PRIM, P, getNumErrors)
    case_METHOD_NOARGS(getNumWarnings, PRIM, P, getNumWarnings)
    case_METHOD_NOARGS(IncludeInDiagnosticCounts, PRIM, P, IncludeInDiagnosticCounts)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::IdentifierInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__IdentifierInfo;
# define CLASSNAME clang::IdentifierInfo
  switch( (currefl::memnames)N ) {
    case currefl::isStr: {
      CHECK_TRAIT_IS_REFLPROP(isStr)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, isStr, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNameStart, PRIM, P, getNameStart)
    case_METHOD_NOARGS(getLength, PRIM, P, getLength)
    case_METHOD_NOARGS(getName, CLASS, P, getName)
    case_METHOD_NOARGS(hasMacroDefinition, PRIM, P, hasMacroDefinition)
    case_METHOD_NOARGS(hadMacroDefinition, PRIM, P, hadMacroDefinition)
    case_METHOD_NOARGS(getTokenID, PRIM, P, getTokenID)
    case_METHOD_NOARGS(hasRevertedTokenIDToIdentifier, PRIM, P, hasRevertedTokenIDToIdentifier)
    case_METHOD_NOARGS(getPPKeywordID, PRIM, P, getPPKeywordID)
    case_METHOD_NOARGS(getObjCKeywordID, PRIM, P, getObjCKeywordID)
    case_METHOD_NOARGS(hasRevertedBuiltin, PRIM, P, hasRevertedBuiltin)
    case_METHOD_NOARGS(getBuiltinID, PRIM, P, getBuiltinID)
    case_METHOD_NOARGS(getObjCOrBuiltinID, PRIM, P, getObjCOrBuiltinID)
    case_METHOD_NOARGS(isExtensionToken, PRIM, P, isExtensionToken)
    case_METHOD_NOARGS(isFutureCompatKeyword, PRIM, P, isFutureCompatKeyword)
    case_METHOD_NOARGS(isPoisoned, PRIM, P, isPoisoned)
    case_METHOD_NOARGS(isCPlusPlusOperatorKeyword, PRIM, P, isCPlusPlusOperatorKeyword)
    case_METHOD_NOARGS(isHandleIdentifierCase, PRIM, P, isHandleIdentifierCase)
    case_METHOD_NOARGS(isFromAST, PRIM, P, isFromAST)
    case_METHOD_NOARGS(hasChangedSinceDeserialization, PRIM, P, hasChangedSinceDeserialization)
    case_METHOD_NOARGS(hasFETokenInfoChangedSinceDeserialization, PRIM, P, hasFETokenInfoChangedSinceDeserialization)
    case_METHOD_NOARGS(isOutOfDate, PRIM, P, isOutOfDate)
    case_METHOD_NOARGS(isModulesImport, PRIM, P, isModulesImport)
    case_METHOD_NOARGS(isEditorPlaceholder, PRIM, P, isEditorPlaceholder)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::IdentifierInfoLookup *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__IdentifierInfoLookup;
# define CLASSNAME clang::IdentifierInfoLookup
  switch( (currefl::memnames)N ) {
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::IdentifierTable *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__IdentifierTable;
# define CLASSNAME clang::IdentifierTable
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getExternalIdentifierLookup, CLASS, P, getExternalIdentifierLookup)
    case_METHOD_NOARGS(size, PRIM, P, size)
    case_METHOD_NOARGS(PrintStats, VOID, P, PrintStats)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::Selector X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__Selector;
# define CLASSNAME clang::Selector
  switch( (currefl::memnames)N ) {
    case currefl::operator_eq_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_eq_eq)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) PRIM_REFLECTION(V, operator==, p0)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_not_eq)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) PRIM_REFLECTION(V, operator!=, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAsOpaquePtr, PRIM, V, getAsOpaquePtr)
    case_METHOD_NOARGS(isNull, PRIM, V, isNull)
    case_METHOD_NOARGS(isKeywordSelector, PRIM, V, isKeywordSelector)
    case_METHOD_NOARGS(isUnarySelector, PRIM, V, isUnarySelector)
    case_METHOD_NOARGS(getNumArgs, PRIM, V, getNumArgs)
    case currefl::getIdentifierInfoForSlot: {
      CHECK_TRAIT_IS_REFLPROP(getIdentifierInfoForSlot)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, getIdentifierInfoForSlot, p0)
      TOO_MANY;
    }
    case currefl::getNameForSlot: {
      CHECK_TRAIT_IS_REFLPROP(getNameForSlot)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, getNameForSlot, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAsString, PRIM, V, getAsString)
    case_METHOD_NOARGS(dump, VOID, V, dump)
    case_METHOD_NOARGS(getMethodFamily, PRIM, V, getMethodFamily)
    case_METHOD_NOARGS(getStringFormatFamily, PRIM, V, getStringFormatFamily)
    case_METHOD_NOARGS(getEmptyMarker, CLASS, S, getEmptyMarker)
    case_METHOD_NOARGS(getTombstoneMarker, CLASS, S, getTombstoneMarker)
    case currefl::getInstTypeMethodFamily: {
      CHECK_TRAIT_IS_REFLPROP(getInstTypeMethodFamily)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) PRIM_REFLECTION(S, getInstTypeMethodFamily, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SelectorTable *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SelectorTable;
# define CLASSNAME clang::SelectorTable
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTotalMemory, PRIM, P, getTotalMemory)
    case currefl::getPropertyNameFromSetterSelector: {
      CHECK_TRAIT_IS_REFLPROP(getPropertyNameFromSetterSelector)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) PRIM_REFLECTION(S, getPropertyNameFromSetterSelector, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::PartialDiagnostic *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__PartialDiagnostic;
# define CLASSNAME clang::PartialDiagnostic
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDiagID, PRIM, P, getDiagID)
    case currefl::AddTaggedVal: {
      CHECK_TRAIT_IS_REFLPROP(AddTaggedVal)
      TRY_LOAD_VAL(long, p0)
      TRY_LOAD_VAL(enum clang::DiagnosticsEngine::ArgumentKind, p1)
      if (no_more) VOID_REFLECTION(P, AddTaggedVal, (intptr_t)p0, (enum DiagnosticsEngine::ArgumentKind)p1)
      TOO_MANY;
    }
    case currefl::AddString: {
      CHECK_TRAIT_IS_REFLPROP(AddString)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) VOID_REFLECTION(P, AddString, (class llvm::StringRef)p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasStorage, PRIM, P, hasStorage)
    case currefl::operator_less_less: {
      TRY_LOAD_VAL(const class clang::PartialDiagnostic &, p0 )
      TRY_LOAD_VAL(unsigned int, p1 )
      if (no_more) CLASS_REFLECTION_CALL(p0 << p1)
      TOO_MANY;
    }
    case currefl::operator_less_less1: {
      TRY_LOAD_VAL(const class clang::PartialDiagnostic &, p0 )
      TRY_LOAD_VAL(int, p1 )
      if (no_more) CLASS_REFLECTION_CALL(p0 << p1)
      TOO_MANY;
    }
    case currefl::operator_less_less2: {
      TRY_LOAD_VAL(const class clang::PartialDiagnostic &, p0 )
      TRY_LOAD_VAL(const char *, p1 )
      if (no_more) CLASS_REFLECTION_CALL(p0 << p1)
      TOO_MANY;
    }
    case currefl::operator_less_less3: {
      TRY_LOAD_VAL(const class clang::PartialDiagnostic &, p0 )
      TRY_LOAD_VAL(class llvm::StringRef, p1 )
      if (no_more) CLASS_REFLECTION_CALL(p0 << p1)
      TOO_MANY;
    }
    case currefl::operator_less_less4: {
      TRY_LOAD_VAL(const class clang::PartialDiagnostic &, p0 )
      TRY_LOAD_VAL(const class clang::IdentifierInfo *, p1 )
      if (no_more) CLASS_REFLECTION_CALL(p0 << p1)
      TOO_MANY;
    }
    case currefl::operator_less_less5: {
      TRY_LOAD_VAL(const class clang::PartialDiagnostic &, p0 )
      TRY_LOAD_VAL(class clang::SourceRange, p1 )
      if (no_more) CLASS_REFLECTION_CALL(p0 << p1)
      TOO_MANY;
    }
    case currefl::operator_less_less6: {
      TRY_LOAD_VAL(const class clang::PartialDiagnostic &, p0 )
      TRY_LOAD_VAL(const class clang::CharSourceRange &, p1 )
      if (no_more) CLASS_REFLECTION_CALL(p0 << p1)
      TOO_MANY;
    }
    case currefl::operator_less_less7: {
      TRY_LOAD_VAL(const class clang::PartialDiagnostic &, p0 )
      TRY_LOAD_VAL(const class clang::FixItHint &, p1 )
      if (no_more) CLASS_REFLECTION_CALL(p0 << p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclarationName X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DeclarationName;
# define CLASSNAME clang::DeclarationName
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getUsingDirectiveName, CLASS, S, getUsingDirectiveName)
    case_METHOD_NOARGS(operator_bool, PRIM, V, operator bool)
    case_METHOD_NOARGS(isEmpty, PRIM, V, isEmpty)
    case_METHOD_NOARGS(isIdentifier, PRIM, V, isIdentifier)
    case_METHOD_NOARGS(isObjCZeroArgSelector, PRIM, V, isObjCZeroArgSelector)
    case_METHOD_NOARGS(isObjCOneArgSelector, PRIM, V, isObjCOneArgSelector)
    case_METHOD_NOARGS(getNameKind, PRIM, V, getNameKind)
    case_METHOD_NOARGS(isDependentName, PRIM, V, isDependentName)
    case_METHOD_NOARGS(getAsString, PRIM, V, getAsString)
    case_METHOD_NOARGS(getAsIdentifierInfo, CLASS, V, getAsIdentifierInfo)
    case_METHOD_NOARGS(getAsOpaqueInteger, PRIM, V, getAsOpaqueInteger)
    case_METHOD_NOARGS(getAsOpaquePtr, PRIM, V, getAsOpaquePtr)
    case currefl::getFromOpaquePtr: {
      CHECK_TRAIT_IS_REFLPROP(getFromOpaquePtr)
      TRY_LOAD_VAL(void *, p0)
      if (no_more) CLASS_REFLECTION(S, getFromOpaquePtr, p0)
      TOO_MANY;
    }
    case currefl::getFromOpaqueInteger: {
      CHECK_TRAIT_IS_REFLPROP(getFromOpaqueInteger)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) CLASS_REFLECTION(S, getFromOpaqueInteger, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getCXXNameType, CLASS, V, getCXXNameType)
    case_METHOD_NOARGS(getCXXDeductionGuideTemplate, CLASS, V, getCXXDeductionGuideTemplate)
    case_METHOD_NOARGS(getCXXOverloadedOperator, PRIM, V, getCXXOverloadedOperator)
    case_METHOD_NOARGS(getCXXLiteralIdentifier, CLASS, V, getCXXLiteralIdentifier)
    case currefl::getCXXIdExprArguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getCXXIdExprArguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(V, getCXXIdExprArguments)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getObjCSelector, CLASS, V, getObjCSelector)
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(class clang::DeclarationName, p0 )
      TRY_LOAD_VAL(class clang::DeclarationName, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(class clang::DeclarationName, p0 )
      TRY_LOAD_VAL(class clang::DeclarationName, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getEmptyMarker, CLASS, S, getEmptyMarker)
    case_METHOD_NOARGS(getTombstoneMarker, CLASS, S, getTombstoneMarker)
    case currefl::compare: {
      CHECK_TRAIT_IS_REFLPROP(compare)
      TRY_LOAD_VAL(class clang::DeclarationName, p0)
      TRY_LOAD_VAL(class clang::DeclarationName, p1)
      if (no_more) PRIM_REFLECTION(S, compare, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(dump, VOID, V, dump)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclarationNameLoc *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DeclarationNameLoc;
# define CLASSNAME clang::DeclarationNameLoc
  switch( (currefl::memnames)N ) {
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclarationNameInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DeclarationNameInfo;
# define CLASSNAME clang::DeclarationNameInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getName, CLASS, P, getName)
    case_METHOD_NOARGS(getLoc, CLASS, P, getLoc)
    case_METHOD_NOARGS(getInfo, CLASS, P, getInfo)
    case_METHOD_NOARGS(getNamedTypeInfo, CLASS, P, getNamedTypeInfo)
    case_METHOD_NOARGS(getCXXOperatorNameRange, CLASS, P, getCXXOperatorNameRange)
    case_METHOD_NOARGS(getCXXLiteralOperatorNameLoc, CLASS, P, getCXXLiteralOperatorNameLoc)
    case_METHOD_NOARGS(getCXXIdExprNameRange, CLASS, P, getCXXIdExprNameRange)
    case_METHOD_NOARGS(isInstantiationDependent, PRIM, P, isInstantiationDependent)
    case_METHOD_NOARGS(containsUnexpandedParameterPack, PRIM, P, containsUnexpandedParameterPack)
    case_METHOD_NOARGS(getAsString, PRIM, P, getAsString)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclarationNameInfo X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DeclarationNameInfo;
# define CLASSNAME clang::DeclarationNameInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getName, CLASS, V, getName)
    case_METHOD_NOARGS(getLoc, CLASS, V, getLoc)
    case_METHOD_NOARGS(getInfo, CLASS, V, getInfo)
    case_METHOD_NOARGS(getNamedTypeInfo, CLASS, V, getNamedTypeInfo)
    case_METHOD_NOARGS(getCXXOperatorNameRange, CLASS, V, getCXXOperatorNameRange)
    case_METHOD_NOARGS(getCXXLiteralOperatorNameLoc, CLASS, V, getCXXLiteralOperatorNameLoc)
    case_METHOD_NOARGS(getCXXIdExprNameRange, CLASS, V, getCXXIdExprNameRange)
    case_METHOD_NOARGS(isInstantiationDependent, PRIM, V, isInstantiationDependent)
    case_METHOD_NOARGS(containsUnexpandedParameterPack, PRIM, V, containsUnexpandedParameterPack)
    case_METHOD_NOARGS(getAsString, PRIM, V, getAsString)
    case_METHOD_NOARGS(getBeginLoc, CLASS, V, getBeginLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, V, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, V, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, V, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, V, getEndLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::VersionTuple *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__VersionTuple;
# define CLASSNAME llvm::VersionTuple
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(empty, PRIM, P, empty)
    case_METHOD_NOARGS(getMajor, PRIM, P, getMajor)
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
    case currefl::operator_less: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 < p1)
      TOO_MANY;
    }
    case currefl::operator_gr: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 > p1)
      TOO_MANY;
    }
    case currefl::operator_less_eq: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 <= p1)
      TOO_MANY;
    }
    case currefl::operator_gr_eq: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 >= p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAsString, PRIM, P, getAsString)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::VersionTuple X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__VersionTuple;
# define CLASSNAME llvm::VersionTuple
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(empty, PRIM, V, empty)
    case_METHOD_NOARGS(getMajor, PRIM, V, getMajor)
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
    case currefl::operator_less: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 < p1)
      TOO_MANY;
    }
    case currefl::operator_gr: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 > p1)
      TOO_MANY;
    }
    case currefl::operator_less_eq: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 <= p1)
      TOO_MANY;
    }
    case currefl::operator_gr_eq: {
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p0 )
      TRY_LOAD_VAL(const class llvm::VersionTuple &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 >= p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAsString, PRIM, V, getAsString)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::Decl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__Decl;
# define CLASSNAME clang::Decl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(getDeclKindName, PRIM, P, getDeclKindName)
    case_METHOD_NOARGS(getNextDeclInContext, CLASS, P, getNextDeclInContext)
    case_METHOD_NOARGS(getDeclContext, CLASS, P, getDeclContext)
    case_METHOD_NOARGS(getNonClosureContext, CLASS, P, getNonClosureContext)
    case_METHOD_NOARGS(getTranslationUnitDecl, CLASS, P, getTranslationUnitDecl)
    case_METHOD_NOARGS(isInAnonymousNamespace, PRIM, P, isInAnonymousNamespace)
    case_METHOD_NOARGS(isInStdNamespace, PRIM, P, isInStdNamespace)
    case_METHOD_NOARGS(getASTContext, CLASS, P, getASTContext)
    case_METHOD_NOARGS(getAccess, PRIM, P, getAccess)
    case_METHOD_NOARGS(getAccessUnsafe, PRIM, P, getAccessUnsafe)
    case_METHOD_NOARGS(hasAttrs, PRIM, P, hasAttrs)
    case currefl::attrs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::attrs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, attrs)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getMaxAlignment, PRIM, P, getMaxAlignment)
    case_METHOD_NOARGS(isInvalidDecl, PRIM, P, isInvalidDecl)
    case_METHOD_NOARGS(isImplicit, PRIM, P, isImplicit)
    case currefl::isUsed: {
      CHECK_TRAIT_IS_REFLPROP(isUsed)
      if (no_more) PRIM_REFLECTION_0(P, isUsed)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) PRIM_REFLECTION(P, isUsed, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isReferenced, PRIM, P, isReferenced)
    case_METHOD_NOARGS(isThisDeclarationReferenced, PRIM, P, isThisDeclarationReferenced)
    case_METHOD_NOARGS(instantiationsWillNeedParsing, PRIM, P, instantiationsWillNeedParsing)
    case_METHOD_NOARGS(isTopLevelDeclInObjCContainer, PRIM, P, isTopLevelDeclInObjCContainer)
    case_METHOD_NOARGS(getExternalSourceSymbolAttr, CLASS, P, getExternalSourceSymbolAttr)
    case_METHOD_NOARGS(isModulePrivate, PRIM, P, isModulePrivate)
    case_METHOD_NOARGS(isExported, PRIM, P, isExported)
    case_METHOD_NOARGS(hasDefiningAttr, PRIM, P, hasDefiningAttr)
    case_METHOD_NOARGS(getDefiningAttr, CLASS, P, getDefiningAttr)
    case_METHOD_NOARGS(getVersionIntroduced, CLASS, P, getVersionIntroduced)
    case_METHOD_NOARGS(isWeakImported, PRIM, P, isWeakImported)
    case_METHOD_NOARGS(isFromASTFile, PRIM, P, isFromASTFile)
    case_METHOD_NOARGS(getGlobalID, PRIM, P, getGlobalID)
    case_METHOD_NOARGS(getOwningModuleID, PRIM, P, getOwningModuleID)
    case_METHOD_NOARGS(getImportedOwningModule, CLASS, P, getImportedOwningModule)
    case_METHOD_NOARGS(getLocalOwningModule, CLASS, P, getLocalOwningModule)
    case_METHOD_NOARGS(hasOwningModule, PRIM, P, hasOwningModule)
    case_METHOD_NOARGS(getOwningModule, CLASS, P, getOwningModule)
    case currefl::getOwningModuleForLinkage: {
      CHECK_TRAIT_IS_REFLPROP(getOwningModuleForLinkage)
      if (no_more) CLASS_REFLECTION_0(P, getOwningModuleForLinkage)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(P, getOwningModuleForLinkage, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isHidden, PRIM, P, isHidden)
    case_METHOD_NOARGS(getModuleOwnershipKind, PRIM, P, getModuleOwnershipKind)
    case_METHOD_NOARGS(getIdentifierNamespace, PRIM, P, getIdentifierNamespace)
    case currefl::isInIdentifierNamespace: {
      CHECK_TRAIT_IS_REFLPROP(isInIdentifierNamespace)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isInIdentifierNamespace, p0)
      TOO_MANY;
    }
    case currefl::getIdentifierNamespaceForKind: {
      CHECK_TRAIT_IS_REFLPROP(getIdentifierNamespaceForKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, getIdentifierNamespaceForKind, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasTagIdentifierNamespace, PRIM, P, hasTagIdentifierNamespace)
    case currefl::isTagIdentifierNamespace: {
      CHECK_TRAIT_IS_REFLPROP(isTagIdentifierNamespace)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(S, isTagIdentifierNamespace, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLexicalDeclContext, CLASS, P, getLexicalDeclContext)
    case_METHOD_NOARGS(isOutOfLine, PRIM, P, isOutOfLine)
    case_METHOD_NOARGS(isTemplated, PRIM, P, isTemplated)
    case_METHOD_NOARGS(isDefinedOutsideFunctionOrMethod, PRIM, P, isDefinedOutsideFunctionOrMethod)
    case_METHOD_NOARGS(isLexicallyWithinFunctionOrMethod, PRIM, P, isLexicallyWithinFunctionOrMethod)
    case_METHOD_NOARGS(getParentFunctionOrMethod, CLASS, P, getParentFunctionOrMethod)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(isCanonicalDecl, PRIM, P, isCanonicalDecl)
    case currefl::redecls: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::redecls))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, redecls)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getPreviousDecl, CLASS, P, getPreviousDecl)
    case_METHOD_NOARGS(isFirstDecl, PRIM, P, isFirstDecl)
    case_METHOD_NOARGS(getMostRecentDecl, CLASS, P, getMostRecentDecl)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(hasBody, PRIM, P, hasBody)
    case_METHOD_NOARGS(getBodyRBrace, CLASS, P, getBodyRBrace)
    case currefl::add: {
      CHECK_TRAIT_IS_REFLPROP(add)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) VOID_REFLECTION(S, add, (enum clang::Decl::Kind)p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(EnableStatistics, VOID, S, EnableStatistics)
    case_METHOD_NOARGS(PrintStats, VOID, S, PrintStats)
    case_METHOD_NOARGS(isTemplateParameter, PRIM, P, isTemplateParameter)
    case_METHOD_NOARGS(isTemplateParameterPack, PRIM, P, isTemplateParameterPack)
    case_METHOD_NOARGS(isParameterPack, PRIM, P, isParameterPack)
    case_METHOD_NOARGS(isTemplateDecl, PRIM, P, isTemplateDecl)
    case_METHOD_NOARGS(isFunctionOrFunctionTemplate, PRIM, P, isFunctionOrFunctionTemplate)
    case_METHOD_NOARGS(getDescribedTemplate, CLASS, P, getDescribedTemplate)
    case_METHOD_NOARGS(getAsFunction, CLASS, P, getAsFunction)
    case_METHOD_NOARGS(getFriendObjectKind, PRIM, P, getFriendObjectKind)
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(dump, VOID, P, dump)
    case_METHOD_NOARGS(dumpColor, VOID, P, dumpColor)
    case currefl::getFunctionType: {
      CHECK_TRAIT_IS_REFLPROP(getFunctionType)
      if (no_more) CLASS_REFLECTION_0(P, getFunctionType)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(P, getFunctionType, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclContextLookupResult X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DeclContextLookupResult;
# define CLASSNAME clang::DeclContextLookupResult
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(V)
      TOO_MANY;
};
    case_METHOD_NOARGS(empty, PRIM, V, empty)
    case_METHOD_NOARGS(data, CLASS, V, data)
    case_METHOD_NOARGS(size, PRIM, V, size)
    case_METHOD_NOARGS(front, CLASS, V, front)
    case_METHOD_NOARGS(back, CLASS, V, back)
    case currefl::operator_sub: {
      CHECK_TRAIT_IS_REFLPROP(operator_sub)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) CLASS_REFLECTION(V, operator[], p0)
      TOO_MANY;
    }
    case currefl::slice: {
      CHECK_TRAIT_IS_REFLPROP(slice)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) CLASS_REFLECTION(V, slice, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclContext *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, Decl::castFromDeclContext(X), dyn );
  }
  namespace currefl = refl::clang__DeclContext;
# define CLASSNAME clang::DeclContext
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDeclKind, PRIM, P, getDeclKind)
    case_METHOD_NOARGS(getDeclKindName, PRIM, P, getDeclKindName)
    case_METHOD_NOARGS(getParent, CLASS, P, getParent)
    case_METHOD_NOARGS(getLexicalParent, CLASS, P, getLexicalParent)
    case_METHOD_NOARGS(getLookupParent, CLASS, P, getLookupParent)
    case_METHOD_NOARGS(getParentASTContext, CLASS, P, getParentASTContext)
    case_METHOD_NOARGS(isClosure, PRIM, P, isClosure)
    case_METHOD_NOARGS(isObjCContainer, PRIM, P, isObjCContainer)
    case_METHOD_NOARGS(isFunctionOrMethod, PRIM, P, isFunctionOrMethod)
    case_METHOD_NOARGS(isLookupContext, PRIM, P, isLookupContext)
    case_METHOD_NOARGS(isFileContext, PRIM, P, isFileContext)
    case_METHOD_NOARGS(isTranslationUnit, PRIM, P, isTranslationUnit)
    case_METHOD_NOARGS(isRecord, PRIM, P, isRecord)
    case_METHOD_NOARGS(isNamespace, PRIM, P, isNamespace)
    case_METHOD_NOARGS(isStdNamespace, PRIM, P, isStdNamespace)
    case_METHOD_NOARGS(isInlineNamespace, PRIM, P, isInlineNamespace)
    case_METHOD_NOARGS(isDependentContext, PRIM, P, isDependentContext)
    case_METHOD_NOARGS(isTransparentContext, PRIM, P, isTransparentContext)
    case_METHOD_NOARGS(isExternCContext, PRIM, P, isExternCContext)
    case_METHOD_NOARGS(getExternCContext, CLASS, P, getExternCContext)
    case_METHOD_NOARGS(isExternCXXContext, PRIM, P, isExternCXXContext)
    case currefl::Equals: {
      CHECK_TRAIT_IS_REFLPROP(Equals)
      TRY_LOAD_VAL(const class clang::DeclContext *, p0)
      if (no_more) PRIM_REFLECTION(P, Equals, p0)
      TOO_MANY;
    }
    case currefl::Encloses: {
      CHECK_TRAIT_IS_REFLPROP(Encloses)
      TRY_LOAD_VAL(const class clang::DeclContext *, p0)
      if (no_more) PRIM_REFLECTION(P, Encloses, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNonClosureAncestor, CLASS, P, getNonClosureAncestor)
    case_METHOD_NOARGS(getPrimaryContext, CLASS, P, getPrimaryContext)
    case_METHOD_NOARGS(getRedeclContext, CLASS, P, getRedeclContext)
    case_METHOD_NOARGS(getEnclosingNamespaceContext, CLASS, P, getEnclosingNamespaceContext)
    case_METHOD_NOARGS(getOuterLexicalRecordContext, CLASS, P, getOuterLexicalRecordContext)
    case currefl::InEnclosingNamespaceSetOf: {
      CHECK_TRAIT_IS_REFLPROP(InEnclosingNamespaceSetOf)
      TRY_LOAD_VAL(const class clang::DeclContext *, p0)
      if (no_more) PRIM_REFLECTION(P, InEnclosingNamespaceSetOf, p0)
      TOO_MANY;
    }
    case currefl::decls: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::decls))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, decls)
      TOO_MANY;
    }
    case_METHOD_NOARGS(decls_empty, PRIM, P, decls_empty)
    case currefl::noload_decls: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::noload_decls))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, noload_decls)
      TOO_MANY;
    }
    case currefl::containsDecl: {
      CHECK_TRAIT_IS_REFLPROP(containsDecl)
      TRY_LOAD_VAL(class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(P, containsDecl, p0)
      TOO_MANY;
    }
    case currefl::containsDeclAndLoad: {
      CHECK_TRAIT_IS_REFLPROP(containsDeclAndLoad)
      TRY_LOAD_VAL(class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(P, containsDeclAndLoad, p0)
      TOO_MANY;
    }
    case currefl::lookup: {
      CHECK_TRAIT_IS_REFLPROP(lookup)
      TRY_LOAD_VAL(class clang::DeclarationName, p0)
      if (no_more) CLASS_REFLECTION(P, lookup, p0)
      TOO_MANY;
    }
    case currefl::lookups: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::lookups))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, lookups)
      TOO_MANY;
    }
    case currefl::ddiags: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::ddiags))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, ddiags)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLookupPtr, CLASS, P, getLookupPtr)
    case_METHOD_NOARGS(hasExternalLexicalStorage, PRIM, P, hasExternalLexicalStorage)
    case_METHOD_NOARGS(hasExternalVisibleStorage, PRIM, P, hasExternalVisibleStorage)
    case currefl::isDeclInLexicalTraversal: {
      CHECK_TRAIT_IS_REFLPROP(isDeclInLexicalTraversal)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(P, isDeclInLexicalTraversal, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(shouldUseQualifiedLookup, PRIM, P, shouldUseQualifiedLookup)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classof1: {
      CHECK_TRAIT_IS_REFLPROP(classof1)
      TRY_LOAD_VAL(const class clang::DeclContext *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(dumpDeclContext, VOID, P, dumpDeclContext)
    case_METHOD_NOARGS(dumpLookups, VOID, P, dumpLookups)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CharUnits *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__CharUnits;
# define CLASSNAME clang::CharUnits
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(Zero, CLASS, S, Zero)
    case_METHOD_NOARGS(One, CLASS, S, One)
    case currefl::fromQuantity: {
      CHECK_TRAIT_IS_REFLPROP(fromQuantity)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(S, fromQuantity, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isZero, PRIM, P, isZero)
    case_METHOD_NOARGS(isOne, PRIM, P, isOne)
    case_METHOD_NOARGS(isPositive, PRIM, P, isPositive)
    case_METHOD_NOARGS(isNegative, PRIM, P, isNegative)
    case_METHOD_NOARGS(isPowerOfTwo, PRIM, P, isPowerOfTwo)
    case currefl::isMultipleOf: {
      CHECK_TRAIT_IS_REFLPROP(isMultipleOf)
      TRY_LOAD_VAL(class clang::CharUnits, p0)
      if (no_more) PRIM_REFLECTION(P, isMultipleOf, p0)
      TOO_MANY;
    }
    case currefl::operator_star: {
      CHECK_TRAIT_IS_REFLPROP(operator_star)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(P, operator*, p0)
      TOO_MANY;
    }
    case currefl::operator_div: {
      CHECK_TRAIT_IS_REFLPROP(operator_div)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(P, operator/, p0)
      TOO_MANY;
    }
    case currefl::operator_mod: {
      CHECK_TRAIT_IS_REFLPROP(operator_mod)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(P, operator%, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(operator_minus, CLASS, P, operator-)
    case_METHOD_NOARGS(getQuantity, PRIM, P, getQuantity)
    case currefl::alignmentAtOffset: {
      CHECK_TRAIT_IS_REFLPROP(alignmentAtOffset)
      TRY_LOAD_VAL(class clang::CharUnits, p0)
      if (no_more) CLASS_REFLECTION(P, alignmentAtOffset, p0)
      TOO_MANY;
    }
    case currefl::alignmentOfArrayElement: {
      CHECK_TRAIT_IS_REFLPROP(alignmentOfArrayElement)
      TRY_LOAD_VAL(class clang::CharUnits, p0)
      if (no_more) CLASS_REFLECTION(P, alignmentOfArrayElement, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CharUnits X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__CharUnits;
# define CLASSNAME clang::CharUnits
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(Zero, CLASS, S, Zero)
    case_METHOD_NOARGS(One, CLASS, S, One)
    case currefl::fromQuantity: {
      CHECK_TRAIT_IS_REFLPROP(fromQuantity)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(S, fromQuantity, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isZero, PRIM, V, isZero)
    case_METHOD_NOARGS(isOne, PRIM, V, isOne)
    case_METHOD_NOARGS(isPositive, PRIM, V, isPositive)
    case_METHOD_NOARGS(isNegative, PRIM, V, isNegative)
    case_METHOD_NOARGS(isPowerOfTwo, PRIM, V, isPowerOfTwo)
    case currefl::isMultipleOf: {
      CHECK_TRAIT_IS_REFLPROP(isMultipleOf)
      TRY_LOAD_VAL(class clang::CharUnits, p0)
      if (no_more) PRIM_REFLECTION(V, isMultipleOf, p0)
      TOO_MANY;
    }
    case currefl::operator_star: {
      CHECK_TRAIT_IS_REFLPROP(operator_star)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(V, operator*, p0)
      TOO_MANY;
    }
    case currefl::operator_div: {
      CHECK_TRAIT_IS_REFLPROP(operator_div)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(V, operator/, p0)
      TOO_MANY;
    }
    case currefl::operator_mod: {
      CHECK_TRAIT_IS_REFLPROP(operator_mod)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(V, operator%, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(operator_minus, CLASS, V, operator-)
    case_METHOD_NOARGS(getQuantity, PRIM, V, getQuantity)
    case currefl::alignmentAtOffset: {
      CHECK_TRAIT_IS_REFLPROP(alignmentAtOffset)
      TRY_LOAD_VAL(class clang::CharUnits, p0)
      if (no_more) CLASS_REFLECTION(V, alignmentAtOffset, p0)
      TOO_MANY;
    }
    case currefl::alignmentOfArrayElement: {
      CHECK_TRAIT_IS_REFLPROP(alignmentOfArrayElement)
      TRY_LOAD_VAL(class clang::CharUnits, p0)
      if (no_more) CLASS_REFLECTION(V, alignmentOfArrayElement, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FileSystemOptions *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FileSystemOptions;
# define CLASSNAME clang::FileSystemOptions
  switch( (currefl::memnames)N ) {
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::sys::fs::UniqueID *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__sys__fs__UniqueID;
# define CLASSNAME llvm::sys::fs::UniqueID
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDevice, PRIM, P, getDevice)
    case_METHOD_NOARGS(getFile, PRIM, P, getFile)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::MemoryBuffer *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__MemoryBuffer;
# define CLASSNAME llvm::MemoryBuffer
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBufferStart, PRIM, P, getBufferStart)
    case_METHOD_NOARGS(getBufferEnd, PRIM, P, getBufferEnd)
    case_METHOD_NOARGS(getBufferSize, PRIM, P, getBufferSize)
    case_METHOD_NOARGS(getBuffer, CLASS, P, getBuffer)
    case_METHOD_NOARGS(getBufferIdentifier, CLASS, P, getBufferIdentifier)
    case_METHOD_NOARGS(getBufferKind, PRIM, P, getBufferKind)
    case_METHOD_NOARGS(getMemBufferRef, CLASS, P, getMemBufferRef)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::MemoryBufferRef X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__MemoryBufferRef;
# define CLASSNAME llvm::MemoryBufferRef
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBuffer, CLASS, V, getBuffer)
    case_METHOD_NOARGS(getBufferIdentifier, CLASS, V, getBufferIdentifier)
    case_METHOD_NOARGS(getBufferStart, PRIM, V, getBufferStart)
    case_METHOD_NOARGS(getBufferEnd, PRIM, V, getBufferEnd)
    case_METHOD_NOARGS(getBufferSize, PRIM, V, getBufferSize)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DirectoryEntry *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DirectoryEntry;
# define CLASSNAME clang::DirectoryEntry
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getName, CLASS, P, getName)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FileEntry *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FileEntry;
# define CLASSNAME clang::FileEntry
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getName, CLASS, P, getName)
    case_METHOD_NOARGS(tryGetRealPathName, CLASS, P, tryGetRealPathName)
    case_METHOD_NOARGS(isValid, PRIM, P, isValid)
    case_METHOD_NOARGS(getSize, PRIM, P, getSize)
    case_METHOD_NOARGS(getUID, PRIM, P, getUID)
    case_METHOD_NOARGS(getUniqueID, CLASS, P, getUniqueID)
    case_METHOD_NOARGS(isInPCH, PRIM, P, isInPCH)
    case_METHOD_NOARGS(getModificationTime, PRIM, P, getModificationTime)
    case_METHOD_NOARGS(getDir, CLASS, P, getDir)
    case_METHOD_NOARGS(isNamedPipe, PRIM, P, isNamedPipe)
    case_METHOD_NOARGS(closeFile, VOID, P, closeFile)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FileManager *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FileManager;
# define CLASSNAME clang::FileManager
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getFileSystemOpts, CLASS, P, getFileSystemOpts)
    case currefl::modifyFileEntry: {
      CHECK_TRAIT_IS_REFLPROP(modifyFileEntry)
      TRY_LOAD_VAL(class clang::FileEntry *, p0)
      TRY_LOAD_VAL(long long, p1)
      TRY_LOAD_VAL(long, p2)
      if (no_more) VOID_REFLECTION(S, modifyFileEntry, (class clang::FileEntry *)p0, (off_t)p1, (time_t)p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(PrintStats, VOID, P, PrintStats)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ASTFileSignature X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ASTFileSignature;
# define CLASSNAME clang::ASTFileSignature
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(operator_bool, PRIM, V, operator bool)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::Module *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__Module;
# define CLASSNAME clang::Module
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, P, DefinitionLoc)
    case_FIELD(PRIM, P, Kind)
    case_FIELD(CLASS, P, Parent)
    case_FIELD(CLASS, P, Directory)
    case_FIELD(CLASS, P, Signature)
    case_FIELD(CLASS, P, ShadowingModule)
    case_FIELD(PRIM, P, IsMissingRequirement)
    case_FIELD(PRIM, P, HasIncompatibleModuleFile)
    case_FIELD(PRIM, P, IsAvailable)
    case_FIELD(PRIM, P, IsFromModuleFile)
    case_FIELD(PRIM, P, IsFramework)
    case_FIELD(PRIM, P, IsExplicit)
    case_FIELD(PRIM, P, IsSystem)
    case_FIELD(PRIM, P, IsExternC)
    case_FIELD(PRIM, P, IsInferred)
    case_FIELD(PRIM, P, InferSubmodules)
    case_FIELD(PRIM, P, InferExplicitSubmodules)
    case_FIELD(PRIM, P, InferExportWildcard)
    case_FIELD(PRIM, P, ConfigMacrosExhaustive)
    case_FIELD(PRIM, P, NoUndeclaredIncludes)
    case_FIELD(PRIM, P, ModuleMapIsPrivate)
    case_FIELD(PRIM, P, NameVisibility)
    case_FIELD(CLASS, P, InferredSubmoduleLoc)
    case_FIELD(PRIM, P, UseExportAsModuleLinkName)
    case_METHOD_NOARGS(isAvailable, PRIM, P, isAvailable)
    case_METHOD_NOARGS(isSubModule, PRIM, P, isSubModule)
    case currefl::isSubModuleOf: {
      CHECK_TRAIT_IS_REFLPROP(isSubModuleOf)
      TRY_LOAD_VAL(const class clang::Module *, p0)
      if (no_more) PRIM_REFLECTION(P, isSubModuleOf, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isPartOfFramework, PRIM, P, isPartOfFramework)
    case_METHOD_NOARGS(isSubFramework, PRIM, P, isSubFramework)
    case currefl::getFullModuleName: {
      CHECK_TRAIT_IS_REFLPROP(getFullModuleName)
      if (no_more) PRIM_REFLECTION_0(P, getFullModuleName)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) PRIM_REFLECTION(P, getFullModuleName, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getTopLevelModule, CLASS, P, getTopLevelModule)
    case_METHOD_NOARGS(getTopLevelModuleName, CLASS, P, getTopLevelModuleName)
    case_METHOD_NOARGS(getASTFile, CLASS, P, getASTFile)
    case_METHOD_NOARGS(getUmbrellaDir, CLASS, P, getUmbrellaDir)
    case_METHOD_NOARGS(getUmbrellaHeader, CLASS, P, getUmbrellaHeader)
    case_METHOD_NOARGS(hasUmbrellaDir, PRIM, P, hasUmbrellaDir)
    case currefl::directlyUses: {
      CHECK_TRAIT_IS_REFLPROP(directlyUses)
      TRY_LOAD_VAL(const class clang::Module *, p0)
      if (no_more) PRIM_REFLECTION(P, directlyUses, p0)
      TOO_MANY;
    }
    case currefl::findSubmodule: {
      CHECK_TRAIT_IS_REFLPROP(findSubmodule)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(P, findSubmodule, p0)
      TOO_MANY;
    }
    case currefl::isModuleVisible: {
      CHECK_TRAIT_IS_REFLPROP(isModuleVisible)
      TRY_LOAD_VAL(const class clang::Module *, p0)
      if (no_more) PRIM_REFLECTION(P, isModuleVisible, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getVisibilityID, PRIM, P, getVisibilityID)
    case currefl::submodules: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::submodules))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, submodules)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getModuleInputBufferName, CLASS, S, getModuleInputBufferName)
    case_METHOD_NOARGS(dump, VOID, P, dump)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::Module::Header X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__Module__Header;
# define CLASSNAME clang::Module::Header
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, V, Entry)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::Module::DirectoryName X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__Module__DirectoryName;
# define CLASSNAME clang::Module::DirectoryName
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, V, Entry)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ExternalASTSource *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ExternalASTSource;
# define CLASSNAME clang::ExternalASTSource
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getGeneration, PRIM, P, getGeneration)
    case_METHOD_NOARGS(getMemoryBufferSizes, CLASS, P, getMemoryBufferSizes)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ExternalASTSource::MemoryBufferSizes X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ExternalASTSource__MemoryBufferSizes;
# define CLASSNAME clang::ExternalASTSource::MemoryBufferSizes
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, V, malloc_bytes)
    case_FIELD(PRIM, V, mmap_bytes)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::FoldingSetBase::Node *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__FoldingSetBase__Node;
# define CLASSNAME llvm::FoldingSetBase::Node
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNextInBucket, PRIM, P, getNextInBucket)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::NestedNameSpecifier *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__NestedNameSpecifier;
# define CLASSNAME clang::NestedNameSpecifier
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getPrefix, CLASS, P, getPrefix)
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(getAsIdentifier, CLASS, P, getAsIdentifier)
    case_METHOD_NOARGS(getAsNamespace, CLASS, P, getAsNamespace)
    case_METHOD_NOARGS(getAsNamespaceAlias, CLASS, P, getAsNamespaceAlias)
    case_METHOD_NOARGS(getAsRecordDecl, CLASS, P, getAsRecordDecl)
    case_METHOD_NOARGS(getAsType, CLASS, P, getAsType)
    case_METHOD_NOARGS(isDependent, PRIM, P, isDependent)
    case_METHOD_NOARGS(isInstantiationDependent, PRIM, P, isInstantiationDependent)
    case_METHOD_NOARGS(containsUnexpandedParameterPack, PRIM, P, containsUnexpandedParameterPack)
    case_METHOD_NOARGS(dump, VOID, P, dump)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::NestedNameSpecifierLoc X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__NestedNameSpecifierLoc;
# define CLASSNAME clang::NestedNameSpecifierLoc
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(operator_bool, PRIM, V, operator bool)
    case_METHOD_NOARGS(hasQualifier, PRIM, V, hasQualifier)
    case_METHOD_NOARGS(getNestedNameSpecifier, CLASS, V, getNestedNameSpecifier)
    case_METHOD_NOARGS(getOpaqueData, PRIM, V, getOpaqueData)
    case_METHOD_NOARGS(getSourceRange, CLASS, V, getSourceRange)
    case_METHOD_NOARGS(getLocalSourceRange, CLASS, V, getLocalSourceRange)
    case_METHOD_NOARGS(getBeginLoc, CLASS, V, getBeginLoc)
    case_METHOD_NOARGS(getEndLoc, CLASS, V, getEndLoc)
    case_METHOD_NOARGS(getLocalBeginLoc, CLASS, V, getLocalBeginLoc)
    case_METHOD_NOARGS(getLocalEndLoc, CLASS, V, getLocalEndLoc)
    case_METHOD_NOARGS(getPrefix, CLASS, V, getPrefix)
    case_METHOD_NOARGS(getTypeLoc, CLASS, V, getTypeLoc)
    case_METHOD_NOARGS(getDataLength, PRIM, V, getDataLength)
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(class clang::NestedNameSpecifierLoc, p0 )
      TRY_LOAD_VAL(class clang::NestedNameSpecifierLoc, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(class clang::NestedNameSpecifierLoc, p0 )
      TRY_LOAD_VAL(class clang::NestedNameSpecifierLoc, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UncommonTemplateNameStorage *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__UncommonTemplateNameStorage;
# define CLASSNAME clang::UncommonTemplateNameStorage
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(size, PRIM, P, size)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OverloadedTemplateStorage *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__OverloadedTemplateStorage;
# define CLASSNAME clang::OverloadedTemplateStorage
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(P)
      TOO_MANY;
};
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SubstTemplateTemplateParmPackStorage *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SubstTemplateTemplateParmPackStorage;
# define CLASSNAME clang::SubstTemplateTemplateParmPackStorage
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getParameterPack, CLASS, P, getParameterPack)
    case_METHOD_NOARGS(getArgumentPack, CLASS, P, getArgumentPack)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateName X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TemplateName;
# define CLASSNAME clang::TemplateName
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isNull, PRIM, V, isNull)
    case_METHOD_NOARGS(getKind, PRIM, V, getKind)
    case_METHOD_NOARGS(getAsTemplateDecl, CLASS, V, getAsTemplateDecl)
    case_METHOD_NOARGS(getAsOverloadedTemplate, CLASS, V, getAsOverloadedTemplate)
    case_METHOD_NOARGS(getAsSubstTemplateTemplateParm, CLASS, V, getAsSubstTemplateTemplateParm)
    case_METHOD_NOARGS(getAsSubstTemplateTemplateParmPack, CLASS, V, getAsSubstTemplateTemplateParmPack)
    case_METHOD_NOARGS(getAsQualifiedTemplateName, CLASS, V, getAsQualifiedTemplateName)
    case_METHOD_NOARGS(getAsDependentTemplateName, CLASS, V, getAsDependentTemplateName)
    case_METHOD_NOARGS(getUnderlying, CLASS, V, getUnderlying)
    case_METHOD_NOARGS(getNameToSubstitute, CLASS, V, getNameToSubstitute)
    case_METHOD_NOARGS(isDependent, PRIM, V, isDependent)
    case_METHOD_NOARGS(isInstantiationDependent, PRIM, V, isInstantiationDependent)
    case_METHOD_NOARGS(containsUnexpandedParameterPack, PRIM, V, containsUnexpandedParameterPack)
    case_METHOD_NOARGS(dump, VOID, V, dump)
    case_METHOD_NOARGS(getAsVoidPointer, PRIM, V, getAsVoidPointer)
    case currefl::getFromVoidPointer: {
      CHECK_TRAIT_IS_REFLPROP(getFromVoidPointer)
      TRY_LOAD_VAL(void *, p0)
      if (no_more) CLASS_REFLECTION(S, getFromVoidPointer, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SubstTemplateTemplateParmStorage *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SubstTemplateTemplateParmStorage;
# define CLASSNAME clang::SubstTemplateTemplateParmStorage
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getParameter, CLASS, P, getParameter)
    case_METHOD_NOARGS(getReplacement, CLASS, P, getReplacement)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::QualifiedTemplateName *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__QualifiedTemplateName;
# define CLASSNAME clang::QualifiedTemplateName
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(hasTemplateKeyword, PRIM, P, hasTemplateKeyword)
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(getTemplateDecl, CLASS, P, getTemplateDecl)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DependentTemplateName *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DependentTemplateName;
# define CLASSNAME clang::DependentTemplateName
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(isIdentifier, PRIM, P, isIdentifier)
    case_METHOD_NOARGS(getIdentifier, CLASS, P, getIdentifier)
    case_METHOD_NOARGS(isOverloadedOperator, PRIM, P, isOverloadedOperator)
    case_METHOD_NOARGS(getOperator, PRIM, P, getOperator)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::LinkageInfo X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__LinkageInfo;
# define CLASSNAME clang::LinkageInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(external, CLASS, S, external)
    case_METHOD_NOARGS(internal, CLASS, S, internal)
    case_METHOD_NOARGS(uniqueExternal, CLASS, S, uniqueExternal)
    case_METHOD_NOARGS(none, CLASS, S, none)
    case_METHOD_NOARGS(visible_none, CLASS, S, visible_none)
    case_METHOD_NOARGS(getLinkage, PRIM, V, getLinkage)
    case_METHOD_NOARGS(getVisibility, PRIM, V, getVisibility)
    case_METHOD_NOARGS(isVisibilityExplicit, PRIM, V, isVisibilityExplicit)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::Qualifiers X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__Qualifiers;
# define CLASSNAME clang::Qualifiers
  switch( (currefl::memnames)N ) {
    case currefl::fromFastMask: {
      CHECK_TRAIT_IS_REFLPROP(fromFastMask)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, fromFastMask, p0)
      TOO_MANY;
    }
    case currefl::fromCVRMask: {
      CHECK_TRAIT_IS_REFLPROP(fromCVRMask)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, fromCVRMask, p0)
      TOO_MANY;
    }
    case currefl::fromCVRUMask: {
      CHECK_TRAIT_IS_REFLPROP(fromCVRUMask)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, fromCVRUMask, p0)
      TOO_MANY;
    }
    case currefl::fromOpaqueValue: {
      CHECK_TRAIT_IS_REFLPROP(fromOpaqueValue)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, fromOpaqueValue, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAsOpaqueValue, PRIM, V, getAsOpaqueValue)
    case_METHOD_NOARGS(hasConst, PRIM, V, hasConst)
    case_METHOD_NOARGS(hasVolatile, PRIM, V, hasVolatile)
    case_METHOD_NOARGS(hasRestrict, PRIM, V, hasRestrict)
    case_METHOD_NOARGS(hasCVRQualifiers, PRIM, V, hasCVRQualifiers)
    case_METHOD_NOARGS(getCVRQualifiers, PRIM, V, getCVRQualifiers)
    case_METHOD_NOARGS(hasUnaligned, PRIM, V, hasUnaligned)
    case_METHOD_NOARGS(hasObjCGCAttr, PRIM, V, hasObjCGCAttr)
    case_METHOD_NOARGS(getObjCGCAttr, PRIM, V, getObjCGCAttr)
    case_METHOD_NOARGS(withoutObjCGCAttr, CLASS, V, withoutObjCGCAttr)
    case_METHOD_NOARGS(withoutObjCLifetime, CLASS, V, withoutObjCLifetime)
    case_METHOD_NOARGS(hasObjCLifetime, PRIM, V, hasObjCLifetime)
    case_METHOD_NOARGS(getObjCLifetime, PRIM, V, getObjCLifetime)
    case_METHOD_NOARGS(hasNonTrivialObjCLifetime, PRIM, V, hasNonTrivialObjCLifetime)
    case_METHOD_NOARGS(hasStrongOrWeakObjCLifetime, PRIM, V, hasStrongOrWeakObjCLifetime)
    case_METHOD_NOARGS(hasAddressSpace, PRIM, V, hasAddressSpace)
    case_METHOD_NOARGS(getAddressSpace, PRIM, V, getAddressSpace)
    case_METHOD_NOARGS(hasTargetSpecificAddressSpace, PRIM, V, hasTargetSpecificAddressSpace)
    case_METHOD_NOARGS(getAddressSpaceAttributePrintValue, PRIM, V, getAddressSpaceAttributePrintValue)
    case_METHOD_NOARGS(hasFastQualifiers, PRIM, V, hasFastQualifiers)
    case_METHOD_NOARGS(getFastQualifiers, PRIM, V, getFastQualifiers)
    case_METHOD_NOARGS(hasNonFastQualifiers, PRIM, V, hasNonFastQualifiers)
    case_METHOD_NOARGS(getNonFastQualifiers, CLASS, V, getNonFastQualifiers)
    case_METHOD_NOARGS(hasQualifiers, PRIM, V, hasQualifiers)
    case_METHOD_NOARGS(empty, PRIM, V, empty)
    case currefl::isAddressSpaceSupersetOf: {
      CHECK_TRAIT_IS_REFLPROP(isAddressSpaceSupersetOf)
      TRY_LOAD_VAL(class clang::Qualifiers, p0)
      if (no_more) PRIM_REFLECTION(V, isAddressSpaceSupersetOf, p0)
      TOO_MANY;
    }
    case currefl::compatiblyIncludes: {
      CHECK_TRAIT_IS_REFLPROP(compatiblyIncludes)
      TRY_LOAD_VAL(class clang::Qualifiers, p0)
      if (no_more) PRIM_REFLECTION(V, compatiblyIncludes, p0)
      TOO_MANY;
    }
    case currefl::compatiblyIncludesObjCLifetime: {
      CHECK_TRAIT_IS_REFLPROP(compatiblyIncludesObjCLifetime)
      TRY_LOAD_VAL(class clang::Qualifiers, p0)
      if (no_more) PRIM_REFLECTION(V, compatiblyIncludesObjCLifetime, p0)
      TOO_MANY;
    }
    case currefl::isStrictSupersetOf: {
      CHECK_TRAIT_IS_REFLPROP(isStrictSupersetOf)
      TRY_LOAD_VAL(class clang::Qualifiers, p0)
      if (no_more) PRIM_REFLECTION(V, isStrictSupersetOf, p0)
      TOO_MANY;
    }
    case currefl::operator_eq_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_eq_eq)
      TRY_LOAD_VAL(class clang::Qualifiers, p0)
      if (no_more) PRIM_REFLECTION(V, operator==, p0)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_not_eq)
      TRY_LOAD_VAL(class clang::Qualifiers, p0)
      if (no_more) PRIM_REFLECTION(V, operator!=, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(operator_bool, PRIM, V, operator bool)
    case currefl::operator_plus: {
      TRY_LOAD_VAL(class clang::Qualifiers, p0 )
      TRY_LOAD_VAL(class clang::Qualifiers, p1 )
      if (no_more) CLASS_REFLECTION_CALL(p0 + p1)
      TOO_MANY;
    }
    case currefl::operator_minus: {
      TRY_LOAD_VAL(class clang::Qualifiers, p0 )
      TRY_LOAD_VAL(class clang::Qualifiers, p1 )
      if (no_more) CLASS_REFLECTION_CALL(p0 - p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAsString, PRIM, V, getAsString)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SplitQualType X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SplitQualType;
# define CLASSNAME clang::SplitQualType
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, V, Ty)
    case_FIELD(CLASS, V, Quals)
    case_METHOD_NOARGS(getSingleStepDesugaredType, CLASS, V, getSingleStepDesugaredType)
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(struct clang::SplitQualType, p0 )
      TRY_LOAD_VAL(struct clang::SplitQualType, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(struct clang::SplitQualType, p0 )
      TRY_LOAD_VAL(struct clang::SplitQualType, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::QualType X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__QualType;
# define CLASSNAME clang::QualType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocalFastQualifiers, PRIM, V, getLocalFastQualifiers)
    case_METHOD_NOARGS(getTypePtr, CLASS, V, getTypePtr)
    case_METHOD_NOARGS(getTypePtrOrNull, CLASS, V, getTypePtrOrNull)
    case_METHOD_NOARGS(getBaseTypeIdentifier, CLASS, V, getBaseTypeIdentifier)
    case_METHOD_NOARGS(split, CLASS, V, split)
    case_METHOD_NOARGS(getAsOpaquePtr, PRIM, V, getAsOpaquePtr)
    case currefl::getFromOpaquePtr: {
      CHECK_TRAIT_IS_REFLPROP(getFromOpaquePtr)
      TRY_LOAD_VAL(const void *, p0)
      if (no_more) CLASS_REFLECTION(S, getFromOpaquePtr, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(operator_star, CLASS, V, operator*)
    case_METHOD_NOARGS(operator_arrow, CLASS, V, operator->)
    case_METHOD_NOARGS(isCanonical, PRIM, V, isCanonical)
    case_METHOD_NOARGS(isCanonicalAsParam, PRIM, V, isCanonicalAsParam)
    case_METHOD_NOARGS(isNull, PRIM, V, isNull)
    case_METHOD_NOARGS(isLocalConstQualified, PRIM, V, isLocalConstQualified)
    case_METHOD_NOARGS(isConstQualified, PRIM, V, isConstQualified)
    case_METHOD_NOARGS(isLocalRestrictQualified, PRIM, V, isLocalRestrictQualified)
    case_METHOD_NOARGS(isRestrictQualified, PRIM, V, isRestrictQualified)
    case_METHOD_NOARGS(isLocalVolatileQualified, PRIM, V, isLocalVolatileQualified)
    case_METHOD_NOARGS(isVolatileQualified, PRIM, V, isVolatileQualified)
    case_METHOD_NOARGS(hasLocalQualifiers, PRIM, V, hasLocalQualifiers)
    case_METHOD_NOARGS(hasQualifiers, PRIM, V, hasQualifiers)
    case_METHOD_NOARGS(hasLocalNonFastQualifiers, PRIM, V, hasLocalNonFastQualifiers)
    case_METHOD_NOARGS(getLocalQualifiers, CLASS, V, getLocalQualifiers)
    case_METHOD_NOARGS(getQualifiers, CLASS, V, getQualifiers)
    case_METHOD_NOARGS(getLocalCVRQualifiers, PRIM, V, getLocalCVRQualifiers)
    case_METHOD_NOARGS(getCVRQualifiers, PRIM, V, getCVRQualifiers)
    case_METHOD_NOARGS(mayBeDynamicClass, PRIM, V, mayBeDynamicClass)
    case_METHOD_NOARGS(mayBeNotDynamicClass, PRIM, V, mayBeNotDynamicClass)
    case_METHOD_NOARGS(withConst, CLASS, V, withConst)
    case_METHOD_NOARGS(withVolatile, CLASS, V, withVolatile)
    case_METHOD_NOARGS(withRestrict, CLASS, V, withRestrict)
    case currefl::withCVRQualifiers: {
      CHECK_TRAIT_IS_REFLPROP(withCVRQualifiers)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, withCVRQualifiers, p0)
      TOO_MANY;
    }
    case currefl::withFastQualifiers: {
      CHECK_TRAIT_IS_REFLPROP(withFastQualifiers)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, withFastQualifiers, p0)
      TOO_MANY;
    }
    case currefl::withExactLocalFastQualifiers: {
      CHECK_TRAIT_IS_REFLPROP(withExactLocalFastQualifiers)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, withExactLocalFastQualifiers, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(withoutLocalFastQualifiers, CLASS, V, withoutLocalFastQualifiers)
    case_METHOD_NOARGS(getCanonicalType, CLASS, V, getCanonicalType)
    case_METHOD_NOARGS(getLocalUnqualifiedType, CLASS, V, getLocalUnqualifiedType)
    case_METHOD_NOARGS(getUnqualifiedType, CLASS, V, getUnqualifiedType)
    case_METHOD_NOARGS(getSplitUnqualifiedType, CLASS, V, getSplitUnqualifiedType)
    case currefl::isMoreQualifiedThan: {
      CHECK_TRAIT_IS_REFLPROP(isMoreQualifiedThan)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(V, isMoreQualifiedThan, p0)
      TOO_MANY;
    }
    case currefl::isAtLeastAsQualifiedAs: {
      CHECK_TRAIT_IS_REFLPROP(isAtLeastAsQualifiedAs)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(V, isAtLeastAsQualifiedAs, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNonReferenceType, CLASS, V, getNonReferenceType)
    case_METHOD_NOARGS(getSplitDesugaredType, CLASS, V, getSplitDesugaredType)
    case_METHOD_NOARGS(IgnoreParens, CLASS, V, IgnoreParens)
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(const class clang::QualType &, p0 )
      TRY_LOAD_VAL(const class clang::QualType &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(const class clang::QualType &, p0 )
      TRY_LOAD_VAL(const class clang::QualType &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAsString, PRIM, V, getAsString)
    case_METHOD_NOARGS(getAsString_NoPrependScope, PRIM, V, getAsString_NoPrependScope)
    case currefl::dump: {
      CHECK_TRAIT_IS_REFLPROP(dump)
      TRY_LOAD_VAL(const char *, p0)
      if (no_more) VOID_REFLECTION(V, dump, (const char *)p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(dump1, VOID, V, dump)
    case_METHOD_NOARGS(getAddressSpace, PRIM, V, getAddressSpace)
    case_METHOD_NOARGS(getObjCGCAttr, PRIM, V, getObjCGCAttr)
    case_METHOD_NOARGS(isObjCGCWeak, PRIM, V, isObjCGCWeak)
    case_METHOD_NOARGS(isObjCGCStrong, PRIM, V, isObjCGCStrong)
    case_METHOD_NOARGS(getObjCLifetime, PRIM, V, getObjCLifetime)
    case_METHOD_NOARGS(hasNonTrivialObjCLifetime, PRIM, V, hasNonTrivialObjCLifetime)
    case_METHOD_NOARGS(hasStrongOrWeakObjCLifetime, PRIM, V, hasStrongOrWeakObjCLifetime)
    case_METHOD_NOARGS(isNonTrivialToPrimitiveDefaultInitialize, PRIM, V, isNonTrivialToPrimitiveDefaultInitialize)
    case_METHOD_NOARGS(isNonTrivialToPrimitiveCopy, PRIM, V, isNonTrivialToPrimitiveCopy)
    case_METHOD_NOARGS(isNonTrivialToPrimitiveDestructiveMove, PRIM, V, isNonTrivialToPrimitiveDestructiveMove)
    case_METHOD_NOARGS(isDestructedType, PRIM, V, isDestructedType)
    case_METHOD_NOARGS(isCForbiddenLValueType, PRIM, V, isCForbiddenLValueType)
    case currefl::substObjCMemberType: {
      CHECK_TRAIT_IS_REFLPROP(substObjCMemberType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(const class clang::DeclContext *, p1)
      TRY_LOAD_VAL(enum clang::ObjCSubstitutionContext, p2)
      if (no_more) CLASS_REFLECTION(V, substObjCMemberType, p0, p1, p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAtomicUnqualifiedType, CLASS, V, getAtomicUnqualifiedType)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ExtQualsTypeCommonBase *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ExtQualsTypeCommonBase;
# define CLASSNAME clang::ExtQualsTypeCommonBase
  switch( (currefl::memnames)N ) {
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::Type *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__Type;
# define CLASSNAME clang::Type
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTypeClass, PRIM, P, getTypeClass)
    case_METHOD_NOARGS(isFromAST, PRIM, P, isFromAST)
    case_METHOD_NOARGS(containsUnexpandedParameterPack, PRIM, P, containsUnexpandedParameterPack)
    case_METHOD_NOARGS(isCanonicalUnqualified, PRIM, P, isCanonicalUnqualified)
    case_METHOD_NOARGS(getLocallyUnqualifiedSingleStepDesugaredType, CLASS, P, getLocallyUnqualifiedSingleStepDesugaredType)
    case currefl::isIncompleteType: {
      CHECK_TRAIT_IS_REFLPROP(isIncompleteType)
      if (no_more) PRIM_REFLECTION_0(P, isIncompleteType)
      TRY_LOAD_VAL(class clang::NamedDecl **, p0)
      if (no_more) PRIM_REFLECTION(P, isIncompleteType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isIncompleteOrObjectType, PRIM, P, isIncompleteOrObjectType)
    case_METHOD_NOARGS(isObjectType, PRIM, P, isObjectType)
    case_METHOD_NOARGS(isStandardLayoutType, PRIM, P, isStandardLayoutType)
    case_METHOD_NOARGS(isBuiltinType, PRIM, P, isBuiltinType)
    case currefl::isSpecificBuiltinType: {
      CHECK_TRAIT_IS_REFLPROP(isSpecificBuiltinType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isSpecificBuiltinType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isPlaceholderType, PRIM, P, isPlaceholderType)
    case_METHOD_NOARGS(getAsPlaceholderType, CLASS, P, getAsPlaceholderType)
    case currefl::isSpecificPlaceholderType: {
      CHECK_TRAIT_IS_REFLPROP(isSpecificPlaceholderType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isSpecificPlaceholderType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isNonOverloadPlaceholderType, PRIM, P, isNonOverloadPlaceholderType)
    case_METHOD_NOARGS(isIntegerType, PRIM, P, isIntegerType)
    case_METHOD_NOARGS(isEnumeralType, PRIM, P, isEnumeralType)
    case_METHOD_NOARGS(isScopedEnumeralType, PRIM, P, isScopedEnumeralType)
    case_METHOD_NOARGS(isBooleanType, PRIM, P, isBooleanType)
    case_METHOD_NOARGS(isCharType, PRIM, P, isCharType)
    case_METHOD_NOARGS(isWideCharType, PRIM, P, isWideCharType)
    case_METHOD_NOARGS(isChar8Type, PRIM, P, isChar8Type)
    case_METHOD_NOARGS(isChar16Type, PRIM, P, isChar16Type)
    case_METHOD_NOARGS(isChar32Type, PRIM, P, isChar32Type)
    case_METHOD_NOARGS(isAnyCharacterType, PRIM, P, isAnyCharacterType)
    case_METHOD_NOARGS(isIntegralOrEnumerationType, PRIM, P, isIntegralOrEnumerationType)
    case_METHOD_NOARGS(isIntegralOrUnscopedEnumerationType, PRIM, P, isIntegralOrUnscopedEnumerationType)
    case_METHOD_NOARGS(isRealFloatingType, PRIM, P, isRealFloatingType)
    case_METHOD_NOARGS(isComplexType, PRIM, P, isComplexType)
    case_METHOD_NOARGS(isAnyComplexType, PRIM, P, isAnyComplexType)
    case_METHOD_NOARGS(isFloatingType, PRIM, P, isFloatingType)
    case_METHOD_NOARGS(isHalfType, PRIM, P, isHalfType)
    case_METHOD_NOARGS(isFloat16Type, PRIM, P, isFloat16Type)
    case_METHOD_NOARGS(isFloat128Type, PRIM, P, isFloat128Type)
    case_METHOD_NOARGS(isRealType, PRIM, P, isRealType)
    case_METHOD_NOARGS(isArithmeticType, PRIM, P, isArithmeticType)
    case_METHOD_NOARGS(isVoidType, PRIM, P, isVoidType)
    case_METHOD_NOARGS(isScalarType, PRIM, P, isScalarType)
    case_METHOD_NOARGS(isAggregateType, PRIM, P, isAggregateType)
    case_METHOD_NOARGS(isFundamentalType, PRIM, P, isFundamentalType)
    case_METHOD_NOARGS(isCompoundType, PRIM, P, isCompoundType)
    case_METHOD_NOARGS(isFunctionType, PRIM, P, isFunctionType)
    case_METHOD_NOARGS(isFunctionNoProtoType, PRIM, P, isFunctionNoProtoType)
    case_METHOD_NOARGS(isFunctionProtoType, PRIM, P, isFunctionProtoType)
    case_METHOD_NOARGS(isPointerType, PRIM, P, isPointerType)
    case_METHOD_NOARGS(isAnyPointerType, PRIM, P, isAnyPointerType)
    case_METHOD_NOARGS(isBlockPointerType, PRIM, P, isBlockPointerType)
    case_METHOD_NOARGS(isVoidPointerType, PRIM, P, isVoidPointerType)
    case_METHOD_NOARGS(isReferenceType, PRIM, P, isReferenceType)
    case_METHOD_NOARGS(isLValueReferenceType, PRIM, P, isLValueReferenceType)
    case_METHOD_NOARGS(isRValueReferenceType, PRIM, P, isRValueReferenceType)
    case_METHOD_NOARGS(isFunctionPointerType, PRIM, P, isFunctionPointerType)
    case_METHOD_NOARGS(isMemberPointerType, PRIM, P, isMemberPointerType)
    case_METHOD_NOARGS(isMemberFunctionPointerType, PRIM, P, isMemberFunctionPointerType)
    case_METHOD_NOARGS(isMemberDataPointerType, PRIM, P, isMemberDataPointerType)
    case_METHOD_NOARGS(isArrayType, PRIM, P, isArrayType)
    case_METHOD_NOARGS(isConstantArrayType, PRIM, P, isConstantArrayType)
    case_METHOD_NOARGS(isIncompleteArrayType, PRIM, P, isIncompleteArrayType)
    case_METHOD_NOARGS(isVariableArrayType, PRIM, P, isVariableArrayType)
    case_METHOD_NOARGS(isDependentSizedArrayType, PRIM, P, isDependentSizedArrayType)
    case_METHOD_NOARGS(isRecordType, PRIM, P, isRecordType)
    case_METHOD_NOARGS(isClassType, PRIM, P, isClassType)
    case_METHOD_NOARGS(isStructureType, PRIM, P, isStructureType)
    case_METHOD_NOARGS(isObjCBoxableRecordType, PRIM, P, isObjCBoxableRecordType)
    case_METHOD_NOARGS(isInterfaceType, PRIM, P, isInterfaceType)
    case_METHOD_NOARGS(isStructureOrClassType, PRIM, P, isStructureOrClassType)
    case_METHOD_NOARGS(isUnionType, PRIM, P, isUnionType)
    case_METHOD_NOARGS(isReflectedType, PRIM, P, isReflectedType)
    case_METHOD_NOARGS(isComplexIntegerType, PRIM, P, isComplexIntegerType)
    case_METHOD_NOARGS(isVectorType, PRIM, P, isVectorType)
    case_METHOD_NOARGS(isExtVectorType, PRIM, P, isExtVectorType)
    case_METHOD_NOARGS(isDependentAddressSpaceType, PRIM, P, isDependentAddressSpaceType)
    case_METHOD_NOARGS(isObjCObjectPointerType, PRIM, P, isObjCObjectPointerType)
    case_METHOD_NOARGS(isObjCRetainableType, PRIM, P, isObjCRetainableType)
    case_METHOD_NOARGS(isObjCLifetimeType, PRIM, P, isObjCLifetimeType)
    case_METHOD_NOARGS(isObjCIndirectLifetimeType, PRIM, P, isObjCIndirectLifetimeType)
    case_METHOD_NOARGS(isObjCNSObjectType, PRIM, P, isObjCNSObjectType)
    case_METHOD_NOARGS(isObjCIndependentClassType, PRIM, P, isObjCIndependentClassType)
    case_METHOD_NOARGS(isObjCObjectType, PRIM, P, isObjCObjectType)
    case_METHOD_NOARGS(isObjCQualifiedInterfaceType, PRIM, P, isObjCQualifiedInterfaceType)
    case_METHOD_NOARGS(isObjCQualifiedIdType, PRIM, P, isObjCQualifiedIdType)
    case_METHOD_NOARGS(isObjCQualifiedClassType, PRIM, P, isObjCQualifiedClassType)
    case_METHOD_NOARGS(isObjCObjectOrInterfaceType, PRIM, P, isObjCObjectOrInterfaceType)
    case_METHOD_NOARGS(isObjCIdType, PRIM, P, isObjCIdType)
    case_METHOD_NOARGS(isObjCInertUnsafeUnretainedType, PRIM, P, isObjCInertUnsafeUnretainedType)
    case_METHOD_NOARGS(isObjCClassType, PRIM, P, isObjCClassType)
    case_METHOD_NOARGS(isObjCClassOrClassKindOfType, PRIM, P, isObjCClassOrClassKindOfType)
    case_METHOD_NOARGS(isObjCSelType, PRIM, P, isObjCSelType)
    case_METHOD_NOARGS(isObjCBuiltinType, PRIM, P, isObjCBuiltinType)
    case_METHOD_NOARGS(isObjCARCBridgableType, PRIM, P, isObjCARCBridgableType)
    case_METHOD_NOARGS(isCARCBridgableType, PRIM, P, isCARCBridgableType)
    case_METHOD_NOARGS(isTemplateTypeParmType, PRIM, P, isTemplateTypeParmType)
    case_METHOD_NOARGS(isNullPtrType, PRIM, P, isNullPtrType)
    case_METHOD_NOARGS(isAlignValT, PRIM, P, isAlignValT)
    case_METHOD_NOARGS(isStdByteType, PRIM, P, isStdByteType)
    case_METHOD_NOARGS(isAtomicType, PRIM, P, isAtomicType)
    case_METHOD_NOARGS(isOCLImage1dROType, PRIM, P, isOCLImage1dROType)
    case_METHOD_NOARGS(isOCLImage1dArrayROType, PRIM, P, isOCLImage1dArrayROType)
    case_METHOD_NOARGS(isOCLImage1dBufferROType, PRIM, P, isOCLImage1dBufferROType)
    case_METHOD_NOARGS(isOCLImage2dROType, PRIM, P, isOCLImage2dROType)
    case_METHOD_NOARGS(isOCLImage2dArrayROType, PRIM, P, isOCLImage2dArrayROType)
    case_METHOD_NOARGS(isOCLImage2dDepthROType, PRIM, P, isOCLImage2dDepthROType)
    case_METHOD_NOARGS(isOCLImage2dArrayDepthROType, PRIM, P, isOCLImage2dArrayDepthROType)
    case_METHOD_NOARGS(isOCLImage2dMSAAROType, PRIM, P, isOCLImage2dMSAAROType)
    case_METHOD_NOARGS(isOCLImage2dArrayMSAAROType, PRIM, P, isOCLImage2dArrayMSAAROType)
    case_METHOD_NOARGS(isOCLImage2dMSAADepthROType, PRIM, P, isOCLImage2dMSAADepthROType)
    case_METHOD_NOARGS(isOCLImage2dArrayMSAADepthROType, PRIM, P, isOCLImage2dArrayMSAADepthROType)
    case_METHOD_NOARGS(isOCLImage3dROType, PRIM, P, isOCLImage3dROType)
    case_METHOD_NOARGS(isOCLImage1dWOType, PRIM, P, isOCLImage1dWOType)
    case_METHOD_NOARGS(isOCLImage1dArrayWOType, PRIM, P, isOCLImage1dArrayWOType)
    case_METHOD_NOARGS(isOCLImage1dBufferWOType, PRIM, P, isOCLImage1dBufferWOType)
    case_METHOD_NOARGS(isOCLImage2dWOType, PRIM, P, isOCLImage2dWOType)
    case_METHOD_NOARGS(isOCLImage2dArrayWOType, PRIM, P, isOCLImage2dArrayWOType)
    case_METHOD_NOARGS(isOCLImage2dDepthWOType, PRIM, P, isOCLImage2dDepthWOType)
    case_METHOD_NOARGS(isOCLImage2dArrayDepthWOType, PRIM, P, isOCLImage2dArrayDepthWOType)
    case_METHOD_NOARGS(isOCLImage2dMSAAWOType, PRIM, P, isOCLImage2dMSAAWOType)
    case_METHOD_NOARGS(isOCLImage2dArrayMSAAWOType, PRIM, P, isOCLImage2dArrayMSAAWOType)
    case_METHOD_NOARGS(isOCLImage2dMSAADepthWOType, PRIM, P, isOCLImage2dMSAADepthWOType)
    case_METHOD_NOARGS(isOCLImage2dArrayMSAADepthWOType, PRIM, P, isOCLImage2dArrayMSAADepthWOType)
    case_METHOD_NOARGS(isOCLImage3dWOType, PRIM, P, isOCLImage3dWOType)
    case_METHOD_NOARGS(isOCLImage1dRWType, PRIM, P, isOCLImage1dRWType)
    case_METHOD_NOARGS(isOCLImage1dArrayRWType, PRIM, P, isOCLImage1dArrayRWType)
    case_METHOD_NOARGS(isOCLImage1dBufferRWType, PRIM, P, isOCLImage1dBufferRWType)
    case_METHOD_NOARGS(isOCLImage2dRWType, PRIM, P, isOCLImage2dRWType)
    case_METHOD_NOARGS(isOCLImage2dArrayRWType, PRIM, P, isOCLImage2dArrayRWType)
    case_METHOD_NOARGS(isOCLImage2dDepthRWType, PRIM, P, isOCLImage2dDepthRWType)
    case_METHOD_NOARGS(isOCLImage2dArrayDepthRWType, PRIM, P, isOCLImage2dArrayDepthRWType)
    case_METHOD_NOARGS(isOCLImage2dMSAARWType, PRIM, P, isOCLImage2dMSAARWType)
    case_METHOD_NOARGS(isOCLImage2dArrayMSAARWType, PRIM, P, isOCLImage2dArrayMSAARWType)
    case_METHOD_NOARGS(isOCLImage2dMSAADepthRWType, PRIM, P, isOCLImage2dMSAADepthRWType)
    case_METHOD_NOARGS(isOCLImage2dArrayMSAADepthRWType, PRIM, P, isOCLImage2dArrayMSAADepthRWType)
    case_METHOD_NOARGS(isOCLImage3dRWType, PRIM, P, isOCLImage3dRWType)
    case_METHOD_NOARGS(isImageType, PRIM, P, isImageType)
    case_METHOD_NOARGS(isSamplerT, PRIM, P, isSamplerT)
    case_METHOD_NOARGS(isEventT, PRIM, P, isEventT)
    case_METHOD_NOARGS(isClkEventT, PRIM, P, isClkEventT)
    case_METHOD_NOARGS(isQueueT, PRIM, P, isQueueT)
    case_METHOD_NOARGS(isReserveIDT, PRIM, P, isReserveIDT)
    case_METHOD_NOARGS(isPipeType, PRIM, P, isPipeType)
    case_METHOD_NOARGS(isOpenCLSpecificType, PRIM, P, isOpenCLSpecificType)
    case_METHOD_NOARGS(isObjCARCImplicitlyUnretainedType, PRIM, P, isObjCARCImplicitlyUnretainedType)
    case_METHOD_NOARGS(getObjCARCImplicitLifetime, PRIM, P, getObjCARCImplicitLifetime)
    case_METHOD_NOARGS(getScalarTypeKind, PRIM, P, getScalarTypeKind)
    case_METHOD_NOARGS(isDependentType, PRIM, P, isDependentType)
    case_METHOD_NOARGS(isInstantiationDependentType, PRIM, P, isInstantiationDependentType)
    case_METHOD_NOARGS(isUndeducedType, PRIM, P, isUndeducedType)
    case_METHOD_NOARGS(isVariablyModifiedType, PRIM, P, isVariablyModifiedType)
    case_METHOD_NOARGS(hasSizedVLAType, PRIM, P, hasSizedVLAType)
    case_METHOD_NOARGS(hasUnnamedOrLocalType, PRIM, P, hasUnnamedOrLocalType)
    case_METHOD_NOARGS(isOverloadableType, PRIM, P, isOverloadableType)
    case_METHOD_NOARGS(isElaboratedTypeSpecifier, PRIM, P, isElaboratedTypeSpecifier)
    case_METHOD_NOARGS(canDecayToPointerType, PRIM, P, canDecayToPointerType)
    case_METHOD_NOARGS(hasPointerRepresentation, PRIM, P, hasPointerRepresentation)
    case_METHOD_NOARGS(hasObjCPointerRepresentation, PRIM, P, hasObjCPointerRepresentation)
    case_METHOD_NOARGS(hasIntegerRepresentation, PRIM, P, hasIntegerRepresentation)
    case_METHOD_NOARGS(hasSignedIntegerRepresentation, PRIM, P, hasSignedIntegerRepresentation)
    case_METHOD_NOARGS(hasUnsignedIntegerRepresentation, PRIM, P, hasUnsignedIntegerRepresentation)
    case_METHOD_NOARGS(hasFloatingRepresentation, PRIM, P, hasFloatingRepresentation)
    case_METHOD_NOARGS(getAsStructureType, CLASS, P, getAsStructureType)
    case_METHOD_NOARGS(getAsUnionType, CLASS, P, getAsUnionType)
    case_METHOD_NOARGS(getAsComplexIntegerType, CLASS, P, getAsComplexIntegerType)
    case_METHOD_NOARGS(getAsObjCInterfaceType, CLASS, P, getAsObjCInterfaceType)
    case_METHOD_NOARGS(getAsObjCInterfacePointerType, CLASS, P, getAsObjCInterfacePointerType)
    case_METHOD_NOARGS(getAsObjCQualifiedIdType, CLASS, P, getAsObjCQualifiedIdType)
    case_METHOD_NOARGS(getAsObjCQualifiedClassType, CLASS, P, getAsObjCQualifiedClassType)
    case_METHOD_NOARGS(getAsObjCQualifiedInterfaceType, CLASS, P, getAsObjCQualifiedInterfaceType)
    case_METHOD_NOARGS(getAsCXXRecordDecl, CLASS, P, getAsCXXRecordDecl)
    case_METHOD_NOARGS(getAsRecordDecl, CLASS, P, getAsRecordDecl)
    case_METHOD_NOARGS(getAsTagDecl, CLASS, P, getAsTagDecl)
    case_METHOD_NOARGS(getPointeeCXXRecordDecl, CLASS, P, getPointeeCXXRecordDecl)
    case_METHOD_NOARGS(getContainedDeducedType, CLASS, P, getContainedDeducedType)
    case_METHOD_NOARGS(getContainedAutoType, CLASS, P, getContainedAutoType)
    case_METHOD_NOARGS(hasAutoForTrailingReturnType, PRIM, P, hasAutoForTrailingReturnType)
    case_METHOD_NOARGS(getAsArrayTypeUnsafe, CLASS, P, getAsArrayTypeUnsafe)
    case_METHOD_NOARGS(castAsArrayTypeUnsafe, CLASS, P, castAsArrayTypeUnsafe)
    case_METHOD_NOARGS(getBaseElementTypeUnsafe, CLASS, P, getBaseElementTypeUnsafe)
    case_METHOD_NOARGS(getArrayElementTypeNoTypeQual, CLASS, P, getArrayElementTypeNoTypeQual)
    case_METHOD_NOARGS(getPointeeOrArrayElementType, CLASS, P, getPointeeOrArrayElementType)
    case_METHOD_NOARGS(getPointeeType, CLASS, P, getPointeeType)
    case_METHOD_NOARGS(getUnqualifiedDesugaredType, CLASS, P, getUnqualifiedDesugaredType)
    case_METHOD_NOARGS(isPromotableIntegerType, PRIM, P, isPromotableIntegerType)
    case_METHOD_NOARGS(isSignedIntegerType, PRIM, P, isSignedIntegerType)
    case_METHOD_NOARGS(isUnsignedIntegerType, PRIM, P, isUnsignedIntegerType)
    case_METHOD_NOARGS(isSignedIntegerOrEnumerationType, PRIM, P, isSignedIntegerOrEnumerationType)
    case_METHOD_NOARGS(isUnsignedIntegerOrEnumerationType, PRIM, P, isUnsignedIntegerOrEnumerationType)
    case_METHOD_NOARGS(isFixedPointType, PRIM, P, isFixedPointType)
    case_METHOD_NOARGS(isSaturatedFixedPointType, PRIM, P, isSaturatedFixedPointType)
    case_METHOD_NOARGS(isUnsaturatedFixedPointType, PRIM, P, isUnsaturatedFixedPointType)
    case_METHOD_NOARGS(isSignedFixedPointType, PRIM, P, isSignedFixedPointType)
    case_METHOD_NOARGS(isUnsignedFixedPointType, PRIM, P, isUnsignedFixedPointType)
    case_METHOD_NOARGS(isConstantSizeType, PRIM, P, isConstantSizeType)
    case_METHOD_NOARGS(isSpecifierType, PRIM, P, isSpecifierType)
    case_METHOD_NOARGS(getLinkage, PRIM, P, getLinkage)
    case_METHOD_NOARGS(getVisibility, PRIM, P, getVisibility)
    case_METHOD_NOARGS(isVisibilityExplicit, PRIM, P, isVisibilityExplicit)
    case_METHOD_NOARGS(getLinkageAndVisibility, CLASS, P, getLinkageAndVisibility)
    case_METHOD_NOARGS(isLinkageValid, PRIM, P, isLinkageValid)
    case currefl::canHaveNullability: {
      CHECK_TRAIT_IS_REFLPROP(canHaveNullability)
      if (no_more) PRIM_REFLECTION_0(P, canHaveNullability)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) PRIM_REFLECTION(P, canHaveNullability, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(acceptsObjCTypeParams, PRIM, P, acceptsObjCTypeParams)
    case_METHOD_NOARGS(getTypeClassName, PRIM, P, getTypeClassName)
    case_METHOD_NOARGS(getCanonicalTypeInternal, CLASS, P, getCanonicalTypeInternal)
    case_METHOD_NOARGS(dump, VOID, P, dump)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::BuiltinType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__BuiltinType;
# define CLASSNAME clang::BuiltinType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case_METHOD_NOARGS(isInteger, PRIM, P, isInteger)
    case_METHOD_NOARGS(isSignedInteger, PRIM, P, isSignedInteger)
    case_METHOD_NOARGS(isUnsignedInteger, PRIM, P, isUnsignedInteger)
    case_METHOD_NOARGS(isFloatingPoint, PRIM, P, isFloatingPoint)
    case currefl::isPlaceholderTypeKind: {
      CHECK_TRAIT_IS_REFLPROP(isPlaceholderTypeKind)
      TRY_LOAD_VAL(enum clang::BuiltinType::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, isPlaceholderTypeKind, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isPlaceholderType, PRIM, P, isPlaceholderType)
    case_METHOD_NOARGS(isNonOverloadPlaceholderType, PRIM, P, isNonOverloadPlaceholderType)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ComplexType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ComplexType;
# define CLASSNAME clang::ComplexType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getElementType, CLASS, P, getElementType)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ParenType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ParenType;
# define CLASSNAME clang::ParenType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getInnerType, CLASS, P, getInnerType)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::PointerType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__PointerType;
# define CLASSNAME clang::PointerType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getPointeeType, CLASS, P, getPointeeType)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::AdjustedType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__AdjustedType;
# define CLASSNAME clang::AdjustedType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOriginalType, CLASS, P, getOriginalType)
    case_METHOD_NOARGS(getAdjustedType, CLASS, P, getAdjustedType)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DecayedType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DecayedType;
# define CLASSNAME clang::DecayedType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDecayedType, CLASS, P, getDecayedType)
    case_METHOD_NOARGS(getPointeeType, CLASS, P, getPointeeType)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::BlockPointerType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__BlockPointerType;
# define CLASSNAME clang::BlockPointerType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getPointeeType, CLASS, P, getPointeeType)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ReferenceType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ReferenceType;
# define CLASSNAME clang::ReferenceType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isSpelledAsLValue, PRIM, P, isSpelledAsLValue)
    case_METHOD_NOARGS(isInnerRef, PRIM, P, isInnerRef)
    case_METHOD_NOARGS(getPointeeTypeAsWritten, CLASS, P, getPointeeTypeAsWritten)
    case_METHOD_NOARGS(getPointeeType, CLASS, P, getPointeeType)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::LValueReferenceType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__LValueReferenceType;
# define CLASSNAME clang::LValueReferenceType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::RValueReferenceType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__RValueReferenceType;
# define CLASSNAME clang::RValueReferenceType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::MemberPointerType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__MemberPointerType;
# define CLASSNAME clang::MemberPointerType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getPointeeType, CLASS, P, getPointeeType)
    case_METHOD_NOARGS(isMemberFunctionPointer, PRIM, P, isMemberFunctionPointer)
    case_METHOD_NOARGS(isMemberDataPointer, PRIM, P, isMemberDataPointer)
    case_METHOD_NOARGS(getClass, CLASS, P, getClass)
    case_METHOD_NOARGS(getMostRecentCXXRecordDecl, CLASS, P, getMostRecentCXXRecordDecl)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ArrayType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ArrayType;
# define CLASSNAME clang::ArrayType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getElementType, CLASS, P, getElementType)
    case_METHOD_NOARGS(getSizeModifier, PRIM, P, getSizeModifier)
    case_METHOD_NOARGS(getIndexTypeQualifiers, CLASS, P, getIndexTypeQualifiers)
    case_METHOD_NOARGS(getIndexTypeCVRQualifiers, PRIM, P, getIndexTypeCVRQualifiers)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ConstantArrayType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ConstantArrayType;
# define CLASSNAME clang::ConstantArrayType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSize, CLASS, P, getSize)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::IncompleteArrayType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__IncompleteArrayType;
# define CLASSNAME clang::IncompleteArrayType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::VariableArrayType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__VariableArrayType;
# define CLASSNAME clang::VariableArrayType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSizeExpr, CLASS, P, getSizeExpr)
    case_METHOD_NOARGS(getBracketsRange, CLASS, P, getBracketsRange)
    case_METHOD_NOARGS(getLBracketLoc, CLASS, P, getLBracketLoc)
    case_METHOD_NOARGS(getRBracketLoc, CLASS, P, getRBracketLoc)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DependentSizedArrayType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DependentSizedArrayType;
# define CLASSNAME clang::DependentSizedArrayType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSizeExpr, CLASS, P, getSizeExpr)
    case_METHOD_NOARGS(getBracketsRange, CLASS, P, getBracketsRange)
    case_METHOD_NOARGS(getLBracketLoc, CLASS, P, getLBracketLoc)
    case_METHOD_NOARGS(getRBracketLoc, CLASS, P, getRBracketLoc)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DependentAddressSpaceType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DependentAddressSpaceType;
# define CLASSNAME clang::DependentAddressSpaceType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAddrSpaceExpr, CLASS, P, getAddrSpaceExpr)
    case_METHOD_NOARGS(getPointeeType, CLASS, P, getPointeeType)
    case_METHOD_NOARGS(getAttributeLoc, CLASS, P, getAttributeLoc)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DependentSizedExtVectorType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DependentSizedExtVectorType;
# define CLASSNAME clang::DependentSizedExtVectorType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSizeExpr, CLASS, P, getSizeExpr)
    case_METHOD_NOARGS(getElementType, CLASS, P, getElementType)
    case_METHOD_NOARGS(getAttributeLoc, CLASS, P, getAttributeLoc)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::VectorType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__VectorType;
# define CLASSNAME clang::VectorType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getElementType, CLASS, P, getElementType)
    case_METHOD_NOARGS(getNumElements, PRIM, P, getNumElements)
    case currefl::isVectorSizeTooLarge: {
      CHECK_TRAIT_IS_REFLPROP(isVectorSizeTooLarge)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(S, isVectorSizeTooLarge, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case_METHOD_NOARGS(getVectorKind, PRIM, P, getVectorKind)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DependentVectorType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DependentVectorType;
# define CLASSNAME clang::DependentVectorType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSizeExpr, CLASS, P, getSizeExpr)
    case_METHOD_NOARGS(getElementType, CLASS, P, getElementType)
    case_METHOD_NOARGS(getAttributeLoc, CLASS, P, getAttributeLoc)
    case_METHOD_NOARGS(getVectorKind, PRIM, P, getVectorKind)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ExtVectorType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ExtVectorType;
# define CLASSNAME clang::ExtVectorType
  switch( (currefl::memnames)N ) {
    case currefl::getPointAccessorIdx: {
      CHECK_TRAIT_IS_REFLPROP(getPointAccessorIdx)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(S, getPointAccessorIdx, p0)
      TOO_MANY;
    }
    case currefl::getNumericAccessorIdx: {
      CHECK_TRAIT_IS_REFLPROP(getNumericAccessorIdx)
      TRY_LOAD_VAL(char, p0)
      if (no_more) PRIM_REFLECTION(S, getNumericAccessorIdx, p0)
      TOO_MANY;
    }
    case currefl::getAccessorIdx: {
      CHECK_TRAIT_IS_REFLPROP(getAccessorIdx)
      TRY_LOAD_VAL(char, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) PRIM_REFLECTION(S, getAccessorIdx, p0, p1)
      TOO_MANY;
    }
    case currefl::isAccessorWithinNumElements: {
      CHECK_TRAIT_IS_REFLPROP(isAccessorWithinNumElements)
      TRY_LOAD_VAL(char, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) PRIM_REFLECTION(P, isAccessorWithinNumElements, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FunctionType;
# define CLASSNAME clang::FunctionType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getReturnType, CLASS, P, getReturnType)
    case_METHOD_NOARGS(getHasRegParm, PRIM, P, getHasRegParm)
    case_METHOD_NOARGS(getRegParmType, PRIM, P, getRegParmType)
    case_METHOD_NOARGS(getNoReturnAttr, PRIM, P, getNoReturnAttr)
    case_METHOD_NOARGS(getCallConv, PRIM, P, getCallConv)
    case_METHOD_NOARGS(getExtInfo, CLASS, P, getExtInfo)
    case_METHOD_NOARGS(isConst, PRIM, P, isConst)
    case_METHOD_NOARGS(isVolatile, PRIM, P, isVolatile)
    case_METHOD_NOARGS(isRestrict, PRIM, P, isRestrict)
    case currefl::getNameForCallConv: {
      CHECK_TRAIT_IS_REFLPROP(getNameForCallConv)
      TRY_LOAD_VAL(enum clang::CallingConv, p0)
      if (no_more) CLASS_REFLECTION(S, getNameForCallConv, p0)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionType::ExtInfo X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FunctionType__ExtInfo;
# define CLASSNAME clang::FunctionType::ExtInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNoReturn, PRIM, V, getNoReturn)
    case_METHOD_NOARGS(getProducesResult, PRIM, V, getProducesResult)
    case_METHOD_NOARGS(getNoCallerSavedRegs, PRIM, V, getNoCallerSavedRegs)
    case_METHOD_NOARGS(getNoCfCheck, PRIM, V, getNoCfCheck)
    case_METHOD_NOARGS(getHasRegParm, PRIM, V, getHasRegParm)
    case_METHOD_NOARGS(getRegParm, PRIM, V, getRegParm)
    case_METHOD_NOARGS(getCC, PRIM, V, getCC)
    case currefl::operator_eq_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_eq_eq)
      TRY_LOAD_VAL(class clang::FunctionType::ExtInfo, p0)
      if (no_more) PRIM_REFLECTION(V, operator==, p0)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      CHECK_TRAIT_IS_REFLPROP(operator_not_eq)
      TRY_LOAD_VAL(class clang::FunctionType::ExtInfo, p0)
      if (no_more) PRIM_REFLECTION(V, operator!=, p0)
      TOO_MANY;
    }
    case currefl::withNoReturn: {
      CHECK_TRAIT_IS_REFLPROP(withNoReturn)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(V, withNoReturn, p0)
      TOO_MANY;
    }
    case currefl::withProducesResult: {
      CHECK_TRAIT_IS_REFLPROP(withProducesResult)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(V, withProducesResult, p0)
      TOO_MANY;
    }
    case currefl::withNoCallerSavedRegs: {
      CHECK_TRAIT_IS_REFLPROP(withNoCallerSavedRegs)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(V, withNoCallerSavedRegs, p0)
      TOO_MANY;
    }
    case currefl::withNoCfCheck: {
      CHECK_TRAIT_IS_REFLPROP(withNoCfCheck)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(V, withNoCfCheck, p0)
      TOO_MANY;
    }
    case currefl::withRegParm: {
      CHECK_TRAIT_IS_REFLPROP(withRegParm)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(V, withRegParm, p0)
      TOO_MANY;
    }
    case currefl::withCallingConv: {
      CHECK_TRAIT_IS_REFLPROP(withCallingConv)
      TRY_LOAD_VAL(enum clang::CallingConv, p0)
      if (no_more) CLASS_REFLECTION(V, withCallingConv, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionNoProtoType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FunctionNoProtoType;
# define CLASSNAME clang::FunctionNoProtoType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionProtoType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FunctionProtoType;
# define CLASSNAME clang::FunctionProtoType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNumParams, PRIM, P, getNumParams)
    case currefl::getParamType: {
      CHECK_TRAIT_IS_REFLPROP(getParamType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getParamType, p0)
      TOO_MANY;
    }
    case currefl::getParamTypes: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getParamTypes))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getParamTypes)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getExtProtoInfo, CLASS, P, getExtProtoInfo)
    case_METHOD_NOARGS(getExceptionSpecType, PRIM, P, getExceptionSpecType)
    case_METHOD_NOARGS(hasExceptionSpec, PRIM, P, hasExceptionSpec)
    case_METHOD_NOARGS(hasDynamicExceptionSpec, PRIM, P, hasDynamicExceptionSpec)
    case_METHOD_NOARGS(hasNoexceptExceptionSpec, PRIM, P, hasNoexceptExceptionSpec)
    case_METHOD_NOARGS(hasDependentExceptionSpec, PRIM, P, hasDependentExceptionSpec)
    case_METHOD_NOARGS(hasInstantiationDependentExceptionSpec, PRIM, P, hasInstantiationDependentExceptionSpec)
    case_METHOD_NOARGS(getNumExceptions, PRIM, P, getNumExceptions)
    case currefl::getExceptionType: {
      CHECK_TRAIT_IS_REFLPROP(getExceptionType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getExceptionType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNoexceptExpr, CLASS, P, getNoexceptExpr)
    case_METHOD_NOARGS(getExceptionSpecDecl, CLASS, P, getExceptionSpecDecl)
    case_METHOD_NOARGS(getExceptionSpecTemplate, CLASS, P, getExceptionSpecTemplate)
    case_METHOD_NOARGS(canThrow, PRIM, P, canThrow)
    case currefl::isNothrow: {
      CHECK_TRAIT_IS_REFLPROP(isNothrow)
      if (no_more) PRIM_REFLECTION_0(P, isNothrow)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) PRIM_REFLECTION(P, isNothrow, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isVariadic, PRIM, P, isVariadic)
    case_METHOD_NOARGS(isTemplateVariadic, PRIM, P, isTemplateVariadic)
    case_METHOD_NOARGS(hasTrailingReturn, PRIM, P, hasTrailingReturn)
    case_METHOD_NOARGS(getTypeQuals, PRIM, P, getTypeQuals)
    case_METHOD_NOARGS(getRefQualifier, PRIM, P, getRefQualifier)
    case currefl::param_types: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::param_types))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, param_types)
      TOO_MANY;
    }
    case currefl::exceptions: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::exceptions))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, exceptions)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasExtParameterInfos, PRIM, P, hasExtParameterInfos)
    case currefl::getExtParameterInfos: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getExtParameterInfos))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getExtParameterInfos)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getExtParameterInfosOrNull, CLASS, P, getExtParameterInfosOrNull)
    case currefl::getExtParameterInfo: {
      CHECK_TRAIT_IS_REFLPROP(getExtParameterInfo)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getExtParameterInfo, p0)
      TOO_MANY;
    }
    case currefl::getParameterABI: {
      CHECK_TRAIT_IS_REFLPROP(getParameterABI)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getParameterABI, p0)
      TOO_MANY;
    }
    case currefl::isParamConsumed: {
      CHECK_TRAIT_IS_REFLPROP(isParamConsumed)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isParamConsumed, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionProtoType::ExtParameterInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FunctionProtoType__ExtParameterInfo;
# define CLASSNAME clang::FunctionProtoType::ExtParameterInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getABI, PRIM, P, getABI)
    case currefl::withABI: {
      CHECK_TRAIT_IS_REFLPROP(withABI)
      TRY_LOAD_VAL(enum clang::ParameterABI, p0)
      if (no_more) CLASS_REFLECTION(P, withABI, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isConsumed, PRIM, P, isConsumed)
    case currefl::withIsConsumed: {
      CHECK_TRAIT_IS_REFLPROP(withIsConsumed)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(P, withIsConsumed, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasPassObjectSize, PRIM, P, hasPassObjectSize)
    case_METHOD_NOARGS(withHasPassObjectSize, CLASS, P, withHasPassObjectSize)
    case_METHOD_NOARGS(isNoEscape, PRIM, P, isNoEscape)
    case currefl::withIsNoEscape: {
      CHECK_TRAIT_IS_REFLPROP(withIsNoEscape)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(P, withIsNoEscape, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getOpaqueValue, PRIM, P, getOpaqueValue)
    case currefl::getFromOpaqueValue: {
      CHECK_TRAIT_IS_REFLPROP(getFromOpaqueValue)
      TRY_LOAD_VAL(unsigned char, p0)
      if (no_more) CLASS_REFLECTION(S, getFromOpaqueValue, p0)
      TOO_MANY;
    }
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(class clang::FunctionProtoType::ExtParameterInfo, p0 )
      TRY_LOAD_VAL(class clang::FunctionProtoType::ExtParameterInfo, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(class clang::FunctionProtoType::ExtParameterInfo, p0 )
      TRY_LOAD_VAL(class clang::FunctionProtoType::ExtParameterInfo, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionProtoType::ExtParameterInfo X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FunctionProtoType__ExtParameterInfo;
# define CLASSNAME clang::FunctionProtoType::ExtParameterInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getABI, PRIM, V, getABI)
    case currefl::withABI: {
      CHECK_TRAIT_IS_REFLPROP(withABI)
      TRY_LOAD_VAL(enum clang::ParameterABI, p0)
      if (no_more) CLASS_REFLECTION(V, withABI, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isConsumed, PRIM, V, isConsumed)
    case currefl::withIsConsumed: {
      CHECK_TRAIT_IS_REFLPROP(withIsConsumed)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(V, withIsConsumed, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasPassObjectSize, PRIM, V, hasPassObjectSize)
    case_METHOD_NOARGS(withHasPassObjectSize, CLASS, V, withHasPassObjectSize)
    case_METHOD_NOARGS(isNoEscape, PRIM, V, isNoEscape)
    case currefl::withIsNoEscape: {
      CHECK_TRAIT_IS_REFLPROP(withIsNoEscape)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(V, withIsNoEscape, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getOpaqueValue, PRIM, V, getOpaqueValue)
    case currefl::getFromOpaqueValue: {
      CHECK_TRAIT_IS_REFLPROP(getFromOpaqueValue)
      TRY_LOAD_VAL(unsigned char, p0)
      if (no_more) CLASS_REFLECTION(S, getFromOpaqueValue, p0)
      TOO_MANY;
    }
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(class clang::FunctionProtoType::ExtParameterInfo, p0 )
      TRY_LOAD_VAL(class clang::FunctionProtoType::ExtParameterInfo, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(class clang::FunctionProtoType::ExtParameterInfo, p0 )
      TRY_LOAD_VAL(class clang::FunctionProtoType::ExtParameterInfo, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionProtoType::ExceptionSpecInfo X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FunctionProtoType__ExceptionSpecInfo;
# define CLASSNAME clang::FunctionProtoType::ExceptionSpecInfo
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, V, Type)
    case_FIELD(CLASS, V, NoexceptExpr)
    case_FIELD(CLASS, V, SourceDecl)
    case_FIELD(CLASS, V, SourceTemplate)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionProtoType::ExtProtoInfo X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FunctionProtoType__ExtProtoInfo;
# define CLASSNAME clang::FunctionProtoType::ExtProtoInfo
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, V, ExtInfo)
    case_FIELD(PRIM, V, Variadic)
    case_FIELD(PRIM, V, HasTrailingReturn)
    case_FIELD(PRIM, V, TypeQuals)
    case_FIELD(PRIM, V, RefQualifier)
    case_FIELD(CLASS, V, ExceptionSpec)
    case_FIELD(CLASS, V, ExtParameterInfos)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UnresolvedUsingType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UnresolvedUsingType;
# define CLASSNAME clang::UnresolvedUsingType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypedefType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TypedefType;
# define CLASSNAME clang::TypedefType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypeOfExprType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TypeOfExprType;
# define CLASSNAME clang::TypeOfExprType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getUnderlyingExpr, CLASS, P, getUnderlyingExpr)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypeOfType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TypeOfType;
# define CLASSNAME clang::TypeOfType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getUnderlyingType, CLASS, P, getUnderlyingType)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DecltypeType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DecltypeType;
# define CLASSNAME clang::DecltypeType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getUnderlyingExpr, CLASS, P, getUnderlyingExpr)
    case_METHOD_NOARGS(getUnderlyingType, CLASS, P, getUnderlyingType)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ReflectedType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ReflectedType;
# define CLASSNAME clang::ReflectedType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTypeReflection, CLASS, P, getTypeReflection)
    case_METHOD_NOARGS(getUnderlyingType, CLASS, P, getUnderlyingType)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UnaryTransformType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UnaryTransformType;
# define CLASSNAME clang::UnaryTransformType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case_METHOD_NOARGS(getUnderlyingType, CLASS, P, getUnderlyingType)
    case_METHOD_NOARGS(getBaseType, CLASS, P, getBaseType)
    case_METHOD_NOARGS(getUTTKind, PRIM, P, getUTTKind)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TagType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TagType;
# define CLASSNAME clang::TagType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(isBeingDefined, PRIM, P, isBeingDefined)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::RecordType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__RecordType;
# define CLASSNAME clang::RecordType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(hasConstFields, PRIM, P, hasConstFields)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::EnumType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__EnumType;
# define CLASSNAME clang::EnumType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::AttributedType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__AttributedType;
# define CLASSNAME clang::AttributedType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAttrKind, PRIM, P, getAttrKind)
    case_METHOD_NOARGS(getModifiedType, CLASS, P, getModifiedType)
    case_METHOD_NOARGS(getEquivalentType, CLASS, P, getEquivalentType)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case_METHOD_NOARGS(isQualifier, PRIM, P, isQualifier)
    case_METHOD_NOARGS(isMSTypeSpec, PRIM, P, isMSTypeSpec)
    case_METHOD_NOARGS(isCallingConv, PRIM, P, isCallingConv)
    case currefl::getNullabilityAttrKind: {
      CHECK_TRAIT_IS_REFLPROP(getNullabilityAttrKind)
      TRY_LOAD_VAL(enum clang::NullabilityKind, p0)
      if (no_more) PRIM_REFLECTION(S, getNullabilityAttrKind, p0)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateTypeParmType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TemplateTypeParmType;
# define CLASSNAME clang::TemplateTypeParmType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDepth, PRIM, P, getDepth)
    case_METHOD_NOARGS(getIndex, PRIM, P, getIndex)
    case_METHOD_NOARGS(isParameterPack, PRIM, P, isParameterPack)
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(getIdentifier, CLASS, P, getIdentifier)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SubstTemplateTypeParmType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__SubstTemplateTypeParmType;
# define CLASSNAME clang::SubstTemplateTypeParmType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getReplacedParameter, CLASS, P, getReplacedParameter)
    case_METHOD_NOARGS(getReplacementType, CLASS, P, getReplacementType)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SubstTemplateTypeParmPackType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__SubstTemplateTypeParmPackType;
# define CLASSNAME clang::SubstTemplateTypeParmPackType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getIdentifier, CLASS, P, getIdentifier)
    case_METHOD_NOARGS(getReplacedParameter, CLASS, P, getReplacedParameter)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case_METHOD_NOARGS(getArgumentPack, CLASS, P, getArgumentPack)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeducedType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DeducedType;
# define CLASSNAME clang::DeducedType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case_METHOD_NOARGS(getDeducedType, CLASS, P, getDeducedType)
    case_METHOD_NOARGS(isDeduced, PRIM, P, isDeduced)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::AutoType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__AutoType;
# define CLASSNAME clang::AutoType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isDecltypeAuto, PRIM, P, isDecltypeAuto)
    case_METHOD_NOARGS(getKeyword, PRIM, P, getKeyword)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeducedTemplateSpecializationType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DeducedTemplateSpecializationType;
# define CLASSNAME clang::DeducedTemplateSpecializationType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTemplateName, CLASS, P, getTemplateName)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateSpecializationType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TemplateSpecializationType;
# define CLASSNAME clang::TemplateSpecializationType
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(P)
      TOO_MANY;
};
    case_METHOD_NOARGS(isCurrentInstantiation, PRIM, P, isCurrentInstantiation)
    case_METHOD_NOARGS(isTypeAlias, PRIM, P, isTypeAlias)
    case_METHOD_NOARGS(getAliasedType, CLASS, P, getAliasedType)
    case_METHOD_NOARGS(getTemplateName, CLASS, P, getTemplateName)
    case_METHOD_NOARGS(getArgs, CLASS, P, getArgs)
    case_METHOD_NOARGS(getNumArgs, PRIM, P, getNumArgs)
    case currefl::getArg: {
      CHECK_TRAIT_IS_REFLPROP(getArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArg, p0)
      TOO_MANY;
    }
    case currefl::template_arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::template_arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, template_arguments)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::InjectedClassNameType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__InjectedClassNameType;
# define CLASSNAME clang::InjectedClassNameType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getInjectedSpecializationType, CLASS, P, getInjectedSpecializationType)
    case_METHOD_NOARGS(getInjectedTST, CLASS, P, getInjectedTST)
    case_METHOD_NOARGS(getTemplateName, CLASS, P, getTemplateName)
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypeWithKeyword *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TypeWithKeyword;
# define CLASSNAME clang::TypeWithKeyword
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKeyword, PRIM, P, getKeyword)
    case currefl::getKeywordForTypeSpec: {
      CHECK_TRAIT_IS_REFLPROP(getKeywordForTypeSpec)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(S, getKeywordForTypeSpec, p0)
      TOO_MANY;
    }
    case currefl::getTagTypeKindForTypeSpec: {
      CHECK_TRAIT_IS_REFLPROP(getTagTypeKindForTypeSpec)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(S, getTagTypeKindForTypeSpec, p0)
      TOO_MANY;
    }
    case currefl::getKeywordForTagTypeKind: {
      CHECK_TRAIT_IS_REFLPROP(getKeywordForTagTypeKind)
      TRY_LOAD_VAL(enum clang::TagTypeKind, p0)
      if (no_more) PRIM_REFLECTION(S, getKeywordForTagTypeKind, p0)
      TOO_MANY;
    }
    case currefl::getTagTypeKindForKeyword: {
      CHECK_TRAIT_IS_REFLPROP(getTagTypeKindForKeyword)
      TRY_LOAD_VAL(enum clang::ElaboratedTypeKeyword, p0)
      if (no_more) PRIM_REFLECTION(S, getTagTypeKindForKeyword, p0)
      TOO_MANY;
    }
    case currefl::KeywordIsTagTypeKind: {
      CHECK_TRAIT_IS_REFLPROP(KeywordIsTagTypeKind)
      TRY_LOAD_VAL(enum clang::ElaboratedTypeKeyword, p0)
      if (no_more) PRIM_REFLECTION(S, KeywordIsTagTypeKind, p0)
      TOO_MANY;
    }
    case currefl::getKeywordName: {
      CHECK_TRAIT_IS_REFLPROP(getKeywordName)
      TRY_LOAD_VAL(enum clang::ElaboratedTypeKeyword, p0)
      if (no_more) CLASS_REFLECTION(S, getKeywordName, p0)
      TOO_MANY;
    }
    case currefl::getTagTypeKindName: {
      CHECK_TRAIT_IS_REFLPROP(getTagTypeKindName)
      TRY_LOAD_VAL(enum clang::TagTypeKind, p0)
      if (no_more) CLASS_REFLECTION(S, getTagTypeKindName, p0)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) CLASS_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypeWithKeyword::CannotCastToThisType X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TypeWithKeyword__CannotCastToThisType;
# define CLASSNAME clang::TypeWithKeyword::CannotCastToThisType
  switch( (currefl::memnames)N ) {
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ElaboratedType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ElaboratedType;
# define CLASSNAME clang::ElaboratedType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getNamedType, CLASS, P, getNamedType)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(getOwnedTagDecl, CLASS, P, getOwnedTagDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DependentNameType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DependentNameType;
# define CLASSNAME clang::DependentNameType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getIdentifier, CLASS, P, getIdentifier)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DependentTemplateSpecializationType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DependentTemplateSpecializationType;
# define CLASSNAME clang::DependentTemplateSpecializationType
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(P)
      TOO_MANY;
};
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getIdentifier, CLASS, P, getIdentifier)
    case_METHOD_NOARGS(getArgs, CLASS, P, getArgs)
    case_METHOD_NOARGS(getNumArgs, PRIM, P, getNumArgs)
    case currefl::getArg: {
      CHECK_TRAIT_IS_REFLPROP(getArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArg, p0)
      TOO_MANY;
    }
    case currefl::template_arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::template_arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, template_arguments)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::PackExpansionType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__PackExpansionType;
# define CLASSNAME clang::PackExpansionType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getPattern, CLASS, P, getPattern)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCTypeParamType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCTypeParamType;
# define CLASSNAME clang::ObjCTypeParamType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCObjectType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCObjectType;
# define CLASSNAME clang::ObjCObjectType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBaseType, CLASS, P, getBaseType)
    case_METHOD_NOARGS(isObjCId, PRIM, P, isObjCId)
    case_METHOD_NOARGS(isObjCClass, PRIM, P, isObjCClass)
    case_METHOD_NOARGS(isObjCUnqualifiedId, PRIM, P, isObjCUnqualifiedId)
    case_METHOD_NOARGS(isObjCUnqualifiedClass, PRIM, P, isObjCUnqualifiedClass)
    case_METHOD_NOARGS(isObjCUnqualifiedIdOrClass, PRIM, P, isObjCUnqualifiedIdOrClass)
    case_METHOD_NOARGS(isObjCQualifiedId, PRIM, P, isObjCQualifiedId)
    case_METHOD_NOARGS(isObjCQualifiedClass, PRIM, P, isObjCQualifiedClass)
    case_METHOD_NOARGS(getInterface, CLASS, P, getInterface)
    case_METHOD_NOARGS(isSpecialized, PRIM, P, isSpecialized)
    case_METHOD_NOARGS(isSpecializedAsWritten, PRIM, P, isSpecializedAsWritten)
    case_METHOD_NOARGS(isUnspecialized, PRIM, P, isUnspecialized)
    case_METHOD_NOARGS(isUnspecializedAsWritten, PRIM, P, isUnspecializedAsWritten)
    case currefl::getTypeArgs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getTypeArgs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getTypeArgs)
      TOO_MANY;
    }
    case currefl::getTypeArgsAsWritten: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getTypeArgsAsWritten))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getTypeArgsAsWritten)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isKindOfTypeAsWritten, PRIM, P, isKindOfTypeAsWritten)
    case_METHOD_NOARGS(isKindOfType, PRIM, P, isKindOfType)
    case_METHOD_NOARGS(getSuperClassType, CLASS, P, getSuperClassType)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCInterfaceType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCInterfaceType;
# define CLASSNAME clang::ObjCInterfaceType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCObjectPointerType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCObjectPointerType;
# define CLASSNAME clang::ObjCObjectPointerType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getPointeeType, CLASS, P, getPointeeType)
    case_METHOD_NOARGS(getObjectType, CLASS, P, getObjectType)
    case_METHOD_NOARGS(getInterfaceType, CLASS, P, getInterfaceType)
    case_METHOD_NOARGS(getInterfaceDecl, CLASS, P, getInterfaceDecl)
    case_METHOD_NOARGS(isObjCIdType, PRIM, P, isObjCIdType)
    case_METHOD_NOARGS(isObjCClassType, PRIM, P, isObjCClassType)
    case_METHOD_NOARGS(isObjCIdOrClassType, PRIM, P, isObjCIdOrClassType)
    case_METHOD_NOARGS(isObjCQualifiedIdType, PRIM, P, isObjCQualifiedIdType)
    case_METHOD_NOARGS(isObjCQualifiedClassType, PRIM, P, isObjCQualifiedClassType)
    case_METHOD_NOARGS(isKindOfType, PRIM, P, isKindOfType)
    case_METHOD_NOARGS(isSpecialized, PRIM, P, isSpecialized)
    case_METHOD_NOARGS(isSpecializedAsWritten, PRIM, P, isSpecializedAsWritten)
    case_METHOD_NOARGS(isUnspecialized, PRIM, P, isUnspecialized)
    case_METHOD_NOARGS(isUnspecializedAsWritten, PRIM, P, isUnspecializedAsWritten)
    case currefl::getTypeArgs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getTypeArgs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getTypeArgs)
      TOO_MANY;
    }
    case currefl::getTypeArgsAsWritten: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getTypeArgsAsWritten))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getTypeArgsAsWritten)
      TOO_MANY;
    }
    case currefl::quals: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::quals))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, quals)
      TOO_MANY;
    }
    case_METHOD_NOARGS(qual_empty, PRIM, P, qual_empty)
    case_METHOD_NOARGS(getNumProtocols, PRIM, P, getNumProtocols)
    case currefl::getProtocol: {
      CHECK_TRAIT_IS_REFLPROP(getProtocol)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getProtocol, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case_METHOD_NOARGS(getSuperClassType, CLASS, P, getSuperClassType)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::AtomicType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__AtomicType;
# define CLASSNAME clang::AtomicType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getValueType, CLASS, P, getValueType)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::PipeType *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__PipeType;
# define CLASSNAME clang::PipeType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getElementType, CLASS, P, getElementType)
    case_METHOD_NOARGS(isSugared, PRIM, P, isSugared)
    case_METHOD_NOARGS(desugar, CLASS, P, desugar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isReadOnly, PRIM, P, isReadOnly)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypeSourceInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TypeSourceInfo;
# define CLASSNAME clang::TypeSourceInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getType, CLASS, P, getType)
    case_METHOD_NOARGS(getTypeLoc, CLASS, P, getTypeLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TranslationUnitDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TranslationUnitDecl;
# define CLASSNAME clang::TranslationUnitDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getASTContext, CLASS, P, getASTContext)
    case_METHOD_NOARGS(getAnonymousNamespace, CLASS, P, getAnonymousNamespace)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::PragmaCommentDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__PragmaCommentDecl;
# define CLASSNAME clang::PragmaCommentDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCommentKind, PRIM, P, getCommentKind)
    case_METHOD_NOARGS(getArg, CLASS, P, getArg)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::PragmaDetectMismatchDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__PragmaDetectMismatchDecl;
# define CLASSNAME clang::PragmaDetectMismatchDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getName, CLASS, P, getName)
    case_METHOD_NOARGS(getValue, CLASS, P, getValue)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ExternCContextDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ExternCContextDecl;
# define CLASSNAME clang::ExternCContextDecl
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::NamedDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__NamedDecl;
# define CLASSNAME clang::NamedDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getIdentifier, CLASS, P, getIdentifier)
    case_METHOD_NOARGS(getName, CLASS, P, getName)
    case_METHOD_NOARGS(getNameAsString, PRIM, P, getNameAsString)
    case_METHOD_NOARGS(getDeclName, CLASS, P, getDeclName)
    case_METHOD_NOARGS(getQualifiedNameAsString, PRIM, P, getQualifiedNameAsString)
    case currefl::declarationReplaces: {
      CHECK_TRAIT_IS_REFLPROP(declarationReplaces)
      TRY_LOAD_VAL(class clang::NamedDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, declarationReplaces, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) PRIM_REFLECTION(P, declarationReplaces, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasLinkage, PRIM, P, hasLinkage)
    case_METHOD_NOARGS(isCXXClassMember, PRIM, P, isCXXClassMember)
    case_METHOD_NOARGS(isCXXInstanceMember, PRIM, P, isCXXInstanceMember)
    case_METHOD_NOARGS(getLinkageInternal, PRIM, P, getLinkageInternal)
    case_METHOD_NOARGS(getFormalLinkage, PRIM, P, getFormalLinkage)
    case_METHOD_NOARGS(hasExternalFormalLinkage, PRIM, P, hasExternalFormalLinkage)
    case_METHOD_NOARGS(isExternallyVisible, PRIM, P, isExternallyVisible)
    case_METHOD_NOARGS(isExternallyDeclarable, PRIM, P, isExternallyDeclarable)
    case_METHOD_NOARGS(getVisibility, PRIM, P, getVisibility)
    case_METHOD_NOARGS(getLinkageAndVisibility, CLASS, P, getLinkageAndVisibility)
    case_METHOD_NOARGS(isLinkageValid, PRIM, P, isLinkageValid)
    case_METHOD_NOARGS(hasLinkageBeenComputed, PRIM, P, hasLinkageBeenComputed)
    case_METHOD_NOARGS(getUnderlyingDecl, CLASS, P, getUnderlyingDecl)
    case_METHOD_NOARGS(getMostRecentDecl, CLASS, P, getMostRecentDecl)
    case_METHOD_NOARGS(getObjCFStringFormattingFamily, PRIM, P, getObjCFStringFormattingFamily)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::LabelDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__LabelDecl;
# define CLASSNAME clang::LabelDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getStmt, CLASS, P, getStmt)
    case_METHOD_NOARGS(isGnuLocal, PRIM, P, isGnuLocal)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(isMSAsmLabel, PRIM, P, isMSAsmLabel)
    case_METHOD_NOARGS(isResolvedMSAsmLabel, PRIM, P, isResolvedMSAsmLabel)
    case_METHOD_NOARGS(getMSAsmLabel, CLASS, P, getMSAsmLabel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::NamespaceDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__NamespaceDecl;
# define CLASSNAME clang::NamespaceDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isAnonymousNamespace, PRIM, P, isAnonymousNamespace)
    case_METHOD_NOARGS(isInline, PRIM, P, isInline)
    case_METHOD_NOARGS(getOriginalNamespace, CLASS, P, getOriginalNamespace)
    case_METHOD_NOARGS(isOriginalNamespace, PRIM, P, isOriginalNamespace)
    case_METHOD_NOARGS(getAnonymousNamespace, CLASS, P, getAnonymousNamespace)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getRBraceLoc, CLASS, P, getRBraceLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ValueDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ValueDecl;
# define CLASSNAME clang::ValueDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getType, CLASS, P, getType)
    case_METHOD_NOARGS(isWeak, PRIM, P, isWeak)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclaratorDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DeclaratorDecl;
# define CLASSNAME clang::DeclaratorDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
    case_METHOD_NOARGS(getInnerLocStart, CLASS, P, getInnerLocStart)
    case_METHOD_NOARGS(getOuterLocStart, CLASS, P, getOuterLocStart)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getNumTemplateParameterLists, PRIM, P, getNumTemplateParameterLists)
    case currefl::getTemplateParameterList: {
      CHECK_TRAIT_IS_REFLPROP(getTemplateParameterList)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getTemplateParameterList, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getTypeSpecStartLoc, CLASS, P, getTypeSpecStartLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::EvaluatedStmt *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__EvaluatedStmt;
# define CLASSNAME clang::EvaluatedStmt
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, P, WasEvaluated)
    case_FIELD(PRIM, P, IsEvaluating)
    case_FIELD(PRIM, P, CheckedICE)
    case_FIELD(PRIM, P, CheckingICE)
    case_FIELD(PRIM, P, IsICE)
    case_FIELD(CLASS, P, Value)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::VarDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__VarDecl;
# define CLASSNAME clang::VarDecl
  switch( (currefl::memnames)N ) {
    case currefl::getStorageClassSpecifierString: {
      CHECK_TRAIT_IS_REFLPROP(getStorageClassSpecifierString)
      TRY_LOAD_VAL(enum clang::StorageClass, p0)
      if (no_more) PRIM_REFLECTION(S, getStorageClassSpecifierString, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getStorageClass, PRIM, P, getStorageClass)
    case_METHOD_NOARGS(getTSCSpec, PRIM, P, getTSCSpec)
    case_METHOD_NOARGS(getTLSKind, PRIM, P, getTLSKind)
    case_METHOD_NOARGS(hasLocalStorage, PRIM, P, hasLocalStorage)
    case_METHOD_NOARGS(isStaticLocal, PRIM, P, isStaticLocal)
    case_METHOD_NOARGS(hasExternalStorage, PRIM, P, hasExternalStorage)
    case_METHOD_NOARGS(hasGlobalStorage, PRIM, P, hasGlobalStorage)
    case_METHOD_NOARGS(getStorageDuration, PRIM, P, getStorageDuration)
    case_METHOD_NOARGS(getLanguageLinkage, PRIM, P, getLanguageLinkage)
    case_METHOD_NOARGS(isExternC, PRIM, P, isExternC)
    case_METHOD_NOARGS(isInExternCContext, PRIM, P, isInExternCContext)
    case_METHOD_NOARGS(isInExternCXXContext, PRIM, P, isInExternCXXContext)
    case_METHOD_NOARGS(isLocalVarDecl, PRIM, P, isLocalVarDecl)
    case_METHOD_NOARGS(isLocalVarDeclOrParm, PRIM, P, isLocalVarDeclOrParm)
    case_METHOD_NOARGS(isFunctionOrMethodVarDecl, PRIM, P, isFunctionOrMethodVarDecl)
    case_METHOD_NOARGS(isStaticDataMember, PRIM, P, isStaticDataMember)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(isThisDeclarationADefinition, PRIM, P, isThisDeclarationADefinition)
    case_METHOD_NOARGS(hasDefinition, PRIM, P, hasDefinition)
    case_METHOD_NOARGS(getActingDefinition, CLASS, P, getActingDefinition)
    case_METHOD_NOARGS(getDefinition, CLASS, P, getDefinition)
    case_METHOD_NOARGS(isOutOfLine, PRIM, P, isOutOfLine)
    case_METHOD_NOARGS(isFileVarDecl, PRIM, P, isFileVarDecl)
    case_METHOD_NOARGS(getAnyInitializer, CLASS, P, getAnyInitializer)
    case_METHOD_NOARGS(hasInit, PRIM, P, hasInit)
    case_METHOD_NOARGS(getInit, CLASS, P, getInit)
    case_METHOD_NOARGS(ensureEvaluatedStmt, CLASS, P, ensureEvaluatedStmt)
    case_METHOD_NOARGS(evaluateValue, CLASS, P, evaluateValue)
    case_METHOD_NOARGS(getEvaluatedValue, CLASS, P, getEvaluatedValue)
    case_METHOD_NOARGS(isInitKnownICE, PRIM, P, isInitKnownICE)
    case_METHOD_NOARGS(isInitICE, PRIM, P, isInitICE)
    case_METHOD_NOARGS(checkInitIsICE, PRIM, P, checkInitIsICE)
    case_METHOD_NOARGS(getInitStyle, PRIM, P, getInitStyle)
    case_METHOD_NOARGS(isDirectInit, PRIM, P, isDirectInit)
    case_METHOD_NOARGS(isThisDeclarationADemotedDefinition, PRIM, P, isThisDeclarationADemotedDefinition)
    case_METHOD_NOARGS(isExceptionVariable, PRIM, P, isExceptionVariable)
    case_METHOD_NOARGS(isNRVOVariable, PRIM, P, isNRVOVariable)
    case_METHOD_NOARGS(isCXXForRangeDecl, PRIM, P, isCXXForRangeDecl)
    case_METHOD_NOARGS(isObjCForDecl, PRIM, P, isObjCForDecl)
    case_METHOD_NOARGS(isARCPseudoStrong, PRIM, P, isARCPseudoStrong)
    case_METHOD_NOARGS(isInline, PRIM, P, isInline)
    case_METHOD_NOARGS(isInlineSpecified, PRIM, P, isInlineSpecified)
    case_METHOD_NOARGS(isConstexpr, PRIM, P, isConstexpr)
    case_METHOD_NOARGS(isInitCapture, PRIM, P, isInitCapture)
    case_METHOD_NOARGS(isPreviousDeclInSameBlockScope, PRIM, P, isPreviousDeclInSameBlockScope)
    case_METHOD_NOARGS(getTemplateInstantiationPattern, CLASS, P, getTemplateInstantiationPattern)
    case_METHOD_NOARGS(getInstantiatedFromStaticDataMember, CLASS, P, getInstantiatedFromStaticDataMember)
    case_METHOD_NOARGS(getTemplateSpecializationKind, PRIM, P, getTemplateSpecializationKind)
    case_METHOD_NOARGS(getPointOfInstantiation, CLASS, P, getPointOfInstantiation)
    case_METHOD_NOARGS(getMemberSpecializationInfo, CLASS, P, getMemberSpecializationInfo)
    case_METHOD_NOARGS(getDescribedVarTemplate, CLASS, P, getDescribedVarTemplate)
    case_METHOD_NOARGS(isKnownToBeDefined, PRIM, P, isKnownToBeDefined)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ImplicitParamDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ImplicitParamDecl;
# define CLASSNAME clang::ImplicitParamDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getParameterKind, PRIM, P, getParameterKind)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ParmVarDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ParmVarDecl;
# define CLASSNAME clang::ParmVarDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(isObjCMethodParameter, PRIM, P, isObjCMethodParameter)
    case_METHOD_NOARGS(getFunctionScopeDepth, PRIM, P, getFunctionScopeDepth)
    case_METHOD_NOARGS(getFunctionScopeIndex, PRIM, P, getFunctionScopeIndex)
    case_METHOD_NOARGS(getObjCDeclQualifier, PRIM, P, getObjCDeclQualifier)
    case_METHOD_NOARGS(isKNRPromoted, PRIM, P, isKNRPromoted)
    case_METHOD_NOARGS(getDefaultArg, CLASS, P, getDefaultArg)
    case_METHOD_NOARGS(getDefaultArgRange, CLASS, P, getDefaultArgRange)
    case_METHOD_NOARGS(getUninstantiatedDefaultArg, CLASS, P, getUninstantiatedDefaultArg)
    case_METHOD_NOARGS(hasDefaultArg, PRIM, P, hasDefaultArg)
    case_METHOD_NOARGS(hasUnparsedDefaultArg, PRIM, P, hasUnparsedDefaultArg)
    case_METHOD_NOARGS(hasUninstantiatedDefaultArg, PRIM, P, hasUninstantiatedDefaultArg)
    case_METHOD_NOARGS(hasInheritedDefaultArg, PRIM, P, hasInheritedDefaultArg)
    case_METHOD_NOARGS(getOriginalType, CLASS, P, getOriginalType)
    case_METHOD_NOARGS(isParameterPack, PRIM, P, isParameterPack)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FunctionDecl;
# define CLASSNAME clang::FunctionDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNameInfo, CLASS, P, getNameInfo)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(hasBody, PRIM, P, hasBody)
    case_METHOD_NOARGS(hasTrivialBody, PRIM, P, hasTrivialBody)
    case_METHOD_NOARGS(isDefined, PRIM, P, isDefined)
    case_METHOD_NOARGS(getDefinition, CLASS, P, getDefinition)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(isThisDeclarationADefinition, PRIM, P, isThisDeclarationADefinition)
    case_METHOD_NOARGS(doesThisDeclarationHaveABody, PRIM, P, doesThisDeclarationHaveABody)
    case_METHOD_NOARGS(isVariadic, PRIM, P, isVariadic)
    case_METHOD_NOARGS(isVirtualAsWritten, PRIM, P, isVirtualAsWritten)
    case_METHOD_NOARGS(isPure, PRIM, P, isPure)
    case_METHOD_NOARGS(isLateTemplateParsed, PRIM, P, isLateTemplateParsed)
    case_METHOD_NOARGS(isTrivial, PRIM, P, isTrivial)
    case_METHOD_NOARGS(isTrivialForCall, PRIM, P, isTrivialForCall)
    case_METHOD_NOARGS(isDefaulted, PRIM, P, isDefaulted)
    case_METHOD_NOARGS(isExplicitlyDefaulted, PRIM, P, isExplicitlyDefaulted)
    case_METHOD_NOARGS(hasImplicitReturnZero, PRIM, P, hasImplicitReturnZero)
    case_METHOD_NOARGS(hasPrototype, PRIM, P, hasPrototype)
    case_METHOD_NOARGS(hasWrittenPrototype, PRIM, P, hasWrittenPrototype)
    case_METHOD_NOARGS(hasInheritedPrototype, PRIM, P, hasInheritedPrototype)
    case_METHOD_NOARGS(isConstexpr, PRIM, P, isConstexpr)
    case_METHOD_NOARGS(instantiationIsPending, PRIM, P, instantiationIsPending)
    case_METHOD_NOARGS(usesSEHTry, PRIM, P, usesSEHTry)
    case_METHOD_NOARGS(isDeleted, PRIM, P, isDeleted)
    case_METHOD_NOARGS(isDeletedAsWritten, PRIM, P, isDeletedAsWritten)
    case_METHOD_NOARGS(isMain, PRIM, P, isMain)
    case_METHOD_NOARGS(isMSVCRTEntryPoint, PRIM, P, isMSVCRTEntryPoint)
    case_METHOD_NOARGS(isReservedGlobalPlacementOperator, PRIM, P, isReservedGlobalPlacementOperator)
    case currefl::isReplaceableGlobalAllocationFunction: {
      CHECK_TRAIT_IS_REFLPROP(isReplaceableGlobalAllocationFunction)
      if (no_more) PRIM_REFLECTION_0(P, isReplaceableGlobalAllocationFunction)
      TRY_LOAD_VAL(bool *, p0)
      if (no_more) PRIM_REFLECTION(P, isReplaceableGlobalAllocationFunction, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isDestroyingOperatorDelete, PRIM, P, isDestroyingOperatorDelete)
    case_METHOD_NOARGS(getLanguageLinkage, PRIM, P, getLanguageLinkage)
    case_METHOD_NOARGS(isExternC, PRIM, P, isExternC)
    case_METHOD_NOARGS(isInExternCContext, PRIM, P, isInExternCContext)
    case_METHOD_NOARGS(isInExternCXXContext, PRIM, P, isInExternCXXContext)
    case_METHOD_NOARGS(isGlobal, PRIM, P, isGlobal)
    case_METHOD_NOARGS(isNoReturn, PRIM, P, isNoReturn)
    case_METHOD_NOARGS(hasSkippedBody, PRIM, P, hasSkippedBody)
    case_METHOD_NOARGS(willHaveBody, PRIM, P, willHaveBody)
    case_METHOD_NOARGS(isMetaprogram, PRIM, P, isMetaprogram)
    case_METHOD_NOARGS(isMultiVersion, PRIM, P, isMultiVersion)
    case_METHOD_NOARGS(isCPUDispatchMultiVersion, PRIM, P, isCPUDispatchMultiVersion)
    case_METHOD_NOARGS(isCPUSpecificMultiVersion, PRIM, P, isCPUSpecificMultiVersion)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getBuiltinID, PRIM, P, getBuiltinID)
    case currefl::parameters: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::parameters))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, parameters)
      TOO_MANY;
    }
    case_METHOD_NOARGS(param_empty, PRIM, P, param_empty)
    case_METHOD_NOARGS(param_size, PRIM, P, param_size)
    case_METHOD_NOARGS(getNumParams, PRIM, P, getNumParams)
    case currefl::getParamDecl: {
      CHECK_TRAIT_IS_REFLPROP(getParamDecl)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getParamDecl, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getMinRequiredArguments, PRIM, P, getMinRequiredArguments)
    case_METHOD_NOARGS(getReturnType, CLASS, P, getReturnType)
    case_METHOD_NOARGS(getReturnTypeSourceRange, CLASS, P, getReturnTypeSourceRange)
    case_METHOD_NOARGS(getExceptionSpecSourceRange, CLASS, P, getExceptionSpecSourceRange)
    case_METHOD_NOARGS(getCallResultType, CLASS, P, getCallResultType)
    case_METHOD_NOARGS(getUnusedResultAttr, CLASS, P, getUnusedResultAttr)
    case_METHOD_NOARGS(hasUnusedResultAttr, PRIM, P, hasUnusedResultAttr)
    case_METHOD_NOARGS(getStorageClass, PRIM, P, getStorageClass)
    case_METHOD_NOARGS(isInlineSpecified, PRIM, P, isInlineSpecified)
    case_METHOD_NOARGS(isInlined, PRIM, P, isInlined)
    case_METHOD_NOARGS(isInlineDefinitionExternallyVisible, PRIM, P, isInlineDefinitionExternallyVisible)
    case_METHOD_NOARGS(isMSExternInline, PRIM, P, isMSExternInline)
    case_METHOD_NOARGS(doesDeclarationForceExternallyVisibleDefinition, PRIM, P, doesDeclarationForceExternallyVisibleDefinition)
    case_METHOD_NOARGS(isOverloadedOperator, PRIM, P, isOverloadedOperator)
    case_METHOD_NOARGS(getOverloadedOperator, PRIM, P, getOverloadedOperator)
    case_METHOD_NOARGS(getLiteralIdentifier, CLASS, P, getLiteralIdentifier)
    case_METHOD_NOARGS(getInstantiatedFromMemberFunction, CLASS, P, getInstantiatedFromMemberFunction)
    case_METHOD_NOARGS(getTemplatedKind, PRIM, P, getTemplatedKind)
    case_METHOD_NOARGS(getMemberSpecializationInfo, CLASS, P, getMemberSpecializationInfo)
    case_METHOD_NOARGS(getDescribedFunctionTemplate, CLASS, P, getDescribedFunctionTemplate)
    case_METHOD_NOARGS(isFunctionTemplateSpecialization, PRIM, P, isFunctionTemplateSpecialization)
    case_METHOD_NOARGS(getClassScopeSpecializationPattern, CLASS, P, getClassScopeSpecializationPattern)
    case_METHOD_NOARGS(getTemplateSpecializationInfo, CLASS, P, getTemplateSpecializationInfo)
    case_METHOD_NOARGS(isImplicitlyInstantiable, PRIM, P, isImplicitlyInstantiable)
    case_METHOD_NOARGS(isTemplateInstantiation, PRIM, P, isTemplateInstantiation)
    case_METHOD_NOARGS(getTemplateInstantiationPattern, CLASS, P, getTemplateInstantiationPattern)
    case_METHOD_NOARGS(getPrimaryTemplate, CLASS, P, getPrimaryTemplate)
    case_METHOD_NOARGS(getTemplateSpecializationArgs, CLASS, P, getTemplateSpecializationArgs)
    case_METHOD_NOARGS(getTemplateSpecializationArgsAsWritten, CLASS, P, getTemplateSpecializationArgsAsWritten)
    case_METHOD_NOARGS(getDependentSpecializationInfo, CLASS, P, getDependentSpecializationInfo)
    case_METHOD_NOARGS(getTemplateSpecializationKind, PRIM, P, getTemplateSpecializationKind)
    case_METHOD_NOARGS(getPointOfInstantiation, CLASS, P, getPointOfInstantiation)
    case_METHOD_NOARGS(isOutOfLine, PRIM, P, isOutOfLine)
    case_METHOD_NOARGS(getMemoryFunctionKind, PRIM, P, getMemoryFunctionKind)
    case_METHOD_NOARGS(getODRHash, PRIM, P, getODRHash)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FieldDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FieldDecl;
# define CLASSNAME clang::FieldDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getFieldIndex, PRIM, P, getFieldIndex)
    case_METHOD_NOARGS(isMutable, PRIM, P, isMutable)
    case_METHOD_NOARGS(isBitField, PRIM, P, isBitField)
    case_METHOD_NOARGS(isUnnamedBitfield, PRIM, P, isUnnamedBitfield)
    case_METHOD_NOARGS(isAnonymousStructOrUnion, PRIM, P, isAnonymousStructOrUnion)
    case_METHOD_NOARGS(getBitWidth, CLASS, P, getBitWidth)
    case_METHOD_NOARGS(getInClassInitStyle, PRIM, P, getInClassInitStyle)
    case_METHOD_NOARGS(hasInClassInitializer, PRIM, P, hasInClassInitializer)
    case_METHOD_NOARGS(getInClassInitializer, CLASS, P, getInClassInitializer)
    case_METHOD_NOARGS(hasCapturedVLAType, PRIM, P, hasCapturedVLAType)
    case_METHOD_NOARGS(getCapturedVLAType, CLASS, P, getCapturedVLAType)
    case_METHOD_NOARGS(getParent, CLASS, P, getParent)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::EnumConstantDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__EnumConstantDecl;
# define CLASSNAME clang::EnumConstantDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getInitExpr, CLASS, P, getInitExpr)
    case_METHOD_NOARGS(getInitVal, CLASS, P, getInitVal)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::IndirectFieldDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__IndirectFieldDecl;
# define CLASSNAME clang::IndirectFieldDecl
  switch( (currefl::memnames)N ) {
    case currefl::chain: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::chain))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, chain)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getChainingSize, PRIM, P, getChainingSize)
    case_METHOD_NOARGS(getAnonField, CLASS, P, getAnonField)
    case_METHOD_NOARGS(getVarDecl, CLASS, P, getVarDecl)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypeDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TypeDecl;
# define CLASSNAME clang::TypeDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTypeForDecl, CLASS, P, getTypeForDecl)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypedefNameDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TypedefNameDecl;
# define CLASSNAME clang::TypedefNameDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isModed, PRIM, P, isModed)
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
    case_METHOD_NOARGS(getUnderlyingType, CLASS, P, getUnderlyingType)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::getAnonDeclWithTypedefName: {
      CHECK_TRAIT_IS_REFLPROP(getAnonDeclWithTypedefName)
      if (no_more) CLASS_REFLECTION_0(P, getAnonDeclWithTypedefName)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(P, getAnonDeclWithTypedefName, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isTransparentTag, PRIM, P, isTransparentTag)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypedefDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TypedefDecl;
# define CLASSNAME clang::TypedefDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypeAliasDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TypeAliasDecl;
# define CLASSNAME clang::TypeAliasDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getDescribedAliasTemplate, CLASS, P, getDescribedAliasTemplate)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TagDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TagDecl;
# define CLASSNAME clang::TagDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBraceRange, CLASS, P, getBraceRange)
    case_METHOD_NOARGS(getInnerLocStart, CLASS, P, getInnerLocStart)
    case_METHOD_NOARGS(getOuterLocStart, CLASS, P, getOuterLocStart)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(isThisDeclarationADefinition, PRIM, P, isThisDeclarationADefinition)
    case_METHOD_NOARGS(isCompleteDefinition, PRIM, P, isCompleteDefinition)
    case_METHOD_NOARGS(isCompleteDefinitionRequired, PRIM, P, isCompleteDefinitionRequired)
    case_METHOD_NOARGS(isBeingDefined, PRIM, P, isBeingDefined)
    case_METHOD_NOARGS(isEmbeddedInDeclarator, PRIM, P, isEmbeddedInDeclarator)
    case_METHOD_NOARGS(isFreeStanding, PRIM, P, isFreeStanding)
    case_METHOD_NOARGS(isDependentType, PRIM, P, isDependentType)
    case_METHOD_NOARGS(getDefinition, CLASS, P, getDefinition)
    case_METHOD_NOARGS(getKindName, CLASS, P, getKindName)
    case_METHOD_NOARGS(getTagKind, PRIM, P, getTagKind)
    case_METHOD_NOARGS(isStruct, PRIM, P, isStruct)
    case_METHOD_NOARGS(isInterface, PRIM, P, isInterface)
    case_METHOD_NOARGS(isClass, PRIM, P, isClass)
    case_METHOD_NOARGS(isUnion, PRIM, P, isUnion)
    case_METHOD_NOARGS(isEnum, PRIM, P, isEnum)
    case_METHOD_NOARGS(hasNameForLinkage, PRIM, P, hasNameForLinkage)
    case_METHOD_NOARGS(getTypedefNameForAnonDecl, CLASS, P, getTypedefNameForAnonDecl)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getNumTemplateParameterLists, PRIM, P, getNumTemplateParameterLists)
    case currefl::getTemplateParameterList: {
      CHECK_TRAIT_IS_REFLPROP(getTemplateParameterList)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getTemplateParameterList, p0)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::EnumDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__EnumDecl;
# define CLASSNAME clang::EnumDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getPreviousDecl, CLASS, P, getPreviousDecl)
    case_METHOD_NOARGS(getMostRecentDecl, CLASS, P, getMostRecentDecl)
    case_METHOD_NOARGS(getDefinition, CLASS, P, getDefinition)
    case currefl::enumerators: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::enumerators))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, enumerators)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getPromotionType, CLASS, P, getPromotionType)
    case_METHOD_NOARGS(getIntegerType, CLASS, P, getIntegerType)
    case_METHOD_NOARGS(getIntegerTypeSourceInfo, CLASS, P, getIntegerTypeSourceInfo)
    case_METHOD_NOARGS(getIntegerTypeRange, CLASS, P, getIntegerTypeRange)
    case_METHOD_NOARGS(getNumPositiveBits, PRIM, P, getNumPositiveBits)
    case_METHOD_NOARGS(getNumNegativeBits, PRIM, P, getNumNegativeBits)
    case_METHOD_NOARGS(isScoped, PRIM, P, isScoped)
    case_METHOD_NOARGS(isScopedUsingClassTag, PRIM, P, isScopedUsingClassTag)
    case_METHOD_NOARGS(isFixed, PRIM, P, isFixed)
    case_METHOD_NOARGS(isComplete, PRIM, P, isComplete)
    case_METHOD_NOARGS(isClosed, PRIM, P, isClosed)
    case_METHOD_NOARGS(isClosedFlag, PRIM, P, isClosedFlag)
    case_METHOD_NOARGS(isClosedNonFlag, PRIM, P, isClosedNonFlag)
    case_METHOD_NOARGS(getTemplateInstantiationPattern, CLASS, P, getTemplateInstantiationPattern)
    case_METHOD_NOARGS(getInstantiatedFromMemberEnum, CLASS, P, getInstantiatedFromMemberEnum)
    case_METHOD_NOARGS(getTemplateSpecializationKind, PRIM, P, getTemplateSpecializationKind)
    case_METHOD_NOARGS(getMemberSpecializationInfo, CLASS, P, getMemberSpecializationInfo)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::RecordDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__RecordDecl;
# define CLASSNAME clang::RecordDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getPreviousDecl, CLASS, P, getPreviousDecl)
    case_METHOD_NOARGS(getMostRecentDecl, CLASS, P, getMostRecentDecl)
    case_METHOD_NOARGS(hasFlexibleArrayMember, PRIM, P, hasFlexibleArrayMember)
    case_METHOD_NOARGS(isAnonymousStructOrUnion, PRIM, P, isAnonymousStructOrUnion)
    case_METHOD_NOARGS(hasObjectMember, PRIM, P, hasObjectMember)
    case_METHOD_NOARGS(hasVolatileMember, PRIM, P, hasVolatileMember)
    case_METHOD_NOARGS(hasLoadedFieldsFromExternalStorage, PRIM, P, hasLoadedFieldsFromExternalStorage)
    case_METHOD_NOARGS(isNonTrivialToPrimitiveDefaultInitialize, PRIM, P, isNonTrivialToPrimitiveDefaultInitialize)
    case_METHOD_NOARGS(isNonTrivialToPrimitiveCopy, PRIM, P, isNonTrivialToPrimitiveCopy)
    case_METHOD_NOARGS(isNonTrivialToPrimitiveDestroy, PRIM, P, isNonTrivialToPrimitiveDestroy)
    case_METHOD_NOARGS(canPassInRegisters, PRIM, P, canPassInRegisters)
    case_METHOD_NOARGS(getArgPassingRestrictions, PRIM, P, getArgPassingRestrictions)
    case_METHOD_NOARGS(isParamDestroyedInCallee, PRIM, P, isParamDestroyedInCallee)
    case_METHOD_NOARGS(isInjectedClassName, PRIM, P, isInjectedClassName)
    case_METHOD_NOARGS(isLambda, PRIM, P, isLambda)
    case_METHOD_NOARGS(isCapturedRecord, PRIM, P, isCapturedRecord)
    case_METHOD_NOARGS(getDefinition, CLASS, P, getDefinition)
    case currefl::fields: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::fields))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, fields)
      TOO_MANY;
    }
    case_METHOD_NOARGS(field_empty, PRIM, P, field_empty)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
    case currefl::mayInsertExtraPadding: {
      CHECK_TRAIT_IS_REFLPROP(mayInsertExtraPadding)
      if (no_more) PRIM_REFLECTION_0(P, mayInsertExtraPadding)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) PRIM_REFLECTION(P, mayInsertExtraPadding, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(findFirstNamedDataMember, CLASS, P, findFirstNamedDataMember)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FileScopeAsmDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FileScopeAsmDecl;
# define CLASSNAME clang::FileScopeAsmDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAsmLoc, CLASS, P, getAsmLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getAsmString, CLASS, P, getAsmString)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::BlockDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__BlockDecl;
# define CLASSNAME clang::BlockDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCaretLocation, CLASS, P, getCaretLocation)
    case_METHOD_NOARGS(isVariadic, PRIM, P, isVariadic)
    case_METHOD_NOARGS(getCompoundBody, CLASS, P, getCompoundBody)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(getSignatureAsWritten, CLASS, P, getSignatureAsWritten)
    case currefl::parameters: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::parameters))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, parameters)
      TOO_MANY;
    }
    case_METHOD_NOARGS(param_empty, PRIM, P, param_empty)
    case_METHOD_NOARGS(param_size, PRIM, P, param_size)
    case_METHOD_NOARGS(getNumParams, PRIM, P, getNumParams)
    case currefl::getParamDecl: {
      CHECK_TRAIT_IS_REFLPROP(getParamDecl)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getParamDecl, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasCaptures, PRIM, P, hasCaptures)
    case_METHOD_NOARGS(getNumCaptures, PRIM, P, getNumCaptures)
    case currefl::captures: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::captures))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, captures)
      TOO_MANY;
    }
    case_METHOD_NOARGS(capturesCXXThis, PRIM, P, capturesCXXThis)
    case_METHOD_NOARGS(blockMissingReturnType, PRIM, P, blockMissingReturnType)
    case_METHOD_NOARGS(isConversionFromLambda, PRIM, P, isConversionFromLambda)
    case_METHOD_NOARGS(doesNotEscape, PRIM, P, doesNotEscape)
    case currefl::capturesVariable: {
      CHECK_TRAIT_IS_REFLPROP(capturesVariable)
      TRY_LOAD_VAL(const class clang::VarDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, capturesVariable, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getBlockManglingNumber, PRIM, P, getBlockManglingNumber)
    case_METHOD_NOARGS(getBlockManglingContextDecl, CLASS, P, getBlockManglingContextDecl)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::BlockDecl::Capture X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__BlockDecl__Capture;
# define CLASSNAME clang::BlockDecl::Capture
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getVariable, CLASS, V, getVariable)
    case_METHOD_NOARGS(isByRef, PRIM, V, isByRef)
    case_METHOD_NOARGS(isNested, PRIM, V, isNested)
    case_METHOD_NOARGS(hasCopyExpr, PRIM, V, hasCopyExpr)
    case_METHOD_NOARGS(getCopyExpr, CLASS, V, getCopyExpr)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CapturedDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CapturedDecl;
# define CLASSNAME clang::CapturedDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(isNothrow, PRIM, P, isNothrow)
    case_METHOD_NOARGS(getNumParams, PRIM, P, getNumParams)
    case currefl::getParam: {
      CHECK_TRAIT_IS_REFLPROP(getParam)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getParam, p0)
      TOO_MANY;
    }
    case currefl::parameters: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::parameters))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, parameters)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getContextParam, CLASS, P, getContextParam)
    case_METHOD_NOARGS(getContextParamPosition, PRIM, P, getContextParamPosition)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ImportDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ImportDecl;
# define CLASSNAME clang::ImportDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getImportedModule, CLASS, P, getImportedModule)
    case currefl::getIdentifierLocs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getIdentifierLocs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getIdentifierLocs)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ExportDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ExportDecl;
# define CLASSNAME clang::ExportDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getExportLoc, CLASS, P, getExportLoc)
    case_METHOD_NOARGS(getRBraceLoc, CLASS, P, getRBraceLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::EmptyDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__EmptyDecl;
# define CLASSNAME clang::EmptyDecl
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclGroup *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DeclGroup;
# define CLASSNAME clang::DeclGroup
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(size, PRIM, P, size)
    case currefl::operator_sub: {
      CHECK_TRAIT_IS_REFLPROP(operator_sub)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, operator[], p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclGroupRef X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DeclGroupRef;
# define CLASSNAME clang::DeclGroupRef
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(V)
      TOO_MANY;
};
    case_METHOD_NOARGS(isNull, PRIM, V, isNull)
    case_METHOD_NOARGS(isSingleDecl, PRIM, V, isSingleDecl)
    case_METHOD_NOARGS(isDeclGroup, PRIM, V, isDeclGroup)
    case_METHOD_NOARGS(getSingleDecl, CLASS, V, getSingleDecl)
    case_METHOD_NOARGS(getDeclGroup, CLASS, V, getDeclGroup)
    case_METHOD_NOARGS(getAsOpaquePtr, PRIM, V, getAsOpaquePtr)
    case currefl::getFromOpaquePtr: {
      CHECK_TRAIT_IS_REFLPROP(getFromOpaquePtr)
      TRY_LOAD_VAL(void *, p0)
      if (no_more) CLASS_REFLECTION(S, getFromOpaquePtr, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::Stmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__Stmt;
# define CLASSNAME clang::Stmt
  switch( (currefl::memnames)N ) {
    case currefl::operator_new: {
      CHECK_TRAIT_IS_REFLPROP(operator_new)
      TRY_LOAD_VAL(unsigned long, p0)
      TRY_LOAD_VAL(const class clang::ASTContext *, p1)
      if (no_more) PRIM_REFLECTION(S, operator new, p0, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(S, operator new, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::operator_new1: {
      CHECK_TRAIT_IS_REFLPROP(operator_new1)
      TRY_LOAD_VAL(unsigned long, p0)
      TRY_LOAD_VAL(void *, p1)
      if (no_more) PRIM_REFLECTION(S, operator new, p0, p1)
      TOO_MANY;
    }
    case currefl::operator_delete: {
      CHECK_TRAIT_IS_REFLPROP(operator_delete)
      TRY_LOAD_VAL(void *, p0)
      TRY_LOAD_VAL(const class clang::ASTContext *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) VOID_REFLECTION(S, operator delete, (void *)p0, (const class clang::ASTContext *)p1, (unsigned int)p2)
      TOO_MANY;
    }
    case currefl::operator_delete1: {
      CHECK_TRAIT_IS_REFLPROP(operator_delete1)
      TRY_LOAD_VAL(void *, p0)
      TRY_LOAD_VAL(unsigned long, p1)
      if (no_more) VOID_REFLECTION(S, operator delete, (void *)p0, (size_t)p1)
      TOO_MANY;
    }
    case currefl::operator_delete2: {
      CHECK_TRAIT_IS_REFLPROP(operator_delete2)
      TRY_LOAD_VAL(void *, p0)
      TRY_LOAD_VAL(void *, p1)
      if (no_more) VOID_REFLECTION(S, operator delete, (void *)p0, (void *)p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getStmtClass, PRIM, P, getStmtClass)
    case_METHOD_NOARGS(getStmtClassName, PRIM, P, getStmtClassName)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::addStmtClass: {
      CHECK_TRAIT_IS_REFLPROP(addStmtClass)
      TRY_LOAD_VAL(const enum clang::Stmt::StmtClass, p0)
      if (no_more) VOID_REFLECTION(S, addStmtClass, (const enum clang::Stmt::StmtClass)p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(EnableStatistics, VOID, S, EnableStatistics)
    case_METHOD_NOARGS(PrintStats, VOID, S, PrintStats)
    case_METHOD_NOARGS(dump, VOID, P, dump)
    case_METHOD_NOARGS(dumpColor, VOID, P, dumpColor)
    case_METHOD_NOARGS(viewAST, VOID, P, viewAST)
    case_METHOD_NOARGS(IgnoreImplicit, CLASS, P, IgnoreImplicit)
    case currefl::IgnoreContainers: {
      CHECK_TRAIT_IS_REFLPROP(IgnoreContainers)
      if (no_more) CLASS_REFLECTION_0(P, IgnoreContainers)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(P, IgnoreContainers, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(stripLabelLikeStatements, CLASS, P, stripLabelLikeStatements)
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DeclStmt;
# define CLASSNAME clang::DeclStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isSingleDecl, PRIM, P, isSingleDecl)
    case_METHOD_NOARGS(getSingleDecl, CLASS, P, getSingleDecl)
    case_METHOD_NOARGS(getDeclGroup, CLASS, P, getDeclGroup)
    case_METHOD_NOARGS(getStartLoc, CLASS, P, getStartLoc)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::decls: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::decls))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, decls)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::NullStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__NullStmt;
# define CLASSNAME clang::NullStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSemiLoc, CLASS, P, getSemiLoc)
    case_METHOD_NOARGS(hasLeadingEmptyMacro, PRIM, P, hasLeadingEmptyMacro)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CompoundStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CompoundStmt;
# define CLASSNAME clang::CompoundStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(body_empty, PRIM, P, body_empty)
    case_METHOD_NOARGS(size, PRIM, P, size)
    case currefl::body: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::body))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, body)
      TOO_MANY;
    }
    case_METHOD_NOARGS(body_front, CLASS, P, body_front)
    case_METHOD_NOARGS(body_back, CLASS, P, body_back)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getLBracLoc, CLASS, P, getLBracLoc)
    case_METHOD_NOARGS(getRBracLoc, CLASS, P, getRBracLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SwitchCase *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__SwitchCase;
# define CLASSNAME clang::SwitchCase
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNextSwitchCase, CLASS, P, getNextSwitchCase)
    case_METHOD_NOARGS(getKeywordLoc, CLASS, P, getKeywordLoc)
    case_METHOD_NOARGS(getColonLoc, CLASS, P, getColonLoc)
    case_METHOD_NOARGS(getSubStmt, CLASS, P, getSubStmt)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CaseStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CaseStmt;
# define CLASSNAME clang::CaseStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCaseLoc, CLASS, P, getCaseLoc)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, P, getEllipsisLoc)
    case_METHOD_NOARGS(getColonLoc, CLASS, P, getColonLoc)
    case_METHOD_NOARGS(getLHS, CLASS, P, getLHS)
    case_METHOD_NOARGS(getRHS, CLASS, P, getRHS)
    case_METHOD_NOARGS(getSubStmt, CLASS, P, getSubStmt)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DefaultStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DefaultStmt;
# define CLASSNAME clang::DefaultStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSubStmt, CLASS, P, getSubStmt)
    case_METHOD_NOARGS(getDefaultLoc, CLASS, P, getDefaultLoc)
    case_METHOD_NOARGS(getColonLoc, CLASS, P, getColonLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::LabelStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__LabelStmt;
# define CLASSNAME clang::LabelStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getIdentLoc, CLASS, P, getIdentLoc)
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(getName, PRIM, P, getName)
    case_METHOD_NOARGS(getSubStmt, CLASS, P, getSubStmt)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::AttributedStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__AttributedStmt;
# define CLASSNAME clang::AttributedStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAttrLoc, CLASS, P, getAttrLoc)
    case currefl::getAttrs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getAttrs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getAttrs)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getSubStmt, CLASS, P, getSubStmt)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::IfStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__IfStmt;
# define CLASSNAME clang::IfStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getConditionVariable, CLASS, P, getConditionVariable)
    case_METHOD_NOARGS(getConditionVariableDeclStmt, CLASS, P, getConditionVariableDeclStmt)
    case_METHOD_NOARGS(getInit, CLASS, P, getInit)
    case_METHOD_NOARGS(getCond, CLASS, P, getCond)
    case_METHOD_NOARGS(getThen, CLASS, P, getThen)
    case_METHOD_NOARGS(getElse, CLASS, P, getElse)
    case_METHOD_NOARGS(getIfLoc, CLASS, P, getIfLoc)
    case_METHOD_NOARGS(getElseLoc, CLASS, P, getElseLoc)
    case_METHOD_NOARGS(isConstexpr, PRIM, P, isConstexpr)
    case_METHOD_NOARGS(isObjCAvailabilityCheck, PRIM, P, isObjCAvailabilityCheck)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SwitchStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__SwitchStmt;
# define CLASSNAME clang::SwitchStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getConditionVariable, CLASS, P, getConditionVariable)
    case_METHOD_NOARGS(getConditionVariableDeclStmt, CLASS, P, getConditionVariableDeclStmt)
    case_METHOD_NOARGS(getInit, CLASS, P, getInit)
    case_METHOD_NOARGS(getCond, CLASS, P, getCond)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(getSwitchCaseList, CLASS, P, getSwitchCaseList)
    case_METHOD_NOARGS(getSwitchLoc, CLASS, P, getSwitchLoc)
    case_METHOD_NOARGS(isAllEnumCasesCovered, PRIM, P, isAllEnumCasesCovered)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::WhileStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__WhileStmt;
# define CLASSNAME clang::WhileStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getConditionVariable, CLASS, P, getConditionVariable)
    case_METHOD_NOARGS(getConditionVariableDeclStmt, CLASS, P, getConditionVariableDeclStmt)
    case_METHOD_NOARGS(getCond, CLASS, P, getCond)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(getWhileLoc, CLASS, P, getWhileLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DoStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DoStmt;
# define CLASSNAME clang::DoStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCond, CLASS, P, getCond)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(getDoLoc, CLASS, P, getDoLoc)
    case_METHOD_NOARGS(getWhileLoc, CLASS, P, getWhileLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ForStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ForStmt;
# define CLASSNAME clang::ForStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getConditionVariable, CLASS, P, getConditionVariable)
    case_METHOD_NOARGS(getConditionVariableDeclStmt, CLASS, P, getConditionVariableDeclStmt)
    case_METHOD_NOARGS(getInit, CLASS, P, getInit)
    case_METHOD_NOARGS(getCond, CLASS, P, getCond)
    case_METHOD_NOARGS(getInc, CLASS, P, getInc)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(getForLoc, CLASS, P, getForLoc)
    case_METHOD_NOARGS(getLParenLoc, CLASS, P, getLParenLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::GotoStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__GotoStmt;
# define CLASSNAME clang::GotoStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLabel, CLASS, P, getLabel)
    case_METHOD_NOARGS(getGotoLoc, CLASS, P, getGotoLoc)
    case_METHOD_NOARGS(getLabelLoc, CLASS, P, getLabelLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::IndirectGotoStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__IndirectGotoStmt;
# define CLASSNAME clang::IndirectGotoStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getGotoLoc, CLASS, P, getGotoLoc)
    case_METHOD_NOARGS(getStarLoc, CLASS, P, getStarLoc)
    case_METHOD_NOARGS(getTarget, CLASS, P, getTarget)
    case_METHOD_NOARGS(getConstantTarget, CLASS, P, getConstantTarget)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ContinueStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ContinueStmt;
# define CLASSNAME clang::ContinueStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getContinueLoc, CLASS, P, getContinueLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::BreakStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__BreakStmt;
# define CLASSNAME clang::BreakStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBreakLoc, CLASS, P, getBreakLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ReturnStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ReturnStmt;
# define CLASSNAME clang::ReturnStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getRetValue, CLASS, P, getRetValue)
    case_METHOD_NOARGS(getReturnLoc, CLASS, P, getReturnLoc)
    case_METHOD_NOARGS(getNRVOCandidate, CLASS, P, getNRVOCandidate)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::AsmStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__AsmStmt;
# define CLASSNAME clang::AsmStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAsmLoc, CLASS, P, getAsmLoc)
    case_METHOD_NOARGS(isSimple, PRIM, P, isSimple)
    case_METHOD_NOARGS(isVolatile, PRIM, P, isVolatile)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getNumOutputs, PRIM, P, getNumOutputs)
    case currefl::getOutputConstraint: {
      CHECK_TRAIT_IS_REFLPROP(getOutputConstraint)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getOutputConstraint, p0)
      TOO_MANY;
    }
    case currefl::isOutputPlusConstraint: {
      CHECK_TRAIT_IS_REFLPROP(isOutputPlusConstraint)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isOutputPlusConstraint, p0)
      TOO_MANY;
    }
    case currefl::getOutputExpr: {
      CHECK_TRAIT_IS_REFLPROP(getOutputExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getOutputExpr, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumPlusOperands, PRIM, P, getNumPlusOperands)
    case_METHOD_NOARGS(getNumInputs, PRIM, P, getNumInputs)
    case currefl::getInputConstraint: {
      CHECK_TRAIT_IS_REFLPROP(getInputConstraint)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getInputConstraint, p0)
      TOO_MANY;
    }
    case currefl::getInputExpr: {
      CHECK_TRAIT_IS_REFLPROP(getInputExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getInputExpr, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumClobbers, PRIM, P, getNumClobbers)
    case currefl::getClobber: {
      CHECK_TRAIT_IS_REFLPROP(getClobber)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getClobber, p0)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::inputs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::inputs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, inputs)
      TOO_MANY;
    }
    case currefl::outputs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::outputs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, outputs)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::GCCAsmStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__GCCAsmStmt;
# define CLASSNAME clang::GCCAsmStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getAsmString, CLASS, P, getAsmString)
    case currefl::getOutputIdentifier: {
      CHECK_TRAIT_IS_REFLPROP(getOutputIdentifier)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getOutputIdentifier, p0)
      TOO_MANY;
    }
    case currefl::getOutputName: {
      CHECK_TRAIT_IS_REFLPROP(getOutputName)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getOutputName, p0)
      TOO_MANY;
    }
    case currefl::getOutputConstraint: {
      CHECK_TRAIT_IS_REFLPROP(getOutputConstraint)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getOutputConstraint, p0)
      TOO_MANY;
    }
    case currefl::getOutputConstraintLiteral: {
      CHECK_TRAIT_IS_REFLPROP(getOutputConstraintLiteral)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getOutputConstraintLiteral, p0)
      TOO_MANY;
    }
    case currefl::getOutputExpr: {
      CHECK_TRAIT_IS_REFLPROP(getOutputExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getOutputExpr, p0)
      TOO_MANY;
    }
    case currefl::getInputIdentifier: {
      CHECK_TRAIT_IS_REFLPROP(getInputIdentifier)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getInputIdentifier, p0)
      TOO_MANY;
    }
    case currefl::getInputName: {
      CHECK_TRAIT_IS_REFLPROP(getInputName)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getInputName, p0)
      TOO_MANY;
    }
    case currefl::getInputConstraint: {
      CHECK_TRAIT_IS_REFLPROP(getInputConstraint)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getInputConstraint, p0)
      TOO_MANY;
    }
    case currefl::getInputConstraintLiteral: {
      CHECK_TRAIT_IS_REFLPROP(getInputConstraintLiteral)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getInputConstraintLiteral, p0)
      TOO_MANY;
    }
    case currefl::getInputExpr: {
      CHECK_TRAIT_IS_REFLPROP(getInputExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getInputExpr, p0)
      TOO_MANY;
    }
    case currefl::getNamedOperand: {
      CHECK_TRAIT_IS_REFLPROP(getNamedOperand)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, getNamedOperand, p0)
      TOO_MANY;
    }
    case currefl::getClobber: {
      CHECK_TRAIT_IS_REFLPROP(getClobber)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getClobber, p0)
      TOO_MANY;
    }
    case currefl::getClobberStringLiteral: {
      CHECK_TRAIT_IS_REFLPROP(getClobberStringLiteral)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getClobberStringLiteral, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::MSAsmStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__MSAsmStmt;
# define CLASSNAME clang::MSAsmStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLBraceLoc, CLASS, P, getLBraceLoc)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(hasBraces, PRIM, P, hasBraces)
    case_METHOD_NOARGS(getAsmString, CLASS, P, getAsmString)
    case currefl::getOutputConstraint: {
      CHECK_TRAIT_IS_REFLPROP(getOutputConstraint)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getOutputConstraint, p0)
      TOO_MANY;
    }
    case currefl::getOutputExpr: {
      CHECK_TRAIT_IS_REFLPROP(getOutputExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getOutputExpr, p0)
      TOO_MANY;
    }
    case currefl::getInputConstraint: {
      CHECK_TRAIT_IS_REFLPROP(getInputConstraint)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getInputConstraint, p0)
      TOO_MANY;
    }
    case currefl::getInputExpr: {
      CHECK_TRAIT_IS_REFLPROP(getInputExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getInputExpr, p0)
      TOO_MANY;
    }
    case currefl::getAllConstraints: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getAllConstraints))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getAllConstraints)
      TOO_MANY;
    }
    case currefl::getClobbers: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getClobbers))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getClobbers)
      TOO_MANY;
    }
    case currefl::getAllExprs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getAllExprs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getAllExprs)
      TOO_MANY;
    }
    case currefl::getClobber: {
      CHECK_TRAIT_IS_REFLPROP(getClobber)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getClobber, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SEHExceptStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__SEHExceptStmt;
# define CLASSNAME clang::SEHExceptStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getExceptLoc, CLASS, P, getExceptLoc)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getFilterExpr, CLASS, P, getFilterExpr)
    case_METHOD_NOARGS(getBlock, CLASS, P, getBlock)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SEHFinallyStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__SEHFinallyStmt;
# define CLASSNAME clang::SEHFinallyStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getFinallyLoc, CLASS, P, getFinallyLoc)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getBlock, CLASS, P, getBlock)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SEHTryStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__SEHTryStmt;
# define CLASSNAME clang::SEHTryStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getTryLoc, CLASS, P, getTryLoc)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getIsCXXTry, PRIM, P, getIsCXXTry)
    case_METHOD_NOARGS(getTryBlock, CLASS, P, getTryBlock)
    case_METHOD_NOARGS(getHandler, CLASS, P, getHandler)
    case_METHOD_NOARGS(getExceptHandler, CLASS, P, getExceptHandler)
    case_METHOD_NOARGS(getFinallyHandler, CLASS, P, getFinallyHandler)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SEHLeaveStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__SEHLeaveStmt;
# define CLASSNAME clang::SEHLeaveStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLeaveLoc, CLASS, P, getLeaveLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CapturedStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CapturedStmt;
# define CLASSNAME clang::CapturedStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCapturedStmt, CLASS, P, getCapturedStmt)
    case_METHOD_NOARGS(getCapturedDecl, CLASS, P, getCapturedDecl)
    case_METHOD_NOARGS(getCapturedRegionKind, PRIM, P, getCapturedRegionKind)
    case_METHOD_NOARGS(getCapturedRecordDecl, CLASS, P, getCapturedRecordDecl)
    case currefl::capturesVariable: {
      CHECK_TRAIT_IS_REFLPROP(capturesVariable)
      TRY_LOAD_VAL(const class clang::VarDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, capturesVariable, p0)
      TOO_MANY;
    }
    case currefl::captures: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::captures))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, captures)
      TOO_MANY;
    }
    case_METHOD_NOARGS(capture_size, PRIM, P, capture_size)
    case currefl::capture_inits: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::capture_inits))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, capture_inits)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CapturedStmt::Capture X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__CapturedStmt__Capture;
# define CLASSNAME clang::CapturedStmt::Capture
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCaptureKind, PRIM, V, getCaptureKind)
    case_METHOD_NOARGS(getLocation, CLASS, V, getLocation)
    case_METHOD_NOARGS(capturesThis, PRIM, V, capturesThis)
    case_METHOD_NOARGS(capturesVariable, PRIM, V, capturesVariable)
    case_METHOD_NOARGS(capturesVariableByCopy, PRIM, V, capturesVariableByCopy)
    case_METHOD_NOARGS(capturesVariableArrayType, PRIM, V, capturesVariableArrayType)
    case_METHOD_NOARGS(getCapturedVar, CLASS, V, getCapturedVar)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateArgument *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TemplateArgument;
# define CLASSNAME clang::TemplateArgument
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getEmptyPack, CLASS, S, getEmptyPack)
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(isNull, PRIM, P, isNull)
    case_METHOD_NOARGS(isDependent, PRIM, P, isDependent)
    case_METHOD_NOARGS(isInstantiationDependent, PRIM, P, isInstantiationDependent)
    case_METHOD_NOARGS(containsUnexpandedParameterPack, PRIM, P, containsUnexpandedParameterPack)
    case_METHOD_NOARGS(isPackExpansion, PRIM, P, isPackExpansion)
    case_METHOD_NOARGS(getAsType, CLASS, P, getAsType)
    case_METHOD_NOARGS(getAsDecl, CLASS, P, getAsDecl)
    case_METHOD_NOARGS(getParamTypeForDecl, CLASS, P, getParamTypeForDecl)
    case_METHOD_NOARGS(getNullPtrType, CLASS, P, getNullPtrType)
    case_METHOD_NOARGS(getAsTemplate, CLASS, P, getAsTemplate)
    case_METHOD_NOARGS(getAsTemplateOrTemplatePattern, CLASS, P, getAsTemplateOrTemplatePattern)
    case_METHOD_NOARGS(getAsIntegral, CLASS, P, getAsIntegral)
    case_METHOD_NOARGS(getIntegralType, CLASS, P, getIntegralType)
    case_METHOD_NOARGS(getNonTypeTemplateArgumentType, CLASS, P, getNonTypeTemplateArgumentType)
    case_METHOD_NOARGS(getAsExpr, CLASS, P, getAsExpr)
    case currefl::pack_elements: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::pack_elements))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, pack_elements)
      TOO_MANY;
    }
    case_METHOD_NOARGS(pack_size, PRIM, P, pack_size)
    case currefl::getPackAsArray: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getPackAsArray))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getPackAsArray)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getPackExpansionPattern, CLASS, P, getPackExpansionPattern)
    case_METHOD_NOARGS(dump, VOID, P, dump)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateArgument X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TemplateArgument;
# define CLASSNAME clang::TemplateArgument
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getEmptyPack, CLASS, S, getEmptyPack)
    case_METHOD_NOARGS(getKind, PRIM, V, getKind)
    case_METHOD_NOARGS(isNull, PRIM, V, isNull)
    case_METHOD_NOARGS(isDependent, PRIM, V, isDependent)
    case_METHOD_NOARGS(isInstantiationDependent, PRIM, V, isInstantiationDependent)
    case_METHOD_NOARGS(containsUnexpandedParameterPack, PRIM, V, containsUnexpandedParameterPack)
    case_METHOD_NOARGS(isPackExpansion, PRIM, V, isPackExpansion)
    case_METHOD_NOARGS(getAsType, CLASS, V, getAsType)
    case_METHOD_NOARGS(getAsDecl, CLASS, V, getAsDecl)
    case_METHOD_NOARGS(getParamTypeForDecl, CLASS, V, getParamTypeForDecl)
    case_METHOD_NOARGS(getNullPtrType, CLASS, V, getNullPtrType)
    case_METHOD_NOARGS(getAsTemplate, CLASS, V, getAsTemplate)
    case_METHOD_NOARGS(getAsTemplateOrTemplatePattern, CLASS, V, getAsTemplateOrTemplatePattern)
    case_METHOD_NOARGS(getAsIntegral, CLASS, V, getAsIntegral)
    case_METHOD_NOARGS(getIntegralType, CLASS, V, getIntegralType)
    case_METHOD_NOARGS(getNonTypeTemplateArgumentType, CLASS, V, getNonTypeTemplateArgumentType)
    case_METHOD_NOARGS(getAsExpr, CLASS, V, getAsExpr)
    case currefl::pack_elements: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::pack_elements))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(V, pack_elements)
      TOO_MANY;
    }
    case_METHOD_NOARGS(pack_size, PRIM, V, pack_size)
    case currefl::getPackAsArray: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getPackAsArray))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(V, getPackAsArray)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getPackExpansionPattern, CLASS, V, getPackExpansionPattern)
    case_METHOD_NOARGS(dump, VOID, V, dump)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateArgumentLocInfo X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TemplateArgumentLocInfo;
# define CLASSNAME clang::TemplateArgumentLocInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAsTypeSourceInfo, CLASS, V, getAsTypeSourceInfo)
    case_METHOD_NOARGS(getAsExpr, CLASS, V, getAsExpr)
    case_METHOD_NOARGS(getTemplateQualifierLoc, CLASS, V, getTemplateQualifierLoc)
    case_METHOD_NOARGS(getTemplateNameLoc, CLASS, V, getTemplateNameLoc)
    case_METHOD_NOARGS(getTemplateEllipsisLoc, CLASS, V, getTemplateEllipsisLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateArgumentLoc *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TemplateArgumentLoc;
# define CLASSNAME clang::TemplateArgumentLoc
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getArgument, CLASS, P, getArgument)
    case_METHOD_NOARGS(getLocInfo, CLASS, P, getLocInfo)
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
    case_METHOD_NOARGS(getSourceExpression, CLASS, P, getSourceExpression)
    case_METHOD_NOARGS(getSourceDeclExpression, CLASS, P, getSourceDeclExpression)
    case_METHOD_NOARGS(getSourceNullPtrExpression, CLASS, P, getSourceNullPtrExpression)
    case_METHOD_NOARGS(getSourceIntegralExpression, CLASS, P, getSourceIntegralExpression)
    case_METHOD_NOARGS(getTemplateQualifierLoc, CLASS, P, getTemplateQualifierLoc)
    case_METHOD_NOARGS(getTemplateNameLoc, CLASS, P, getTemplateNameLoc)
    case_METHOD_NOARGS(getTemplateEllipsisLoc, CLASS, P, getTemplateEllipsisLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateArgumentLoc X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TemplateArgumentLoc;
# define CLASSNAME clang::TemplateArgumentLoc
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocation, CLASS, V, getLocation)
    case_METHOD_NOARGS(getSourceRange, CLASS, V, getSourceRange)
    case_METHOD_NOARGS(getArgument, CLASS, V, getArgument)
    case_METHOD_NOARGS(getLocInfo, CLASS, V, getLocInfo)
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, V, getTypeSourceInfo)
    case_METHOD_NOARGS(getSourceExpression, CLASS, V, getSourceExpression)
    case_METHOD_NOARGS(getSourceDeclExpression, CLASS, V, getSourceDeclExpression)
    case_METHOD_NOARGS(getSourceNullPtrExpression, CLASS, V, getSourceNullPtrExpression)
    case_METHOD_NOARGS(getSourceIntegralExpression, CLASS, V, getSourceIntegralExpression)
    case_METHOD_NOARGS(getTemplateQualifierLoc, CLASS, V, getTemplateQualifierLoc)
    case_METHOD_NOARGS(getTemplateNameLoc, CLASS, V, getTemplateNameLoc)
    case_METHOD_NOARGS(getTemplateEllipsisLoc, CLASS, V, getTemplateEllipsisLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateArgumentListInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TemplateArgumentListInfo;
# define CLASSNAME clang::TemplateArgumentListInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLAngleLoc, CLASS, P, getLAngleLoc)
    case_METHOD_NOARGS(getRAngleLoc, CLASS, P, getRAngleLoc)
    case_METHOD_NOARGS(size, PRIM, P, size)
    case_METHOD_NOARGS(getArgumentArray, CLASS, P, getArgumentArray)
    case currefl::arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, arguments)
      TOO_MANY;
    }
    case currefl::operator_sub: {
      CHECK_TRAIT_IS_REFLPROP(operator_sub)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, operator[], p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ASTTemplateArgumentListInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ASTTemplateArgumentListInfo;
# define CLASSNAME clang::ASTTemplateArgumentListInfo
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, P, LAngleLoc)
    case_FIELD(CLASS, P, RAngleLoc)
    case_FIELD(PRIM, P, NumTemplateArgs)
    case_METHOD_NOARGS(getTemplateArgs, CLASS, P, getTemplateArgs)
    case currefl::arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, arguments)
      TOO_MANY;
    }
    case currefl::operator_sub: {
      CHECK_TRAIT_IS_REFLPROP(operator_sub)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, operator[], p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypeLoc X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TypeLoc;
# define CLASSNAME clang::TypeLoc
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTypeLocClass, PRIM, V, getTypeLocClass)
    case_METHOD_NOARGS(isNull, PRIM, V, isNull)
    case_METHOD_NOARGS(operator_bool, PRIM, V, operator bool)
    case currefl::getFullDataSizeForType: {
      CHECK_TRAIT_IS_REFLPROP(getFullDataSizeForType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(S, getFullDataSizeForType, p0)
      TOO_MANY;
    }
    case currefl::getLocalAlignmentForType: {
      CHECK_TRAIT_IS_REFLPROP(getLocalAlignmentForType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(S, getLocalAlignmentForType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getType, CLASS, V, getType)
    case_METHOD_NOARGS(getTypePtr, CLASS, V, getTypePtr)
    case_METHOD_NOARGS(getOpaqueData, PRIM, V, getOpaqueData)
    case_METHOD_NOARGS(getBeginLoc, CLASS, V, getBeginLoc)
    case_METHOD_NOARGS(getEndLoc, CLASS, V, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, V, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, V, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, V, getLocEnd)
    case_METHOD_NOARGS(getLocalSourceRange, CLASS, V, getLocalSourceRange)
    case_METHOD_NOARGS(getFullDataSize, PRIM, V, getFullDataSize)
    case_METHOD_NOARGS(getNextTypeLoc, CLASS, V, getNextTypeLoc)
    case_METHOD_NOARGS(getUnqualifiedLoc, CLASS, V, getUnqualifiedLoc)
    case_METHOD_NOARGS(IgnoreParens, CLASS, V, IgnoreParens)
    case_METHOD_NOARGS(findExplicitQualifierLoc, CLASS, V, findExplicitQualifierLoc)
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(const class clang::TypeLoc &, p0 )
      TRY_LOAD_VAL(const class clang::TypeLoc &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(const class clang::TypeLoc &, p0 )
      TRY_LOAD_VAL(const class clang::TypeLoc &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(findNullabilityLoc, CLASS, V, findNullabilityLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UnqualTypeLoc X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__UnqualTypeLoc;
# define CLASSNAME clang::UnqualTypeLoc
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTypePtr, CLASS, V, getTypePtr)
    case_METHOD_NOARGS(getTypeLocClass, PRIM, V, getTypeLocClass)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CommentOptions X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__CommentOptions;
# define CLASSNAME clang::CommentOptions
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, V, ParseAllComments)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::comments::CommandInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__comments__CommandInfo;
# define CLASSNAME clang::comments::CommandInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getID, PRIM, P, getID)
    case_FIELD(PRIM, P, Name)
    case_FIELD(PRIM, P, EndCommandName)
    case_FIELD(PRIM, P, ID)
    case_FIELD(PRIM, P, NumArgs)
    case_FIELD(PRIM, P, IsInlineCommand)
    case_FIELD(PRIM, P, IsBlockCommand)
    case_FIELD(PRIM, P, IsBriefCommand)
    case_FIELD(PRIM, P, IsReturnsCommand)
    case_FIELD(PRIM, P, IsParamCommand)
    case_FIELD(PRIM, P, IsTParamCommand)
    case_FIELD(PRIM, P, IsThrowsCommand)
    case_FIELD(PRIM, P, IsDeprecatedCommand)
    case_FIELD(PRIM, P, IsHeaderfileCommand)
    case_FIELD(PRIM, P, IsEmptyParagraphAllowed)
    case_FIELD(PRIM, P, IsVerbatimBlockCommand)
    case_FIELD(PRIM, P, IsVerbatimBlockEndCommand)
    case_FIELD(PRIM, P, IsVerbatimLineCommand)
    case_FIELD(PRIM, P, IsDeclarationCommand)
    case_FIELD(PRIM, P, IsFunctionDeclarationCommand)
    case_FIELD(PRIM, P, IsRecordLikeDetailCommand)
    case_FIELD(PRIM, P, IsRecordLikeDeclarationCommand)
    case_FIELD(PRIM, P, IsUnknownCommand)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::comments::CommandTraits *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__comments__CommandTraits;
# define CLASSNAME clang::comments::CommandTraits
  switch( (currefl::memnames)N ) {
    case currefl::getCommandInfoOrNULL: {
      CHECK_TRAIT_IS_REFLPROP(getCommandInfoOrNULL)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(P, getCommandInfoOrNULL, p0)
      TOO_MANY;
    }
    case currefl::getCommandInfo: {
      CHECK_TRAIT_IS_REFLPROP(getCommandInfo)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(P, getCommandInfo, p0)
      TOO_MANY;
    }
    case currefl::getTypoCorrectCommandInfo: {
      CHECK_TRAIT_IS_REFLPROP(getTypoCorrectCommandInfo)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(P, getTypoCorrectCommandInfo, p0)
      TOO_MANY;
    }
    case currefl::getCommandInfo1: {
      CHECK_TRAIT_IS_REFLPROP(getCommandInfo1)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getCommandInfo, p0)
      TOO_MANY;
    }
    case currefl::getBuiltinCommandInfo: {
      CHECK_TRAIT_IS_REFLPROP(getBuiltinCommandInfo)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(S, getBuiltinCommandInfo, p0)
      TOO_MANY;
    }
    case currefl::getBuiltinCommandInfo1: {
      CHECK_TRAIT_IS_REFLPROP(getBuiltinCommandInfo1)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(S, getBuiltinCommandInfo, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ComparisonCategoryInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ComparisonCategoryInfo;
# define CLASSNAME clang::ComparisonCategoryInfo
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, P, Record)
    case_FIELD(PRIM, P, Kind)
    case_METHOD_NOARGS(getType, CLASS, P, getType)
    case currefl::getValueInfo: {
      CHECK_TRAIT_IS_REFLPROP(getValueInfo)
      TRY_LOAD_VAL(enum clang::ComparisonCategoryResult, p0)
      if (no_more) CLASS_REFLECTION(P, getValueInfo, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isEquality, PRIM, P, isEquality)
    case_METHOD_NOARGS(isOrdered, PRIM, P, isOrdered)
    case_METHOD_NOARGS(isStrong, PRIM, P, isStrong)
    case_METHOD_NOARGS(isPartial, PRIM, P, isPartial)
    case currefl::makeWeakResult: {
      CHECK_TRAIT_IS_REFLPROP(makeWeakResult)
      TRY_LOAD_VAL(enum clang::ComparisonCategoryResult, p0)
      if (no_more) PRIM_REFLECTION(P, makeWeakResult, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getEqualOrEquiv, CLASS, P, getEqualOrEquiv)
    case_METHOD_NOARGS(getNonequalOrNonequiv, CLASS, P, getNonequalOrNonequiv)
    case_METHOD_NOARGS(getLess, CLASS, P, getLess)
    case_METHOD_NOARGS(getGreater, CLASS, P, getGreater)
    case_METHOD_NOARGS(getUnordered, CLASS, P, getUnordered)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ComparisonCategoryInfo::ValueInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ComparisonCategoryInfo__ValueInfo;
# define CLASSNAME clang::ComparisonCategoryInfo::ValueInfo
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, P, Kind)
    case_FIELD(CLASS, P, VD)
    case_METHOD_NOARGS(hasValidIntValue, PRIM, P, hasValidIntValue)
    case_METHOD_NOARGS(getIntValue, CLASS, P, getIntValue)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ComparisonCategories X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ComparisonCategories;
# define CLASSNAME clang::ComparisonCategories
  switch( (currefl::memnames)N ) {
    case currefl::getCategoryString: {
      CHECK_TRAIT_IS_REFLPROP(getCategoryString)
      TRY_LOAD_VAL(enum clang::ComparisonCategoryType, p0)
      if (no_more) CLASS_REFLECTION(S, getCategoryString, p0)
      TOO_MANY;
    }
    case currefl::getResultString: {
      CHECK_TRAIT_IS_REFLPROP(getResultString)
      TRY_LOAD_VAL(enum clang::ComparisonCategoryResult, p0)
      if (no_more) CLASS_REFLECTION(S, getResultString, p0)
      TOO_MANY;
    }
    case currefl::getInfo: {
      CHECK_TRAIT_IS_REFLPROP(getInfo)
      TRY_LOAD_VAL(enum clang::ComparisonCategoryType, p0)
      if (no_more) CLASS_REFLECTION(V, getInfo, p0)
      TOO_MANY;
    }
    case currefl::getInfoForType: {
      CHECK_TRAIT_IS_REFLPROP(getInfoForType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(V, getInfoForType, p0)
      TOO_MANY;
    }
    case currefl::lookupInfo: {
      CHECK_TRAIT_IS_REFLPROP(lookupInfo)
      TRY_LOAD_VAL(enum clang::ComparisonCategoryType, p0)
      if (no_more) CLASS_REFLECTION(V, lookupInfo, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::Triple *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__Triple;
# define CLASSNAME llvm::Triple
  switch( (currefl::memnames)N ) {
    case currefl::normalize: {
      CHECK_TRAIT_IS_REFLPROP(normalize)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(S, normalize, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(normalize1, PRIM, P, normalize)
    case_METHOD_NOARGS(getArch, PRIM, P, getArch)
    case_METHOD_NOARGS(getSubArch, PRIM, P, getSubArch)
    case_METHOD_NOARGS(getVendor, PRIM, P, getVendor)
    case_METHOD_NOARGS(getOS, PRIM, P, getOS)
    case_METHOD_NOARGS(hasEnvironment, PRIM, P, hasEnvironment)
    case_METHOD_NOARGS(getEnvironment, PRIM, P, getEnvironment)
    case_METHOD_NOARGS(getObjectFormat, PRIM, P, getObjectFormat)
    case_METHOD_NOARGS(getOSMajorVersion, PRIM, P, getOSMajorVersion)
    case_METHOD_NOARGS(getArchName, CLASS, P, getArchName)
    case_METHOD_NOARGS(getVendorName, CLASS, P, getVendorName)
    case_METHOD_NOARGS(getOSName, CLASS, P, getOSName)
    case_METHOD_NOARGS(getEnvironmentName, CLASS, P, getEnvironmentName)
    case_METHOD_NOARGS(getOSAndEnvironmentName, CLASS, P, getOSAndEnvironmentName)
    case_METHOD_NOARGS(isArch64Bit, PRIM, P, isArch64Bit)
    case_METHOD_NOARGS(isArch32Bit, PRIM, P, isArch32Bit)
    case_METHOD_NOARGS(isArch16Bit, PRIM, P, isArch16Bit)
    case currefl::isOSVersionLT: {
      CHECK_TRAIT_IS_REFLPROP(isOSVersionLT)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isOSVersionLT, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(P, isOSVersionLT, p0, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(P, isOSVersionLT, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::isMacOSXVersionLT: {
      CHECK_TRAIT_IS_REFLPROP(isMacOSXVersionLT)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isMacOSXVersionLT, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(P, isMacOSXVersionLT, p0, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(P, isMacOSXVersionLT, p0, p1, p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isMacOSX, PRIM, P, isMacOSX)
    case_METHOD_NOARGS(isiOS, PRIM, P, isiOS)
    case_METHOD_NOARGS(isTvOS, PRIM, P, isTvOS)
    case_METHOD_NOARGS(isWatchOS, PRIM, P, isWatchOS)
    case_METHOD_NOARGS(isWatchABI, PRIM, P, isWatchABI)
    case_METHOD_NOARGS(isOSDarwin, PRIM, P, isOSDarwin)
    case_METHOD_NOARGS(isSimulatorEnvironment, PRIM, P, isSimulatorEnvironment)
    case_METHOD_NOARGS(isOSNetBSD, PRIM, P, isOSNetBSD)
    case_METHOD_NOARGS(isOSOpenBSD, PRIM, P, isOSOpenBSD)
    case_METHOD_NOARGS(isOSFreeBSD, PRIM, P, isOSFreeBSD)
    case_METHOD_NOARGS(isOSFuchsia, PRIM, P, isOSFuchsia)
    case_METHOD_NOARGS(isOSDragonFly, PRIM, P, isOSDragonFly)
    case_METHOD_NOARGS(isOSSolaris, PRIM, P, isOSSolaris)
    case_METHOD_NOARGS(isOSIAMCU, PRIM, P, isOSIAMCU)
    case_METHOD_NOARGS(isOSUnknown, PRIM, P, isOSUnknown)
    case_METHOD_NOARGS(isGNUEnvironment, PRIM, P, isGNUEnvironment)
    case_METHOD_NOARGS(isOSContiki, PRIM, P, isOSContiki)
    case_METHOD_NOARGS(isOSHaiku, PRIM, P, isOSHaiku)
    case_METHOD_NOARGS(isWindowsMSVCEnvironment, PRIM, P, isWindowsMSVCEnvironment)
    case_METHOD_NOARGS(isKnownWindowsMSVCEnvironment, PRIM, P, isKnownWindowsMSVCEnvironment)
    case_METHOD_NOARGS(isWindowsCoreCLREnvironment, PRIM, P, isWindowsCoreCLREnvironment)
    case_METHOD_NOARGS(isWindowsItaniumEnvironment, PRIM, P, isWindowsItaniumEnvironment)
    case_METHOD_NOARGS(isWindowsCygwinEnvironment, PRIM, P, isWindowsCygwinEnvironment)
    case_METHOD_NOARGS(isWindowsGNUEnvironment, PRIM, P, isWindowsGNUEnvironment)
    case_METHOD_NOARGS(isOSCygMing, PRIM, P, isOSCygMing)
    case_METHOD_NOARGS(isOSMSVCRT, PRIM, P, isOSMSVCRT)
    case_METHOD_NOARGS(isOSWindows, PRIM, P, isOSWindows)
    case_METHOD_NOARGS(isOSNaCl, PRIM, P, isOSNaCl)
    case_METHOD_NOARGS(isOSLinux, PRIM, P, isOSLinux)
    case_METHOD_NOARGS(isOSKFreeBSD, PRIM, P, isOSKFreeBSD)
    case_METHOD_NOARGS(isOSGlibc, PRIM, P, isOSGlibc)
    case_METHOD_NOARGS(isOSBinFormatELF, PRIM, P, isOSBinFormatELF)
    case_METHOD_NOARGS(isOSBinFormatCOFF, PRIM, P, isOSBinFormatCOFF)
    case_METHOD_NOARGS(isOSBinFormatMachO, PRIM, P, isOSBinFormatMachO)
    case_METHOD_NOARGS(isOSBinFormatWasm, PRIM, P, isOSBinFormatWasm)
    case_METHOD_NOARGS(isPS4CPU, PRIM, P, isPS4CPU)
    case_METHOD_NOARGS(isPS4, PRIM, P, isPS4)
    case_METHOD_NOARGS(isAndroid, PRIM, P, isAndroid)
    case currefl::isAndroidVersionLT: {
      CHECK_TRAIT_IS_REFLPROP(isAndroidVersionLT)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isAndroidVersionLT, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isMusl, PRIM, P, isMusl)
    case_METHOD_NOARGS(isNVPTX, PRIM, P, isNVPTX)
    case_METHOD_NOARGS(isThumb, PRIM, P, isThumb)
    case_METHOD_NOARGS(isARM, PRIM, P, isARM)
    case_METHOD_NOARGS(isAArch64, PRIM, P, isAArch64)
    case_METHOD_NOARGS(isMIPS32, PRIM, P, isMIPS32)
    case_METHOD_NOARGS(isMIPS64, PRIM, P, isMIPS64)
    case_METHOD_NOARGS(isMIPS, PRIM, P, isMIPS)
    case_METHOD_NOARGS(supportsCOMDAT, PRIM, P, supportsCOMDAT)
    case_METHOD_NOARGS(hasDefaultEmulatedTLS, PRIM, P, hasDefaultEmulatedTLS)
    case_METHOD_NOARGS(get32BitArchVariant, CLASS, P, get32BitArchVariant)
    case_METHOD_NOARGS(get64BitArchVariant, CLASS, P, get64BitArchVariant)
    case_METHOD_NOARGS(getBigEndianArchVariant, CLASS, P, getBigEndianArchVariant)
    case_METHOD_NOARGS(getLittleEndianArchVariant, CLASS, P, getLittleEndianArchVariant)
    case currefl::getARMCPUForArch: {
      CHECK_TRAIT_IS_REFLPROP(getARMCPUForArch)
      if (no_more) CLASS_REFLECTION_0(P, getARMCPUForArch)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(P, getARMCPUForArch, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isLittleEndian, PRIM, P, isLittleEndian)
    case currefl::getArchTypeName: {
      CHECK_TRAIT_IS_REFLPROP(getArchTypeName)
      TRY_LOAD_VAL(enum llvm::Triple::ArchType, p0)
      if (no_more) CLASS_REFLECTION(S, getArchTypeName, p0)
      TOO_MANY;
    }
    case currefl::getArchTypePrefix: {
      CHECK_TRAIT_IS_REFLPROP(getArchTypePrefix)
      TRY_LOAD_VAL(enum llvm::Triple::ArchType, p0)
      if (no_more) CLASS_REFLECTION(S, getArchTypePrefix, p0)
      TOO_MANY;
    }
    case currefl::getVendorTypeName: {
      CHECK_TRAIT_IS_REFLPROP(getVendorTypeName)
      TRY_LOAD_VAL(enum llvm::Triple::VendorType, p0)
      if (no_more) CLASS_REFLECTION(S, getVendorTypeName, p0)
      TOO_MANY;
    }
    case currefl::getOSTypeName: {
      CHECK_TRAIT_IS_REFLPROP(getOSTypeName)
      TRY_LOAD_VAL(enum llvm::Triple::OSType, p0)
      if (no_more) CLASS_REFLECTION(S, getOSTypeName, p0)
      TOO_MANY;
    }
    case currefl::getEnvironmentTypeName: {
      CHECK_TRAIT_IS_REFLPROP(getEnvironmentTypeName)
      TRY_LOAD_VAL(enum llvm::Triple::EnvironmentType, p0)
      if (no_more) CLASS_REFLECTION(S, getEnvironmentTypeName, p0)
      TOO_MANY;
    }
    case currefl::getArchTypeForLLVMName: {
      CHECK_TRAIT_IS_REFLPROP(getArchTypeForLLVMName)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(S, getArchTypeForLLVMName, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::Triple X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__Triple;
# define CLASSNAME llvm::Triple
  switch( (currefl::memnames)N ) {
    case currefl::normalize: {
      CHECK_TRAIT_IS_REFLPROP(normalize)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(S, normalize, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(normalize1, PRIM, V, normalize)
    case_METHOD_NOARGS(getArch, PRIM, V, getArch)
    case_METHOD_NOARGS(getSubArch, PRIM, V, getSubArch)
    case_METHOD_NOARGS(getVendor, PRIM, V, getVendor)
    case_METHOD_NOARGS(getOS, PRIM, V, getOS)
    case_METHOD_NOARGS(hasEnvironment, PRIM, V, hasEnvironment)
    case_METHOD_NOARGS(getEnvironment, PRIM, V, getEnvironment)
    case_METHOD_NOARGS(getObjectFormat, PRIM, V, getObjectFormat)
    case_METHOD_NOARGS(getOSMajorVersion, PRIM, V, getOSMajorVersion)
    case_METHOD_NOARGS(getArchName, CLASS, V, getArchName)
    case_METHOD_NOARGS(getVendorName, CLASS, V, getVendorName)
    case_METHOD_NOARGS(getOSName, CLASS, V, getOSName)
    case_METHOD_NOARGS(getEnvironmentName, CLASS, V, getEnvironmentName)
    case_METHOD_NOARGS(getOSAndEnvironmentName, CLASS, V, getOSAndEnvironmentName)
    case_METHOD_NOARGS(isArch64Bit, PRIM, V, isArch64Bit)
    case_METHOD_NOARGS(isArch32Bit, PRIM, V, isArch32Bit)
    case_METHOD_NOARGS(isArch16Bit, PRIM, V, isArch16Bit)
    case currefl::isOSVersionLT: {
      CHECK_TRAIT_IS_REFLPROP(isOSVersionLT)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(V, isOSVersionLT, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(V, isOSVersionLT, p0, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(V, isOSVersionLT, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::isMacOSXVersionLT: {
      CHECK_TRAIT_IS_REFLPROP(isMacOSXVersionLT)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(V, isMacOSXVersionLT, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(V, isMacOSXVersionLT, p0, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(V, isMacOSXVersionLT, p0, p1, p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isMacOSX, PRIM, V, isMacOSX)
    case_METHOD_NOARGS(isiOS, PRIM, V, isiOS)
    case_METHOD_NOARGS(isTvOS, PRIM, V, isTvOS)
    case_METHOD_NOARGS(isWatchOS, PRIM, V, isWatchOS)
    case_METHOD_NOARGS(isWatchABI, PRIM, V, isWatchABI)
    case_METHOD_NOARGS(isOSDarwin, PRIM, V, isOSDarwin)
    case_METHOD_NOARGS(isSimulatorEnvironment, PRIM, V, isSimulatorEnvironment)
    case_METHOD_NOARGS(isOSNetBSD, PRIM, V, isOSNetBSD)
    case_METHOD_NOARGS(isOSOpenBSD, PRIM, V, isOSOpenBSD)
    case_METHOD_NOARGS(isOSFreeBSD, PRIM, V, isOSFreeBSD)
    case_METHOD_NOARGS(isOSFuchsia, PRIM, V, isOSFuchsia)
    case_METHOD_NOARGS(isOSDragonFly, PRIM, V, isOSDragonFly)
    case_METHOD_NOARGS(isOSSolaris, PRIM, V, isOSSolaris)
    case_METHOD_NOARGS(isOSIAMCU, PRIM, V, isOSIAMCU)
    case_METHOD_NOARGS(isOSUnknown, PRIM, V, isOSUnknown)
    case_METHOD_NOARGS(isGNUEnvironment, PRIM, V, isGNUEnvironment)
    case_METHOD_NOARGS(isOSContiki, PRIM, V, isOSContiki)
    case_METHOD_NOARGS(isOSHaiku, PRIM, V, isOSHaiku)
    case_METHOD_NOARGS(isWindowsMSVCEnvironment, PRIM, V, isWindowsMSVCEnvironment)
    case_METHOD_NOARGS(isKnownWindowsMSVCEnvironment, PRIM, V, isKnownWindowsMSVCEnvironment)
    case_METHOD_NOARGS(isWindowsCoreCLREnvironment, PRIM, V, isWindowsCoreCLREnvironment)
    case_METHOD_NOARGS(isWindowsItaniumEnvironment, PRIM, V, isWindowsItaniumEnvironment)
    case_METHOD_NOARGS(isWindowsCygwinEnvironment, PRIM, V, isWindowsCygwinEnvironment)
    case_METHOD_NOARGS(isWindowsGNUEnvironment, PRIM, V, isWindowsGNUEnvironment)
    case_METHOD_NOARGS(isOSCygMing, PRIM, V, isOSCygMing)
    case_METHOD_NOARGS(isOSMSVCRT, PRIM, V, isOSMSVCRT)
    case_METHOD_NOARGS(isOSWindows, PRIM, V, isOSWindows)
    case_METHOD_NOARGS(isOSNaCl, PRIM, V, isOSNaCl)
    case_METHOD_NOARGS(isOSLinux, PRIM, V, isOSLinux)
    case_METHOD_NOARGS(isOSKFreeBSD, PRIM, V, isOSKFreeBSD)
    case_METHOD_NOARGS(isOSGlibc, PRIM, V, isOSGlibc)
    case_METHOD_NOARGS(isOSBinFormatELF, PRIM, V, isOSBinFormatELF)
    case_METHOD_NOARGS(isOSBinFormatCOFF, PRIM, V, isOSBinFormatCOFF)
    case_METHOD_NOARGS(isOSBinFormatMachO, PRIM, V, isOSBinFormatMachO)
    case_METHOD_NOARGS(isOSBinFormatWasm, PRIM, V, isOSBinFormatWasm)
    case_METHOD_NOARGS(isPS4CPU, PRIM, V, isPS4CPU)
    case_METHOD_NOARGS(isPS4, PRIM, V, isPS4)
    case_METHOD_NOARGS(isAndroid, PRIM, V, isAndroid)
    case currefl::isAndroidVersionLT: {
      CHECK_TRAIT_IS_REFLPROP(isAndroidVersionLT)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(V, isAndroidVersionLT, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isMusl, PRIM, V, isMusl)
    case_METHOD_NOARGS(isNVPTX, PRIM, V, isNVPTX)
    case_METHOD_NOARGS(isThumb, PRIM, V, isThumb)
    case_METHOD_NOARGS(isARM, PRIM, V, isARM)
    case_METHOD_NOARGS(isAArch64, PRIM, V, isAArch64)
    case_METHOD_NOARGS(isMIPS32, PRIM, V, isMIPS32)
    case_METHOD_NOARGS(isMIPS64, PRIM, V, isMIPS64)
    case_METHOD_NOARGS(isMIPS, PRIM, V, isMIPS)
    case_METHOD_NOARGS(supportsCOMDAT, PRIM, V, supportsCOMDAT)
    case_METHOD_NOARGS(hasDefaultEmulatedTLS, PRIM, V, hasDefaultEmulatedTLS)
    case_METHOD_NOARGS(get32BitArchVariant, CLASS, V, get32BitArchVariant)
    case_METHOD_NOARGS(get64BitArchVariant, CLASS, V, get64BitArchVariant)
    case_METHOD_NOARGS(getBigEndianArchVariant, CLASS, V, getBigEndianArchVariant)
    case_METHOD_NOARGS(getLittleEndianArchVariant, CLASS, V, getLittleEndianArchVariant)
    case currefl::getARMCPUForArch: {
      CHECK_TRAIT_IS_REFLPROP(getARMCPUForArch)
      if (no_more) CLASS_REFLECTION_0(V, getARMCPUForArch)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(V, getARMCPUForArch, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isLittleEndian, PRIM, V, isLittleEndian)
    case currefl::getArchTypeName: {
      CHECK_TRAIT_IS_REFLPROP(getArchTypeName)
      TRY_LOAD_VAL(enum llvm::Triple::ArchType, p0)
      if (no_more) CLASS_REFLECTION(S, getArchTypeName, p0)
      TOO_MANY;
    }
    case currefl::getArchTypePrefix: {
      CHECK_TRAIT_IS_REFLPROP(getArchTypePrefix)
      TRY_LOAD_VAL(enum llvm::Triple::ArchType, p0)
      if (no_more) CLASS_REFLECTION(S, getArchTypePrefix, p0)
      TOO_MANY;
    }
    case currefl::getVendorTypeName: {
      CHECK_TRAIT_IS_REFLPROP(getVendorTypeName)
      TRY_LOAD_VAL(enum llvm::Triple::VendorType, p0)
      if (no_more) CLASS_REFLECTION(S, getVendorTypeName, p0)
      TOO_MANY;
    }
    case currefl::getOSTypeName: {
      CHECK_TRAIT_IS_REFLPROP(getOSTypeName)
      TRY_LOAD_VAL(enum llvm::Triple::OSType, p0)
      if (no_more) CLASS_REFLECTION(S, getOSTypeName, p0)
      TOO_MANY;
    }
    case currefl::getEnvironmentTypeName: {
      CHECK_TRAIT_IS_REFLPROP(getEnvironmentTypeName)
      TRY_LOAD_VAL(enum llvm::Triple::EnvironmentType, p0)
      if (no_more) CLASS_REFLECTION(S, getEnvironmentTypeName, p0)
      TOO_MANY;
    }
    case currefl::getArchTypeForLLVMName: {
      CHECK_TRAIT_IS_REFLPROP(getArchTypeForLLVMName)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(S, getArchTypeForLLVMName, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCRuntime X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ObjCRuntime;
# define CLASSNAME clang::ObjCRuntime
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKind, PRIM, V, getKind)
    case_METHOD_NOARGS(getVersion, CLASS, V, getVersion)
    case_METHOD_NOARGS(isNonFragile, PRIM, V, isNonFragile)
    case_METHOD_NOARGS(isFragile, PRIM, V, isFragile)
    case_METHOD_NOARGS(isGNUFamily, PRIM, V, isGNUFamily)
    case_METHOD_NOARGS(isNeXTFamily, PRIM, V, isNeXTFamily)
    case_METHOD_NOARGS(allowsARC, PRIM, V, allowsARC)
    case_METHOD_NOARGS(hasNativeARC, PRIM, V, hasNativeARC)
    case_METHOD_NOARGS(hasOptimizedSetter, PRIM, V, hasOptimizedSetter)
    case_METHOD_NOARGS(allowsWeak, PRIM, V, allowsWeak)
    case_METHOD_NOARGS(hasNativeWeak, PRIM, V, hasNativeWeak)
    case_METHOD_NOARGS(hasSubscripting, PRIM, V, hasSubscripting)
    case_METHOD_NOARGS(allowsSizeofAlignof, PRIM, V, allowsSizeofAlignof)
    case_METHOD_NOARGS(allowsPointerArithmetic, PRIM, V, allowsPointerArithmetic)
    case_METHOD_NOARGS(isSubscriptPointerArithmetic, PRIM, V, isSubscriptPointerArithmetic)
    case_METHOD_NOARGS(hasTerminate, PRIM, V, hasTerminate)
    case_METHOD_NOARGS(hasWeakClassImport, PRIM, V, hasWeakClassImport)
    case_METHOD_NOARGS(hasUnwindExceptions, PRIM, V, hasUnwindExceptions)
    case_METHOD_NOARGS(hasAtomicCopyHelper, PRIM, V, hasAtomicCopyHelper)
    case_METHOD_NOARGS(hasARCUnsafeClaimAutoreleasedReturnValue, PRIM, V, hasARCUnsafeClaimAutoreleasedReturnValue)
    case_METHOD_NOARGS(hasEmptyCollections, PRIM, V, hasEmptyCollections)
    case_METHOD_NOARGS(getAsString, PRIM, V, getAsString)
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(const class clang::ObjCRuntime &, p0 )
      TRY_LOAD_VAL(const class clang::ObjCRuntime &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(const class clang::ObjCRuntime &, p0 )
      TRY_LOAD_VAL(const class clang::ObjCRuntime &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SanitizerSet X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SanitizerSet;
# define CLASSNAME clang::SanitizerSet
  switch( (currefl::memnames)N ) {
    case currefl::has: {
      CHECK_TRAIT_IS_REFLPROP(has)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(V, has, p0)
      TOO_MANY;
    }
    case currefl::hasOneOf: {
      CHECK_TRAIT_IS_REFLPROP(hasOneOf)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(V, hasOneOf, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(empty, PRIM, V, empty)
    case_FIELD(PRIM, V, Mask)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::LangOptionsBase *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__LangOptionsBase;
# define CLASSNAME clang::LangOptionsBase
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, P, C99)
    case_FIELD(PRIM, P, C11)
    case_FIELD(PRIM, P, C17)
    case_FIELD(PRIM, P, MSVCCompat)
    case_FIELD(PRIM, P, MicrosoftExt)
    case_FIELD(PRIM, P, AsmBlocks)
    case_FIELD(PRIM, P, Borland)
    case_FIELD(PRIM, P, CPlusPlus)
    case_FIELD(PRIM, P, CPlusPlus11)
    case_FIELD(PRIM, P, CPlusPlus14)
    case_FIELD(PRIM, P, CPlusPlus17)
    case_FIELD(PRIM, P, CPlusPlus2a)
    case_FIELD(PRIM, P, ObjC1)
    case_FIELD(PRIM, P, ObjC2)
    case_FIELD(PRIM, P, ObjCDefaultSynthProperties)
    case_FIELD(PRIM, P, EncodeExtendedBlockSig)
    case_FIELD(PRIM, P, ObjCInferRelatedResultType)
    case_FIELD(PRIM, P, AppExt)
    case_FIELD(PRIM, P, Trigraphs)
    case_FIELD(PRIM, P, LineComment)
    case_FIELD(PRIM, P, Bool)
    case_FIELD(PRIM, P, Half)
    case_FIELD(PRIM, P, WChar)
    case_FIELD(PRIM, P, Char8)
    case_FIELD(PRIM, P, DeclSpecKeyword)
    case_FIELD(PRIM, P, DollarIdents)
    case_FIELD(PRIM, P, AsmPreprocessor)
    case_FIELD(PRIM, P, GNUMode)
    case_FIELD(PRIM, P, GNUKeywords)
    case_FIELD(PRIM, P, ImplicitInt)
    case_FIELD(PRIM, P, Digraphs)
    case_FIELD(PRIM, P, HexFloats)
    case_FIELD(PRIM, P, CXXOperatorNames)
    case_FIELD(PRIM, P, AppleKext)
    case_FIELD(PRIM, P, PascalStrings)
    case_FIELD(PRIM, P, WritableStrings)
    case_FIELD(PRIM, P, ConstStrings)
    case_FIELD(PRIM, P, LaxVectorConversions)
    case_FIELD(PRIM, P, AltiVec)
    case_FIELD(PRIM, P, ZVector)
    case_FIELD(PRIM, P, Exceptions)
    case_FIELD(PRIM, P, ObjCExceptions)
    case_FIELD(PRIM, P, CXXExceptions)
    case_FIELD(PRIM, P, DWARFExceptions)
    case_FIELD(PRIM, P, SjLjExceptions)
    case_FIELD(PRIM, P, SEHExceptions)
    case_FIELD(PRIM, P, ExternCNoUnwind)
    case_FIELD(PRIM, P, TraditionalCPP)
    case_FIELD(PRIM, P, RTTI)
    case_FIELD(PRIM, P, RTTIData)
    case_FIELD(PRIM, P, MSBitfields)
    case_FIELD(PRIM, P, Freestanding)
    case_FIELD(PRIM, P, NoBuiltin)
    case_FIELD(PRIM, P, NoMathBuiltin)
    case_FIELD(PRIM, P, GNUAsm)
    case_FIELD(PRIM, P, CoroutinesTS)
    case_FIELD(PRIM, P, RelaxedTemplateTemplateArgs)
    case_FIELD(PRIM, P, DoubleSquareBracketAttributes)
    case_FIELD(PRIM, P, ThreadsafeStatics)
    case_FIELD(PRIM, P, POSIXThreads)
    case_FIELD(PRIM, P, Blocks)
    case_FIELD(PRIM, P, EmitAllDecls)
    case_FIELD(PRIM, P, MathErrno)
    case_FIELD(PRIM, P, HeinousExtensions)
    case_FIELD(PRIM, P, Modules)
    case_FIELD(PRIM, P, ModulesTS)
    case_FIELD(PRIM, P, CompilingPCH)
    case_FIELD(PRIM, P, BuildingPCHWithObjectFile)
    case_FIELD(PRIM, P, ModulesDeclUse)
    case_FIELD(PRIM, P, ModulesSearchAll)
    case_FIELD(PRIM, P, ModulesStrictDeclUse)
    case_FIELD(PRIM, P, ModulesErrorRecovery)
    case_FIELD(PRIM, P, ImplicitModules)
    case_FIELD(PRIM, P, ModulesLocalVisibility)
    case_FIELD(PRIM, P, Optimize)
    case_FIELD(PRIM, P, OptimizeSize)
    case_FIELD(PRIM, P, Static)
    case_FIELD(PRIM, P, PackStruct)
    case_FIELD(PRIM, P, MaxTypeAlign)
    case_FIELD(PRIM, P, AlignDouble)
    case_FIELD(PRIM, P, PICLevel)
    case_FIELD(PRIM, P, PIE)
    case_FIELD(PRIM, P, GNUInline)
    case_FIELD(PRIM, P, NoInlineDefine)
    case_FIELD(PRIM, P, Deprecated)
    case_FIELD(PRIM, P, FastMath)
    case_FIELD(PRIM, P, FiniteMathOnly)
    case_FIELD(PRIM, P, UnsafeFPMath)
    case_FIELD(PRIM, P, ObjCGCBitmapPrint)
    case_FIELD(PRIM, P, AccessControl)
    case_FIELD(PRIM, P, CharIsSigned)
    case_FIELD(PRIM, P, WCharSize)
    case_FIELD(PRIM, P, WCharIsSigned)
    case_FIELD(PRIM, P, ShortEnums)
    case_FIELD(PRIM, P, OpenCL)
    case_FIELD(PRIM, P, OpenCLVersion)
    case_FIELD(PRIM, P, OpenCLCPlusPlus)
    case_FIELD(PRIM, P, OpenCLCPlusPlusVersion)
    case_FIELD(PRIM, P, NativeHalfType)
    case_FIELD(PRIM, P, NativeHalfArgsAndReturns)
    case_FIELD(PRIM, P, HalfArgsAndReturns)
    case_FIELD(PRIM, P, CUDA)
    case_FIELD(PRIM, P, HIP)
    case_FIELD(PRIM, P, OpenMP)
    case_FIELD(PRIM, P, OpenMPSimd)
    case_FIELD(PRIM, P, OpenMPUseTLS)
    case_FIELD(PRIM, P, OpenMPIsDevice)
    case_FIELD(PRIM, P, OpenMPCUDAMode)
    case_FIELD(PRIM, P, OpenMPHostCXXExceptions)
    case_FIELD(PRIM, P, RenderScript)
    case_FIELD(PRIM, P, CUDAIsDevice)
    case_FIELD(PRIM, P, CUDAAllowVariadicFunctions)
    case_FIELD(PRIM, P, CUDAHostDeviceConstexpr)
    case_FIELD(PRIM, P, CUDADeviceApproxTranscendentals)
    case_FIELD(PRIM, P, CUDARelocatableDeviceCode)
    case_FIELD(PRIM, P, SizedDeallocation)
    case_FIELD(PRIM, P, AlignedAllocation)
    case_FIELD(PRIM, P, AlignedAllocationUnavailable)
    case_FIELD(PRIM, P, NewAlignOverride)
    case_FIELD(PRIM, P, ConceptsTS)
    case_FIELD(PRIM, P, Reflection)
    case_FIELD(PRIM, P, ModulesCodegen)
    case_FIELD(PRIM, P, ModulesDebugInfo)
    case_FIELD(PRIM, P, ElideConstructors)
    case_FIELD(PRIM, P, DumpRecordLayouts)
    case_FIELD(PRIM, P, DumpRecordLayoutsSimple)
    case_FIELD(PRIM, P, DumpVTableLayouts)
    case_FIELD(PRIM, P, NoConstantCFStrings)
    case_FIELD(PRIM, P, InlineVisibilityHidden)
    case_FIELD(PRIM, P, ParseUnknownAnytype)
    case_FIELD(PRIM, P, DebuggerSupport)
    case_FIELD(PRIM, P, DebuggerCastResultToId)
    case_FIELD(PRIM, P, DebuggerObjCLiteral)
    case_FIELD(PRIM, P, SpellChecking)
    case_FIELD(PRIM, P, SinglePrecisionConstants)
    case_FIELD(PRIM, P, FastRelaxedMath)
    case_FIELD(PRIM, P, NoBitFieldTypeAlign)
    case_FIELD(PRIM, P, HexagonQdsp6Compat)
    case_FIELD(PRIM, P, ObjCAutoRefCount)
    case_FIELD(PRIM, P, ObjCWeakRuntime)
    case_FIELD(PRIM, P, ObjCWeak)
    case_FIELD(PRIM, P, ObjCSubscriptingLegacyRuntime)
    case_FIELD(PRIM, P, CFProtectionBranch)
    case_FIELD(PRIM, P, FakeAddressSpaceMap)
    case_FIELD(PRIM, P, IncludeDefaultHeader)
    case_FIELD(PRIM, P, DelayedTemplateParsing)
    case_FIELD(PRIM, P, BlocksRuntimeOptional)
    case_FIELD(PRIM, P, CompleteMemberPointers)
    case_FIELD(PRIM, P, ArrowDepth)
    case_FIELD(PRIM, P, InstantiationDepth)
    case_FIELD(PRIM, P, ConstexprCallDepth)
    case_FIELD(PRIM, P, ConstexprStepLimit)
    case_FIELD(PRIM, P, BracketDepth)
    case_FIELD(PRIM, P, NumLargeByValueCopy)
    case_FIELD(PRIM, P, MSCompatibilityVersion)
    case_FIELD(PRIM, P, VtorDispMode)
    case_FIELD(PRIM, P, ApplePragmaPack)
    case_FIELD(PRIM, P, RetainCommentsFromSystemHeaders)
    case_FIELD(PRIM, P, SanitizeAddressFieldPadding)
    case_FIELD(PRIM, P, XRayInstrument)
    case_FIELD(PRIM, P, XRayAlwaysEmitCustomEvents)
    case_FIELD(PRIM, P, XRayAlwaysEmitTypedEvents)
    case_FIELD(PRIM, P, ForceEmitVTables)
    case_FIELD(PRIM, P, AllowEditorPlaceholders)
    case_FIELD(PRIM, P, FunctionAlignment)
    case_FIELD(PRIM, P, FixedPoint)
    case_FIELD(PRIM, P, PaddingOnUnsignedFixedPoint)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::LangOptions *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__LangOptions;
# define CLASSNAME clang::LangOptions
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, P, Sanitize)
    case_FIELD(CLASS, P, ObjCRuntime)
    case_FIELD(CLASS, P, CommentOpts)
    case_FIELD(PRIM, P, IsHeaderFile)
    case_METHOD_NOARGS(getCompilingModule, PRIM, P, getCompilingModule)
    case_METHOD_NOARGS(getMSPointerToMemberRepresentationMethod, PRIM, P, getMSPointerToMemberRepresentationMethod)
    case_METHOD_NOARGS(getDefaultCallingConv, PRIM, P, getDefaultCallingConv)
    case_METHOD_NOARGS(getDefaultFPContractMode, PRIM, P, getDefaultFPContractMode)
    case_METHOD_NOARGS(getAddressSpaceMapMangling, PRIM, P, getAddressSpaceMapMangling)
    case_METHOD_NOARGS(getGC, PRIM, P, getGC)
    case_METHOD_NOARGS(getValueVisibilityMode, PRIM, P, getValueVisibilityMode)
    case_METHOD_NOARGS(getTypeVisibilityMode, PRIM, P, getTypeVisibilityMode)
    case_METHOD_NOARGS(getStackProtector, PRIM, P, getStackProtector)
    case_METHOD_NOARGS(getSignedOverflowBehavior, PRIM, P, getSignedOverflowBehavior)
    case_METHOD_NOARGS(getClangABICompat, PRIM, P, getClangABICompat)
    case_METHOD_NOARGS(isCompilingModule, PRIM, P, isCompilingModule)
    case_METHOD_NOARGS(trackLocalOwningModule, PRIM, P, trackLocalOwningModule)
    case_METHOD_NOARGS(isSignedOverflowDefined, PRIM, P, isSignedOverflowDefined)
    case_METHOD_NOARGS(isSubscriptPointerArithmetic, PRIM, P, isSubscriptPointerArithmetic)
    case currefl::isCompatibleWithMSVC: {
      CHECK_TRAIT_IS_REFLPROP(isCompatibleWithMSVC)
      TRY_LOAD_VAL(enum clang::LangOptions::MSVCMajorVersion, p0)
      if (no_more) PRIM_REFLECTION(P, isCompatibleWithMSVC, p0)
      TOO_MANY;
    }
    case currefl::isNoBuiltinFunc: {
      CHECK_TRAIT_IS_REFLPROP(isNoBuiltinFunc)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, isNoBuiltinFunc, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(allowsNonTrivialObjCLifetimeQualifiers, PRIM, P, allowsNonTrivialObjCLifetimeQualifiers)
    case_METHOD_NOARGS(assumeFunctionsAreConvergent, PRIM, P, assumeFunctionsAreConvergent)
    case_METHOD_NOARGS(getOpenCLVersionTuple, CLASS, P, getOpenCLVersionTuple)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FPOptions X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FPOptions;
# define CLASSNAME clang::FPOptions
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(allowFPContractWithinStatement, PRIM, V, allowFPContractWithinStatement)
    case_METHOD_NOARGS(allowFPContractAcrossStatement, PRIM, V, allowFPContractAcrossStatement)
    case_METHOD_NOARGS(getInt, PRIM, V, getInt)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::PrintingPolicy *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__PrintingPolicy;
# define CLASSNAME clang::PrintingPolicy
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, P, Indentation)
    case_FIELD(PRIM, P, SuppressSpecifiers)
    case_FIELD(PRIM, P, SuppressTagKeyword)
    case_FIELD(PRIM, P, IncludeTagDefinition)
    case_FIELD(PRIM, P, SuppressScope)
    case_FIELD(PRIM, P, SuppressUnwrittenScope)
    case_FIELD(PRIM, P, SuppressInitializers)
    case_FIELD(PRIM, P, ConstantArraySizeAsWritten)
    case_FIELD(PRIM, P, AnonymousTagLocations)
    case_FIELD(PRIM, P, SuppressStrongLifetime)
    case_FIELD(PRIM, P, SuppressLifetimeQualifiers)
    case_FIELD(PRIM, P, SuppressTemplateArgsInCXXConstructors)
    case_FIELD(PRIM, P, Bool)
    case_FIELD(PRIM, P, Restrict)
    case_FIELD(PRIM, P, Alignof)
    case_FIELD(PRIM, P, UnderscoreAlignof)
    case_FIELD(PRIM, P, UseVoidForZeroParams)
    case_FIELD(PRIM, P, TerseOutput)
    case_FIELD(PRIM, P, PolishForDeclaration)
    case_FIELD(PRIM, P, Half)
    case_FIELD(PRIM, P, MSWChar)
    case_FIELD(PRIM, P, IncludeNewlines)
    case_FIELD(PRIM, P, MSVCFormatting)
    case_FIELD(PRIM, P, ConstantsAsWritten)
    case_FIELD(PRIM, P, SuppressImplicitBase)
    case_FIELD(PRIM, P, FullyQualifiedName)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SrcMgr::ContentCache *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SrcMgr__ContentCache;
# define CLASSNAME clang::SrcMgr::ContentCache
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, P, OrigEntry)
    case_FIELD(CLASS, P, ContentsEntry)
    case_FIELD(PRIM, P, SourceLineCache)
    case_FIELD(PRIM, P, NumLines)
    case_FIELD(PRIM, P, BufferOverridden)
    case_FIELD(PRIM, P, IsSystemFile)
    case_FIELD(PRIM, P, IsTransient)
    case_METHOD_NOARGS(getSize, PRIM, P, getSize)
    case_METHOD_NOARGS(getSizeBytesMapped, PRIM, P, getSizeBytesMapped)
    case_METHOD_NOARGS(getMemoryBufferKind, PRIM, P, getMemoryBufferKind)
    case_METHOD_NOARGS(getRawBuffer, CLASS, P, getRawBuffer)
    case_METHOD_NOARGS(isBufferInvalid, PRIM, P, isBufferInvalid)
    case_METHOD_NOARGS(shouldFreeBuffer, PRIM, P, shouldFreeBuffer)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SrcMgr::FileInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SrcMgr__FileInfo;
# define CLASSNAME clang::SrcMgr::FileInfo
  switch( (currefl::memnames)N ) {
    case currefl::get: {
      CHECK_TRAIT_IS_REFLPROP(get)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(const class clang::SrcMgr::ContentCache *, p1)
      TRY_LOAD_VAL(enum clang::SrcMgr::CharacteristicKind, p2)
      if (no_more) CLASS_REFLECTION(S, get, p0, p1, p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getIncludeLoc, CLASS, P, getIncludeLoc)
    case_METHOD_NOARGS(getContentCache, CLASS, P, getContentCache)
    case_METHOD_NOARGS(getFileCharacteristic, PRIM, P, getFileCharacteristic)
    case_METHOD_NOARGS(hasLineDirectives, PRIM, P, hasLineDirectives)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SrcMgr::FileInfo X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SrcMgr__FileInfo;
# define CLASSNAME clang::SrcMgr::FileInfo
  switch( (currefl::memnames)N ) {
    case currefl::get: {
      CHECK_TRAIT_IS_REFLPROP(get)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(const class clang::SrcMgr::ContentCache *, p1)
      TRY_LOAD_VAL(enum clang::SrcMgr::CharacteristicKind, p2)
      if (no_more) CLASS_REFLECTION(S, get, p0, p1, p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getIncludeLoc, CLASS, V, getIncludeLoc)
    case_METHOD_NOARGS(getContentCache, CLASS, V, getContentCache)
    case_METHOD_NOARGS(getFileCharacteristic, PRIM, V, getFileCharacteristic)
    case_METHOD_NOARGS(hasLineDirectives, PRIM, V, hasLineDirectives)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SrcMgr::ExpansionInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SrcMgr__ExpansionInfo;
# define CLASSNAME clang::SrcMgr::ExpansionInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSpellingLoc, CLASS, P, getSpellingLoc)
    case_METHOD_NOARGS(getExpansionLocStart, CLASS, P, getExpansionLocStart)
    case_METHOD_NOARGS(getExpansionLocEnd, CLASS, P, getExpansionLocEnd)
    case_METHOD_NOARGS(isExpansionTokenRange, PRIM, P, isExpansionTokenRange)
    case_METHOD_NOARGS(getExpansionLocRange, CLASS, P, getExpansionLocRange)
    case_METHOD_NOARGS(isMacroArgExpansion, PRIM, P, isMacroArgExpansion)
    case_METHOD_NOARGS(isMacroBodyExpansion, PRIM, P, isMacroBodyExpansion)
    case_METHOD_NOARGS(isFunctionMacroExpansion, PRIM, P, isFunctionMacroExpansion)
    case currefl::create: {
      CHECK_TRAIT_IS_REFLPROP(create)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      TRY_LOAD_VAL(class clang::SourceLocation, p2)
      if (no_more) CLASS_REFLECTION(S, create, p0, p1, p2)
      TRY_LOAD_VAL(bool, p3)
      if (no_more) CLASS_REFLECTION(S, create, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::createForMacroArg: {
      CHECK_TRAIT_IS_REFLPROP(createForMacroArg)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) CLASS_REFLECTION(S, createForMacroArg, p0, p1)
      TOO_MANY;
    }
    case currefl::createForTokenSplit: {
      CHECK_TRAIT_IS_REFLPROP(createForTokenSplit)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      TRY_LOAD_VAL(class clang::SourceLocation, p2)
      if (no_more) CLASS_REFLECTION(S, createForTokenSplit, p0, p1, p2)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SrcMgr::ExpansionInfo X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SrcMgr__ExpansionInfo;
# define CLASSNAME clang::SrcMgr::ExpansionInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSpellingLoc, CLASS, V, getSpellingLoc)
    case_METHOD_NOARGS(getExpansionLocStart, CLASS, V, getExpansionLocStart)
    case_METHOD_NOARGS(getExpansionLocEnd, CLASS, V, getExpansionLocEnd)
    case_METHOD_NOARGS(isExpansionTokenRange, PRIM, V, isExpansionTokenRange)
    case_METHOD_NOARGS(getExpansionLocRange, CLASS, V, getExpansionLocRange)
    case_METHOD_NOARGS(isMacroArgExpansion, PRIM, V, isMacroArgExpansion)
    case_METHOD_NOARGS(isMacroBodyExpansion, PRIM, V, isMacroBodyExpansion)
    case_METHOD_NOARGS(isFunctionMacroExpansion, PRIM, V, isFunctionMacroExpansion)
    case currefl::create: {
      CHECK_TRAIT_IS_REFLPROP(create)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      TRY_LOAD_VAL(class clang::SourceLocation, p2)
      if (no_more) CLASS_REFLECTION(S, create, p0, p1, p2)
      TRY_LOAD_VAL(bool, p3)
      if (no_more) CLASS_REFLECTION(S, create, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::createForMacroArg: {
      CHECK_TRAIT_IS_REFLPROP(createForMacroArg)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) CLASS_REFLECTION(S, createForMacroArg, p0, p1)
      TOO_MANY;
    }
    case currefl::createForTokenSplit: {
      CHECK_TRAIT_IS_REFLPROP(createForTokenSplit)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      TRY_LOAD_VAL(class clang::SourceLocation, p2)
      if (no_more) CLASS_REFLECTION(S, createForTokenSplit, p0, p1, p2)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SrcMgr::SLocEntry *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SrcMgr__SLocEntry;
# define CLASSNAME clang::SrcMgr::SLocEntry
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOffset, PRIM, P, getOffset)
    case_METHOD_NOARGS(isExpansion, PRIM, P, isExpansion)
    case_METHOD_NOARGS(isFile, PRIM, P, isFile)
    case_METHOD_NOARGS(getFile, CLASS, P, getFile)
    case_METHOD_NOARGS(getExpansion, CLASS, P, getExpansion)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SourceManager *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SourceManager;
# define CLASSNAME clang::SourceManager
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDiagnostics, CLASS, P, getDiagnostics)
    case_METHOD_NOARGS(getFileManager, CLASS, P, getFileManager)
    case_METHOD_NOARGS(userFilesAreVolatile, PRIM, P, userFilesAreVolatile)
    case_METHOD_NOARGS(getMainFileID, CLASS, P, getMainFileID)
    case_METHOD_NOARGS(getPreambleFileID, CLASS, P, getPreambleFileID)
    case currefl::isFileOverridden: {
      CHECK_TRAIT_IS_REFLPROP(isFileOverridden)
      TRY_LOAD_VAL(const class clang::FileEntry *, p0)
      if (no_more) PRIM_REFLECTION(P, isFileOverridden, p0)
      TOO_MANY;
    }
    case currefl::getBuffer: {
      CHECK_TRAIT_IS_REFLPROP(getBuffer)
      TRY_LOAD_VAL(class clang::FileID, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) CLASS_REFLECTION(P, getBuffer, p0, p1)
      TRY_LOAD_VAL(bool *, p2)
      if (no_more) CLASS_REFLECTION(P, getBuffer, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getBuffer1: {
      CHECK_TRAIT_IS_REFLPROP(getBuffer1)
      TRY_LOAD_VAL(class clang::FileID, p0)
      if (no_more) CLASS_REFLECTION(P, getBuffer, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) CLASS_REFLECTION(P, getBuffer, p0, p1)
      TOO_MANY;
    }
    case currefl::getFileEntryForID: {
      CHECK_TRAIT_IS_REFLPROP(getFileEntryForID)
      TRY_LOAD_VAL(class clang::FileID, p0)
      if (no_more) CLASS_REFLECTION(P, getFileEntryForID, p0)
      TOO_MANY;
    }
    case currefl::getBufferData: {
      CHECK_TRAIT_IS_REFLPROP(getBufferData)
      TRY_LOAD_VAL(class clang::FileID, p0)
      if (no_more) CLASS_REFLECTION(P, getBufferData, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) CLASS_REFLECTION(P, getBufferData, p0, p1)
      TOO_MANY;
    }
    case currefl::getNumCreatedFIDsForFileID: {
      CHECK_TRAIT_IS_REFLPROP(getNumCreatedFIDsForFileID)
      TRY_LOAD_VAL(class clang::FileID, p0)
      if (no_more) PRIM_REFLECTION(P, getNumCreatedFIDsForFileID, p0)
      TOO_MANY;
    }
    case currefl::setNumCreatedFIDsForFileID: {
      CHECK_TRAIT_IS_REFLPROP(setNumCreatedFIDsForFileID)
      TRY_LOAD_VAL(class clang::FileID, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) VOID_REFLECTION(P, setNumCreatedFIDsForFileID, (class clang::FileID)p0, (unsigned int)p1)
      TOO_MANY;
    }
    case currefl::getFileID: {
      CHECK_TRAIT_IS_REFLPROP(getFileID)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getFileID, p0)
      TOO_MANY;
    }
    case currefl::getFilename: {
      CHECK_TRAIT_IS_REFLPROP(getFilename)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getFilename, p0)
      TOO_MANY;
    }
    case currefl::getLocForStartOfFile: {
      CHECK_TRAIT_IS_REFLPROP(getLocForStartOfFile)
      TRY_LOAD_VAL(class clang::FileID, p0)
      if (no_more) CLASS_REFLECTION(P, getLocForStartOfFile, p0)
      TOO_MANY;
    }
    case currefl::getLocForEndOfFile: {
      CHECK_TRAIT_IS_REFLPROP(getLocForEndOfFile)
      TRY_LOAD_VAL(class clang::FileID, p0)
      if (no_more) CLASS_REFLECTION(P, getLocForEndOfFile, p0)
      TOO_MANY;
    }
    case currefl::getIncludeLoc: {
      CHECK_TRAIT_IS_REFLPROP(getIncludeLoc)
      TRY_LOAD_VAL(class clang::FileID, p0)
      if (no_more) CLASS_REFLECTION(P, getIncludeLoc, p0)
      TOO_MANY;
    }
    case currefl::getExpansionLoc: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionLoc)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getExpansionLoc, p0)
      TOO_MANY;
    }
    case currefl::getFileLoc: {
      CHECK_TRAIT_IS_REFLPROP(getFileLoc)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getFileLoc, p0)
      TOO_MANY;
    }
    case currefl::getImmediateExpansionRange: {
      CHECK_TRAIT_IS_REFLPROP(getImmediateExpansionRange)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getImmediateExpansionRange, p0)
      TOO_MANY;
    }
    case currefl::getExpansionRange: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionRange)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getExpansionRange, p0)
      TOO_MANY;
    }
    case currefl::getExpansionRange1: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionRange1)
      TRY_LOAD_VAL(class clang::SourceRange, p0)
      if (no_more) CLASS_REFLECTION(P, getExpansionRange, p0)
      TOO_MANY;
    }
    case currefl::getExpansionRange2: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionRange2)
      TRY_LOAD_VAL(class clang::CharSourceRange, p0)
      if (no_more) CLASS_REFLECTION(P, getExpansionRange, p0)
      TOO_MANY;
    }
    case currefl::getSpellingLoc: {
      CHECK_TRAIT_IS_REFLPROP(getSpellingLoc)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getSpellingLoc, p0)
      TOO_MANY;
    }
    case currefl::getImmediateSpellingLoc: {
      CHECK_TRAIT_IS_REFLPROP(getImmediateSpellingLoc)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getImmediateSpellingLoc, p0)
      TOO_MANY;
    }
    case currefl::getComposedLoc: {
      CHECK_TRAIT_IS_REFLPROP(getComposedLoc)
      TRY_LOAD_VAL(class clang::FileID, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(P, getComposedLoc, p0, p1)
      TOO_MANY;
    }
    case currefl::getFileOffset: {
      CHECK_TRAIT_IS_REFLPROP(getFileOffset)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, getFileOffset, p0)
      TOO_MANY;
    }
    case currefl::isMacroArgExpansion: {
      CHECK_TRAIT_IS_REFLPROP(isMacroArgExpansion)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isMacroArgExpansion, p0)
      TRY_LOAD_VAL(class clang::SourceLocation *, p1)
      if (no_more) PRIM_REFLECTION(P, isMacroArgExpansion, p0, p1)
      TOO_MANY;
    }
    case currefl::isMacroBodyExpansion: {
      CHECK_TRAIT_IS_REFLPROP(isMacroBodyExpansion)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isMacroBodyExpansion, p0)
      TOO_MANY;
    }
    case currefl::isAtStartOfImmediateMacroExpansion: {
      CHECK_TRAIT_IS_REFLPROP(isAtStartOfImmediateMacroExpansion)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isAtStartOfImmediateMacroExpansion, p0)
      TRY_LOAD_VAL(class clang::SourceLocation *, p1)
      if (no_more) PRIM_REFLECTION(P, isAtStartOfImmediateMacroExpansion, p0, p1)
      TOO_MANY;
    }
    case currefl::isAtEndOfImmediateMacroExpansion: {
      CHECK_TRAIT_IS_REFLPROP(isAtEndOfImmediateMacroExpansion)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isAtEndOfImmediateMacroExpansion, p0)
      TRY_LOAD_VAL(class clang::SourceLocation *, p1)
      if (no_more) PRIM_REFLECTION(P, isAtEndOfImmediateMacroExpansion, p0, p1)
      TOO_MANY;
    }
    case currefl::isInSLocAddrSpace: {
      CHECK_TRAIT_IS_REFLPROP(isInSLocAddrSpace)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) PRIM_REFLECTION(P, isInSLocAddrSpace, p0, p1, p2)
      TRY_LOAD_VAL(unsigned int *, p3)
      if (no_more) PRIM_REFLECTION(P, isInSLocAddrSpace, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::isInSameSLocAddrSpace: {
      CHECK_TRAIT_IS_REFLPROP(isInSameSLocAddrSpace)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      TRY_LOAD_VAL(int *, p2)
      if (no_more) PRIM_REFLECTION(P, isInSameSLocAddrSpace, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getCharacterData: {
      CHECK_TRAIT_IS_REFLPROP(getCharacterData)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, getCharacterData, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) PRIM_REFLECTION(P, getCharacterData, p0, p1)
      TOO_MANY;
    }
    case currefl::getColumnNumber: {
      CHECK_TRAIT_IS_REFLPROP(getColumnNumber)
      TRY_LOAD_VAL(class clang::FileID, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(P, getColumnNumber, p0, p1)
      TRY_LOAD_VAL(bool *, p2)
      if (no_more) PRIM_REFLECTION(P, getColumnNumber, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getSpellingColumnNumber: {
      CHECK_TRAIT_IS_REFLPROP(getSpellingColumnNumber)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, getSpellingColumnNumber, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) PRIM_REFLECTION(P, getSpellingColumnNumber, p0, p1)
      TOO_MANY;
    }
    case currefl::getExpansionColumnNumber: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionColumnNumber)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, getExpansionColumnNumber, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) PRIM_REFLECTION(P, getExpansionColumnNumber, p0, p1)
      TOO_MANY;
    }
    case currefl::getPresumedColumnNumber: {
      CHECK_TRAIT_IS_REFLPROP(getPresumedColumnNumber)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, getPresumedColumnNumber, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) PRIM_REFLECTION(P, getPresumedColumnNumber, p0, p1)
      TOO_MANY;
    }
    case currefl::getLineNumber: {
      CHECK_TRAIT_IS_REFLPROP(getLineNumber)
      TRY_LOAD_VAL(class clang::FileID, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(P, getLineNumber, p0, p1)
      TRY_LOAD_VAL(bool *, p2)
      if (no_more) PRIM_REFLECTION(P, getLineNumber, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getSpellingLineNumber: {
      CHECK_TRAIT_IS_REFLPROP(getSpellingLineNumber)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, getSpellingLineNumber, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) PRIM_REFLECTION(P, getSpellingLineNumber, p0, p1)
      TOO_MANY;
    }
    case currefl::getExpansionLineNumber: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionLineNumber)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, getExpansionLineNumber, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) PRIM_REFLECTION(P, getExpansionLineNumber, p0, p1)
      TOO_MANY;
    }
    case currefl::getPresumedLineNumber: {
      CHECK_TRAIT_IS_REFLPROP(getPresumedLineNumber)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, getPresumedLineNumber, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) PRIM_REFLECTION(P, getPresumedLineNumber, p0, p1)
      TOO_MANY;
    }
    case currefl::getBufferName: {
      CHECK_TRAIT_IS_REFLPROP(getBufferName)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getBufferName, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) CLASS_REFLECTION(P, getBufferName, p0, p1)
      TOO_MANY;
    }
    case currefl::getFileCharacteristic: {
      CHECK_TRAIT_IS_REFLPROP(getFileCharacteristic)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, getFileCharacteristic, p0)
      TOO_MANY;
    }
    case currefl::getPresumedLoc: {
      CHECK_TRAIT_IS_REFLPROP(getPresumedLoc)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getPresumedLoc, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, getPresumedLoc, p0, p1)
      TOO_MANY;
    }
    case currefl::isInMainFile: {
      CHECK_TRAIT_IS_REFLPROP(isInMainFile)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isInMainFile, p0)
      TOO_MANY;
    }
    case currefl::isWrittenInSameFile: {
      CHECK_TRAIT_IS_REFLPROP(isWrittenInSameFile)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) PRIM_REFLECTION(P, isWrittenInSameFile, p0, p1)
      TOO_MANY;
    }
    case currefl::isWrittenInMainFile: {
      CHECK_TRAIT_IS_REFLPROP(isWrittenInMainFile)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isWrittenInMainFile, p0)
      TOO_MANY;
    }
    case currefl::isInSystemHeader: {
      CHECK_TRAIT_IS_REFLPROP(isInSystemHeader)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isInSystemHeader, p0)
      TOO_MANY;
    }
    case currefl::isInExternCSystemHeader: {
      CHECK_TRAIT_IS_REFLPROP(isInExternCSystemHeader)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isInExternCSystemHeader, p0)
      TOO_MANY;
    }
    case currefl::isInSystemMacro: {
      CHECK_TRAIT_IS_REFLPROP(isInSystemMacro)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isInSystemMacro, p0)
      TOO_MANY;
    }
    case currefl::getFileIDSize: {
      CHECK_TRAIT_IS_REFLPROP(getFileIDSize)
      TRY_LOAD_VAL(class clang::FileID, p0)
      if (no_more) PRIM_REFLECTION(P, getFileIDSize, p0)
      TOO_MANY;
    }
    case currefl::isInFileID: {
      CHECK_TRAIT_IS_REFLPROP(isInFileID)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::FileID, p1)
      if (no_more) PRIM_REFLECTION(P, isInFileID, p0, p1)
      TRY_LOAD_VAL(unsigned int *, p2)
      if (no_more) PRIM_REFLECTION(P, isInFileID, p0, p1, p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasLineTable, PRIM, P, hasLineTable)
    case_METHOD_NOARGS(getContentCacheSize, PRIM, P, getContentCacheSize)
    case_METHOD_NOARGS(getMemoryBufferSizes, CLASS, P, getMemoryBufferSizes)
    case_METHOD_NOARGS(getDataStructureSizes, PRIM, P, getDataStructureSizes)
    case currefl::translateFileLineCol: {
      CHECK_TRAIT_IS_REFLPROP(translateFileLineCol)
      TRY_LOAD_VAL(const class clang::FileEntry *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) CLASS_REFLECTION(P, translateFileLineCol, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::translateFile: {
      CHECK_TRAIT_IS_REFLPROP(translateFile)
      TRY_LOAD_VAL(const class clang::FileEntry *, p0)
      if (no_more) CLASS_REFLECTION(P, translateFile, p0)
      TOO_MANY;
    }
    case currefl::translateLineCol: {
      CHECK_TRAIT_IS_REFLPROP(translateLineCol)
      TRY_LOAD_VAL(class clang::FileID, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) CLASS_REFLECTION(P, translateLineCol, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getMacroArgExpandedLocation: {
      CHECK_TRAIT_IS_REFLPROP(getMacroArgExpandedLocation)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getMacroArgExpandedLocation, p0)
      TOO_MANY;
    }
    case currefl::isBeforeInTranslationUnit: {
      CHECK_TRAIT_IS_REFLPROP(isBeforeInTranslationUnit)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) PRIM_REFLECTION(P, isBeforeInTranslationUnit, p0, p1)
      TOO_MANY;
    }
    case currefl::isBeforeInSLocAddrSpace: {
      CHECK_TRAIT_IS_REFLPROP(isBeforeInSLocAddrSpace)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) PRIM_REFLECTION(P, isBeforeInSLocAddrSpace, p0, p1)
      TOO_MANY;
    }
    case currefl::isBeforeInSLocAddrSpace1: {
      CHECK_TRAIT_IS_REFLPROP(isBeforeInSLocAddrSpace1)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(P, isBeforeInSLocAddrSpace, p0, p1)
      TOO_MANY;
    }
    case currefl::isPointWithin: {
      CHECK_TRAIT_IS_REFLPROP(isPointWithin)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      TRY_LOAD_VAL(class clang::SourceLocation, p2)
      if (no_more) PRIM_REFLECTION(P, isPointWithin, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::hasFileInfo: {
      CHECK_TRAIT_IS_REFLPROP(hasFileInfo)
      TRY_LOAD_VAL(const class clang::FileEntry *, p0)
      if (no_more) PRIM_REFLECTION(P, hasFileInfo, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(PrintStats, VOID, P, PrintStats)
    case_METHOD_NOARGS(dump, VOID, P, dump)
    case_METHOD_NOARGS(local_sloc_entry_size, PRIM, P, local_sloc_entry_size)
    case currefl::getLocalSLocEntry: {
      CHECK_TRAIT_IS_REFLPROP(getLocalSLocEntry)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getLocalSLocEntry, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) CLASS_REFLECTION(P, getLocalSLocEntry, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(loaded_sloc_entry_size, PRIM, P, loaded_sloc_entry_size)
    case currefl::getLoadedSLocEntry: {
      CHECK_TRAIT_IS_REFLPROP(getLoadedSLocEntry)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getLoadedSLocEntry, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) CLASS_REFLECTION(P, getLoadedSLocEntry, p0, p1)
      TOO_MANY;
    }
    case currefl::getSLocEntry: {
      CHECK_TRAIT_IS_REFLPROP(getSLocEntry)
      TRY_LOAD_VAL(class clang::FileID, p0)
      if (no_more) CLASS_REFLECTION(P, getSLocEntry, p0)
      TRY_LOAD_VAL(bool *, p1)
      if (no_more) CLASS_REFLECTION(P, getSLocEntry, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNextLocalOffset, PRIM, P, getNextLocalOffset)
    case currefl::isLoadedSourceLocation: {
      CHECK_TRAIT_IS_REFLPROP(isLoadedSourceLocation)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isLoadedSourceLocation, p0)
      TOO_MANY;
    }
    case currefl::isLocalSourceLocation: {
      CHECK_TRAIT_IS_REFLPROP(isLocalSourceLocation)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, isLocalSourceLocation, p0)
      TOO_MANY;
    }
    case currefl::isLoadedFileID: {
      CHECK_TRAIT_IS_REFLPROP(isLoadedFileID)
      TRY_LOAD_VAL(class clang::FileID, p0)
      if (no_more) PRIM_REFLECTION(P, isLoadedFileID, p0)
      TOO_MANY;
    }
    case currefl::isLocalFileID: {
      CHECK_TRAIT_IS_REFLPROP(isLocalFileID)
      TRY_LOAD_VAL(class clang::FileID, p0)
      if (no_more) PRIM_REFLECTION(P, isLocalFileID, p0)
      TOO_MANY;
    }
    case currefl::getImmediateMacroCallerLoc: {
      CHECK_TRAIT_IS_REFLPROP(getImmediateMacroCallerLoc)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getImmediateMacroCallerLoc, p0)
      TOO_MANY;
    }
    case currefl::getTopMacroCallerLoc: {
      CHECK_TRAIT_IS_REFLPROP(getTopMacroCallerLoc)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getTopMacroCallerLoc, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SourceManager::MemoryBufferSizes X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SourceManager__MemoryBufferSizes;
# define CLASSNAME clang::SourceManager::MemoryBufferSizes
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, V, malloc_bytes)
    case_FIELD(PRIM, V, mmap_bytes)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::RawComment *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__RawComment;
# define CLASSNAME clang::RawComment
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(isInvalid, PRIM, P, isInvalid)
    case_METHOD_NOARGS(isMerged, PRIM, P, isMerged)
    case_METHOD_NOARGS(isAttached, PRIM, P, isAttached)
    case_METHOD_NOARGS(isTrailingComment, PRIM, P, isTrailingComment)
    case_METHOD_NOARGS(isAlmostTrailingComment, PRIM, P, isAlmostTrailingComment)
    case_METHOD_NOARGS(isOrdinary, PRIM, P, isOrdinary)
    case_METHOD_NOARGS(isDocumentation, PRIM, P, isDocumentation)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::RawCommentList X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__RawCommentList;
# define CLASSNAME clang::RawCommentList
  switch( (currefl::memnames)N ) {
    case currefl::getComments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getComments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(V, getComments)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SanitizerBlacklist *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__SanitizerBlacklist;
# define CLASSNAME clang::SanitizerBlacklist
  switch( (currefl::memnames)N ) {
    case currefl::isBlacklistedGlobal: {
      CHECK_TRAIT_IS_REFLPROP(isBlacklistedGlobal)
      TRY_LOAD_VAL(unsigned long long, p0)
      TRY_LOAD_VAL(class llvm::StringRef, p1)
      if (no_more) PRIM_REFLECTION(P, isBlacklistedGlobal, p0, p1)
      TRY_LOAD_VAL(class llvm::StringRef, p2)
      if (no_more) PRIM_REFLECTION(P, isBlacklistedGlobal, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::isBlacklistedType: {
      CHECK_TRAIT_IS_REFLPROP(isBlacklistedType)
      TRY_LOAD_VAL(unsigned long long, p0)
      TRY_LOAD_VAL(class llvm::StringRef, p1)
      if (no_more) PRIM_REFLECTION(P, isBlacklistedType, p0, p1)
      TRY_LOAD_VAL(class llvm::StringRef, p2)
      if (no_more) PRIM_REFLECTION(P, isBlacklistedType, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::isBlacklistedFunction: {
      CHECK_TRAIT_IS_REFLPROP(isBlacklistedFunction)
      TRY_LOAD_VAL(unsigned long long, p0)
      TRY_LOAD_VAL(class llvm::StringRef, p1)
      if (no_more) PRIM_REFLECTION(P, isBlacklistedFunction, p0, p1)
      TOO_MANY;
    }
    case currefl::isBlacklistedFile: {
      CHECK_TRAIT_IS_REFLPROP(isBlacklistedFile)
      TRY_LOAD_VAL(unsigned long long, p0)
      TRY_LOAD_VAL(class llvm::StringRef, p1)
      if (no_more) PRIM_REFLECTION(P, isBlacklistedFile, p0, p1)
      TRY_LOAD_VAL(class llvm::StringRef, p2)
      if (no_more) PRIM_REFLECTION(P, isBlacklistedFile, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::isBlacklistedLocation: {
      CHECK_TRAIT_IS_REFLPROP(isBlacklistedLocation)
      TRY_LOAD_VAL(unsigned long long, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) PRIM_REFLECTION(P, isBlacklistedLocation, p0, p1)
      TRY_LOAD_VAL(class llvm::StringRef, p2)
      if (no_more) PRIM_REFLECTION(P, isBlacklistedLocation, p0, p1, p2)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TargetCXXABI X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TargetCXXABI;
# define CLASSNAME clang::TargetCXXABI
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKind, PRIM, V, getKind)
    case_METHOD_NOARGS(isItaniumFamily, PRIM, V, isItaniumFamily)
    case_METHOD_NOARGS(isMicrosoft, PRIM, V, isMicrosoft)
    case_METHOD_NOARGS(areMemberFunctionsAligned, PRIM, V, areMemberFunctionsAligned)
    case_METHOD_NOARGS(areArgsDestroyedLeftToRightInCallee, PRIM, V, areArgsDestroyedLeftToRightInCallee)
    case_METHOD_NOARGS(hasConstructorVariants, PRIM, V, hasConstructorVariants)
    case_METHOD_NOARGS(hasPrimaryVBases, PRIM, V, hasPrimaryVBases)
    case_METHOD_NOARGS(hasKeyFunctions, PRIM, V, hasKeyFunctions)
    case_METHOD_NOARGS(canKeyFunctionBeInline, PRIM, V, canKeyFunctionBeInline)
    case_METHOD_NOARGS(getTailPaddingUseRules, PRIM, V, getTailPaddingUseRules)
    case currefl::operator_eq_eq: {
      TRY_LOAD_VAL(const class clang::TargetCXXABI &, p0 )
      TRY_LOAD_VAL(const class clang::TargetCXXABI &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 == p1)
      TOO_MANY;
    }
    case currefl::operator_not_eq: {
      TRY_LOAD_VAL(const class clang::TargetCXXABI &, p0 )
      TRY_LOAD_VAL(const class clang::TargetCXXABI &, p1 )
      if (no_more) PRIM_REFLECTION_CALL(p0 != p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OpenCLOptions *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__OpenCLOptions;
# define CLASSNAME clang::OpenCLOptions
  switch( (currefl::memnames)N ) {
    case currefl::isKnown: {
      CHECK_TRAIT_IS_REFLPROP(isKnown)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, isKnown, p0)
      TOO_MANY;
    }
    case currefl::isEnabled: {
      CHECK_TRAIT_IS_REFLPROP(isEnabled)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, isEnabled, p0)
      TOO_MANY;
    }
    case currefl::isSupported: {
      CHECK_TRAIT_IS_REFLPROP(isSupported)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(P, isSupported, p0, p1)
      TOO_MANY;
    }
    case currefl::isSupportedCore: {
      CHECK_TRAIT_IS_REFLPROP(isSupportedCore)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(P, isSupportedCore, p0, p1)
      TOO_MANY;
    }
    case currefl::isSupportedExtension: {
      CHECK_TRAIT_IS_REFLPROP(isSupportedExtension)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(P, isSupportedExtension, p0, p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OpenCLOptions X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__OpenCLOptions;
# define CLASSNAME clang::OpenCLOptions
  switch( (currefl::memnames)N ) {
    case currefl::isKnown: {
      CHECK_TRAIT_IS_REFLPROP(isKnown)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, isKnown, p0)
      TOO_MANY;
    }
    case currefl::isEnabled: {
      CHECK_TRAIT_IS_REFLPROP(isEnabled)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(V, isEnabled, p0)
      TOO_MANY;
    }
    case currefl::isSupported: {
      CHECK_TRAIT_IS_REFLPROP(isSupported)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(V, isSupported, p0, p1)
      TOO_MANY;
    }
    case currefl::isSupportedCore: {
      CHECK_TRAIT_IS_REFLPROP(isSupportedCore)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(V, isSupportedCore, p0, p1)
      TOO_MANY;
    }
    case currefl::isSupportedExtension: {
      CHECK_TRAIT_IS_REFLPROP(isSupportedExtension)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(V, isSupportedExtension, p0, p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TargetOptions *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TargetOptions;
# define CLASSNAME clang::TargetOptions
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, P, EABIVersion)
    case_FIELD(CLASS, P, SupportedOpenCLOptions)
    case_FIELD(PRIM, P, ForceEnableInt128)
    case_FIELD(PRIM, P, NVPTXUseShortPointers)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::Type *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__Type;
# define CLASSNAME llvm::Type
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(dump, VOID, P, dump)
    case_METHOD_NOARGS(getTypeID, PRIM, P, getTypeID)
    case_METHOD_NOARGS(isVoidTy, PRIM, P, isVoidTy)
    case_METHOD_NOARGS(isHalfTy, PRIM, P, isHalfTy)
    case_METHOD_NOARGS(isFloatTy, PRIM, P, isFloatTy)
    case_METHOD_NOARGS(isDoubleTy, PRIM, P, isDoubleTy)
    case_METHOD_NOARGS(isX86_FP80Ty, PRIM, P, isX86_FP80Ty)
    case_METHOD_NOARGS(isFP128Ty, PRIM, P, isFP128Ty)
    case_METHOD_NOARGS(isPPC_FP128Ty, PRIM, P, isPPC_FP128Ty)
    case_METHOD_NOARGS(isFloatingPointTy, PRIM, P, isFloatingPointTy)
    case_METHOD_NOARGS(isX86_MMXTy, PRIM, P, isX86_MMXTy)
    case_METHOD_NOARGS(isFPOrFPVectorTy, PRIM, P, isFPOrFPVectorTy)
    case_METHOD_NOARGS(isLabelTy, PRIM, P, isLabelTy)
    case_METHOD_NOARGS(isMetadataTy, PRIM, P, isMetadataTy)
    case_METHOD_NOARGS(isTokenTy, PRIM, P, isTokenTy)
    case_METHOD_NOARGS(isIntegerTy, PRIM, P, isIntegerTy)
    case currefl::isIntegerTy1: {
      CHECK_TRAIT_IS_REFLPROP(isIntegerTy1)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isIntegerTy, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isIntOrIntVectorTy, PRIM, P, isIntOrIntVectorTy)
    case currefl::isIntOrIntVectorTy1: {
      CHECK_TRAIT_IS_REFLPROP(isIntOrIntVectorTy1)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, isIntOrIntVectorTy, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isIntOrPtrTy, PRIM, P, isIntOrPtrTy)
    case_METHOD_NOARGS(isFunctionTy, PRIM, P, isFunctionTy)
    case_METHOD_NOARGS(isStructTy, PRIM, P, isStructTy)
    case_METHOD_NOARGS(isArrayTy, PRIM, P, isArrayTy)
    case_METHOD_NOARGS(isPointerTy, PRIM, P, isPointerTy)
    case_METHOD_NOARGS(isPtrOrPtrVectorTy, PRIM, P, isPtrOrPtrVectorTy)
    case_METHOD_NOARGS(isVectorTy, PRIM, P, isVectorTy)
    case currefl::canLosslesslyBitCastTo: {
      CHECK_TRAIT_IS_REFLPROP(canLosslesslyBitCastTo)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, canLosslesslyBitCastTo, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isEmptyTy, PRIM, P, isEmptyTy)
    case_METHOD_NOARGS(isFirstClassType, PRIM, P, isFirstClassType)
    case_METHOD_NOARGS(isSingleValueType, PRIM, P, isSingleValueType)
    case_METHOD_NOARGS(isAggregateType, PRIM, P, isAggregateType)
    case_METHOD_NOARGS(getPrimitiveSizeInBits, PRIM, P, getPrimitiveSizeInBits)
    case_METHOD_NOARGS(getScalarSizeInBits, PRIM, P, getScalarSizeInBits)
    case_METHOD_NOARGS(getFPMantissaWidth, PRIM, P, getFPMantissaWidth)
    case_METHOD_NOARGS(getScalarType, CLASS, P, getScalarType)
    case currefl::subtypes: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::subtypes))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, subtypes)
      TOO_MANY;
    }
    case currefl::getContainedType: {
      CHECK_TRAIT_IS_REFLPROP(getContainedType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getContainedType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumContainedTypes, PRIM, P, getNumContainedTypes)
    case_METHOD_NOARGS(getIntegerBitWidth, PRIM, P, getIntegerBitWidth)
    case currefl::getFunctionParamType: {
      CHECK_TRAIT_IS_REFLPROP(getFunctionParamType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getFunctionParamType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getFunctionNumParams, PRIM, P, getFunctionNumParams)
    case_METHOD_NOARGS(isFunctionVarArg, PRIM, P, isFunctionVarArg)
    case_METHOD_NOARGS(getStructName, CLASS, P, getStructName)
    case_METHOD_NOARGS(getStructNumElements, PRIM, P, getStructNumElements)
    case currefl::getStructElementType: {
      CHECK_TRAIT_IS_REFLPROP(getStructElementType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getStructElementType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getSequentialElementType, CLASS, P, getSequentialElementType)
    case_METHOD_NOARGS(getArrayNumElements, PRIM, P, getArrayNumElements)
    case_METHOD_NOARGS(getArrayElementType, CLASS, P, getArrayElementType)
    case_METHOD_NOARGS(getVectorNumElements, PRIM, P, getVectorNumElements)
    case_METHOD_NOARGS(getVectorElementType, CLASS, P, getVectorElementType)
    case_METHOD_NOARGS(getPointerElementType, CLASS, P, getPointerElementType)
    case_METHOD_NOARGS(getPointerAddressSpace, PRIM, P, getPointerAddressSpace)
    case currefl::getPointerTo: {
      CHECK_TRAIT_IS_REFLPROP(getPointerTo)
      if (no_more) CLASS_REFLECTION_0(P, getPointerTo)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getPointerTo, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::CompositeType *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__CompositeType;
# define CLASSNAME llvm::CompositeType
  switch( (currefl::memnames)N ) {
    case currefl::getTypeAtIndex: {
      CHECK_TRAIT_IS_REFLPROP(getTypeAtIndex)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeAtIndex, p0)
      TOO_MANY;
    }
    case currefl::indexValid: {
      CHECK_TRAIT_IS_REFLPROP(indexValid)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, indexValid, p0)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::StructType *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__StructType;
# define CLASSNAME llvm::StructType
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isPacked, PRIM, P, isPacked)
    case_METHOD_NOARGS(isLiteral, PRIM, P, isLiteral)
    case_METHOD_NOARGS(isOpaque, PRIM, P, isOpaque)
    case_METHOD_NOARGS(hasName, PRIM, P, hasName)
    case_METHOD_NOARGS(getName, CLASS, P, getName)
    case currefl::isValidElementType: {
      CHECK_TRAIT_IS_REFLPROP(isValidElementType)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, isValidElementType, p0)
      TOO_MANY;
    }
    case currefl::elements: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::elements))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, elements)
      TOO_MANY;
    }
    case currefl::isLayoutIdentical: {
      CHECK_TRAIT_IS_REFLPROP(isLayoutIdentical)
      TRY_LOAD_VAL(class llvm::StructType *, p0)
      if (no_more) PRIM_REFLECTION(P, isLayoutIdentical, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumElements, PRIM, P, getNumElements)
    case currefl::getElementType: {
      CHECK_TRAIT_IS_REFLPROP(getElementType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getElementType, p0)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::PointerType *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__PointerType;
# define CLASSNAME llvm::PointerType
  switch( (currefl::memnames)N ) {
    case currefl::get: {
      CHECK_TRAIT_IS_REFLPROP(get)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(S, get, p0, p1)
      TOO_MANY;
    }
    case currefl::getUnqual: {
      CHECK_TRAIT_IS_REFLPROP(getUnqual)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) CLASS_REFLECTION(S, getUnqual, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getElementType, CLASS, P, getElementType)
    case currefl::isValidElementType: {
      CHECK_TRAIT_IS_REFLPROP(isValidElementType)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, isValidElementType, p0)
      TOO_MANY;
    }
    case currefl::isLoadableOrStorableType: {
      CHECK_TRAIT_IS_REFLPROP(isLoadableOrStorableType)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, isLoadableOrStorableType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAddressSpace, PRIM, P, getAddressSpace)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::DataLayout *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__DataLayout;
# define CLASSNAME llvm::DataLayout
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isLittleEndian, PRIM, P, isLittleEndian)
    case_METHOD_NOARGS(isBigEndian, PRIM, P, isBigEndian)
    case_METHOD_NOARGS(isDefault, PRIM, P, isDefault)
    case currefl::isLegalInteger: {
      CHECK_TRAIT_IS_REFLPROP(isLegalInteger)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, isLegalInteger, p0)
      TOO_MANY;
    }
    case currefl::isIllegalInteger: {
      CHECK_TRAIT_IS_REFLPROP(isIllegalInteger)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, isIllegalInteger, p0)
      TOO_MANY;
    }
    case currefl::exceedsNaturalStackAlignment: {
      CHECK_TRAIT_IS_REFLPROP(exceedsNaturalStackAlignment)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, exceedsNaturalStackAlignment, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getStackAlignment, PRIM, P, getStackAlignment)
    case_METHOD_NOARGS(getAllocaAddrSpace, PRIM, P, getAllocaAddrSpace)
    case_METHOD_NOARGS(getProgramAddressSpace, PRIM, P, getProgramAddressSpace)
    case_METHOD_NOARGS(hasMicrosoftFastStdCallMangling, PRIM, P, hasMicrosoftFastStdCallMangling)
    case_METHOD_NOARGS(doNotMangleLeadingQuestionMark, PRIM, P, doNotMangleLeadingQuestionMark)
    case_METHOD_NOARGS(hasLinkerPrivateGlobalPrefix, PRIM, P, hasLinkerPrivateGlobalPrefix)
    case_METHOD_NOARGS(getLinkerPrivateGlobalPrefix, CLASS, P, getLinkerPrivateGlobalPrefix)
    case_METHOD_NOARGS(getGlobalPrefix, PRIM, P, getGlobalPrefix)
    case_METHOD_NOARGS(getPrivateGlobalPrefix, CLASS, P, getPrivateGlobalPrefix)
    case currefl::fitsInLegalInteger: {
      CHECK_TRAIT_IS_REFLPROP(fitsInLegalInteger)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, fitsInLegalInteger, p0)
      TOO_MANY;
    }
    case currefl::getPointerABIAlignment: {
      CHECK_TRAIT_IS_REFLPROP(getPointerABIAlignment)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getPointerABIAlignment, p0)
      TOO_MANY;
    }
    case currefl::getPointerPrefAlignment: {
      CHECK_TRAIT_IS_REFLPROP(getPointerPrefAlignment)
      if (no_more) PRIM_REFLECTION_0(P, getPointerPrefAlignment)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getPointerPrefAlignment, p0)
      TOO_MANY;
    }
    case currefl::getPointerSize: {
      CHECK_TRAIT_IS_REFLPROP(getPointerSize)
      if (no_more) PRIM_REFLECTION_0(P, getPointerSize)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getPointerSize, p0)
      TOO_MANY;
    }
    case currefl::getIndexSize: {
      CHECK_TRAIT_IS_REFLPROP(getIndexSize)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getIndexSize, p0)
      TOO_MANY;
    }
    case currefl::getNonIntegralAddressSpaces: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getNonIntegralAddressSpaces))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getNonIntegralAddressSpaces)
      TOO_MANY;
    }
    case currefl::isNonIntegralPointerType: {
      CHECK_TRAIT_IS_REFLPROP(isNonIntegralPointerType)
      TRY_LOAD_VAL(class llvm::PointerType *, p0)
      if (no_more) PRIM_REFLECTION(P, isNonIntegralPointerType, p0)
      TOO_MANY;
    }
    case currefl::isNonIntegralPointerType1: {
      CHECK_TRAIT_IS_REFLPROP(isNonIntegralPointerType1)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, isNonIntegralPointerType, p0)
      TOO_MANY;
    }
    case currefl::getPointerSizeInBits: {
      CHECK_TRAIT_IS_REFLPROP(getPointerSizeInBits)
      if (no_more) PRIM_REFLECTION_0(P, getPointerSizeInBits)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getPointerSizeInBits, p0)
      TOO_MANY;
    }
    case currefl::getIndexSizeInBits: {
      CHECK_TRAIT_IS_REFLPROP(getIndexSizeInBits)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getIndexSizeInBits, p0)
      TOO_MANY;
    }
    case currefl::getPointerTypeSizeInBits: {
      CHECK_TRAIT_IS_REFLPROP(getPointerTypeSizeInBits)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getPointerTypeSizeInBits, p0)
      TOO_MANY;
    }
    case currefl::getIndexTypeSizeInBits: {
      CHECK_TRAIT_IS_REFLPROP(getIndexTypeSizeInBits)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getIndexTypeSizeInBits, p0)
      TOO_MANY;
    }
    case currefl::getPointerTypeSize: {
      CHECK_TRAIT_IS_REFLPROP(getPointerTypeSize)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getPointerTypeSize, p0)
      TOO_MANY;
    }
    case currefl::getTypeSizeInBits: {
      CHECK_TRAIT_IS_REFLPROP(getTypeSizeInBits)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeSizeInBits, p0)
      TOO_MANY;
    }
    case currefl::getTypeStoreSize: {
      CHECK_TRAIT_IS_REFLPROP(getTypeStoreSize)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeStoreSize, p0)
      TOO_MANY;
    }
    case currefl::getTypeStoreSizeInBits: {
      CHECK_TRAIT_IS_REFLPROP(getTypeStoreSizeInBits)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeStoreSizeInBits, p0)
      TOO_MANY;
    }
    case currefl::getTypeAllocSize: {
      CHECK_TRAIT_IS_REFLPROP(getTypeAllocSize)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeAllocSize, p0)
      TOO_MANY;
    }
    case currefl::getTypeAllocSizeInBits: {
      CHECK_TRAIT_IS_REFLPROP(getTypeAllocSizeInBits)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeAllocSizeInBits, p0)
      TOO_MANY;
    }
    case currefl::getABITypeAlignment: {
      CHECK_TRAIT_IS_REFLPROP(getABITypeAlignment)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getABITypeAlignment, p0)
      TOO_MANY;
    }
    case currefl::getABIIntegerTypeAlignment: {
      CHECK_TRAIT_IS_REFLPROP(getABIIntegerTypeAlignment)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getABIIntegerTypeAlignment, p0)
      TOO_MANY;
    }
    case currefl::getPrefTypeAlignment: {
      CHECK_TRAIT_IS_REFLPROP(getPrefTypeAlignment)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getPrefTypeAlignment, p0)
      TOO_MANY;
    }
    case currefl::getPreferredTypeAlignmentShift: {
      CHECK_TRAIT_IS_REFLPROP(getPreferredTypeAlignmentShift)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getPreferredTypeAlignmentShift, p0)
      TOO_MANY;
    }
    case currefl::getIntPtrType: {
      CHECK_TRAIT_IS_REFLPROP(getIntPtrType)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) CLASS_REFLECTION(P, getIntPtrType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLargestLegalIntTypeSizeInBits, PRIM, P, getLargestLegalIntTypeSizeInBits)
    case currefl::getIndexType: {
      CHECK_TRAIT_IS_REFLPROP(getIndexType)
      TRY_LOAD_VAL(class llvm::Type *, p0)
      if (no_more) CLASS_REFLECTION(P, getIndexType, p0)
      TOO_MANY;
    }
    case currefl::getStructLayout: {
      CHECK_TRAIT_IS_REFLPROP(getStructLayout)
      TRY_LOAD_VAL(class llvm::StructType *, p0)
      if (no_more) CLASS_REFLECTION(P, getStructLayout, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , llvm::StructLayout *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::llvm__StructLayout;
# define CLASSNAME llvm::StructLayout
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSizeInBytes, PRIM, P, getSizeInBytes)
    case_METHOD_NOARGS(getSizeInBits, PRIM, P, getSizeInBits)
    case_METHOD_NOARGS(getAlignment, PRIM, P, getAlignment)
    case_METHOD_NOARGS(hasPadding, PRIM, P, hasPadding)
    case currefl::getElementContainingOffset: {
      CHECK_TRAIT_IS_REFLPROP(getElementContainingOffset)
      TRY_LOAD_VAL(unsigned long long, p0)
      if (no_more) PRIM_REFLECTION(P, getElementContainingOffset, p0)
      TOO_MANY;
    }
    case currefl::getElementOffset: {
      CHECK_TRAIT_IS_REFLPROP(getElementOffset)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getElementOffset, p0)
      TOO_MANY;
    }
    case currefl::getElementOffsetInBits: {
      CHECK_TRAIT_IS_REFLPROP(getElementOffsetInBits)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getElementOffsetInBits, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TargetInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TargetInfo;
# define CLASSNAME clang::TargetInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTargetOpts, CLASS, P, getTargetOpts)
    case_METHOD_NOARGS(getSizeType, PRIM, P, getSizeType)
    case_METHOD_NOARGS(getSignedSizeType, PRIM, P, getSignedSizeType)
    case_METHOD_NOARGS(getIntMaxType, PRIM, P, getIntMaxType)
    case_METHOD_NOARGS(getUIntMaxType, PRIM, P, getUIntMaxType)
    case currefl::getPtrDiffType: {
      CHECK_TRAIT_IS_REFLPROP(getPtrDiffType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getPtrDiffType, p0)
      TOO_MANY;
    }
    case currefl::getUnsignedPtrDiffType: {
      CHECK_TRAIT_IS_REFLPROP(getUnsignedPtrDiffType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getUnsignedPtrDiffType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getIntPtrType, PRIM, P, getIntPtrType)
    case_METHOD_NOARGS(getUIntPtrType, PRIM, P, getUIntPtrType)
    case_METHOD_NOARGS(getWCharType, PRIM, P, getWCharType)
    case_METHOD_NOARGS(getWIntType, PRIM, P, getWIntType)
    case_METHOD_NOARGS(getChar16Type, PRIM, P, getChar16Type)
    case_METHOD_NOARGS(getChar32Type, PRIM, P, getChar32Type)
    case_METHOD_NOARGS(getInt64Type, PRIM, P, getInt64Type)
    case_METHOD_NOARGS(getUInt64Type, PRIM, P, getUInt64Type)
    case_METHOD_NOARGS(getSigAtomicType, PRIM, P, getSigAtomicType)
    case_METHOD_NOARGS(getProcessIDType, PRIM, P, getProcessIDType)
    case currefl::getCorrespondingUnsignedType: {
      CHECK_TRAIT_IS_REFLPROP(getCorrespondingUnsignedType)
      TRY_LOAD_VAL(enum clang::TargetInfo::IntType, p0)
      if (no_more) PRIM_REFLECTION(S, getCorrespondingUnsignedType, p0)
      TOO_MANY;
    }
    case currefl::getTypeWidth: {
      CHECK_TRAIT_IS_REFLPROP(getTypeWidth)
      TRY_LOAD_VAL(enum clang::TargetInfo::IntType, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeWidth, p0)
      TOO_MANY;
    }
    case currefl::getIntTypeByWidth: {
      CHECK_TRAIT_IS_REFLPROP(getIntTypeByWidth)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) PRIM_REFLECTION(P, getIntTypeByWidth, p0, p1)
      TOO_MANY;
    }
    case currefl::getLeastIntTypeByWidth: {
      CHECK_TRAIT_IS_REFLPROP(getLeastIntTypeByWidth)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) PRIM_REFLECTION(P, getLeastIntTypeByWidth, p0, p1)
      TOO_MANY;
    }
    case currefl::getRealTypeByWidth: {
      CHECK_TRAIT_IS_REFLPROP(getRealTypeByWidth)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getRealTypeByWidth, p0)
      TOO_MANY;
    }
    case currefl::getTypeAlign: {
      CHECK_TRAIT_IS_REFLPROP(getTypeAlign)
      TRY_LOAD_VAL(enum clang::TargetInfo::IntType, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeAlign, p0)
      TOO_MANY;
    }
    case currefl::isTypeSigned: {
      CHECK_TRAIT_IS_REFLPROP(isTypeSigned)
      TRY_LOAD_VAL(enum clang::TargetInfo::IntType, p0)
      if (no_more) PRIM_REFLECTION(S, isTypeSigned, p0)
      TOO_MANY;
    }
    case currefl::getPointerWidth: {
      CHECK_TRAIT_IS_REFLPROP(getPointerWidth)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getPointerWidth, p0)
      TOO_MANY;
    }
    case currefl::getPointerAlign: {
      CHECK_TRAIT_IS_REFLPROP(getPointerAlign)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getPointerAlign, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getMaxPointerWidth, PRIM, P, getMaxPointerWidth)
    case currefl::getNullPointerValue: {
      CHECK_TRAIT_IS_REFLPROP(getNullPointerValue)
      TRY_LOAD_VAL(enum clang::LangAS, p0)
      if (no_more) PRIM_REFLECTION(P, getNullPointerValue, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getBoolWidth, PRIM, P, getBoolWidth)
    case_METHOD_NOARGS(getBoolAlign, PRIM, P, getBoolAlign)
    case_METHOD_NOARGS(getCharWidth, PRIM, P, getCharWidth)
    case_METHOD_NOARGS(getCharAlign, PRIM, P, getCharAlign)
    case_METHOD_NOARGS(getShortWidth, PRIM, P, getShortWidth)
    case_METHOD_NOARGS(getShortAlign, PRIM, P, getShortAlign)
    case_METHOD_NOARGS(getIntWidth, PRIM, P, getIntWidth)
    case_METHOD_NOARGS(getIntAlign, PRIM, P, getIntAlign)
    case_METHOD_NOARGS(getLongWidth, PRIM, P, getLongWidth)
    case_METHOD_NOARGS(getLongAlign, PRIM, P, getLongAlign)
    case_METHOD_NOARGS(getLongLongWidth, PRIM, P, getLongLongWidth)
    case_METHOD_NOARGS(getLongLongAlign, PRIM, P, getLongLongAlign)
    case_METHOD_NOARGS(getShortAccumWidth, PRIM, P, getShortAccumWidth)
    case_METHOD_NOARGS(getShortAccumAlign, PRIM, P, getShortAccumAlign)
    case_METHOD_NOARGS(getAccumWidth, PRIM, P, getAccumWidth)
    case_METHOD_NOARGS(getAccumAlign, PRIM, P, getAccumAlign)
    case_METHOD_NOARGS(getLongAccumWidth, PRIM, P, getLongAccumWidth)
    case_METHOD_NOARGS(getLongAccumAlign, PRIM, P, getLongAccumAlign)
    case_METHOD_NOARGS(getShortFractWidth, PRIM, P, getShortFractWidth)
    case_METHOD_NOARGS(getShortFractAlign, PRIM, P, getShortFractAlign)
    case_METHOD_NOARGS(getFractWidth, PRIM, P, getFractWidth)
    case_METHOD_NOARGS(getFractAlign, PRIM, P, getFractAlign)
    case_METHOD_NOARGS(getLongFractWidth, PRIM, P, getLongFractWidth)
    case_METHOD_NOARGS(getLongFractAlign, PRIM, P, getLongFractAlign)
    case_METHOD_NOARGS(getShortAccumScale, PRIM, P, getShortAccumScale)
    case_METHOD_NOARGS(getShortAccumIBits, PRIM, P, getShortAccumIBits)
    case_METHOD_NOARGS(getAccumScale, PRIM, P, getAccumScale)
    case_METHOD_NOARGS(getAccumIBits, PRIM, P, getAccumIBits)
    case_METHOD_NOARGS(getLongAccumScale, PRIM, P, getLongAccumScale)
    case_METHOD_NOARGS(getLongAccumIBits, PRIM, P, getLongAccumIBits)
    case_METHOD_NOARGS(getUnsignedShortAccumScale, PRIM, P, getUnsignedShortAccumScale)
    case_METHOD_NOARGS(getUnsignedShortAccumIBits, PRIM, P, getUnsignedShortAccumIBits)
    case_METHOD_NOARGS(getUnsignedAccumScale, PRIM, P, getUnsignedAccumScale)
    case_METHOD_NOARGS(getUnsignedAccumIBits, PRIM, P, getUnsignedAccumIBits)
    case_METHOD_NOARGS(getUnsignedLongAccumScale, PRIM, P, getUnsignedLongAccumScale)
    case_METHOD_NOARGS(getUnsignedLongAccumIBits, PRIM, P, getUnsignedLongAccumIBits)
    case_METHOD_NOARGS(getShortFractScale, PRIM, P, getShortFractScale)
    case_METHOD_NOARGS(getFractScale, PRIM, P, getFractScale)
    case_METHOD_NOARGS(getLongFractScale, PRIM, P, getLongFractScale)
    case_METHOD_NOARGS(getUnsignedShortFractScale, PRIM, P, getUnsignedShortFractScale)
    case_METHOD_NOARGS(getUnsignedFractScale, PRIM, P, getUnsignedFractScale)
    case_METHOD_NOARGS(getUnsignedLongFractScale, PRIM, P, getUnsignedLongFractScale)
    case_METHOD_NOARGS(hasInt128Type, PRIM, P, hasInt128Type)
    case_METHOD_NOARGS(hasLegalHalfType, PRIM, P, hasLegalHalfType)
    case_METHOD_NOARGS(hasFloat128Type, PRIM, P, hasFloat128Type)
    case_METHOD_NOARGS(getSuitableAlign, PRIM, P, getSuitableAlign)
    case_METHOD_NOARGS(getDefaultAlignForAttributeAligned, PRIM, P, getDefaultAlignForAttributeAligned)
    case_METHOD_NOARGS(getMinGlobalAlign, PRIM, P, getMinGlobalAlign)
    case_METHOD_NOARGS(getNewAlign, PRIM, P, getNewAlign)
    case_METHOD_NOARGS(getWCharWidth, PRIM, P, getWCharWidth)
    case_METHOD_NOARGS(getWCharAlign, PRIM, P, getWCharAlign)
    case_METHOD_NOARGS(getChar16Width, PRIM, P, getChar16Width)
    case_METHOD_NOARGS(getChar16Align, PRIM, P, getChar16Align)
    case_METHOD_NOARGS(getChar32Width, PRIM, P, getChar32Width)
    case_METHOD_NOARGS(getChar32Align, PRIM, P, getChar32Align)
    case_METHOD_NOARGS(getHalfWidth, PRIM, P, getHalfWidth)
    case_METHOD_NOARGS(getHalfAlign, PRIM, P, getHalfAlign)
    case_METHOD_NOARGS(getFloatWidth, PRIM, P, getFloatWidth)
    case_METHOD_NOARGS(getFloatAlign, PRIM, P, getFloatAlign)
    case_METHOD_NOARGS(getDoubleWidth, PRIM, P, getDoubleWidth)
    case_METHOD_NOARGS(getDoubleAlign, PRIM, P, getDoubleAlign)
    case_METHOD_NOARGS(getLongDoubleWidth, PRIM, P, getLongDoubleWidth)
    case_METHOD_NOARGS(getLongDoubleAlign, PRIM, P, getLongDoubleAlign)
    case_METHOD_NOARGS(getFloat128Width, PRIM, P, getFloat128Width)
    case_METHOD_NOARGS(getFloat128Align, PRIM, P, getFloat128Align)
    case_METHOD_NOARGS(useFloat128ManglingForLongDouble, PRIM, P, useFloat128ManglingForLongDouble)
    case_METHOD_NOARGS(getFloatEvalMethod, PRIM, P, getFloatEvalMethod)
    case_METHOD_NOARGS(getLargeArrayMinWidth, PRIM, P, getLargeArrayMinWidth)
    case_METHOD_NOARGS(getLargeArrayAlign, PRIM, P, getLargeArrayAlign)
    case_METHOD_NOARGS(getMaxAtomicPromoteWidth, PRIM, P, getMaxAtomicPromoteWidth)
    case_METHOD_NOARGS(getMaxAtomicInlineWidth, PRIM, P, getMaxAtomicInlineWidth)
    case currefl::hasBuiltinAtomic: {
      CHECK_TRAIT_IS_REFLPROP(hasBuiltinAtomic)
      TRY_LOAD_VAL(unsigned long long, p0)
      TRY_LOAD_VAL(unsigned long long, p1)
      if (no_more) PRIM_REFLECTION(P, hasBuiltinAtomic, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getMaxVectorAlign, PRIM, P, getMaxVectorAlign)
    case_METHOD_NOARGS(getSimdDefaultAlign, PRIM, P, getSimdDefaultAlign)
    case_METHOD_NOARGS(getIntMaxTWidth, PRIM, P, getIntMaxTWidth)
    case_METHOD_NOARGS(getUnwindWordWidth, PRIM, P, getUnwindWordWidth)
    case_METHOD_NOARGS(getRegisterWidth, PRIM, P, getRegisterWidth)
    case_METHOD_NOARGS(getMCountName, PRIM, P, getMCountName)
    case_METHOD_NOARGS(useSignedCharForObjCBool, PRIM, P, useSignedCharForObjCBool)
    case_METHOD_NOARGS(useBitFieldTypeAlignment, PRIM, P, useBitFieldTypeAlignment)
    case_METHOD_NOARGS(useZeroLengthBitfieldAlignment, PRIM, P, useZeroLengthBitfieldAlignment)
    case_METHOD_NOARGS(getZeroLengthBitfieldBoundary, PRIM, P, getZeroLengthBitfieldBoundary)
    case_METHOD_NOARGS(useExplicitBitFieldAlignment, PRIM, P, useExplicitBitFieldAlignment)
    case_METHOD_NOARGS(hasAlignMac68kSupport, PRIM, P, hasAlignMac68kSupport)
    case currefl::getTypeName: {
      CHECK_TRAIT_IS_REFLPROP(getTypeName)
      TRY_LOAD_VAL(enum clang::TargetInfo::IntType, p0)
      if (no_more) PRIM_REFLECTION(S, getTypeName, p0)
      TOO_MANY;
    }
    case currefl::getTypeConstantSuffix: {
      CHECK_TRAIT_IS_REFLPROP(getTypeConstantSuffix)
      TRY_LOAD_VAL(enum clang::TargetInfo::IntType, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeConstantSuffix, p0)
      TOO_MANY;
    }
    case currefl::getTypeFormatModifier: {
      CHECK_TRAIT_IS_REFLPROP(getTypeFormatModifier)
      TRY_LOAD_VAL(enum clang::TargetInfo::IntType, p0)
      if (no_more) PRIM_REFLECTION(S, getTypeFormatModifier, p0)
      TOO_MANY;
    }
    case currefl::useObjCFPRetForRealType: {
      CHECK_TRAIT_IS_REFLPROP(useObjCFPRetForRealType)
      TRY_LOAD_VAL(enum clang::TargetInfo::RealType, p0)
      if (no_more) PRIM_REFLECTION(P, useObjCFPRetForRealType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(useObjCFP2RetForComplexLongDouble, PRIM, P, useObjCFP2RetForComplexLongDouble)
    case_METHOD_NOARGS(useFP16ConversionIntrinsics, PRIM, P, useFP16ConversionIntrinsics)
    case_METHOD_NOARGS(useAddressSpaceMapMangling, PRIM, P, useAddressSpaceMapMangling)
    case_METHOD_NOARGS(isCLZForZeroUndef, PRIM, P, isCLZForZeroUndef)
    case_METHOD_NOARGS(getBuiltinVaListKind, PRIM, P, getBuiltinVaListKind)
    case_METHOD_NOARGS(hasBuiltinMSVaList, PRIM, P, hasBuiltinMSVaList)
    case_METHOD_NOARGS(isRenderScriptTarget, PRIM, P, isRenderScriptTarget)
    case currefl::isValidClobber: {
      CHECK_TRAIT_IS_REFLPROP(isValidClobber)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, isValidClobber, p0)
      TOO_MANY;
    }
    case currefl::isValidGCCRegisterName: {
      CHECK_TRAIT_IS_REFLPROP(isValidGCCRegisterName)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, isValidGCCRegisterName, p0)
      TOO_MANY;
    }
    case currefl::getNormalizedGCCRegisterName: {
      CHECK_TRAIT_IS_REFLPROP(getNormalizedGCCRegisterName)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(P, getNormalizedGCCRegisterName, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, getNormalizedGCCRegisterName, p0, p1)
      TOO_MANY;
    }
    case currefl::getConstraintRegister: {
      CHECK_TRAIT_IS_REFLPROP(getConstraintRegister)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      TRY_LOAD_VAL(class llvm::StringRef, p1)
      if (no_more) CLASS_REFLECTION(P, getConstraintRegister, p0, p1)
      TOO_MANY;
    }
    case currefl::validateOutputSize: {
      CHECK_TRAIT_IS_REFLPROP(validateOutputSize)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(P, validateOutputSize, p0, p1)
      TOO_MANY;
    }
    case currefl::validateInputSize: {
      CHECK_TRAIT_IS_REFLPROP(validateInputSize)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(P, validateInputSize, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getClobbers, PRIM, P, getClobbers)
    case_METHOD_NOARGS(isNan2008, PRIM, P, isNan2008)
    case_METHOD_NOARGS(getTriple, CLASS, P, getTriple)
    case_METHOD_NOARGS(getDataLayout, CLASS, P, getDataLayout)
    case_METHOD_NOARGS(hasProtectedVisibility, PRIM, P, hasProtectedVisibility)
    case currefl::isValidSectionSpecifier: {
      CHECK_TRAIT_IS_REFLPROP(isValidSectionSpecifier)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, isValidSectionSpecifier, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getABI, CLASS, P, getABI)
    case_METHOD_NOARGS(getCXXABI, CLASS, P, getCXXABI)
    case currefl::isValidCPUName: {
      CHECK_TRAIT_IS_REFLPROP(isValidCPUName)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, isValidCPUName, p0)
      TOO_MANY;
    }
    case currefl::isValidFeatureName: {
      CHECK_TRAIT_IS_REFLPROP(isValidFeatureName)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, isValidFeatureName, p0)
      TOO_MANY;
    }
    case currefl::hasFeature: {
      CHECK_TRAIT_IS_REFLPROP(hasFeature)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, hasFeature, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(supportsMultiVersioning, PRIM, P, supportsMultiVersioning)
    case currefl::validateCpuSupports: {
      CHECK_TRAIT_IS_REFLPROP(validateCpuSupports)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, validateCpuSupports, p0)
      TOO_MANY;
    }
    case currefl::multiVersionSortPriority: {
      CHECK_TRAIT_IS_REFLPROP(multiVersionSortPriority)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, multiVersionSortPriority, p0)
      TOO_MANY;
    }
    case currefl::validateCpuIs: {
      CHECK_TRAIT_IS_REFLPROP(validateCpuIs)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, validateCpuIs, p0)
      TOO_MANY;
    }
    case currefl::validateCPUSpecificCPUDispatch: {
      CHECK_TRAIT_IS_REFLPROP(validateCPUSpecificCPUDispatch)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, validateCPUSpecificCPUDispatch, p0)
      TOO_MANY;
    }
    case currefl::CPUSpecificManglingCharacter: {
      CHECK_TRAIT_IS_REFLPROP(CPUSpecificManglingCharacter)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, CPUSpecificManglingCharacter, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getRegParmMax, PRIM, P, getRegParmMax)
    case_METHOD_NOARGS(isTLSSupported, PRIM, P, isTLSSupported)
    case_METHOD_NOARGS(getMaxTLSAlign, PRIM, P, getMaxTLSAlign)
    case_METHOD_NOARGS(isVLASupported, PRIM, P, isVLASupported)
    case_METHOD_NOARGS(isSEHTrySupported, PRIM, P, isSEHTrySupported)
    case_METHOD_NOARGS(hasNoAsmVariants, PRIM, P, hasNoAsmVariants)
    case currefl::getEHDataRegisterNumber: {
      CHECK_TRAIT_IS_REFLPROP(getEHDataRegisterNumber)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getEHDataRegisterNumber, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getStaticInitSectionSpecifier, PRIM, P, getStaticInitSectionSpecifier)
    case_METHOD_NOARGS(getPlatformName, CLASS, P, getPlatformName)
    case_METHOD_NOARGS(getPlatformMinVersion, CLASS, P, getPlatformMinVersion)
    case_METHOD_NOARGS(isBigEndian, PRIM, P, isBigEndian)
    case_METHOD_NOARGS(isLittleEndian, PRIM, P, isLittleEndian)
    case currefl::getDefaultCallingConv: {
      CHECK_TRAIT_IS_REFLPROP(getDefaultCallingConv)
      TRY_LOAD_VAL(enum clang::TargetInfo::CallingConvMethodType, p0)
      if (no_more) PRIM_REFLECTION(P, getDefaultCallingConv, p0)
      TOO_MANY;
    }
    case currefl::checkCallingConvention: {
      CHECK_TRAIT_IS_REFLPROP(checkCallingConvention)
      TRY_LOAD_VAL(enum clang::CallingConv, p0)
      if (no_more) PRIM_REFLECTION(P, checkCallingConvention, p0)
      TOO_MANY;
    }
    case currefl::getCallingConvKind: {
      CHECK_TRAIT_IS_REFLPROP(getCallingConvKind)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) PRIM_REFLECTION(P, getCallingConvKind, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasSjLjLowering, PRIM, P, hasSjLjLowering)
    case_METHOD_NOARGS(allowsLargerPreferedTypeAlignment, PRIM, P, allowsLargerPreferedTypeAlignment)
    case_METHOD_NOARGS(getSupportedOpenCLOpts, CLASS, P, getSupportedOpenCLOpts)
    case currefl::getOpenCLTypeAddrSpace: {
      CHECK_TRAIT_IS_REFLPROP(getOpenCLTypeAddrSpace)
      TRY_LOAD_VAL(enum clang::TargetInfo::OpenCLTypeKind, p0)
      if (no_more) PRIM_REFLECTION(P, getOpenCLTypeAddrSpace, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getVtblPtrAddressSpace, PRIM, P, getVtblPtrAddressSpace)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::XRayFunctionFilter *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__XRayFunctionFilter;
# define CLASSNAME clang::XRayFunctionFilter
  switch( (currefl::memnames)N ) {
    case currefl::shouldImbueFunction: {
      CHECK_TRAIT_IS_REFLPROP(shouldImbueFunction)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, shouldImbueFunction, p0)
      TOO_MANY;
    }
    case currefl::shouldImbueFunctionsInFile: {
      CHECK_TRAIT_IS_REFLPROP(shouldImbueFunctionsInFile)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) PRIM_REFLECTION(P, shouldImbueFunctionsInFile, p0)
      TRY_LOAD_VAL(class llvm::StringRef, p1)
      if (no_more) PRIM_REFLECTION(P, shouldImbueFunctionsInFile, p0, p1)
      TOO_MANY;
    }
    case currefl::shouldImbueLocation: {
      CHECK_TRAIT_IS_REFLPROP(shouldImbueLocation)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) PRIM_REFLECTION(P, shouldImbueLocation, p0)
      TRY_LOAD_VAL(class llvm::StringRef, p1)
      if (no_more) PRIM_REFLECTION(P, shouldImbueLocation, p0, p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypeInfo X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TypeInfo;
# define CLASSNAME clang::TypeInfo
  switch( (currefl::memnames)N ) {
    case_FIELD(PRIM, V, Width)
    case_FIELD(PRIM, V, Align)
    case_FIELD(PRIM, V, AlignIsRequired)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ASTContext *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ASTContext;
# define CLASSNAME clang::ASTContext
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, P, Idents)
    case_FIELD(CLASS, P, Selectors)
    case_METHOD_NOARGS(getPrintingPolicy, CLASS, P, getPrintingPolicy)
    case_METHOD_NOARGS(getSourceManager, CLASS, P, getSourceManager)
    case currefl::Allocate: {
      CHECK_TRAIT_IS_REFLPROP(Allocate)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) PRIM_REFLECTION(P, Allocate, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) PRIM_REFLECTION(P, Allocate, p0, p1)
      TOO_MANY;
    }
    case currefl::Deallocate: {
      CHECK_TRAIT_IS_REFLPROP(Deallocate)
      TRY_LOAD_VAL(void *, p0)
      if (no_more) VOID_REFLECTION(P, Deallocate, (void *)p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getASTAllocatedMemory, PRIM, P, getASTAllocatedMemory)
    case_METHOD_NOARGS(getSideTableAllocatedMemory, PRIM, P, getSideTableAllocatedMemory)
    case_METHOD_NOARGS(getTargetInfo, CLASS, P, getTargetInfo)
    case_METHOD_NOARGS(getAuxTargetInfo, CLASS, P, getAuxTargetInfo)
    case currefl::getIntTypeForBitwidth: {
      CHECK_TRAIT_IS_REFLPROP(getIntTypeForBitwidth)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(P, getIntTypeForBitwidth, p0, p1)
      TOO_MANY;
    }
    case currefl::getRealTypeForBitwidth: {
      CHECK_TRAIT_IS_REFLPROP(getRealTypeForBitwidth)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getRealTypeForBitwidth, p0)
      TOO_MANY;
    }
    case currefl::AtomicUsesUnsupportedLibcall: {
      CHECK_TRAIT_IS_REFLPROP(AtomicUsesUnsupportedLibcall)
      TRY_LOAD_VAL(const class clang::AtomicExpr *, p0)
      if (no_more) PRIM_REFLECTION(P, AtomicUsesUnsupportedLibcall, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLangOpts, CLASS, P, getLangOpts)
    case_METHOD_NOARGS(getSanitizerBlacklist, CLASS, P, getSanitizerBlacklist)
    case_METHOD_NOARGS(getXRayFilter, CLASS, P, getXRayFilter)
    case_METHOD_NOARGS(getDiagnostics, CLASS, P, getDiagnostics)
    case currefl::getFullLoc: {
      CHECK_TRAIT_IS_REFLPROP(getFullLoc)
      TRY_LOAD_VAL(class clang::SourceLocation, p0)
      if (no_more) CLASS_REFLECTION(P, getFullLoc, p0)
      TOO_MANY;
    }
    case_FIELD(CLASS, P, Comments)
    case_FIELD(PRIM, P, CommentsLoaded)
    case currefl::getRawCommentForDeclNoCache: {
      CHECK_TRAIT_IS_REFLPROP(getRawCommentForDeclNoCache)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) CLASS_REFLECTION(P, getRawCommentForDeclNoCache, p0)
      TOO_MANY;
    }
    case currefl::getRawCommentForAnyRedecl: {
      CHECK_TRAIT_IS_REFLPROP(getRawCommentForAnyRedecl)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) CLASS_REFLECTION(P, getRawCommentForAnyRedecl, p0)
      TRY_LOAD_VAL(const class clang::Decl **, p1)
      if (no_more) CLASS_REFLECTION(P, getRawCommentForAnyRedecl, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getCommentCommandTraits, CLASS, P, getCommentCommandTraits)
    case currefl::overridden_methods_size: {
      CHECK_TRAIT_IS_REFLPROP(overridden_methods_size)
      TRY_LOAD_VAL(const class clang::CXXMethodDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, overridden_methods_size, p0)
      TOO_MANY;
    }
    case currefl::getNextLocalImport: {
      CHECK_TRAIT_IS_REFLPROP(getNextLocalImport)
      TRY_LOAD_VAL(class clang::ImportDecl *, p0)
      if (no_more) CLASS_REFLECTION(S, getNextLocalImport, p0)
      TOO_MANY;
    }
    case currefl::local_imports: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::local_imports))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, local_imports)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getTranslationUnitDecl, CLASS, P, getTranslationUnitDecl)
    case_METHOD_NOARGS(getExternCContextDecl, CLASS, P, getExternCContextDecl)
    case_METHOD_NOARGS(getMakeIntegerSeqDecl, CLASS, P, getMakeIntegerSeqDecl)
    case_METHOD_NOARGS(getTypePackElementDecl, CLASS, P, getTypePackElementDecl)
    case_FIELD(CLASS, P, AutoDeductTy)
    case_FIELD(CLASS, P, AutoRRefDeductTy)
    case_FIELD(CLASS, P, VaListTagDecl)
    case_METHOD_NOARGS(getExternalSource, CLASS, P, getExternalSource)
    case_METHOD_NOARGS(PrintStats, VOID, P, PrintStats)
    case currefl::buildImplicitRecord: {
      CHECK_TRAIT_IS_REFLPROP(buildImplicitRecord)
      TRY_LOAD_VAL(class llvm::StringRef, p0)
      if (no_more) CLASS_REFLECTION(P, buildImplicitRecord, p0)
      TRY_LOAD_VAL(enum clang::TagTypeKind, p1)
      if (no_more) CLASS_REFLECTION(P, buildImplicitRecord, p0, p1)
      TOO_MANY;
    }
    case currefl::buildImplicitTypedef: {
      CHECK_TRAIT_IS_REFLPROP(buildImplicitTypedef)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class llvm::StringRef, p1)
      if (no_more) CLASS_REFLECTION(P, buildImplicitTypedef, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getInt128Decl, CLASS, P, getInt128Decl)
    case_METHOD_NOARGS(getUInt128Decl, CLASS, P, getUInt128Decl)
    case currefl::getAddrSpaceQualType: {
      CHECK_TRAIT_IS_REFLPROP(getAddrSpaceQualType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(enum clang::LangAS, p1)
      if (no_more) CLASS_REFLECTION(P, getAddrSpaceQualType, p0, p1)
      TOO_MANY;
    }
    case currefl::removeAddrSpaceQualType: {
      CHECK_TRAIT_IS_REFLPROP(removeAddrSpaceQualType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, removeAddrSpaceQualType, p0)
      TOO_MANY;
    }
    case currefl::getObjCGCQualType: {
      CHECK_TRAIT_IS_REFLPROP(getObjCGCQualType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(enum clang::Qualifiers::GC, p1)
      if (no_more) CLASS_REFLECTION(P, getObjCGCQualType, p0, p1)
      TOO_MANY;
    }
    case currefl::getRestrictType: {
      CHECK_TRAIT_IS_REFLPROP(getRestrictType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getRestrictType, p0)
      TOO_MANY;
    }
    case currefl::getVolatileType: {
      CHECK_TRAIT_IS_REFLPROP(getVolatileType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getVolatileType, p0)
      TOO_MANY;
    }
    case currefl::getConstType: {
      CHECK_TRAIT_IS_REFLPROP(getConstType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getConstType, p0)
      TOO_MANY;
    }
    case currefl::getComplexType: {
      CHECK_TRAIT_IS_REFLPROP(getComplexType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getComplexType, p0)
      TOO_MANY;
    }
    case currefl::getPointerType: {
      CHECK_TRAIT_IS_REFLPROP(getPointerType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getPointerType, p0)
      TOO_MANY;
    }
    case currefl::getAdjustedType: {
      CHECK_TRAIT_IS_REFLPROP(getAdjustedType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) CLASS_REFLECTION(P, getAdjustedType, p0, p1)
      TOO_MANY;
    }
    case currefl::getDecayedType: {
      CHECK_TRAIT_IS_REFLPROP(getDecayedType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getDecayedType, p0)
      TOO_MANY;
    }
    case currefl::getAtomicType: {
      CHECK_TRAIT_IS_REFLPROP(getAtomicType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getAtomicType, p0)
      TOO_MANY;
    }
    case currefl::getBlockPointerType: {
      CHECK_TRAIT_IS_REFLPROP(getBlockPointerType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getBlockPointerType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getBlockDescriptorType, CLASS, P, getBlockDescriptorType)
    case currefl::getReadPipeType: {
      CHECK_TRAIT_IS_REFLPROP(getReadPipeType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getReadPipeType, p0)
      TOO_MANY;
    }
    case currefl::getWritePipeType: {
      CHECK_TRAIT_IS_REFLPROP(getWritePipeType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getWritePipeType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getBlockDescriptorExtendedType, CLASS, P, getBlockDescriptorExtendedType)
    case currefl::getOpenCLTypeKind: {
      CHECK_TRAIT_IS_REFLPROP(getOpenCLTypeKind)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getOpenCLTypeKind, p0)
      TOO_MANY;
    }
    case currefl::getOpenCLTypeAddrSpace: {
      CHECK_TRAIT_IS_REFLPROP(getOpenCLTypeAddrSpace)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getOpenCLTypeAddrSpace, p0)
      TOO_MANY;
    }
    case currefl::getLValueReferenceType: {
      CHECK_TRAIT_IS_REFLPROP(getLValueReferenceType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getLValueReferenceType, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, getLValueReferenceType, p0, p1)
      TOO_MANY;
    }
    case currefl::getRValueReferenceType: {
      CHECK_TRAIT_IS_REFLPROP(getRValueReferenceType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getRValueReferenceType, p0)
      TOO_MANY;
    }
    case currefl::getMemberPointerType: {
      CHECK_TRAIT_IS_REFLPROP(getMemberPointerType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(const class clang::Type *, p1)
      if (no_more) CLASS_REFLECTION(P, getMemberPointerType, p0, p1)
      TOO_MANY;
    }
    case currefl::getVariableArrayType: {
      CHECK_TRAIT_IS_REFLPROP(getVariableArrayType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::Expr *, p1)
      TRY_LOAD_VAL(enum clang::ArrayType::ArraySizeModifier, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      TRY_LOAD_VAL(class clang::SourceRange, p4)
      if (no_more) CLASS_REFLECTION(P, getVariableArrayType, p0, p1, p2, p3, p4)
      TOO_MANY;
    }
    case currefl::getDependentSizedArrayType: {
      CHECK_TRAIT_IS_REFLPROP(getDependentSizedArrayType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::Expr *, p1)
      TRY_LOAD_VAL(enum clang::ArrayType::ArraySizeModifier, p2)
      TRY_LOAD_VAL(unsigned int, p3)
      TRY_LOAD_VAL(class clang::SourceRange, p4)
      if (no_more) CLASS_REFLECTION(P, getDependentSizedArrayType, p0, p1, p2, p3, p4)
      TOO_MANY;
    }
    case currefl::getIncompleteArrayType: {
      CHECK_TRAIT_IS_REFLPROP(getIncompleteArrayType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(enum clang::ArrayType::ArraySizeModifier, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) CLASS_REFLECTION(P, getIncompleteArrayType, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getVariableArrayDecayedType: {
      CHECK_TRAIT_IS_REFLPROP(getVariableArrayDecayedType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getVariableArrayDecayedType, p0)
      TOO_MANY;
    }
    case currefl::getVectorType: {
      CHECK_TRAIT_IS_REFLPROP(getVectorType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(enum clang::VectorType::VectorKind, p2)
      if (no_more) CLASS_REFLECTION(P, getVectorType, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getDependentVectorType: {
      CHECK_TRAIT_IS_REFLPROP(getDependentVectorType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::Expr *, p1)
      TRY_LOAD_VAL(class clang::SourceLocation, p2)
      TRY_LOAD_VAL(enum clang::VectorType::VectorKind, p3)
      if (no_more) CLASS_REFLECTION(P, getDependentVectorType, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::getExtVectorType: {
      CHECK_TRAIT_IS_REFLPROP(getExtVectorType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(P, getExtVectorType, p0, p1)
      TOO_MANY;
    }
    case currefl::getDependentSizedExtVectorType: {
      CHECK_TRAIT_IS_REFLPROP(getDependentSizedExtVectorType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::Expr *, p1)
      TRY_LOAD_VAL(class clang::SourceLocation, p2)
      if (no_more) CLASS_REFLECTION(P, getDependentSizedExtVectorType, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getDependentAddressSpaceType: {
      CHECK_TRAIT_IS_REFLPROP(getDependentAddressSpaceType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::Expr *, p1)
      TRY_LOAD_VAL(class clang::SourceLocation, p2)
      if (no_more) CLASS_REFLECTION(P, getDependentAddressSpaceType, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getFunctionNoProtoType: {
      CHECK_TRAIT_IS_REFLPROP(getFunctionNoProtoType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getFunctionNoProtoType, p0)
      TOO_MANY;
    }
    case currefl::adjustStringLiteralBaseType: {
      CHECK_TRAIT_IS_REFLPROP(adjustStringLiteralBaseType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, adjustStringLiteralBaseType, p0)
      TOO_MANY;
    }
    case currefl::getTypeDeclType: {
      CHECK_TRAIT_IS_REFLPROP(getTypeDeclType)
      TRY_LOAD_VAL(const class clang::TypeDecl *, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeDeclType, p0)
      TRY_LOAD_VAL(const class clang::TypeDecl *, p1)
      if (no_more) CLASS_REFLECTION(P, getTypeDeclType, p0, p1)
      TOO_MANY;
    }
    case currefl::getTypedefType: {
      CHECK_TRAIT_IS_REFLPROP(getTypedefType)
      TRY_LOAD_VAL(const class clang::TypedefNameDecl *, p0)
      if (no_more) CLASS_REFLECTION(P, getTypedefType, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) CLASS_REFLECTION(P, getTypedefType, p0, p1)
      TOO_MANY;
    }
    case currefl::getRecordType: {
      CHECK_TRAIT_IS_REFLPROP(getRecordType)
      TRY_LOAD_VAL(const class clang::RecordDecl *, p0)
      if (no_more) CLASS_REFLECTION(P, getRecordType, p0)
      TOO_MANY;
    }
    case currefl::getEnumType: {
      CHECK_TRAIT_IS_REFLPROP(getEnumType)
      TRY_LOAD_VAL(const class clang::EnumDecl *, p0)
      if (no_more) CLASS_REFLECTION(P, getEnumType, p0)
      TOO_MANY;
    }
    case currefl::getInjectedClassNameType: {
      CHECK_TRAIT_IS_REFLPROP(getInjectedClassNameType)
      TRY_LOAD_VAL(class clang::CXXRecordDecl *, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) CLASS_REFLECTION(P, getInjectedClassNameType, p0, p1)
      TOO_MANY;
    }
    case currefl::getSubstTemplateTypeParmType: {
      CHECK_TRAIT_IS_REFLPROP(getSubstTemplateTypeParmType)
      TRY_LOAD_VAL(const class clang::TemplateTypeParmType *, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) CLASS_REFLECTION(P, getSubstTemplateTypeParmType, p0, p1)
      TOO_MANY;
    }
    case currefl::getTemplateTypeParmType: {
      CHECK_TRAIT_IS_REFLPROP(getTemplateTypeParmType)
      TRY_LOAD_VAL(unsigned int, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      TRY_LOAD_VAL(bool, p2)
      if (no_more) CLASS_REFLECTION(P, getTemplateTypeParmType, p0, p1, p2)
      TRY_LOAD_VAL(class clang::TemplateTypeParmDecl *, p3)
      if (no_more) CLASS_REFLECTION(P, getTemplateTypeParmType, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::getParenType: {
      CHECK_TRAIT_IS_REFLPROP(getParenType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getParenType, p0)
      TOO_MANY;
    }
    case currefl::getElaboratedType: {
      CHECK_TRAIT_IS_REFLPROP(getElaboratedType)
      TRY_LOAD_VAL(enum clang::ElaboratedTypeKeyword, p0)
      TRY_LOAD_VAL(class clang::NestedNameSpecifier *, p1)
      TRY_LOAD_VAL(class clang::QualType, p2)
      if (no_more) CLASS_REFLECTION(P, getElaboratedType, p0, p1, p2)
      TRY_LOAD_VAL(class clang::TagDecl *, p3)
      if (no_more) CLASS_REFLECTION(P, getElaboratedType, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::getDependentNameType: {
      CHECK_TRAIT_IS_REFLPROP(getDependentNameType)
      TRY_LOAD_VAL(enum clang::ElaboratedTypeKeyword, p0)
      TRY_LOAD_VAL(class clang::NestedNameSpecifier *, p1)
      TRY_LOAD_VAL(const class clang::IdentifierInfo *, p2)
      if (no_more) CLASS_REFLECTION(P, getDependentNameType, p0, p1, p2)
      TRY_LOAD_VAL(class clang::QualType, p3)
      if (no_more) CLASS_REFLECTION(P, getDependentNameType, p0, p1, p2, p3)
      TOO_MANY;
    }
    case currefl::getObjCInterfaceType: {
      CHECK_TRAIT_IS_REFLPROP(getObjCInterfaceType)
      TRY_LOAD_VAL(const class clang::ObjCInterfaceDecl *, p0)
      if (no_more) CLASS_REFLECTION(P, getObjCInterfaceType, p0)
      TRY_LOAD_VAL(class clang::ObjCInterfaceDecl *, p1)
      if (no_more) CLASS_REFLECTION(P, getObjCInterfaceType, p0, p1)
      TOO_MANY;
    }
    case currefl::getObjCObjectType: {
      CHECK_TRAIT_IS_REFLPROP(getObjCObjectType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::ObjCProtocolDecl *const *, p1)
      TRY_LOAD_VAL(unsigned int, p2)
      if (no_more) CLASS_REFLECTION(P, getObjCObjectType, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getObjCObjectPointerType: {
      CHECK_TRAIT_IS_REFLPROP(getObjCObjectPointerType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getObjCObjectPointerType, p0)
      TOO_MANY;
    }
    case currefl::getTypeOfExprType: {
      CHECK_TRAIT_IS_REFLPROP(getTypeOfExprType)
      TRY_LOAD_VAL(class clang::Expr *, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeOfExprType, p0)
      TOO_MANY;
    }
    case currefl::getTypeOfType: {
      CHECK_TRAIT_IS_REFLPROP(getTypeOfType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeOfType, p0)
      TOO_MANY;
    }
    case currefl::getDecltypeType: {
      CHECK_TRAIT_IS_REFLPROP(getDecltypeType)
      TRY_LOAD_VAL(class clang::Expr *, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) CLASS_REFLECTION(P, getDecltypeType, p0, p1)
      TOO_MANY;
    }
    case currefl::getReflectedType: {
      CHECK_TRAIT_IS_REFLPROP(getReflectedType)
      TRY_LOAD_VAL(class clang::Expr *, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) CLASS_REFLECTION(P, getReflectedType, p0, p1)
      TOO_MANY;
    }
    case currefl::getUnaryTransformType: {
      CHECK_TRAIT_IS_REFLPROP(getUnaryTransformType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      TRY_LOAD_VAL(enum clang::UnaryTransformType::UTTKind, p2)
      if (no_more) CLASS_REFLECTION(P, getUnaryTransformType, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getAutoType: {
      CHECK_TRAIT_IS_REFLPROP(getAutoType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(enum clang::AutoTypeKeyword, p1)
      TRY_LOAD_VAL(bool, p2)
      if (no_more) CLASS_REFLECTION(P, getAutoType, p0, p1, p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAutoDeductType, CLASS, P, getAutoDeductType)
    case_METHOD_NOARGS(getAutoRRefDeductType, CLASS, P, getAutoRRefDeductType)
    case currefl::getDeducedTemplateSpecializationType: {
      CHECK_TRAIT_IS_REFLPROP(getDeducedTemplateSpecializationType)
      TRY_LOAD_VAL(class clang::TemplateName, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      TRY_LOAD_VAL(bool, p2)
      if (no_more) CLASS_REFLECTION(P, getDeducedTemplateSpecializationType, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getTagDeclType: {
      CHECK_TRAIT_IS_REFLPROP(getTagDeclType)
      TRY_LOAD_VAL(const class clang::TagDecl *, p0)
      if (no_more) CLASS_REFLECTION(P, getTagDeclType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getWCharType, CLASS, P, getWCharType)
    case_METHOD_NOARGS(getWideCharType, CLASS, P, getWideCharType)
    case_METHOD_NOARGS(getSignedWCharType, CLASS, P, getSignedWCharType)
    case_METHOD_NOARGS(getUnsignedWCharType, CLASS, P, getUnsignedWCharType)
    case_METHOD_NOARGS(getWIntType, CLASS, P, getWIntType)
    case_METHOD_NOARGS(getIntPtrType, CLASS, P, getIntPtrType)
    case_METHOD_NOARGS(getUIntPtrType, CLASS, P, getUIntPtrType)
    case_METHOD_NOARGS(getPointerDiffType, CLASS, P, getPointerDiffType)
    case_METHOD_NOARGS(getUnsignedPointerDiffType, CLASS, P, getUnsignedPointerDiffType)
    case_METHOD_NOARGS(getProcessIDType, CLASS, P, getProcessIDType)
    case_METHOD_NOARGS(getCFConstantStringType, CLASS, P, getCFConstantStringType)
    case_METHOD_NOARGS(getObjCSuperType, CLASS, P, getObjCSuperType)
    case_METHOD_NOARGS(getRawCFConstantStringType, CLASS, P, getRawCFConstantStringType)
    case_METHOD_NOARGS(getCFConstantStringDecl, CLASS, P, getCFConstantStringDecl)
    case_METHOD_NOARGS(getCFConstantStringTagDecl, CLASS, P, getCFConstantStringTagDecl)
    case_METHOD_NOARGS(getObjCConstantStringInterface, CLASS, P, getObjCConstantStringInterface)
    case_METHOD_NOARGS(getObjCNSStringType, CLASS, P, getObjCNSStringType)
    case_METHOD_NOARGS(getObjCIdRedefinitionType, CLASS, P, getObjCIdRedefinitionType)
    case_METHOD_NOARGS(getObjCClassRedefinitionType, CLASS, P, getObjCClassRedefinitionType)
    case_METHOD_NOARGS(getObjCSelRedefinitionType, CLASS, P, getObjCSelRedefinitionType)
    case_METHOD_NOARGS(getBoolName, CLASS, P, getBoolName)
    case_METHOD_NOARGS(getMakeIntegerSeqName, CLASS, P, getMakeIntegerSeqName)
    case_METHOD_NOARGS(getTypePackElementName, CLASS, P, getTypePackElementName)
    case_METHOD_NOARGS(getFILEType, CLASS, P, getFILEType)
    case_METHOD_NOARGS(getjmp_bufType, CLASS, P, getjmp_bufType)
    case_METHOD_NOARGS(getsigjmp_bufType, CLASS, P, getsigjmp_bufType)
    case_METHOD_NOARGS(getucontext_tType, CLASS, P, getucontext_tType)
    case_METHOD_NOARGS(getLogicalOperationType, CLASS, P, getLogicalOperationType)
    case currefl::getObjCEncodingForFunctionDecl: {
      CHECK_TRAIT_IS_REFLPROP(getObjCEncodingForFunctionDecl)
      TRY_LOAD_VAL(const class clang::FunctionDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, getObjCEncodingForFunctionDecl, p0)
      TOO_MANY;
    }
    case currefl::getObjCEncodingForMethodDecl: {
      CHECK_TRAIT_IS_REFLPROP(getObjCEncodingForMethodDecl)
      TRY_LOAD_VAL(const class clang::ObjCMethodDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, getObjCEncodingForMethodDecl, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) PRIM_REFLECTION(P, getObjCEncodingForMethodDecl, p0, p1)
      TOO_MANY;
    }
    case currefl::getObjCEncodingForBlock: {
      CHECK_TRAIT_IS_REFLPROP(getObjCEncodingForBlock)
      TRY_LOAD_VAL(const class clang::BlockExpr *, p0)
      if (no_more) PRIM_REFLECTION(P, getObjCEncodingForBlock, p0)
      TOO_MANY;
    }
    case currefl::getObjCEncodingForPropertyDecl: {
      CHECK_TRAIT_IS_REFLPROP(getObjCEncodingForPropertyDecl)
      TRY_LOAD_VAL(const class clang::ObjCPropertyDecl *, p0)
      TRY_LOAD_VAL(const class clang::Decl *, p1)
      if (no_more) PRIM_REFLECTION(P, getObjCEncodingForPropertyDecl, p0, p1)
      TOO_MANY;
    }
    case currefl::ProtocolCompatibleWithProtocol: {
      CHECK_TRAIT_IS_REFLPROP(ProtocolCompatibleWithProtocol)
      TRY_LOAD_VAL(class clang::ObjCProtocolDecl *, p0)
      TRY_LOAD_VAL(class clang::ObjCProtocolDecl *, p1)
      if (no_more) PRIM_REFLECTION(P, ProtocolCompatibleWithProtocol, p0, p1)
      TOO_MANY;
    }
    case currefl::getObjCPropertyImplDeclForPropertyDecl: {
      CHECK_TRAIT_IS_REFLPROP(getObjCPropertyImplDeclForPropertyDecl)
      TRY_LOAD_VAL(const class clang::ObjCPropertyDecl *, p0)
      TRY_LOAD_VAL(const class clang::Decl *, p1)
      if (no_more) CLASS_REFLECTION(P, getObjCPropertyImplDeclForPropertyDecl, p0, p1)
      TOO_MANY;
    }
    case currefl::getObjCEncodingTypeSize: {
      CHECK_TRAIT_IS_REFLPROP(getObjCEncodingTypeSize)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getObjCEncodingTypeSize, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getObjCIdDecl, CLASS, P, getObjCIdDecl)
    case_METHOD_NOARGS(getObjCIdType, CLASS, P, getObjCIdType)
    case_METHOD_NOARGS(getObjCSelDecl, CLASS, P, getObjCSelDecl)
    case_METHOD_NOARGS(getObjCSelType, CLASS, P, getObjCSelType)
    case_METHOD_NOARGS(getObjCClassDecl, CLASS, P, getObjCClassDecl)
    case_METHOD_NOARGS(getObjCClassType, CLASS, P, getObjCClassType)
    case_METHOD_NOARGS(getObjCProtocolDecl, CLASS, P, getObjCProtocolDecl)
    case_METHOD_NOARGS(getBOOLDecl, CLASS, P, getBOOLDecl)
    case_METHOD_NOARGS(getBOOLType, CLASS, P, getBOOLType)
    case_METHOD_NOARGS(getObjCProtoType, CLASS, P, getObjCProtoType)
    case_METHOD_NOARGS(getBuiltinVaListDecl, CLASS, P, getBuiltinVaListDecl)
    case_METHOD_NOARGS(getBuiltinVaListType, CLASS, P, getBuiltinVaListType)
    case_METHOD_NOARGS(getVaListTagDecl, CLASS, P, getVaListTagDecl)
    case_METHOD_NOARGS(getBuiltinMSVaListDecl, CLASS, P, getBuiltinMSVaListDecl)
    case_METHOD_NOARGS(getBuiltinMSVaListType, CLASS, P, getBuiltinMSVaListType)
    case currefl::canBuiltinBeRedeclared: {
      CHECK_TRAIT_IS_REFLPROP(canBuiltinBeRedeclared)
      TRY_LOAD_VAL(const class clang::FunctionDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, canBuiltinBeRedeclared, p0)
      TOO_MANY;
    }
    case currefl::getCVRQualifiedType: {
      CHECK_TRAIT_IS_REFLPROP(getCVRQualifiedType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(P, getCVRQualifiedType, p0, p1)
      TOO_MANY;
    }
    case currefl::getQualifiedType: {
      CHECK_TRAIT_IS_REFLPROP(getQualifiedType)
      TRY_LOAD_VAL(struct clang::SplitQualType, p0)
      if (no_more) CLASS_REFLECTION(P, getQualifiedType, p0)
      TOO_MANY;
    }
    case currefl::getQualifiedType1: {
      CHECK_TRAIT_IS_REFLPROP(getQualifiedType1)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::Qualifiers, p1)
      if (no_more) CLASS_REFLECTION(P, getQualifiedType, p0, p1)
      TOO_MANY;
    }
    case currefl::getQualifiedType2: {
      CHECK_TRAIT_IS_REFLPROP(getQualifiedType2)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      TRY_LOAD_VAL(class clang::Qualifiers, p1)
      if (no_more) CLASS_REFLECTION(P, getQualifiedType, p0, p1)
      TOO_MANY;
    }
    case currefl::getUnqualifiedObjCPointerType: {
      CHECK_TRAIT_IS_REFLPROP(getUnqualifiedObjCPointerType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getUnqualifiedObjCPointerType, p0)
      TOO_MANY;
    }
    case currefl::getFixedPointScale: {
      CHECK_TRAIT_IS_REFLPROP(getFixedPointScale)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getFixedPointScale, p0)
      TOO_MANY;
    }
    case currefl::getFixedPointIBits: {
      CHECK_TRAIT_IS_REFLPROP(getFixedPointIBits)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getFixedPointIBits, p0)
      TOO_MANY;
    }
    case currefl::getNameForTemplate: {
      CHECK_TRAIT_IS_REFLPROP(getNameForTemplate)
      TRY_LOAD_VAL(class clang::TemplateName, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) CLASS_REFLECTION(P, getNameForTemplate, p0, p1)
      TOO_MANY;
    }
    case currefl::getOverloadedTemplateName: {
      CHECK_TRAIT_IS_REFLPROP(getOverloadedTemplateName)
      TRY_LOAD_VAL(class clang::UnresolvedSetIterator, p0)
      TRY_LOAD_VAL(class clang::UnresolvedSetIterator, p1)
      if (no_more) CLASS_REFLECTION(P, getOverloadedTemplateName, p0, p1)
      TOO_MANY;
    }
    case currefl::getQualifiedTemplateName: {
      CHECK_TRAIT_IS_REFLPROP(getQualifiedTemplateName)
      TRY_LOAD_VAL(class clang::NestedNameSpecifier *, p0)
      TRY_LOAD_VAL(bool, p1)
      TRY_LOAD_VAL(class clang::TemplateDecl *, p2)
      if (no_more) CLASS_REFLECTION(P, getQualifiedTemplateName, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getDependentTemplateName: {
      CHECK_TRAIT_IS_REFLPROP(getDependentTemplateName)
      TRY_LOAD_VAL(class clang::NestedNameSpecifier *, p0)
      TRY_LOAD_VAL(const class clang::IdentifierInfo *, p1)
      if (no_more) CLASS_REFLECTION(P, getDependentTemplateName, p0, p1)
      TOO_MANY;
    }
    case currefl::getDependentTemplateName1: {
      CHECK_TRAIT_IS_REFLPROP(getDependentTemplateName1)
      TRY_LOAD_VAL(class clang::NestedNameSpecifier *, p0)
      TRY_LOAD_VAL(enum clang::OverloadedOperatorKind, p1)
      if (no_more) CLASS_REFLECTION(P, getDependentTemplateName, p0, p1)
      TOO_MANY;
    }
    case currefl::getSubstTemplateTemplateParm: {
      CHECK_TRAIT_IS_REFLPROP(getSubstTemplateTemplateParm)
      TRY_LOAD_VAL(class clang::TemplateTemplateParmDecl *, p0)
      TRY_LOAD_VAL(class clang::TemplateName, p1)
      if (no_more) CLASS_REFLECTION(P, getSubstTemplateTemplateParm, p0, p1)
      TOO_MANY;
    }
    case_FIELD(CLASS, P, CompCategories)
    case currefl::getObjCGCAttrKind: {
      CHECK_TRAIT_IS_REFLPROP(getObjCGCAttrKind)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getObjCGCAttrKind, p0)
      TOO_MANY;
    }
    case currefl::isObjCNSObjectType: {
      CHECK_TRAIT_IS_REFLPROP(isObjCNSObjectType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(S, isObjCNSObjectType, p0)
      TOO_MANY;
    }
    case currefl::getTypeInfo: {
      CHECK_TRAIT_IS_REFLPROP(getTypeInfo)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeInfo, p0)
      TOO_MANY;
    }
    case currefl::getTypeInfo1: {
      CHECK_TRAIT_IS_REFLPROP(getTypeInfo1)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeInfo, p0)
      TOO_MANY;
    }
    case currefl::getOpenMPDefaultSimdAlign: {
      CHECK_TRAIT_IS_REFLPROP(getOpenMPDefaultSimdAlign)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getOpenMPDefaultSimdAlign, p0)
      TOO_MANY;
    }
    case currefl::getTypeSize: {
      CHECK_TRAIT_IS_REFLPROP(getTypeSize)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeSize, p0)
      TOO_MANY;
    }
    case currefl::getTypeSize1: {
      CHECK_TRAIT_IS_REFLPROP(getTypeSize1)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeSize, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getCharWidth, PRIM, P, getCharWidth)
    case currefl::toCharUnitsFromBits: {
      CHECK_TRAIT_IS_REFLPROP(toCharUnitsFromBits)
      TRY_LOAD_VAL(long long, p0)
      if (no_more) CLASS_REFLECTION(P, toCharUnitsFromBits, p0)
      TOO_MANY;
    }
    case currefl::toBits: {
      CHECK_TRAIT_IS_REFLPROP(toBits)
      TRY_LOAD_VAL(class clang::CharUnits, p0)
      if (no_more) PRIM_REFLECTION(P, toBits, p0)
      TOO_MANY;
    }
    case currefl::getTypeSizeInChars: {
      CHECK_TRAIT_IS_REFLPROP(getTypeSizeInChars)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeSizeInChars, p0)
      TOO_MANY;
    }
    case currefl::getTypeSizeInChars1: {
      CHECK_TRAIT_IS_REFLPROP(getTypeSizeInChars1)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeSizeInChars, p0)
      TOO_MANY;
    }
    case currefl::getTypeAlign: {
      CHECK_TRAIT_IS_REFLPROP(getTypeAlign)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeAlign, p0)
      TOO_MANY;
    }
    case currefl::getTypeAlign1: {
      CHECK_TRAIT_IS_REFLPROP(getTypeAlign1)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeAlign, p0)
      TOO_MANY;
    }
    case currefl::getTypeUnadjustedAlign: {
      CHECK_TRAIT_IS_REFLPROP(getTypeUnadjustedAlign)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeUnadjustedAlign, p0)
      TOO_MANY;
    }
    case currefl::getTypeUnadjustedAlign1: {
      CHECK_TRAIT_IS_REFLPROP(getTypeUnadjustedAlign1)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeUnadjustedAlign, p0)
      TOO_MANY;
    }
    case currefl::getTypeAlignIfKnown: {
      CHECK_TRAIT_IS_REFLPROP(getTypeAlignIfKnown)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getTypeAlignIfKnown, p0)
      TOO_MANY;
    }
    case currefl::getTypeAlignInChars: {
      CHECK_TRAIT_IS_REFLPROP(getTypeAlignInChars)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeAlignInChars, p0)
      TOO_MANY;
    }
    case currefl::getTypeAlignInChars1: {
      CHECK_TRAIT_IS_REFLPROP(getTypeAlignInChars1)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeAlignInChars, p0)
      TOO_MANY;
    }
    case currefl::getTypeUnadjustedAlignInChars: {
      CHECK_TRAIT_IS_REFLPROP(getTypeUnadjustedAlignInChars)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeUnadjustedAlignInChars, p0)
      TOO_MANY;
    }
    case currefl::getTypeUnadjustedAlignInChars1: {
      CHECK_TRAIT_IS_REFLPROP(getTypeUnadjustedAlignInChars1)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) CLASS_REFLECTION(P, getTypeUnadjustedAlignInChars, p0)
      TOO_MANY;
    }
    case currefl::isAlignmentRequired: {
      CHECK_TRAIT_IS_REFLPROP(isAlignmentRequired)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, isAlignmentRequired, p0)
      TOO_MANY;
    }
    case currefl::isAlignmentRequired1: {
      CHECK_TRAIT_IS_REFLPROP(isAlignmentRequired1)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, isAlignmentRequired, p0)
      TOO_MANY;
    }
    case currefl::getPreferredTypeAlign: {
      CHECK_TRAIT_IS_REFLPROP(getPreferredTypeAlign)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) PRIM_REFLECTION(P, getPreferredTypeAlign, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getTargetDefaultAlignForAttributeAligned, PRIM, P, getTargetDefaultAlignForAttributeAligned)
    case currefl::getAlignOfGlobalVar: {
      CHECK_TRAIT_IS_REFLPROP(getAlignOfGlobalVar)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getAlignOfGlobalVar, p0)
      TOO_MANY;
    }
    case currefl::getAlignOfGlobalVarInChars: {
      CHECK_TRAIT_IS_REFLPROP(getAlignOfGlobalVarInChars)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getAlignOfGlobalVarInChars, p0)
      TOO_MANY;
    }
    case currefl::getDeclAlign: {
      CHECK_TRAIT_IS_REFLPROP(getDeclAlign)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) CLASS_REFLECTION(P, getDeclAlign, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, getDeclAlign, p0, p1)
      TOO_MANY;
    }
    case currefl::getOffsetOfBaseWithVBPtr: {
      CHECK_TRAIT_IS_REFLPROP(getOffsetOfBaseWithVBPtr)
      TRY_LOAD_VAL(const class clang::CXXRecordDecl *, p0)
      if (no_more) CLASS_REFLECTION(P, getOffsetOfBaseWithVBPtr, p0)
      TOO_MANY;
    }
    case currefl::getFieldOffset: {
      CHECK_TRAIT_IS_REFLPROP(getFieldOffset)
      TRY_LOAD_VAL(const class clang::ValueDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, getFieldOffset, p0)
      TOO_MANY;
    }
    case currefl::lookupFieldBitOffset: {
      CHECK_TRAIT_IS_REFLPROP(lookupFieldBitOffset)
      TRY_LOAD_VAL(const class clang::ObjCInterfaceDecl *, p0)
      TRY_LOAD_VAL(const class clang::ObjCImplementationDecl *, p1)
      TRY_LOAD_VAL(const class clang::ObjCIvarDecl *, p2)
      if (no_more) PRIM_REFLECTION(P, lookupFieldBitOffset, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::isNearlyEmpty: {
      CHECK_TRAIT_IS_REFLPROP(isNearlyEmpty)
      TRY_LOAD_VAL(const class clang::CXXRecordDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, isNearlyEmpty, p0)
      TOO_MANY;
    }
    case currefl::CountNonClassIvars: {
      CHECK_TRAIT_IS_REFLPROP(CountNonClassIvars)
      TRY_LOAD_VAL(const class clang::ObjCInterfaceDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, CountNonClassIvars, p0)
      TOO_MANY;
    }
    case currefl::hasUniqueObjectRepresentations: {
      CHECK_TRAIT_IS_REFLPROP(hasUniqueObjectRepresentations)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, hasUniqueObjectRepresentations, p0)
      TOO_MANY;
    }
    case currefl::getCanonicalType: {
      CHECK_TRAIT_IS_REFLPROP(getCanonicalType)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) CLASS_REFLECTION(P, getCanonicalType, p0)
      TOO_MANY;
    }
    case currefl::hasSameType: {
      CHECK_TRAIT_IS_REFLPROP(hasSameType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) PRIM_REFLECTION(P, hasSameType, p0, p1)
      TOO_MANY;
    }
    case currefl::hasSameType1: {
      CHECK_TRAIT_IS_REFLPROP(hasSameType1)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      TRY_LOAD_VAL(const class clang::Type *, p1)
      if (no_more) PRIM_REFLECTION(P, hasSameType, p0, p1)
      TOO_MANY;
    }
    case currefl::hasSameUnqualifiedType: {
      CHECK_TRAIT_IS_REFLPROP(hasSameUnqualifiedType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) PRIM_REFLECTION(P, hasSameUnqualifiedType, p0, p1)
      TOO_MANY;
    }
    case currefl::hasSameNullabilityTypeQualifier: {
      CHECK_TRAIT_IS_REFLPROP(hasSameNullabilityTypeQualifier)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      TRY_LOAD_VAL(bool, p2)
      if (no_more) PRIM_REFLECTION(P, hasSameNullabilityTypeQualifier, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getCanonicalNestedNameSpecifier: {
      CHECK_TRAIT_IS_REFLPROP(getCanonicalNestedNameSpecifier)
      TRY_LOAD_VAL(class clang::NestedNameSpecifier *, p0)
      if (no_more) CLASS_REFLECTION(P, getCanonicalNestedNameSpecifier, p0)
      TOO_MANY;
    }
    case currefl::getDefaultCallingConvention: {
      CHECK_TRAIT_IS_REFLPROP(getDefaultCallingConvention)
      TRY_LOAD_VAL(bool, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) PRIM_REFLECTION(P, getDefaultCallingConvention, p0, p1)
      TOO_MANY;
    }
    case currefl::getCanonicalTemplateName: {
      CHECK_TRAIT_IS_REFLPROP(getCanonicalTemplateName)
      TRY_LOAD_VAL(class clang::TemplateName, p0)
      if (no_more) CLASS_REFLECTION(P, getCanonicalTemplateName, p0)
      TOO_MANY;
    }
    case currefl::getAsArrayType: {
      CHECK_TRAIT_IS_REFLPROP(getAsArrayType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getAsArrayType, p0)
      TOO_MANY;
    }
    case currefl::getAsConstantArrayType: {
      CHECK_TRAIT_IS_REFLPROP(getAsConstantArrayType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getAsConstantArrayType, p0)
      TOO_MANY;
    }
    case currefl::getAsVariableArrayType: {
      CHECK_TRAIT_IS_REFLPROP(getAsVariableArrayType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getAsVariableArrayType, p0)
      TOO_MANY;
    }
    case currefl::getAsIncompleteArrayType: {
      CHECK_TRAIT_IS_REFLPROP(getAsIncompleteArrayType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getAsIncompleteArrayType, p0)
      TOO_MANY;
    }
    case currefl::getAsDependentSizedArrayType: {
      CHECK_TRAIT_IS_REFLPROP(getAsDependentSizedArrayType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getAsDependentSizedArrayType, p0)
      TOO_MANY;
    }
    case currefl::getBaseElementType: {
      CHECK_TRAIT_IS_REFLPROP(getBaseElementType)
      TRY_LOAD_VAL(const class clang::ArrayType *, p0)
      if (no_more) CLASS_REFLECTION(P, getBaseElementType, p0)
      TOO_MANY;
    }
    case currefl::getBaseElementType1: {
      CHECK_TRAIT_IS_REFLPROP(getBaseElementType1)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getBaseElementType, p0)
      TOO_MANY;
    }
    case currefl::getConstantArrayElementCount: {
      CHECK_TRAIT_IS_REFLPROP(getConstantArrayElementCount)
      TRY_LOAD_VAL(const class clang::ConstantArrayType *, p0)
      if (no_more) PRIM_REFLECTION(P, getConstantArrayElementCount, p0)
      TOO_MANY;
    }
    case currefl::getAdjustedParameterType: {
      CHECK_TRAIT_IS_REFLPROP(getAdjustedParameterType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getAdjustedParameterType, p0)
      TOO_MANY;
    }
    case currefl::getSignatureParameterType: {
      CHECK_TRAIT_IS_REFLPROP(getSignatureParameterType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getSignatureParameterType, p0)
      TOO_MANY;
    }
    case currefl::getExceptionObjectType: {
      CHECK_TRAIT_IS_REFLPROP(getExceptionObjectType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getExceptionObjectType, p0)
      TOO_MANY;
    }
    case currefl::getArrayDecayedType: {
      CHECK_TRAIT_IS_REFLPROP(getArrayDecayedType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getArrayDecayedType, p0)
      TOO_MANY;
    }
    case currefl::getPromotedIntegerType: {
      CHECK_TRAIT_IS_REFLPROP(getPromotedIntegerType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getPromotedIntegerType, p0)
      TOO_MANY;
    }
    case currefl::getInnerObjCOwnership: {
      CHECK_TRAIT_IS_REFLPROP(getInnerObjCOwnership)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getInnerObjCOwnership, p0)
      TOO_MANY;
    }
    case currefl::isPromotableBitField: {
      CHECK_TRAIT_IS_REFLPROP(isPromotableBitField)
      TRY_LOAD_VAL(class clang::Expr *, p0)
      if (no_more) CLASS_REFLECTION(P, isPromotableBitField, p0)
      TOO_MANY;
    }
    case currefl::getIntegerTypeOrder: {
      CHECK_TRAIT_IS_REFLPROP(getIntegerTypeOrder)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) PRIM_REFLECTION(P, getIntegerTypeOrder, p0, p1)
      TOO_MANY;
    }
    case currefl::getFloatingTypeOrder: {
      CHECK_TRAIT_IS_REFLPROP(getFloatingTypeOrder)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) PRIM_REFLECTION(P, getFloatingTypeOrder, p0, p1)
      TOO_MANY;
    }
    case currefl::getFloatingTypeOfSizeWithinDomain: {
      CHECK_TRAIT_IS_REFLPROP(getFloatingTypeOfSizeWithinDomain)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) CLASS_REFLECTION(P, getFloatingTypeOfSizeWithinDomain, p0, p1)
      TOO_MANY;
    }
    case currefl::getTargetAddressSpace: {
      CHECK_TRAIT_IS_REFLPROP(getTargetAddressSpace)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getTargetAddressSpace, p0)
      TOO_MANY;
    }
    case currefl::getTargetAddressSpace1: {
      CHECK_TRAIT_IS_REFLPROP(getTargetAddressSpace1)
      TRY_LOAD_VAL(class clang::Qualifiers, p0)
      if (no_more) PRIM_REFLECTION(P, getTargetAddressSpace, p0)
      TOO_MANY;
    }
    case currefl::getTargetAddressSpace2: {
      CHECK_TRAIT_IS_REFLPROP(getTargetAddressSpace2)
      TRY_LOAD_VAL(enum clang::LangAS, p0)
      if (no_more) PRIM_REFLECTION(P, getTargetAddressSpace, p0)
      TOO_MANY;
    }
    case currefl::getTargetNullPointerValue: {
      CHECK_TRAIT_IS_REFLPROP(getTargetNullPointerValue)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getTargetNullPointerValue, p0)
      TOO_MANY;
    }
    case currefl::addressSpaceMapManglingFor: {
      CHECK_TRAIT_IS_REFLPROP(addressSpaceMapManglingFor)
      TRY_LOAD_VAL(enum clang::LangAS, p0)
      if (no_more) PRIM_REFLECTION(P, addressSpaceMapManglingFor, p0)
      TOO_MANY;
    }
    case currefl::isObjCIdType: {
      CHECK_TRAIT_IS_REFLPROP(isObjCIdType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, isObjCIdType, p0)
      TOO_MANY;
    }
    case currefl::isObjCClassType: {
      CHECK_TRAIT_IS_REFLPROP(isObjCClassType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, isObjCClassType, p0)
      TOO_MANY;
    }
    case currefl::isObjCSelType: {
      CHECK_TRAIT_IS_REFLPROP(isObjCSelType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, isObjCSelType, p0)
      TOO_MANY;
    }
    case currefl::getIntWidth: {
      CHECK_TRAIT_IS_REFLPROP(getIntWidth)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(P, getIntWidth, p0)
      TOO_MANY;
    }
    case currefl::getCorrespondingUnsignedType: {
      CHECK_TRAIT_IS_REFLPROP(getCorrespondingUnsignedType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getCorrespondingUnsignedType, p0)
      TOO_MANY;
    }
    case currefl::getCorrespondingSaturatedType: {
      CHECK_TRAIT_IS_REFLPROP(getCorrespondingSaturatedType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getCorrespondingSaturatedType, p0)
      TOO_MANY;
    }
    case currefl::MakeIntValue: {
      CHECK_TRAIT_IS_REFLPROP(MakeIntValue)
      TRY_LOAD_VAL(unsigned long long, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) CLASS_REFLECTION(P, MakeIntValue, p0, p1)
      TOO_MANY;
    }
    case currefl::getObjCMethodRedeclaration: {
      CHECK_TRAIT_IS_REFLPROP(getObjCMethodRedeclaration)
      TRY_LOAD_VAL(const class clang::ObjCMethodDecl *, p0)
      if (no_more) CLASS_REFLECTION(P, getObjCMethodRedeclaration, p0)
      TOO_MANY;
    }
    case currefl::getObjContainingInterface: {
      CHECK_TRAIT_IS_REFLPROP(getObjContainingInterface)
      TRY_LOAD_VAL(const class clang::NamedDecl *, p0)
      if (no_more) CLASS_REFLECTION(P, getObjContainingInterface, p0)
      TOO_MANY;
    }
    case currefl::CreateTypeSourceInfo: {
      CHECK_TRAIT_IS_REFLPROP(CreateTypeSourceInfo)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, CreateTypeSourceInfo, p0)
      TRY_LOAD_VAL(unsigned int, p1)
      if (no_more) CLASS_REFLECTION(P, CreateTypeSourceInfo, p0, p1)
      TOO_MANY;
    }
    case currefl::getTrivialTypeSourceInfo: {
      CHECK_TRAIT_IS_REFLPROP(getTrivialTypeSourceInfo)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getTrivialTypeSourceInfo, p0)
      TRY_LOAD_VAL(class clang::SourceLocation, p1)
      if (no_more) CLASS_REFLECTION(P, getTrivialTypeSourceInfo, p0, p1)
      TOO_MANY;
    }
    case currefl::GetGVALinkageForFunction: {
      CHECK_TRAIT_IS_REFLPROP(GetGVALinkageForFunction)
      TRY_LOAD_VAL(const class clang::FunctionDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, GetGVALinkageForFunction, p0)
      TOO_MANY;
    }
    case currefl::getManglingNumber: {
      CHECK_TRAIT_IS_REFLPROP(getManglingNumber)
      TRY_LOAD_VAL(const class clang::NamedDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, getManglingNumber, p0)
      TOO_MANY;
    }
    case currefl::getStaticLocalNumber: {
      CHECK_TRAIT_IS_REFLPROP(getStaticLocalNumber)
      TRY_LOAD_VAL(const class clang::VarDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, getStaticLocalNumber, p0)
      TOO_MANY;
    }
    case currefl::getParameterIndex: {
      CHECK_TRAIT_IS_REFLPROP(getParameterIndex)
      TRY_LOAD_VAL(const class clang::ParmVarDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, getParameterIndex, p0)
      TOO_MANY;
    }
    case currefl::isMSStaticDataMemberInlineDefinition: {
      CHECK_TRAIT_IS_REFLPROP(isMSStaticDataMemberInlineDefinition)
      TRY_LOAD_VAL(const class clang::VarDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, isMSStaticDataMemberInlineDefinition, p0)
      TOO_MANY;
    }
    case currefl::getInlineVariableDefinitionKind: {
      CHECK_TRAIT_IS_REFLPROP(getInlineVariableDefinitionKind)
      TRY_LOAD_VAL(const class clang::VarDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, getInlineVariableDefinitionKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclAccessPair *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DeclAccessPair;
# define CLASSNAME clang::DeclAccessPair
  switch( (currefl::memnames)N ) {
    case currefl::make: {
      CHECK_TRAIT_IS_REFLPROP(make)
      TRY_LOAD_VAL(class clang::NamedDecl *, p0)
      TRY_LOAD_VAL(enum clang::AccessSpecifier, p1)
      if (no_more) CLASS_REFLECTION(S, make, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(getAccess, PRIM, P, getAccess)
    case_METHOD_NOARGS(operator_arrow, CLASS, P, operator->)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclAccessPair X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DeclAccessPair;
# define CLASSNAME clang::DeclAccessPair
  switch( (currefl::memnames)N ) {
    case currefl::make: {
      CHECK_TRAIT_IS_REFLPROP(make)
      TRY_LOAD_VAL(class clang::NamedDecl *, p0)
      TRY_LOAD_VAL(enum clang::AccessSpecifier, p1)
      if (no_more) CLASS_REFLECTION(S, make, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getDecl, CLASS, V, getDecl)
    case_METHOD_NOARGS(getAccess, PRIM, V, getAccess)
    case_METHOD_NOARGS(operator_arrow, CLASS, V, operator->)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UnresolvedSetIterator X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__UnresolvedSetIterator;
# define CLASSNAME clang::UnresolvedSetIterator
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDecl, CLASS, V, getDecl)
    case currefl::setDecl: {
      CHECK_TRAIT_IS_REFLPROP(setDecl)
      TRY_LOAD_VAL(class clang::NamedDecl *, p0)
      if (no_more) VOID_REFLECTION(V, setDecl, (class clang::NamedDecl *)p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAccess, PRIM, V, getAccess)
    case_METHOD_NOARGS(getPair, CLASS, V, getPair)
    case_METHOD_NOARGS(operator_star, CLASS, V, operator*)
    case_METHOD_NOARGS(operator_arrow, CLASS, V, operator->)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::Expr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__Expr;
# define CLASSNAME clang::Expr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getType, CLASS, P, getType)
    case_METHOD_NOARGS(isValueDependent, PRIM, P, isValueDependent)
    case_METHOD_NOARGS(isTypeDependent, PRIM, P, isTypeDependent)
    case_METHOD_NOARGS(isInstantiationDependent, PRIM, P, isInstantiationDependent)
    case_METHOD_NOARGS(containsUnexpandedParameterPack, PRIM, P, containsUnexpandedParameterPack)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case_METHOD_NOARGS(isLValue, PRIM, P, isLValue)
    case_METHOD_NOARGS(isRValue, PRIM, P, isRValue)
    case_METHOD_NOARGS(isXValue, PRIM, P, isXValue)
    case_METHOD_NOARGS(isGLValue, PRIM, P, isGLValue)
    case currefl::getValueKindForType: {
      CHECK_TRAIT_IS_REFLPROP(getValueKindForType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) PRIM_REFLECTION(S, getValueKindForType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getValueKind, PRIM, P, getValueKind)
    case_METHOD_NOARGS(getObjectKind, PRIM, P, getObjectKind)
    case_METHOD_NOARGS(isOrdinaryOrBitFieldObject, PRIM, P, isOrdinaryOrBitFieldObject)
    case_METHOD_NOARGS(refersToBitField, PRIM, P, refersToBitField)
    case_METHOD_NOARGS(getSourceBitField, CLASS, P, getSourceBitField)
    case_METHOD_NOARGS(getReferencedDeclOfCallee, CLASS, P, getReferencedDeclOfCallee)
    case_METHOD_NOARGS(getObjCProperty, CLASS, P, getObjCProperty)
    case_METHOD_NOARGS(isObjCSelfExpr, PRIM, P, isObjCSelfExpr)
    case_METHOD_NOARGS(refersToVectorElement, PRIM, P, refersToVectorElement)
    case_METHOD_NOARGS(refersToGlobalRegisterVar, PRIM, P, refersToGlobalRegisterVar)
    case_METHOD_NOARGS(hasPlaceholderType, PRIM, P, hasPlaceholderType)
    case currefl::hasPlaceholderType1: {
      CHECK_TRAIT_IS_REFLPROP(hasPlaceholderType1)
      TRY_LOAD_VAL(enum clang::BuiltinType::Kind, p0)
      if (no_more) PRIM_REFLECTION(P, hasPlaceholderType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isKnownToHaveBooleanValue, PRIM, P, isKnownToHaveBooleanValue)
    case currefl::findBoundMemberType: {
      CHECK_TRAIT_IS_REFLPROP(findBoundMemberType)
      TRY_LOAD_VAL(const class clang::Expr *, p0)
      if (no_more) CLASS_REFLECTION(S, findBoundMemberType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(IgnoreImplicit, CLASS, P, IgnoreImplicit)
    case_METHOD_NOARGS(IgnoreConversionOperator, CLASS, P, IgnoreConversionOperator)
    case_METHOD_NOARGS(IgnoreParenImpCasts, CLASS, P, IgnoreParenImpCasts)
    case_METHOD_NOARGS(IgnoreParenLValueCasts, CLASS, P, IgnoreParenLValueCasts)
    case_METHOD_NOARGS(ignoreParenBaseCasts, CLASS, P, ignoreParenBaseCasts)
    case_METHOD_NOARGS(isDefaultArgument, PRIM, P, isDefaultArgument)
    case_METHOD_NOARGS(isImplicitCXXThis, PRIM, P, isImplicitCXXThis)
    case_METHOD_NOARGS(IgnoreImpCasts, CLASS, P, IgnoreImpCasts)
    case_METHOD_NOARGS(IgnoreParens, CLASS, P, IgnoreParens)
    case_METHOD_NOARGS(IgnoreParenCasts, CLASS, P, IgnoreParenCasts)
    case_METHOD_NOARGS(IgnoreCasts, CLASS, P, IgnoreCasts)
    case_METHOD_NOARGS(getBestDynamicClassType, CLASS, P, getBestDynamicClassType)
    case_METHOD_NOARGS(getBestDynamicClassTypeExpr, CLASS, P, getBestDynamicClassTypeExpr)
    case_METHOD_NOARGS(skipRValueSubobjectAdjustments, CLASS, P, skipRValueSubobjectAdjustments)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OpaqueValueExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OpaqueValueExpr;
# define CLASSNAME clang::OpaqueValueExpr
  switch( (currefl::memnames)N ) {
    case currefl::findInCopyConstruct: {
      CHECK_TRAIT_IS_REFLPROP(findInCopyConstruct)
      TRY_LOAD_VAL(const class clang::Expr *, p0)
      if (no_more) CLASS_REFLECTION(S, findInCopyConstruct, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getSourceExpr, CLASS, P, getSourceExpr)
    case_METHOD_NOARGS(isUnique, PRIM, P, isUnique)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DeclRefExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DeclRefExpr;
# define CLASSNAME clang::DeclRefExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(getNameInfo, CLASS, P, getNameInfo)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(hasQualifier, PRIM, P, hasQualifier)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getFoundDecl, CLASS, P, getFoundDecl)
    case_METHOD_NOARGS(hasTemplateKWAndArgsInfo, PRIM, P, hasTemplateKWAndArgsInfo)
    case_METHOD_NOARGS(getTemplateKeywordLoc, CLASS, P, getTemplateKeywordLoc)
    case_METHOD_NOARGS(getLAngleLoc, CLASS, P, getLAngleLoc)
    case_METHOD_NOARGS(getRAngleLoc, CLASS, P, getRAngleLoc)
    case_METHOD_NOARGS(hasTemplateKeyword, PRIM, P, hasTemplateKeyword)
    case_METHOD_NOARGS(hasExplicitTemplateArgs, PRIM, P, hasExplicitTemplateArgs)
    case_METHOD_NOARGS(getTemplateArgs, CLASS, P, getTemplateArgs)
    case_METHOD_NOARGS(getNumTemplateArgs, PRIM, P, getNumTemplateArgs)
    case currefl::template_arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::template_arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, template_arguments)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hadMultipleCandidates, PRIM, P, hadMultipleCandidates)
    case_METHOD_NOARGS(refersToEnclosingVariableOrCapture, PRIM, P, refersToEnclosingVariableOrCapture)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::PredefinedExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__PredefinedExpr;
# define CLASSNAME clang::PredefinedExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getIdentType, PRIM, P, getIdentType)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getFunctionName, CLASS, P, getFunctionName)
    case currefl::getIdentTypeName: {
      CHECK_TRAIT_IS_REFLPROP(getIdentTypeName)
      TRY_LOAD_VAL(enum clang::PredefinedExpr::IdentType, p0)
      if (no_more) CLASS_REFLECTION(S, getIdentTypeName, p0)
      TOO_MANY;
    }
    case currefl::ComputeName: {
      CHECK_TRAIT_IS_REFLPROP(ComputeName)
      TRY_LOAD_VAL(enum clang::PredefinedExpr::IdentType, p0)
      TRY_LOAD_VAL(const class clang::Decl *, p1)
      if (no_more) PRIM_REFLECTION(S, ComputeName, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::APIntStorage *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__APIntStorage;
# define CLASSNAME clang::APIntStorage
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getValue, CLASS, P, getValue)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::IntegerLiteral *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__IntegerLiteral;
# define CLASSNAME clang::IntegerLiteral
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FixedPointLiteral *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FixedPointLiteral;
# define CLASSNAME clang::FixedPointLiteral
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::getValueAsString: {
      CHECK_TRAIT_IS_REFLPROP(getValueAsString)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) PRIM_REFLECTION(P, getValueAsString, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CharacterLiteral *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CharacterLiteral;
# define CLASSNAME clang::CharacterLiteral
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getValue, PRIM, P, getValue)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FloatingLiteral *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FloatingLiteral;
# define CLASSNAME clang::FloatingLiteral
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getValue, CLASS, P, getValue)
    case_METHOD_NOARGS(isExact, PRIM, P, isExact)
    case_METHOD_NOARGS(getValueAsApproximateDouble, PRIM, P, getValueAsApproximateDouble)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ImaginaryLiteral *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ImaginaryLiteral;
# define CLASSNAME clang::ImaginaryLiteral
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::StringLiteral *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__StringLiteral;
# define CLASSNAME clang::StringLiteral
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getString, CLASS, P, getString)
    case_METHOD_NOARGS(getBytes, CLASS, P, getBytes)
    case currefl::getCodeUnit: {
      CHECK_TRAIT_IS_REFLPROP(getCodeUnit)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) PRIM_REFLECTION(P, getCodeUnit, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getByteLength, PRIM, P, getByteLength)
    case_METHOD_NOARGS(getLength, PRIM, P, getLength)
    case_METHOD_NOARGS(getCharByteWidth, PRIM, P, getCharByteWidth)
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(isAscii, PRIM, P, isAscii)
    case_METHOD_NOARGS(isWide, PRIM, P, isWide)
    case_METHOD_NOARGS(isUTF8, PRIM, P, isUTF8)
    case_METHOD_NOARGS(isUTF16, PRIM, P, isUTF16)
    case_METHOD_NOARGS(isUTF32, PRIM, P, isUTF32)
    case_METHOD_NOARGS(isPascal, PRIM, P, isPascal)
    case_METHOD_NOARGS(containsNonAscii, PRIM, P, containsNonAscii)
    case_METHOD_NOARGS(containsNonAsciiOrNull, PRIM, P, containsNonAsciiOrNull)
    case_METHOD_NOARGS(getNumConcatenated, PRIM, P, getNumConcatenated)
    case currefl::getStrTokenLoc: {
      CHECK_TRAIT_IS_REFLPROP(getStrTokenLoc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getStrTokenLoc, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ParenExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ParenExpr;
# define CLASSNAME clang::ParenExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getLParen, CLASS, P, getLParen)
    case_METHOD_NOARGS(getRParen, CLASS, P, getRParen)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UnaryOperator *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UnaryOperator;
# define CLASSNAME clang::UnaryOperator
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOpcode, PRIM, P, getOpcode)
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(canOverflow, PRIM, P, canOverflow)
    case currefl::isPostfix: {
      CHECK_TRAIT_IS_REFLPROP(isPostfix)
      TRY_LOAD_VAL(enum clang::UnaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isPostfix, p0)
      TOO_MANY;
    }
    case currefl::isPrefix: {
      CHECK_TRAIT_IS_REFLPROP(isPrefix)
      TRY_LOAD_VAL(enum clang::UnaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isPrefix, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isPrefix1, PRIM, P, isPrefix)
    case_METHOD_NOARGS(isPostfix1, PRIM, P, isPostfix)
    case currefl::isIncrementOp: {
      CHECK_TRAIT_IS_REFLPROP(isIncrementOp)
      TRY_LOAD_VAL(enum clang::UnaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isIncrementOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isIncrementOp1, PRIM, P, isIncrementOp)
    case currefl::isDecrementOp: {
      CHECK_TRAIT_IS_REFLPROP(isDecrementOp)
      TRY_LOAD_VAL(enum clang::UnaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isDecrementOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isDecrementOp1, PRIM, P, isDecrementOp)
    case currefl::isIncrementDecrementOp: {
      CHECK_TRAIT_IS_REFLPROP(isIncrementDecrementOp)
      TRY_LOAD_VAL(enum clang::UnaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isIncrementDecrementOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isIncrementDecrementOp1, PRIM, P, isIncrementDecrementOp)
    case currefl::isArithmeticOp: {
      CHECK_TRAIT_IS_REFLPROP(isArithmeticOp)
      TRY_LOAD_VAL(enum clang::UnaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isArithmeticOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isArithmeticOp1, PRIM, P, isArithmeticOp)
    case currefl::getOpcodeStr: {
      CHECK_TRAIT_IS_REFLPROP(getOpcodeStr)
      TRY_LOAD_VAL(enum clang::UnaryOperatorKind, p0)
      if (no_more) CLASS_REFLECTION(S, getOpcodeStr, p0)
      TOO_MANY;
    }
    case currefl::getOverloadedOpcode: {
      CHECK_TRAIT_IS_REFLPROP(getOverloadedOpcode)
      TRY_LOAD_VAL(enum clang::OverloadedOperatorKind, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) PRIM_REFLECTION(S, getOverloadedOpcode, p0, p1)
      TOO_MANY;
    }
    case currefl::getOverloadedOperator: {
      CHECK_TRAIT_IS_REFLPROP(getOverloadedOperator)
      TRY_LOAD_VAL(enum clang::UnaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, getOverloadedOperator, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OffsetOfNode *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__OffsetOfNode;
# define CLASSNAME clang::OffsetOfNode
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(getArrayExprIndex, PRIM, P, getArrayExprIndex)
    case_METHOD_NOARGS(getField, CLASS, P, getField)
    case_METHOD_NOARGS(getFieldName, CLASS, P, getFieldName)
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OffsetOfExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OffsetOfExpr;
# define CLASSNAME clang::OffsetOfExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
    case currefl::getComponent: {
      CHECK_TRAIT_IS_REFLPROP(getComponent)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getComponent, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumComponents, PRIM, P, getNumComponents)
    case currefl::getIndexExpr: {
      CHECK_TRAIT_IS_REFLPROP(getIndexExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getIndexExpr, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumExpressions, PRIM, P, getNumExpressions)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UnaryExprOrTypeTraitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UnaryExprOrTypeTraitExpr;
# define CLASSNAME clang::UnaryExprOrTypeTraitExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(isArgumentType, PRIM, P, isArgumentType)
    case_METHOD_NOARGS(getArgumentType, CLASS, P, getArgumentType)
    case_METHOD_NOARGS(getArgumentTypeInfo, CLASS, P, getArgumentTypeInfo)
    case_METHOD_NOARGS(getArgumentExpr, CLASS, P, getArgumentExpr)
    case_METHOD_NOARGS(getTypeOfArgument, CLASS, P, getTypeOfArgument)
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ArraySubscriptExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ArraySubscriptExpr;
# define CLASSNAME clang::ArraySubscriptExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLHS, CLASS, P, getLHS)
    case_METHOD_NOARGS(getRHS, CLASS, P, getRHS)
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(getIdx, CLASS, P, getIdx)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getRBracketLoc, CLASS, P, getRBracketLoc)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CallExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CallExpr;
# define CLASSNAME clang::CallExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCallee, CLASS, P, getCallee)
    case_METHOD_NOARGS(getCalleeDecl, CLASS, P, getCalleeDecl)
    case_METHOD_NOARGS(getDirectCallee, CLASS, P, getDirectCallee)
    case_METHOD_NOARGS(getNumArgs, PRIM, P, getNumArgs)
    case_METHOD_NOARGS(getArgs, CLASS, P, getArgs)
    case currefl::getArg: {
      CHECK_TRAIT_IS_REFLPROP(getArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArg, p0)
      TOO_MANY;
    }
    case currefl::arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, arguments)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumCommas, PRIM, P, getNumCommas)
    case_METHOD_NOARGS(getBuiltinCallee, PRIM, P, getBuiltinCallee)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(isCallToStdMove, PRIM, P, isCallToStdMove)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::MemberExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__MemberExpr;
# define CLASSNAME clang::MemberExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(getMemberDecl, CLASS, P, getMemberDecl)
    case_METHOD_NOARGS(getFoundDecl, CLASS, P, getFoundDecl)
    case_METHOD_NOARGS(hasQualifier, PRIM, P, hasQualifier)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getTemplateKeywordLoc, CLASS, P, getTemplateKeywordLoc)
    case_METHOD_NOARGS(getLAngleLoc, CLASS, P, getLAngleLoc)
    case_METHOD_NOARGS(getRAngleLoc, CLASS, P, getRAngleLoc)
    case_METHOD_NOARGS(hasTemplateKeyword, PRIM, P, hasTemplateKeyword)
    case_METHOD_NOARGS(hasExplicitTemplateArgs, PRIM, P, hasExplicitTemplateArgs)
    case_METHOD_NOARGS(getTemplateArgs, CLASS, P, getTemplateArgs)
    case_METHOD_NOARGS(getNumTemplateArgs, PRIM, P, getNumTemplateArgs)
    case currefl::template_arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::template_arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, template_arguments)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getMemberNameInfo, CLASS, P, getMemberNameInfo)
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(isArrow, PRIM, P, isArrow)
    case_METHOD_NOARGS(getMemberLoc, CLASS, P, getMemberLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case_METHOD_NOARGS(isImplicitAccess, PRIM, P, isImplicitAccess)
    case_METHOD_NOARGS(hadMultipleCandidates, PRIM, P, hadMultipleCandidates)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CompoundLiteralExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CompoundLiteralExpr;
# define CLASSNAME clang::CompoundLiteralExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getInitializer, CLASS, P, getInitializer)
    case_METHOD_NOARGS(isFileScope, PRIM, P, isFileScope)
    case_METHOD_NOARGS(getLParenLoc, CLASS, P, getLParenLoc)
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CastExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CastExpr;
# define CLASSNAME clang::CastExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCastKind, PRIM, P, getCastKind)
    case currefl::getCastKindName: {
      CHECK_TRAIT_IS_REFLPROP(getCastKindName)
      TRY_LOAD_VAL(enum clang::CastKind, p0)
      if (no_more) PRIM_REFLECTION(S, getCastKindName, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getCastKindName1, PRIM, P, getCastKindName)
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(getSubExprAsWritten, CLASS, P, getSubExprAsWritten)
    case_METHOD_NOARGS(getConversionFunction, CLASS, P, getConversionFunction)
    case_METHOD_NOARGS(path_empty, PRIM, P, path_empty)
    case_METHOD_NOARGS(path_size, PRIM, P, path_size)
    case_METHOD_NOARGS(getTargetUnionField, CLASS, P, getTargetUnionField)
    case currefl::getTargetFieldForToUnionCast: {
      CHECK_TRAIT_IS_REFLPROP(getTargetFieldForToUnionCast)
      TRY_LOAD_VAL(class clang::QualType, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) CLASS_REFLECTION(S, getTargetFieldForToUnionCast, p0, p1)
      TOO_MANY;
    }
    case currefl::getTargetFieldForToUnionCast1: {
      CHECK_TRAIT_IS_REFLPROP(getTargetFieldForToUnionCast1)
      TRY_LOAD_VAL(const class clang::RecordDecl *, p0)
      TRY_LOAD_VAL(class clang::QualType, p1)
      if (no_more) CLASS_REFLECTION(S, getTargetFieldForToUnionCast, p0, p1)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ImplicitCastExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ImplicitCastExpr;
# define CLASSNAME clang::ImplicitCastExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isPartOfExplicitCast, PRIM, P, isPartOfExplicitCast)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ExplicitCastExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ExplicitCastExpr;
# define CLASSNAME clang::ExplicitCastExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTypeInfoAsWritten, CLASS, P, getTypeInfoAsWritten)
    case_METHOD_NOARGS(getTypeAsWritten, CLASS, P, getTypeAsWritten)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CStyleCastExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CStyleCastExpr;
# define CLASSNAME clang::CStyleCastExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLParenLoc, CLASS, P, getLParenLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::BinaryOperator *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__BinaryOperator;
# define CLASSNAME clang::BinaryOperator
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(getOpcode, PRIM, P, getOpcode)
    case_METHOD_NOARGS(getLHS, CLASS, P, getLHS)
    case_METHOD_NOARGS(getRHS, CLASS, P, getRHS)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::getOpcodeStr: {
      CHECK_TRAIT_IS_REFLPROP(getOpcodeStr)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) CLASS_REFLECTION(S, getOpcodeStr, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getOpcodeStr1, CLASS, P, getOpcodeStr)
    case currefl::getOverloadedOpcode: {
      CHECK_TRAIT_IS_REFLPROP(getOverloadedOpcode)
      TRY_LOAD_VAL(enum clang::OverloadedOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, getOverloadedOpcode, p0)
      TOO_MANY;
    }
    case currefl::getOverloadedOperator: {
      CHECK_TRAIT_IS_REFLPROP(getOverloadedOperator)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, getOverloadedOperator, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isPtrMemOp, PRIM, P, isPtrMemOp)
    case currefl::isMultiplicativeOp: {
      CHECK_TRAIT_IS_REFLPROP(isMultiplicativeOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isMultiplicativeOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isMultiplicativeOp1, PRIM, P, isMultiplicativeOp)
    case currefl::isAdditiveOp: {
      CHECK_TRAIT_IS_REFLPROP(isAdditiveOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isAdditiveOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isAdditiveOp1, PRIM, P, isAdditiveOp)
    case currefl::isShiftOp: {
      CHECK_TRAIT_IS_REFLPROP(isShiftOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isShiftOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isShiftOp1, PRIM, P, isShiftOp)
    case currefl::isBitwiseOp: {
      CHECK_TRAIT_IS_REFLPROP(isBitwiseOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isBitwiseOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isBitwiseOp1, PRIM, P, isBitwiseOp)
    case currefl::isRelationalOp: {
      CHECK_TRAIT_IS_REFLPROP(isRelationalOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isRelationalOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isRelationalOp1, PRIM, P, isRelationalOp)
    case currefl::isEqualityOp: {
      CHECK_TRAIT_IS_REFLPROP(isEqualityOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isEqualityOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isEqualityOp1, PRIM, P, isEqualityOp)
    case currefl::isComparisonOp: {
      CHECK_TRAIT_IS_REFLPROP(isComparisonOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isComparisonOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isComparisonOp1, PRIM, P, isComparisonOp)
    case currefl::negateComparisonOp: {
      CHECK_TRAIT_IS_REFLPROP(negateComparisonOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, negateComparisonOp, p0)
      TOO_MANY;
    }
    case currefl::reverseComparisonOp: {
      CHECK_TRAIT_IS_REFLPROP(reverseComparisonOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, reverseComparisonOp, p0)
      TOO_MANY;
    }
    case currefl::isLogicalOp: {
      CHECK_TRAIT_IS_REFLPROP(isLogicalOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isLogicalOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isLogicalOp1, PRIM, P, isLogicalOp)
    case currefl::isAssignmentOp: {
      CHECK_TRAIT_IS_REFLPROP(isAssignmentOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isAssignmentOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isAssignmentOp1, PRIM, P, isAssignmentOp)
    case currefl::isCompoundAssignmentOp: {
      CHECK_TRAIT_IS_REFLPROP(isCompoundAssignmentOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isCompoundAssignmentOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isCompoundAssignmentOp1, PRIM, P, isCompoundAssignmentOp)
    case currefl::getOpForCompoundAssignment: {
      CHECK_TRAIT_IS_REFLPROP(getOpForCompoundAssignment)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, getOpForCompoundAssignment, p0)
      TOO_MANY;
    }
    case currefl::isShiftAssignOp: {
      CHECK_TRAIT_IS_REFLPROP(isShiftAssignOp)
      TRY_LOAD_VAL(enum clang::BinaryOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isShiftAssignOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isShiftAssignOp1, PRIM, P, isShiftAssignOp)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getFPFeatures, CLASS, P, getFPFeatures)
    case_METHOD_NOARGS(isFPContractableWithinStatement, PRIM, P, isFPContractableWithinStatement)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CompoundAssignOperator *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CompoundAssignOperator;
# define CLASSNAME clang::CompoundAssignOperator
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getComputationLHSType, CLASS, P, getComputationLHSType)
    case_METHOD_NOARGS(getComputationResultType, CLASS, P, getComputationResultType)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::AbstractConditionalOperator *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__AbstractConditionalOperator;
# define CLASSNAME clang::AbstractConditionalOperator
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCond, CLASS, P, getCond)
    case_METHOD_NOARGS(getTrueExpr, CLASS, P, getTrueExpr)
    case_METHOD_NOARGS(getFalseExpr, CLASS, P, getFalseExpr)
    case_METHOD_NOARGS(getQuestionLoc, CLASS, P, getQuestionLoc)
    case_METHOD_NOARGS(getColonLoc, CLASS, P, getColonLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ConditionalOperator *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ConditionalOperator;
# define CLASSNAME clang::ConditionalOperator
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCond, CLASS, P, getCond)
    case_METHOD_NOARGS(getTrueExpr, CLASS, P, getTrueExpr)
    case_METHOD_NOARGS(getFalseExpr, CLASS, P, getFalseExpr)
    case_METHOD_NOARGS(getLHS, CLASS, P, getLHS)
    case_METHOD_NOARGS(getRHS, CLASS, P, getRHS)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::BinaryConditionalOperator *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__BinaryConditionalOperator;
# define CLASSNAME clang::BinaryConditionalOperator
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCommon, CLASS, P, getCommon)
    case_METHOD_NOARGS(getOpaqueValue, CLASS, P, getOpaqueValue)
    case_METHOD_NOARGS(getCond, CLASS, P, getCond)
    case_METHOD_NOARGS(getTrueExpr, CLASS, P, getTrueExpr)
    case_METHOD_NOARGS(getFalseExpr, CLASS, P, getFalseExpr)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::AddrLabelExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__AddrLabelExpr;
# define CLASSNAME clang::AddrLabelExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAmpAmpLoc, CLASS, P, getAmpAmpLoc)
    case_METHOD_NOARGS(getLabelLoc, CLASS, P, getLabelLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getLabel, CLASS, P, getLabel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::StmtExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__StmtExpr;
# define CLASSNAME clang::StmtExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSubStmt, CLASS, P, getSubStmt)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getLParenLoc, CLASS, P, getLParenLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ShuffleVectorExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ShuffleVectorExpr;
# define CLASSNAME clang::ShuffleVectorExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBuiltinLoc, CLASS, P, getBuiltinLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumSubExprs, PRIM, P, getNumSubExprs)
    case currefl::getExpr: {
      CHECK_TRAIT_IS_REFLPROP(getExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getExpr, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ConvertVectorExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ConvertVectorExpr;
# define CLASSNAME clang::ConvertVectorExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSrcExpr, CLASS, P, getSrcExpr)
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
    case_METHOD_NOARGS(getBuiltinLoc, CLASS, P, getBuiltinLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ChooseExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ChooseExpr;
# define CLASSNAME clang::ChooseExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isConditionTrue, PRIM, P, isConditionTrue)
    case_METHOD_NOARGS(isConditionDependent, PRIM, P, isConditionDependent)
    case_METHOD_NOARGS(getChosenSubExpr, CLASS, P, getChosenSubExpr)
    case_METHOD_NOARGS(getCond, CLASS, P, getCond)
    case_METHOD_NOARGS(getLHS, CLASS, P, getLHS)
    case_METHOD_NOARGS(getRHS, CLASS, P, getRHS)
    case_METHOD_NOARGS(getBuiltinLoc, CLASS, P, getBuiltinLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::GNUNullExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__GNUNullExpr;
# define CLASSNAME clang::GNUNullExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTokenLocation, CLASS, P, getTokenLocation)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::VAArgExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__VAArgExpr;
# define CLASSNAME clang::VAArgExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(isMicrosoftABI, PRIM, P, isMicrosoftABI)
    case_METHOD_NOARGS(getWrittenTypeInfo, CLASS, P, getWrittenTypeInfo)
    case_METHOD_NOARGS(getBuiltinLoc, CLASS, P, getBuiltinLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::InitListExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__InitListExpr;
# define CLASSNAME clang::InitListExpr
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(P)
      TOO_MANY;
};
    case_METHOD_NOARGS(getNumInits, PRIM, P, getNumInits)
    case_METHOD_NOARGS(getInits, CLASS, P, getInits)
    case currefl::inits: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::inits))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, inits)
      TOO_MANY;
    }
    case currefl::getInit: {
      CHECK_TRAIT_IS_REFLPROP(getInit)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getInit, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getArrayFiller, CLASS, P, getArrayFiller)
    case_METHOD_NOARGS(hasArrayFiller, PRIM, P, hasArrayFiller)
    case_METHOD_NOARGS(getInitializedFieldInUnion, CLASS, P, getInitializedFieldInUnion)
    case_METHOD_NOARGS(isExplicit, PRIM, P, isExplicit)
    case_METHOD_NOARGS(isStringLiteralInit, PRIM, P, isStringLiteralInit)
    case_METHOD_NOARGS(isTransparent, PRIM, P, isTransparent)
    case_METHOD_NOARGS(getLBraceLoc, CLASS, P, getLBraceLoc)
    case_METHOD_NOARGS(getRBraceLoc, CLASS, P, getRBraceLoc)
    case_METHOD_NOARGS(isSemanticForm, PRIM, P, isSemanticForm)
    case_METHOD_NOARGS(getSemanticForm, CLASS, P, getSemanticForm)
    case_METHOD_NOARGS(isSyntacticForm, PRIM, P, isSyntacticForm)
    case_METHOD_NOARGS(getSyntacticForm, CLASS, P, getSyntacticForm)
    case_METHOD_NOARGS(hadArrayRangeDesignator, PRIM, P, hadArrayRangeDesignator)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DesignatedInitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DesignatedInitExpr;
# define CLASSNAME clang::DesignatedInitExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(size, PRIM, P, size)
    case currefl::designators: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::designators))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, designators)
      TOO_MANY;
    }
    case currefl::getDesignator: {
      CHECK_TRAIT_IS_REFLPROP(getDesignator)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getDesignator, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getEqualOrColonLoc, CLASS, P, getEqualOrColonLoc)
    case_METHOD_NOARGS(usesGNUSyntax, PRIM, P, usesGNUSyntax)
    case_METHOD_NOARGS(getInit, CLASS, P, getInit)
    case_METHOD_NOARGS(getNumSubExprs, PRIM, P, getNumSubExprs)
    case currefl::getSubExpr: {
      CHECK_TRAIT_IS_REFLPROP(getSubExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getSubExpr, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getDesignatorsSourceRange, CLASS, P, getDesignatorsSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DesignatedInitExpr::Designator *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DesignatedInitExpr__Designator;
# define CLASSNAME clang::DesignatedInitExpr::Designator
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isFieldDesignator, PRIM, P, isFieldDesignator)
    case_METHOD_NOARGS(isArrayDesignator, PRIM, P, isArrayDesignator)
    case_METHOD_NOARGS(isArrayRangeDesignator, PRIM, P, isArrayRangeDesignator)
    case_METHOD_NOARGS(getFieldName, CLASS, P, getFieldName)
    case_METHOD_NOARGS(getField, CLASS, P, getField)
    case_METHOD_NOARGS(getDotLoc, CLASS, P, getDotLoc)
    case_METHOD_NOARGS(getFieldLoc, CLASS, P, getFieldLoc)
    case_METHOD_NOARGS(getLBracketLoc, CLASS, P, getLBracketLoc)
    case_METHOD_NOARGS(getRBracketLoc, CLASS, P, getRBracketLoc)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, P, getEllipsisLoc)
    case_METHOD_NOARGS(getFirstExprIndex, PRIM, P, getFirstExprIndex)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DesignatedInitExpr::Designator X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DesignatedInitExpr__Designator;
# define CLASSNAME clang::DesignatedInitExpr::Designator
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isFieldDesignator, PRIM, V, isFieldDesignator)
    case_METHOD_NOARGS(isArrayDesignator, PRIM, V, isArrayDesignator)
    case_METHOD_NOARGS(isArrayRangeDesignator, PRIM, V, isArrayRangeDesignator)
    case_METHOD_NOARGS(getFieldName, CLASS, V, getFieldName)
    case_METHOD_NOARGS(getField, CLASS, V, getField)
    case_METHOD_NOARGS(getDotLoc, CLASS, V, getDotLoc)
    case_METHOD_NOARGS(getFieldLoc, CLASS, V, getFieldLoc)
    case_METHOD_NOARGS(getLBracketLoc, CLASS, V, getLBracketLoc)
    case_METHOD_NOARGS(getRBracketLoc, CLASS, V, getRBracketLoc)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, V, getEllipsisLoc)
    case_METHOD_NOARGS(getFirstExprIndex, PRIM, V, getFirstExprIndex)
    case_METHOD_NOARGS(getLocStart, CLASS, V, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, V, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, V, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, V, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, V, getSourceRange)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::NoInitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__NoInitExpr;
# define CLASSNAME clang::NoInitExpr
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DesignatedInitUpdateExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DesignatedInitUpdateExpr;
# define CLASSNAME clang::DesignatedInitUpdateExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(getUpdater, CLASS, P, getUpdater)
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ArrayInitLoopExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ArrayInitLoopExpr;
# define CLASSNAME clang::ArrayInitLoopExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCommonExpr, CLASS, P, getCommonExpr)
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(getArraySize, CLASS, P, getArraySize)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ArrayInitIndexExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ArrayInitIndexExpr;
# define CLASSNAME clang::ArrayInitIndexExpr
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ImplicitValueInitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ImplicitValueInitExpr;
# define CLASSNAME clang::ImplicitValueInitExpr
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ParenListExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ParenListExpr;
# define CLASSNAME clang::ParenListExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNumExprs, PRIM, P, getNumExprs)
    case currefl::getExpr: {
      CHECK_TRAIT_IS_REFLPROP(getExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getExpr, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLParenLoc, CLASS, P, getLParenLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::GenericSelectionExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__GenericSelectionExpr;
# define CLASSNAME clang::GenericSelectionExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNumAssocs, PRIM, P, getNumAssocs)
    case_METHOD_NOARGS(getGenericLoc, CLASS, P, getGenericLoc)
    case_METHOD_NOARGS(getDefaultLoc, CLASS, P, getDefaultLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case currefl::getAssocExpr: {
      CHECK_TRAIT_IS_REFLPROP(getAssocExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getAssocExpr, p0)
      TOO_MANY;
    }
    case currefl::getAssocExprs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getAssocExprs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getAssocExprs)
      TOO_MANY;
    }
    case currefl::getAssocTypeSourceInfo: {
      CHECK_TRAIT_IS_REFLPROP(getAssocTypeSourceInfo)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getAssocTypeSourceInfo, p0)
      TOO_MANY;
    }
    case currefl::getAssocTypeSourceInfos: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getAssocTypeSourceInfos))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getAssocTypeSourceInfos)
      TOO_MANY;
    }
    case currefl::getAssocType: {
      CHECK_TRAIT_IS_REFLPROP(getAssocType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getAssocType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getControllingExpr, CLASS, P, getControllingExpr)
    case_METHOD_NOARGS(isResultDependent, PRIM, P, isResultDependent)
    case_METHOD_NOARGS(getResultIndex, PRIM, P, getResultIndex)
    case_METHOD_NOARGS(getResultExpr, CLASS, P, getResultExpr)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ExtVectorElementExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ExtVectorElementExpr;
# define CLASSNAME clang::ExtVectorElementExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(getAccessor, CLASS, P, getAccessor)
    case_METHOD_NOARGS(getAccessorLoc, CLASS, P, getAccessorLoc)
    case_METHOD_NOARGS(getNumElements, PRIM, P, getNumElements)
    case_METHOD_NOARGS(containsDuplicateElements, PRIM, P, containsDuplicateElements)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(isArrow, PRIM, P, isArrow)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::BlockExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__BlockExpr;
# define CLASSNAME clang::BlockExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBlockDecl, CLASS, P, getBlockDecl)
    case_METHOD_NOARGS(getCaretLocation, CLASS, P, getCaretLocation)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getFunctionType, CLASS, P, getFunctionType)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::AsTypeExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__AsTypeExpr;
# define CLASSNAME clang::AsTypeExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSrcExpr, CLASS, P, getSrcExpr)
    case_METHOD_NOARGS(getBuiltinLoc, CLASS, P, getBuiltinLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::PseudoObjectExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__PseudoObjectExpr;
# define CLASSNAME clang::PseudoObjectExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSyntacticForm, CLASS, P, getSyntacticForm)
    case_METHOD_NOARGS(getResultExprIndex, PRIM, P, getResultExprIndex)
    case_METHOD_NOARGS(getResultExpr, CLASS, P, getResultExpr)
    case_METHOD_NOARGS(getNumSemanticExprs, PRIM, P, getNumSemanticExprs)
    case currefl::semantics: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::semantics))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, semantics)
      TOO_MANY;
    }
    case currefl::getSemanticExpr: {
      CHECK_TRAIT_IS_REFLPROP(getSemanticExpr)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getSemanticExpr, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::AtomicExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__AtomicExpr;
# define CLASSNAME clang::AtomicExpr
  switch( (currefl::memnames)N ) {
    case currefl::getNumSubExprs: {
      CHECK_TRAIT_IS_REFLPROP(getNumSubExprs)
      TRY_LOAD_VAL(enum clang::AtomicExpr::AtomicOp, p0)
      if (no_more) PRIM_REFLECTION(S, getNumSubExprs, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getPtr, CLASS, P, getPtr)
    case_METHOD_NOARGS(getOrder, CLASS, P, getOrder)
    case_METHOD_NOARGS(getScope, CLASS, P, getScope)
    case_METHOD_NOARGS(getVal1, CLASS, P, getVal1)
    case_METHOD_NOARGS(getOrderFail, CLASS, P, getOrderFail)
    case_METHOD_NOARGS(getVal2, CLASS, P, getVal2)
    case_METHOD_NOARGS(getWeak, CLASS, P, getWeak)
    case_METHOD_NOARGS(getValueType, CLASS, P, getValueType)
    case_METHOD_NOARGS(getOp, PRIM, P, getOp)
    case_METHOD_NOARGS(getNumSubExprs1, PRIM, P, getNumSubExprs)
    case_METHOD_NOARGS(getSubExprs, CLASS, P, getSubExprs)
    case_METHOD_NOARGS(isVolatile, PRIM, P, isVolatile)
    case_METHOD_NOARGS(isCmpXChg, PRIM, P, isCmpXChg)
    case_METHOD_NOARGS(isOpenCL, PRIM, P, isOpenCL)
    case_METHOD_NOARGS(getBuiltinLoc, CLASS, P, getBuiltinLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypoExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TypoExpr;
# define CLASSNAME clang::TypoExpr
  switch( (currefl::memnames)N ) {
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::Attr *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__Attr;
# define CLASSNAME clang::Attr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(getSpellingListIndex, PRIM, P, getSpellingListIndex)
    case_METHOD_NOARGS(getSpelling, PRIM, P, getSpelling)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getRange, CLASS, P, getRange)
    case_METHOD_NOARGS(isInherited, PRIM, P, isInherited)
    case_METHOD_NOARGS(isImplicit, PRIM, P, isImplicit)
    case_METHOD_NOARGS(isPackExpansion, PRIM, P, isPackExpansion)
    case_METHOD_NOARGS(isLateParsed, PRIM, P, isLateParsed)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::InheritableAttr *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__InheritableAttr;
# define CLASSNAME clang::InheritableAttr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(shouldInheritEvenIfAlreadyPresent, PRIM, P, shouldInheritEvenIfAlreadyPresent)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Attr *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ExternalSourceSymbolAttr *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ExternalSourceSymbolAttr;
# define CLASSNAME clang::ExternalSourceSymbolAttr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSpelling, PRIM, P, getSpelling)
    case_METHOD_NOARGS(getLanguage, CLASS, P, getLanguage)
    case_METHOD_NOARGS(getLanguageLength, PRIM, P, getLanguageLength)
    case_METHOD_NOARGS(getDefinedIn, CLASS, P, getDefinedIn)
    case_METHOD_NOARGS(getDefinedInLength, PRIM, P, getDefinedInLength)
    case_METHOD_NOARGS(getGeneratedDeclaration, PRIM, P, getGeneratedDeclaration)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Attr *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::MSInheritanceAttr *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__MSInheritanceAttr;
# define CLASSNAME clang::MSInheritanceAttr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSpelling, PRIM, P, getSpelling)
    case_METHOD_NOARGS(getSemanticSpelling, PRIM, P, getSemanticSpelling)
    case_METHOD_NOARGS(getBestCase, PRIM, P, getBestCase)
    case currefl::hasVBPtrOffsetField: {
      CHECK_TRAIT_IS_REFLPROP(hasVBPtrOffsetField)
      TRY_LOAD_VAL(enum clang::MSInheritanceAttr::Spelling, p0)
      if (no_more) PRIM_REFLECTION(S, hasVBPtrOffsetField, p0)
      TOO_MANY;
    }
    case currefl::hasNVOffsetField: {
      CHECK_TRAIT_IS_REFLPROP(hasNVOffsetField)
      TRY_LOAD_VAL(bool, p0)
      TRY_LOAD_VAL(enum clang::MSInheritanceAttr::Spelling, p1)
      if (no_more) PRIM_REFLECTION(S, hasNVOffsetField, p0, p1)
      TOO_MANY;
    }
    case currefl::hasVBTableOffsetField: {
      CHECK_TRAIT_IS_REFLPROP(hasVBTableOffsetField)
      TRY_LOAD_VAL(enum clang::MSInheritanceAttr::Spelling, p0)
      if (no_more) PRIM_REFLECTION(S, hasVBTableOffsetField, p0)
      TOO_MANY;
    }
    case currefl::hasOnlyOneField: {
      CHECK_TRAIT_IS_REFLPROP(hasOnlyOneField)
      TRY_LOAD_VAL(bool, p0)
      TRY_LOAD_VAL(enum clang::MSInheritanceAttr::Spelling, p1)
      if (no_more) PRIM_REFLECTION(S, hasOnlyOneField, p0, p1)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Attr *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::MSVtorDispAttr *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__MSVtorDispAttr;
# define CLASSNAME clang::MSVtorDispAttr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSpelling, PRIM, P, getSpelling)
    case_METHOD_NOARGS(getVdm, PRIM, P, getVdm)
    case_METHOD_NOARGS(getVtorDispMode, PRIM, P, getVtorDispMode)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Attr *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::LambdaCapture *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__LambdaCapture;
# define CLASSNAME clang::LambdaCapture
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCaptureKind, PRIM, P, getCaptureKind)
    case_METHOD_NOARGS(capturesThis, PRIM, P, capturesThis)
    case_METHOD_NOARGS(capturesVariable, PRIM, P, capturesVariable)
    case_METHOD_NOARGS(capturesVLAType, PRIM, P, capturesVLAType)
    case_METHOD_NOARGS(getCapturedVar, CLASS, P, getCapturedVar)
    case_METHOD_NOARGS(isImplicit, PRIM, P, isImplicit)
    case_METHOD_NOARGS(isExplicit, PRIM, P, isExplicit)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(isPackExpansion, PRIM, P, isPackExpansion)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, P, getEllipsisLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::LambdaCapture X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__LambdaCapture;
# define CLASSNAME clang::LambdaCapture
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCaptureKind, PRIM, V, getCaptureKind)
    case_METHOD_NOARGS(capturesThis, PRIM, V, capturesThis)
    case_METHOD_NOARGS(capturesVariable, PRIM, V, capturesVariable)
    case_METHOD_NOARGS(capturesVLAType, PRIM, V, capturesVLAType)
    case_METHOD_NOARGS(getCapturedVar, CLASS, V, getCapturedVar)
    case_METHOD_NOARGS(isImplicit, PRIM, V, isImplicit)
    case_METHOD_NOARGS(isExplicit, PRIM, V, isExplicit)
    case_METHOD_NOARGS(getLocation, CLASS, V, getLocation)
    case_METHOD_NOARGS(isPackExpansion, PRIM, V, isPackExpansion)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, V, getEllipsisLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::AccessSpecDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__AccessSpecDecl;
# define CLASSNAME clang::AccessSpecDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAccessSpecifierLoc, CLASS, P, getAccessSpecifierLoc)
    case_METHOD_NOARGS(getColonLoc, CLASS, P, getColonLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXBaseSpecifier *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__CXXBaseSpecifier;
# define CLASSNAME clang::CXXBaseSpecifier
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getBaseTypeLoc, CLASS, P, getBaseTypeLoc)
    case_METHOD_NOARGS(isVirtual, PRIM, P, isVirtual)
    case_METHOD_NOARGS(isBaseOfClass, PRIM, P, isBaseOfClass)
    case_METHOD_NOARGS(isPackExpansion, PRIM, P, isPackExpansion)
    case_METHOD_NOARGS(getInheritConstructors, PRIM, P, getInheritConstructors)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, P, getEllipsisLoc)
    case_METHOD_NOARGS(getAccessSpecifier, PRIM, P, getAccessSpecifier)
    case_METHOD_NOARGS(getAccessSpecifierAsWritten, PRIM, P, getAccessSpecifierAsWritten)
    case_METHOD_NOARGS(getType, CLASS, P, getType)
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXBaseSpecifier X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__CXXBaseSpecifier;
# define CLASSNAME clang::CXXBaseSpecifier
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSourceRange, CLASS, V, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, V, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, V, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, V, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, V, getEndLoc)
    case_METHOD_NOARGS(getBaseTypeLoc, CLASS, V, getBaseTypeLoc)
    case_METHOD_NOARGS(isVirtual, PRIM, V, isVirtual)
    case_METHOD_NOARGS(isBaseOfClass, PRIM, V, isBaseOfClass)
    case_METHOD_NOARGS(isPackExpansion, PRIM, V, isPackExpansion)
    case_METHOD_NOARGS(getInheritConstructors, PRIM, V, getInheritConstructors)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, V, getEllipsisLoc)
    case_METHOD_NOARGS(getAccessSpecifier, PRIM, V, getAccessSpecifier)
    case_METHOD_NOARGS(getAccessSpecifierAsWritten, PRIM, V, getAccessSpecifierAsWritten)
    case_METHOD_NOARGS(getType, CLASS, V, getType)
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, V, getTypeSourceInfo)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXRecordDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXRecordDecl;
# define CLASSNAME clang::CXXRecordDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getPreviousDecl, CLASS, P, getPreviousDecl)
    case_METHOD_NOARGS(getMostRecentDecl, CLASS, P, getMostRecentDecl)
    case_METHOD_NOARGS(getMostRecentNonInjectedDecl, CLASS, P, getMostRecentNonInjectedDecl)
    case_METHOD_NOARGS(getDefinition, CLASS, P, getDefinition)
    case_METHOD_NOARGS(hasDefinition, PRIM, P, hasDefinition)
    case_METHOD_NOARGS(isDynamicClass, PRIM, P, isDynamicClass)
    case_METHOD_NOARGS(mayBeDynamicClass, PRIM, P, mayBeDynamicClass)
    case_METHOD_NOARGS(mayBeNonDynamicClass, PRIM, P, mayBeNonDynamicClass)
    case_METHOD_NOARGS(isParsingBaseSpecifiers, PRIM, P, isParsingBaseSpecifiers)
    case_METHOD_NOARGS(isInstantiation, PRIM, P, isInstantiation)
    case_METHOD_NOARGS(getODRHash, PRIM, P, getODRHash)
    case_METHOD_NOARGS(getNumBases, PRIM, P, getNumBases)
    case currefl::bases: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::bases))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, bases)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumVBases, PRIM, P, getNumVBases)
    case currefl::vbases: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::vbases))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, vbases)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasAnyDependentBases, PRIM, P, hasAnyDependentBases)
    case currefl::methods: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::methods))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, methods)
      TOO_MANY;
    }
    case currefl::ctors: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::ctors))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, ctors)
      TOO_MANY;
    }
    case currefl::friends: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::friends))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, friends)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasFriends, PRIM, P, hasFriends)
    case_METHOD_NOARGS(defaultedCopyConstructorIsDeleted, PRIM, P, defaultedCopyConstructorIsDeleted)
    case_METHOD_NOARGS(defaultedMoveConstructorIsDeleted, PRIM, P, defaultedMoveConstructorIsDeleted)
    case_METHOD_NOARGS(defaultedDestructorIsDeleted, PRIM, P, defaultedDestructorIsDeleted)
    case_METHOD_NOARGS(hasSimpleCopyConstructor, PRIM, P, hasSimpleCopyConstructor)
    case_METHOD_NOARGS(hasSimpleMoveConstructor, PRIM, P, hasSimpleMoveConstructor)
    case_METHOD_NOARGS(hasSimpleMoveAssignment, PRIM, P, hasSimpleMoveAssignment)
    case_METHOD_NOARGS(hasSimpleDestructor, PRIM, P, hasSimpleDestructor)
    case_METHOD_NOARGS(hasDefaultConstructor, PRIM, P, hasDefaultConstructor)
    case_METHOD_NOARGS(needsImplicitDefaultConstructor, PRIM, P, needsImplicitDefaultConstructor)
    case_METHOD_NOARGS(hasUserDeclaredConstructor, PRIM, P, hasUserDeclaredConstructor)
    case_METHOD_NOARGS(hasUserProvidedDefaultConstructor, PRIM, P, hasUserProvidedDefaultConstructor)
    case_METHOD_NOARGS(hasUserDeclaredCopyConstructor, PRIM, P, hasUserDeclaredCopyConstructor)
    case_METHOD_NOARGS(needsImplicitCopyConstructor, PRIM, P, needsImplicitCopyConstructor)
    case_METHOD_NOARGS(needsOverloadResolutionForCopyConstructor, PRIM, P, needsOverloadResolutionForCopyConstructor)
    case_METHOD_NOARGS(implicitCopyConstructorHasConstParam, PRIM, P, implicitCopyConstructorHasConstParam)
    case_METHOD_NOARGS(hasCopyConstructorWithConstParam, PRIM, P, hasCopyConstructorWithConstParam)
    case_METHOD_NOARGS(hasUserDeclaredMoveOperation, PRIM, P, hasUserDeclaredMoveOperation)
    case_METHOD_NOARGS(hasUserDeclaredMoveConstructor, PRIM, P, hasUserDeclaredMoveConstructor)
    case_METHOD_NOARGS(hasMoveConstructor, PRIM, P, hasMoveConstructor)
    case_METHOD_NOARGS(needsImplicitMoveConstructor, PRIM, P, needsImplicitMoveConstructor)
    case_METHOD_NOARGS(needsOverloadResolutionForMoveConstructor, PRIM, P, needsOverloadResolutionForMoveConstructor)
    case_METHOD_NOARGS(hasUserDeclaredCopyAssignment, PRIM, P, hasUserDeclaredCopyAssignment)
    case_METHOD_NOARGS(needsImplicitCopyAssignment, PRIM, P, needsImplicitCopyAssignment)
    case_METHOD_NOARGS(needsOverloadResolutionForCopyAssignment, PRIM, P, needsOverloadResolutionForCopyAssignment)
    case_METHOD_NOARGS(implicitCopyAssignmentHasConstParam, PRIM, P, implicitCopyAssignmentHasConstParam)
    case_METHOD_NOARGS(hasCopyAssignmentWithConstParam, PRIM, P, hasCopyAssignmentWithConstParam)
    case_METHOD_NOARGS(hasUserDeclaredMoveAssignment, PRIM, P, hasUserDeclaredMoveAssignment)
    case_METHOD_NOARGS(hasMoveAssignment, PRIM, P, hasMoveAssignment)
    case_METHOD_NOARGS(needsImplicitMoveAssignment, PRIM, P, needsImplicitMoveAssignment)
    case_METHOD_NOARGS(needsOverloadResolutionForMoveAssignment, PRIM, P, needsOverloadResolutionForMoveAssignment)
    case_METHOD_NOARGS(hasUserDeclaredDestructor, PRIM, P, hasUserDeclaredDestructor)
    case_METHOD_NOARGS(needsImplicitDestructor, PRIM, P, needsImplicitDestructor)
    case_METHOD_NOARGS(needsOverloadResolutionForDestructor, PRIM, P, needsOverloadResolutionForDestructor)
    case_METHOD_NOARGS(isLambda, PRIM, P, isLambda)
    case_METHOD_NOARGS(isGenericLambda, PRIM, P, isGenericLambda)
    case_METHOD_NOARGS(getLambdaCallOperator, CLASS, P, getLambdaCallOperator)
    case_METHOD_NOARGS(getLambdaStaticInvoker, CLASS, P, getLambdaStaticInvoker)
    case_METHOD_NOARGS(getGenericLambdaTemplateParameterList, CLASS, P, getGenericLambdaTemplateParameterList)
    case_METHOD_NOARGS(getLambdaCaptureDefault, PRIM, P, getLambdaCaptureDefault)
    case currefl::captures: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::captures))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, captures)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isAggregate, PRIM, P, isAggregate)
    case_METHOD_NOARGS(hasInClassInitializer, PRIM, P, hasInClassInitializer)
    case_METHOD_NOARGS(hasUninitializedReferenceMember, PRIM, P, hasUninitializedReferenceMember)
    case_METHOD_NOARGS(isPOD, PRIM, P, isPOD)
    case_METHOD_NOARGS(isCLike, PRIM, P, isCLike)
    case_METHOD_NOARGS(isEmpty, PRIM, P, isEmpty)
    case_METHOD_NOARGS(hasDirectFields, PRIM, P, hasDirectFields)
    case_METHOD_NOARGS(isPolymorphic, PRIM, P, isPolymorphic)
    case_METHOD_NOARGS(isAbstract, PRIM, P, isAbstract)
    case_METHOD_NOARGS(isStandardLayout, PRIM, P, isStandardLayout)
    case_METHOD_NOARGS(isCXX11StandardLayout, PRIM, P, isCXX11StandardLayout)
    case_METHOD_NOARGS(hasMutableFields, PRIM, P, hasMutableFields)
    case_METHOD_NOARGS(hasVariantMembers, PRIM, P, hasVariantMembers)
    case_METHOD_NOARGS(hasTrivialDefaultConstructor, PRIM, P, hasTrivialDefaultConstructor)
    case_METHOD_NOARGS(hasNonTrivialDefaultConstructor, PRIM, P, hasNonTrivialDefaultConstructor)
    case_METHOD_NOARGS(hasConstexprNonCopyMoveConstructor, PRIM, P, hasConstexprNonCopyMoveConstructor)
    case_METHOD_NOARGS(defaultedDefaultConstructorIsConstexpr, PRIM, P, defaultedDefaultConstructorIsConstexpr)
    case_METHOD_NOARGS(hasConstexprDefaultConstructor, PRIM, P, hasConstexprDefaultConstructor)
    case_METHOD_NOARGS(hasTrivialCopyConstructor, PRIM, P, hasTrivialCopyConstructor)
    case_METHOD_NOARGS(hasTrivialCopyConstructorForCall, PRIM, P, hasTrivialCopyConstructorForCall)
    case_METHOD_NOARGS(hasNonTrivialCopyConstructor, PRIM, P, hasNonTrivialCopyConstructor)
    case_METHOD_NOARGS(hasNonTrivialCopyConstructorForCall, PRIM, P, hasNonTrivialCopyConstructorForCall)
    case_METHOD_NOARGS(hasTrivialMoveConstructor, PRIM, P, hasTrivialMoveConstructor)
    case_METHOD_NOARGS(hasTrivialMoveConstructorForCall, PRIM, P, hasTrivialMoveConstructorForCall)
    case_METHOD_NOARGS(hasNonTrivialMoveConstructor, PRIM, P, hasNonTrivialMoveConstructor)
    case_METHOD_NOARGS(hasNonTrivialMoveConstructorForCall, PRIM, P, hasNonTrivialMoveConstructorForCall)
    case_METHOD_NOARGS(hasTrivialCopyAssignment, PRIM, P, hasTrivialCopyAssignment)
    case_METHOD_NOARGS(hasNonTrivialCopyAssignment, PRIM, P, hasNonTrivialCopyAssignment)
    case_METHOD_NOARGS(hasTrivialMoveAssignment, PRIM, P, hasTrivialMoveAssignment)
    case_METHOD_NOARGS(hasNonTrivialMoveAssignment, PRIM, P, hasNonTrivialMoveAssignment)
    case_METHOD_NOARGS(hasTrivialDestructor, PRIM, P, hasTrivialDestructor)
    case_METHOD_NOARGS(hasTrivialDestructorForCall, PRIM, P, hasTrivialDestructorForCall)
    case_METHOD_NOARGS(hasNonTrivialDestructor, PRIM, P, hasNonTrivialDestructor)
    case_METHOD_NOARGS(hasNonTrivialDestructorForCall, PRIM, P, hasNonTrivialDestructorForCall)
    case_METHOD_NOARGS(allowConstDefaultInit, PRIM, P, allowConstDefaultInit)
    case_METHOD_NOARGS(hasIrrelevantDestructor, PRIM, P, hasIrrelevantDestructor)
    case_METHOD_NOARGS(hasNonLiteralTypeFieldsOrBases, PRIM, P, hasNonLiteralTypeFieldsOrBases)
    case_METHOD_NOARGS(hasInheritedConstructor, PRIM, P, hasInheritedConstructor)
    case_METHOD_NOARGS(hasInheritedAssignment, PRIM, P, hasInheritedAssignment)
    case_METHOD_NOARGS(isTriviallyCopyable, PRIM, P, isTriviallyCopyable)
    case_METHOD_NOARGS(isTrivial, PRIM, P, isTrivial)
    case_METHOD_NOARGS(isLiteral, PRIM, P, isLiteral)
    case_METHOD_NOARGS(getInstantiatedFromMemberClass, CLASS, P, getInstantiatedFromMemberClass)
    case_METHOD_NOARGS(getMemberSpecializationInfo, CLASS, P, getMemberSpecializationInfo)
    case_METHOD_NOARGS(getDescribedClassTemplate, CLASS, P, getDescribedClassTemplate)
    case_METHOD_NOARGS(getTemplateSpecializationKind, PRIM, P, getTemplateSpecializationKind)
    case_METHOD_NOARGS(getTemplateInstantiationPattern, CLASS, P, getTemplateInstantiationPattern)
    case_METHOD_NOARGS(getDestructor, CLASS, P, getDestructor)
    case_METHOD_NOARGS(isAnyDestructorNoReturn, PRIM, P, isAnyDestructorNoReturn)
    case_METHOD_NOARGS(isLocalClass, CLASS, P, isLocalClass)
    case currefl::isCurrentInstantiation: {
      CHECK_TRAIT_IS_REFLPROP(isCurrentInstantiation)
      TRY_LOAD_VAL(const class clang::DeclContext *, p0)
      if (no_more) PRIM_REFLECTION(P, isCurrentInstantiation, p0)
      TOO_MANY;
    }
    case currefl::isDerivedFrom: {
      CHECK_TRAIT_IS_REFLPROP(isDerivedFrom)
      TRY_LOAD_VAL(const class clang::CXXRecordDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, isDerivedFrom, p0)
      TOO_MANY;
    }
    case currefl::isVirtuallyDerivedFrom: {
      CHECK_TRAIT_IS_REFLPROP(isVirtuallyDerivedFrom)
      TRY_LOAD_VAL(const class clang::CXXRecordDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, isVirtuallyDerivedFrom, p0)
      TOO_MANY;
    }
    case currefl::isProvablyNotDerivedFrom: {
      CHECK_TRAIT_IS_REFLPROP(isProvablyNotDerivedFrom)
      TRY_LOAD_VAL(const class clang::CXXRecordDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, isProvablyNotDerivedFrom, p0)
      TOO_MANY;
    }
    case currefl::MergeAccess: {
      CHECK_TRAIT_IS_REFLPROP(MergeAccess)
      TRY_LOAD_VAL(enum clang::AccessSpecifier, p0)
      TRY_LOAD_VAL(enum clang::AccessSpecifier, p1)
      if (no_more) PRIM_REFLECTION(S, MergeAccess, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(mayBeAbstract, PRIM, P, mayBeAbstract)
    case_METHOD_NOARGS(getLambdaManglingNumber, PRIM, P, getLambdaManglingNumber)
    case_METHOD_NOARGS(getLambdaContextDecl, CLASS, P, getLambdaContextDecl)
    case_METHOD_NOARGS(getMSInheritanceModel, PRIM, P, getMSInheritanceModel)
    case_METHOD_NOARGS(calculateInheritanceModel, PRIM, P, calculateInheritanceModel)
    case_METHOD_NOARGS(nullFieldOffsetIsZero, PRIM, P, nullFieldOffsetIsZero)
    case_METHOD_NOARGS(getMSVtorDispMode, PRIM, P, getMSVtorDispMode)
    case_METHOD_NOARGS(isDependentLambda, PRIM, P, isDependentLambda)
    case_METHOD_NOARGS(getLambdaTypeInfo, CLASS, P, getLambdaTypeInfo)
    case_METHOD_NOARGS(isInterfaceLike, PRIM, P, isInterfaceLike)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXDeductionGuideDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXDeductionGuideDecl;
# define CLASSNAME clang::CXXDeductionGuideDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isExplicit, PRIM, P, isExplicit)
    case_METHOD_NOARGS(isExplicitSpecified, PRIM, P, isExplicitSpecified)
    case_METHOD_NOARGS(getDeducedTemplate, CLASS, P, getDeducedTemplate)
    case_METHOD_NOARGS(isCopyDeductionCandidate, PRIM, P, isCopyDeductionCandidate)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXMethodDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXMethodDecl;
# define CLASSNAME clang::CXXMethodDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isStatic, PRIM, P, isStatic)
    case_METHOD_NOARGS(isInstance, PRIM, P, isInstance)
    case currefl::isStaticOverloadedOperator: {
      CHECK_TRAIT_IS_REFLPROP(isStaticOverloadedOperator)
      TRY_LOAD_VAL(enum clang::OverloadedOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isStaticOverloadedOperator, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isConst, PRIM, P, isConst)
    case_METHOD_NOARGS(isVolatile, PRIM, P, isVolatile)
    case_METHOD_NOARGS(isVirtual, PRIM, P, isVirtual)
    case currefl::getDevirtualizedMethod: {
      CHECK_TRAIT_IS_REFLPROP(getDevirtualizedMethod)
      TRY_LOAD_VAL(const class clang::Expr *, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, getDevirtualizedMethod, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isUsualDeallocationFunction, PRIM, P, isUsualDeallocationFunction)
    case_METHOD_NOARGS(isCopyAssignmentOperator, PRIM, P, isCopyAssignmentOperator)
    case_METHOD_NOARGS(isMoveAssignmentOperator, PRIM, P, isMoveAssignmentOperator)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getMostRecentDecl, CLASS, P, getMostRecentDecl)
    case_METHOD_NOARGS(isUserProvided, PRIM, P, isUserProvided)
    case_METHOD_NOARGS(size_overridden_methods, PRIM, P, size_overridden_methods)
    case_METHOD_NOARGS(getParent, CLASS, P, getParent)
    case_METHOD_NOARGS(getTypeQualifiers, PRIM, P, getTypeQualifiers)
    case_METHOD_NOARGS(getRefQualifier, PRIM, P, getRefQualifier)
    case_METHOD_NOARGS(hasInlineBody, PRIM, P, hasInlineBody)
    case_METHOD_NOARGS(isLambdaStaticInvoker, PRIM, P, isLambdaStaticInvoker)
    case currefl::getCorrespondingMethodInClass: {
      CHECK_TRAIT_IS_REFLPROP(getCorrespondingMethodInClass)
      TRY_LOAD_VAL(const class clang::CXXRecordDecl *, p0)
      if (no_more) CLASS_REFLECTION(P, getCorrespondingMethodInClass, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, getCorrespondingMethodInClass, p0, p1)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXCtorInitializer *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__CXXCtorInitializer;
# define CLASSNAME clang::CXXCtorInitializer
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isBaseInitializer, PRIM, P, isBaseInitializer)
    case_METHOD_NOARGS(isMemberInitializer, PRIM, P, isMemberInitializer)
    case_METHOD_NOARGS(isAnyMemberInitializer, PRIM, P, isAnyMemberInitializer)
    case_METHOD_NOARGS(isIndirectMemberInitializer, PRIM, P, isIndirectMemberInitializer)
    case_METHOD_NOARGS(isInClassMemberInitializer, PRIM, P, isInClassMemberInitializer)
    case_METHOD_NOARGS(isDelegatingInitializer, PRIM, P, isDelegatingInitializer)
    case_METHOD_NOARGS(isPackExpansion, PRIM, P, isPackExpansion)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, P, getEllipsisLoc)
    case_METHOD_NOARGS(getBaseClassLoc, CLASS, P, getBaseClassLoc)
    case_METHOD_NOARGS(getBaseClass, CLASS, P, getBaseClass)
    case_METHOD_NOARGS(isBaseVirtual, PRIM, P, isBaseVirtual)
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
    case_METHOD_NOARGS(getMember, CLASS, P, getMember)
    case_METHOD_NOARGS(getAnyMember, CLASS, P, getAnyMember)
    case_METHOD_NOARGS(getIndirectMember, CLASS, P, getIndirectMember)
    case_METHOD_NOARGS(getMemberLocation, CLASS, P, getMemberLocation)
    case_METHOD_NOARGS(getSourceLocation, CLASS, P, getSourceLocation)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(isWritten, PRIM, P, isWritten)
    case_METHOD_NOARGS(getSourceOrder, PRIM, P, getSourceOrder)
    case_METHOD_NOARGS(getLParenLoc, CLASS, P, getLParenLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getInit, CLASS, P, getInit)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::InheritedConstructor X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__InheritedConstructor;
# define CLASSNAME clang::InheritedConstructor
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(operator_bool, PRIM, V, operator bool)
    case_METHOD_NOARGS(getShadowDecl, CLASS, V, getShadowDecl)
    case_METHOD_NOARGS(getConstructor, CLASS, V, getConstructor)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXConstructorDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXConstructorDecl;
# define CLASSNAME clang::CXXConstructorDecl
  switch( (currefl::memnames)N ) {
    case currefl::inits: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::inits))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, inits)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumCtorInitializers, PRIM, P, getNumCtorInitializers)
    case_METHOD_NOARGS(isExplicitSpecified, PRIM, P, isExplicitSpecified)
    case_METHOD_NOARGS(isExplicit, PRIM, P, isExplicit)
    case_METHOD_NOARGS(isDelegatingConstructor, PRIM, P, isDelegatingConstructor)
    case_METHOD_NOARGS(getTargetConstructor, CLASS, P, getTargetConstructor)
    case_METHOD_NOARGS(isDefaultConstructor, PRIM, P, isDefaultConstructor)
    case_METHOD_NOARGS(isCopyConstructor, PRIM, P, isCopyConstructor)
    case_METHOD_NOARGS(isMoveConstructor, PRIM, P, isMoveConstructor)
    case_METHOD_NOARGS(isCopyOrMoveConstructor, PRIM, P, isCopyOrMoveConstructor)
    case currefl::isConvertingConstructor: {
      CHECK_TRAIT_IS_REFLPROP(isConvertingConstructor)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) PRIM_REFLECTION(P, isConvertingConstructor, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isSpecializationCopyingObject, PRIM, P, isSpecializationCopyingObject)
    case_METHOD_NOARGS(isInheritingConstructor, PRIM, P, isInheritingConstructor)
    case_METHOD_NOARGS(getInheritedConstructor, CLASS, P, getInheritedConstructor)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXDestructorDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXDestructorDecl;
# define CLASSNAME clang::CXXDestructorDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOperatorDelete, CLASS, P, getOperatorDelete)
    case_METHOD_NOARGS(getOperatorDeleteThisArg, CLASS, P, getOperatorDeleteThisArg)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXConversionDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXConversionDecl;
# define CLASSNAME clang::CXXConversionDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isExplicitSpecified, PRIM, P, isExplicitSpecified)
    case_METHOD_NOARGS(isExplicit, PRIM, P, isExplicit)
    case_METHOD_NOARGS(getConversionType, CLASS, P, getConversionType)
    case_METHOD_NOARGS(isLambdaToBlockPointerConversion, PRIM, P, isLambdaToBlockPointerConversion)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::LinkageSpecDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__LinkageSpecDecl;
# define CLASSNAME clang::LinkageSpecDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLanguage, PRIM, P, getLanguage)
    case_METHOD_NOARGS(hasBraces, PRIM, P, hasBraces)
    case_METHOD_NOARGS(getExternLoc, CLASS, P, getExternLoc)
    case_METHOD_NOARGS(getRBraceLoc, CLASS, P, getRBraceLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UsingDirectiveDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UsingDirectiveDecl;
# define CLASSNAME clang::UsingDirectiveDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getNominatedNamespaceAsWritten, CLASS, P, getNominatedNamespaceAsWritten)
    case_METHOD_NOARGS(getNominatedNamespace, CLASS, P, getNominatedNamespace)
    case_METHOD_NOARGS(getCommonAncestor, CLASS, P, getCommonAncestor)
    case_METHOD_NOARGS(getUsingLoc, CLASS, P, getUsingLoc)
    case_METHOD_NOARGS(getNamespaceKeyLocation, CLASS, P, getNamespaceKeyLocation)
    case_METHOD_NOARGS(getIdentLocation, CLASS, P, getIdentLocation)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::NamespaceAliasDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__NamespaceAliasDecl;
# define CLASSNAME clang::NamespaceAliasDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getNamespace, CLASS, P, getNamespace)
    case_METHOD_NOARGS(getAliasLoc, CLASS, P, getAliasLoc)
    case_METHOD_NOARGS(getNamespaceLoc, CLASS, P, getNamespaceLoc)
    case_METHOD_NOARGS(getTargetNameLoc, CLASS, P, getTargetNameLoc)
    case_METHOD_NOARGS(getAliasedNamespace, CLASS, P, getAliasedNamespace)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UsingShadowDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UsingShadowDecl;
# define CLASSNAME clang::UsingShadowDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getTargetDecl, CLASS, P, getTargetDecl)
    case_METHOD_NOARGS(getUsingDecl, CLASS, P, getUsingDecl)
    case_METHOD_NOARGS(getNextUsingShadowDecl, CLASS, P, getNextUsingShadowDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ConstructorUsingShadowDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ConstructorUsingShadowDecl;
# define CLASSNAME clang::ConstructorUsingShadowDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getParent, CLASS, P, getParent)
    case_METHOD_NOARGS(getNominatedBaseClassShadowDecl, CLASS, P, getNominatedBaseClassShadowDecl)
    case_METHOD_NOARGS(getConstructedBaseClassShadowDecl, CLASS, P, getConstructedBaseClassShadowDecl)
    case_METHOD_NOARGS(getNominatedBaseClass, CLASS, P, getNominatedBaseClass)
    case_METHOD_NOARGS(getConstructedBaseClass, CLASS, P, getConstructedBaseClass)
    case_METHOD_NOARGS(constructsVirtualBase, PRIM, P, constructsVirtualBase)
    case_METHOD_NOARGS(getConstructor, CLASS, P, getConstructor)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UsingDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UsingDecl;
# define CLASSNAME clang::UsingDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getUsingLoc, CLASS, P, getUsingLoc)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getNameInfo, CLASS, P, getNameInfo)
    case_METHOD_NOARGS(isAccessDeclaration, PRIM, P, isAccessDeclaration)
    case_METHOD_NOARGS(hasTypename, PRIM, P, hasTypename)
    case currefl::shadows: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::shadows))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, shadows)
      TOO_MANY;
    }
    case_METHOD_NOARGS(shadow_size, PRIM, P, shadow_size)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UsingPackDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UsingPackDecl;
# define CLASSNAME clang::UsingPackDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getInstantiatedFromUsingDecl, CLASS, P, getInstantiatedFromUsingDecl)
    case currefl::expansions: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::expansions))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, expansions)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UnresolvedUsingValueDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UnresolvedUsingValueDecl;
# define CLASSNAME clang::UnresolvedUsingValueDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getUsingLoc, CLASS, P, getUsingLoc)
    case_METHOD_NOARGS(isAccessDeclaration, PRIM, P, isAccessDeclaration)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getNameInfo, CLASS, P, getNameInfo)
    case_METHOD_NOARGS(isPackExpansion, PRIM, P, isPackExpansion)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, P, getEllipsisLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UnresolvedUsingTypenameDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UnresolvedUsingTypenameDecl;
# define CLASSNAME clang::UnresolvedUsingTypenameDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getUsingLoc, CLASS, P, getUsingLoc)
    case_METHOD_NOARGS(getTypenameLoc, CLASS, P, getTypenameLoc)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getNameInfo, CLASS, P, getNameInfo)
    case_METHOD_NOARGS(isPackExpansion, PRIM, P, isPackExpansion)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, P, getEllipsisLoc)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::StaticAssertDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__StaticAssertDecl;
# define CLASSNAME clang::StaticAssertDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAssertExpr, CLASS, P, getAssertExpr)
    case_METHOD_NOARGS(getMessage, CLASS, P, getMessage)
    case_METHOD_NOARGS(isFailed, PRIM, P, isFailed)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::BindingDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__BindingDecl;
# define CLASSNAME clang::BindingDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBinding, CLASS, P, getBinding)
    case_METHOD_NOARGS(getHoldingVar, CLASS, P, getHoldingVar)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DecompositionDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DecompositionDecl;
# define CLASSNAME clang::DecompositionDecl
  switch( (currefl::memnames)N ) {
    case currefl::bindings: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::bindings))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, bindings)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::MSPropertyDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__MSPropertyDecl;
# define CLASSNAME clang::MSPropertyDecl
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasGetter, PRIM, P, hasGetter)
    case_METHOD_NOARGS(getGetterId, CLASS, P, getGetterId)
    case_METHOD_NOARGS(hasSetter, PRIM, P, hasSetter)
    case_METHOD_NOARGS(getSetterId, CLASS, P, getSetterId)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ConstexprDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ConstexprDecl;
# define CLASSNAME clang::ConstexprDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLambdaExpr, CLASS, P, getLambdaExpr)
    case_METHOD_NOARGS(isDependent, PRIM, P, isDependent)
    case_METHOD_NOARGS(hasFunctionRepresentation, PRIM, P, hasFunctionRepresentation)
    case_METHOD_NOARGS(hasLambdaRepresentation, PRIM, P, hasLambdaRepresentation)
    case_METHOD_NOARGS(getFunctionDecl, CLASS, P, getFunctionDecl)
    case_METHOD_NOARGS(getClosureDecl, CLASS, P, getClosureDecl)
    case_METHOD_NOARGS(getClosureCallOperator, CLASS, P, getClosureCallOperator)
    case_METHOD_NOARGS(hasBody, PRIM, P, hasBody)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(getCallExpr, CLASS, P, getCallExpr)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateParameterList *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TemplateParameterList;
# define CLASSNAME clang::TemplateParameterList
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(P)
      TOO_MANY;
};
    case_METHOD_NOARGS(size, PRIM, P, size)
    case currefl::asArray: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::asArray))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, asArray)
      TOO_MANY;
    }
    case currefl::getParam: {
      CHECK_TRAIT_IS_REFLPROP(getParam)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getParam, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getMinRequiredArguments, PRIM, P, getMinRequiredArguments)
    case_METHOD_NOARGS(getDepth, PRIM, P, getDepth)
    case_METHOD_NOARGS(containsUnexpandedParameterPack, PRIM, P, containsUnexpandedParameterPack)
    case_METHOD_NOARGS(getRequiresClause, CLASS, P, getRequiresClause)
    case_METHOD_NOARGS(getTemplateLoc, CLASS, P, getTemplateLoc)
    case_METHOD_NOARGS(getLAngleLoc, CLASS, P, getLAngleLoc)
    case_METHOD_NOARGS(getRAngleLoc, CLASS, P, getRAngleLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateArgumentList *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__TemplateArgumentList;
# define CLASSNAME clang::TemplateArgumentList
  switch( (currefl::memnames)N ) {
    case currefl::get: {
      CHECK_TRAIT_IS_REFLPROP(get)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, get, p0)
      TOO_MANY;
    }
    case currefl::operator_sub: {
      CHECK_TRAIT_IS_REFLPROP(operator_sub)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, operator[], p0)
      TOO_MANY;
    }
    case currefl::asArray: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::asArray))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, asArray)
      TOO_MANY;
    }
    case_METHOD_NOARGS(size, PRIM, P, size)
    case_METHOD_NOARGS(data, CLASS, P, data)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TemplateDecl;
# define CLASSNAME clang::TemplateDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTemplateParameters, CLASS, P, getTemplateParameters)
    case_METHOD_NOARGS(getRequiresClause, CLASS, P, getRequiresClause)
    case_METHOD_NOARGS(getAssociatedConstraints, CLASS, P, getAssociatedConstraints)
    case_METHOD_NOARGS(getTemplatedDecl, CLASS, P, getTemplatedDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionTemplateSpecializationInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__FunctionTemplateSpecializationInfo;
# define CLASSNAME clang::FunctionTemplateSpecializationInfo
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, P, Function)
    case_FIELD(CLASS, P, TemplateArguments)
    case_FIELD(CLASS, P, TemplateArgumentsAsWritten)
    case_FIELD(CLASS, P, PointOfInstantiation)
    case_METHOD_NOARGS(getTemplate, CLASS, P, getTemplate)
    case_METHOD_NOARGS(getTemplateSpecializationKind, PRIM, P, getTemplateSpecializationKind)
    case_METHOD_NOARGS(isExplicitSpecialization, PRIM, P, isExplicitSpecialization)
    case_METHOD_NOARGS(isExplicitInstantiationOrSpecialization, PRIM, P, isExplicitInstantiationOrSpecialization)
    case_METHOD_NOARGS(getPointOfInstantiation, CLASS, P, getPointOfInstantiation)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::MemberSpecializationInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__MemberSpecializationInfo;
# define CLASSNAME clang::MemberSpecializationInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getInstantiatedFrom, CLASS, P, getInstantiatedFrom)
    case_METHOD_NOARGS(getTemplateSpecializationKind, PRIM, P, getTemplateSpecializationKind)
    case_METHOD_NOARGS(isExplicitSpecialization, PRIM, P, isExplicitSpecialization)
    case_METHOD_NOARGS(getPointOfInstantiation, CLASS, P, getPointOfInstantiation)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DependentFunctionTemplateSpecializationInfo *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DependentFunctionTemplateSpecializationInfo;
# define CLASSNAME clang::DependentFunctionTemplateSpecializationInfo
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNumTemplates, PRIM, P, getNumTemplates)
    case currefl::getTemplate: {
      CHECK_TRAIT_IS_REFLPROP(getTemplate)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getTemplate, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getTemplateArgs, CLASS, P, getTemplateArgs)
    case_METHOD_NOARGS(getNumTemplateArgs, PRIM, P, getNumTemplateArgs)
    case currefl::getTemplateArg: {
      CHECK_TRAIT_IS_REFLPROP(getTemplateArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getTemplateArg, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLAngleLoc, CLASS, P, getLAngleLoc)
    case_METHOD_NOARGS(getRAngleLoc, CLASS, P, getRAngleLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::RedeclarableTemplateDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__RedeclarableTemplateDecl;
# define CLASSNAME clang::RedeclarableTemplateDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(isMemberSpecialization, PRIM, P, isMemberSpecialization)
    case_METHOD_NOARGS(getInstantiatedFromMemberTemplate, CLASS, P, getInstantiatedFromMemberTemplate)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionTemplateDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FunctionTemplateDecl;
# define CLASSNAME clang::FunctionTemplateDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(LoadLazySpecializations, VOID, P, LoadLazySpecializations)
    case_METHOD_NOARGS(getTemplatedDecl, CLASS, P, getTemplatedDecl)
    case_METHOD_NOARGS(isThisDeclarationADefinition, PRIM, P, isThisDeclarationADefinition)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getPreviousDecl, CLASS, P, getPreviousDecl)
    case_METHOD_NOARGS(getMostRecentDecl, CLASS, P, getMostRecentDecl)
    case_METHOD_NOARGS(getInstantiatedFromMemberTemplate, CLASS, P, getInstantiatedFromMemberTemplate)
    case currefl::specializations: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::specializations))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, specializations)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateTypeParmDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TemplateTypeParmDecl;
# define CLASSNAME clang::TemplateTypeParmDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(wasDeclaredWithTypename, PRIM, P, wasDeclaredWithTypename)
    case_METHOD_NOARGS(hasDefaultArgument, PRIM, P, hasDefaultArgument)
    case_METHOD_NOARGS(getDefaultArgument, CLASS, P, getDefaultArgument)
    case_METHOD_NOARGS(getDefaultArgumentInfo, CLASS, P, getDefaultArgumentInfo)
    case_METHOD_NOARGS(getDefaultArgumentLoc, CLASS, P, getDefaultArgumentLoc)
    case_METHOD_NOARGS(defaultArgumentWasInherited, PRIM, P, defaultArgumentWasInherited)
    case_METHOD_NOARGS(getDepth, PRIM, P, getDepth)
    case_METHOD_NOARGS(getIndex, PRIM, P, getIndex)
    case_METHOD_NOARGS(isParameterPack, PRIM, P, isParameterPack)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::NonTypeTemplateParmDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__NonTypeTemplateParmDecl;
# define CLASSNAME clang::NonTypeTemplateParmDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(hasDefaultArgument, PRIM, P, hasDefaultArgument)
    case_METHOD_NOARGS(getDefaultArgument, CLASS, P, getDefaultArgument)
    case_METHOD_NOARGS(getDefaultArgumentLoc, CLASS, P, getDefaultArgumentLoc)
    case_METHOD_NOARGS(defaultArgumentWasInherited, PRIM, P, defaultArgumentWasInherited)
    case_METHOD_NOARGS(isParameterPack, PRIM, P, isParameterPack)
    case_METHOD_NOARGS(isPackExpansion, PRIM, P, isPackExpansion)
    case_METHOD_NOARGS(isExpandedParameterPack, PRIM, P, isExpandedParameterPack)
    case_METHOD_NOARGS(getNumExpansionTypes, PRIM, P, getNumExpansionTypes)
    case currefl::getExpansionType: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionType)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getExpansionType, p0)
      TOO_MANY;
    }
    case currefl::getExpansionTypeSourceInfo: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionTypeSourceInfo)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getExpansionTypeSourceInfo, p0)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TemplateTemplateParmDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TemplateTemplateParmDecl;
# define CLASSNAME clang::TemplateTemplateParmDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isParameterPack, PRIM, P, isParameterPack)
    case_METHOD_NOARGS(isPackExpansion, PRIM, P, isPackExpansion)
    case_METHOD_NOARGS(isExpandedParameterPack, PRIM, P, isExpandedParameterPack)
    case_METHOD_NOARGS(getNumExpansionTemplateParameters, PRIM, P, getNumExpansionTemplateParameters)
    case currefl::getExpansionTemplateParameters: {
      CHECK_TRAIT_IS_REFLPROP(getExpansionTemplateParameters)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getExpansionTemplateParameters, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasDefaultArgument, PRIM, P, hasDefaultArgument)
    case_METHOD_NOARGS(getDefaultArgument, CLASS, P, getDefaultArgument)
    case_METHOD_NOARGS(getDefaultArgumentLoc, CLASS, P, getDefaultArgumentLoc)
    case_METHOD_NOARGS(defaultArgumentWasInherited, PRIM, P, defaultArgumentWasInherited)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::BuiltinTemplateDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__BuiltinTemplateDecl;
# define CLASSNAME clang::BuiltinTemplateDecl
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ClassTemplateSpecializationDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ClassTemplateSpecializationDecl;
# define CLASSNAME clang::ClassTemplateSpecializationDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSpecializedTemplate, CLASS, P, getSpecializedTemplate)
    case_METHOD_NOARGS(getTemplateArgs, CLASS, P, getTemplateArgs)
    case_METHOD_NOARGS(getSpecializationKind, PRIM, P, getSpecializationKind)
    case_METHOD_NOARGS(isExplicitSpecialization, PRIM, P, isExplicitSpecialization)
    case_METHOD_NOARGS(isExplicitInstantiationOrSpecialization, PRIM, P, isExplicitInstantiationOrSpecialization)
    case_METHOD_NOARGS(getPointOfInstantiation, CLASS, P, getPointOfInstantiation)
    case_METHOD_NOARGS(getTemplateInstantiationArgs, CLASS, P, getTemplateInstantiationArgs)
    case_METHOD_NOARGS(getTypeAsWritten, CLASS, P, getTypeAsWritten)
    case_METHOD_NOARGS(getExternLoc, CLASS, P, getExternLoc)
    case_METHOD_NOARGS(getTemplateKeywordLoc, CLASS, P, getTemplateKeywordLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ClassTemplatePartialSpecializationDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ClassTemplatePartialSpecializationDecl;
# define CLASSNAME clang::ClassTemplatePartialSpecializationDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTemplateParameters, CLASS, P, getTemplateParameters)
    case_METHOD_NOARGS(getTemplateArgsAsWritten, CLASS, P, getTemplateArgsAsWritten)
    case_METHOD_NOARGS(getInstantiatedFromMember, CLASS, P, getInstantiatedFromMember)
    case_METHOD_NOARGS(getInstantiatedFromMemberTemplate, CLASS, P, getInstantiatedFromMemberTemplate)
    case_METHOD_NOARGS(getInjectedSpecializationType, CLASS, P, getInjectedSpecializationType)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ClassTemplateDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ClassTemplateDecl;
# define CLASSNAME clang::ClassTemplateDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(LoadLazySpecializations, VOID, P, LoadLazySpecializations)
    case_METHOD_NOARGS(getTemplatedDecl, CLASS, P, getTemplatedDecl)
    case_METHOD_NOARGS(isThisDeclarationADefinition, PRIM, P, isThisDeclarationADefinition)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getPreviousDecl, CLASS, P, getPreviousDecl)
    case_METHOD_NOARGS(getMostRecentDecl, CLASS, P, getMostRecentDecl)
    case_METHOD_NOARGS(getInstantiatedFromMemberTemplate, CLASS, P, getInstantiatedFromMemberTemplate)
    case currefl::specializations: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::specializations))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, specializations)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FriendTemplateDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FriendTemplateDecl;
# define CLASSNAME clang::FriendTemplateDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getFriendType, CLASS, P, getFriendType)
    case_METHOD_NOARGS(getFriendDecl, CLASS, P, getFriendDecl)
    case_METHOD_NOARGS(getFriendLoc, CLASS, P, getFriendLoc)
    case currefl::getTemplateParameterList: {
      CHECK_TRAIT_IS_REFLPROP(getTemplateParameterList)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getTemplateParameterList, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumTemplateParameters, PRIM, P, getNumTemplateParameters)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypeAliasTemplateDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TypeAliasTemplateDecl;
# define CLASSNAME clang::TypeAliasTemplateDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTemplatedDecl, CLASS, P, getTemplatedDecl)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getPreviousDecl, CLASS, P, getPreviousDecl)
    case_METHOD_NOARGS(getInstantiatedFromMemberTemplate, CLASS, P, getInstantiatedFromMemberTemplate)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ClassScopeFunctionSpecializationDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ClassScopeFunctionSpecializationDecl;
# define CLASSNAME clang::ClassScopeFunctionSpecializationDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSpecialization, CLASS, P, getSpecialization)
    case_METHOD_NOARGS(hasExplicitTemplateArgs, PRIM, P, hasExplicitTemplateArgs)
    case_METHOD_NOARGS(templateArgs, CLASS, P, templateArgs)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::VarTemplateSpecializationDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__VarTemplateSpecializationDecl;
# define CLASSNAME clang::VarTemplateSpecializationDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSpecializedTemplate, CLASS, P, getSpecializedTemplate)
    case_METHOD_NOARGS(getTemplateArgs, CLASS, P, getTemplateArgs)
    case_METHOD_NOARGS(getTemplateArgsInfo, CLASS, P, getTemplateArgsInfo)
    case_METHOD_NOARGS(getSpecializationKind, PRIM, P, getSpecializationKind)
    case_METHOD_NOARGS(isExplicitSpecialization, PRIM, P, isExplicitSpecialization)
    case_METHOD_NOARGS(isExplicitInstantiationOrSpecialization, PRIM, P, isExplicitInstantiationOrSpecialization)
    case_METHOD_NOARGS(getPointOfInstantiation, CLASS, P, getPointOfInstantiation)
    case_METHOD_NOARGS(getTemplateInstantiationArgs, CLASS, P, getTemplateInstantiationArgs)
    case_METHOD_NOARGS(getTypeAsWritten, CLASS, P, getTypeAsWritten)
    case_METHOD_NOARGS(getExternLoc, CLASS, P, getExternLoc)
    case_METHOD_NOARGS(getTemplateKeywordLoc, CLASS, P, getTemplateKeywordLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::VarTemplatePartialSpecializationDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__VarTemplatePartialSpecializationDecl;
# define CLASSNAME clang::VarTemplatePartialSpecializationDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTemplateParameters, CLASS, P, getTemplateParameters)
    case_METHOD_NOARGS(getTemplateArgsAsWritten, CLASS, P, getTemplateArgsAsWritten)
    case_METHOD_NOARGS(getInstantiatedFromMember, CLASS, P, getInstantiatedFromMember)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::VarTemplateDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__VarTemplateDecl;
# define CLASSNAME clang::VarTemplateDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(LoadLazySpecializations, VOID, P, LoadLazySpecializations)
    case_METHOD_NOARGS(getTemplatedDecl, CLASS, P, getTemplatedDecl)
    case_METHOD_NOARGS(isThisDeclarationADefinition, PRIM, P, isThisDeclarationADefinition)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getPreviousDecl, CLASS, P, getPreviousDecl)
    case_METHOD_NOARGS(getMostRecentDecl, CLASS, P, getMostRecentDecl)
    case_METHOD_NOARGS(getInstantiatedFromMemberTemplate, CLASS, P, getInstantiatedFromMemberTemplate)
    case currefl::specializations: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::specializations))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, specializations)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FriendDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FriendDecl;
# define CLASSNAME clang::FriendDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getFriendType, CLASS, P, getFriendType)
    case_METHOD_NOARGS(getFriendTypeNumTemplateParameterLists, PRIM, P, getFriendTypeNumTemplateParameterLists)
    case currefl::getFriendTypeTemplateParameterList: {
      CHECK_TRAIT_IS_REFLPROP(getFriendTypeTemplateParameterList)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getFriendTypeTemplateParameterList, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getFriendDecl, CLASS, P, getFriendDecl)
    case_METHOD_NOARGS(getFriendLoc, CLASS, P, getFriendLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(isUnsupportedFriend, PRIM, P, isUnsupportedFriend)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCListBase *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ObjCListBase;
# define CLASSNAME clang::ObjCListBase
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(size, PRIM, P, size)
    case_METHOD_NOARGS(empty, PRIM, P, empty)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCProtocolList *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ObjCProtocolList;
# define CLASSNAME clang::ObjCProtocolList
  switch( (currefl::memnames)N ) {
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCMethodDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCMethodDecl;
# define CLASSNAME clang::ObjCMethodDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getObjCDeclQualifier, PRIM, P, getObjCDeclQualifier)
    case_METHOD_NOARGS(hasRelatedResultType, PRIM, P, hasRelatedResultType)
    case_METHOD_NOARGS(isRedeclaration, PRIM, P, isRedeclaration)
    case_METHOD_NOARGS(getDeclaratorEndLoc, CLASS, P, getDeclaratorEndLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getSelectorStartLoc, CLASS, P, getSelectorStartLoc)
    case currefl::getSelectorLoc: {
      CHECK_TRAIT_IS_REFLPROP(getSelectorLoc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getSelectorLoc, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumSelectorLocs, PRIM, P, getNumSelectorLocs)
    case_METHOD_NOARGS(getClassInterface, CLASS, P, getClassInterface)
    case_METHOD_NOARGS(getSelector, CLASS, P, getSelector)
    case_METHOD_NOARGS(getReturnType, CLASS, P, getReturnType)
    case_METHOD_NOARGS(getReturnTypeSourceRange, CLASS, P, getReturnTypeSourceRange)
    case_METHOD_NOARGS(getSendResultType, CLASS, P, getSendResultType)
    case currefl::getSendResultType1: {
      CHECK_TRAIT_IS_REFLPROP(getSendResultType1)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getSendResultType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getReturnTypeSourceInfo, CLASS, P, getReturnTypeSourceInfo)
    case_METHOD_NOARGS(param_size, PRIM, P, param_size)
    case currefl::parameters: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::parameters))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, parameters)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getSelfDecl, CLASS, P, getSelfDecl)
    case_METHOD_NOARGS(getCmdDecl, CLASS, P, getCmdDecl)
    case_METHOD_NOARGS(getMethodFamily, PRIM, P, getMethodFamily)
    case_METHOD_NOARGS(isInstanceMethod, PRIM, P, isInstanceMethod)
    case_METHOD_NOARGS(isVariadic, PRIM, P, isVariadic)
    case_METHOD_NOARGS(isClassMethod, PRIM, P, isClassMethod)
    case_METHOD_NOARGS(isPropertyAccessor, PRIM, P, isPropertyAccessor)
    case_METHOD_NOARGS(isDefined, PRIM, P, isDefined)
    case_METHOD_NOARGS(isOverriding, PRIM, P, isOverriding)
    case_METHOD_NOARGS(hasSkippedBody, PRIM, P, hasSkippedBody)
    case currefl::findPropertyDecl: {
      CHECK_TRAIT_IS_REFLPROP(findPropertyDecl)
      if (no_more) CLASS_REFLECTION_0(P, findPropertyDecl)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) CLASS_REFLECTION(P, findPropertyDecl, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getImplementationControl, PRIM, P, getImplementationControl)
    case_METHOD_NOARGS(isOptional, PRIM, P, isOptional)
    case_METHOD_NOARGS(isThisDeclarationADesignatedInitializer, PRIM, P, isThisDeclarationADesignatedInitializer)
    case currefl::isDesignatedInitializerForTheInterface: {
      CHECK_TRAIT_IS_REFLPROP(isDesignatedInitializerForTheInterface)
      if (no_more) PRIM_REFLECTION_0(P, isDesignatedInitializerForTheInterface)
      TRY_LOAD_VAL(const class clang::ObjCMethodDecl **, p0)
      if (no_more) PRIM_REFLECTION(P, isDesignatedInitializerForTheInterface, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasBody, PRIM, P, hasBody)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(isThisDeclarationADefinition, PRIM, P, isThisDeclarationADefinition)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCTypeParamDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCTypeParamDecl;
# define CLASSNAME clang::ObjCTypeParamDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getVariance, PRIM, P, getVariance)
    case_METHOD_NOARGS(getVarianceLoc, CLASS, P, getVarianceLoc)
    case_METHOD_NOARGS(getIndex, PRIM, P, getIndex)
    case_METHOD_NOARGS(hasExplicitBound, PRIM, P, hasExplicitBound)
    case_METHOD_NOARGS(getColonLoc, CLASS, P, getColonLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCTypeParamList *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ObjCTypeParamList;
# define CLASSNAME clang::ObjCTypeParamList
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(P)
      TOO_MANY;
};
    case_METHOD_NOARGS(size, PRIM, P, size)
    case_METHOD_NOARGS(front, CLASS, P, front)
    case_METHOD_NOARGS(back, CLASS, P, back)
    case_METHOD_NOARGS(getLAngleLoc, CLASS, P, getLAngleLoc)
    case_METHOD_NOARGS(getRAngleLoc, CLASS, P, getRAngleLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCPropertyDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCPropertyDecl;
# define CLASSNAME clang::ObjCPropertyDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAtLoc, CLASS, P, getAtLoc)
    case_METHOD_NOARGS(getLParenLoc, CLASS, P, getLParenLoc)
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
    case_METHOD_NOARGS(getType, CLASS, P, getType)
    case currefl::getUsageType: {
      CHECK_TRAIT_IS_REFLPROP(getUsageType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getUsageType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getPropertyAttributes, PRIM, P, getPropertyAttributes)
    case_METHOD_NOARGS(getPropertyAttributesAsWritten, PRIM, P, getPropertyAttributesAsWritten)
    case_METHOD_NOARGS(isReadOnly, PRIM, P, isReadOnly)
    case_METHOD_NOARGS(isAtomic, PRIM, P, isAtomic)
    case_METHOD_NOARGS(isRetaining, PRIM, P, isRetaining)
    case_METHOD_NOARGS(isInstanceProperty, PRIM, P, isInstanceProperty)
    case_METHOD_NOARGS(isClassProperty, PRIM, P, isClassProperty)
    case_METHOD_NOARGS(getQueryKind, PRIM, P, getQueryKind)
    case currefl::getQueryKind1: {
      CHECK_TRAIT_IS_REFLPROP(getQueryKind1)
      TRY_LOAD_VAL(bool, p0)
      if (no_more) PRIM_REFLECTION(S, getQueryKind, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getSetterKind, PRIM, P, getSetterKind)
    case_METHOD_NOARGS(getGetterName, CLASS, P, getGetterName)
    case_METHOD_NOARGS(getGetterNameLoc, CLASS, P, getGetterNameLoc)
    case_METHOD_NOARGS(getSetterName, CLASS, P, getSetterName)
    case_METHOD_NOARGS(getSetterNameLoc, CLASS, P, getSetterNameLoc)
    case_METHOD_NOARGS(getGetterMethodDecl, CLASS, P, getGetterMethodDecl)
    case_METHOD_NOARGS(getSetterMethodDecl, CLASS, P, getSetterMethodDecl)
    case_METHOD_NOARGS(getPropertyImplementation, PRIM, P, getPropertyImplementation)
    case_METHOD_NOARGS(isOptional, PRIM, P, isOptional)
    case_METHOD_NOARGS(getPropertyIvarDecl, CLASS, P, getPropertyIvarDecl)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::findPropertyDecl: {
      CHECK_TRAIT_IS_REFLPROP(findPropertyDecl)
      TRY_LOAD_VAL(const class clang::DeclContext *, p0)
      TRY_LOAD_VAL(const class clang::IdentifierInfo *, p1)
      TRY_LOAD_VAL(enum clang::ObjCPropertyQueryKind, p2)
      if (no_more) CLASS_REFLECTION(S, findPropertyDecl, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCContainerDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCContainerDecl;
# define CLASSNAME clang::ObjCContainerDecl
  switch( (currefl::memnames)N ) {
    case currefl::properties: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::properties))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, properties)
      TOO_MANY;
    }
    case currefl::instance_properties: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::instance_properties))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, instance_properties)
      TOO_MANY;
    }
    case currefl::class_properties: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::class_properties))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, class_properties)
      TOO_MANY;
    }
    case currefl::methods: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::methods))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, methods)
      TOO_MANY;
    }
    case currefl::instance_methods: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::instance_methods))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, instance_methods)
      TOO_MANY;
    }
    case currefl::class_methods: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::class_methods))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, class_methods)
      TOO_MANY;
    }
    case currefl::getMethod: {
      CHECK_TRAIT_IS_REFLPROP(getMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, getMethod, p0, p1)
      TRY_LOAD_VAL(bool, p2)
      if (no_more) CLASS_REFLECTION(P, getMethod, p0, p1, p2)
      TOO_MANY;
    }
    case currefl::getInstanceMethod: {
      CHECK_TRAIT_IS_REFLPROP(getInstanceMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) CLASS_REFLECTION(P, getInstanceMethod, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, getInstanceMethod, p0, p1)
      TOO_MANY;
    }
    case currefl::getClassMethod: {
      CHECK_TRAIT_IS_REFLPROP(getClassMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) CLASS_REFLECTION(P, getClassMethod, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, getClassMethod, p0, p1)
      TOO_MANY;
    }
    case currefl::HasUserDeclaredSetterMethod: {
      CHECK_TRAIT_IS_REFLPROP(HasUserDeclaredSetterMethod)
      TRY_LOAD_VAL(const class clang::ObjCPropertyDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, HasUserDeclaredSetterMethod, p0)
      TOO_MANY;
    }
    case currefl::getIvarDecl: {
      CHECK_TRAIT_IS_REFLPROP(getIvarDecl)
      TRY_LOAD_VAL(class clang::IdentifierInfo *, p0)
      if (no_more) CLASS_REFLECTION(P, getIvarDecl, p0)
      TOO_MANY;
    }
    case currefl::FindPropertyDeclaration: {
      CHECK_TRAIT_IS_REFLPROP(FindPropertyDeclaration)
      TRY_LOAD_VAL(const class clang::IdentifierInfo *, p0)
      TRY_LOAD_VAL(enum clang::ObjCPropertyQueryKind, p1)
      if (no_more) CLASS_REFLECTION(P, FindPropertyDeclaration, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getAtStartLoc, CLASS, P, getAtStartLoc)
    case_METHOD_NOARGS(getAtEndRange, CLASS, P, getAtEndRange)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCInterfaceDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCInterfaceDecl;
# define CLASSNAME clang::ObjCInterfaceDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTypeParamList, CLASS, P, getTypeParamList)
    case_METHOD_NOARGS(getTypeParamListAsWritten, CLASS, P, getTypeParamListAsWritten)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(hasDesignatedInitializers, PRIM, P, hasDesignatedInitializers)
    case_METHOD_NOARGS(declaresOrInheritsDesignatedInitializers, PRIM, P, declaresOrInheritsDesignatedInitializers)
    case_METHOD_NOARGS(getReferencedProtocols, CLASS, P, getReferencedProtocols)
    case_METHOD_NOARGS(getImplementation, CLASS, P, getImplementation)
    case currefl::FindCategoryDeclaration: {
      CHECK_TRAIT_IS_REFLPROP(FindCategoryDeclaration)
      TRY_LOAD_VAL(class clang::IdentifierInfo *, p0)
      if (no_more) CLASS_REFLECTION(P, FindCategoryDeclaration, p0)
      TOO_MANY;
    }
    case currefl::getCategoryInstanceMethod: {
      CHECK_TRAIT_IS_REFLPROP(getCategoryInstanceMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) CLASS_REFLECTION(P, getCategoryInstanceMethod, p0)
      TOO_MANY;
    }
    case currefl::getCategoryClassMethod: {
      CHECK_TRAIT_IS_REFLPROP(getCategoryClassMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) CLASS_REFLECTION(P, getCategoryClassMethod, p0)
      TOO_MANY;
    }
    case currefl::getCategoryMethod: {
      CHECK_TRAIT_IS_REFLPROP(getCategoryMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, getCategoryMethod, p0, p1)
      TOO_MANY;
    }
    case currefl::protocols: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::protocols))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, protocols)
      TOO_MANY;
    }
    case currefl::protocol_locs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::protocol_locs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, protocol_locs)
      TOO_MANY;
    }
    case currefl::all_referenced_protocols: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::all_referenced_protocols))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, all_referenced_protocols)
      TOO_MANY;
    }
    case currefl::ivars: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::ivars))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, ivars)
      TOO_MANY;
    }
    case_METHOD_NOARGS(ivar_size, PRIM, P, ivar_size)
    case_METHOD_NOARGS(ivar_empty, PRIM, P, ivar_empty)
    case_METHOD_NOARGS(getObjCRuntimeNameAsString, CLASS, P, getObjCRuntimeNameAsString)
    case currefl::isDesignatedInitializer: {
      CHECK_TRAIT_IS_REFLPROP(isDesignatedInitializer)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) PRIM_REFLECTION(P, isDesignatedInitializer, p0)
      TRY_LOAD_VAL(const class clang::ObjCMethodDecl **, p1)
      if (no_more) PRIM_REFLECTION(P, isDesignatedInitializer, p0, p1)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isThisDeclarationADefinition, PRIM, P, isThisDeclarationADefinition)
    case_METHOD_NOARGS(hasDefinition, PRIM, P, hasDefinition)
    case_METHOD_NOARGS(getDefinition, CLASS, P, getDefinition)
    case_METHOD_NOARGS(getSuperClassType, CLASS, P, getSuperClassType)
    case_METHOD_NOARGS(getSuperClassTInfo, CLASS, P, getSuperClassTInfo)
    case_METHOD_NOARGS(getSuperClass, CLASS, P, getSuperClass)
    case currefl::visible_categories: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::visible_categories))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, visible_categories)
      TOO_MANY;
    }
    case_METHOD_NOARGS(visible_categories_empty, PRIM, P, visible_categories_empty)
    case currefl::known_categories: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::known_categories))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, known_categories)
      TOO_MANY;
    }
    case_METHOD_NOARGS(known_categories_empty, PRIM, P, known_categories_empty)
    case currefl::visible_extensions: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::visible_extensions))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, visible_extensions)
      TOO_MANY;
    }
    case_METHOD_NOARGS(visible_extensions_empty, PRIM, P, visible_extensions_empty)
    case currefl::known_extensions: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::known_extensions))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, known_extensions)
      TOO_MANY;
    }
    case_METHOD_NOARGS(known_extensions_empty, PRIM, P, known_extensions_empty)
    case_METHOD_NOARGS(getCategoryListRaw, CLASS, P, getCategoryListRaw)
    case currefl::FindPropertyVisibleInPrimaryClass: {
      CHECK_TRAIT_IS_REFLPROP(FindPropertyVisibleInPrimaryClass)
      TRY_LOAD_VAL(class clang::IdentifierInfo *, p0)
      TRY_LOAD_VAL(enum clang::ObjCPropertyQueryKind, p1)
      if (no_more) CLASS_REFLECTION(P, FindPropertyVisibleInPrimaryClass, p0, p1)
      TOO_MANY;
    }
    case currefl::isSuperClassOf: {
      CHECK_TRAIT_IS_REFLPROP(isSuperClassOf)
      TRY_LOAD_VAL(const class clang::ObjCInterfaceDecl *, p0)
      if (no_more) PRIM_REFLECTION(P, isSuperClassOf, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isArcWeakrefUnavailable, PRIM, P, isArcWeakrefUnavailable)
    case_METHOD_NOARGS(isObjCRequiresPropertyDefs, CLASS, P, isObjCRequiresPropertyDefs)
    case currefl::lookupMethod: {
      CHECK_TRAIT_IS_REFLPROP(lookupMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, lookupMethod, p0, p1)
      TRY_LOAD_VAL(bool, p2)
      if (no_more) CLASS_REFLECTION(P, lookupMethod, p0, p1, p2)
      TRY_LOAD_VAL(bool, p3)
      if (no_more) CLASS_REFLECTION(P, lookupMethod, p0, p1, p2, p3)
      TRY_LOAD_VAL(const class clang::ObjCCategoryDecl *, p4)
      if (no_more) CLASS_REFLECTION(P, lookupMethod, p0, p1, p2, p3, p4)
      TOO_MANY;
    }
    case currefl::lookupInstanceMethod: {
      CHECK_TRAIT_IS_REFLPROP(lookupInstanceMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) CLASS_REFLECTION(P, lookupInstanceMethod, p0)
      TOO_MANY;
    }
    case currefl::lookupClassMethod: {
      CHECK_TRAIT_IS_REFLPROP(lookupClassMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) CLASS_REFLECTION(P, lookupClassMethod, p0)
      TOO_MANY;
    }
    case currefl::lookupPropertyAccessor: {
      CHECK_TRAIT_IS_REFLPROP(lookupPropertyAccessor)
      TRY_LOAD_VAL(const class clang::Selector, p0)
      TRY_LOAD_VAL(const class clang::ObjCCategoryDecl *, p1)
      TRY_LOAD_VAL(bool, p2)
      if (no_more) CLASS_REFLECTION(P, lookupPropertyAccessor, p0, p1, p2)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getEndOfDefinitionLoc, CLASS, P, getEndOfDefinitionLoc)
    case_METHOD_NOARGS(getSuperClassLoc, CLASS, P, getSuperClassLoc)
    case_METHOD_NOARGS(isImplicitInterfaceDecl, PRIM, P, isImplicitInterfaceDecl)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case_METHOD_NOARGS(getTypeForDecl, CLASS, P, getTypeForDecl)
    case currefl::setTypeForDecl: {
      CHECK_TRAIT_IS_REFLPROP(setTypeForDecl)
      TRY_LOAD_VAL(const class clang::Type *, p0)
      if (no_more) VOID_REFLECTION(P, setTypeForDecl, (const class clang::Type *)p0)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCIvarDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCIvarDecl;
# define CLASSNAME clang::ObjCIvarDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getContainingInterface, CLASS, P, getContainingInterface)
    case_METHOD_NOARGS(getNextIvar, CLASS, P, getNextIvar)
    case_METHOD_NOARGS(getAccessControl, PRIM, P, getAccessControl)
    case_METHOD_NOARGS(getCanonicalAccessControl, PRIM, P, getCanonicalAccessControl)
    case_METHOD_NOARGS(getSynthesize, PRIM, P, getSynthesize)
    case currefl::getUsageType: {
      CHECK_TRAIT_IS_REFLPROP(getUsageType)
      TRY_LOAD_VAL(class clang::QualType, p0)
      if (no_more) CLASS_REFLECTION(P, getUsageType, p0)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCAtDefsFieldDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCAtDefsFieldDecl;
# define CLASSNAME clang::ObjCAtDefsFieldDecl
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCProtocolDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCProtocolDecl;
# define CLASSNAME clang::ObjCProtocolDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getReferencedProtocols, CLASS, P, getReferencedProtocols)
    case currefl::protocols: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::protocols))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, protocols)
      TOO_MANY;
    }
    case currefl::protocol_locs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::protocol_locs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, protocol_locs)
      TOO_MANY;
    }
    case_METHOD_NOARGS(protocol_size, PRIM, P, protocol_size)
    case currefl::lookupMethod: {
      CHECK_TRAIT_IS_REFLPROP(lookupMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) CLASS_REFLECTION(P, lookupMethod, p0, p1)
      TOO_MANY;
    }
    case currefl::lookupInstanceMethod: {
      CHECK_TRAIT_IS_REFLPROP(lookupInstanceMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) CLASS_REFLECTION(P, lookupInstanceMethod, p0)
      TOO_MANY;
    }
    case currefl::lookupClassMethod: {
      CHECK_TRAIT_IS_REFLPROP(lookupClassMethod)
      TRY_LOAD_VAL(class clang::Selector, p0)
      if (no_more) CLASS_REFLECTION(P, lookupClassMethod, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasDefinition, PRIM, P, hasDefinition)
    case_METHOD_NOARGS(getDefinition, CLASS, P, getDefinition)
    case_METHOD_NOARGS(isThisDeclarationADefinition, PRIM, P, isThisDeclarationADefinition)
    case_METHOD_NOARGS(getObjCRuntimeNameAsString, CLASS, P, getObjCRuntimeNameAsString)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getCanonicalDecl, CLASS, P, getCanonicalDecl)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCCategoryDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCCategoryDecl;
# define CLASSNAME clang::ObjCCategoryDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getClassInterface, CLASS, P, getClassInterface)
    case_METHOD_NOARGS(getTypeParamList, CLASS, P, getTypeParamList)
    case_METHOD_NOARGS(getImplementation, CLASS, P, getImplementation)
    case_METHOD_NOARGS(getReferencedProtocols, CLASS, P, getReferencedProtocols)
    case currefl::protocols: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::protocols))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, protocols)
      TOO_MANY;
    }
    case_METHOD_NOARGS(protocol_size, PRIM, P, protocol_size)
    case currefl::protocol_locs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::protocol_locs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, protocol_locs)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNextClassCategory, CLASS, P, getNextClassCategory)
    case_METHOD_NOARGS(getNextClassCategoryRaw, CLASS, P, getNextClassCategoryRaw)
    case_METHOD_NOARGS(IsClassExtension, PRIM, P, IsClassExtension)
    case currefl::ivars: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::ivars))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, ivars)
      TOO_MANY;
    }
    case_METHOD_NOARGS(ivar_size, PRIM, P, ivar_size)
    case_METHOD_NOARGS(ivar_empty, PRIM, P, ivar_empty)
    case_METHOD_NOARGS(getCategoryNameLoc, CLASS, P, getCategoryNameLoc)
    case_METHOD_NOARGS(getIvarLBraceLoc, CLASS, P, getIvarLBraceLoc)
    case_METHOD_NOARGS(getIvarRBraceLoc, CLASS, P, getIvarRBraceLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCImplDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCImplDecl;
# define CLASSNAME clang::ObjCImplDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getClassInterface, CLASS, P, getClassInterface)
    case currefl::FindPropertyImplDecl: {
      CHECK_TRAIT_IS_REFLPROP(FindPropertyImplDecl)
      TRY_LOAD_VAL(class clang::IdentifierInfo *, p0)
      TRY_LOAD_VAL(enum clang::ObjCPropertyQueryKind, p1)
      if (no_more) CLASS_REFLECTION(P, FindPropertyImplDecl, p0, p1)
      TOO_MANY;
    }
    case currefl::FindPropertyImplIvarDecl: {
      CHECK_TRAIT_IS_REFLPROP(FindPropertyImplIvarDecl)
      TRY_LOAD_VAL(class clang::IdentifierInfo *, p0)
      if (no_more) CLASS_REFLECTION(P, FindPropertyImplIvarDecl, p0)
      TOO_MANY;
    }
    case currefl::property_impls: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::property_impls))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, property_impls)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCCategoryImplDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCCategoryImplDecl;
# define CLASSNAME clang::ObjCCategoryImplDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCategoryDecl, CLASS, P, getCategoryDecl)
    case_METHOD_NOARGS(getCategoryNameLoc, CLASS, P, getCategoryNameLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCImplementationDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCImplementationDecl;
# define CLASSNAME clang::ObjCImplementationDecl
  switch( (currefl::memnames)N ) {
    case currefl::inits: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::inits))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, inits)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumIvarInitializers, PRIM, P, getNumIvarInitializers)
    case_METHOD_NOARGS(hasNonZeroConstructors, PRIM, P, hasNonZeroConstructors)
    case_METHOD_NOARGS(hasDestructors, PRIM, P, hasDestructors)
    case_METHOD_NOARGS(getIdentifier, CLASS, P, getIdentifier)
    case_METHOD_NOARGS(getName, CLASS, P, getName)
    case_METHOD_NOARGS(getNameAsString, PRIM, P, getNameAsString)
    case_METHOD_NOARGS(getObjCRuntimeNameAsString, CLASS, P, getObjCRuntimeNameAsString)
    case_METHOD_NOARGS(getSuperClass, CLASS, P, getSuperClass)
    case_METHOD_NOARGS(getSuperClassLoc, CLASS, P, getSuperClassLoc)
    case_METHOD_NOARGS(getIvarLBraceLoc, CLASS, P, getIvarLBraceLoc)
    case_METHOD_NOARGS(getIvarRBraceLoc, CLASS, P, getIvarRBraceLoc)
    case currefl::ivars: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::ivars))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, ivars)
      TOO_MANY;
    }
    case_METHOD_NOARGS(ivar_size, PRIM, P, ivar_size)
    case_METHOD_NOARGS(ivar_empty, PRIM, P, ivar_empty)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCCompatibleAliasDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCCompatibleAliasDecl;
# define CLASSNAME clang::ObjCCompatibleAliasDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getClassInterface, CLASS, P, getClassInterface)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCPropertyImplDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCPropertyImplDecl;
# define CLASSNAME clang::ObjCPropertyImplDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getPropertyDecl, CLASS, P, getPropertyDecl)
    case_METHOD_NOARGS(getPropertyImplementation, PRIM, P, getPropertyImplementation)
    case_METHOD_NOARGS(getPropertyIvarDecl, CLASS, P, getPropertyIvarDecl)
    case_METHOD_NOARGS(getPropertyIvarDeclLoc, CLASS, P, getPropertyIvarDeclLoc)
    case_METHOD_NOARGS(isIvarNameSpecified, PRIM, P, isIvarNameSpecified)
    case_METHOD_NOARGS(getGetterCXXConstructor, CLASS, P, getGetterCXXConstructor)
    case_METHOD_NOARGS(getSetterCXXAssignment, CLASS, P, getSetterCXXAssignment)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPThreadPrivateDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPThreadPrivateDecl;
# define CLASSNAME clang::OMPThreadPrivateDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(varlist_size, PRIM, P, varlist_size)
    case_METHOD_NOARGS(varlist_empty, PRIM, P, varlist_empty)
    case currefl::varlists: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::varlists))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, varlists)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPDeclareReductionDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPDeclareReductionDecl;
# define CLASSNAME clang::OMPDeclareReductionDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCombiner, CLASS, P, getCombiner)
    case_METHOD_NOARGS(getInitializer, CLASS, P, getInitializer)
    case_METHOD_NOARGS(getInitializerKind, PRIM, P, getInitializerKind)
    case_METHOD_NOARGS(getPrevDeclInScope, CLASS, P, getPrevDeclInScope)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPCapturedExprDecl *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPCapturedExprDecl;
# define CLASSNAME clang::OMPCapturedExprDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::classofKind: {
      CHECK_TRAIT_IS_REFLPROP(classofKind)
      TRY_LOAD_VAL(enum clang::Decl::Kind, p0)
      if (no_more) PRIM_REFLECTION(S, classofKind, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXOperatorCallExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXOperatorCallExpr;
# define CLASSNAME clang::CXXOperatorCallExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOperator, PRIM, P, getOperator)
    case currefl::isAssignmentOp: {
      CHECK_TRAIT_IS_REFLPROP(isAssignmentOp)
      TRY_LOAD_VAL(enum clang::OverloadedOperatorKind, p0)
      if (no_more) PRIM_REFLECTION(S, isAssignmentOp, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isAssignmentOp1, PRIM, P, isAssignmentOp)
    case_METHOD_NOARGS(isInfixBinaryOp, PRIM, P, isInfixBinaryOp)
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getFPFeatures, CLASS, P, getFPFeatures)
    case_METHOD_NOARGS(isFPContractableWithinStatement, PRIM, P, isFPContractableWithinStatement)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXMemberCallExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXMemberCallExpr;
# define CLASSNAME clang::CXXMemberCallExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getImplicitObjectArgument, CLASS, P, getImplicitObjectArgument)
    case_METHOD_NOARGS(getMethodDecl, CLASS, P, getMethodDecl)
    case_METHOD_NOARGS(getRecordDecl, CLASS, P, getRecordDecl)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CUDAKernelCallExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CUDAKernelCallExpr;
# define CLASSNAME clang::CUDAKernelCallExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getConfig, CLASS, P, getConfig)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXNamedCastExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXNamedCastExpr;
# define CLASSNAME clang::CXXNamedCastExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCastName, PRIM, P, getCastName)
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getAngleBrackets, CLASS, P, getAngleBrackets)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXStaticCastExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXStaticCastExpr;
# define CLASSNAME clang::CXXStaticCastExpr
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXDynamicCastExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXDynamicCastExpr;
# define CLASSNAME clang::CXXDynamicCastExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isAlwaysNull, PRIM, P, isAlwaysNull)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXReinterpretCastExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXReinterpretCastExpr;
# define CLASSNAME clang::CXXReinterpretCastExpr
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXConstCastExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXConstCastExpr;
# define CLASSNAME clang::CXXConstCastExpr
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UserDefinedLiteral *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UserDefinedLiteral;
# define CLASSNAME clang::UserDefinedLiteral
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLiteralOperatorKind, PRIM, P, getLiteralOperatorKind)
    case_METHOD_NOARGS(getCookedLiteral, CLASS, P, getCookedLiteral)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getUDSuffixLoc, CLASS, P, getUDSuffixLoc)
    case_METHOD_NOARGS(getUDSuffix, CLASS, P, getUDSuffix)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXBoolLiteralExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXBoolLiteralExpr;
# define CLASSNAME clang::CXXBoolLiteralExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getValue, PRIM, P, getValue)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXNullPtrLiteralExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXNullPtrLiteralExpr;
# define CLASSNAME clang::CXXNullPtrLiteralExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXStdInitializerListExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXStdInitializerListExpr;
# define CLASSNAME clang::CXXStdInitializerListExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXTypeidExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXTypeidExpr;
# define CLASSNAME clang::CXXTypeidExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isPotentiallyEvaluated, PRIM, P, isPotentiallyEvaluated)
    case_METHOD_NOARGS(isTypeOperand, PRIM, P, isTypeOperand)
    case_METHOD_NOARGS(getTypeOperandSourceInfo, CLASS, P, getTypeOperandSourceInfo)
    case_METHOD_NOARGS(getExprOperand, CLASS, P, getExprOperand)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::MSPropertyRefExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__MSPropertyRefExpr;
# define CLASSNAME clang::MSPropertyRefExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(isImplicitAccess, PRIM, P, isImplicitAccess)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getBaseExpr, CLASS, P, getBaseExpr)
    case_METHOD_NOARGS(getPropertyDecl, CLASS, P, getPropertyDecl)
    case_METHOD_NOARGS(isArrow, PRIM, P, isArrow)
    case_METHOD_NOARGS(getMemberLoc, CLASS, P, getMemberLoc)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::MSPropertySubscriptExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__MSPropertySubscriptExpr;
# define CLASSNAME clang::MSPropertySubscriptExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(getIdx, CLASS, P, getIdx)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getRBracketLoc, CLASS, P, getRBracketLoc)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXUuidofExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXUuidofExpr;
# define CLASSNAME clang::CXXUuidofExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isTypeOperand, PRIM, P, isTypeOperand)
    case_METHOD_NOARGS(getTypeOperandSourceInfo, CLASS, P, getTypeOperandSourceInfo)
    case_METHOD_NOARGS(getExprOperand, CLASS, P, getExprOperand)
    case_METHOD_NOARGS(getUuidStr, CLASS, P, getUuidStr)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXThisExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXThisExpr;
# define CLASSNAME clang::CXXThisExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(isImplicit, PRIM, P, isImplicit)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXThrowExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXThrowExpr;
# define CLASSNAME clang::CXXThrowExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(getThrowLoc, CLASS, P, getThrowLoc)
    case_METHOD_NOARGS(isThrownVariableInScope, PRIM, P, isThrownVariableInScope)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXDefaultArgExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXDefaultArgExpr;
# define CLASSNAME clang::CXXDefaultArgExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getParam, CLASS, P, getParam)
    case_METHOD_NOARGS(getExpr, CLASS, P, getExpr)
    case_METHOD_NOARGS(getUsedLocation, CLASS, P, getUsedLocation)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXDefaultInitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXDefaultInitExpr;
# define CLASSNAME clang::CXXDefaultInitExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getField, CLASS, P, getField)
    case_METHOD_NOARGS(getExpr, CLASS, P, getExpr)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXTemporary *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__CXXTemporary;
# define CLASSNAME clang::CXXTemporary
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDestructor, CLASS, P, getDestructor)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXBindTemporaryExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXBindTemporaryExpr;
# define CLASSNAME clang::CXXBindTemporaryExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTemporary, CLASS, P, getTemporary)
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXConstructExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXConstructExpr;
# define CLASSNAME clang::CXXConstructExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getConstructor, CLASS, P, getConstructor)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(isElidable, PRIM, P, isElidable)
    case_METHOD_NOARGS(hadMultipleCandidates, PRIM, P, hadMultipleCandidates)
    case_METHOD_NOARGS(isListInitialization, PRIM, P, isListInitialization)
    case_METHOD_NOARGS(isStdInitListInitialization, PRIM, P, isStdInitListInitialization)
    case_METHOD_NOARGS(requiresZeroInitialization, PRIM, P, requiresZeroInitialization)
    case_METHOD_NOARGS(getConstructionKind, PRIM, P, getConstructionKind)
    case currefl::arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, arguments)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getArgs, CLASS, P, getArgs)
    case_METHOD_NOARGS(getNumArgs, PRIM, P, getNumArgs)
    case currefl::getArg: {
      CHECK_TRAIT_IS_REFLPROP(getArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArg, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getParenOrBraceRange, CLASS, P, getParenOrBraceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXInheritedCtorInitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXInheritedCtorInitExpr;
# define CLASSNAME clang::CXXInheritedCtorInitExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getConstructor, CLASS, P, getConstructor)
    case_METHOD_NOARGS(constructsVBase, PRIM, P, constructsVBase)
    case_METHOD_NOARGS(getConstructionKind, PRIM, P, getConstructionKind)
    case_METHOD_NOARGS(inheritedFromVBase, PRIM, P, inheritedFromVBase)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXFunctionalCastExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXFunctionalCastExpr;
# define CLASSNAME clang::CXXFunctionalCastExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLParenLoc, CLASS, P, getLParenLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(isListInitialization, PRIM, P, isListInitialization)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXTemporaryObjectExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXTemporaryObjectExpr;
# define CLASSNAME clang::CXXTemporaryObjectExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::LambdaExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__LambdaExpr;
# define CLASSNAME clang::LambdaExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCaptureDefault, PRIM, P, getCaptureDefault)
    case_METHOD_NOARGS(getCaptureDefaultLoc, CLASS, P, getCaptureDefaultLoc)
    case currefl::isInitCapture: {
      CHECK_TRAIT_IS_REFLPROP(isInitCapture)
      TRY_LOAD_VAL(const class clang::LambdaCapture *, p0)
      if (no_more) PRIM_REFLECTION(P, isInitCapture, p0)
      TOO_MANY;
    }
    case currefl::captures: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::captures))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, captures)
      TOO_MANY;
    }
    case_METHOD_NOARGS(capture_size, PRIM, P, capture_size)
    case currefl::explicit_captures: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::explicit_captures))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, explicit_captures)
      TOO_MANY;
    }
    case currefl::implicit_captures: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::implicit_captures))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, implicit_captures)
      TOO_MANY;
    }
    case currefl::capture_inits: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::capture_inits))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, capture_inits)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getIntroducerRange, CLASS, P, getIntroducerRange)
    case_METHOD_NOARGS(getLambdaClass, CLASS, P, getLambdaClass)
    case_METHOD_NOARGS(getCallOperator, CLASS, P, getCallOperator)
    case_METHOD_NOARGS(getTemplateParameterList, CLASS, P, getTemplateParameterList)
    case_METHOD_NOARGS(isGenericLambda, PRIM, P, isGenericLambda)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(isMutable, PRIM, P, isMutable)
    case_METHOD_NOARGS(hasExplicitParameters, PRIM, P, hasExplicitParameters)
    case_METHOD_NOARGS(hasExplicitResultType, PRIM, P, hasExplicitResultType)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXScalarValueInitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXScalarValueInitExpr;
# define CLASSNAME clang::CXXScalarValueInitExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXNewExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXNewExpr;
# define CLASSNAME clang::CXXNewExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAllocatedType, CLASS, P, getAllocatedType)
    case_METHOD_NOARGS(getAllocatedTypeSourceInfo, CLASS, P, getAllocatedTypeSourceInfo)
    case_METHOD_NOARGS(getOperatorNew, CLASS, P, getOperatorNew)
    case_METHOD_NOARGS(getOperatorDelete, CLASS, P, getOperatorDelete)
    case_METHOD_NOARGS(isArray, PRIM, P, isArray)
    case_METHOD_NOARGS(getArraySize, CLASS, P, getArraySize)
    case_METHOD_NOARGS(getNumPlacementArgs, PRIM, P, getNumPlacementArgs)
    case currefl::getPlacementArg: {
      CHECK_TRAIT_IS_REFLPROP(getPlacementArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getPlacementArg, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isParenTypeId, PRIM, P, isParenTypeId)
    case_METHOD_NOARGS(getTypeIdParens, CLASS, P, getTypeIdParens)
    case_METHOD_NOARGS(isGlobalNew, PRIM, P, isGlobalNew)
    case_METHOD_NOARGS(hasInitializer, PRIM, P, hasInitializer)
    case_METHOD_NOARGS(getInitializationStyle, PRIM, P, getInitializationStyle)
    case_METHOD_NOARGS(getInitializer, CLASS, P, getInitializer)
    case_METHOD_NOARGS(getConstructExpr, CLASS, P, getConstructExpr)
    case_METHOD_NOARGS(passAlignment, PRIM, P, passAlignment)
    case_METHOD_NOARGS(doesUsualArrayDeleteWantSize, PRIM, P, doesUsualArrayDeleteWantSize)
    case currefl::placement_arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::placement_arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, placement_arguments)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getStartLoc, CLASS, P, getStartLoc)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getDirectInitRange, CLASS, P, getDirectInitRange)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXDeleteExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXDeleteExpr;
# define CLASSNAME clang::CXXDeleteExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isGlobalDelete, PRIM, P, isGlobalDelete)
    case_METHOD_NOARGS(isArrayForm, PRIM, P, isArrayForm)
    case_METHOD_NOARGS(isArrayFormAsWritten, PRIM, P, isArrayFormAsWritten)
    case_METHOD_NOARGS(doesUsualArrayDeleteWantSize, PRIM, P, doesUsualArrayDeleteWantSize)
    case_METHOD_NOARGS(getOperatorDelete, CLASS, P, getOperatorDelete)
    case_METHOD_NOARGS(getArgument, CLASS, P, getArgument)
    case_METHOD_NOARGS(getDestroyedType, CLASS, P, getDestroyedType)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXPseudoDestructorExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXPseudoDestructorExpr;
# define CLASSNAME clang::CXXPseudoDestructorExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(hasQualifier, PRIM, P, hasQualifier)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(isArrow, PRIM, P, isArrow)
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(getScopeTypeInfo, CLASS, P, getScopeTypeInfo)
    case_METHOD_NOARGS(getColonColonLoc, CLASS, P, getColonColonLoc)
    case_METHOD_NOARGS(getTildeLoc, CLASS, P, getTildeLoc)
    case_METHOD_NOARGS(getDestroyedTypeInfo, CLASS, P, getDestroyedTypeInfo)
    case_METHOD_NOARGS(getDestroyedTypeIdentifier, CLASS, P, getDestroyedTypeIdentifier)
    case_METHOD_NOARGS(getDestroyedType, CLASS, P, getDestroyedType)
    case_METHOD_NOARGS(getDestroyedTypeLoc, CLASS, P, getDestroyedTypeLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::TypeTraitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__TypeTraitExpr;
# define CLASSNAME clang::TypeTraitExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTrait, PRIM, P, getTrait)
    case_METHOD_NOARGS(getValue, PRIM, P, getValue)
    case_METHOD_NOARGS(getNumArgs, PRIM, P, getNumArgs)
    case currefl::getArg: {
      CHECK_TRAIT_IS_REFLPROP(getArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArg, p0)
      TOO_MANY;
    }
    case currefl::getArgs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getArgs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getArgs)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ArrayTypeTraitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ArrayTypeTraitExpr;
# define CLASSNAME clang::ArrayTypeTraitExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getTrait, PRIM, P, getTrait)
    case_METHOD_NOARGS(getQueriedType, CLASS, P, getQueriedType)
    case_METHOD_NOARGS(getQueriedTypeSourceInfo, CLASS, P, getQueriedTypeSourceInfo)
    case_METHOD_NOARGS(getValue, PRIM, P, getValue)
    case_METHOD_NOARGS(getDimensionExpression, CLASS, P, getDimensionExpression)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ExpressionTraitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ExpressionTraitExpr;
# define CLASSNAME clang::ExpressionTraitExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getTrait, PRIM, P, getTrait)
    case_METHOD_NOARGS(getQueriedExpression, CLASS, P, getQueriedExpression)
    case_METHOD_NOARGS(getValue, PRIM, P, getValue)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OverloadExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OverloadExpr;
# define CLASSNAME clang::OverloadExpr
  switch( (currefl::memnames)N ) {
    case currefl::find: {
      CHECK_TRAIT_IS_REFLPROP(find)
      TRY_LOAD_VAL(class clang::Expr *, p0)
      if (no_more) CLASS_REFLECTION(S, find, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNamingClass, CLASS, P, getNamingClass)
    case currefl::decls: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::decls))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, decls)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumDecls, PRIM, P, getNumDecls)
    case_METHOD_NOARGS(getNameInfo, CLASS, P, getNameInfo)
    case_METHOD_NOARGS(getName, CLASS, P, getName)
    case_METHOD_NOARGS(getNameLoc, CLASS, P, getNameLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getTemplateKeywordLoc, CLASS, P, getTemplateKeywordLoc)
    case_METHOD_NOARGS(getLAngleLoc, CLASS, P, getLAngleLoc)
    case_METHOD_NOARGS(getRAngleLoc, CLASS, P, getRAngleLoc)
    case_METHOD_NOARGS(hasTemplateKeyword, PRIM, P, hasTemplateKeyword)
    case_METHOD_NOARGS(hasExplicitTemplateArgs, PRIM, P, hasExplicitTemplateArgs)
    case_METHOD_NOARGS(getTemplateArgs, CLASS, P, getTemplateArgs)
    case_METHOD_NOARGS(getNumTemplateArgs, PRIM, P, getNumTemplateArgs)
    case currefl::template_arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::template_arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, template_arguments)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OverloadExpr::FindResult X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__OverloadExpr__FindResult;
# define CLASSNAME clang::OverloadExpr::FindResult
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, V, Expression)
    case_FIELD(PRIM, V, IsAddressOfOperand)
    case_FIELD(PRIM, V, HasFormOfMemberPointer)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UnresolvedLookupExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UnresolvedLookupExpr;
# define CLASSNAME clang::UnresolvedLookupExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(requiresADL, PRIM, P, requiresADL)
    case_METHOD_NOARGS(isOverloaded, PRIM, P, isOverloaded)
    case_METHOD_NOARGS(getNamingClass, CLASS, P, getNamingClass)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DependentScopeDeclRefExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DependentScopeDeclRefExpr;
# define CLASSNAME clang::DependentScopeDeclRefExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNameInfo, CLASS, P, getNameInfo)
    case_METHOD_NOARGS(getDeclName, CLASS, P, getDeclName)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getTemplateKeywordLoc, CLASS, P, getTemplateKeywordLoc)
    case_METHOD_NOARGS(getLAngleLoc, CLASS, P, getLAngleLoc)
    case_METHOD_NOARGS(getRAngleLoc, CLASS, P, getRAngleLoc)
    case_METHOD_NOARGS(hasTemplateKeyword, PRIM, P, hasTemplateKeyword)
    case_METHOD_NOARGS(hasExplicitTemplateArgs, PRIM, P, hasExplicitTemplateArgs)
    case_METHOD_NOARGS(getTemplateArgs, CLASS, P, getTemplateArgs)
    case_METHOD_NOARGS(getNumTemplateArgs, PRIM, P, getNumTemplateArgs)
    case currefl::template_arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::template_arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, template_arguments)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ExprWithCleanups *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ExprWithCleanups;
# define CLASSNAME clang::ExprWithCleanups
  switch( (currefl::memnames)N ) {
    case currefl::getObjects: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getObjects))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getObjects)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumObjects, PRIM, P, getNumObjects)
    case currefl::getObject: {
      CHECK_TRAIT_IS_REFLPROP(getObject)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getObject, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(cleanupsHaveSideEffects, PRIM, P, cleanupsHaveSideEffects)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXUnresolvedConstructExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXUnresolvedConstructExpr;
# define CLASSNAME clang::CXXUnresolvedConstructExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTypeAsWritten, CLASS, P, getTypeAsWritten)
    case_METHOD_NOARGS(getTypeSourceInfo, CLASS, P, getTypeSourceInfo)
    case_METHOD_NOARGS(getLParenLoc, CLASS, P, getLParenLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(isListInitialization, PRIM, P, isListInitialization)
    case_METHOD_NOARGS(arg_size, PRIM, P, arg_size)
    case currefl::getArg: {
      CHECK_TRAIT_IS_REFLPROP(getArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArg, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXDependentScopeMemberExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXDependentScopeMemberExpr;
# define CLASSNAME clang::CXXDependentScopeMemberExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isImplicitAccess, PRIM, P, isImplicitAccess)
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(getBaseType, CLASS, P, getBaseType)
    case_METHOD_NOARGS(isArrow, PRIM, P, isArrow)
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(getQualifier, CLASS, P, getQualifier)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getFirstQualifierFoundInScope, CLASS, P, getFirstQualifierFoundInScope)
    case_METHOD_NOARGS(getMemberNameInfo, CLASS, P, getMemberNameInfo)
    case_METHOD_NOARGS(getMember, CLASS, P, getMember)
    case_METHOD_NOARGS(getMemberLoc, CLASS, P, getMemberLoc)
    case_METHOD_NOARGS(getTemplateKeywordLoc, CLASS, P, getTemplateKeywordLoc)
    case_METHOD_NOARGS(getLAngleLoc, CLASS, P, getLAngleLoc)
    case_METHOD_NOARGS(getRAngleLoc, CLASS, P, getRAngleLoc)
    case_METHOD_NOARGS(hasTemplateKeyword, PRIM, P, hasTemplateKeyword)
    case_METHOD_NOARGS(hasExplicitTemplateArgs, PRIM, P, hasExplicitTemplateArgs)
    case_METHOD_NOARGS(getTemplateArgs, CLASS, P, getTemplateArgs)
    case_METHOD_NOARGS(getNumTemplateArgs, PRIM, P, getNumTemplateArgs)
    case currefl::template_arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::template_arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, template_arguments)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::UnresolvedMemberExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__UnresolvedMemberExpr;
# define CLASSNAME clang::UnresolvedMemberExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isImplicitAccess, PRIM, P, isImplicitAccess)
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(getBaseType, CLASS, P, getBaseType)
    case_METHOD_NOARGS(hasUnresolvedUsing, PRIM, P, hasUnresolvedUsing)
    case_METHOD_NOARGS(isArrow, PRIM, P, isArrow)
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(getNamingClass, CLASS, P, getNamingClass)
    case_METHOD_NOARGS(getMemberNameInfo, CLASS, P, getMemberNameInfo)
    case_METHOD_NOARGS(getMemberName, CLASS, P, getMemberName)
    case_METHOD_NOARGS(getMemberLoc, CLASS, P, getMemberLoc)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXNoexceptExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXNoexceptExpr;
# define CLASSNAME clang::CXXNoexceptExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOperand, CLASS, P, getOperand)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getValue, PRIM, P, getValue)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::PackExpansionExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__PackExpansionExpr;
# define CLASSNAME clang::PackExpansionExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getPattern, CLASS, P, getPattern)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, P, getEllipsisLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SizeOfPackExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__SizeOfPackExpr;
# define CLASSNAME clang::SizeOfPackExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(getPackLoc, CLASS, P, getPackLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getPack, CLASS, P, getPack)
    case_METHOD_NOARGS(getPackLength, PRIM, P, getPackLength)
    case_METHOD_NOARGS(isPartiallySubstituted, PRIM, P, isPartiallySubstituted)
    case currefl::getPartialArguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getPartialArguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getPartialArguments)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SubstNonTypeTemplateParmExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__SubstNonTypeTemplateParmExpr;
# define CLASSNAME clang::SubstNonTypeTemplateParmExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNameLoc, CLASS, P, getNameLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getReplacement, CLASS, P, getReplacement)
    case_METHOD_NOARGS(getParameter, CLASS, P, getParameter)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::SubstNonTypeTemplateParmPackExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__SubstNonTypeTemplateParmPackExpr;
# define CLASSNAME clang::SubstNonTypeTemplateParmPackExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getParameterPack, CLASS, P, getParameterPack)
    case_METHOD_NOARGS(getParameterPackLocation, CLASS, P, getParameterPackLocation)
    case_METHOD_NOARGS(getArgumentPack, CLASS, P, getArgumentPack)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::FunctionParmPackExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__FunctionParmPackExpr;
# define CLASSNAME clang::FunctionParmPackExpr
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(P)
      TOO_MANY;
};
    case_METHOD_NOARGS(getParameterPack, CLASS, P, getParameterPack)
    case_METHOD_NOARGS(getParameterPackLocation, CLASS, P, getParameterPackLocation)
    case_METHOD_NOARGS(getNumExpansions, PRIM, P, getNumExpansions)
    case currefl::getExpansion: {
      CHECK_TRAIT_IS_REFLPROP(getExpansion)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getExpansion, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::MaterializeTemporaryExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__MaterializeTemporaryExpr;
# define CLASSNAME clang::MaterializeTemporaryExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTemporary, CLASS, P, getTemporary)
    case_METHOD_NOARGS(GetTemporaryExpr, CLASS, P, GetTemporaryExpr)
    case_METHOD_NOARGS(getStorageDuration, PRIM, P, getStorageDuration)
    case_METHOD_NOARGS(getExtendingDecl, CLASS, P, getExtendingDecl)
    case_METHOD_NOARGS(getManglingNumber, PRIM, P, getManglingNumber)
    case_METHOD_NOARGS(isBoundToLvalueReference, PRIM, P, isBoundToLvalueReference)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXFoldExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXFoldExpr;
# define CLASSNAME clang::CXXFoldExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLHS, CLASS, P, getLHS)
    case_METHOD_NOARGS(getRHS, CLASS, P, getRHS)
    case_METHOD_NOARGS(isRightFold, PRIM, P, isRightFold)
    case_METHOD_NOARGS(isLeftFold, PRIM, P, isLeftFold)
    case_METHOD_NOARGS(getPattern, CLASS, P, getPattern)
    case_METHOD_NOARGS(getInit, CLASS, P, getInit)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, P, getEllipsisLoc)
    case_METHOD_NOARGS(getOperator, PRIM, P, getOperator)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CoroutineSuspendExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CoroutineSuspendExpr;
# define CLASSNAME clang::CoroutineSuspendExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKeywordLoc, CLASS, P, getKeywordLoc)
    case_METHOD_NOARGS(getCommonExpr, CLASS, P, getCommonExpr)
    case_METHOD_NOARGS(getOpaqueValue, CLASS, P, getOpaqueValue)
    case_METHOD_NOARGS(getReadyExpr, CLASS, P, getReadyExpr)
    case_METHOD_NOARGS(getSuspendExpr, CLASS, P, getSuspendExpr)
    case_METHOD_NOARGS(getResumeExpr, CLASS, P, getResumeExpr)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CoawaitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CoawaitExpr;
# define CLASSNAME clang::CoawaitExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOperand, CLASS, P, getOperand)
    case_METHOD_NOARGS(isImplicit, PRIM, P, isImplicit)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DependentCoawaitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__DependentCoawaitExpr;
# define CLASSNAME clang::DependentCoawaitExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOperand, CLASS, P, getOperand)
    case_METHOD_NOARGS(getOperatorCoawaitLookup, CLASS, P, getOperatorCoawaitLookup)
    case_METHOD_NOARGS(getKeywordLoc, CLASS, P, getKeywordLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CoyieldExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CoyieldExpr;
# define CLASSNAME clang::CoyieldExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getOperand, CLASS, P, getOperand)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ReflectionExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ReflectionExpr;
# define CLASSNAME clang::ReflectionExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isReflexpr, PRIM, P, isReflexpr)
    case_METHOD_NOARGS(hasTypeOperand, PRIM, P, hasTypeOperand)
    case_METHOD_NOARGS(hasExpressionOperand, PRIM, P, hasExpressionOperand)
    case_METHOD_NOARGS(getTypeOperand, CLASS, P, getTypeOperand)
    case_METHOD_NOARGS(getExpressionOperand, CLASS, P, getExpressionOperand)
    case_METHOD_NOARGS(getOperatorLoc, CLASS, P, getOperatorLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CompilerMessageExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CompilerMessageExpr;
# define CLASSNAME clang::CompilerMessageExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getMessage, CLASS, P, getMessage)
    case_METHOD_NOARGS(getKWLoc, CLASS, P, getKWLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CompilerDiagnosticExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CompilerDiagnosticExpr;
# define CLASSNAME clang::CompilerDiagnosticExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKWLoc, CLASS, P, getKWLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(arg_size, PRIM, P, arg_size)
    case currefl::getArgs: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getArgs))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getArgs)
      TOO_MANY;
    }
    case currefl::getArg: {
      CHECK_TRAIT_IS_REFLPROP(getArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArg, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isError, PRIM, P, isError)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ReflectionTraitExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ReflectionTraitExpr;
# define CLASSNAME clang::ReflectionTraitExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getTraitKind, PRIM, P, getTraitKind)
    case_METHOD_NOARGS(getNumArgs, PRIM, P, getNumArgs)
    case currefl::getArg: {
      CHECK_TRAIT_IS_REFLPROP(getArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArg, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getArgs, CLASS, P, getArgs)
    case_METHOD_NOARGS(getKWLoc, CLASS, P, getKWLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ReflectionTraitTypedExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ReflectionTraitTypedExpr;
# define CLASSNAME clang::ReflectionTraitTypedExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getObjKind, PRIM, P, getObjKind)
    case_METHOD_NOARGS(getMemNum, PRIM, P, getMemNum)
    case_METHOD_NOARGS(isPtr, PRIM, P, isPtr)
    case_METHOD_NOARGS(getNonprimCTDorNull, CLASS, P, getNonprimCTDorNull)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::VoidReflectionExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__VoidReflectionExpr;
# define CLASSNAME clang::VoidReflectionExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(doit, VOID, P, doit)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::NonvoidReflectionExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__NonvoidReflectionExpr;
# define CLASSNAME clang::NonvoidReflectionExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(doit, VOID, P, doit)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ReflectNewExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ReflectNewExpr;
# define CLASSNAME clang::ReflectNewExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNumArgs, PRIM, P, getNumArgs)
    case currefl::getArg: {
      CHECK_TRAIT_IS_REFLPROP(getArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArg, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getArgs, CLASS, P, getArgs)
    case_METHOD_NOARGS(getObjKind, PRIM, P, getObjKind)
    case_METHOD_NOARGS(getTraitLoc, CLASS, P, getTraitLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ReflectDeleteExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ReflectDeleteExpr;
# define CLASSNAME clang::ReflectDeleteExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNumArgs, PRIM, P, getNumArgs)
    case currefl::getArg: {
      CHECK_TRAIT_IS_REFLPROP(getArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArg, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getArgs, CLASS, P, getArgs)
    case_METHOD_NOARGS(getObjKind, PRIM, P, getObjKind)
    case_METHOD_NOARGS(getTraitLoc, CLASS, P, getTraitLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXConstantExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXConstantExpr;
# define CLASSNAME clang::CXXConstantExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getExpression, CLASS, P, getExpression)
    case_METHOD_NOARGS(getValue, CLASS, P, getValue)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXDependentIdExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXDependentIdExpr;
# define CLASSNAME clang::CXXDependentIdExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNameInfo, CLASS, P, getNameInfo)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXConcatenateExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXConcatenateExpr;
# define CLASSNAME clang::CXXConcatenateExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNumOperands, PRIM, P, getNumOperands)
    case currefl::getOperand: {
      CHECK_TRAIT_IS_REFLPROP(getOperand)
      TRY_LOAD_VAL(unsigned long, p0)
      if (no_more) CLASS_REFLECTION(P, getOperand, p0)
      TOO_MANY;
    }
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getIntroLoc, CLASS, P, getIntroLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCStringLiteral *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCStringLiteral;
# define CLASSNAME clang::ObjCStringLiteral
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getString, CLASS, P, getString)
    case_METHOD_NOARGS(getAtLoc, CLASS, P, getAtLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCBoolLiteralExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCBoolLiteralExpr;
# define CLASSNAME clang::ObjCBoolLiteralExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getValue, PRIM, P, getValue)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCBoxedExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCBoxedExpr;
# define CLASSNAME clang::ObjCBoxedExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(getBoxingMethod, CLASS, P, getBoxingMethod)
    case_METHOD_NOARGS(getAtLoc, CLASS, P, getAtLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCArrayLiteral *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCArrayLiteral;
# define CLASSNAME clang::ObjCArrayLiteral
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(getElements, CLASS, P, getElements)
    case_METHOD_NOARGS(getNumElements, PRIM, P, getNumElements)
    case currefl::getElement: {
      CHECK_TRAIT_IS_REFLPROP(getElement)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getElement, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getArrayWithObjectsMethod, CLASS, P, getArrayWithObjectsMethod)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCDictionaryElement X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__ObjCDictionaryElement;
# define CLASSNAME clang::ObjCDictionaryElement
  switch( (currefl::memnames)N ) {
    case_FIELD(CLASS, V, Key)
    case_FIELD(CLASS, V, Value)
    case_FIELD(CLASS, V, EllipsisLoc)
    case_METHOD_NOARGS(isPackExpansion, PRIM, V, isPackExpansion)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCDictionaryLiteral *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCDictionaryLiteral;
# define CLASSNAME clang::ObjCDictionaryLiteral
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNumElements, PRIM, P, getNumElements)
    case currefl::getKeyValueElement: {
      CHECK_TRAIT_IS_REFLPROP(getKeyValueElement)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getKeyValueElement, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getDictWithObjectsMethod, CLASS, P, getDictWithObjectsMethod)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCEncodeExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCEncodeExpr;
# define CLASSNAME clang::ObjCEncodeExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAtLoc, CLASS, P, getAtLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getEncodedType, CLASS, P, getEncodedType)
    case_METHOD_NOARGS(getEncodedTypeSourceInfo, CLASS, P, getEncodedTypeSourceInfo)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCSelectorExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCSelectorExpr;
# define CLASSNAME clang::ObjCSelectorExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSelector, CLASS, P, getSelector)
    case_METHOD_NOARGS(getAtLoc, CLASS, P, getAtLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getNumArgs, PRIM, P, getNumArgs)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCProtocolExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCProtocolExpr;
# define CLASSNAME clang::ObjCProtocolExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getProtocol, CLASS, P, getProtocol)
    case_METHOD_NOARGS(getProtocolIdLoc, CLASS, P, getProtocolIdLoc)
    case_METHOD_NOARGS(getAtLoc, CLASS, P, getAtLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCIvarRefExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCIvarRefExpr;
# define CLASSNAME clang::ObjCIvarRefExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDecl, CLASS, P, getDecl)
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(isArrow, PRIM, P, isArrow)
    case_METHOD_NOARGS(isFreeIvar, PRIM, P, isFreeIvar)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getOpLoc, CLASS, P, getOpLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCPropertyRefExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCPropertyRefExpr;
# define CLASSNAME clang::ObjCPropertyRefExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isImplicitProperty, PRIM, P, isImplicitProperty)
    case_METHOD_NOARGS(isExplicitProperty, PRIM, P, isExplicitProperty)
    case_METHOD_NOARGS(getExplicitProperty, CLASS, P, getExplicitProperty)
    case_METHOD_NOARGS(getImplicitPropertyGetter, CLASS, P, getImplicitPropertyGetter)
    case_METHOD_NOARGS(getImplicitPropertySetter, CLASS, P, getImplicitPropertySetter)
    case_METHOD_NOARGS(getGetterSelector, CLASS, P, getGetterSelector)
    case_METHOD_NOARGS(getSetterSelector, CLASS, P, getSetterSelector)
    case_METHOD_NOARGS(isMessagingGetter, PRIM, P, isMessagingGetter)
    case_METHOD_NOARGS(isMessagingSetter, PRIM, P, isMessagingSetter)
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(getLocation, CLASS, P, getLocation)
    case_METHOD_NOARGS(getReceiverLocation, CLASS, P, getReceiverLocation)
    case_METHOD_NOARGS(getSuperReceiverType, CLASS, P, getSuperReceiverType)
    case_METHOD_NOARGS(getClassReceiver, CLASS, P, getClassReceiver)
    case_METHOD_NOARGS(isObjectReceiver, PRIM, P, isObjectReceiver)
    case_METHOD_NOARGS(isSuperReceiver, PRIM, P, isSuperReceiver)
    case_METHOD_NOARGS(isClassReceiver, PRIM, P, isClassReceiver)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCSubscriptRefExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCSubscriptRefExpr;
# define CLASSNAME clang::ObjCSubscriptRefExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getRBracket, CLASS, P, getRBracket)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getBaseExpr, CLASS, P, getBaseExpr)
    case_METHOD_NOARGS(getKeyExpr, CLASS, P, getKeyExpr)
    case_METHOD_NOARGS(getAtIndexMethodDecl, CLASS, P, getAtIndexMethodDecl)
    case_METHOD_NOARGS(setAtIndexMethodDecl, CLASS, P, setAtIndexMethodDecl)
    case_METHOD_NOARGS(isArraySubscriptRefExpr, PRIM, P, isArraySubscriptRefExpr)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCMessageExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCMessageExpr;
# define CLASSNAME clang::ObjCMessageExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(isImplicit, PRIM, P, isImplicit)
    case_METHOD_NOARGS(getReceiverKind, PRIM, P, getReceiverKind)
    case_METHOD_NOARGS(getReceiverRange, CLASS, P, getReceiverRange)
    case_METHOD_NOARGS(isInstanceMessage, PRIM, P, isInstanceMessage)
    case_METHOD_NOARGS(isClassMessage, PRIM, P, isClassMessage)
    case_METHOD_NOARGS(getInstanceReceiver, CLASS, P, getInstanceReceiver)
    case_METHOD_NOARGS(getClassReceiver, CLASS, P, getClassReceiver)
    case_METHOD_NOARGS(getClassReceiverTypeInfo, CLASS, P, getClassReceiverTypeInfo)
    case_METHOD_NOARGS(getSuperLoc, CLASS, P, getSuperLoc)
    case_METHOD_NOARGS(getReceiverType, CLASS, P, getReceiverType)
    case_METHOD_NOARGS(getReceiverInterface, CLASS, P, getReceiverInterface)
    case_METHOD_NOARGS(getSuperType, CLASS, P, getSuperType)
    case_METHOD_NOARGS(getSelector, CLASS, P, getSelector)
    case_METHOD_NOARGS(getMethodDecl, CLASS, P, getMethodDecl)
    case_METHOD_NOARGS(getMethodFamily, PRIM, P, getMethodFamily)
    case_METHOD_NOARGS(getNumArgs, PRIM, P, getNumArgs)
    case_METHOD_NOARGS(getArgs, CLASS, P, getArgs)
    case currefl::getArg: {
      CHECK_TRAIT_IS_REFLPROP(getArg)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getArg, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(isDelegateInitCall, PRIM, P, isDelegateInitCall)
    case_METHOD_NOARGS(getLeftLoc, CLASS, P, getLeftLoc)
    case_METHOD_NOARGS(getRightLoc, CLASS, P, getRightLoc)
    case_METHOD_NOARGS(getSelectorStartLoc, CLASS, P, getSelectorStartLoc)
    case currefl::getSelectorLoc: {
      CHECK_TRAIT_IS_REFLPROP(getSelectorLoc)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getSelectorLoc, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getNumSelectorLocs, PRIM, P, getNumSelectorLocs)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::arguments: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::arguments))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, arguments)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCIsaExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCIsaExpr;
# define CLASSNAME clang::ObjCIsaExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case_METHOD_NOARGS(isArrow, PRIM, P, isArrow)
    case_METHOD_NOARGS(getIsaMemberLoc, CLASS, P, getIsaMemberLoc)
    case_METHOD_NOARGS(getOpLoc, CLASS, P, getOpLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getBaseLocEnd, CLASS, P, getBaseLocEnd)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCIndirectCopyRestoreExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCIndirectCopyRestoreExpr;
# define CLASSNAME clang::ObjCIndirectCopyRestoreExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSubExpr, CLASS, P, getSubExpr)
    case_METHOD_NOARGS(shouldCopy, PRIM, P, shouldCopy)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCBridgedCastExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCBridgedCastExpr;
# define CLASSNAME clang::ObjCBridgedCastExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLParenLoc, CLASS, P, getLParenLoc)
    case_METHOD_NOARGS(getBridgeKind, PRIM, P, getBridgeKind)
    case_METHOD_NOARGS(getBridgeKindName, CLASS, P, getBridgeKindName)
    case_METHOD_NOARGS(getBridgeKeywordLoc, CLASS, P, getBridgeKeywordLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCAvailabilityCheckExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCAvailabilityCheckExpr;
# define CLASSNAME clang::ObjCAvailabilityCheckExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getSourceRange, CLASS, P, getSourceRange)
    case_METHOD_NOARGS(hasVersion, PRIM, P, hasVersion)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPArraySectionExpr *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPArraySectionExpr;
# define CLASSNAME clang::OMPArraySectionExpr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getBase, CLASS, P, getBase)
    case currefl::getBaseOriginalType: {
      CHECK_TRAIT_IS_REFLPROP(getBaseOriginalType)
      TRY_LOAD_VAL(const class clang::Expr *, p0)
      if (no_more) CLASS_REFLECTION(S, getBaseOriginalType, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLowerBound, CLASS, P, getLowerBound)
    case_METHOD_NOARGS(getLength, CLASS, P, getLength)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getColonLoc, CLASS, P, getColonLoc)
    case_METHOD_NOARGS(getRBracketLoc, CLASS, P, getRBracketLoc)
    case_METHOD_NOARGS(getExprLoc, CLASS, P, getExprLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXCatchStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXCatchStmt;
# define CLASSNAME clang::CXXCatchStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getCatchLoc, CLASS, P, getCatchLoc)
    case_METHOD_NOARGS(getExceptionDecl, CLASS, P, getExceptionDecl)
    case_METHOD_NOARGS(getCaughtType, CLASS, P, getCaughtType)
    case_METHOD_NOARGS(getHandlerBlock, CLASS, P, getHandlerBlock)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXTryStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXTryStmt;
# define CLASSNAME clang::CXXTryStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getTryLoc, CLASS, P, getTryLoc)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getTryBlock, CLASS, P, getTryBlock)
    case_METHOD_NOARGS(getNumHandlers, PRIM, P, getNumHandlers)
    case currefl::getHandler: {
      CHECK_TRAIT_IS_REFLPROP(getHandler)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getHandler, p0)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXForRangeStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXForRangeStmt;
# define CLASSNAME clang::CXXForRangeStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLoopVariable, CLASS, P, getLoopVariable)
    case_METHOD_NOARGS(getRangeInit, CLASS, P, getRangeInit)
    case_METHOD_NOARGS(getRangeStmt, CLASS, P, getRangeStmt)
    case_METHOD_NOARGS(getBeginStmt, CLASS, P, getBeginStmt)
    case_METHOD_NOARGS(getEndStmt, CLASS, P, getEndStmt)
    case_METHOD_NOARGS(getCond, CLASS, P, getCond)
    case_METHOD_NOARGS(getInc, CLASS, P, getInc)
    case_METHOD_NOARGS(getLoopVarStmt, CLASS, P, getLoopVarStmt)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(getForLoc, CLASS, P, getForLoc)
    case_METHOD_NOARGS(getCoawaitLoc, CLASS, P, getCoawaitLoc)
    case_METHOD_NOARGS(getColonLoc, CLASS, P, getColonLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXExpansionStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXExpansionStmt;
# define CLASSNAME clang::CXXExpansionStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getRangeVarStmt, CLASS, P, getRangeVarStmt)
    case_METHOD_NOARGS(getRangeVariable, CLASS, P, getRangeVariable)
    case_METHOD_NOARGS(getLoopVarStmt, CLASS, P, getLoopVarStmt)
    case_METHOD_NOARGS(getLoopVariable, CLASS, P, getLoopVariable)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(getSize, PRIM, P, getSize)
    case currefl::getInstantiatedStatements: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getInstantiatedStatements))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getInstantiatedStatements)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getForLoc, CLASS, P, getForLoc)
    case_METHOD_NOARGS(getEllipsisLoc, CLASS, P, getEllipsisLoc)
    case_METHOD_NOARGS(getColonLoc, CLASS, P, getColonLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXTupleExpansionStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXTupleExpansionStmt;
# define CLASSNAME clang::CXXTupleExpansionStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getRangeVarStmt, CLASS, P, getRangeVarStmt)
    case_METHOD_NOARGS(getRangeInit, CLASS, P, getRangeInit)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXPackExpansionStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXPackExpansionStmt;
# define CLASSNAME clang::CXXPackExpansionStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getUnexpandedPack, CLASS, P, getUnexpandedPack)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::MSDependentExistsStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__MSDependentExistsStmt;
# define CLASSNAME clang::MSDependentExistsStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKeywordLoc, CLASS, P, getKeywordLoc)
    case_METHOD_NOARGS(isIfExists, PRIM, P, isIfExists)
    case_METHOD_NOARGS(isIfNotExists, PRIM, P, isIfNotExists)
    case_METHOD_NOARGS(getQualifierLoc, CLASS, P, getQualifierLoc)
    case_METHOD_NOARGS(getNameInfo, CLASS, P, getNameInfo)
    case_METHOD_NOARGS(getSubStmt, CLASS, P, getSubStmt)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CoroutineBodyStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CoroutineBodyStmt;
# define CLASSNAME clang::CoroutineBodyStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasDependentPromiseType, PRIM, P, hasDependentPromiseType)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(getPromiseDeclStmt, CLASS, P, getPromiseDeclStmt)
    case_METHOD_NOARGS(getPromiseDecl, CLASS, P, getPromiseDecl)
    case_METHOD_NOARGS(getInitSuspendStmt, CLASS, P, getInitSuspendStmt)
    case_METHOD_NOARGS(getFinalSuspendStmt, CLASS, P, getFinalSuspendStmt)
    case_METHOD_NOARGS(getExceptionHandler, CLASS, P, getExceptionHandler)
    case_METHOD_NOARGS(getFallthroughHandler, CLASS, P, getFallthroughHandler)
    case_METHOD_NOARGS(getAllocate, CLASS, P, getAllocate)
    case_METHOD_NOARGS(getDeallocate, CLASS, P, getDeallocate)
    case_METHOD_NOARGS(getReturnValueInit, CLASS, P, getReturnValueInit)
    case_METHOD_NOARGS(getResultDecl, CLASS, P, getResultDecl)
    case_METHOD_NOARGS(getReturnStmt, CLASS, P, getReturnStmt)
    case_METHOD_NOARGS(getReturnStmtOnAllocFailure, CLASS, P, getReturnStmtOnAllocFailure)
    case currefl::getParamMoves: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::getParamMoves))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, getParamMoves)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CoreturnStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CoreturnStmt;
# define CLASSNAME clang::CoreturnStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKeywordLoc, CLASS, P, getKeywordLoc)
    case_METHOD_NOARGS(getOperand, CLASS, P, getOperand)
    case_METHOD_NOARGS(getPromiseCall, CLASS, P, getPromiseCall)
    case_METHOD_NOARGS(isImplicit, PRIM, P, isImplicit)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::CXXMetaparseStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__CXXMetaparseStmt;
# define CLASSNAME clang::CXXMetaparseStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getNewSrcCode, CLASS, P, getNewSrcCode)
    case_METHOD_NOARGS(getIntroLoc, CLASS, P, getIntroLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCForCollectionStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCForCollectionStmt;
# define CLASSNAME clang::ObjCForCollectionStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getElement, CLASS, P, getElement)
    case_METHOD_NOARGS(getCollection, CLASS, P, getCollection)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case_METHOD_NOARGS(getForLoc, CLASS, P, getForLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCAtCatchStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCAtCatchStmt;
# define CLASSNAME clang::ObjCAtCatchStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCatchBody, CLASS, P, getCatchBody)
    case_METHOD_NOARGS(getCatchParamDecl, CLASS, P, getCatchParamDecl)
    case_METHOD_NOARGS(getAtCatchLoc, CLASS, P, getAtCatchLoc)
    case_METHOD_NOARGS(getRParenLoc, CLASS, P, getRParenLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(hasEllipsis, PRIM, P, hasEllipsis)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCAtFinallyStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCAtFinallyStmt;
# define CLASSNAME clang::ObjCAtFinallyStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getFinallyBody, CLASS, P, getFinallyBody)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getAtFinallyLoc, CLASS, P, getAtFinallyLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCAtTryStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCAtTryStmt;
# define CLASSNAME clang::ObjCAtTryStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAtTryLoc, CLASS, P, getAtTryLoc)
    case_METHOD_NOARGS(getTryBody, CLASS, P, getTryBody)
    case_METHOD_NOARGS(getNumCatchStmts, PRIM, P, getNumCatchStmts)
    case currefl::getCatchStmt: {
      CHECK_TRAIT_IS_REFLPROP(getCatchStmt)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getCatchStmt, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getFinallyStmt, CLASS, P, getFinallyStmt)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCAtSynchronizedStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCAtSynchronizedStmt;
# define CLASSNAME clang::ObjCAtSynchronizedStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getAtSynchronizedLoc, CLASS, P, getAtSynchronizedLoc)
    case_METHOD_NOARGS(getSynchBody, CLASS, P, getSynchBody)
    case_METHOD_NOARGS(getSynchExpr, CLASS, P, getSynchExpr)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCAtThrowStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCAtThrowStmt;
# define CLASSNAME clang::ObjCAtThrowStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getThrowExpr, CLASS, P, getThrowExpr)
    case_METHOD_NOARGS(getThrowLoc, CLASS, P, getThrowLoc)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::ObjCAutoreleasePoolStmt *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__ObjCAutoreleasePoolStmt;
# define CLASSNAME clang::ObjCAutoreleasePoolStmt
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getSubStmt, CLASS, P, getSubStmt)
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getAtLoc, CLASS, P, getAtLoc)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPClause *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__OMPClause;
# define CLASSNAME clang::OMPClause
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getClauseKind, PRIM, P, getClauseKind)
    case_METHOD_NOARGS(isImplicit, PRIM, P, isImplicit)
    case currefl::children: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::children))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, children)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::OMPClause *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPExecutableDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPExecutableDirective;
# define CLASSNAME clang::OMPExecutableDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getLocStart, CLASS, P, getLocStart)
    case_METHOD_NOARGS(getBeginLoc, CLASS, P, getBeginLoc)
    case_METHOD_NOARGS(getLocEnd, CLASS, P, getLocEnd)
    case_METHOD_NOARGS(getEndLoc, CLASS, P, getEndLoc)
    case_METHOD_NOARGS(getNumClauses, PRIM, P, getNumClauses)
    case currefl::getClause: {
      CHECK_TRAIT_IS_REFLPROP(getClause)
      TRY_LOAD_VAL(unsigned int, p0)
      if (no_more) CLASS_REFLECTION(P, getClause, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(hasAssociatedStmt, PRIM, P, hasAssociatedStmt)
    case_METHOD_NOARGS(getAssociatedStmt, CLASS, P, getAssociatedStmt)
    case currefl::getCapturedStmt: {
      CHECK_TRAIT_IS_REFLPROP(getCapturedStmt)
      TRY_LOAD_VAL(enum clang::OpenMPDirectiveKind, p0)
      if (no_more) CLASS_REFLECTION(P, getCapturedStmt, p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(getInnermostCapturedStmt, CLASS, P, getInnermostCapturedStmt)
    case_METHOD_NOARGS(getDirectiveKind, PRIM, P, getDirectiveKind)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
    case currefl::clauses: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::clauses))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, clauses)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPParallelDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPParallelDirective;
# define CLASSNAME clang::OMPParallelDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasCancel, PRIM, P, hasCancel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPLoopDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPLoopDirective;
# define CLASSNAME clang::OMPLoopDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCollapsedNumber, PRIM, P, getCollapsedNumber)
    case_METHOD_NOARGS(getIterationVariable, CLASS, P, getIterationVariable)
    case_METHOD_NOARGS(getLastIteration, CLASS, P, getLastIteration)
    case_METHOD_NOARGS(getCalcLastIteration, CLASS, P, getCalcLastIteration)
    case_METHOD_NOARGS(getPreCond, CLASS, P, getPreCond)
    case_METHOD_NOARGS(getCond, CLASS, P, getCond)
    case_METHOD_NOARGS(getInit, CLASS, P, getInit)
    case_METHOD_NOARGS(getInc, CLASS, P, getInc)
    case_METHOD_NOARGS(getPreInits, CLASS, P, getPreInits)
    case_METHOD_NOARGS(getIsLastIterVariable, CLASS, P, getIsLastIterVariable)
    case_METHOD_NOARGS(getLowerBoundVariable, CLASS, P, getLowerBoundVariable)
    case_METHOD_NOARGS(getUpperBoundVariable, CLASS, P, getUpperBoundVariable)
    case_METHOD_NOARGS(getStrideVariable, CLASS, P, getStrideVariable)
    case_METHOD_NOARGS(getEnsureUpperBound, CLASS, P, getEnsureUpperBound)
    case_METHOD_NOARGS(getNextLowerBound, CLASS, P, getNextLowerBound)
    case_METHOD_NOARGS(getNextUpperBound, CLASS, P, getNextUpperBound)
    case_METHOD_NOARGS(getNumIterations, CLASS, P, getNumIterations)
    case_METHOD_NOARGS(getPrevLowerBoundVariable, CLASS, P, getPrevLowerBoundVariable)
    case_METHOD_NOARGS(getPrevUpperBoundVariable, CLASS, P, getPrevUpperBoundVariable)
    case_METHOD_NOARGS(getDistInc, CLASS, P, getDistInc)
    case_METHOD_NOARGS(getPrevEnsureUpperBound, CLASS, P, getPrevEnsureUpperBound)
    case_METHOD_NOARGS(getCombinedLowerBoundVariable, CLASS, P, getCombinedLowerBoundVariable)
    case_METHOD_NOARGS(getCombinedUpperBoundVariable, CLASS, P, getCombinedUpperBoundVariable)
    case_METHOD_NOARGS(getCombinedEnsureUpperBound, CLASS, P, getCombinedEnsureUpperBound)
    case_METHOD_NOARGS(getCombinedInit, CLASS, P, getCombinedInit)
    case_METHOD_NOARGS(getCombinedCond, CLASS, P, getCombinedCond)
    case_METHOD_NOARGS(getCombinedNextLowerBound, CLASS, P, getCombinedNextLowerBound)
    case_METHOD_NOARGS(getCombinedNextUpperBound, CLASS, P, getCombinedNextUpperBound)
    case_METHOD_NOARGS(getBody, CLASS, P, getBody)
    case currefl::counters: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::counters))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, counters)
      TOO_MANY;
    }
    case currefl::private_counters: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::private_counters))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, private_counters)
      TOO_MANY;
    }
    case currefl::inits: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::inits))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, inits)
      TOO_MANY;
    }
    case currefl::updates: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::updates))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, updates)
      TOO_MANY;
    }
    case currefl::finals: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << ("(iterator_range/ArrayRef-returning) Mem num "
                    + std::to_string(currefl::finals))
                  << "__reflect_prop";
          return ExprError();
      }
      if (no_more) RANGE_REFLECTION_0(P, finals)
      TOO_MANY;
    }
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPSimdDirective;
# define CLASSNAME clang::OMPSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPForDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPForDirective;
# define CLASSNAME clang::OMPForDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasCancel, PRIM, P, hasCancel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPForSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPForSimdDirective;
# define CLASSNAME clang::OMPForSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPSectionsDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPSectionsDirective;
# define CLASSNAME clang::OMPSectionsDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasCancel, PRIM, P, hasCancel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPSectionDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPSectionDirective;
# define CLASSNAME clang::OMPSectionDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasCancel, PRIM, P, hasCancel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPSingleDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPSingleDirective;
# define CLASSNAME clang::OMPSingleDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPMasterDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPMasterDirective;
# define CLASSNAME clang::OMPMasterDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPCriticalDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPCriticalDirective;
# define CLASSNAME clang::OMPCriticalDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getDirectiveName, CLASS, P, getDirectiveName)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPParallelForDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPParallelForDirective;
# define CLASSNAME clang::OMPParallelForDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasCancel, PRIM, P, hasCancel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPParallelForSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPParallelForSimdDirective;
# define CLASSNAME clang::OMPParallelForSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPParallelSectionsDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPParallelSectionsDirective;
# define CLASSNAME clang::OMPParallelSectionsDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasCancel, PRIM, P, hasCancel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTaskDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTaskDirective;
# define CLASSNAME clang::OMPTaskDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasCancel, PRIM, P, hasCancel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTaskyieldDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTaskyieldDirective;
# define CLASSNAME clang::OMPTaskyieldDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPBarrierDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPBarrierDirective;
# define CLASSNAME clang::OMPBarrierDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTaskwaitDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTaskwaitDirective;
# define CLASSNAME clang::OMPTaskwaitDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTaskgroupDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTaskgroupDirective;
# define CLASSNAME clang::OMPTaskgroupDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getReductionRef, CLASS, P, getReductionRef)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPFlushDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPFlushDirective;
# define CLASSNAME clang::OMPFlushDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPOrderedDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPOrderedDirective;
# define CLASSNAME clang::OMPOrderedDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPAtomicDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPAtomicDirective;
# define CLASSNAME clang::OMPAtomicDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getX, CLASS, P, getX)
    case_METHOD_NOARGS(getUpdateExpr, CLASS, P, getUpdateExpr)
    case_METHOD_NOARGS(isXLHSInRHSPart, PRIM, P, isXLHSInRHSPart)
    case_METHOD_NOARGS(isPostfixUpdate, PRIM, P, isPostfixUpdate)
    case_METHOD_NOARGS(getV, CLASS, P, getV)
    case_METHOD_NOARGS(getExpr, CLASS, P, getExpr)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetDirective;
# define CLASSNAME clang::OMPTargetDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetDataDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetDataDirective;
# define CLASSNAME clang::OMPTargetDataDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetEnterDataDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetEnterDataDirective;
# define CLASSNAME clang::OMPTargetEnterDataDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetExitDataDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetExitDataDirective;
# define CLASSNAME clang::OMPTargetExitDataDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetParallelDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetParallelDirective;
# define CLASSNAME clang::OMPTargetParallelDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetParallelForDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetParallelForDirective;
# define CLASSNAME clang::OMPTargetParallelForDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasCancel, PRIM, P, hasCancel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTeamsDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTeamsDirective;
# define CLASSNAME clang::OMPTeamsDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPCancellationPointDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPCancellationPointDirective;
# define CLASSNAME clang::OMPCancellationPointDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCancelRegion, PRIM, P, getCancelRegion)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPCancelDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPCancelDirective;
# define CLASSNAME clang::OMPCancelDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getCancelRegion, PRIM, P, getCancelRegion)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTaskLoopDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTaskLoopDirective;
# define CLASSNAME clang::OMPTaskLoopDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTaskLoopSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTaskLoopSimdDirective;
# define CLASSNAME clang::OMPTaskLoopSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPDistributeDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPDistributeDirective;
# define CLASSNAME clang::OMPDistributeDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetUpdateDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetUpdateDirective;
# define CLASSNAME clang::OMPTargetUpdateDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPDistributeParallelForDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPDistributeParallelForDirective;
# define CLASSNAME clang::OMPDistributeParallelForDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasCancel, PRIM, P, hasCancel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPDistributeParallelForSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPDistributeParallelForSimdDirective;
# define CLASSNAME clang::OMPDistributeParallelForSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPDistributeSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPDistributeSimdDirective;
# define CLASSNAME clang::OMPDistributeSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetParallelForSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetParallelForSimdDirective;
# define CLASSNAME clang::OMPTargetParallelForSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetSimdDirective;
# define CLASSNAME clang::OMPTargetSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTeamsDistributeDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTeamsDistributeDirective;
# define CLASSNAME clang::OMPTeamsDistributeDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTeamsDistributeSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTeamsDistributeSimdDirective;
# define CLASSNAME clang::OMPTeamsDistributeSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTeamsDistributeParallelForSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTeamsDistributeParallelForSimdDirective;
# define CLASSNAME clang::OMPTeamsDistributeParallelForSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTeamsDistributeParallelForDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTeamsDistributeParallelForDirective;
# define CLASSNAME clang::OMPTeamsDistributeParallelForDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasCancel, PRIM, P, hasCancel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetTeamsDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetTeamsDirective;
# define CLASSNAME clang::OMPTargetTeamsDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetTeamsDistributeDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetTeamsDistributeDirective;
# define CLASSNAME clang::OMPTargetTeamsDistributeDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetTeamsDistributeParallelForDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetTeamsDistributeParallelForDirective;
# define CLASSNAME clang::OMPTargetTeamsDistributeParallelForDirective
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(hasCancel, PRIM, P, hasCancel)
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetTeamsDistributeParallelForSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetTeamsDistributeParallelForSimdDirective;
# define CLASSNAME clang::OMPTargetTeamsDistributeParallelForSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::OMPTargetTeamsDistributeSimdDirective *X) {
  if (TraitKind == RTK_cast) {
    TRY_LOAD_VAL(bool, dyn)
    return ReflectCast( (ReflectionObjKind)N, X, dyn );
  }
  namespace currefl = refl::clang__OMPTargetTeamsDistributeSimdDirective;
# define CLASSNAME clang::OMPTargetTeamsDistributeSimdDirective
  switch( (currefl::memnames)N ) {
    case currefl::classof: {
      CHECK_TRAIT_IS_REFLPROP(classof)
      TRY_LOAD_VAL(const class clang::Stmt *, p0)
      if (no_more) PRIM_REFLECTION(S, classof, p0)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::StoredDeclsMap *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__StoredDeclsMap;
# define CLASSNAME clang::StoredDeclsMap
  switch( (currefl::memnames)N ) {
    case currefl::DestroyAll: {
      CHECK_TRAIT_IS_REFLPROP(DestroyAll)
      TRY_LOAD_VAL(class clang::StoredDeclsMap *, p0)
      TRY_LOAD_VAL(bool, p1)
      if (no_more) VOID_REFLECTION(S, DestroyAll, (class clang::StoredDeclsMap *)p0, (bool)p1)
      TOO_MANY;
    }
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , clang::DependentDiagnostic *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::clang__DependentDiagnostic;
# define CLASSNAME clang::DependentDiagnostic
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(getKind, PRIM, P, getKind)
    case_METHOD_NOARGS(isAccessToMember, PRIM, P, isAccessToMember)
    case_METHOD_NOARGS(getAccess, PRIM, P, getAccess)
    case_METHOD_NOARGS(getAccessLoc, CLASS, P, getAccessLoc)
    case_METHOD_NOARGS(getAccessTarget, CLASS, P, getAccessTarget)
    case_METHOD_NOARGS(getAccessNamingClass, CLASS, P, getAccessNamingClass)
    case_METHOD_NOARGS(getAccessBaseObjectType, CLASS, P, getAccessBaseObjectType)
    case_METHOD_NOARGS(getDiagnostic, CLASS, P, getDiagnostic)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , reflcontainers::VectorDecl *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::reflcontainers__VectorDecl;
# define CLASSNAME reflcontainers::VectorDecl
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(P)
      TOO_MANY;
};
    case_METHOD_NOARGS(size, PRIM, P, size)
    case_METHOD_NOARGS(empty, PRIM, P, empty)
    case currefl::push_back: {
      CHECK_TRAIT_IS_REFLPROP(push_back)
      TRY_LOAD_VAL(const class clang::Decl *, p0)
      if (no_more) VOID_REFLECTION(P, push_back, (const clang::Decl *)p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(pop_back, VOID, P, pop_back)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , reflcontainers::VectorStr *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::reflcontainers__VectorStr;
# define CLASSNAME reflcontainers::VectorStr
  switch( (currefl::memnames)N ) {
    case currefl::_this_: {
      std::size_t N;
      switch (TraitKind) {
        case RTK_range_nth: {
          TRY_LOAD_VAL(std::size_t, M)
          N = M;
          break;
        }
        case RTK_range_size: break;
        default:
          S.Diag(KWLoc, diag::err_refl_not_supported) 
                 << "_this_"
                 << "__reflect_prop";
          return ExprError();
      }
      if (no_more) CLASS_RANGE_REFLECTION(P)
      TOO_MANY;
};
    case_METHOD_NOARGS(size, PRIM, P, size)
    case_METHOD_NOARGS(empty, PRIM, P, empty)
    case currefl::push_back: {
      CHECK_TRAIT_IS_REFLPROP(push_back)
      TRY_LOAD_VAL(const char *, p0)
      if (no_more) VOID_REFLECTION(P, push_back, (const char *)p0)
      TOO_MANY;
    }
    case_METHOD_NOARGS(pop_back, VOID, P, pop_back)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , reflcontainers::SetDecl *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::reflcontainers__SetDecl;
# define CLASSNAME reflcontainers::SetDecl
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(dummyfcn, PRIM, P, dummyfcn)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , reflcontainers::SetStr *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::reflcontainers__SetStr;
# define CLASSNAME reflcontainers::SetStr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(dummyfcn, PRIM, P, dummyfcn)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , reflcontainers::MapDeclStr *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::reflcontainers__MapDeclStr;
# define CLASSNAME reflcontainers::MapDeclStr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(dummyfcn, PRIM, P, dummyfcn)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
ExprResult Reflector::ReflectProp(unsigned N , reflcontainers::MapStrStr *X) {
  if (TraitKind == RTK_cast)
    S.Diag(KWLoc, diag::err_reflcast_nonpointer);
  namespace currefl = refl::reflcontainers__MapStrStr;
# define CLASSNAME reflcontainers::MapStrStr
  switch( (currefl::memnames)N ) {
    case_METHOD_NOARGS(dummyfcn, PRIM, P, dummyfcn)
  }
  reflprop_unreachable();
# undef CLASSNAME
}
