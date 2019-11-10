//DWR ADDN:
//===--- APValueCharIter.h - iterator that advances over APValues, dereferences to char -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the APValueCharIter class, which is a wrapper for an APValue
//  array of APSInts whose dereference operator casts to result to char.  This is
//  used to lex compile-time concatenated char arrays, for meta-programming.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LEX_APVALUECHARITER_H
#define LLVM_CLANG_LEX_APVALUECHARITER_H

#include "clang/AST/APValue.h"

namespace clang {

/// A helper const char* iterator over APValue int arrays (for lexing compile-time concatenated string literals
/// expressed as char arrays).  Used in ApplyExpand below.
class APValueCharIter {
  const APValue *_arr;
  unsigned int _cur;
public:
  APValueCharIter(const APValue *arr, unsigned int cur = 0) : _arr(arr), _cur(cur) {
    assert(_arr->isArray() && "arr must be an array!");
  }
  APValueCharIter() {}

  //Subscript operator: the only non-generic operator implem:
  char operator[](int idx) const {
//    assert(_arr->getArrayInitializedElt(_cur + idx).isInt() && "APValue array contains non-int values!");
    return _arr->getArrayInitializedElt(_cur + idx).getInt().getZExtValue();
  }

  //Now all the generic operator implems:
  char operator*() const {
    return operator[](0);
  }

  bool operator==(const APValueCharIter &rhs) const {
    assert(_arr == rhs._arr && "Comparing APValueCharIters with different _arrs!  (Disallowed for better efficiency)");
    return _cur == rhs._cur;
  }

  bool operator!=(const APValueCharIter &rhs) const {
    return !operator==(rhs);
  }

  bool operator<(const APValueCharIter &rhs) const {
    assert(_arr == rhs._arr && "Comparing APValueCharIters with different _arrs!");
    return  _cur < rhs._cur;
  }

  bool operator<=(const APValueCharIter &rhs) const {
    return operator<(rhs) || operator==(rhs);
  }

  bool operator>(const APValueCharIter &rhs) const {
    return !operator<=(rhs);
  }

  bool operator>=(const APValueCharIter &rhs) const {
    return !operator<(rhs);
  }

  //Non-const arithmetic operators:
  APValueCharIter &operator++() { //Prefix increment
    ++_cur;
    return *this;
  }

  APValueCharIter operator++(int) { //Postfix increment
    APValueCharIter tmp(*this); // copy
    operator++(); // pre-increment
    return tmp;   // return old value
  }

  APValueCharIter &operator--() { //Prefix decrement
    --_cur;
    return *this;
  }

  APValueCharIter operator--(int) { //Postfix decrement
    APValueCharIter tmp(*this); // copy
    operator--(); // pre-increment
    return tmp;   // return old value
  }

  APValueCharIter &operator+=(unsigned rhs) {
    _cur += rhs;
    return *this;
  }

  APValueCharIter &operator-=(unsigned rhs) {
    _cur -= rhs;
    return *this;
  }

  //Const arithmetic operators:
  APValueCharIter operator+(unsigned rhs) const {
    return APValueCharIter(_arr, _cur + rhs);
  }

  APValueCharIter operator-(unsigned rhs) const {
    return APValueCharIter(_arr, _cur - rhs);
  }

  uint64_t operator-(APValueCharIter rhs) const {
    assert(_arr == rhs._arr && "Subtracting APValueCharIters with different array data members!");
    assert(rhs._cur > _cur && "Subtracting APValueCharIter whose _cur is greater than the lhs._cur!");
    return _cur - rhs._cur;
  }

};

} //namespace clang

#endif //LLVM_CLANG_LEX_APVALUECHARITER_H
//END DWR ADDN
