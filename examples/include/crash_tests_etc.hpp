//
//  crash_tests_etc.hpp
//  newclang_test2
//
//  Created by David Rector on 11/4/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_crash_tests_etc_hpp
#define dwrmeta_crash_tests_etc_hpp

//
//namespace misc_tests {
//
//  constexpr void dummyfunc() {}
//
//  // TESTS OF __metaparse_expr/QPARSE diagnostics and weird cases:
//  DO_META {
//    __metaparse_expr("", void); //Works, does nothing
//    __metaparse_expr("misc_tests::dummyfunc()", void);
////    __metaparse_expr("misc_tests::misspelling()", void); //ERROR
////    __metaparse_expr("wtf(i t)", void); //ERROR
////    __metaparse_expr("misc_tests::dummyfunc())", void); //ERROR: extraneous chars
////    __metaparse_expr("misc_tests::dummyfunc()(", void); //ERROR: unterminated metaparse (will probably cause weird subsequent errors, but no crash)
////    __metaparse_expr("misc_tests::dummyfunc()([((<)(>]<([", void); //ERROR another unterminated metaparse, still no crash
//
////    __metaparse_expr("\"", void); //ERROR
//
//    QPARSE("");
//    QPARSE("static int i = 3;");
////    QPARSE("void f())"); //ERROR
////    QPARSE("void f("); //ERROR but no crash
////    QPARSE("template<int I>>"); //ERROR but no crash
////    QPARSE("template<int i, template<typename, "); //ERROR but no crash
////    QPARSE("template<int I>"); //ERROR but no crash
////    QPARSE("\"");
//
////    QPARSE("/*"); //ERROR
////    QPARSE("//");
//  }
//
//
//  // TESTS OF MACROS (most of these crashed so I took most macro functionality out):
//
//  static int x = 8;
//#define MYMACROA ((2+4) * x)
//#define MYMACRO_AA(a,b) a-b
//#define MYMACRO_AAA MYMACRO_AA(5,MYMACROA) + 6
//  DO_META {
////      QPARSE("static_assert(MYMACROA==2);"); //ERROR
////      int asdf = __metaparse_expr("MYMACROA", int); //ERROR
////      QPARSE("static_assert(MYMACRO_AAA==9);"); //ERROR
////      QPARSE("static_assert(MYMACRO_AA(5,3)==2;"); //ERROR
////      QPARSE("#define MYMACRO_BB(a,b) a+b"); //ERROR
////      QPARSE("#define MYMACRO_B 3"); //ERROR
////      QPARSE("int asdf2 = MYMACRO_B;"); //ERROR
////      QPARSE("#define MYMACRO_C  MYMACROA + MYMACRO_B"); //ERROR
////      QPARSE("int asdf3 = MYMACRO_C;"); //ERROR
//    QPARSE("#undef MY_MACRO_AA");
//  }
//
//
//
////  //Doesn't compile, but here's where metaparse_expr could help:
////  template<typename T>
////  struct DeclNT {
////    const char *str;
////    constexpr DeclNT(intptr_t i) : str(__concatenate(i)) {}
////    constexpr auto mymethod() const {
//////      return __metaparse_expr(__concatenate(str, "+2"), int);
//////      constexpr int XX = __metaparse_expr(str, int);
//////      return XX;
////      return __reflect_prop(reflenums::RK_clang__Decl, reflenums::clang__Decl::getLocStart, /*IsPtr*/1, XX);
////    }
////  };
////
////  DO_META {
////    constexpr const char *str32 = "32";
////    constexpr DeclNT<int> d(32);
////    constexpr auto i = d.mymethod();
////  }
//
//constexpr void myfcn(const char *arg) {
//  QPARSE(arg);
//
//
//}
//
///*
// TODO:
// --
// */
//
////struct ASDF {
////  constexpr const char *f() const {
////    return "qwer";
////  }
////  constexpr int g() const {
////    return 23;
////  }
////};
////
////
//////Good demos of __metaparse_expr:
////constexpr void myfcn2() {
////  constexpr const char *arg2 = "34";
////  constexpr int j = __metaparse_expr(arg2, int);
////  ce_assert(j == 34);
////  constexpr int k = __metaparse_expr("32+32", int);
////  ce_assert(k == 64);
////  constexpr int l = __metaparse_expr("ASDF().g()", int);
////  ce_assert(l == 23);
////  ce_assert(ce_streq(__metaparse_expr("\"qwer\"", const char *), "qwer"));
////  constexpr const char *mystrA = __metaparse_expr("\"qwer\"", const char *);
////  constexpr const char *mystrB = __metaparse_expr("ASDF().f()", const char *);
////
////  constexpr int m = __metaparse_expr(__metaparse_expr("\"54\"", const char *), int);
////  ce_assert(m == 54);
////
////  ce_assert(ce_streq(mystrA, "qwer"));
////
////  ce_assert(ce_streq(mystrB, "qwer"));
////}
////DO_META {
////  myfcn("class BBB {};");
////  constexpr int j = __metaparse_expr(__concatenate("3", "4"), int);
////  ce_assert(j == 34);
////  myfcn2();
////}
////
////
////struct MyDummyA {
////  constexpr operator const char *() const {
////    return "MyDummyA()";
////  }
////  constexpr int g() const {
////    return 22;
////  }
////};
////
////template<typename T>
////constexpr int f(T t) {
////  return __metaparse_expr(__concatenate((const char *)t, ".g()"), int);
////}
////
////DO_META {
////  constexpr MyDummyA d;
////  ce_assert(f(d)==22);
////}
////
//////#ifndef NMETA
//////using asdf = BBB;
//////#endif
////
////
//////template<intptr_t... Xs>
//////struct concat_w_commas;
//////
//////template<intptr_t X, intptr_t... Xs>
//////struct concat_w_commas<X, Xs...> {
//////  static constexpr const char *value = __concatenate(X, ",", concat_w_commas<Xs...>::value);
//////};
//////
//////template<intptr_t X>
//////struct concat_w_commas<X> {
//////  static constexpr const char *value = __concatenate(X); //need __concatenate to turn the int into a string
//////};
//////
//////DO_META {
//////  ce_debug_var((concat_w_commas<1, 104352, 234>::value));
//////}
////
////
////struct AAA {};
////
////
////
//////  //PUT THIS IN AN EXAMPLE:
//////  DO_META {
//////    constexpr const char* AAAreflctorstr = (const char *)reflexpr(AAA);
//////    ce_debug_var(AAAreflctorstr);
//////    QPARSE("constexpr const char *AAAname = ", AAAreflctorstr, "->getName();");
//////    DEFERRED_META {
//////      ce_debug_var(AAAname);
//////    };
//////  }
////
////
//////  DO_META {
//////    constexpr auto AAArefl = cast<CXXRecordDecl>(reflexpr(AAA));
//////  }
////
////
//////// DOES NOT WORK:
//////  static constexpr const char MyCode[] = "int i;";
//////
//////  template<const char *Code>
//////  struct MyMetaClsA {
//////    DO_META { QPARSE(Code); }
//////  };
//////
//////#ifndef NMETA
//////  void mydummyfunc1() {
//////    MyMetaClsA<MyCode> m;
//////    m.i;
//////  }
//////#endif
////////END
////
////
////
//////  struct MyClass {
//////    static constexpr const char *value = "int i;";
//////  };
//////
//////  template<typename T>
//////  struct MyMetaClsB {
//////    DO_META { QPARSE(T::value); }
//////  };
//////
//////#ifndef NMETA
//////  void mydummyfunc2() {
//////    MyMetaClsB<MyClass> m;
//////    [[maybe_unused]] auto val = m.i;
////////    m.j; //ERROR
//////  }
//////#endif
////
////
////
//////  // DWR this works now
//////  template<intptr_t... Xs>
//////  constexpr void concatparampackexpansion() {
//////    ce_debug(__concatenate("The Xs, no commas between: ", Xs...));
//////  }
//////  DO_META {
//////    concatparampackexpansion<1,5,60234>();
//////  }
//////  //END
//
//} //namespace misc_tests
//
////static constexpr int myintarray[5] = {0, 1, 2, 3, 4};
////
////constexpr void myfunc() {
////  for (unsigned i = 0; i < 5; ++i) {
////    constexpr int j = __metaparse_expr(__concatenate("myintarray[", i, "]"), int);
////  }
////}

#endif /* dwrmeta_crash_tests_etc_hpp */
