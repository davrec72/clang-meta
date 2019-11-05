//
//  inhwrp.hpp
//  clang_meta_examples
//
//  Created by David Rector on 10/18/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_meta_util_inhwrp_hpp
#define dwrmeta_meta_util_inhwrp_hpp

#include "./reflect_arg.hpp"
#include "./macros.hpp"

using AccessSpecifier = cppx::meta::clang::AccessSpecifier;

template<typename T, AccessSpecifier ACC, bool VIR>
struct basespec;

/// inhwrp: a tuple of basespec<...> types that inherits
/// from each specified base with the specified access/virtual-ness.
template<typename... BASESPECs>
struct inhwrp;

//SPEC: multiple bases, initial is public nonvirtual
template<typename T, typename T2, typename... REMs>
struct inhwrp< basespec<T, AccessSpecifier::AS_public, false>, T2, REMs... >
: public T, public inhwrp<T2, REMs...>
{};
//SPEC: multiple bases, private nonvirtual
template<typename T, typename T2, typename... REMs>
struct inhwrp< basespec<T, AccessSpecifier::AS_private, false>, T2, REMs... >
: private T, public inhwrp<T2, REMs...>
{};
//SPEC: multiple bases, protected nonvirtual
template<typename T, typename T2, typename... REMs>
struct inhwrp< basespec<T, AccessSpecifier::AS_protected, false>, T2, REMs... >
: protected T, public inhwrp<T2, REMs...>
{};
//SPEC: multiple bases, public virtual
template<typename T, typename T2, typename... REMs>
struct inhwrp< basespec<T, AccessSpecifier::AS_public, true>, T2, REMs... >
: public virtual T, public inhwrp<T2, REMs...>
{};
//SPEC: multiple bases, private virtual
template<typename T, typename T2, typename... REMs>
struct inhwrp< basespec<T, AccessSpecifier::AS_private, true>, T2, REMs... >
: private virtual T, public inhwrp<T2, REMs...>
{};
//SPEC: multiple bases, protected virtual
template<typename T, typename T2, typename... REMs>
struct inhwrp< basespec<T, AccessSpecifier::AS_protected, true>, T2, REMs... >
: protected virtual T, public inhwrp<T2, REMs...>
{};


//SPEC: one base, public nonvirtual
template<typename T>
struct inhwrp< basespec<T, AccessSpecifier::AS_public, false>>
: public T
{};
//SPEC: one base, private nonvirtual
template<typename T>
struct inhwrp< basespec<T, AccessSpecifier::AS_private, false>>
: private T
{};
//SPEC: one base, protected nonvirtual
template<typename T>
struct inhwrp< basespec<T, AccessSpecifier::AS_protected, false>>
: protected T
{};
//SPEC: one base, public virtual
template<typename T>
struct inhwrp< basespec<T, AccessSpecifier::AS_public, true>>
: public virtual T
{};
//SPEC: one base, private virtual
template<typename T>
struct inhwrp< basespec<T, AccessSpecifier::AS_private, true>>
: private virtual T
{};
//SPEC: one base, protected virtual
template<typename T>
struct inhwrp< basespec<T, AccessSpecifier::AS_protected, true>>
: protected virtual T
{};


//SPEC: no bases
template<>
struct inhwrp<>
{};



/// get_inhwrp_bases: a type trait to extract the bases of T
/// into a 'using type = inhwrp<...>;' alias.
template<typename T>
struct get_inhwrp_bases {
#ifdef NMETA
    using type = T; //dummy to avoid erroneous IDE flag
#endif

    DO_META {
        using namespace cppx::meta;
        using namespace cppx::meta::clang;

        auto_ Trefl = get_tmplarg_as_RD(reflexpr(T));
        const char *dfltaccessstr = Trefl->isClass()
            ? "AccessSpecifier::AS_private"
            : "AccessSpecifier::AS_public";

        
        
        QPARSE("using type = inhwrp<");
        bool first = true;
        FOR ((CXXBaseSpecifier) B : Trefl->bases()) {
            if (first) {
                first = false;
                QPARSE("basespec<");
            } else
                QPARSE(", basespec<");

            auto_ BaseClass = B.getType()->getAsCXXRecordDecl();
            ce_assert(BaseClass);
            QPARSE(BaseClass->getQualifiedNameAsString(), ",");

            switch (B.getAccessSpecifier()) {
                case AccessSpecifier::AS_public:    QPARSE("AccessSpecifier::AS_public"); break;
                case AccessSpecifier::AS_protected: QPARSE("AccessSpecifier::AS_protected"); break;
                case AccessSpecifier::AS_private:   QPARSE("AccessSpecifier::AS_private"); break;
                case AccessSpecifier::AS_none:      QPARSE(dfltaccessstr); break;
            }
            QPARSE(B.isVirtual() ? ", 1>" : ", 0>"); //DWR TODO get this working
        }
        QPARSE(">;");
    }
};

template<typename T>
using get_inhwrp_bases_t = typename get_inhwrp_bases<T>::type;






#ifndef NMETA
namespace test {
    namespace get_inhwrp_bases_test {
        struct A { void afunc() {} };
        struct B { void bfunc() {} };
        struct C : A, B {};
        class D : A, public B {};
        struct E {};
        struct F : protected virtual D, public E {};

        static_assert(std::is_same< get_inhwrp_bases_t<C>
                      , inhwrp< basespec<A, AccessSpecifier::AS_public, 0>
                      , basespec<B, AccessSpecifier::AS_public, 0>
                      >
                      >::value);

        static_assert(std::is_same< get_inhwrp_bases_t<D>
                      , inhwrp< basespec<A, AccessSpecifier::AS_private, 0>
                      , basespec<B, AccessSpecifier::AS_public, 0>
                      >
                      >::value);

        static_assert(std::is_same< get_inhwrp_bases_t<F>
                      , inhwrp< basespec<D, AccessSpecifier::AS_protected, 1>
                      , basespec<E, AccessSpecifier::AS_public, 0>
                      >
                      >::value);

        struct Cclone : get_inhwrp_bases_t<C> {};

        void dummyfunc() {
            Cclone cc;
            cc.afunc();
            cc.bfunc();
        }
    }
}
#endif //NMETA

#endif /* dwrmeta_meta_util_inhwrp_hpp */
