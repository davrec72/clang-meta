//
//  set.hpp
//  clang_meta_examples
//
//  Created by David Rector on 11/3/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_ce_set_hpp
#define dwrmeta_ce_set_hpp

#include <iterator>
#include "../metacore.hpp"
#include "metacast.hpp"
#include "as_const.hpp"

namespace ce {
    
    template<typename ELEM>
    class set {
        using intelem_t = intptr_t;
        static constexpr bool StoreAsInt = std::is_integral<ELEM>::value
                                           || ( std::is_enum<ELEM>::value &&
                                                std::is_convertible<ELEM, intelem_t>::value );
        
        static constexpr const char *ContainerUTypeStr = StoreAsInt ? "SetInt" : "SetStr";
            //^ if these change, be sure to change the ctors below
        using u_elem_t = typename std::conditional<StoreAsInt, intelem_t, const char *>::type;
        
        static constexpr bool ElemMetaparsingNeeded = std::is_same<u_elem_t, const char *>::value &&
                                                     !std::is_same<ELEM, const char *>::value;
        
        const char *container_refstr;
        
        constexpr intptr_t getclonep() const {
            return __metaparse_expr(__concatenate(container_refstr, ".clone()"), intptr_t);
        }
    public:
        constexpr set()
        : container_refstr(__metaparse_expr(__concatenate("(const char *)*__reflect_new("
                                                          "cppx::meta::refldetail::GetReflectionObjKind<cppx::meta::refldetail::reflcontainers::",
                                                          ContainerUTypeStr, ">::value)"),
                                            const char *))
        {}
        constexpr set(const set &other)
        : container_refstr(__concatenate("cppx::meta::refldetail::reflcontainers::",
                                         ContainerUTypeStr, "::impl<1, ", //IsPtr = 1
                                         other.getclonep(), ">()") )
        {}
        constexpr set(set &&other)
        : container_refstr(std::move(other.container_refstr))
        {}
        constexpr operator const char *() {
            using namespace cppx::meta;
            using namespace cppx::meta::clang;
            auto_ ELEMreflQT = cast<SubstTemplateTypeParmType>(reflexpr(ELEM))->getReplacementType();
            return __concatenate("ce::set<", ELEMreflQT.getAsString(), ">(", container_refstr, ")");
        }
        constexpr operator const char *() const {
            return __concatenate("ce::as_const(", const_cast<set *>(this)->operator const char *(), ")");
        }
        template<intptr_t X,
                 bool StoreAsInt_ = StoreAsInt,
                 typename = std::enable_if_t<StoreAsInt_> >
        constexpr set(cppx::meta::refldetail::reflcontainers::SetInt::impl IFMETA_ELSE((<1, X>), ()) reflcontainer)
        : container_refstr((const char *)reflcontainer)
        {}
        template<intptr_t X,
                 bool StoreAsInt_ = StoreAsInt,
                 typename = std::enable_if_t</**/!StoreAsInt_> >
        constexpr set(cppx::meta::refldetail::reflcontainers::SetStr::impl IFMETA_ELSE((<1, X>), ()) reflcontainer)
        : container_refstr((const char *)reflcontainer)
        {}
        
        constexpr std::size_t size() const {
            return IFMETA_ELSE( (__metaparse_expr(__concatenate(container_refstr, ".size()"), std::size_t)) , (1) );
        }
        constexpr bool empty() const {
            return size()==0;
        }
        constexpr void clear() {
            __metaparse_expr(__concatenate(container_refstr, ".clear()"), void);
        }
        constexpr void dealloc() {
            // TODO: if ELEMs have dealloc methods, loop through calling dealloc on each of them first.
            // (So, you need some a type trait/metafunction to test for a dealloc() method,
            // an if constexpr that tests that, then a normal for loop if true.)
            clear();
        }
        
        // DISTINCTIVE SET METHODS:
        
        constexpr bool contains(ELEM elem) const {
            return __metaparse_expr(__concatenate(container_refstr, ".contains(", metacast<u_elem_t>(elem), ")"), bool);
        }
        /// \returns whether elem was inserted (i.e. false if this elem was already in the set)
        constexpr bool insert(ELEM elem) {
            return __metaparse_expr(__concatenate(container_refstr, ".insert(", metacast<u_elem_t>(elem), ")"), bool);
        }
        constexpr void erase(ELEM elem) {
            __metaparse_expr(__concatenate(container_refstr, ".erase(", metacast<u_elem_t>(elem), ")"), void);
        }
        
    private:
        friend class iterator;
        constexpr u_elem_t at_ith(std::size_t I) const {
            nce_assert(I < size() && "out of range");
            return __metaparse_expr(__concatenate(container_refstr, ".at_ith(", I, ")"), u_elem_t);
        }
        /// Note this one should definitely NOT be made into a random-access iterator.
        class iterator : public std::iterator<
                         std::input_iterator_tag, // iterator_category
                         ELEM,                    // value_type
                         ptrdiff_t,               // difference_type
                         const ELEM*,             // pointer (UNUSED)
                         ELEM>                    // reference: just create a copy; reflection does that anyway:
        {
            const set &data;
            std::size_t I;
        public:
            constexpr explicit iterator(const set & data, std::size_t I = 0) : data(data), I(I) {}
            constexpr iterator& operator++()                { ++I; return *this; }
            constexpr iterator operator++(int)              { iterator retval = *this; ++(*this); return retval; }
            constexpr bool operator==(iterator other) const { return I == other.I; } //NB ignore comparing data
            constexpr bool operator!=(iterator other) const { return !(*this == other); }
            constexpr ELEM operator*() const                { return data.at_ith(I); }
        };
    public:
        constexpr iterator  begin() const { return iterator(*this, 0); }
        constexpr iterator  end()   const { return iterator(*this, size()); }
    };
    
} //namespace ce

#endif /* dwrmeta_ce_set_hpp */
