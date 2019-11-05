//
//  vector.hpp
//  clang_meta_examples
//
//  Created by David Rector on 11/3/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_ce_vector_hpp
#define dwrmeta_ce_vector_hpp

#include <iterator>
#include "../metacore.hpp"
#include "metacast.hpp"
#include "as_const.hpp"

namespace ce {
    
    template<typename ELEM>
    class vector {
        using intelem_t = intptr_t; //this should match VectorInt's elem type, TODO fetch from there directly
        static constexpr bool StoreAsInt = std::is_integral<ELEM>::value
                                           || ( std::is_enum<ELEM>::value &&
                                                std::is_convertible<ELEM, intelem_t>::value );
        
        static constexpr const char *ContainerUTypeStr = StoreAsInt ? "VectorInt" : "VectorStr";
            //^ if these change, be sure to change the ctors below
        using u_elem_t = typename std::conditional<StoreAsInt, intelem_t, const char *>::type;
        
        static constexpr bool ElemMetaparsingNeeded = std::is_same<u_elem_t, const char *>::value &&
                                                     !std::is_same<ELEM, const char *>::value;
        
        /// The only data member (though maybe you can add a few cache values -- size for example.
        /// TODO try adding cache data members, test compile time speed effect.
        const char *container_refstr;
        
        constexpr intptr_t getclonep() const {
            return __metaparse_expr(__concatenate(container_refstr, ".clone()"), intptr_t);
        }
    public:
        
        // CONSTRUCTORS/CONVERSIONS
        
        /// Default constructor
        // TODO figure out how to use STRINGIZE properly here to expand the reflnew(...) macro here;
        // had to manually expand it here to get the string right, looks ugly:
        constexpr vector()
        : container_refstr(__metaparse_expr(__concatenate("(const char *)*__reflect_new("
                                                          "cppx::meta::refldetail::GetReflectionObjKind<cppx::meta::refldetail::reflcontainers::",
                                                          ContainerUTypeStr, ">::value)"),
                                            const char *))
        {}  //^ Note that we are metaparsing the above expression into another const char *.  I.e. the
            //^ container_refstr will be the result of const char *-converting a reflect_new'd object,
            //^ NOT the above string itself.
            // Contrast with below -- uses the strring direclty, no metaparsing...
        
        /// Copy constructor
        constexpr vector(const vector &other)
        : container_refstr(__concatenate("cppx::meta::refldetail::reflcontainers::",
                                         ContainerUTypeStr, "::impl<1, ", //IsPtr = 1
                                         other.getclonep(), ">()") )
        {}  //^ The actual copying of the data occurs inside clang during other.getclonep(),
            //^ Then we build a string that would construct the fully typed container
            //^ (really, just a cheap wrapper around a reflected container that clang manages).
        
        /// Move ctor (avoids cloning):
        constexpr vector(vector &&other)
        : container_refstr(std::move(other.container_refstr))
        {}
        
        /// The const char * conversion operator: allows this object to be an element type of
        /// another ce container; e.g. you can make a ce::vector<ce::vector<int>> because of this
        /// AND the ctors that follow.  First the non-const version:
        constexpr operator const char *() {
            using namespace cppx::meta;
            using namespace cppx::meta::clang;
            // First a little reflection to get the ELEM name:
            auto_ ELEMreflQT = cast<SubstTemplateTypeParmType>(reflexpr(ELEM))->getReplacementType();
            // Then return a string that, if __metaparse_expr'd, would reconstruct this object
            // (which is just a cheap wrapper, basically an elaborate pointer):
            return __concatenate("ce::vector<", ELEMreflQT.getAsString(), ">(", container_refstr, ")");
        }
        /// Now the const version; this should be the standard procedure for
        /// implementing const versions of this operator (we should really
        /// define a metaclass to handle stuff like this, TODO...):
        constexpr operator const char *() const {
            return __concatenate("ce::as_const(", const_cast<vector *>(this)->operator const char *(), ")");
        }
        /// Constructor from a reflected VectorInt (needed to make the const char * conversion
        /// operator work; you should probably not need this/use this otherwise):
        template<intptr_t X,
                 bool StoreAsInt_ = StoreAsInt,
                 typename = std::enable_if_t<StoreAsInt_> >
        constexpr vector(cppx::meta::refldetail::reflcontainers::VectorInt::impl IFMETA_ELSE((<1, X>), ()) reflcontainer)
        : container_refstr((const char *)reflcontainer)
        {}
        /// Constructor from a reflected VectorStr (needed to make the const char * conversion
        /// operator work; you should probably not need this/use this otherwise):
        template<intptr_t X,
                 bool StoreAsInt_ = StoreAsInt,
                 typename = std::enable_if_t</**/!StoreAsInt_> >
        constexpr vector(cppx::meta::refldetail::reflcontainers::VectorStr::impl IFMETA_ELSE((<1, X>), ()) reflcontainer)
        : container_refstr((const char *)reflcontainer)
        {}
        
        // SIZE/CAPACITY METHODS:
        
        constexpr std::size_t size() const {
            return IFMETA_ELSE( (__metaparse_expr(__concatenate(container_refstr, ".size()"), std::size_t)) , (1) );
        }
        constexpr bool empty() const {
            return size()==0;
        }
        constexpr void clear() {
            __metaparse_expr(__concatenate(container_refstr, ".clear()"), void);
        }
        constexpr void resize(std::size_t N) {
            __metaparse_expr(__concatenate(container_refstr, ".resize(", N, ")"), void);
        }
        
        constexpr std::size_t capacity() const {
            return IFMETA_ELSE( (__metaparse_expr(__concatenate(container_refstr, ".capacity()"), std::size_t)) , (1) );
        }
        constexpr void reserve(std::size_t N) {
            __metaparse_expr(__concatenate(container_refstr, ".reserve(", N, ")"), void);
        }
        constexpr void shrink_to_fit() {
            __metaparse_expr(__concatenate(container_refstr, ".shrink_to_fit()"), void);
        }
        
        /// You should call this when you're done with a container.
        /// Clang will handle the ultimate destruction of all containers
        /// at the end of the build, but this frees up space mid-build.
        /// (Remember there is no run-time allocation with ce containers;
        /// this only affects for compile-time performance.)
        constexpr void dealloc() {
            // TODO: if ELEMs have dealloc methods, loop through calling dealloc on each of them first.
            // (So, you need some a type trait/metafunction to test for a dealloc() method,
            // an if constexpr that tests that, then a normal for loop if true.)
            resize(0);
            shrink_to_fit();
        }
        
        
        // ELEMENT RETRIEVAL/MODIFICATION METHODS:
        
        constexpr void push_back(ELEM elem) {
            __metaparse_expr(__concatenate(container_refstr, ".push_back(", metacast<u_elem_t>(elem), ")"), void);
        }
        constexpr void emplace_back(ELEM &&elem) {
            __metaparse_expr(__concatenate(container_refstr, ".push_back(", metacast<u_elem_t>(std::move(elem)), ")"), void);
        }
        constexpr void pop_back() {
            __metaparse_expr(__concatenate(container_refstr, ".pop_back()"), void);
        }
        
        constexpr void assign(std::size_t I, ELEM elem) {
            nce_assert(I < size() && "out of range");
            __metaparse_expr(__concatenate(container_refstr, ".assign(", I, ", ", metacast<u_elem_t>(elem), ")"), void);
        }
    private:
        constexpr u_elem_t at_hlpr(std::size_t I) const {
            nce_assert(I < size() && "out of range");
            return __metaparse_expr(__concatenate(container_refstr, ".at(", I, ")"), u_elem_t);
        }
    public:
        /// Returns a value, not a reference.
        constexpr ELEM at(std::size_t I) const {
            if constexpr (!ElemMetaparsingNeeded)
                return static_cast<ELEM>(at_hlpr(I));
            else
                return IFMETA_ELSE( (__metaparse_expr(at_hlpr(I), ELEM)) , (ELEM()) );
        }
        constexpr ELEM front() const {
            return at(0);
        }
        constexpr ELEM back() const {
            return at(size()-1);
        }
        
        // TODO for int containers, maybe have an operator[] that returns a non-const reference.
        // Won't work for non-integral ELEM types though.
        
    private:
        /// Not a great iterator, but works for now.
        /// TODO improve.  E.g. make random_access_tag compatible.
        class iterator : public std::iterator<
                         std::input_iterator_tag, // iterator_category
                         ELEM,                    // value_type
                         ptrdiff_t,               // difference_type
                         const ELEM*,             // pointer (UNUSED)
                         ELEM>                    // reference: just create a copy; reflection does that anyway:
        {
            const vector &vec;
            std::size_t I;
        public:
            constexpr explicit iterator(const vector & vec, std::size_t I = 0) : vec(vec), I(I) {}
            constexpr iterator& operator++()                { ++I; return *this; }
            constexpr iterator operator++(int)              { iterator retval = *this; ++(*this); return retval; }
            constexpr bool operator==(iterator other) const {
                // NB technically we should check if &vec == &other.vec,
                // but we'll get an error if we iterate out of range so
                // we'll keep it cheap:
                return I == other.I;
            }
            constexpr bool operator!=(iterator other) const { return !(*this == other); }
            constexpr ELEM operator*() const           { return vec.at(I); }
            //TODO once you define an operator[] for the int version, have this return a reference to it here.
        };
    public:
        constexpr iterator  begin() const { return iterator(*this, 0); }
        constexpr iterator  end()   const { return iterator(*this, size()); }
    };
    
} //namespace ce

#endif /* dwrmeta_ce_vector_hpp */
