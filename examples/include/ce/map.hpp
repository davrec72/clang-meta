//
//  map.hpp
//  clang_meta_examples
//
//  Created by David Rector on 11/3/19.
//  Copyright © 2019 David Rector. All rights reserved.
//

#ifndef dwrmeta_ce_map_hpp
#define dwrmeta_ce_map_hpp

#include <iterator>
#include "../metacore.hpp"
#include "metacast.hpp"
#include "as_const.hpp"

namespace ce {
    
    template<typename K, typename V>
    class map {
        using intelem_t = intptr_t;
        static constexpr bool StoreKAsInt = std::is_integral<K>::value
                                            || ( std::is_enum<K>::value &&
                                                 std::is_convertible<K, intelem_t>::value );
        static constexpr bool StoreVAsInt = std::is_integral<V>::value
                                            || ( std::is_enum<V>::value &&
                                                 std::is_convertible<V, intelem_t>::value );
        
        static constexpr const char *ContainerUTypeStr =
            StoreKAsInt ? (StoreVAsInt ? "MapIntInt" : "MapIntStr")
                        : (StoreVAsInt ? "MapStrInt" : "MapStrStr");
        using u_K = typename std::conditional<StoreKAsInt, intelem_t, const char *>::type;
        using u_V = typename std::conditional<StoreVAsInt, intelem_t, const char *>::type;
        
        static constexpr bool KMetaparsingNeeded = std::is_same<u_K, const char *>::value &&
                                                  !std::is_same<K,   const char *>::value;
        static constexpr bool VMetaparsingNeeded = std::is_same<u_V, const char *>::value &&
                                                  !std::is_same<V,   const char *>::value;
        
        const char *container_refstr;
        
        constexpr intptr_t getclonep() const {
            return __metaparse_expr(__concatenate(container_refstr, ".clone()"), intptr_t);
        }
    public:
        constexpr map()
        : container_refstr(__metaparse_expr(__concatenate("(const char *)*__reflect_new("
                                                          "cppx::meta::refldetail::GetReflectionObjKind<cppx::meta::refldetail::reflcontainers::",
                                                          ContainerUTypeStr, ">::value)"),
                                            const char *))
        {}
        constexpr map(const map &other)
        : container_refstr(__concatenate("cppx::meta::refldetail::reflcontainers::",
                                         ContainerUTypeStr, "::impl<1, ", //IsPtr = 1
                                         other.getclonep(), ">()") )
        {}
        constexpr map(map &&other)
        : container_refstr(std::move(other.container_refstr))
        {}
        //TODO: make part of this static, so you don't have to get the K and V as string every time.  Do same for above as well
        constexpr operator const char *() {
            using namespace cppx::meta;
            using namespace cppx::meta::clang;
            auto_ KreflQT = cast<SubstTemplateTypeParmType>(reflexpr(K))->getReplacementType();
            auto_ VreflQT = cast<SubstTemplateTypeParmType>(reflexpr(V))->getReplacementType();
            return __concatenate("ce::map<", KreflQT.getAsString(), ",", VreflQT.getAsString(), ">(", container_refstr, ")");
        }
        constexpr operator const char *() const {
            return __concatenate("ce::as_const(", const_cast<map *>(this)->operator const char *(), ")");
        }
        
        /// Rather than set up all four different enable-ifd overloads,
        /// let's just give an arbitrary constructor for T;
        /// little less type safe, but users almost certainly should
        /// not be using this constructor anyways:
        template<typename T> //T should be a reflected map type appropriate for K and V.
        constexpr map(T reflcontainer)
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
            // TODO: if Vs have dealloc methods, loop through calling dealloc on each of them first.
            // (Ks too I guess, but would be unusual for K to be something with a dealloc.)
            // (So, you need some a type trait/metafunction to test for a dealloc() method,
            // an if constexpr that tests that, then a normal for loop if true.)
            clear();
        }
        
        // SET-LIKE METHODS:
        
        /// Updates cache after, but does NOT
        /// check cache first, so e.g.:
        /// \code
        ///   mymap.assign(key, myval);
        ///   ce_assert(mymap.contains(key));
        /// \endcode
        /// will do TWO lookups.
        constexpr bool contains(K key) const {
            return __metaparse_expr(__concatenate(container_refstr, ".contains(", metacast<u_K>(key), ")"), bool);
        }
//        /// \returns whether the key was inserted (i.e. false if the key was already in the map)
//        constexpr bool insert(std::pair<K,V> &&kv) {
//            return __metaparse_expr(__concatenate(container_refstr, ".insert(", metacast<u_K>(kv.first), ",", metacast<u_V>(kv.second), ")"), bool);
//        }
        constexpr void erase(K key) {
            __metaparse_expr(__concatenate(container_refstr, ".erase(", metacast<u_K>(key), ")"), void);
        }
        
        // VECTOR-LIKE METHODS:
        
        /// Overwrites any existing value for key.
        /// Tries cache first, updates cache after.
        ///
        /// E.g.:
        ///
        /// \code
        ///   if (mymap.at(key) != desiredval)
        ///     mymap.assign(key, desiredval);
        /// \endcode
        ///
        /// will do ONE lookup/insertion, and even this:
        ///
        /// \code
        /// if (mymap.contains(key)) {
        ///   if (mymap.at(key) != desiredval)
        ///     mymap.assign(key, desiredval);
        /// }
        /// if (stilldontlikeit(mymap.at(key))
        ///    mymap.assign( key, somefunc(mymap.at(key)) );
        /// \endcode
        ///
        /// will still do only ONE lookup/insertion.
        /// But any intervening
        /// at/assign/contains call with a key2 != key will change
        /// the cache and thereby incur extra lookups; e.g.:
        ///
        /// \code
        /// if (mymap.contains(key1))
        ///   mymap.assign(key1, mymap.at(key2));
        /// \endcode
        ///
        /// could incur THREE lookups.
        /// So try to group together operations on the same key
        /// where possible.
        ///
        constexpr void assign(K key, V val) {
            __metaparse_expr(__concatenate(container_refstr, ".assign(", metacast<u_K>(key), ", ", metacast<u_V>(val), ")"), void);
        }
    private:
        constexpr u_V at_hlpr(u_K key) const {
            return __metaparse_expr(__concatenate(container_refstr, ".at(", key, ")"), u_V);
        }
    public:
        /// @returns a value, not a reference.  (Use
        /// assign(key,val) to modify the value at key.)
        /// Returns a default constructed value if not found.
        /// Tries cache first, udpates cache afterward.
        /// E.g.:
        ///
        /// \code
        ///   if (mymap.at(key) != desiredval)
        ///     mymap.assign(key, desiredval);
        /// \endcode
        ///
        /// will do ONE lookup/insertion, and even this:
        ///
        /// \code
        /// if (mymap.contains(key)) {
        ///   if (mymap.at(key) != desiredval)
        ///     mymap.assign(key, desiredval);
        /// }
        /// if (stilldontlikeit(mymap.at(key))
        ///    mymap.assign( key, somefunc(mymap.at(key)) );
        /// \endcode
        ///
        /// will still do only ONE lookup/insertion.
        /// But any intervening
        /// at/assign/contains call with a key2 != key will change
        /// the cache and thereby incur extra lookups; e.g.:
        ///
        /// \code
        /// if (mymap.contains(key1))
        ///   mymap.assign(key1, mymap.at(key2));
        /// \endcode
        ///
        /// will incur THREE lookups.
        /// So try to group together operations on the same key
        /// where possible.
        ///
        constexpr V at(K key) const {
            if constexpr (!VMetaparsingNeeded)
                return static_cast<V>(at_hlpr(metacast<u_K>(key)));
            else {
#ifndef NMETA
                return __metaparse_expr(at_hlpr(metacast<u_K>(key)), V);
#else
                return V();
#endif
            }
        }
        
        
    private:
        friend class iterator;
        struct u_kvpair {
            u_K first;
            u_V second;
        };
        struct kvpair {
            K first;
            V second;
        };
        constexpr u_kvpair at_ith_hlpr(std::size_t I) const {
            nce_assert(I < size() && "out of range");
            // This is SO inefficient -- but we'll cache stuff in Ith to at least make it constant time if you
            // just did it on the I-1th value.  But still, really need to improve this -- figure out how to
            // let std::pairs and std::tuples through reflection.  (Or, again, once you get templates out of here,
            // we can access the pair type right here, so don't worry about it yet...)
            return { __metaparse_expr(__concatenate(container_refstr, ".at_ith_key(", I, ")"), u_K),
                     __metaparse_expr(__concatenate(container_refstr, ".at_ith_val(", I, ")"), u_V) };
        }
        constexpr kvpair at_ith(std::size_t I) const {
            u_kvpair ures = at_ith_hlpr(I);
#ifndef NMETA
            if constexpr (KMetaparsingNeeded) {
                K k = __metaparse_expr(ures.first, K);
                if constexpr (VMetaparsingNeeded)
                    return { k, __metaparse_expr(ures.second, V) };
                else
                    return { k, static_cast<V>(ures.second) };
            } else {
                K k = static_cast<K>(ures.first);
                if constexpr (VMetaparsingNeeded)
                    return { k, __metaparse_expr(ures.second, V) };
                else
                    return { k, static_cast<V>(ures.second) };
            }
#else
            return {K(), V()}; // avoids erroneous IDE flags
#endif
        }
        
        /// Note this one should definitely NOT be made into a random-access iterator.
        class iterator : public std::iterator<
                         std::input_iterator_tag, // iterator_category
                         kvpair,                    // value_type
                         ptrdiff_t,               // difference_type
                         const kvpair*,             // pointer (UNUSED)
                         kvpair>                    // reference: just create a copy; reflection does that anyway:
        {
            const map &data;
            std::size_t I;
        public:
            constexpr explicit iterator(const map & data, std::size_t I = 0) : data(data), I(I) {}
            constexpr iterator& operator++()                { ++I; return *this; }
            constexpr iterator operator++(int)              { iterator retval = *this; ++(*this); return retval; }
            constexpr bool operator==(iterator other) const { return I == other.I; } //NB ignore comparing data
            constexpr bool operator!=(iterator other) const { return !(*this == other); }
            constexpr kvpair operator*() const              { return data.at_ith(I); }
        };
    public:
        constexpr iterator  begin() const { return iterator(*this, 0); }
        constexpr iterator  end()   const { return iterator(*this, size()); }
    };
    
} //namespace ce

#endif /* dwrmeta_ce_map_hpp */
