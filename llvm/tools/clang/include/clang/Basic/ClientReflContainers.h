#ifndef CLIENTREFLCONTAINERS_H
#define CLIENTREFLCONTAINERS_H

/*
 * NOTES:
 * You can use templates to help you with your definitions, BUT nested  member classes within templates will be
 * problematic if they are used as return types/param types to reflected functions, so avoid those;
 * and also, inherit your template specializations into non-template classes; you can't reflect the template
 * specialization itself.
 * I hope these restrictions are temporary -- the GenReflectionSrc.cpp is currently very hacky, needs
 * to be totally rewritten.
 *
 * */



#include <vector>
#include <set>
#include <map>
#include <iterator>

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h" //for debugging
#include "clang/AST/ASTContext.h"

//#include "clang/Basic/ContainerGetNth.h" //so we can specialize this to avoid O(N^2) iteration in for... loops

//DWR TODO: these ContainerGetNth specializations are not doing much for us, given that decls() etc.
// are already implemented as lists and so aren't efficient either, so might as well take that stuff out
// and instead focusing on better implementing TupleExpansion.

////DWR TODO add specializations of this to Set and
//template<typename T>
//auto ContainerGetNth(const T& t, std::size_t N) -> decltype(*t.begin()) {
//  return *std::next(t.begin(), N);
//}



#define CLANG_REFLECT_CONSTRUCTIBLE  __attribute__((annotate("reflect_constructible")))


#define DEF_ITERS(data)\
public:\
  using iterator        = typename decltype(data)::iterator;\
  using const_iterator  = typename decltype(data)::const_iterator;\
  inline const_iterator  begin() const { return data.begin(); }\
  inline const_iterator  end()   const { return data.end(); }\

#define DEF_ITERS_ETC(data)\
  DEF_ITERS(data)\
  inline std::size_t     size()  const { return data.size(); }\
  inline bool            empty() const { return data.empty(); }\
  inline void            clear()       { data.clear(); }\
/**/

#define CTORS_ETC(Name)\
protected:\
  clang::ASTContext &C;\
public:\
  Name(clang::ASTContext &C) : C(C) {}\
  Name(const Name &other) : data(other.data), C(other.C) {}\
  /** @returns a pointer to the copied container, cast as\
    * a IntType */\
  IntType clone() const {\
    return reinterpret_cast<IntType>(new (C) Name(*this));\
  }\
/**/








namespace reflcontainers {

using StringType = llvm::StringRef;
using IntType = intptr_t;

namespace {
  template<typename T>
  T nullval() {
    return T();
  }

  // We want to make sure our nullval strings still have nonnull const char *:
  template<>
  StringType nullval() {
    return "";
  }
}

////////////// VECTORS /////////////

//#define DEF_REFLVECTOR_CONTENTS(MP_type)\
//private:\
//  std::vector<MP_type> data;\
//public:\
//  DEF_ITERS_ETC(data)\
//  inline void resize(std::size_t N)   { data.resize(N); }\
//  inline std::size_t capacity() const { return data.capacity(); }\
//  inline void reserve(std::size_t N)  { data.reserve(N); }\
//  inline void shrink_to_fit()         { data.shrink_to_fit(); }\
//  inline void push_back(MP_type x)    { data.push_back(x); }\
//  inline void pop_back()              { data.pop_back(); }\
//  void dealloc() { \
//    resize(0); \
//    shrink_to_fit(); \
//  }\
///**/

template<typename elem_t>
class Vector {
  std::vector<elem_t> data;
public:
  CTORS_ETC(Vector)
  DEF_ITERS_ETC(data)

  inline void resize(std::size_t N)   { data.resize(N); }
  inline std::size_t capacity() const { return data.capacity(); }
  inline void reserve(std::size_t N)  { data.reserve(N); }
  inline void shrink_to_fit()         { data.shrink_to_fit(); }
  inline void push_back(elem_t x)     { data.push_back(x); }
  inline void pop_back()              { data.pop_back(); }
  void dealloc() {
    resize(0);
    shrink_to_fit();
  }

  /// If I is out of range, returns default construction.
  inline elem_t at(std::size_t I) const {
    return (I >= data.size() ? nullval<elem_t>() : data[I]);
  }

  /// If I is out of range, does nothing
  void assign(std::size_t I, elem_t val) {
    if (I < data.size())
      data[I] = val;
  }

  /// If size==0, returns null
  inline elem_t front() const {
    return at(0);
  }

  /// If size==0, returns null
  inline elem_t back() const {
    return empty() ? nullval<elem_t>() : at(size()-1);
  }
};

namespace {
  // For GenReflectionSrc to see the iterator types properly,
  // need to preinstantiate all the base templates we'll use:
  LLVM_ATTRIBUTE_UNUSED void dummyVectorInstantiator(clang::ASTContext &C) {
    Vector<IntType>::const_iterator m1;
    Vector<StringType>::const_iterator m2;
  }
}

class CLANG_REFLECT_CONSTRUCTIBLE VectorStr : public Vector<StringType> {
public:
  VectorStr(clang::ASTContext &C) : Vector<StringType> (C) {}

  /// @returns a pointer to the copied container, cast as
  /// a IntType
  IntType clone() const {
    return reinterpret_cast<IntType>(new (C) VectorStr(*this));
  }
};

class CLANG_REFLECT_CONSTRUCTIBLE VectorInt : public Vector<IntType> {
public:
  VectorInt(clang::ASTContext &C) : Vector<IntType> (C) {}

  /// @returns a pointer to the copied container, cast as
  /// a IntType
  IntType clone() const {
    return reinterpret_cast<IntType>(new (C) VectorInt(*this));
  }
  // void * overloads:
  using Vector<IntType>::assign;
  using Vector<IntType>::push_back;
  inline void assign(std::size_t I, void *val) {
    Vector<IntType>::assign(I, reinterpret_cast<IntType>(val));
  }
  inline void push_back(void *x) {
    Vector<IntType>::push_back(reinterpret_cast<IntType>(x));
  }
};



//class CLANG_REFLECT_CONSTRUCTIBLE VectorInt {
//  DEF_REFLVECTOR_CONTENTS(IntType)
//  CTORS_ETC(VectorInt)

//  // void pointer overload:
//  inline void push_back(void *x) {
//    data.push_back(reinterpret_cast<IntType>(x));
//  }

//  /// If I is out of range, returns null.
//  inline IntType at(std::size_t I) const {
//    return (I >= data.size() ? 0 : data[I]);
//  }

//  /// If I is out of range, does nothing
//  void assign(std::size_t I, IntType val) {
//    if (I < data.size())
//      data[I] = val;
//  }

//  /// If I is out of range, does nothing
//  void assign(std::size_t I, void *val) {
//    assign(I, reinterpret_cast<IntType>(val));
//  }

//  /// If size==0, returns null
//  inline IntType front() const {
//    return at(0);
//  }

//  /// If size==0, returns null
//  inline IntType back() const {
//    return empty() ? 0 : at(size()-1);
//  }

//};


//class CLANG_REFLECT_CONSTRUCTIBLE VectorStr {
//  DEF_REFLVECTOR_CONTENTS(StringType)
//  CTORS_ETC(VectorStr)

//  /// If N is out of range, returns an empty string.
//  inline StringType at(std::size_t N) const {
//    return (N >= data.size() ? "" : data[N]);
//  }

//  /// If I is out of range, does nothing
//  void assign(std::size_t I, StringType val) {
//    if (I < data.size())
//      data[I] = val;
//  }

//  /// If size==0, returns empty string
//  inline StringType front() const {
//    return at(0);
//  }

//  /// If size==0, returns empty string
//  inline StringType back() const {
//    return empty() ? "" : at(size()-1);
//  }
//};



//////////////// SETS //////////////////


//IntType ContainerGetNth(const class SetInt& t, std::size_t N); //cpp



template<typename key_t>
class Set {
  std::set<key_t> data;
public:
  CTORS_ETC(Set)
  DEF_ITERS_ETC(data)

//private:
//  /// Only ContainerGetNth should touch this!!!
//  mutable iterator containergetnthONLY_cachedlast_iter;
//  mutable std::size_t containergetnthONLY_cachedlast_N;
//  friend IntType ContainerGetNth(const Set& t, std::size_t N);
//public:

  inline bool contains(key_t key) const { return data.count(key); }
  /// @returns whether key was inserted
  inline bool insert(key_t key)         { return data.insert(key).second; }
  inline void erase(key_t key)          { data.erase(key); }

  /// A helper function for iterating; can feed I == 0 through I == size()-1 into this.
  /// Not efficient right now, but could easily be made efficient by caching the I-1st
  /// value...but once we can get normal iterators to work, won't be needed at all.
  /// Bottom line don't rely directly on this, consider it deprecated.
  key_t at_ith(unsigned I) const {
    if (I > size())
      return nullval<key_t>();
    return *std::next(data.begin(), I); //INEFFICIENT
  }

  inline void dealloc() { clear(); }
};

namespace {
// For GenReflectionSrc to see the iterator types properly,
// need to preinstantiate all the base templates we'll use:
  LLVM_ATTRIBUTE_UNUSED void dummySetInstantiator(clang::ASTContext &C) {
    Set<IntType>::const_iterator m1;
    Set<StringType>::const_iterator m2;
  }
}

class CLANG_REFLECT_CONSTRUCTIBLE SetInt : public Set<IntType> {
public:
  SetInt(clang::ASTContext &C) : Set<IntType> (C) {}

  /// @returns a pointer to the copied container, cast as
  /// a IntType
  IntType clone() const {
    return reinterpret_cast<IntType>(new (C) SetInt(*this));
  }

  //void * overloads (mainly so IDE doesn't complain, but it's conceivable you
  // could find a legit pointer you want to pass:
  using Set<IntType>::contains;
  using Set<IntType>::insert;
  using Set<IntType>::erase;
  inline bool contains(void *key) const { return Set<IntType>::contains(reinterpret_cast<IntType>(key)); }
  /// @returns whether key was inserted.
  inline bool insert(void *key)         { return Set<IntType>::insert(reinterpret_cast<IntType>(key)); }
  inline void erase(void *key)          { Set<IntType>::erase(reinterpret_cast<IntType>(key)); }
};

class CLANG_REFLECT_CONSTRUCTIBLE SetStr : public Set<StringType> {
public:
  SetStr(clang::ASTContext &C) : Set<StringType> (C) {}

  /// @returns a pointer to the copied container, cast as
  /// a IntType
  IntType clone() const {
    return reinterpret_cast<IntType>(new (C) SetStr(*this));
  }
};




//StringType ContainerGetNth(const class SetStr& t, std::size_t N); //cpp

//class CLANG_REFLECT_CONSTRUCTIBLE SetStr {
//  std::set<StringType> data;
//public:
//  CTORS_ETC(SetStr)
//  DEF_ITERS_ETC(data)

////private:
////  /// Only ContainerGetNth should touch these!!!
////  mutable iterator containergetnthONLY_cachedlast_iter;
////  mutable std::size_t containergetnthONLY_cachedlast_N;
////  friend StringType ContainerGetNth(const class SetStr& t, std::size_t N);
////public:

//  inline bool contains(StringType key) const { return data.count(key); }
//  /// @returns whether key was already in the set.
//  inline bool insert(StringType key)         { return data.insert(key).second; }
//  inline void erase(StringType key)          { data.erase(key); }

//  /// A helper function for iterating; can feed I == 0 through I == size()-1 into this.
//  /// If I out of range, returns default construction.
//  /// Not efficient right now, but could easily be made efficient by caching the I-1st
//  /// value...but once we can get normal iterators to work, won't be needed at all.
//  /// Bottom line don't rely directly on this, consider it deprecated.
//  StringType at_ith(unsigned I) const {
//    if (I >= size())
//      return StringType();
//    return *std::next(data.begin(), I); //INEFFICIENT
//  }

//  inline void dealloc() { clear(); }
//};





//////////////// MAPS //////////////////




//struct IntStrPair {
//  IntType first;
//  StringType second;

//  template<typename T>
//  IntStrPair(T it) : first(it->first), second(it->second) {}

//  IntStrPair() {}
//};

////IntStrPair ContainerGetNth(const class MapIntStr& t, std::size_t N);

////DWR TODO:
////First, the cache val should always be a pointer,
//// that you dereference as needed.
//// Then, all the assign functions should check the cache,
//// since you'll often fetch the same key or check whether
//// the key is contained (even in just a ce_assert) before
//// assigning to a key.  Then we can just

//class CLANG_REFLECT_CONSTRUCTIBLE MapIntStr {
//public:
//  using key_t = IntType;
//  using val_t = StringType;

//private:
//  std::MapBase data;
//  /// cache for efficiency when calling
//  /// at(key) right after contains(key)
//  mutable decltype(data)::const_iterator cachedKV;
//  mutable bool validcache = false;
//  mutable bool cachedKV_is_tentative_insertion_val = false;

//  inline bool cachedKV_is_tentative_insertion() const {
//    return cachedKV_is_tentative_insertion_val && validcache;
//  }

//  void set_cache(decltype(cachedKV) newCachedKV,
//                 bool is_tentative_insertion = false) const {
//    // First handle the old cache data.  If it was a tentative insertion
//    // and you didn't end up assigning anything to it, erase it.
//    if (cachedKV_is_tentative_insertion())
//      const_cast<std::map<k,v> &>(data).erase(cachedKV);

//    // Now we can overwrite the old cache:
//    cachedKV_is_tentative_insertion_val = is_tentative_insertion;
//    cachedKV = newCachedKV;
//    validcache = true;
//  }

//  void load_cache_if_necc(key_t key) const {
//    if (!validcache || key != cachedKV->first) {
//      contains(key);
//    }
//  }
//  using realiter_t = decltype(data)::const_iterator;
//public:
//  CTORS_ETC(MapIntStr)

//  // ITERATOR/RANGE ITERATION STUFF:
//  // Need to manually define the iterator class, instead of passing along
//  // data's iterator like we can for vectors and sets, b/c GenReflectionSrcs.cpp
//  // can't handle templated return types in general.
//  // Really should just fix that issue: just name template instances in some
//  // standardized way, treat them like classes, each spec. with its own
//  // ReflectionObjKind value etc. But this nonsense works for now.
////  struct kvpair_t {
////    key_t first;
////    val_t second;
////    kvpair_t(realiter_t realit) : first(realit->first), second(realit->second) {}
////  };
//  using kvpair_t = IntStrPair;

//  class iterator : public std::iterator<
//                         std::input_iterator_tag,   // iterator_category
//                         kvpair_t,                    // value_type
//                         ptrdiff_t,                 // difference_type
//                         const kvpair_t*,             // pointer (UNUSED)
//                         kvpair_t>                    // reference: just create a copy; reflection does that anyway:
//  {
//    realiter_t _realiter;
//  public:
//    explicit iterator(realiter_t realiter = {}) : _realiter(realiter) {}
//    iterator& operator++()                { ++_realiter; return *this; }
//    iterator operator++(int)              { iterator retval = *this; ++(*this); return retval; }
//    bool operator==(iterator other) const { return _realiter == other._realiter; }
//    bool operator!=(iterator other) const { return !(*this == other); }
//    reference operator*() const           { return kvpair_t(_realiter); }
//  };

//  using const_iterator = const iterator;
//  inline const_iterator  begin() const    { return iterator(data.begin()); }
//  inline const_iterator  end()   const    { return iterator(data.end()); }

//  //END iterator stuff

////private:
////  /// Only ContainerGetNth should touch this!!!
////  mutable iterator containergetnthONLY_cachedlast_iter;
////  mutable std::size_t containergetnthONLY_cachedlast_N;
////  friend IntStrPair ContainerGetNth(const MapIntStr& t, std::size_t N);
////public:


//  inline std::size_t size()  const {
//    return data.size() - cachedKV_is_tentative_insertion();
//  }
//  inline bool empty() const {
//    return size() == 0;
//  }
//  void clear() {
//    data.clear();
//    validcache = false;
//    assert(empty());
//  }

//  /// A helper function for iterating; can feed I == 0 through I == size()-1 into this.
//  /// If I out of range, returns default construction.
//  /// Not efficient right now, but could easily be made efficient by caching the I-1st
//  /// value...but once we can get normal iterators to work, won't be needed at all.
//  /// Bottom line don't rely on this too much, consider it deprecated.
//  IntStrPair at_ith(unsigned I) const {
//    if (I >= size())
//      return {};
//    return IntStrPair(std::next(data.begin(), I)); //INEFFICIENT
//  }

//  inline void dealloc() { clear(); }


//  /// Updates cache after, but does NOT
//  /// check cache first, so e.g.:
//  /// \code
//  ///   mymap.assign(key, myval);
//  ///   ce_assert(mymap.contains(key));
//  /// \endcode
//  /// will do TWO lookups.
//  inline bool contains(void *key) const { return contains(reinterpret_cast<IntType>(key)); }
//  bool contains(key_t key) const {
//    auto insres = const_cast<std::MapBase &>(data).insert( {key, nullval<val_t>()} );
//    set_cache(insres.first, /*insertion occurred=*/insres.second);
//    return !insres.second;
//  }

//  /// @returns a value, not a reference.  (Use
//  /// assign(key,val) to modify the value at key.)
//  /// Returns a default constructed value if not found.
//  /// Tries cache first, udpates cache afterward.
//  /// E.g.:
//  ///
//  /// \code
//  ///   if (mymap.at(key) != desiredval)
//  ///     mymap.assign(key, desiredval);
//  /// \endcode
//  ///
//  /// will do ONE lookup/insertion, and even this:
//  ///
//  /// \code
//  /// if (mymap.contains(key)) {
//  ///   if (mymap.at(key) != desiredval)
//  ///     mymap.assign(key, desiredval);
//  /// }
//  /// if (stilldontlikeit(mymap.at(key))
//  ///    mymap.assign( key, somefunc(mymap.at(key)) );
//  /// \endcode
//  ///
//  /// will still do only ONE lookup/insertion.
//  /// But any intervening
//  /// at/assign/contains call with a key2 != key will change
//  /// the cache and thereby incur extra lookups; e.g.:
//  ///
//  /// \code
//  /// if (mymap.contains(key1))
//  ///   mymap.assign(key1, mymap.at(key2));
//  /// \endcode
//  ///
//  /// will incur THREE lookups.
//  /// So try to group together operations on the same key
//  /// where possible.
//  ///
//  inline val_t at(void *key) const { return at(reinterpret_cast<IntType>(key)); }
//  val_t at(key_t key) const {
//    load_cache_if_necc(key);
//    assert(validcache && key == cachedKV->first); //sanity check
//    return cachedKV->second;
//  }

//  /// Overwrites any existing value for key.
//  /// Tries cache first, updates cache after.
//  ///
//  /// E.g.:
//  ///
//  /// \code
//  ///   if (mymap.at(key) != desiredval)
//  ///     mymap.assign(key, desiredval);
//  /// \endcode
//  ///
//  /// will do ONE lookup/insertion, and even this:
//  ///
//  /// \code
//  /// if (mymap.contains(key)) {
//  ///   if (mymap.at(key) != desiredval)
//  ///     mymap.assign(key, desiredval);
//  /// }
//  /// if (stilldontlikeit(mymap.at(key))
//  ///    mymap.assign( key, somefunc(mymap.at(key)) );
//  /// \endcode
//  ///
//  /// will still do only ONE lookup/insertion.
//  /// But any intervening
//  /// at/assign/contains call with a key2 != key will change
//  /// the cache and thereby incur extra lookups; e.g.:
//  ///
//  /// \code
//  /// if (mymap.contains(key1))
//  ///   mymap.assign(key1, mymap.at(key2));
//  /// \endcode
//  ///
//  /// could incur THREE lookups.
//  /// So try to group together operations on the same key
//  /// where possible.
//  ///
//  inline void assign(void *key, val_t val) { assign(reinterpret_cast<IntType>(key), val); }
//  void assign(key_t key, val_t val) {
//    load_cache_if_necc(key);
//    assert(validcache && key == cachedKV->first); //sanity check
//    const_cast<decltype(val) &>(cachedKV->second) = val;
//    assert(data[key] == val); // DWR TEMP DEBUG sanity check, REMOVE ME
//    // If it was tentative, it's now real:
//    cachedKV_is_tentative_insertion_val = false;
//  }


//  inline void erase(void *key) { erase(reinterpret_cast<IntType>(key)); }
//  void erase(key_t key) {
//    if (key == cachedKV->first) {
//      validcache = false;
//      data.erase(cachedKV);
//    } else {
//      data.erase(key);
//    }
//  }


//};



template<typename key_t, typename val_t, typename kvpair_t>
class Map {
  std::map<key_t, val_t> data;
  /// cache for efficiency when calling
  /// at(key) right after contains(key)
  mutable typename decltype(data)::const_iterator cachedKV;
  mutable bool validcache = false;
  mutable bool cachedKV_is_tentative_insertion_val = false;

  inline bool cachedKV_is_tentative_insertion() const {
    return cachedKV_is_tentative_insertion_val && validcache;
  }

  void set_cache(decltype(cachedKV) newCachedKV,
                 bool is_tentative_insertion = false) const {
    // First handle the old cache data.  If it was a tentative insertion
    // and you didn't end up assigning anything to it, erase it.
    if (cachedKV_is_tentative_insertion())
      const_cast<decltype(data) &>(data).erase(cachedKV);

    // Now we can overwrite the old cache:
    cachedKV_is_tentative_insertion_val = is_tentative_insertion;
    cachedKV = newCachedKV;
    validcache = true;
  }

  void load_cache_if_necc(key_t key) const {
    if (!validcache || key != cachedKV->first) {
      contains(key);
    }
  }
  using realiter_t = typename decltype(data)::const_iterator;
public:
  CTORS_ETC(Map)

  // ITERATOR/RANGE ITERATION STUFF:
  // Need to manually define the iterator class, instead of passing along
  // data's iterator like we can for vectors and sets, b/c GenReflectionSrcs.cpp
  // can't handle templated return types in general.
  // Really should just fix that issue: just name template instances in some
  // standardized way, treat them like classes, each spec. with its own
  // ReflectionObjKind value etc. But this nonsense works for now.
//  struct kvpair_t {
//    key_t first;
//    val_t second;
//    kvpair_t(realiter_t realit) : first(realit->first), second(realit->second) {}
//    kvpair_t() : first(nullval<key_t>()), second(nullval<val_t>()) {}
//  };
////  using kvpair_t = IntStrPair;

  class iterator : public std::iterator<
                         std::input_iterator_tag,   // iterator_category
                         kvpair_t,                    // value_type
                         ptrdiff_t,                 // difference_type
                         const kvpair_t*,             // pointer (UNUSED)
                         kvpair_t>                    // reference: just create a copy; reflection does that anyway:
  {
    realiter_t _realiter;
  public:
    explicit iterator(realiter_t realiter = {}) : _realiter(realiter) {}
    iterator& operator++()                { ++_realiter; return *this; }
    iterator operator++(int)              { iterator retval = *this; ++(*this); return retval; }
    bool operator==(iterator other) const { return _realiter == other._realiter; }
    bool operator!=(iterator other) const { return !(*this == other); }
    kvpair_t operator*() const           { return kvpair_t(_realiter); }
  };

  inline iterator  begin() const    { return iterator(data.begin()); }
  inline iterator  end()   const    { return iterator(data.end()); }

  //END iterator stuff

//private:
//  /// Only ContainerGetNth should touch this!!!
//  mutable iterator containergetnthONLY_cachedlast_iter;
//  mutable std::size_t containergetnthONLY_cachedlast_N;
//  friend IntStrPair ContainerGetNth(const MapIntStr& t, std::size_t N);
//public:


  inline std::size_t size()  const {
    return data.size() - cachedKV_is_tentative_insertion();
  }
  inline bool empty() const {
    return size() == 0;
  }
  void clear() {
    data.clear();
    validcache = false;
    assert(empty());
  }

  /// A helper function for iterating; can feed I == 0 through I == size()-1 into this.
  /// If I out of range, returns default construction.
  /// Not efficient right now, but could easily be made efficient by caching the I-1st
  /// value...but once we can get normal iterators to work, won't be needed at all.
  /// Bottom line don't rely on this too much, consider it deprecated.
  kvpair_t at_ith(unsigned I) const {
    if (I >= size())
      return {};
    return kvpair_t(std::next(data.begin(), I)); //INEFFICIENT
  }

  /// Okay actually since we can't return classes for now, we have to return the key
  /// and val separately in order to iterate -- soo slow, but this will be fixed
  /// once we get rid of the need for templates.
  key_t at_ith_key(unsigned I) const {
    if (I >= size())
      return {};
    return std::next(data.begin(), I)->first; //INEFFICIENT
  }
  val_t at_ith_val(unsigned I) const {
    if (I >= size())
      return {};
    return std::next(data.begin(), I)->second; //INEFFICIENT
  }

  inline void dealloc() { clear(); }


  /// Updates cache after, but does NOT
  /// check cache first, so e.g.:
  /// \code
  ///   mymap.assign(key, myval);
  ///   ce_assert(mymap.contains(key));
  /// \endcode
  /// will do TWO lookups.
  bool contains(key_t key) const {
    auto insres = const_cast<decltype(data) &>(data).insert( {key, nullval<val_t>()} );
    set_cache(insres.first, /*insertion occurred=*/insres.second);
    return !insres.second;
  }

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
  val_t at(key_t key) const {
    load_cache_if_necc(key);
    assert(validcache && key == cachedKV->first); //sanity check
    return cachedKV->second;
  }

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
  void assign(key_t key, val_t val) {
    load_cache_if_necc(key);
    assert(validcache && key == cachedKV->first); //sanity check
    const_cast<decltype(val) &>(cachedKV->second) = val;
    assert(data[key] == val); // DWR TEMP DEBUG sanity check, REMOVE ME
    // If it was tentative, it's now real:
    cachedKV_is_tentative_insertion_val = false;
  }

  void erase(key_t key) {
    if (key == cachedKV->first) {
      validcache = false;
      data.erase(cachedKV);
    } else {
      data.erase(key);
    }
  }

};

#define DEFPAIRCLASS(NAME, key_t, val_t)\
struct NAME {\
  key_t first;\
  val_t second;\
  template<typename realiter_t>\
  NAME(realiter_t realit) : first(realit->first), second(realit->second) {}\
  NAME() : first(nullval<key_t>()), second(nullval<val_t>()) {}\
}
/**/

DEFPAIRCLASS(IntIntPair, IntType, IntType);
DEFPAIRCLASS(IntStrPair, IntType, StringType);
DEFPAIRCLASS(StrIntPair, StringType, IntType);
DEFPAIRCLASS(StrStrPair, StringType, StringType);

#undef DEFPAIRCLASS


namespace {
  // For GenReflectionSrc to see the iterator types AND the element kvpair_t properly,
  // need to preinstantiate BOTH.  SO dumb, but easiest to just do it for now:
  LLVM_ATTRIBUTE_UNUSED void dummyMapInstantiator(clang::ASTContext &C) {

    Map<IntType, StringType, IntStrPair>::iterator m2;
    Map<StringType, IntType, StrIntPair>::iterator m3;
    Map<StringType, StringType, StrStrPair>::iterator m4;
    Map<IntType, IntType, IntIntPair>::iterator m1;

//    Map<IntType, StringType, IntStrPair>::kvpair_t p2;
//    Map<StringType, IntType, StrIntPair>::kvpair_t p3;
//    Map<StringType, StringType, StrStrPair>::kvpair_t p4;
//    Map<IntType, IntType, IntIntPair>::kvpair_t p1;
  }
}






class CLANG_REFLECT_CONSTRUCTIBLE MapIntStr : public Map<IntType, StringType, IntStrPair> {
public:
  using key_t = IntType;
  using val_t = StringType;
  using MapBase = Map<IntType, StringType, IntStrPair>;
  MapIntStr(clang::ASTContext &C) : MapBase (C) {}

  /// @returns a pointer to the copied container, cast as
  /// a IntType
  IntType clone() const {
    return reinterpret_cast<IntType>(new (C) MapIntStr(*this));
  }

  // void * overloads:
  using MapBase::erase;
  using MapBase::contains;
  using MapBase::at;
  using MapBase::assign;
  inline void erase(void *key) {
    MapBase::erase(reinterpret_cast<key_t>(key));
  }
  inline bool contains(void *key) const {
    return MapBase::contains(reinterpret_cast<key_t>(key));
  }
  inline val_t at(void *key) const {
    return MapBase::at(reinterpret_cast<key_t>(key));
  }
  inline void assign(void *key, val_t val) {
    MapBase::assign(reinterpret_cast<key_t>(key), val);
  }
};

class CLANG_REFLECT_CONSTRUCTIBLE MapStrInt : public Map<StringType, IntType, StrIntPair> {
public:
  using key_t = StringType;
  using val_t = IntType;
  using MapBase = Map<StringType, IntType, StrIntPair>;
  MapStrInt(clang::ASTContext &C) : MapBase(C) {}

  /// @returns a pointer to the copied container, cast as
  /// a IntType
  IntType clone() const {
    return reinterpret_cast<IntType>(new (C) MapStrInt(*this));
  }

  // void * overload:
  using MapBase::assign;
  inline void assign(key_t key, void *val) {
    MapBase::assign(key, reinterpret_cast<val_t>(val));
  }
};

class CLANG_REFLECT_CONSTRUCTIBLE MapStrStr : public Map<StringType, StringType, StrStrPair> {
public:
  using MapBase = Map<StringType, StringType, StrStrPair>;
  MapStrStr(clang::ASTContext &C) : MapBase (C) {}

  /// @returns a pointer to the copied container, cast as
  /// a IntType
  IntType clone() const {
    return reinterpret_cast<IntType>(new (C) MapStrStr(*this));
  }
};

class CLANG_REFLECT_CONSTRUCTIBLE MapIntInt : public Map<IntType, IntType, IntIntPair> {
public:
  using key_t = IntType;
  using val_t = IntType;
  using MapBase = Map<IntType, IntType, IntIntPair>;
  MapIntInt(clang::ASTContext &C) : MapBase(C) {}

  /// @returns a pointer to the copied container, cast as
  /// a IntType
  IntType clone() const {
    return reinterpret_cast<IntType>(new (C) MapIntInt(*this));
  }

  // void * overloads:
  using MapBase::erase;
  using MapBase::contains;
  using MapBase::at;
  using MapBase::assign;
  inline void erase(void *key) {
    MapBase::erase(reinterpret_cast<key_t>(key));
  }
  inline bool contains(void *key) const {
    return MapBase::contains(reinterpret_cast<key_t>(key));
  }
  inline val_t at(void *key) const {
    return MapBase::at(reinterpret_cast<key_t>(key));
  }
  // Need three different combinations for the void * overloads here:
  inline void assign(void *key, val_t val) {
    MapBase::assign(reinterpret_cast<key_t>(key), val);
  }
  inline void assign(key_t key, void *val) {
    MapBase::assign(key, reinterpret_cast<val_t>(val));
  }
  inline void assign(void *key, void *val) {
    MapBase::assign(reinterpret_cast<key_t>(key), reinterpret_cast<val_t>(val));
  }
};










} //namespace reflcontainers

#endif // CLIENTREFLCONTAINERS_H
