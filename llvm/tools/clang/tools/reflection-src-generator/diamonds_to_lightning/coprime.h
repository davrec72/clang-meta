//
//  coprime.h
//  forceSingleInheritance
//
//  Created by David Rector on 5/25/19.
//

#ifndef coprime_h
#define coprime_h

namespace dwr {
    
//FROM https://developercommunity.visualstudio.com/content/problem/233356/stdgcd-performance-issue.html

    namespace detail {
        constexpr unsigned gcd_positive_numbers( const unsigned a, const unsigned b )
        {
            if( a == 0 || a == b )
                return b;
            if( b == 0 )
                return a;
            if( a > b )
                return gcd_positive_numbers( a % b, b );
            return gcd_positive_numbers( a, b % a );
        }
    }
    

    constexpr unsigned gcd( int a_, int b_ )
    {
        unsigned a = (a_ < 0 ? -a_ : a_);
        unsigned b = (b_ < 0 ? -b_ : b_);
        if( a == 0 || a == b )
            return b;
        if( b == 0 )
            return a;
        if( a > b )
            return detail::gcd_positive_numbers( a % b, b );
        return detail::gcd_positive_numbers( a, b % a );
    }
    
    constexpr bool coprime( int a_, int b_ ) {
        return gcd(a_, b_) <= 1;
    }
    
#ifndef NDEBUG
    //Compile-time TESTS
    static_assert( coprime(2,3), "Test failed!");
    static_assert( coprime(3,4), "Test failed!");
    static_assert(!coprime(2,4), "Test failed!");
    static_assert( coprime(1,1), "Test failed!");
    static_assert( coprime(1,10), "Test failed!");
    static_assert( coprime(10,1), "Test failed!");
    
    //Note these idiosyncrasies when dealing with zero:
    static_assert( coprime(0,0), "Test failed!");
    static_assert( coprime(0,1), "Test failed!"); //are coprime
    static_assert( coprime(1,0), "Test failed!");
    static_assert(!coprime(0,2), "Test failed!"); //are NOT coprime!
    static_assert(!coprime(2,0), "Test failed!");
#endif

    
} //dwr
#endif /* coprime_h */
