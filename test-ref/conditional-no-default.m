gcd a b = gcd (a-b) b, a>b
        = gcd a (b-a), a<b
        = a, a==b
gcd 6 9