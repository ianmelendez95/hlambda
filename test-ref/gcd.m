gcd a b = gcd diff b,    a>b 
        = gcd a (-diff), a<b
        = a,             a==b
  where 
    diff = a-b

gcd 6 9