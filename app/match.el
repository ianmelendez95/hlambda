demo f []     ys     = A f ys
demo f (x:xs) []     = B f x xs
demo f (x:xs) (y:ys) = C f x xs y ys

demo = \u1.\u2.\u3.
  match [u1, u2, u3]
        [ ([f, NIL,       ys       ], (A f ys)),
          ([f, CONS x xs, NIL      ], (B f x xs)),
          ([f, CONS x xs, CONS y ys], (C f x xs y ys)) ]
        ERROR

-- by the variable rule
demo = \u1.\u2.\u3.
  match [u2, u3]
        [ ([NIL,       ys       ], (A u1 ys)), -- updated f -> u1
          ([CONS x xs, NIL      ], (B u1 x xs)),
          ([CONS x xs, CONS y ys], (C u1 x xs y ys)) ]
        ERROR

-- by the constructor rule 
demo = \u1.\u2.\u3.
  case u2 of 
    NIL =>
      match [u3]
            [ ([ys], (A u1 ys)) ]
            ERROR
    CONS u4 u5 =>
      match [u4, u5, u3] -- introduced new names
            [ ([x, xs, NIL      ], (B u1 x xs)),
              ([x, xs, CONS y ys], (C u1 x xs y ys)) ]
            ERROR

--------------------------------------------------------------------------------
-- mappairs

mappairs f [] ys = []
mappairs f (x:xs) [] = []
mappairs f (x:xs) (y:ys) = f x y : mappairs f xs ys

mappairs = \u1.\u2.\u3.
  match [u1, u2, u3] 
        [([f, NIL,       ys       ], (NIL)),
         ([f, CONS x xs, NIL      ], (NIL)),
         ([f, CONS x xs, CONS y ys], (CONS (f x y) (mappairs f xs ys)))]

match [u1, u2, u3] 
      [([f, NIL,       ys       ], (NIL)),
       ([f, CONS x xs, NIL      ], (NIL)),
       ([f, CONS x xs, CONS y ys], (CONS (f x y) (mappairs f xs ys)))]

match [u2, u3] 
      [([NIL,       ys       ], (NIL)),
       ([CONS x xs, NIL      ], (NIL)),
       ([CONS x xs, CONS y ys], (CONS (u1 x y) (mappairs u1 xs ys)))]

case u2 of 
  NIL => 
    match [u3] 
          [([ys], (NIL))]
  CONS u4 u5 => 
    match [u4, u5, u3] 
          [([x, xs, NIL      ], (NIL)),
           ([x, xs, CONS y ys], (CONS (u1 x y) (mappairs u1 xs ys)))]
  
case u2 of 
  NIL => 
    match [] [([], (NIL))] -- by variable rule: ys -> u3
  CONS u4 u5 => 
    match [u5, u3] 
          [([xs, NIL      ], (NIL)),
           ([xs, CONS y ys], (CONS (u1 u4 y) (mappairs u1 xs ys)))]

case u2 of 
  NIL => NIL -- by empty rule
  CONS u4 u5 => 
    match [u3] 
          [([NIL      ], (NIL)),
           ([CONS y ys], (CONS (u1 u4 y) (mappairs u1 u5 ys)))]

case u2 of 
  NIL => NIL -- by empty rule
  CONS u4 u5 => 
    case u3 of 
      NIL => 
        match [] [([], (NIL))]
      CONS u6 u7 => 
        match [u6, u7] 
              [([y, ys], (CONS (u1 u4 y) (mappairs u1 u5 ys)))]

mappairs = \u1.\u2.\u3.
  case u2 of 
    NIL => NIL -- by empty rule
    CONS u4 u5 => 
      case u3 of 
        NIL => NIL
        CONS u6 u7 => CONS (u1 u4 u6) (mappairs u1 u5 u7)

--------------------------------------------------------------------------------
-- Mixture Rule

demo' f [] ys = A f ys
demo' f xs [] = B f xs
demo' f (x:xs) (y:ys) = C f x xs y ys

-- so first every component is converted to enriched, then we introduce this matched stuff?
-- for simplicity, sure

demo' = \u1.\u2.\u3.
match 4
      [u1, u2, u3]
      [([f, NIL,       ys       ],(A f ys)),
       ([f, xs,        NIL      ],(B f xs)),
       ([f, CONS x xs, CONS y ys],(C f x xs y ys))]
      ERROR

match 4
      [u2, u3]
      [([NIL,       ys       ],(A u1 ys)),
       ([xs,        NIL      ],(B u1 xs)),
       ([CONS x xs, CONS y ys],(C u1 x xs y ys))]
      ERROR

match 4
      [u2, u3]
      [([NIL,       ys       ],(A u1 ys)),
       ([xs,        NIL      ],(B u1 xs)),
       ([CONS x xs, CONS y ys],(C u1 x xs y ys))]
      ERROR

match 4
      [u2, u3]
      [([NIL, ys],(A u1 ys))]
      (match 4 
             [u2, u3]
             [([xs,        NIL      ],(B u1 xs)),
              ([CONS x xs, CONS y ys],(C u1 x xs y ys))]
             ERROR)

match 4
      [u2, u3]
      [([NIL, ys],(A u1 ys))]
      (match 4 
             [u2, u3]
             [([xs, NIL],(B u1 xs))]
             (match 4 
                    [u2, u3]
                    [([CONS x xs, CONS y ys],(C u1 x xs y ys))]
                    ERROR))

case u2 of 
  NIL => match 4
               [u2, u3]
               [([NIL, ys],(A u1 ys))]
               (match 4 
                      [u2, u3]
                      [([xs, NIL],(B u1 xs))]
                      (match 4 
                             [u2, u3]
                             [([CONS x xs, CONS y ys],(C u1 x xs y ys))]
                             ERROR))
match 4
      [u2, u3]
      [([NIL, ys],(A u1 ys))]
      (match 4 
             [u2, u3]
             [([xs, NIL],(B u1 xs))]
             (match 4 
                    [u2, u3]
                    [([CONS x xs, CONS y ys],(C u1 x xs y ys))]
                    ERROR))