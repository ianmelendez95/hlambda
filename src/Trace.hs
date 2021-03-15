module Trace where 

import Debug.Trace 

traceShows :: Show a => [a] -> b -> b
traceShows ss = traceLines (map show ss)

traceLines :: [String] -> a -> a
traceLines ls = trace (unlines ("\n--TRACE--" : ls))