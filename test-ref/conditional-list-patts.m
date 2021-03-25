funnyLastElt (x:xs) = x, x<0
funnyLastElt (x:[]) = x
funnyLastElt (x:xs) = funnyLastElt xs
funnyLastElt [1,2,3]