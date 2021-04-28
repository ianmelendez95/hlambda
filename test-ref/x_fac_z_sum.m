x = fac z
fac n = if (n == 0) 1 (n * (fac (n - 1)))
z = 4
sum x y = if (x == 0) y (sum (x - 1) (y + 1))

sum x z