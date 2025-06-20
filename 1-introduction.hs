double :: Num a => a -> a
double x = x + x

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns


rsort :: Ord a => [a] -> [a]
rsort [] = []
rsort (x:xs) = rsort larger ++ [x] ++ rsort smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
