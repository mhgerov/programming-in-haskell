import Data.Char

-- 5.4 String Comprehensions

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- 5.5 Caesar Cipher
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x| x <- xs]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
            where n = lowers xs

-- 5.7 Exercises

ex1 = sum [x^2 | x <- [1..100]]

grid m n = [(x,y) | x <- [0..m], y <- [0..n]] -- ex2

square n = [g | g <- grid n n, g /= (0,0) && g /= (n,n)] -- ex3