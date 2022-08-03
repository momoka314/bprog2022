module First where

double :: Int -> Int
double x = x + x

{-
   double (1 + 3)
-> (1 + 3) + (1 + 3)
-}

quadruple :: Int -> Int
quadruple x = double (double x)

factorial :: Integer -> Integer
factorial n = product [1 .. n]




average :: [Int] -> Int
average xs = sum xs `div` length xs

ave :: [Int] -> Int
ave xs = div (sum xs) (length xs)

myinit :: [a] -> [a]
myinit (x:[]) = []
myinit (x:xs) = x : (init xs)

myconcat :: [[a]] -> [a]
myconcat xss = [ x | xs <- xss, x <- xs ]

mylength :: [a] -> Int
mylength xs = sum [ 1 | _ <- xs ]
