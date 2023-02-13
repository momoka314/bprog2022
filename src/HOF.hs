module HOF where

import Data.Char

double :: Int -> Int
double x = x + x

doubleList :: [Int] -> [Int]
doubleList []     = []
doubleList (x:xs) = double x : doubleList xs

inc :: Int -> Int
inc x = x + 1

incList :: [Int] -> [Int]
incList []     = []
incList (x:xs) = inc x : incList xs

sample :: [Int]
sample = [3,1,4,1,5,9]

wa :: [Int] -> Int
wa []     = 0
wa (x:xs) = x + wa xs

seki :: [Int] -> Int
seki []     = 1
seki (x:xs) =x * seki xs

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr phi 0
    where
        phi x y = x + 2 * y

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = mod n 2 : int2bin (div n 2)

make8 :: [Bit] -> [Bit]
make8 bs = take 8 (bs ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap  (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int ) . chop8
