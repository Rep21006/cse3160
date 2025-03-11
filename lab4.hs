{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib
    (appxPiList
    , primes
    , take'
    , drop'
    , foldl'
    , scanl'
    , randomNums
    , listMin
    , listMax
    , map'
    , filter'
    , elem'
    , any'
    , all'
    , base10
    ) where

import Data.Char

-- Implement your version below. Feel free to add auxiliary functions
-- as needed
checkPrime :: (Integral t) => t -> t -> Bool
checkPrime 0 _ = False
checkPrime 1 _ = False
checkPrime _ 1 = True
checkPrime n d = n `mod` d /= 0 && checkPrime n (d-1)

isPrime :: (Integral a) => a -> Bool
isPrime n = checkPrime n (n `div` 2)


primes :: [Integer]
primes = [x | x <- [1..], isPrime x]

appxPiList :: [Double]
appxPiList = aux 1 1 0
   where aux denom sign acc = acc:aux (denom + 2) (-1 * sign) (acc + (sign * 4/denom))


appxPiList' :: [Double]
appxPiList' = scanl (+) 0 (map (\x -> (4*((-1)**(x-1)) / ((2*x)-1))) [1..])

take' :: Int -> [a] -> [a]
take' 0 _  = []
take' _ [] = []
take' n (h:t) = h:take' (n-1) t

drop' :: Int -> [a] -> [a]
drop' 0 l  = l
drop' _ [] = []
drop' n (_:t) = drop' (n-1) t

foldl' :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl' _ acc [] = acc
foldl' f acc (h:t) = foldl' f (f acc h) t

scanl' :: (a1 -> a2 -> a1) -> a1 -> [a2] -> [a1]
scanl' f acc l = acc:aux acc l
   where aux _ []    = []
         aux acc' (h:t) = let retVal = f acc' h
             in retVal:aux retVal t

randomNums :: Integral p => p -> p -> p -> [p]
randomNums mult m seed = aux seed
   where aux acc = acc:(aux ((mult * acc + 1) `mod` m))

listMin :: Ord a => [a] -> a
listMin = foldl1 min

listMax :: Ord a => [a] -> a
listMax = foldr1 max

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\ x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\ x acc -> if p x then x:acc else acc) [] xs

elem' :: Eq a => a -> [a] -> Bool
elem' i xs = foldr (\ x acc -> x == i || acc) False xs

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = foldl1 (||) $ map p xs
-- any' = (foldl1 (||) .) . map

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = foldl1 (&&) $ map p xs
-- all' = (foldl1 (&&) .) . map 

base10 :: String -> Int
base10 s
    | head s == '-'  = foldl (\acc d -> 10*acc - digitToInt d) 0 (tail s)
    | otherwise = foldl (\acc d -> 10*acc + digitToInt d) 0 s

-- base10 s@(d:ds)
--     | d == '-'  = foldl (\acc d -> 10*acc - digitToInt d) 0 ds
--     | otherwise = foldl (\acc d -> 10*acc + digitToInt d) 0 s

