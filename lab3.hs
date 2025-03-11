
module Lib
    ( fac
    , dupPairs
    , sumEvens
    , squaresUnder
    , nbSpecial
    , sumPolyOuts
    , adjDiff
    ) where


-- Implement your versions below.
-- These are all one-line functions using only lists, list functions, 
-- and lambdas as needed in the function bodies.


fac :: (Integral n) => n -> n
fac k = product [1..k]


dupPairs :: [b] -> [(b, b)]
dupPairs l = zip l l 


sumEvens :: (Num a, Enum a) => a -> a
sumEvens n = sum [2,4..n]


squaresUnder :: (Num a, Ord a, Enum a) => a -> [a]
squaresUnder n = filter (<n) $ map (^2) [1..n]


nbSpecial :: (Ord a, Num a, Enum a) => [a] -> Int
nbSpecial l = length $ filter (>=0) $ zipWith (\n idx -> n - 7*idx) l [0..]
-- nbSpecial l = length $ filter (>=0) $ zipWith diff l [0..]
--   where
--     diff n idx = n - 7*idx


sumPolyOuts :: (Num a) => (Int -> a) -> Int -> a
sumPolyOuts f k = sum $ map f [1..k]


adjDiff :: (Ord a, Num a) => a -> [a] -> Int
adjDiff k l = length $ filter (>k) $ zipWith (\x y -> abs $ x-y) l (tail l)
-- adjDiff k l = length $ filter (>k) $ zipWith absDiff l (tail l)
--   where
--     absDiff x y = abs $ x - y



