
module Lib
    ( checkPrime,isPrime, or', xor', nand', triangleNumbers, pow, choose
    ) where


-- Implement your versions below.
-- Feel free to add auxiliary functions as needed.
checkPrime :: (Integral t) => t -> t -> Bool
checkPrime 0 _ = False
checkPrime 1 _ = False
checkPrime _ 1 = True
checkPrime n d = n `mod` d /= 0 && checkPrime n (d-1)


isPrime :: (Integral a) => a -> Bool
isPrime n = checkPrime n (n `div` 2)


or' :: Bool -> Bool -> Bool
or' False False = False
or' _     _     = True

xor' :: Bool -> Bool -> Bool
xor' True  True  = False
xor' False False = False
xor' _     _     = True

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _    _    = True

triangleNumbers :: (Ord a, Num a) => a -> a
triangleNumbers 0 = 0
triangleNumbers t = t + triangleNumbers (t-1)

pow :: (Integral a) => a -> a -> a
pow 0 _ = 0
pow _ 0 = 1
pow a b = a * pow a (b-1)

choose :: (Integral a) => a -> a -> a
choose n k
  | k < 0     = 0
  | n < k     = 0
  | otherwise = fac n `div` (fac k * fac (n - k))
  where fac :: (Eq t,Num t) => t -> t
        fac 0 = 1
        fac a = a * fac (a - 1)
