module Lib
    ( sumSQ, sumEVEN, hProd, divisors, appxpi, goldenAppx,
      goldenRootAppx,twor, randomGen, perrin, perrinFast,perrinFast2,
      alternatingFac
    ) where

sumSQ :: (Eq t,Num t) => t -> t 
sumSQ 0 = 0
sumSQ n = (n * n) + sumSQ (n - 1)

sumEVEN :: (Eq t,Num t) => t -> t 
sumEVEN 0 = 0
sumEVEN n = (2 * n) + sumEVEN (n - 1)


hProd :: (Fractional a, Integral t) => t -> a
hProd 1 = 1
hProd k = (1.0 - (1.0 / fromIntegral k)) * hProd (k - 1)

divides :: Integral a => a -> a -> Bool
divides a b = b `mod` a == 0

divisorsUpto :: (Integral t, Num a) => t -> t -> a
divisorsUpto _ 1 = 1
divisorsUpto n k | divides k n = 1 + divisorsUpto n (k - 1)
                 | otherwise = divisorsUpto n (k - 1)

divisors :: (Integral t, Num a) => t -> a
divisors n = divisorsUpto n n

appxpi :: (Eq a,Num a,Floating b) => a -> b
appxpi n = let aux 0 _ _ = 0
               aux k sign denom = (sign * 4 / denom) + aux (k-1) (-sign) (denom + 2)
           in aux n 1 1

goldenAppx :: (Eq a,Num a,Floating b) => a -> b
goldenAppx n = let goldenAppxAux 0 = 0
                   goldenAppxAux k = 1 / (1 + goldenAppxAux (k - 1))
               in 1 + goldenAppxAux n

goldenRootAppx :: (Eq a,Num a,Floating b) => a -> b
goldenRootAppx 0 = 0
goldenRootAppx n = sqrt (1 + goldenRootAppx (n-1))

-- From your lab.... solutions to simple routines you will reuse here.

fac :: (Eq a, Num a) => a -> a
fac a
  | a == 0 = 1
  | otherwise = a * fac (a-1)


-- true if and only if at least 2 out of 3 are true.
twor :: Bool -> Bool -> Bool -> Bool
twor True True _ = True
twor True _ True = True
twor _ True True = True
twor _ _ _       = False

randomGen :: (Integral a) => a -> a -> a -> a -> a
randomGen mult m seed = aux
  where
    aux  0 = seed
    aux  x = ((aux $ x-1) * mult + 1) `mod` m
    
--random_generator mult m seed = \x -> (random_series mult m seed x)

-- naive version (2 rec call, lib fib)
perrin :: (Eq a, Num a) => a -> a
perrin n
  | n == 0 = 3
  | n == 1 = 0
  | n == 2 = 2
  | otherwise =  (perrin $ n-2) + (perrin $ n-3)

-- insist on linear (fast) recursion (can do tail if you wish, not mandatory)
perrinFast :: (Eq a, Num a) => a -> a
perrinFast 0 = 3
perrinFast 1 = 0
perrinFast 2 = 2
perrinFast n = aux 3 0 2 n
  where
    aux a _ _ 0 = a
    aux a b c i = aux b c (a+b) (i-1)


perrinFast2 :: (Eq a, Num a) => a -> a
perrinFast2 n = let (v,_,_) = aux n
                in v
  where aux 0 = (3,0,2)
        aux k = let (a,b,c) = aux (k-1) in (b,c,a+b)
    
-- [n! - (n-1)!] + [ (n-2)! - (n-3)! ] + ...
alternatingFac :: (Eq a, Num a) => a -> a
alternatingFac 1 = 1
alternatingFac a = (fac a) - (alternatingFac $ a-1)
