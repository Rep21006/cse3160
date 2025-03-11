{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
    ( sdiv, has, cblank, consonants, inter, shortw, muls, facts,
      fibs, psum, makeStore, readStore, writeStore, evaluate,
      derive, compile, Expr (..)
    ) where

import Data.Char
import Data.List

sdiv :: Integral a => a -> [a]
sdiv n = [x | x <- [1..], x `mod` n == 0]

has :: (Num a1, Enum a1, Show a1, Show a2) => a2 -> [a1]
has n = [x | x <- [0..], (head $ show n) `elem` show x]

cblank :: Num a => [Char] -> a
cblank s = sum [1 | ' ' <- s]
-- cblank s = fromIntegral $ length [c | c <- s, c == ' ']
-- cblank s = fromIntegral $ length [' ' | ' ' <- s]

consonants :: [Char] -> [Char]
-- consonants s = [x | x <- [head cl | cl <- group $ sort s], not (x `elem` "aeiouyAEIOUY"), isAlpha x]
consonants s = [x | x <- [head charGrp | charGrp <- group $ sort s], x `elem` cons]
  where
    cons = "bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQRSTVWXZ"

inter :: Eq a => [a] -> [a] -> [a]
inter xs ys = [x | x <- xs, y <- ys, x == y]

shortw :: String -> Int -> [String]
shortw text n = [w | w <- words text, length w <= n]

muls :: Num a => [a] -> [a] -> [a]
muls xs ys = [x * y | x <- xs | y <- ys]

facts :: [Integer]
facts = [x | x <- 1 : muls [1..] facts]

fibs :: [Integer]
fibs = [x | x <- 0 : 1 : zipWith (+) fibs (tail fibs)]

psum :: Num a => [a] -> [a]
psum s = [x | x <- 0 : zipWith (+) (psum s) s]
 
data Expr a b = Add (Expr a b) (Expr a b)
              | Sub (Expr a b) (Expr a b)
              | Mul (Expr a b) (Expr a b )
              | Pow (Expr a b) b
              | Negate (Expr a b)
              | Literal a
              | Variable String  deriving (Eq,Show,Read)

type Store a = [(String,a)]  
makeStore :: (Num a) => Store a
makeStore = []

writeStore :: (Num a) => String -> a -> Store a -> Store a
writeStore k v s = (k,v):s


readStore :: (Num a) => Store a -> String -> a
readStore ((k,v):ps) key
    | k == key  = v
    | otherwise = readStore ps key

evaluate :: (Num a,Integral b) => Expr a b -> Store a -> a
evaluate (Literal n)  _  = n
evaluate (Variable k) s  = readStore s k
evaluate (Negate e)   s  = negate $ evaluate e s
evaluate (Pow e pow)  s  = (evaluate e s) ^ pow
evaluate (Mul e1 e2)  s  = (evaluate e1 s) * (evaluate e2 s)
evaluate (Sub e1 e2)  s  = (evaluate e1 s) - (evaluate e2 s)
evaluate (Add e1 e2)  s  = (evaluate e1 s) + (evaluate e2 s)

derive :: (Num a,Integral b) => Expr a b -> String -> Expr a b
derive (Literal _)  _   = Literal 0
derive (Variable v) x   = if v == x then Literal 1 else Literal 0
derive (Negate e)   x   = Negate (derive e x)
derive (Add e1 e2)  x   = Add (derive e1 x) (derive e2 x)
derive (Sub e1 e2)  x   = Sub (derive e1 x) (derive e2 x)
derive (Pow e p)    x   = Mul (Mul (Literal $ fromIntegral p) (Pow e (p-1)))
                              (derive e x)
derive (Mul e1 e2)  x   = Add (Mul (derive e1 x) e2) (Mul e1 (derive e2 x))

compile :: (Num a,Integral b) => Expr a b -> String -> a -> a
compile (Literal a)  _  = \_ -> a
compile (Variable _) _  = \x -> x  -- id
compile (Negate e)   s  = \x -> -1 * (compile e s $ x)
compile (Add e1 e2)  s  = \x -> (compile e1 s $ x) + (compile e2 s $ x) 
compile (Sub e1 e2)  s  = \x -> (compile e1 s $ x) - (compile e2 s $ x) 
compile (Mul e1 e2)  s  = \x -> (compile e1 s $ x) * (compile e2 s $ x) 
compile (Pow e p)    s  = \x -> (compile e s $ x) ^ p

