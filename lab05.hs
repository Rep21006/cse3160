module Lib
    ( muls, divs, pows, facts, signs, splice, sint,
      tx, mysin, flatten, innerProduct, dedup, vigenereEnc, vigenereDec
    ) where

import Data.Char

-- Vanilla flattening of list of lists
flatten :: [[a]] -> [a]
flatten = foldr (++) []
-- flatten [] = []
-- flatten (l:ls) = l ++ flatten ls

-- innerproduct of two vectors of the same length. 2 implementation shown
innerProduct :: Num a => [a] -> [a] -> a
innerProduct = (sum .) . (zipWith (*))
-- innerProduct [] _ =  0
-- innerProduct _ [] =  0
-- innerProduct (x:xs) (y:ys) =  x*y + innerProduct xs ys

-- remove all duplicates (keep only one copy of each value. Can be O(n^2). From first principles.
dedup :: Eq a => [a] -> [a]
dedup []     = []
dedup (h:t) = h:[x | x <- dedup t, x /= h]
-- dedup []     = []
-- dedup (x:xs) = x : filter (/=x) (dedup xs)

encodeChar :: Char -> Char -> Char
encodeChar k ch = chr newOrd
  where
    curr = ord ch - ord 'a'
    shift = ord k - ord 'a'
    newOrd = ord 'a' + ((curr + shift) `mod` 26)

decodeChar :: Char -> Char -> Char
decodeChar k ch = chr newOrd                          
  where
    curr = ord ch - ord 'a'
    shift = ord k - ord 'a'
    newOrd = ord 'a' + ((curr - shift) `mod` 26)

-- vigenereEnc :: [Char] -> String -> String
-- vigenereEnc key text = unwords $ encodeWords (cycle key) $ words text
--   where
--     encodeWords :: String -> [String] -> [String]
--     encodeWords _ [] = []
--     encodeWords key (w:ws) = let (remKey, encryptedWord) = encodeWord key w
--                              in encryptedWord : encodeWords remKey ws
--     encodeWord :: String -> String -> (String, String)
--     encodeWord k [] = (k, [])
--     encodeWord (k:ks) (c:cs) = let (remKey, encTail) = encodeWord ks cs
                               -- in (remKey, encodeChar k c : encTail)

-- Notice that the decryption function would be almost the same as above,
-- except for using decodeChar in place of encodeChar. So, let's refactor
-- the above code to be able to carry out either encryption or decryption,
-- depending on the character transforming function that is passed in.

vigenereTransform :: (Char -> Char -> Char) -> String -> String -> String
vigenereTransform charTransform key text = unwords $ transformWords (cycle key) $ words text
  where
    -- transforms a list of words using given charTransform function
    transformWords :: String -> [String] -> [String]
    transformWords _ [] = []
    transformWords key (w:ws) = let (remKey, transformedWord) = transformWord key w
                                in transformedWord : transformWords remKey ws

    -- returns tuple of remainder of key and transformed word
    transformWord :: String -> String -> (String, String)
    transformWord k [] = (k, [])
    transformWord (k:ks) (c:cs) = let (remKey, transformedTail) = transformWord ks cs
                                  in (remKey, charTransform k c : transformedTail)
    

vigenereEnc :: [Char] -> String -> String
vigenereEnc = vigenereTransform encodeChar


vigenereDec :: [Char] -> String -> String
vigenereDec = vigenereTransform decodeChar


muls :: Num a => [a] -> [a] -> [a]
muls = zipWith (*)
-- muls [] _ = []
-- muls _ [] = []
-- muls (x:xs) (y:ys) = x*y : muls xs ys


divs :: RealFloat a => [a] -> [a] -> [a]
divs = zipWith (/)
-- divs _ [] = []
-- divs [] _ = []
-- divs (x:xs) (y:ys) = x/y : divs xs ys


-- powers of x from 0 : [x^0,x^1,x^2,x^3,...
pows :: Num t => t -> [t]
pows x = 1 : muls (repeat x) (pows x)

-- all the factorials [0!,1!,2!,3!,....
facts :: [Double]
facts = 1 : muls [1..] facts

-- alternating signs [1,-1,1,-1,1,-1,....
signs :: [Double]
signs = cycle [1,-1]

-- splice a stream [a0,a1,a2,a3,a4... to [a0,a2,a4....
splice :: [a] -> [a]
splice (x:_:t) = x : splice t

-- the stream of cofficients for the sine terms! 
sint :: [Double]
sint = muls signs $ splice $ tail $ divs (repeat 1) facts

-- this is a stream of evaluated terms at x as well, but powers are streams too
tx :: Double -> [Double]
tx x = muls (splice $ tail $ pows x) sint
  
mysin ::  Int -> Double -> Double
-- mysin n x = sum $ take n $ tx x
mysin n = sum . (take n) . tx 

