module Lib
    (f91,arSerie,makeLinear,pyramidVolume
    ) where

pyramidVolume :: (Fractional v) => v -> v -> v -> v
pyramidVolume w l h = w * l * h / 3.0

f91 :: (Integral n) => n -> n
f91 n = if n > 100
        then n - 10
        else f91 (f91 (n+11))

arSerie :: (Integral n) => n -> n
arSerie t = if t == 0
            then 10
            else 1 + 2 * (arSerie (t-1)) 

makeLinear :: (Integral v,Eq t,Num t) => v -> v -> v -> (t -> v)
makeLinear base a b t = if t == 0
                        then base
                        else b + a * (makeLinear base a b (t-1))
  



