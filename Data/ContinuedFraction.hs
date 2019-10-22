module Data.ContinuedFraction where

import Prelude hiding (Rational)
import Data.List (mapAccumL, concatMap)

import Data.RatioInfty


type CF = [Int]

-- The canonical representation of a continued fraction as a rational number
quo :: CF -> Rational
quo = foldr (\n q -> (n % 1) + 1 / q) (1/0)

-- A useful map, which is an involution over 1 < |p/q| < infinity
invol :: Rational -> Rational
invol x = p % (q - (signum q) * p)
  where (p,q) = (numerator' x, denominator' x)

-- Given a rational number |p/q| >= 1, its positive continued fraction expansion (up to `invol`)
posCF :: Rational -> CF
posCF q = if (abs q) < 1 then error "Rational number p/q does not satisfy |p/q| >= 1"
          else if q < 0 then posCF (invol q) else euclid (numerator q) (denominator q)
  where euclid :: (Integral a) => a -> a -> [a]
        euclid _ 0 = []
        euclid x y = (x `div` y):(euclid y (x `mod` y))

isPositive :: CF -> Bool
isPositive = all (> 0)

toPos :: CF -> CF
toPos cf = if isPositive cf then cf else posCF (quo cf)

ensurePos :: CF -> (a -> a)
ensurePos cf = if isPositive cf then id else error "Continued fraction is not positive"

-- Given a rational number |p/q| >= 1, its even continued fraction expansion (up to `invol`)
evenCF :: Rational -> CF
evenCF q = if (abs q) < 1 then error "Rational number p/q does not satisfy |p/q| >= 1"
           else if odd (numerator q) && odd (denominator q) then evenCF (invol q) else euclid' (numerator q) (denominator q)
  where euclid' :: (Integral a) => a -> a -> [a]
        euclid' _ 0 = []
        euclid' x y = if even (x `div` y)
                      then (x `div` y):(euclid' y (x `mod` y))
                      else (x `div` y + 1):(euclid' y (x `mod` y - y))

isEven :: CF -> Bool
isEven = all (\b -> even b && b /= 0)

toEven :: CF -> CF
toEven cf = if isEven cf then cf else evenCF (quo cf)

ensureEven :: CF -> (a -> a)
ensureEven cf = if isEven cf then id else error "Continued fraction is not positive"


-- Type and sign sequences
----------------------------

data Sgn = Pl | Mn deriving (Eq, Ord)

instance Show Sgn where
  show Pl = "1"
  show Mn = "-1"

neg :: Sgn -> Sgn
neg Pl = Mn
neg Mn = Pl

-- multiply the sign of an integer by a Sgn
mulSgn :: Int -> Sgn -> Sgn
mulSgn k = if k >= 0 then id else neg

toInt :: Sgn -> Int
toInt Pl =  1
toInt Mn = -1

type TypeSeq = [Sgn]

typ :: CF -> TypeSeq
typ cf = snd $ mapAccumL (\curr_sgn c -> (neg curr_sgn, c `mulSgn` curr_sgn)) Pl cf

type SignSeq = [Sgn]

sgn :: CF -> SignSeq
sgn cf = concat (zipWith (\c t -> replicate (abs c) t) cf (typ cf))
