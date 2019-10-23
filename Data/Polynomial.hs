{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Data.Polynomial where

import Prelude hiding (Rational,exp)
import Data.List (foldl', dropWhileEnd, elem, find, sortBy, maximumBy)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Function (on)

import Data.RatioInfty


-- A RawPoly is a list of Monomials, where each Monomial is a coefficient and a list of exponents
--  e.g. x^2 - 2x^(-1)y^3 + y could be represented as [(1,[2]),(-2,[-1,3]),(1,[0,1])] :: RawPoly
type Exps = [Int]
type Monomial = (Int, Exps)
type RawPoly = [Monomial]

zipExps :: (Int -> Int -> a) -> Exps -> Exps -> [a]
zipExps f = go
  where go [] es' = map (f 0) es'
        go es []  = map (flip f 0) es
        go (e:es) (e':es') = f e e' : go es es'

eqExps :: Exps -> Exps -> Bool
eqExps es es' = and (zipExps (==) es es')

-- `cmpExps False` compares exponents so that: x^(-1) < 1 < x < x^2
-- `cmpExps True`  compares exponents to that: 1 < x < x^(-1) < x^2
cmpExps :: Bool -> Exps -> Exps -> Ordering
cmpExps cmpAbs es es' = fromMaybe EQ (find (/= EQ) (reverse $ zipExps (compare `on` (if cmpAbs then f else id)) es es'))
  where f :: Int -> Int
        f k | k <= 0 = 2*(-k)
            | k >  0 = 2*k - 1

-- (see `cmpExps`)
cmpOne :: Bool -> Monomial -> Monomial -> Ordering
cmpOne b m m' = cmpExps b (snd m) (snd m')

leadingTerm :: RawPoly -> Monomial
leadingTerm f = maximumBy (cmpOne False) f


-- Reducing exponent lists, Monomials, and RawPolys to normal forms

reduceExps :: Exps -> Exps
reduceExps = dropWhileEnd (==0)

-- Reduces a monomial as much as possible, returning nothing if it's zero
reduceOne :: Monomial -> Maybe Monomial
reduceOne (0,_) = Nothing
reduceOne (e,es) = Just (e,reduceExps es)

-- Factors a monomial into a polynomial, appending it if it can't be combined with any term
factorInOne :: RawPoly -> Monomial -> RawPoly
factorInOne [] (c,es) = maybe [] (:[]) (reduceOne (c,es))
factorInOne ((c',es'):xs) (c,es)
  | eqExps es' es = maybe xs (:xs) (reduceOne (c'+c,es'))
  | otherwise = (c',es'):(factorInOne xs (c,es))

-- (Use as an infix!) Factors the right polynomial into the left
factorIn :: RawPoly -> RawPoly -> RawPoly
factorIn = foldl' factorInOne

-- Reduces a polynomial as much as possible
reduce :: RawPoly -> RawPoly
reduce xs = [] `factorIn` xs

-- Orders the terms of a polynomial according to their exponents
sortPoly :: RawPoly -> RawPoly
sortPoly = sortBy (cmpOne True)


-- Algebraic operations on Monomials and RawPolys

-- Multiplies a monomial by a monomial
multMono :: Monomial -> Monomial -> Monomial
multMono (c,es) (c',es') = (c*c', zipExps (+) es es')

-- Multiplies a monomial by a polynomial, without reduction
multOne :: Monomial -> RawPoly -> RawPoly
multOne = map . multMono

-- Multiplies a monomial by a polynomial, while reducing results (assuming the second argument is already reduced)
multOneR :: Monomial -> RawPoly -> RawPoly
multOneR m = mapMaybe (reduceOne . multMono m)

-- Multiplies a polynomial by a polynomial, without reduction
mult :: RawPoly -> RawPoly -> RawPoly
mult f g = foldl' (\h m -> h ++ multOne m g) [] f

-- Multiplies a polynomial by a polynomial, while reducing results (assuming the second argument is already reduced)
multR :: RawPoly -> RawPoly -> RawPoly
multR f g = foldl' (\h m -> h `factorIn` (multOneR m g)) [] f

-- Multiplies a polynomial by itself a non-negative number of times
-- Note: negative powers are accepted only if the input is a monomial with coefficient 1 or -1
exp :: RawPoly -> Int -> RawPoly
exp [] _ = []
exp [( 1,es)] k = [(1, map (*k) es)]
exp [(-1,es)] k = [(if even k then 1 else -1, map (*k) es)]
exp [(c,es)] k = [(c^k, map (*k) es)]
exp xs k = go xs [(1,[])] k
  where go f g k
          | k < 0 = error "exp: negative exponent"
          | k == 0 = g
          | otherwise = go f (multR f g) (k-1)

-- Performs a substitution, variables not listed are given the value 1
subst :: [RawPoly] -> RawPoly -> RawPoly
subst ss = foldl' (\h m -> h `factorIn` (substOne m)) []
  where substOne :: Monomial -> RawPoly
        substOne (c,es) = multOne (c,[]) (foldl' multR [(1,[])] (zipWith exp ss es))


-- A list of variable names and denominators (see example below) for a RawPoly
--  e.g. [('a',1),('b',-2)] means [(1,[1]),(1,[0,1]),(1,[0,2])] should be displayed as 'a + b^(-1/2) + b^(-1)'
type PolyVars = [(Char,Int)]
data WithVars t = Poly PolyVars t deriving (Functor)

raw :: WithVars t -> t
raw (Poly _ x) = x

-- A polynomial is a RawPoly with variable information
type Poly = WithVars RawPoly

showExps :: PolyVars -> Exps -> String
showExps lbls es = unwords $ mapMaybe showExp (zip es lbls)
  where showExp :: (Int,(Char,Int)) -> Maybe String
        showExp (0,_) = Nothing
        showExp (e,(v,d))
          | (e%d) == 1                    = Just [v]
          | denominator (e%d) /= 1        = Just $ v:'^':("(" ++ show (numerator (e%d)) ++ "/" ++ show (denominator (e%d)) ++ ")")
          | numerator (e%d) `elem` [0..9] = Just $ v:'^':(show (numerator (e%d)))
          | otherwise                     = Just $ v:'^':("(" ++ show (numerator (e%d)) ++ ")")

showTerm :: PolyVars -> Monomial -> String
showTerm lbls (c,es) | c <  0 = " - " ++ go (-c,es)
                     | c >= 0 = " + " ++ go (c,es)
  where go (0,_) = "0"
        go (1,es) = case showExps lbls es of
                      [] -> "1"
                      str -> str
        go (c,es) = show c ++ showExps lbls es

instance Show Poly where
  show (Poly vs f) = cleanup (concatMap (showTerm vs) f)
    where cleanup :: String -> String
          cleanup [] = "0"
          cleanup (' ':'+':' ':xs) = xs
          cleanup (' ':xs) = xs
          cleanup xs = xs
