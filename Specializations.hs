module Specializations where

import Prelude hiding (Rational,exp)
import Data.List (foldl', maximumBy)
import Data.Function (on)

import Data.RatioInfty
import Data.ContinuedFraction
import Data.Poset
import Data.Polynomial

head0 :: [Int] -> Int
head0 [] = 0
head0 (x:_) = x


type FPolySpecialization = (PolyVars, Int -> RawPoly)

specialize :: FPolySpecialization -> [OrderIdeal] -> Poly
specialize (vs,f) fpoly = Poly vs (reduce $ concatMap (foldl' (\h i -> h `mult` (f i)) [(1,[])]) fpoly)


-- Jones specialization from [LS19]

vsJones :: PolyVars
vsJones = [('t',2)]

spJones :: FPolySpecialization
spJones = (vsJones, \i -> if i == 1 then [(1,[-4])] else [(-1,[-2])])

-- the Jones-specialized F-polynomial of the continued fraction [c1,...,cn]
sFJones :: CF -> Poly
sFJones cf = specialize spJones (ideals (poset cf))

leadingTerm :: RawPoly -> RawPoly
leadingTerm f = [maximumBy (compare `on` (head0 . snd)) f]


-- HOMFLY specialization

vsHOMFLY :: PolyVars
vsHOMFLY = undefined

spHOMFLY :: Int -> FPolySpecialization
spHOMFLY b1 = undefined

-- the HOMFLY-specialized F-polynomial of the even continued fraction [b1,...,bn]
sFHOMFLY :: CF -> Poly
sFHOMFLY cf = undefined

m :: CF -> Poly
m cf = undefined

sub_w :: Int -> RawPoly -> RawPoly
sub_w = undefined

msFHOMFLY :: CF -> Poly
msFHOMFLY cf = undefined


-- the Alexander specialization from the HOMFLY specialization above

vsAlexander = vsJones

subst_HOMFLY_to_Alexander :: Poly -> Poly
subst_HOMFLY_to_Alexander = undefined

spAlexander :: FPolySpecialization
spAlexander = undefined

sFAlexander :: CF -> Poly
sFAlexander = undefined

msFAlexander :: CF -> Poly
msFAlexander = undefined
