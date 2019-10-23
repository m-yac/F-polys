module Specialization.Jones where

import Prelude hiding (lookup)

import Data.ContinuedFraction
import Data.Polynomial
import Data.Poset

import qualified Specialization.HOMFLY as HOMFLY


vars :: PolyVars
vars = [('t',2)]

spec :: FPolySpecialization
spec = (vars, \i -> if i == 1 then [(1,[-4])] else [(-1,[-2])])

-- the Jones-specialized F-polynomial of the continued fraction [c1,...,cn]
sF :: CF -> Poly
sF cf = specialize spec (ideals (poset cf))


subst_from_HOMFLY :: Poly -> Poly
subst_from_HOMFLY (Poly _ f) = Poly vars (subst subs2 . subst subs1 $ f)
  where subs1 = [ [(1,[-2])], [(1,[1])], [(1,[0,1])], [(1,[])] ] -- l -> t^(-1), q -> t, z -> z
        subs2 = [ [(1,[1])], [(1,[1]),(1,[-1])] ] -- t -> t, z -> t^(1/2) - t^(-1/2)

lookup :: CF -> IO Poly
lookup cf = subst_from_HOMFLY <$> HOMFLY.lookup cf
