module Specialization.Alexander where

import Prelude hiding (lookup)

import Data.ContinuedFraction
import Data.Polynomial
import Data.Poset

import qualified Specialization.HOMFLY as HOMFLY
import Specialization.HOMFLY (m)


vars :: PolyVars
vars = [('t',2)]

subst_from_HOMFLY :: Poly -> Poly
subst_from_HOMFLY (Poly _ f) = Poly vars (subst subs2 . subst subs1 $ f)
  where subs1 = [ [(1,[])], [(1,[1])], [(1,[0,1])], [(-1,[2])] ] -- l -> 1, q -> t, z -> z
        subs2 = [ [(1,[1])], [(1,[1]),(1,[-1])] ] -- t -> t, z -> t^(1/2) - t^(-1/2)

spec :: FPolySpecialization
spec = (vars, \i -> [(-1,[2*(-1)^i])])

-- the HOMFLY-specialized F-polynomial of the even continued fraction [b1,...,bn]
sF :: CF -> Poly
sF cf = ensureEven cf $ specialize spec (ideals (poset cf))

msF :: CF -> Poly
msF cf = mult (raw (subst_from_HOMFLY (m cf))) <$> sF cf


lookup :: CF -> IO Poly
lookup cf = subst_from_HOMFLY <$> HOMFLY.lookup cf
