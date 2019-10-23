module Specialization.HOMFLY where

import Prelude hiding (Rational,lookup)
import System.IO (openFile, IOMode(..), hGetContents, hClose)
import Data.Maybe (mapMaybe)
import Data.List (foldl')

import Data.RatioInfty
import Data.ContinuedFraction
import Data.Polynomial
import Data.Poset

head0 :: [Int] -> Int
head0 [] = 0
head0 (x:_) = x


-- The HOMFLY F-Polynomial specialization

vars_lq :: PolyVars
vars_lq = [('l',1),('q',2),('z',1),('w',1)]
-- where z = q^(1/2) - q^(-1/2), w = (1 - l^2q)/(1 - q^(-1))

spec :: Int -> FPolySpecialization
spec b1 = (vars_lq, fP)
  where fP :: Int -> RawPoly
        fP 1 | b1 > 0 = [(1,[2,0,0,-1])]
             | b1 < 0 = [(1,[0,-4,0,1])]
        fP i | even i = [(-1,[2,2])]
             | odd i  = [(-1,[0,-2])]

-- the HOMFLY-specialized F-polynomial of the even continued fraction [b1,...,bn]
sF :: CF -> Poly
sF cf = ensureEven cf $ specialize (spec (head0 cf)) (ideals (poset cf))

m :: CF -> Poly
m cf = Poly vars_lq [fst $ foldl' go ((1,[]),Nothing) (zip (typ cf) cf)]
  where go :: (Monomial, Maybe Sgn) -> (Sgn, Int) -> (Monomial, Maybe Sgn)
        go (_, Nothing) (Mn,b1) = ((-1,[1,1])               , Just Mn)
        go (_, Nothing) (Pl,b1) = ((-1,[-(abs b1)-1,-1,0,1]), Just Pl)
        go (mon, Just _ ) (Mn,bn) = ((-1,[1,1])            `multMono` mon, Just Mn)
        go (mon, Just Mn) (Pl,bn) = ((-1,[-(abs bn)-1,-1]) `multMono` mon, Just Pl)
        go (mon, Just Pl) (Pl,bn) = ((1,[1-(abs bn),1])    `multMono` mon, Just Pl)

-- replaces all instances of 'w' with one of two equivalent expressions involving 'z^(-1)'
sub_w :: Int -> RawPoly -> RawPoly
sub_w b1 = subst [ [(1,[1])], [(1,[0,1])], [(1,[0,0,1])]
                 , if b1 > 0 then -- w = (1 - l^2) q^(1/2) z^(-1) - l^2q   (5.3)
                                  [(1,[0,1,-1]),(-1,[2,1,-1]),(-1,[2,2])]
                             else -- w = (1 - l^2) q^(3/2) z^(-1) - q      (5.5) x q^2
                                  [(1,[0,3,-1]),(-1,[2,3,-1]),(-1,[0,2])] ]

mult_sub_w :: Int -> Poly -> Poly -> Poly
mult_sub_w b1 f = fmap (sub_w b1 . mult (raw f))

-- m[b1,...,bn] times the HOMFLY-specialized F-polynomial of [b1,...,bn]
msF :: CF -> Poly
msF cf = mult_sub_w (head0 cf) (m cf) (sF cf)


-- Looking up the actual HOMFLY Polynomial, from [DS15]

vars_za :: PolyVars
vars_za = [('z',1),('a',1)]

za_to_lq :: RawPoly -> Poly
za_to_lq = Poly vars_lq . subst subs . map cullNegZs
  where subs = [ [(1,[0,1]),(-1,[0,-1])] -- z^k for k > 0 maps to (q^(1/2) - q^(-1/2))^k
               , [(1,[1])]               -- a^k maps to l^k
               , [(1,[0,0,1])]]          -- z^k for k < 0 maps to z^k (in vsHOMFLY)
        -- Moves all instaces of z^(-k) to a third variable
        cullNegZs :: Monomial -> Monomial
        cullNegZs (c,[])    = (c,[])
        cullNegZs (c,[z])   = if z > 0 then (c,[z]) else (c,[0,0,z])
        cullNegZs (c,[z,a]) = if z > 0 then (c,[z,a]) else (c,[0,a,z])
        cullNegZs (_,_) = error "too many variables?"

lookup_za :: Rational -> IO Poly
lookup_za q = do
  file <- openFile "homfly200c.txt" ReadMode
  ls <- map words . lines <$> hGetContents file
  let qstr  = show (numerator (recip (-q))) ++ "/" ++ show (denominator (recip (-q)))
      -- qstr' = show (numerator (recip (-(invol q)))) ++ "/" ++ show (denominator (recip (-(invol q))))
  let ls' = dropWhile (\ws -> not (ws == [qstr])) ls -- || ws == [qstr']
      ls'' = takeWhile (\ws -> not ('/' `elem` (head ws))) (tail ls')
      poly = mapMaybe reduceOne . concatMap interpretLine $! ls''
  seq poly (return ())
  hClose file
  return (Poly vars_za poly)
  where interpretLine :: [String] -> RawPoly
        interpretLine (kstr:mstr:cstrs) = map (\(c,i) -> (c,[k,m+2*i])) (zip (map read cstrs) [0..])
          where (k,m) = (read kstr, read mstr)
        interpretLine l = error $ "malformed line: " ++ show l

-- looks up the HOMFLY polynomial of an even continued fraction
lookup :: CF -> IO Poly
lookup cf = za_to_lq . raw <$> lookup_za (quo (toEven cf))
