module Main where

import Prelude hiding (Rational,Exp)
import System.IO (openFile, IOMode(..), hGetContents, hClose)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

import Data.RatioInfty
import Data.ContinuedFraction
import Data.Poset
import Data.Polynomial
import Specializations


vsHOMFLY_za :: PolyVars
vsHOMFLY_za = [('z',1),('a',1)]

-- looks up the actual HOMFLY polynomial from [DS15], with z/a variables
lookupHOMFLY_za :: Rational -> IO Poly
lookupHOMFLY_za q = do
  file <- openFile "homfly200c.txt" ReadMode
  ls <- map words . lines <$> hGetContents file
  let qstr  = show (numerator (recip (-q))) ++ "/" ++ show (denominator (recip (-q)))
      -- qstr' = show (numerator (recip (-(invol q)))) ++ "/" ++ show (denominator (recip (-(invol q))))
  let ls' = dropWhile (\ws -> not (ws == [qstr])) ls -- || ws == [qstr']
      ls'' = takeWhile (\ws -> not ('/' `elem` (head ws))) (tail ls')
      poly = mapMaybe reduceOne . concatMap interpretLine $! ls''
  seq poly (return ())
  hClose file
  return (Poly vsHOMFLY_za poly)
  where interpretLine :: [String] -> RawPoly
        interpretLine (kstr:mstr:cstrs) = map (\(c,i) -> (c,[k,m+2*i])) (zip (map read cstrs) [0..])
          where (k,m) = (read kstr, read mstr)
        interpretLine l = error $ "malformed line: " ++ show l

vsHOMFLY_za_to_lq :: RawPoly -> RawPoly
vsHOMFLY_za_to_lq = subst subs . map cullNegZs
  where subs = [ [(1,[0,1]),(-1,[0,-1])] -- z^k for k > 0 maps to (q^(1/2) - q^(-1/2))^k
               , [(1,[1])]               -- a^k maps to l^k
               , [(1,[0,0,1])]]          -- z^k for k < 0 maps to z^k (in vsHOMFLY)
        cullNegZs :: Monomial -> Monomial
        cullNegZs (c,[]) = (c,[])
        cullNegZs (c,[z]) = if z > 0 then (c,[z]) else (c,[0,0,z])
        cullNegZs (c,[z,a]) = if z > 0 then (c,[z,a]) else (c,[0,a,z])
        cullNegZs (_,_) = error "too many variables?"

-- looks up the HOMFLY polynomial of an even continued fraction
lookupHOMFLY :: CF -> IO Poly
lookupHOMFLY cf = fmap (Poly vsHOMFLY . vsHOMFLY_za_to_lq . raw) (lookupHOMFLY_za (quo (toEven cf)))


goHOMFLY :: CF -> IO ()
goHOMFLY cf = do
  homfly <- lookupHOMFLY cf
  let ps   = poset cf
      idls = ideals ps
      sF   = sFHOMFLY cf
      msF  = msFHOMFLY cf
  putStrLn $ "Even CF: " ++ show cf ++ " = " ++ show (quo cf)
  putStrLn $ showPosetPrefix " " ps
  putStrLn $ "Ideals: " ++ intercalate ", " (map show idls)
  putStrLn $ "Specialized F-poly:      " ++ show sF
  putStrLn $ "Normalized Spec. F-poly: " ++ show msF
  putStrLn $ "HOMFLY Polynomial:       " ++ show homfly
  putStrLn []

goAlexander :: CF -> IO ()
goAlexander cf = do
  alex <- subst_HOMFLY_to_Alexander <$> lookupHOMFLY cf
  let ps   = poset cf
      idls = ideals ps
      sF   = sFAlexander cf
      msF  = msFAlexander cf
      lmon = head (raw (leadingTerm <$> alex))
  putStrLn $ "Even CF: " ++ show cf ++ " = " ++ show (quo cf)
  putStrLn $ showPosetPrefix " " ps
  putStrLn $ "Ideals: " ++ intercalate ", " (map show idls)
  putStrLn $ "Specialized F-poly:      " ++ show sF
  putStrLn $ "Normalized Spec. F-poly: " ++ show (sortPoly <$> msF)
  putStrLn $ "Alexander Polynomial:    " ++ show (sortPoly <$> alex)
  putStrLn $ "(Leading term: sgn(c0) = " ++ show (signum (fst lmon))
                            ++ ", e0 = " ++ show (head (snd lmon) % 2) ++ ")"
  putStrLn []


main :: IO ()
main = return ()
