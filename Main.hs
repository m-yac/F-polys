module Main where

import Prelude hiding (Rational,exp)
import Data.List (intercalate)

import Data.RatioInfty
import Data.ContinuedFraction
import Data.Poset
import Data.Polynomial

import qualified Specialization.Jones as Jones
import qualified Specialization.HOMFLY as HOMFLY
import qualified Specialization.Alexander as Alexander
import Specialization.HOMFLY (head0, m, sub_w, mult_sub_w)


checkHOMFLY :: CF -> IO ()
checkHOMFLY cfIn = let cf = toEven cfIn in do
  homfly <- HOMFLY.lookup cf
  let ps   = poset cf
      idls = ideals ps
      sF   = specialize (HOMFLY.spec (head0 cf)) idls
      msF  = mult_sub_w (head0 cf) (m cf) sF
  putStrLn $ "Even CF: " ++ show cf ++ " = " ++ show (quo cf)
  putStrLn $ showPosetPrefix " " ps
  putStrLn $ "Ideals: " ++ intercalate ", " (map show idls)
  putStrLn $ "Specialized F-poly:      " ++ show sF
  putStrLn $ "Normalized Spec. F-poly: " ++ show msF
  putStrLn $ "HOMFLY Polynomial:       " ++ show homfly
  putStrLn $ []

checkAlexander :: CF -> IO ()
checkAlexander cfIn = let cf = toEven cfIn in do
  alex <- Alexander.lookup cf
  let ps   = poset cf
      idls = ideals ps
      sF   = specialize Alexander.spec idls
      m'   = Alexander.subst_from_HOMFLY (m cf)
      ltrm = leadingTerm (raw alex)
      msF  = mult (raw m') <$> sF
  putStrLn $ "Even CF: " ++ show cf ++ " = " ++ show (quo cf)
  putStrLn $ showPosetPrefix " " ps
  putStrLn $ "Ideals: " ++ intercalate ", " (map show idls)
  putStrLn $ "Specialized F-poly:      " ++ show sF
  putStrLn $ "Normalized Spec. F-poly: " ++ show (sortPoly <$> msF)
  putStrLn $ "Alexander Polynomial:    " ++ show (sortPoly <$> alex)
  putStrLn $ "(sgn(c0)t^e0 = " ++ show (Poly Alexander.vars [(signum (fst ltrm), snd ltrm)])
               ++ ", m[..] = " ++ show m' ++ ")"
  putStrLn $ []

checkJones :: CF -> IO ()
checkJones cf = do
  jones <- Jones.lookup cf
  let ps   = poset cf
      idls = ideals ps
      sF   = specialize Jones.spec idls
      m'   = Jones.subst_from_HOMFLY (m (toEven cf))
      ltrm = Poly Jones.vars [leadingTerm (raw jones)]
      msF  = mult (raw m') <$> sF
  putStrLn $ "CF: " ++ show cf ++ " = " ++ show (quo cf)
  putStrLn $ showPosetPrefix " " ps
  putStrLn $ "Ideals: " ++ intercalate ", " (map show idls)
  putStrLn $ "Specialized F-poly:      " ++ show sF
  putStrLn $ "Normalized Spec. F-poly: " ++ show (sortPoly <$> msF)
  putStrLn $ "Jones Polynomial:        " ++ show (sortPoly <$> jones)
  putStrLn $ "(c0t^e0 = " ++ show ltrm ++ ", m[..] = " ++ show m' ++ ")"
  putStrLn $ []


main :: IO ()
main = return ()
