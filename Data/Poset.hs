module Data.Poset where

import Prelude hiding (Rational)
import Data.List (foldl', transpose, mapAccumL, concatMap, intercalate, sort, sortOn)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.RatioInfty
import Data.ContinuedFraction
import Data.Polynomial

deleteNth :: Int -> [a] -> [a]
deleteNth n xs = let (a, b) = splitAt n xs
                  in a ++ tail b

applyToHead :: (a -> a) -> [a] -> [a]
applyToHead _ [] = []
applyToHead f (x:xs) = (f x):xs


-- An adjacency 'matrix' of a poset, where if `adj :: PosetAdj` then `adj !! i` is the
--  list of indices in [0,length adj) less than `i` in the poset
type PosetAdj = [[Int]]

-- A poset is a list of adjacencies and a list of labels (assume length adj == length labels)
data Poset = Poset { adj :: PosetAdj, labels :: [Int] }


-- The quiver associated to a sign sequence of a positive continued fraction
makeQuiver :: SignSeq -> [[(Int,Sgn)]]
makeQuiver [] = []
makeQuiver [Pl] = []
makeQuiver ss = reverse . snd $ foldl' go (0,[[]]) (init . tail $ ss)
  where go :: (Int, [[(Int,Sgn)]]) -> Sgn -> (Int, [[(Int,Sgn)]])
        go (i,qvr) s = (i+1, [(i, neg s)] : (applyToHead ((i+1, s):) qvr))

quiverToPosetAdj :: [[(Int,Sgn)]] -> [[Int]]
quiverToPosetAdj = map (mapMaybe (\(i,s) -> if s == Mn then Just i else Nothing))

-- The poset of a continued fraction with absolute value |[c1,...,cn]| >= 1
poset :: CF -> Poset
poset cf = Poset (map sort . quiverToPosetAdj . makeQuiver $ sgn (toPos cf)) labels
    where labels :: [Int]
          labels = concat . snd $ mapAccumL go ((Mn,0),0) (zip (typ cf) cf)
          go :: ((Sgn, Int), Int) -> (Sgn, Int) -> (((Sgn, Int), Int), [Int])
          go ((tlast, clast), l) (t, c) = (((t, c), l + abs c), conn ++ [(l+1)..(l + abs c - 1)])
            where conn = if tlast /= t && l > 0 then [l]
                         else if l == 0 || (abs c > 1 && abs clast > 1) then []
                              else error "Continued fraction does not satisfy |p/q| >= 1"

-- A general (but inefficient) recursive algorithm for computing the order ideals of any poset

minimums :: PosetAdj -> [Int]
minimums ps = map snd (filter (null . fst) (zip ps [0..]))

removeFromPoset :: Int -> PosetAdj -> PosetAdj
removeFromPoset i ps = deleteNth i (map (mapMaybe f) ps)
  where f :: Int -> Maybe Int
        f j | j > i     = Just (j - 1)
            | j == i    = Nothing
            | otherwise = Just j

idealsSet :: PosetAdj -> Set (Set Int)
idealsSet [] = Set.insert Set.empty Set.empty
idealsSet ps = Set.insert Set.empty (Set.unions $ map (\i -> Set.map (toEachIdeal i) (idealsSet (removeFromPoset i ps))) (minimums ps))
  where toEachIdeal :: Int -> Set Int -> Set Int
        toEachIdeal i idl = Set.insert i (Set.map (unremoveIdx i) idl)
        unremoveIdx :: Int -> Int -> Int
        unremoveIdx i j | j >= i    = j + 1
                        | otherwise = j

type OrderIdeal = [Int]

ideals :: Poset -> [OrderIdeal]
ideals ps = map (map (\i -> labels ps !! i)) . sortOn length . Set.toList . Set.map Set.toList $ idealsSet (adj ps)


-- F-polynomial specializations

type FPolySpecialization = (PolyVars, Int -> RawPoly)

specialize :: FPolySpecialization -> [OrderIdeal] -> Poly
specialize (vs,f) fpoly = Poly vs (reduce $ concatMap (foldl' mult [(1,[])] . map f) fpoly)


-- Drawing a path poset

data PosetPart = Up | Down | Vert Int deriving Show

pathPosetToParts :: Poset -> [PosetPart]
pathPosetToParts ps = concatMap go (zip3 (adj ps) (labels ps) [0..])
  where go :: ([Int], Int, Int) -> [PosetPart]
        go (as, lbl, i) | length as == 2 = [Up, Vert lbl, Down]
                        | length as == 1 && head as < i = [Up, Vert lbl]
                        | length as == 1 && head as > i = [Vert lbl, Down]
                        | length as == 0 = [Vert lbl]
                        | otherwise = error "Given poset is not a path poset"

showPosetPrefix :: String -> Poset -> String
showPosetPrefix prefix ps = intercalate "\n" (map (prefix ++) ls)
  where showPart :: (Int, Int, Int) -> PosetPart -> ((Int, Int, Int), [(Int, Char)])
        showPart (i,mn,mx) (Vert k) = ((i, mn, mx), map ((,) i) (show k))
        showPart (i,mn,mx) Up       = ((i-2, min mn (i-2), mx), [(i-1,'/')])
        showPart (i,mn,mx) Down     = ((i+2, mn, max mx (i+2)), [(i+1,'\\')])

        pass2 :: ((Int, Int, Int), [[(Int, Char)]]) -> (Int, [(Int, Char)])
        pass2 ((_,mn,mx), xs) = (mx-mn, concatMap (map (\(i,c) -> (i-mn,c))) xs)

        putCharAt :: Int -> (Int, Char) -> String
        putCharAt mx (i,c) = replicate i ' ' ++ [c] ++ replicate (mx-i) ' '

        mxps = pass2 (mapAccumL showPart (0,0,0) (pathPosetToParts ps))
        ls = transpose (map (putCharAt (fst mxps)) (snd mxps))

instance Show Poset where
  show = showPosetPrefix []
