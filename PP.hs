{-#LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleInstances, TupleSections, FlexibleContexts, AllowAmbiguousTypes, ConstrainedClassMethods #-}
module PP( module PP, module Data.Int, module Data.Bits, module Data.Ord, module Data.Char, module Data.Maybe, module Data.List, module Data.List.Split, module Control.Arrow, module Control.Applicative, module Data.Function) where

import Debug.Trace
import Data.Int
import Data.Bits
import Data.Ord
import Data.Char
import Data.Function
import Data.Maybe
import Data.List
import Data.List.Split
import Control.Arrow
import Control.Applicative
import qualified Data.Set as S
import qualified Data.IntMap as IM

between :: Ord a => a -> a -> a -> Bool
between x1 x2 x = case compare x1 x2 of
                    EQ -> x == x1
                    LT -> x >= x1 && x <= x2
                    GT -> x <= x1 && x >= x2

strictlyBetween :: Ord a => a -> a -> a -> Bool
strictlyBetween x1 x2 x = case compare x1 x2 of
                            EQ -> False
                            LT -> x > x1 && x < x2
                            GT -> x < x1 && x > x2

newtype Show' = Show' String
instance Show Show' where
  show (Show' s) = s

newtype FL = FindLetters [[Bool]]
instance Show FL where
  show (FindLetters xs) = findLetters xs

findLetters :: [[Bool]] -> String
findLetters ss = findLetters' ss ss
  where
    findLetters' _ [] = ""
    findLetters' ss0 ss = if length ss0 < 6
      then ""
      else
        case testpos ss of
          Nothing ->
            if (maximum (map length ss) >= 4)
              then
                findLetters' ss0 (map tail ss)
              else
                  let ss0' = tail ss0 in     findLetters' ss0' ss0'
          Just c  ->
            if (maximum (map length ss) >= 9)
              then
                c : findLetters' ss0 (map (drop 4) ss)
              else
                  let ss0' = tail ss0 in c : findLetters' ss0' ss0'
    letters = IM.fromList [(6922137 , 'A'), (6916246, 'C'), (16312456, 'F'), (10144425, 'K'), (15310472, 'P'), (15310505, 'R'), (10066326, 'U'), (8933922, 'Y'), (15803535, 'Z')]
    take' n xs = zipWith const (xs ++ repeat False) [1..n]
    mkHash ss = foldl (\a x -> a * 2 + (if x then 1 else 0)) 0 $ concat $ take 6 $ map (take' 4) ss
    testpos ss = IM.lookup (mkHash ss) letters

splitBy c = filter (/=[]) . linesBy (==c)

findAll :: Read a => String -> [a]
findAll = map read . filter (/=[]) . linesBy notIsNum
  where
    notIsNum x = not (isDigit x || x == '-')

interact' :: Show b => ([a] -> b) -> (String -> a) -> IO ()
interact' f r = interact $ (++"\n") . show . f . map r . lines

interact1 :: Show b => (String -> b) -> IO ()
interact1 f = interact $ (++"\n") . show . f . concat . lines

interactBy :: Show b => (Char -> Bool) -> ([String] -> b) -> IO ()
interactBy p f = interact $ (++"\n") . show . f . linesBy p

takeUntilDuplicate :: Ord a => [a] -> [a]
takeUntilDuplicate xs = foldr go (const []) xs S.empty
  where
    go x cont set
      | x `S.member` set = []
      | otherwise        = x:cont (S.insert x set)

takeUntilDuplicateOn :: Ord b => (a -> b) -> [a] -> [a]
takeUntilDuplicateOn p xs = foldr (go p) (const []) xs S.empty
  where
    go p x cont set
      | (p x) `S.member` set = []
      | otherwise            = x:cont (S.insert (p x) set)

mapSize :: [String] -> V2i
mapSize = maxX &&& length
  where
    maxX []    = 0
    maxX (s:_) = length s

mapCoordsBy :: (a -> Bool) -> [[a]] -> [V2i]
mapCoordsBy p ss = concat $ zipWith (\y line -> map (,y) line) [0..] $ map doLine ss
  where
    doLine xs  = concatMap maybeToList $ zipWith (\x y -> if p x then Just y else Nothing) xs [0..]

mapCoords :: [[Bool]] -> [V2i]
mapCoords = mapCoordsBy id

mapCoords' :: Char -> [String] -> [V2i]
mapCoords' c = mapCoordsBy (==c)

coordsManhatten :: [V2i]
coordsManhatten = coordsManhatten' 0 0
  where
    coordsManhatten' 0 0 = (0,0):coordsManhatten' 1 0
    coordsManhatten' 0 n = (0,-n):(0,n):coordsManhatten' (n+1) 0
    coordsManhatten' n 0 = (-n,0):(n,0):coordsManhatten' (n-1) 1
    coordsManhatten' n m = (-n,-m):(-n,m):(n,-m):(n,m):coordsManhatten' (n-1) (m+1)

type (V2 a) = (a, a)
type (V3 a) = (a, a, a)
type (V4 a) = (a, a, a, a)
type (V5 a) = (a, a, a, a, a)
type (V6 a) = (a, a, a, a, a, a)
type (V7 a) = (a, a, a, a, a, a, a)
type (V8 a) = (a, a, a, a, a, a, a, a)
type (V9 a) = (a, a, a, a, a, a, a, a, a)
type (V10 a) = (a, a, a, a, a, a, a, a, a, a)
type (V11 a) = (a, a, a, a, a, a, a, a, a, a, a)
type (V12 a) = (a, a, a, a, a, a, a, a, a, a, a, a)

type V2i = V2 Int
type V3i = V3 Int
type V4i = V4 Int
type V5i = V5 Int
type V6i = V6 Int
type V7i = V7 Int
type V8i = V8 Int
type V9i = V9 Int
type V10i = V10 Int
type V11i = V11 Int
type V12i = V12 Int

type family Tup v
type instance Tup (V2 a) = a
type instance Tup (V3 a) = a
type instance Tup (V4 a) = a
type instance Tup (V5 a) = a
type instance Tup (V6 a) = a
type instance Tup (V7 a) = a
type instance Tup (V8 a) = a
type instance Tup (V9 a) = a
type instance Tup (V10 a) = a
type instance Tup (V11 a) = a
type instance Tup (V12 a) = a

type family Tup1 v
type instance Tup1 (a, _) = a
type instance Tup1 (a, _, _) = a
type instance Tup1 (a, _, _, _) = a
type instance Tup1 (a, _, _, _, _) = a
type instance Tup1 (a, _, _, _, _, _) = a
type instance Tup1 (a, _, _, _, _, _, _) = a
type instance Tup1 (a, _, _, _, _, _, _, _) = a
type instance Tup1 (a, _, _, _, _, _, _, _, _) = a
type instance Tup1 (a, _, _, _, _, _, _, _, _, _) = a
type instance Tup1 (a, _, _, _, _, _, _, _, _, _, _) = a
type instance Tup1 (a, _, _, _, _, _, _, _, _, _, _, _) = a

type family Tup2 v
type instance Tup2 (_, a) = a
type instance Tup2 (_, a, _) = a
type instance Tup2 (_, a, _, _) = a
type instance Tup2 (_, a, _, _, _) = a
type instance Tup2 (_, a, _, _, _, _) = a
type instance Tup2 (_, a, _, _, _, _, _) = a
type instance Tup2 (_, a, _, _, _, _, _, _) = a
type instance Tup2 (_, a, _, _, _, _, _, _, _) = a
type instance Tup2 (_, a, _, _, _, _, _, _, _, _) = a
type instance Tup2 (_, a, _, _, _, _, _, _, _, _, _) = a
type instance Tup2 (_, a, _, _, _, _, _, _, _, _, _, _) = a

type family Tup3 v
type instance Tup3 (_, _, a) = a
type instance Tup3 (_, _, a, _) = a
type instance Tup3 (_, _, a, _, _) = a
type instance Tup3 (_, _, a, _, _, _) = a
type instance Tup3 (_, _, a, _, _, _, _) = a
type instance Tup3 (_, _, a, _, _, _, _, _) = a
type instance Tup3 (_, _, a, _, _, _, _, _, _) = a
type instance Tup3 (_, _, a, _, _, _, _, _, _, _) = a
type instance Tup3 (_, _, a, _, _, _, _, _, _, _, _) = a
type instance Tup3 (_, _, a, _, _, _, _, _, _, _, _, _) = a

type family Tup4 v
type instance Tup4 (_, _, _, a) = a
type instance Tup4 (_, _, _, a, _) = a
type instance Tup4 (_, _, _, a, _, _) = a
type instance Tup4 (_, _, _, a, _, _, _) = a
type instance Tup4 (_, _, _, a, _, _, _, _) = a
type instance Tup4 (_, _, _, a, _, _, _, _, _) = a
type instance Tup4 (_, _, _, a, _, _, _, _, _, _) = a
type instance Tup4 (_, _, _, a, _, _, _, _, _, _, _) = a
type instance Tup4 (_, _, _, a, _, _, _, _, _, _, _, _) = a

type family Tup5 v
type instance Tup5 (_, _, _, _, a) = a
type instance Tup5 (_, _, _, _, a, _) = a
type instance Tup5 (_, _, _, _, a, _, _) = a
type instance Tup5 (_, _, _, _, a, _, _, _) = a
type instance Tup5 (_, _, _, _, a, _, _, _, _) = a
type instance Tup5 (_, _, _, _, a, _, _, _, _, _) = a
type instance Tup5 (_, _, _, _, a, _, _, _, _, _, _) = a
type instance Tup5 (_, _, _, _, a, _, _, _, _, _, _, _) = a

type family Tup6 v
type instance Tup6 (_, _, _, _, _, a) = a
type instance Tup6 (_, _, _, _, _, a, _) = a
type instance Tup6 (_, _, _, _, _, a, _, _) = a
type instance Tup6 (_, _, _, _, _, a, _, _, _) = a
type instance Tup6 (_, _, _, _, _, a, _, _, _, _) = a
type instance Tup6 (_, _, _, _, _, a, _, _, _, _, _) = a
type instance Tup6 (_, _, _, _, _, a, _, _, _, _, _, _) = a

type family Tup7 v
type instance Tup7 (_, _, _, _, _, _, a) = a
type instance Tup7 (_, _, _, _, _, _, a, _) = a
type instance Tup7 (_, _, _, _, _, _, a, _, _) = a
type instance Tup7 (_, _, _, _, _, _, a, _, _, _) = a
type instance Tup7 (_, _, _, _, _, _, a, _, _, _, _) = a
type instance Tup7 (_, _, _, _, _, _, a, _, _, _, _, _) = a

type family Tup8 v
type instance Tup8 (_, _, _, _, _, _, _, a) = a
type instance Tup8 (_, _, _, _, _, _, _, a, _) = a
type instance Tup8 (_, _, _, _, _, _, _, a, _, _) = a
type instance Tup8 (_, _, _, _, _, _, _, a, _, _, _) = a
type instance Tup8 (_, _, _, _, _, _, _, a, _, _, _, _) = a

type family Tup9 v
type instance Tup9 (_, _, _, _, _, _, _, _, a) = a
type instance Tup9 (_, _, _, _, _, _, _, _, a, _) = a
type instance Tup9 (_, _, _, _, _, _, _, _, a, _, _) = a
type instance Tup9 (_, _, _, _, _, _, _, _, a, _, _, _) = a

type family Tup10 v
type instance Tup10 (_, _, _, _, _, _, _, _, _, a) = a
type instance Tup10 (_, _, _, _, _, _, _, _, _, a, _) = a
type instance Tup10 (_, _, _, _, _, _, _, _, _, a, _, _) = a

type family Tup11 v
type instance Tup11 (_, _, _, _, _, _, _, _, _, _, a) = a
type instance Tup11 (_, _, _, _, _, _, _, _, _, _, a, _) = a

type family Tup12 v
type instance Tup12 (_, _, _, _, _, _, _, _, _, _, _, a) = a

type instance Tup (a, b) = a

class Num (Tup v) => Dist v where
  manhattan :: v -> v -> Tup v
  dist2 :: v -> v -> Tup v -- the square of the Pythagorean distance
  dist :: (Real (Tup v), Floating c) => v -> v -> c
  dist z1 z2 = sqrt $ realToFrac $ dist2 z1 z2

class ToList v where
  toList :: v -> [Tup v]
class FromList v where
  fromList :: [Tup v] -> v

class T1 v where t1 :: v -> Tup1 v
class T2 v where t2 :: v -> Tup2 v
class T3 v where t3 :: v -> Tup3 v
class T4 v where t4 :: v -> Tup4 v
class T5 v where t5 :: v -> Tup5 v
class T6 v where t6 :: v -> Tup6 v
class T7 v where t7 :: v -> Tup7 v
class T8 v where t8 :: v -> Tup8 v
class T9 v where t9 :: v -> Tup9 v
class T10 v where t10 :: v -> Tup10 v
class T11 v where t11 :: v -> Tup11 v
class T12 v where t12 :: v -> Tup12 v

instance (Num a1, a1 ~ a2) => Dist (a1, a2) where
  manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
  dist2 (x1, y1) (x2, y2)   = let xd = x2 - x1; yd =  y2 - y1 in xd * xd + yd * yd
instance T1 (a1, a2) where
  t1 (x, _) = x
instance T2 (a1, a2) where
  t2 (_, x) = x
instance a1 ~ a2 => ToList (a1, a2) where
  toList (x1, x2) = [x1, x2]
instance a1 ~ a2 => FromList (a1, a2) where
  fromList [x1, x2] = (x1, x2)

instance (Num a1, a1 ~ a2, a2 ~ a3) => Dist (a1, a2, a3) where
  manhattan (x1, y1, z1) (x2, y2, z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (x1, y1, z1) (x2, y2, z2)   = let xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in xd * xd + yd * yd + zd * zd
instance T1 (a1, a2, a3) where
  t1 (x, _, _) = x
instance T2 (a1, a2, a3) where
  t2 (_, x, _) = x
instance T3 (a1, a2, a3) where
  t3 (_, _, x) = x
instance (a1 ~ a2, a2 ~ a3) => ToList (a1, a2, a3) where
  toList (x1, x2, x3) = [x1, x2, x3]
instance (a1 ~ a2, a2 ~ a3) => FromList (a1, a2, a3) where
  fromList [x1, x2, x3] = (x1, x2, x3)

instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4) => Dist (a1, a2, a3, a4) where
  manhattan (w1, x1, y1, z1) (w2, x2, y2, z2) = abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (w1, x1, y1, z1) (w2, x2, y2, z2)   = let wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in wd * wd + xd * xd + yd * yd + zd * zd
instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4) => T1 (a1, a2, a3, a4) where
  t1 (x, _, _, _) = x
instance T2 (a1, a2, a3, a4) where
  t2 (_, x, _, _) = x
instance T3 (a1, a2, a3, a4) where
  t3 (_, _, x, _) = x
instance T4 (a1, a2, a3, a4) where
  t4 (_, _, _, x) = x
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4) => ToList (a1, a2, a3, a4) where
  toList (x1, x2, x3, x4) = [x1, x2, x3, x4]
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4) => FromList (a1, a2, a3, a4) where
  fromList [x1, x2, x3, x4] = (x1, x2, x3, x4)

instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5) => Dist (a1, a2, a3, a4, a5) where
  manhattan (v1, w1, x1, y1, z1) (v2, w2, x2, y2, z2) = abs (v2 - v1) + abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (v1, w1, x1, y1, z1) (v2, w2, x2, y2, z2)   = let vd = v2 - v1; wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in vd * vd + wd * wd + xd * xd + yd * yd + zd * zd
instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5) => T1 (a1, a2, a3, a4, a5) where
  t1 (x, _, _, _, _) = x
instance T2 (a1, a2, a3, a4, a5) where
  t2 (_, x, _, _, _) = x
instance T3 (a1, a2, a3, a4, a5) where
  t3 (_, _, x, _, _) = x
instance T4 (a1, a2, a3, a4, a5) where
  t4 (_, _, _, x, _) = x
instance T5 (a1, a2, a3, a4, a5) where
  t5 (_, _, _, _, x) = x
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5) => ToList (a1, a2, a3, a4, a5) where
  toList (x1, x2, x3, x4, x5) = [x1, x2, x3, x4, x5]
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5) => FromList (a1, a2, a3, a4, a5) where
  fromList [x1, x2, x3, x4, x5] = (x1, x2, x3, x4, x5)

instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6) => Dist (a1, a2, a3, a4, a5, a6) where
  manhattan (u1, v1, w1, x1, y1, z1) (u2, v2, w2, x2, y2, z2) = abs (u2 - u1) + abs (v2 - v1) + abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (u1, v1, w1, x1, y1, z1) (u2, v2, w2, x2, y2, z2)   = let ud = u2 - u1; vd = v2 - v1; wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in ud * ud + vd * vd + wd * wd + xd * xd + yd * yd + zd * zd
instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6) => T1 (a1, a2, a3, a4, a5, a6) where
  t1 (x, _, _, _, _, _) = x
instance T2 (a1, a2, a3, a4, a5, a6) where
  t2 (_, x, _, _, _, _) = x
instance T3 (a1, a2, a3, a4, a5, a6) where
  t3 (_, _, x, _, _, _) = x
instance T4 (a1, a2, a3, a4, a5, a6) where
  t4 (_, _, _, x, _, _) = x
instance T5 (a1, a2, a3, a4, a5, a6) where
  t5 (_, _, _, _, x, _) = x
instance T6 (a1, a2, a3, a4, a5, a6) where
  t6 (_, _, _, _, _, x) = x
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6) => ToList (a1, a2, a3, a4, a5, a6) where
  toList (x1, x2, x3, x4, x5, x6) = [x1, x2, x3, x4, x5, x6]
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6) => FromList (a1, a2, a3, a4, a5, a6) where
  fromList [x1, x2, x3, x4, x5, x6] = (x1, x2, x3, x4, x5, x6)

instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7) => Dist (a1, a2, a3, a4, a5, a6, a7) where
  manhattan (t1, u1, v1, w1, x1, y1, z1) (t2, u2, v2, w2, x2, y2, z2) = abs (t2 - t1) + abs (u2 - u1) + abs (v2 - v1) + abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (t1, u1, v1, w1, x1, y1, z1) (t2, u2, v2, w2, x2, y2, z2)   = let td = t2 - t1; ud = u2 - u1; vd = v2 - v1; wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in td * td + ud * ud + vd * vd + wd * wd + xd * xd + yd * yd + zd * zd
instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7) => T1 (a1, a2, a3, a4, a5, a6, a7) where
  t1 (x, _, _, _, _, _, _) = x
instance T2 (a1, a2, a3, a4, a5, a6, a7) where
  t2 (_, x, _, _, _, _, _) = x
instance T3 (a1, a2, a3, a4, a5, a6, a7) where
  t3 (_, _, x, _, _, _, _) = x
instance T4 (a1, a2, a3, a4, a5, a6, a7) where
  t4 (_, _, _, x, _, _, _) = x
instance T5 (a1, a2, a3, a4, a5, a6, a7) where
  t5 (_, _, _, _, x, _, _) = x
instance T6 (a1, a2, a3, a4, a5, a6, a7) where
  t6 (_, _, _, _, _, x, _) = x
instance T7 (a1, a2, a3, a4, a5, a6, a7) where
  t7 (_, _, _, _, _, _, x) = x
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7) => ToList (a1, a2, a3, a4, a5, a6, a7) where
  toList (x1, x2, x3, x4, x5, x6, x7) = [x1, x2, x3, x4, x5, x6, x7]
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7) => FromList (a1, a2, a3, a4, a5, a6, a7) where
  fromList [x1, x2, x3, x4, x5, x6, x7] = (x1, x2, x3, x4, x5, x6, x7)

instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8) => Dist (a1, a2, a3, a4, a5, a6, a7, a8) where
  manhattan (s1, t1, u1, v1, w1, x1, y1, z1) (s2, t2, u2, v2, w2, x2, y2, z2) = abs (s2 - s1) + abs (t2 - t1) + abs (u2 - u1) + abs (v2 - v1) + abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (s1, t1, u1, v1, w1, x1, y1, z1) (s2, t2, u2, v2, w2, x2, y2, z2)   = let sd = s2 - s1; td = t2 - t1; ud = u2 - u1; vd = v2 - v1; wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in sd * sd + td * td + ud * ud + vd * vd + wd * wd + xd * xd + yd * yd + zd * zd
instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8) => T1 (a1, a2, a3, a4, a5, a6, a7, a8) where
  t1 (x, _, _, _, _, _, _, _) = x
instance T2 (a1, a2, a3, a4, a5, a6, a7, a8) where
  t2 (_, x, _, _, _, _, _, _) = x
instance T3 (a1, a2, a3, a4, a5, a6, a7, a8) where
  t3 (_, _, x, _, _, _, _, _) = x
instance T4 (a1, a2, a3, a4, a5, a6, a7, a8) where
  t4 (_, _, _, x, _, _, _, _) = x
instance T5 (a1, a2, a3, a4, a5, a6, a7, a8) where
  t5 (_, _, _, _, x, _, _, _) = x
instance T6 (a1, a2, a3, a4, a5, a6, a7, a8) where
  t6 (_, _, _, _, _, x, _, _) = x
instance T7 (a1, a2, a3, a4, a5, a6, a7, a8) where
  t7 (_, _, _, _, _, _, x, _) = x
instance T8 (a1, a2, a3, a4, a5, a6, a7, a8) where
  t8 (_, _, _, _, _, _, _, x) = x
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8) => ToList (a1, a2, a3, a4, a5, a6, a7, a8) where
  toList (x1, x2, x3, x4, x5, x6, x7, x8) = [x1, x2, x3, x4, x5, x6, x7, x8]
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8) => FromList (a1, a2, a3, a4, a5, a6, a7, a8) where
  fromList [x1, x2, x3, x4, x5, x6, x7, x8] = (x1, x2, x3, x4, x5, x6, x7, x8)

instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9) => Dist (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  manhattan (r1, s1, t1, u1, v1, w1, x1, y1, z1) (r2, s2, t2, u2, v2, w2, x2, y2, z2) = abs (r2 - r1) + abs (s2 - s1) + abs (t2 - t1) + abs (u2 - u1) + abs (v2 - v1) + abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (r1, s1, t1, u1, v1, w1, x1, y1, z1) (r2, s2, t2, u2, v2, w2, x2, y2, z2)   = let rd = r2 - r1; sd = s2 - s1; td = t2 - t1; ud = u2 - u1; vd = v2 - v1; wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in rd * rd + sd * sd + td * td + ud * ud + vd * vd + wd * wd + xd * xd + yd * yd + zd * zd
instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9) => T1 (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  t1 (x, _, _, _, _, _, _, _, _) = x
instance T2 (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  t2 (_, x, _, _, _, _, _, _, _) = x
instance T3 (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  t3 (_, _, x, _, _, _, _, _, _) = x
instance T4 (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  t4 (_, _, _, x, _, _, _, _, _) = x
instance T5 (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  t5 (_, _, _, _, x, _, _, _, _) = x
instance T6 (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  t6 (_, _, _, _, _, x, _, _, _) = x
instance T7 (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  t7 (_, _, _, _, _, _, x, _, _) = x
instance T8 (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  t8 (_, _, _, _, _, _, _, x, _) = x
instance T9 (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  t9 (_, _, _, _, _, _, _, _, x) = x
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9) => ToList (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  toList (x1, x2, x3, x4, x5, x6, x7, x8, x9) = [x1, x2, x3, x4, x5, x6, x7, x8, x9]
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9) => FromList (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  fromList [x1, x2, x3, x4, x5, x6, x7, x8, x9] = (x1, x2, x3, x4, x5, x6, x7, x8, x9)

instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9, a9 ~ a10) => T1 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  t1 (x, _, _, _, _, _, _, _, _, _) = x
instance T2 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  t2 (_, x, _, _, _, _, _, _, _, _) = x
instance T3 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  t3 (_, _, x, _, _, _, _, _, _, _) = x
instance T4 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  t4 (_, _, _, x, _, _, _, _, _, _) = x
instance T5 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  t5 (_, _, _, _, x, _, _, _, _, _) = x
instance T6 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  t6 (_, _, _, _, _, x, _, _, _, _) = x
instance T7 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  t7 (_, _, _, _, _, _, x, _, _, _) = x
instance T8 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  t8 (_, _, _, _, _, _, _, x, _, _) = x
instance T9 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  t9 (_, _, _, _, _, _, _, _, x, _) = x
instance T10 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  t10 (_, _, _, _, _, _, _, _, _, x) = x
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9, a9 ~ a10) => ToList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  toList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10]
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9, a9 ~ a10) => FromList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  fromList [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9, a9 ~ a10, a10 ~ a11) => T1 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  t1 (x, _, _, _, _, _, _, _, _, _, _) = x
instance T2 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  t2 (_, x, _, _, _, _, _, _, _, _, _) = x
instance T3 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  t3 (_, _, x, _, _, _, _, _, _, _, _) = x
instance T4 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  t4 (_, _, _, x, _, _, _, _, _, _, _) = x
instance T5 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  t5 (_, _, _, _, x, _, _, _, _, _, _) = x
instance T6 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  t6 (_, _, _, _, _, x, _, _, _, _, _) = x
instance T7 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  t7 (_, _, _, _, _, _, x, _, _, _, _) = x
instance T8 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  t8 (_, _, _, _, _, _, _, x, _, _, _) = x
instance T9 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  t9 (_, _, _, _, _, _, _, _, x, _, _) = x
instance T10 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  t10 (_, _, _, _, _, _, _, _, _, x, _) = x
instance T11 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  t11 (_, _, _, _, _, _, _, _, _, _, x) = x
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9, a9 ~ a10, a10 ~ a11) => ToList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  toList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11]
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9, a9 ~ a10, a10 ~ a11) => FromList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
  fromList [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)

instance (Num a1, a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9, a9 ~ a10, a10 ~ a11, a11 ~ a12) => T1 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t1 (x, _, _, _, _, _, _, _, _, _, _, _) = x
instance T2 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t2 (_, x, _, _, _, _, _, _, _, _, _, _) = x
instance T3 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t3 (_, _, x, _, _, _, _, _, _, _, _, _) = x
instance T4 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t4 (_, _, _, x, _, _, _, _, _, _, _, _) = x
instance T5 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t5 (_, _, _, _, x, _, _, _, _, _, _, _) = x
instance T6 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t6 (_, _, _, _, _, x, _, _, _, _, _, _) = x
instance T7 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t7 (_, _, _, _, _, _, x, _, _, _, _, _) = x
instance T8 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t8 (_, _, _, _, _, _, _, x, _, _, _, _) = x
instance T9 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t9 (_, _, _, _, _, _, _, _, x, _, _, _) = x
instance T10 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t10 (_, _, _, _, _, _, _, _, _, x, _, _) = x
instance T11 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t11 (_, _, _, _, _, _, _, _, _, _, x, _) = x
instance T12 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  t12 (_, _, _, _, _, _, _, _, _, _, _, x) = x
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9, a9 ~ a10, a10 ~ a11, a11 ~ a12) => ToList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  toList (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12]
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7, a7 ~ a8, a8 ~ a9, a9 ~ a10, a10 ~ a11, a11 ~ a12) => FromList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
  fromList [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)

-- Data.Composition

(∘) :: (b -> c) -> (a -> b) -> a -> c
(∘) = (.)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) = (.) . (.:)

(.::) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.::) = (.) . (.:.)

infixr 9 ∘
infixr 8 .:
infixr 8 .:.
infixr 8 .::
