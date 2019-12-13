{-#LANGUAGE TypeSynonymInstances, TypeFamilies, FlexibleInstances, TupleSections, FlexibleContexts, AllowAmbiguousTypes #-}
module PP( module PP, module Data.Int, module Data.Ord, module Data.Char, module Data.Maybe, module Data.List, module Data.List.Split, module Control.Arrow) where

import Data.Int
import Data.Ord
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import Control.Arrow
import qualified Data.Set as S

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
type instance Tup (a,a) = a
type instance Tup (a,a,a) = a
type instance Tup (a,a,a,a) = a
type instance Tup (a,a,a,a,a) = a
type instance Tup (a,a,a,a,a,a) = a
type instance Tup (a,a,a,a,a,a,a) = a
type instance Tup (a,a,a,a,a,a,a,a) = a
type instance Tup (a,a,a,a,a,a,a,a,a) = a
type instance Tup (a,a,a,a,a,a,a,a,a,a) = a
type instance Tup (a,a,a,a,a,a,a,a,a,a,a) = a
type instance Tup (a,a,a,a,a,a,a,a,a,a,a,a) = a

class Num (Tup v) => Dist v where
  manDist :: v -> v -> Tup v
  dist2 :: v -> v -> Tup v
  -- dist :: v -> v -> Float
  -- dist z1 z2 = sqrt $ dist2 z1 z2

class T1 v where t1 :: v -> Tup v
class T2 v where t2 :: v -> Tup v
class T3 v where t3 :: v -> Tup v
class T4 v where t4 :: v -> Tup v
class T5 v where t5 :: v -> Tup v
class T6 v where t6 :: v -> Tup v
class T7 v where t7 :: v -> Tup v
class T8 v where t8 :: v -> Tup v
class T9 v where t9 :: v -> Tup v
class T10 v where t10 :: v -> Tup v
class T11 v where t11 :: v -> Tup v
class T12 v where t12 :: v -> Tup v

instance Num a => Dist (V2 a) where
  manDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
  dist2 (x1, y1) (x2, y2)   = let xd = x2 - x1; yd =  y2 - y1 in xd * xd + yd * yd
instance T1 (V2 a) where
  t1 (x, _) = x
instance T2 (V2 a) where
  t2 (_, x) = x

instance Num a => Dist (V3 a) where
  manDist (x1, y1, z1) (x2, y2, z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (x1, y1, z1) (x2, y2, z2)   = let xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in xd * xd + yd * yd + zd * zd
instance T1 (V3 a) where
  t1 (x, _, _) = x
instance T2 (V3 a) where
  t2 (_, x, _) = x
instance T3 (V3 a) where
  t3 (_, _, x) = x

instance Num a => Dist (V4 a) where
  manDist (w1, x1, y1, z1) (w2, x2, y2, z2) = abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (w1, x1, y1, z1) (w2, x2, y2, z2)   = let wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in wd * wd + xd * xd + yd * yd + zd * zd
instance T1 (V4 a) where
  t1 (x, _, _, _) = x
instance T2 (V4 a) where
  t2 (_, x, _, _) = x
instance T3 (V4 a) where
  t3 (_, _, x, _) = x
instance T4 (V4 a) where
  t4 (_, _, _, x) = x

instance Num a => Dist (V5 a) where
  manDist (v1, w1, x1, y1, z1) (v2, w2, x2, y2, z2) = abs (v2 - v1) + abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (v1, w1, x1, y1, z1) (v2, w2, x2, y2, z2)   = let vd = v2 - v1; wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in vd * vd + wd * wd + xd * xd + yd * yd + zd * zd
instance T1 (V5 a) where
  t1 (x, _, _, _, _) = x
instance T2 (V5 a) where
  t2 (_, x, _, _, _) = x
instance T3 (V5 a) where
  t3 (_, _, x, _, _) = x
instance T4 (V5 a) where
  t4 (_, _, _, x, _) = x
instance T5 (V5 a) where
  t5 (_, _, _, _, x) = x

instance Num a => Dist (V6 a) where
  manDist (u1, v1, w1, x1, y1, z1) (u2, v2, w2, x2, y2, z2) = abs (u2 - u1) + abs (v2 - v1) + abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (u1, v1, w1, x1, y1, z1) (u2, v2, w2, x2, y2, z2)   = let ud = u2 - u1; vd = v2 - v1; wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in ud * ud + vd * vd + wd * wd + xd * xd + yd * yd + zd * zd
instance T1 (V6 a) where
  t1 (x, _, _, _, _, _) = x
instance T2 (V6 a) where
  t2 (_, x, _, _, _, _) = x
instance T3 (V6 a) where
  t3 (_, _, x, _, _, _) = x
instance T4 (V6 a) where
  t4 (_, _, _, x, _, _) = x
instance T5 (V6 a) where
  t5 (_, _, _, _, x, _) = x
instance T6 (V6 a) where
  t6 (_, _, _, _, _, x) = x

instance Num a => Dist (V7 a) where
  manDist (t1, u1, v1, w1, x1, y1, z1) (t2, u2, v2, w2, x2, y2, z2) = abs (t2 - t1) + abs (u2 - u1) + abs (v2 - v1) + abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (t1, u1, v1, w1, x1, y1, z1) (t2, u2, v2, w2, x2, y2, z2)   = let td = t2 - t1; ud = u2 - u1; vd = v2 - v1; wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in td * td + ud * ud + vd * vd + wd * wd + xd * xd + yd * yd + zd * zd
instance T1 (V7 a) where
  t1 (x, _, _, _, _, _, _) = x
instance T2 (V7 a) where
  t2 (_, x, _, _, _, _, _) = x
instance T3 (V7 a) where
  t3 (_, _, x, _, _, _, _) = x
instance T4 (V7 a) where
  t4 (_, _, _, x, _, _, _) = x
instance T5 (V7 a) where
  t5 (_, _, _, _, x, _, _) = x
instance T6 (V7 a) where
  t6 (_, _, _, _, _, x, _) = x
instance T7 (V7 a) where
  t7 (_, _, _, _, _, _, x) = x

instance Num a => Dist (V8 a) where
  manDist (s1, t1, u1, v1, w1, x1, y1, z1) (s2, t2, u2, v2, w2, x2, y2, z2) = abs (s2 - s1) + abs (t2 - t1) + abs (u2 - u1) + abs (v2 - v1) + abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (s1, t1, u1, v1, w1, x1, y1, z1) (s2, t2, u2, v2, w2, x2, y2, z2)   = let sd = s2 - s1; td = t2 - t1; ud = u2 - u1; vd = v2 - v1; wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in sd * sd + td * td + ud * ud + vd * vd + wd * wd + xd * xd + yd * yd + zd * zd
instance T1 (V8 a) where
  t1 (x, _, _, _, _, _, _, _) = x
instance T2 (V8 a) where
  t2 (_, x, _, _, _, _, _, _) = x
instance T3 (V8 a) where
  t3 (_, _, x, _, _, _, _, _) = x
instance T4 (V8 a) where
  t4 (_, _, _, x, _, _, _, _) = x
instance T5 (V8 a) where
  t5 (_, _, _, _, x, _, _, _) = x
instance T6 (V8 a) where
  t6 (_, _, _, _, _, x, _, _) = x
instance T7 (V8 a) where
  t7 (_, _, _, _, _, _, x, _) = x
instance T8 (V8 a) where
  t8 (_, _, _, _, _, _, _, x) = x

instance Num a => Dist (V9 a) where
  manDist (r1, s1, t1, u1, v1, w1, x1, y1, z1) (r2, s2, t2, u2, v2, w2, x2, y2, z2) = abs (r2 - r1) + abs (s2 - s1) + abs (t2 - t1) + abs (u2 - u1) + abs (v2 - v1) + abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
  dist2 (r1, s1, t1, u1, v1, w1, x1, y1, z1) (r2, s2, t2, u2, v2, w2, x2, y2, z2)   = let rd = r2 - r1; sd = s2 - s1; td = t2 - t1; ud = u2 - u1; vd = v2 - v1; wd = w2 - w1; xd = x2 - x1; yd = y2 - y1; zd = z2 - z1 in rd * rd + sd * sd + td * td + ud * ud + vd * vd + wd * wd + xd * xd + yd * yd + zd * zd
instance T1 (V9 a) where
  t1 (x, _, _, _, _, _, _, _, _) = x
instance T2 (V9 a) where
  t2 (_, x, _, _, _, _, _, _, _) = x
instance T3 (V9 a) where
  t3 (_, _, x, _, _, _, _, _, _) = x
instance T4 (V9 a) where
  t4 (_, _, _, x, _, _, _, _, _) = x
instance T5 (V9 a) where
  t5 (_, _, _, _, x, _, _, _, _) = x
instance T6 (V9 a) where
  t6 (_, _, _, _, _, x, _, _, _) = x
instance T7 (V9 a) where
  t7 (_, _, _, _, _, _, x, _, _) = x
instance T8 (V9 a) where
  t8 (_, _, _, _, _, _, _, x, _) = x
instance T9 (V9 a) where
  t9 (_, _, _, _, _, _, _, _, x) = x

instance T1 (V10 a) where
  t1 (x, _, _, _, _, _, _, _, _, _) = x
instance T2 (V10 a) where
  t2 (_, x, _, _, _, _, _, _, _, _) = x
instance T3 (V10 a) where
  t3 (_, _, x, _, _, _, _, _, _, _) = x
instance T4 (V10 a) where
  t4 (_, _, _, x, _, _, _, _, _, _) = x
instance T5 (V10 a) where
  t5 (_, _, _, _, x, _, _, _, _, _) = x
instance T6 (V10 a) where
  t6 (_, _, _, _, _, x, _, _, _, _) = x
instance T7 (V10 a) where
  t7 (_, _, _, _, _, _, x, _, _, _) = x
instance T8 (V10 a) where
  t8 (_, _, _, _, _, _, _, x, _, _) = x
instance T9 (V10 a) where
  t9 (_, _, _, _, _, _, _, _, x, _) = x
instance T10 (V10 a) where
  t10 (_, _, _, _, _, _, _, _, _, x) = x

instance T1 (V11 a) where
  t1 (x, _, _, _, _, _, _, _, _, _, _) = x
instance T2 (V11 a) where
  t2 (_, x, _, _, _, _, _, _, _, _, _) = x
instance T3 (V11 a) where
  t3 (_, _, x, _, _, _, _, _, _, _, _) = x
instance T4 (V11 a) where
  t4 (_, _, _, x, _, _, _, _, _, _, _) = x
instance T5 (V11 a) where
  t5 (_, _, _, _, x, _, _, _, _, _, _) = x
instance T6 (V11 a) where
  t6 (_, _, _, _, _, x, _, _, _, _, _) = x
instance T7 (V11 a) where
  t7 (_, _, _, _, _, _, x, _, _, _, _) = x
instance T8 (V11 a) where
  t8 (_, _, _, _, _, _, _, x, _, _, _) = x
instance T9 (V11 a) where
  t9 (_, _, _, _, _, _, _, _, x, _, _) = x
instance T10 (V11 a) where
  t10 (_, _, _, _, _, _, _, _, _, x, _) = x
instance T11 (V11 a) where
  t11 (_, _, _, _, _, _, _, _, _, _, x) = x

instance T1 (V12 a) where
  t1 (x, _, _, _, _, _, _, _, _, _, _, _) = x
instance T2 (V12 a) where
  t2 (_, x, _, _, _, _, _, _, _, _, _, _) = x
instance T3 (V12 a) where
  t3 (_, _, x, _, _, _, _, _, _, _, _, _) = x
instance T4 (V12 a) where
  t4 (_, _, _, x, _, _, _, _, _, _, _, _) = x
instance T5 (V12 a) where
  t5 (_, _, _, _, x, _, _, _, _, _, _, _) = x
instance T6 (V12 a) where
  t6 (_, _, _, _, _, x, _, _, _, _, _, _) = x
instance T7 (V12 a) where
  t7 (_, _, _, _, _, _, x, _, _, _, _, _) = x
instance T8 (V12 a) where
  t8 (_, _, _, _, _, _, _, x, _, _, _, _) = x
instance T9 (V12 a) where
  t9 (_, _, _, _, _, _, _, _, x, _, _, _) = x
instance T10 (V12 a) where
  t10 (_, _, _, _, _, _, _, _, _, x, _, _) = x
instance T11 (V12 a) where
  t11 (_, _, _, _, _, _, _, _, _, _, x, _) = x
instance T12 (V12 a) where
  t12 (_, _, _, _, _, _, _, _, _, _, _, x) = x
