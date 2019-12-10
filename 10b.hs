{-# LANGUAGE TupleSections #-}

import Data.List
import Data.Maybe

main = interact $ (++"\n") . show . getanswer . lines

getanswer ss = x * 100 + y
  where
    (x, y) = findnth 200 basecoords xs
    maxY = length ss
    maxX = length $ head ss
    xs = converttocoords ss
    basecoords = snd $ maximum $ map (\(z, bs) -> (length $ xs \\ bs, z)) $ tryit (maxX, maxY) xs xs

converttocoords :: [String] -> [(Int, Int)]
converttocoords ss = concat $ zipWith (\y line -> map (\x -> (x, y)) line) [0..] $ map doLine ss
  where
    doLine xs = concatMap maybeToList $ zipWith (\x y -> if x=='#' then Just y else Nothing) xs [0..]

findnth n z zs = sweep' z zs !! (n - 1)
sweep' z zs = sweep z zs ++ sweep z (zs \\ sweep z zs)

sweep z zs = map (\(_,_,_,z') -> z') $ nubBy (\(s1, m1, _, _) (s2, m2, _, _) -> s1 == s2 && (m1 == m2 || abs (m2 - m1) < 0.00000001)) $ sort $ concatMap (maybeToList . grad z) (delete z zs)

grad :: (Int, Int) -> (Int, Int) -> Maybe (Bool, Float, Int, (Int, Int))
grad (x1, y1) (x, y) = if (x,y)==(x1,y1) then Nothing else Just ((x1 > x), (realToFrac y - realToFrac y1) / (realToFrac x - realToFrac x1), dist2 (x1, y1) (x, y), (x, y))

dist2 :: (Int, Int) -> (Int, Int) -> Int
dist2 (x1, y1) (x2, y2) = let dy = y2 - y1; dx = x2 - x1 in dy * dy + dx * dx

tryit :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), [(Int, Int)])]
tryit _ _ []       = []
tryit m xs (x:xs') = (x, concatMap (blocklist m x) xs):tryit m xs xs'

blocklist :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
blocklist (maxX, maxY) (x1, y1) (x, y) =
  let dx = dx' `div` gcd dx' dy'
      dy = dy' `div` gcd dx' dy'
      dx' = x - x1
      dy' = y - y1
  in  if (x1, y1) == (x, y)
      then [(x, y)]
      else blocklist' [] (x+dx, y+dy) (dx, dy)
  where
    blocklist' bs (x, y) (dx, dy) =
      if x >= maxX || y >= maxY || x < 0 || y < 0
      then bs
      else blocklist' ((x, y):bs) (x + dx, y + dy) (dx, dy)
