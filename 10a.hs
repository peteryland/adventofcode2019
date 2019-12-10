import Data.List
import Data.Maybe

main = interact $ (++"\n") . show . getanswer . lines

getanswer ss = maximum $ map (\bs -> length $ xs \\ bs) $ tryit (maxX, maxY) xs xs
  where
    maxY = length ss
    maxX = length $ head ss
    xs = converttocoords ss

converttocoords :: [String] -> [(Int, Int)]
converttocoords ss = concat $ zipWith (\y line -> map (\x -> (x, y)) line) [0..] $ map doLine ss
  where
    doLine xs = concatMap maybeToList $ zipWith (\x y -> if x=='#' then Just y else Nothing) xs [0..]

tryit :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
tryit _ _ []       = []
tryit m xs (x:xs') = concatMap (blocklist m x) xs:tryit m xs xs'

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

