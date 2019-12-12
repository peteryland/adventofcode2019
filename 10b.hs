{-# LANGUAGE TupleSections #-}
import PP

main = interact' (getanswer . (mapSize &&& mapCoords' '#')) id

getanswer ((maxX, maxY), xs) = x * 100 + y
  where
    (x, y) = findnth 200 basecoords xs
    basecoords = snd $ maximum $ map (\(z, bs) -> (length $ xs \\ bs, z)) $ tryit (maxX, maxY) xs xs

findnth n z zs = sweep' z zs !! (n - 1)
sweep' z zs = sweep z zs ++ sweep z (zs \\ sweep z zs)

sweep z zs = map (\(_,_,_,z') -> z') $ nubBy (\(s1, m1, _, _) (s2, m2, _, _) -> s1 == s2 && (m1 == m2 || abs (m2 - m1) < 0.00000001)) $ sort $ concatMap (maybeToList . grad z) (delete z zs)

grad :: V2i -> V2i -> Maybe (Bool, Float, Int, V2i)
grad (x1, y1) (x, y) = if (x,y)==(x1,y1) then Nothing else Just ((x1 > x), (realToFrac y - realToFrac y1) / (realToFrac x - realToFrac x1), dist2 (x1, y1) (x, y), (x, y))

tryit :: V2i -> [V2i] -> [V2i] -> [(V2i, [V2i])]
tryit _ _ []       = []
tryit m xs (x:xs') = (x, concatMap (blocklist m x) xs):tryit m xs xs'

blocklist :: V2i -> V2i -> V2i -> [V2i]
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
