import PP

main = interact' (getanswer . (mapSize &&& mapCoords' '#')) id

getanswer ((maxX, maxY), xs) = maximum $ map (\bs -> length $ xs \\ bs) $ tryit (maxX, maxY) xs xs

tryit :: V2i -> [V2i] -> [V2i] -> [[V2i]]
tryit _ _ []       = []
tryit m xs (x:xs') = concatMap (blocklist m x) xs:tryit m xs xs'

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
