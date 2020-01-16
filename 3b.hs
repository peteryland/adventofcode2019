import PP

twoPerpendicularLinesCross :: (Int, V2i, Int) -> (Int, V2i, Int) -> Maybe Int -- first line horizontal, second line vertical, returns total path distance if they cross
twoPerpendicularLinesCross (n1, (x1, y1), d1) (n2, (x2, y2), d2) = if strictlyBetween x1 (x1 + d1) x2 && strictlyBetween y2 (y2 + d2) y1
                                                                   then Just $ n1 + n2 + abs (x2 - x1) + abs (y2 - y1)
                                                                   else Nothing

getCoords :: [String] -> ([(Int, V2i, Int)], [(Int, V2i, Int)]) -- returns list of horizontal lines and list of vertical lines, including path distance
getCoords xs = getCoords' 0 (0,0) xs
  where
    getCoords' _ _ [] = ([], [])
    getCoords' n (x, y) ((i:t):is) = let t' = read t :: Int
                                         n' = n + t'
                                         getCoords'' v = getCoords' n' v is
                                     in  case i of
                                           'R' -> let (hs, vs) = getCoords'' (x + t', y) in ((n, (x, y),  t'):hs, vs)
                                           'L' -> let (hs, vs) = getCoords'' (x - t', y) in ((n, (x, y), -t'):hs, vs)
                                           'U' -> let (hs, vs) = getCoords'' (x, y + t') in (hs, (n, (x, y),  t'):vs)
                                           'D' -> let (hs, vs) = getCoords'' (x, y - t') in (hs, (n, (x, y), -t'):vs)

main = do
  is <- lines <$> getContents
  let [(hs0, vs0), (hs1, vs1)] = map (getCoords . splitBy ',') is
  print $ minimum $ mapMaybe id $ [ twoPerpendicularLinesCross c0 c1 | c0 <- hs0, c1 <- vs1] ++ [ twoPerpendicularLinesCross c1 c0 | c0 <- vs0, c1 <- hs1]
