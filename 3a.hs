import PP

twoPerpendicularLinesCross :: (V2i, Int) -> (V2i, Int) -> Maybe Int  -- first line horizontal, second line vertical, returns manhatten distance if they cross
twoPerpendicularLinesCross ((x1, y1), d1) ((x2, y2), d2) = let v1 = if d1 < 0 then ((x1 + d1, y1), -d1) else ((x1, y1), d1)
                                                               v2 = if d2 < 0 then ((x2, y2 + d2), -d2) else ((x2, y2), d2)
                                                           in  twoPerpendicularLinesCross' v1 v2
  where
    twoPerpendicularLinesCross' ((x1', y1'), d1') ((x2', y2'), d2') = if x2' < x1' || x2' > x1' + d1' || y1' < y2' || y1' > y2' + d2'
                                                                      then Nothing
                                                                      else Just $ x2' + y1'


getCoords :: [String] -> ([(V2i, Int)], [(V2i, Int)]) -- returns list of horizontal lines and list of vertical lines
getCoords xs = getCoords' (0,0) xs
  where
    getCoords' _ [] = ([], [])
    getCoords' (x, y) ((i:t):is) = let t' = read t :: Int
                                   in  case i of
                                         'R' -> let (hs, vs) = getCoords' (x + t', y) is in (((x, y),  t'):hs, vs)
                                         'L' -> let (hs, vs) = getCoords' (x - t', y) is in (((x, y), -t'):hs, vs)
                                         'U' -> let (hs, vs) = getCoords' (x, y + t') is in (hs, ((x, y),  t'):vs)
                                         'D' -> let (hs, vs) = getCoords' (x, y - t') is in (hs, ((x, y), -t'):vs)

main = do
  is <- lines <$> getContents
  let [(hs0, vs0), (hs1, vs1)] = map (getCoords . splitBy ',') is
  print $ minimum $ mapMaybe id $ [ twoPerpendicularLinesCross c0 c1 | c0 <- hs0, c1 <- vs1] ++ [ twoPerpendicularLinesCross c1 c0 | c0 <- vs0, c1 <- hs1]
