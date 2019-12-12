import PP

getCoords :: [String] -> [V3i]
getCoords xs = getCoords' (0,0,0) xs
  where
    getCoords' _ [] = []
    getCoords' (x, y, k) ((i:t):is) = let t' = read t :: Int
                                          (cs, xy') = case i of
                                                        'R' -> (map (\j -> (x + j, y, k + j)) [1..t'], (x + t', y, k + t'))
                                                        'L' -> (map (\j -> (x - j, y, k + j)) [1..t'], (x - t', y, k + t'))
                                                        'U' -> (map (\j -> (x, y + j, k + j)) [1..t'], (x, y + t', k + t'))
                                                        'D' -> (map (\j -> (x, y - j, k + j)) [1..t'], (x, y - t', k + t'))
                                      in cs ++ getCoords' xy' is

intersectBy' eq xs ys = [(x, y) | x <- xs, y <- take 1 $ filter (eq x) ys]

dist :: ((Int, Int, Int), (Int, Int, Int)) -> Int
dist ((_, _, k1), (_, _, k2)) = k1 + k2

main = do
  is <- lines <$> getContents
  let [cs0,cs1] = map (getCoords . splitBy ',') is
  let cs' = intersectBy' (\(x1, y1, _) (x2, y2, _) -> x1 == x2 && y1 == y2) cs0 cs1
  let closest = minimumBy (comparing dist) cs'
  print $ dist closest
