{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.Ord

splitBy c s = case break (== c) s of
                (t, s') -> t:(case s' of
                                []    -> []
                                _:s'' -> splitBy c s'')

getCoords :: [String] -> [(Int, Int)]
getCoords !xs = getCoords' (0,0) xs
  where
    getCoords' _ [] = []
    getCoords' (!x, !y) ((!i: (!t)): (!is)) = let t' = read t :: Int
                                                  (cs, xy') = case i of
                                                                'R' -> (map (\j -> (x + j, y)) [1..t'], (x + t', y))
                                                                'L' -> (map (\j -> (x - j, y)) [1..t'], (x - t', y))
                                                                'U' -> (map (\j -> (x, y + j)) [1..t'], (x, y + t'))
                                                                'D' -> (map (\j -> (x, y - j)) [1..t'], (x, y - t'))
                                              in cs ++ getCoords' xy' is

intersect' !xs !ys = [x | x <- xs, y <- take 1 $ filter (==x) ys]

dist :: (Int, Int) -> Int
dist (x1, y1) = abs x1 + abs y1

main = do
  is <- lines <$> getContents
  let [cs0,cs1] = map (getCoords . splitBy ',') is
  let cs' = intersect' cs0 cs1
  let closest = minimumBy (comparing dist) cs'
  print $ dist closest
