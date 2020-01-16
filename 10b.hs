{-# LANGUAGE TupleSections #-}
import PP

main = interact' (getanswer . (mapSize &&& mapCoords' '#')) id

getanswer ((maxX, maxY), xs) = x * 100 + y
  where
    (x, y) = findnth 200 basecoords xs
    basecoords = snd $ maximum $ map (\x -> (length $ flip tryit xs x, x)) xs

findnth n z zs = sweep' z zs !! (n - 1)
sweep' z zs = sweep z zs ++ sweep z (zs \\ sweep z zs)

sweep z zs = map t4 $ nubBy (\(s1, m1, _, _) (s2, m2, _, _) -> s1 == s2 && (m1 == m2 || abs (m2 - m1) < 0.00000001)) $ sort $ concatMap (maybeToList . grad z) (delete z zs)

grad :: V2i -> V2i -> Maybe (Bool, Float, Int, V2i)
grad (x1, y1) (x, y) = if (x,y)==(x1,y1) then Nothing else Just ((x1 > x), (realToFrac y - realToFrac y1) / (realToFrac x - realToFrac x1), dist2 (x1, y1) (x, y), (x, y))

compareGradients (x, y) (u, v) | signum x /= signum u || signum y /= signum v = False
compareGradients (x, y) (u, v) = v * x == u * y

tryit (x0, y0) = nubBy compareGradients . filter (/= (0, 0)) . map (\(x, y) -> (x - x0, y - y0))
