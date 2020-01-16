import PP

main = interactBy (=='-') $ length . (\[x, y] -> getPossbilities x y)

getPossbilities x high | x > high = []
getPossbilities x high = let next = getPossbilities (findNext $ show $ read x + 1) high
                         in  if a && b
                             then x:next
                             else next
  where
    findNext [] = []
    findNext [x] = [x]
    findNext xs@(x:y:ys) = if x > y then zipWith const (repeat x) xs else x:findNext (y:ys)
    a = or $ map ((>1) . length) $ group x
    b = and $ map (uncurry (>=)) $ zip (tail x) x
