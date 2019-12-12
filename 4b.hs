import PP

main = interactBy (=='-') $ length . filter id . map (ok . show) . (\[x, y] -> [x..y] :: [Int]) . map read

ok x = a && b && c
  where
    a = length x == 6
    b = or $ map ((==2) . length) $ group x
    c = and $ map (uncurry (>=)) $ zip (tail x) x
