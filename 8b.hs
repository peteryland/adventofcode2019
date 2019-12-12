import PP

main = interact $ unlines . ("":) . chunksOf 25 . map vis . foldl1 (zipWith g) . chunksOf lsize . concat . lines
  where
    lsize = 25*6
    g s1 s2 = case s1 of
                '2' -> s2
                _   -> s1

    vis x = if x == '0'
            then ' '
            else '#'
