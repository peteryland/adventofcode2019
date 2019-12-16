import PP

main = interact1 $ FindLetters . chunksOf 25 . map (/= '0') . foldl1 (zipWith g) . chunksOf lsize
  where
    lsize = 25*6
    g s1 s2 = if s1 == '2' then s2 else s1
