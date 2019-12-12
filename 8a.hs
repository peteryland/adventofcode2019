import PP

main = interact1 $ doCalc . findMin0s . chunksOf lsize
  where
    lsize = 25*6
    lf c = length . filter (==c)
    doCalc s = (lf '1' s) * (lf '2' s)
    findMin0s ss = snd $ minimum $ zip (map (lf '0') ss) ss
