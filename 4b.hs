import Data.List(group)

input = (165432, 707912)

ok x = a && b && c
  where
    x' = show x
    a = length x' == 6
    b = or $ map ((==2) . length) $ group x'
    c = and $ map (uncurry (>=)) $ zip (tail x') x'

main = print $ length $ filter id $ map ok [fst input..snd input]
