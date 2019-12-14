import PP

data Thing = Thing Int String deriving (Show, Eq, Ord)

main = interact' getFuelForTrillion (readline . splitBy '=')

readline :: [String] -> ([Thing], Thing)
readline (x:y:[]) = (map readthing $ splitBy ',' x, readthing $ drop 2 y)

readthing :: String -> Thing
readthing s = let [n, t] = splitBy ' ' s in Thing (read n) t

get :: [([Thing], Thing)] -> Thing -> [Thing]
get [] t = [t]
get ((ris, Thing m t):rs) (Thing n s) = if n == 0 then
                                          []
                                        else if n < 0 then
                                          [Thing n s]
                                        else
                                          if t == s then
                                            let num = ((n - 1) `div` m) + 1
                                                leftover = num * m - n
                                                needs = map (\(Thing o u) -> Thing (o*num) u) ris
                                            in if leftover == 0 then needs else (Thing (-leftover) s) : needs
                                          else get rs (Thing n s)

combine = combine' . sortOn (\(Thing _ s) -> s)
combine' [] = []
combine' [t] = [t]
combine' ((Thing n s):(Thing m t):needs) = if s == t then combine' ((Thing (n + m) s) : needs) else (Thing n s) : combine' ((Thing m t) : needs)

onlyPos [] = []
onlyPos ((Thing n s):ns) = if n > 0 then (Thing n s):onlyPos ns else onlyPos ns

step allrs needs = let needs' = combine $ concatMap (get allrs) needs
                   in case onlyPos needs' of
                     [(Thing r "ORE")] -> Just r
                     _ -> step allrs needs'

getFuel :: [([Thing], Thing)] -> Int
getFuel rs = case step rs [(Thing 1 "FUEL")] of Just x -> x

getFuelForTrillion :: [([Thing], Thing)] -> Int
getFuelForTrillion rs = let n = 1000000000000 `div` (getFuel rs) in tryGet n (n `shiftR` 1)
  where
    tryGet n n2 = if n2 == 0 then
                    n
                  else
                    if try n > 1000000000000 then
                      tryGet (n - n2) (n2 `shiftR` 1)
                    else
                      tryGet (n + n2) (n2 `shiftR` 1)
    try n = case step rs [(Thing n "FUEL")] of Just x -> x
