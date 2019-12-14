import PP

data Thing = Thing Int String deriving (Show, Eq, Ord)

main = interact' getFuel (readline . splitBy '=')

readline :: [String] -> ([Thing], Thing)
readline (x:y:[]) = (map readthing $ splitBy ',' x, readthing $ drop 2 y)

readthing :: String -> Thing
readthing s = let [n, t] = splitBy ' ' s in Thing (read n) t

-- react is (ris, ro) = (ro:) <$> foldl' use' (Just is) ris

-- use' :: Maybe [Thing] -> Thing -> Maybe [Thing]
-- use' Nothing _ = Nothing
-- use' (Just []) _ = Nothing
-- use' (Just (i:is)) ri = case use i ri of
  -- Nothing -> case use' (Just is) ri of
               -- Nothing -> Nothing
               -- Just is' -> Just $ i:is'
  -- Just i' -> Just (i':is)

-- use (Thing n s) (Thing m t) = if s == t && n >= m then Nothing else Just $ Thing (n-m) s

-- howMuchOre :: [([Thing], Thing)] -> [Thing] -> [([Thing], Thing)] -> Maybe Int
-- howMuchOre allrs [need] rs = howMuchOre' allrs need rs
-- howMuchOre allrs (need:needs) rs = (+) <$> howMuchOre' allrs need rs <*> howMuchOre allrs needs rs

-- howMuchOre' :: [([Thing], Thing)] -> Thing -> [([Thing], Thing)] -> Maybe Int
-- howMuchOre' _ _ [] = Nothing
-- howMuchOre' allrs (Thing n s) ((ris, Thing m t):rs) = if s == t
                                                      -- then
                                                        -- let num = ((n - 1) `div` m) + 1
                                                            -- ris' = map (\(Thing o u) -> Thing (o*num) u) ris
                                                        -- in case ris' of
                                                             -- [(Thing r "ORE")] -> Just r
                                                             -- _ -> howMuchOre allrs ris' allrs
                                                      -- else
                                                        -- howMuchOre' allrs (Thing n s) rs

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
