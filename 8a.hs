import Data.List.Split
import Data.Char

main = interact $ (++ "\n") . show . doCalc . findMin0s . chunksOf lsize . filter isDigit
  where
    lsize = 25*6
    lf c = length . filter (==c)
    doCalc s = (lf '1' s) * (lf '2' s)
    findMin0s ss = snd $ minimum $ zip (map (lf '0') ss) ss
