main = interact ((++"\n") . show . sum . map (recurCalcFuel . calcFuel . read) . lines)

calcFuel x = x `div` 3 - 2

recurCalcFuel x = x + if f <= 0 then 0 else recurCalcFuel f
  where
    f = calcFuel x
