main = interact ((++"\n") . show . sum . map (calcFuel . read) . lines)

calcFuel x = x `div` 3 - 2
