import PP

main = interact' sum (recurCalcFuel . calcFuel . read)

calcFuel x = x `div` 3 - 2

recurCalcFuel x = x + let f = calcFuel x
                      in  if f <= 0
                            then 0
                            else recurCalcFuel f
