import PP

main = interact' sum (calcFuel . read)

calcFuel x = x `div` 3 - 2
