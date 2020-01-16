import PP

main = interact' (getanswer . mapCoords' '#') id

getanswer xs = maximum $ map (length . flip tryit xs) xs

compareGradients (x, y) (u, v) | signum x /= signum u || signum y /= signum v = False
compareGradients (x, y) (u, v) = v * x == u * y

tryit (x0, y0) = nubBy compareGradients . filter (/= (0, 0)) . map (\(x, y) -> (x - x0, y - y0))
