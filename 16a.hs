import PP

pat :: [Int]
pat = [0, 1, 0, -1]

f n pat = tail $ concat $ repeat $ concatMap (\x -> replicate n x) pat

g x = abs x `mod` 10

step :: [Int] -> [Int]
step xs = step' 0 xs
step' n xs = if n == length xs
             then []
             else (g $ sum (zipWith (*) xs (f (n+1) pat))):step' (n+1) xs

getAnswer :: [Int] -> Int
getAnswer xs = read . concatMap show $ take 8 $ iterate step xs !! 100

main = interact1 $ getAnswer . map (read . (:[]))
