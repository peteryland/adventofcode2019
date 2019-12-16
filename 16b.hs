{-# LANGUAGE BangPatterns #-}
import PP

g x = abs x `mod` 10

step :: [Int] -> [Int]
step = step' 0
  where
    step' _ [] = []
    step' !acc !(x:xs) = let x' = g $ x + acc
                         in  x' : step' x' xs

getAnswer :: [Int] -> String
-- getAnswer xs = read $ concatMap show $ take 8 $ reverse $ (iterate step (reverse $ drop offset $ concat $ replicate 10000 xs)) !! 100
getAnswer xs = concatMap show $ reverse $ drop (length xs * 10000 - offset - 8) $ (iterate step (take (10000 * length xs - offset) $ concat $ replicate 10000 $ reverse xs)) !! 100
  where
    offset :: Int
    offset = read $ concatMap show $ take 7 xs

main = interact1 $ Show' . getAnswer . map (read . (:[]))
