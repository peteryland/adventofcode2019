import Data.Char

line2moon :: String -> (Int, Int, Int, Int, Int, Int)
line2moon s = (read x, read y, read z, 0, 0, 0)
  where
    (x, s') = getNextNum s
    (y, s'') = getNextNum s'
    (z, _) = getNextNum s''
    getNextNum s = span isNum $ snd $ span (not . isNum) s
    isNum x = isDigit x || x == '-'

main = interact $ (++"\n") . show . tte . stepn 1000 . map line2moon . lines

stepn :: Int -> [(Int, Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
stepn 0 moons = moons
stepn n moons = stepn (n-1) $ step moons

step :: [(Int, Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
step moons = applyvelocity $ applygravity moons

applygravity :: [(Int, Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
applygravity moons = map (applyg moons) moons
applyg :: [(Int, Int, Int, Int, Int, Int)] -> (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
applyg moons m = foldl applyg' m moons
applyg' (x, y, z, vx, vy, vz) (xg, yg, zg, vxg, vyg, vzg) = (x, y, z, adj x xg vx vxg, adj y yg vy vyg, adj z zg vz vzg)
adj w wg vw vwg
  | w > wg = vw - 1
  | w == wg = vw
  | otherwise = vw + 1

applyvelocity :: [(Int, Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
applyvelocity moons = map applyv moons
applyv (x, y, z, vx, vy, vz) = (x + vx, y + vy, z + vz, vx, vy, vz)

tte :: [(Int, Int, Int, Int, Int, Int)] -> Int
tte moons = sum $ map te moons

ke (x, y, z, vx, vy, vz) = abs vx + abs vy + abs vz
pe (x, y, z, vx, vy, vz) = abs x + abs y + abs z
te m = ke m * pe m
