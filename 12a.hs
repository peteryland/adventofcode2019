import PP

line2moon :: String -> V6i
line2moon s = case findAll s of [x, y, z] -> (x, y, z, 0, 0, 0); otherwise -> (0, 0, 0, 0, 0, 0)

main = interact' (tte . stepn 1000) line2moon

stepn :: Int -> [V6i] -> [V6i]
stepn 0 moons = moons
stepn n moons = stepn (n-1) $ step moons

step :: [V6i] -> [V6i]
step moons = applyvelocity $ applygravity moons

applygravity :: [V6i] -> [V6i]
applygravity moons = map (applyg moons) moons
applyg :: [V6i] -> V6i -> V6i
applyg moons m = foldl applyg' m moons
applyg' (x, y, z, vx, vy, vz) (xg, yg, zg, vxg, vyg, vzg) = (x, y, z, adj x xg vx vxg, adj y yg vy vyg, adj z zg vz vzg)
adj w wg vw vwg
  | w > wg = vw - 1
  | w == wg = vw
  | otherwise = vw + 1

applyvelocity :: [V6i] -> [V6i]
applyvelocity moons = map applyv moons
applyv (x, y, z, vx, vy, vz) = (x + vx, y + vy, z + vz, vx, vy, vz)

tte :: [V6i] -> Int
tte moons = sum $ map te moons

ke (x, y, z, vx, vy, vz) = abs vx + abs vy + abs vz
pe (x, y, z, vx, vy, vz) = abs x + abs y + abs z
te m = ke m * pe m
