import Data.Char
import Data.List
import Data.Set (member)
import qualified Data.Set as S

dropUntilDuplicate :: Ord a => [(Int, a)] -> [(Int, a)]
dropUntilDuplicate xs = foldr go (const []) xs S.empty
  where
    go (n,x) cont set
      | x `member` set = [(n,x)]
      | otherwise      = cont (S.insert x set)

line2moon :: String -> (Int, Int, Int, Int, Int, Int)
line2moon s = (read x, read y, read z, 0, 0, 0)
  where
    (x, s') = getNextNum s
    (y, s'') = getNextNum s'
    (z, _) = getNextNum s''
    getNextNum s = span isNum $ snd $ span (not . isNum) s
    isNum x = isDigit x || x == '-'

main = do
  moons0 <- map line2moon . lines <$> getContents
  let steps = iterate' step moons0
  let findn f = fst $ head $ dropUntilDuplicate $ zip [0..] $ map (map f) steps
  let nx = findn (\(x, _, _, vx, _, _) -> (x,vx))
  let ny = findn (\(_, y, _, _, vy, _) -> (y,vy))
  let nz = findn (\(_, _, z, _, _, vz) -> (z,vz))
  print $ foldl1 lcm [nx, ny, nz]

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
