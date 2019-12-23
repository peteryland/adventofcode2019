import PP

main = interact' solve readline

data Shuffle = NewStack | Cut Integer | Increment Integer deriving Show

readline ('c':'u':'t':' ':s) = Cut (read s)
readline ('d':'e':'a':'l':' ':'w':'i':'t':'h':' ':'i':'n':'c':'r':'e':'m':'e':'n':'t':' ':s) = Increment (read s)
readline "deal into new stack" = NewStack

ncards = 119315717514047 :: Integer
ncount = 101741582076661 :: Integer
solve xs = runop ncards (combinemany ncards ncount $ opmany ncards xs) 2020

inv a m = let (d, x, y) = extendedGCD a m in x `mod` m

extendedGCD a b = (d, x * signum a, y * signum b)
  where
    (d, x, y) = eGCD 0 1 1 0 (abs a) (abs b)
    eGCD n1 o1 n2 o2 r s
      | s == 0    = (r, o1, o2)
      | otherwise = case r `quotRem` s of
                      (q, t) -> eGCD (o1 - q*n1) n1 (o2 - q*n2) n2 s t

-- (m, b) represents mx + b
op l (Cut n) (m, b) = (m, (b + n) `mod` l)
op l NewStack (m, b) = ((-m) `mod` l, (-b - 1) `mod` l)
op l (Increment n) (m, b) = let n' = inv n l in ((m * n') `mod` l, (b * n') `mod` l)

opmany l xs = foldl' (flip $ op l) (1, 0) $ reverse xs

combine l (m, b) (m', b') = ((m'*m) `mod` l, (m'*b+b') `mod` l)

powMod :: (Integral a, Integral b) => a -> b -> a -> a
powMod x y m
  | m <= 0    = error "powModInt: non-positive modulo"
  | y <  0    = error "powModInt: negative exponent"
  | otherwise = f (x `rem` m) y 1 `mod` m
  where
    f _ 0 acc = acc
    f b e acc = f (b * b `rem` m) (e `quot` 2)
      (if odd e then (b * acc `rem` m) else acc)

combinemany :: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
combinemany l n (m, b) = let p = powMod m n l in (p, (b * (p - 1) * (inv (m - 1) l)) `mod` l)

runop l (m, b) x = (m * x + b) `mod` l
