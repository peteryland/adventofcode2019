import PP

main = interact' solve readline

data Shuffle = NewStack | Cut Int | Increment Int deriving Show

readline ('c':'u':'t':' ':s) = Cut (read s)
readline ('d':'e':'a':'l':' ':'w':'i':'t':'h':' ':'i':'n':'c':'r':'e':'m':'e':'n':'t':' ':s) = Increment (read s)
readline "deal into new stack" = NewStack

solve = foldl' (flip $ runop 10007) 2019

runop l (Cut n) x = (x - n) `mod` l
runop l NewStack x = l - x - 1
runop l (Increment n) x = (x * n) `mod` l
