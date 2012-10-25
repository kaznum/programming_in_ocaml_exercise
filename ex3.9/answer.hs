-- The following haskell code works well
cond :: Bool -> Int -> Int -> Int
cond True  e1 _ = e1
cond False _ e2 = e2
fact :: Int -> Int
fact n = cond (n == 1) 1 (n * fact (n-1))
