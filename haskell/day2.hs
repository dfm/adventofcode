replaceVal :: (Integral a, Integral b) => a -> b -> [b] -> [b]
replaceVal _ _ [] = []
replaceVal n val (x:xs)
    | n == 0 = val:xs
    | otherwise = x:replaceVal (n - 1) val xs

data Op = Op Int [Int]

performOp :: (Integral a) => (a -> a -> a) -> [a] -> [a] -> [a]
performOp func [in1, in2, dest] xs = replaceVal dest (func in1 in2) xs
performOp _ _ xs = xs

-- parseOps :: (Integral a, Integral b) => b -> [a] -> [a]
-- parseOps _, [] = []
-- parseOps n, xs
--     | op == 99 = xs
--     | op == 1 = performOp (+)
--         replaceVal (x !! (n + 3)) ((x !! (n + 1)) + (x !! (n + 2)))
--     | op == 2 = replaceVal (x !! (n + 3)) ((x !! (n + 1)) * (x !! (n + 2)))
--     where op = x !! n

-- executeOp :: (Integral a, Integral b) => a -> [b] -> [b]
-- executeOp _ [] = []
-- executeOp n (code:in1:in2:dest:xs)
--     | n == 0 && x == 1 = x:xs
--     | otherwise = x:executeOp (n - 1) xs
