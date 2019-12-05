main = do
    contents <- getContents
    let fuel = sum(map (getFuel . read) $ lines contents :: [Int])
    print fuel

getFuel :: (Integral a) => a -> a
getFuel mass
    | newMass <= 0 = 0
    | otherwise = newMass + getFuel(newMass)
    where newMass = mass `div` 3 - 2