readIt :: FilePath -> IO Int
readIt file = 
  do
    l <- readFile file
    let ll = lines l
    let ii = map read ll :: [Int]
    return (sum (map countAll ii))

countIt :: Int -> Int
countIt i = i `div` 3 - 2

countAll :: Int -> Int
countAll i | ii <= 0 = 0
           | otherwise = ii + countAll ii 
                where ii = countIt i


