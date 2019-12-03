import System.IO
import Data.List.Split
import Data.List
import Test.QuickCheck

main = do
    input <- openFile "input.txt" ReadMode
    contents <- hGetContents input
    let intContents =  map (read::String->Int) (endBy "," contents)
    --putStrLn $ "Answer for first part: " ++ show (parseList 0 intContents)
    putStrLn $ "Answer for second part: " ++ show (partB intContents)
    hClose input

partB :: [Int] -> [[Int]]
partB list = [(parseList 0 (setelt 3 (setelt 2 list x) y) ) | x <- [0..99], y <- [0..99], head (parseList 0 (setelt 3 (setelt 2 list x) y) ) == 19690720]
      --where newList = (parseList 0 (setelt 3 (setelt 2 list x) y) )

parseList :: Int -> [Int] -> [Int] 
parseList i ii | ii!!i == 99   = ii
               | ii!!i == 1    = parseList (i+4) 
                             ( setelt (ii!!(i+3) +1) ii (ii!!(ii!!(i+1)) + ii!!(ii!!(i+2))) )
               | ii!!i == 2    = parseList (i+4) 
                             ( setelt (ii!!(i+3) +1) ii (ii!!(ii!!(i+1)) * ii!!(ii!!(i+2))) )
               | otherwise = error "parseList: i not 1, 2 or 99"

prop_parseList :: Bool
prop_parseList = parseList 0 [1,0,0,0,99]     == [2,0,0,0,99]
         && parseList 0 [2,3,0,3,99]          == [2,3,0,6,99]
         && parseList 0 [2,4,4,5,99,0]        == [2,4,4,5,99,9801]
         && parseList 0 [1,1,1,4,99,5,6,0,99] == [30,1,1,4,2,5,6,0,99]
{--}           
           

setelt:: Int -> [a] -> a -> [a]
setelt i list newValue = 
  let (ys,zs) = splitAt (i-1) list in  ys ++ [newValue] ++ tail zs
