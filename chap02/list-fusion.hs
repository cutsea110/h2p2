import Data.List

inc :: [Int] -> [Int]
inc (x:xs) = x + 1 : inc xs
inc [] = []

summer :: Int -> [Int] -> [Int]
summer a (x:xs) = let r = a + x in r `seq` r : summer r xs
summer _ []     = []

-- main = print $ sum $ summer 0 $ inc [1..100000]
main = print $ sum $ scanl (+) 0 $ map (+1) [1..100000]
