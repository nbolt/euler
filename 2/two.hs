import Data.List

fibs = 0 : scanl (+) 1 fibs
(Just limit) = findIndex (> 4000000) fibs

main = putStrLn . show $ sum [x | x <- take limit fibs, even x]
