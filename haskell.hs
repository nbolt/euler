import Data.List(findIndex)
import System(getArgs)

main = do
  args <- getArgs
  mapM_ solve args

-- one
one = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]

-- two
two = sum [x | x <- take limit fibs, even x]

fibs = 0 : scanl (+) 1 fibs
(Just limit) = findIndex (> 4000000) fibs

-- helper function
solve x =
  case x of
    "1" -> putStrLn $ "01: " ++ show one
    "2" -> putStrLn $ "02: " ++ show two
    x   -> putStrLn $ "0" ++ x ++ ": not found/solved"
