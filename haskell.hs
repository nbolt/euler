import Data.List(find, findIndex)
import System.Environment(getArgs)

main = do
  args <- getArgs
  mapM_ solve args

-- helper functions
solved = [("1", one), ("2", two)]

solve num =
  let pair = find (\pair -> fst pair == num) solved in
    case pair of
      Nothing   -> putStrLn $ zeroPad num ++ ": unsolved"
      Just pair -> putStrLn $ zeroPad num ++ ": " ++ show (snd pair)

zeroPad str =
  case length str of
    1 -> "0" ++ str
    _ -> str

-- one
one = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]

-- two
two = sum [x | x <- take limit fibs, even x]

fibs = 0 : scanl (+) 1 fibs
(Just limit) = findIndex (> 4000000) fibs