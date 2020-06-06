module Main

import Data.Strings
import Data.Vect
import System

%default total

ints : Vect 4 Int
ints = [1, 2, 3, 4]

main : IO ()
main = do
  [_, arg] <- getArgs
      | _ => do putStrLn "One argument expected."
                exitFailure
  let n = stringToNatOrZ arg
  case natToFin n (length ints + 1) of
       Nothing => do putStrLn "Invalid number."
                     exitFailure
       Just (FS i) => putStrLn $ "Value: " ++ show (index i ints)
