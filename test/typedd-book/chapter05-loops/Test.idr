module Main

import Data.String
import System

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do putStrLn (show (S secs))
                        usleep 1000000
                        countdown secs

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (stringToNatOrZ input))
     else pure Nothing

main : IO ()
main = do putStr "Enter starting number: "
          Just startNum <- readNumber
              | Nothing => do putStrLn "Invalid input"
                              main
          countdown startNum
          putStr "Another (y/n)? "
          yn <- getLine
          if yn == "y" then main
                        else pure ()
