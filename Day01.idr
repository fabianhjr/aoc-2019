module Main

fuelNeeded : Nat -> Nat
fuelNeeded weight = (weight `div` 3) `minus` 2

accumulateFuel : Nat -> IO ()
accumulateFuel fuel = do
  nextWeight <- getLine
  if nextWeight == ""
    then putStrLn $ cast fuel
    else accumulateFuel $ fuel + fuelNeeded (cast nextWeight)

recursiveFuel : Nat -> Nat
recursiveFuel weight = sum . takeWhile (> 0) . take 10000 $ the (Stream Nat) (drop 1 $ iterate fuelNeeded weight)

accumulateFuel2 : Nat -> IO ()
accumulateFuel2 fuel = do
  nextWeight <- getLine
  if nextWeight == ""
    then putStrLn $ cast fuel
    else accumulateFuel2 $ fuel + recursiveFuel (cast nextWeight)

main : IO ()
main = accumulateFuel2 Z
