module Main

fuelNeeded : Nat -> Nat
fuelNeeded weight = (weight `div` 3) `minus` 2

accumulateFuel : Nat -> IO ()
accumulateFuel fuel = do
  nextWeight <- getLine
  if nextWeight == ""
    then putStrLn $ cast fuel
    else accumulateFuel $ fuel + fuelNeeded (cast nextWeight)

main : IO ()
main = accumulateFuel Z
