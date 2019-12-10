module Main

-- TODO: Auto-proof divNatNZ

-- | Part 1. The fuel mass required to put in orbit a given mass equal to a
-- third of the given mass rounded down minus 2 mass units.
total fuelNeeded : Nat -> Nat
fuelNeeded weight = (divNatNZ weight 3 SIsNotZ) `minus` 2

-- | Part 2. Consider fuel cost of any fuel added to put a given mass into orbit
total recursiveFuelNeeded : Nat -> Nat
recursiveFuelNeeded weight =
  -- Sum the stream up to 1,000 terms to ensure totality
  sum . takeWhile (> 0) . take 1000 .
    -- The stream of iterative applications of fuelNeeded minus fuelNeeded^(0)
    the (Stream Nat) . drop 1 $ iterate fuelNeeded weight

-- | Generic main to use either fuelNeeded or recursiveFuelNeeded
partial main' : (Nat -> Nat) -> Nat -> IO ()
main' calculator accumulatedFuel = do
  nextModuleWeight <- getLine
  if nextModuleWeight == ""
    then putStrLn $ "The fuel needed is: " ++ (cast accumulatedFuel)
    else main' calculator $ accumulatedFuel + calculator (cast nextModuleWeight)

partial main : IO ()
main = main' recursiveFuelNeeded Z
