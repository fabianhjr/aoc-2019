module Main

total
update : (list: List a)
       -> (index: Nat)
       -> a
       -> {auto ok: InBounds index list}
       -> List a
update list index new = take index list ++ [new] ++ drop (index + 1) list

data Terminated = Error Integer | Success Integer

Eq Terminated where
  (Error e1) == (Error e2) = e1 == e2
  (Success s1) == (Success s2) = s1 == s2
  _ == _ = False

Cast Terminated String where
  cast (Error n) = "!Error! " ++ cast n
  cast (Success n) = cast n

Cast String Terminated where
  cast s = Success . the Integer . cast $ s

partial
evaluate : (ip: Nat)
         -> (mem: List Integer)
         -> {auto ok: InBounds ip mem}
         -> Terminated
evaluate instructionPointer memory =
  case index instructionPointer memory of
    1 => case inBounds (instructionPointer + 4) opCode1Eval of
      Yes prf =>
          evaluate (instructionPointer + 4) opCode1Eval
      No ctr => Error $ 0
    2  => case inBounds (instructionPointer + 4) opCode2Eval of
      Yes prf =>
          evaluate (instructionPointer + 4) opCode2Eval
      No ctr => Error $ 0
    99 => Success . atIndex $ 0
    other => Error . fromInteger $ other
  where
    atIndex : Nat -> Integer
    atIndex idx = index idx memory {ok=?prf1}
    val1 : Integer
    val1 = atIndex . cast . atIndex $ instructionPointer + 1
    val2 : Integer
    val2 = atIndex . cast . atIndex $ instructionPointer + 2
    updatedIndex : Nat
    updatedIndex = cast . atIndex $ instructionPointer + 3
    opCode1Eval : List Integer
    opCode1Eval = update memory updatedIndex (val1 + val2) {ok=?prf2}
    opCode2Eval : List Integer
    opCode2Eval = update memory updatedIndex (val1 * val2) {ok=?prf3}

parseInput : String -> List Integer
parseInput input = map cast $ Strings.split (== ',') input

-- | Part 1. Evaluate a program
main' : IO ()
main' = do
  input <- getLine
  putStrLn . cast $ evaluate 0 (parseInput input) {ok=?prf4}

bruteSearch : List Integer -> List (Integer, Integer, Terminated)
bruteSearch input =
  [(x, y, evaluate 0 (attempt x y) {ok=?prf5})
  | x <- [0..100],
    y <- [0..100]]
  where
    attempt : Integer -> Integer -> List Integer
    attempt x y = update (update input 1 x {ok=believe_me ()}) 2 y {ok=?prf7}

-- | Part 2. Search parameter space
main : IO ()
main = do
  search <- getLine
  input <- getLine
  putStr . unlines .
    map (\(x, y, res) => "X: " ++ cast x ++ ", Y:" ++ cast y ++ ", Res: " ++ cast res) .
    filter(\(_, _, res) => (cast search) == res) $
    bruteSearch (parseInput input)
