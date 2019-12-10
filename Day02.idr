module Main

total
update : (list: List a)
       -> (index: Nat)
       -> a
       -> {auto ok: InBounds index list}
       -> List a
update list index new = take index list ++ [new] ++ drop (index + 1) list

data Terminated = Error Nat | Success Nat

partial
evaluate : (ip: Nat)
         -> (mem: List Nat)
         -> {auto ok: InBounds ip mem}
         -> Either (List Nat) Terminated
evaluate instructionPointer memory =
  case the Integer (fromNat nextInstruction) of
    1 => case inBounds (instructionPointer + 4) opCode1Eval of
      Yes prf =>
          evaluate (instructionPointer + 4) opCode1Eval
      No ctr => Right . Error $ 0
    2  => case inBounds (instructionPointer + 4) opCode2Eval of
      Yes prf =>
          evaluate (instructionPointer + 4) opCode2Eval
      No ctr => Right . Error $ 0
    99 => Right . Success $ atIndex 0
    other => Right . Error . fromInteger $ other
  where
    nextInstruction : Nat
    nextInstruction = index instructionPointer memory
    atIndex : Nat -> Nat
    atIndex idx = index idx memory {ok=believe_me ()}
    val1 : Nat
    val1 = atIndex . atIndex $ instructionPointer + 1
    val2 : Nat
    val2 = atIndex . atIndex $ instructionPointer + 2
    updatedIndex : Nat
    updatedIndex = atIndex $ instructionPointer + 3
    opCode1Eval : List Nat
    opCode1Eval = update memory updatedIndex (val1 + val2) {ok=believe_me ()}
    opCode2Eval : List Nat
    opCode2Eval = update memory updatedIndex (val1 * val2) {ok=believe_me ()}

main : IO ()
main = do
  input <- getLine
  case evaluate 0 (map cast $ Strings.split (== ',') input) {ok=believe_me()} of
    Left list => putStrLn . concat . map cast $ list
    Right res => case res of
      Error err => putStrLn $ "Error: " ++ cast err
      Success s => putStrLn $ "Success: " ++ cast s
