/-
  **FILE:** `Main.lean`
  **PURPOSE:** Main entry point for the `dodo` executable
-/

/- IMPORTS: -/

import Dodo



/- LAUNCH: -/

def main : IO UInt32 := do
  match (←getHomeDirectory) with
  | none => return EXIT_NOT_OK
  | some home =>
  try
    match (← Parser.dodo.run <$> readFromFile home) with
    | none =>
      IO.eprintln FAILED_TO_PARSE
      return EXIT_NOT_OK
    | some dodo =>
      match (← getDate) with
      | none => return EXIT_NOT_OK
      | some currentDate =>
        Terminal.clearTerminal
        dodo.writeToTerminal currentDate
        IO.print "> "
        let index? := (←(←IO.getStdin).getLine).removeNewline
        match (Parser.token Parser.nat).runOnString index? with
        | none =>
          IO.eprintln s!"`{index?}` is not a valid index mate"
          return EXIT_NOT_OK
        | some index =>
          if index ≥ dodo.items.length then do
            IO.eprintln s!"`{index?}` is not a valid index mate"
            return EXIT_NOT_OK
          IO.println <| dodo.items.get! index |>.toTerminal currentDate index

          return EXIT_OK
      -- match (← dodo.writeToFile home) with
      -- | none =>
      --   IO.eprintln "<!> Error writing dodo!"
      --   return EXIT_NOT_OK
      -- | some () =>
      --   IO.eprintln s!"<!> Writing was successful!"
      --   return EXIT_OK
  catch e =>
    IO.eprintln s!"dodo: Error caught by Lean T^T. Wasn't expected by me... Here it is ;~;!"
    IO.eprintln e.toString
    return EXIT_NOT_OK

-- #eval main
