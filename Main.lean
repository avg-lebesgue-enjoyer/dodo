/-
  **FILE:** `Main.lean`
  **PURPOSE:** Main entry point for the `dodo` executable
-/

import Dodo

def main : IO Unit :=
  for testString
  in  [ "Amogus"
      , "2025 06 08 22 43"
      , "2025 06 08 e"
      , "2025 06 08 f"
      , "   2025 06     08 e   \t"
      , "   2025 06     08 22 e   \t\n"
      ]
  do
    IO.println ∘ ToString.toString ∘ Parser.date.runOnString <| testString

#eval main
