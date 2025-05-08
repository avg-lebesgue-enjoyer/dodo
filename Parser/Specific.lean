/-
  **FILE:** `Parser/Specific.lean`
  **PURPOSE:** Give specific, useful example `Parser`s.
-/

/- IMPORTS: -/

import Parser.Generic
import Parser.Help



/- SECTION: Useful parsers -/
namespace Parser

  /-- Parse a single digit. -/
  def digit : Parser Char Nat :=
    Char.toDigit <$> oneIf Char.isDigit

  /-- Parse a natural number. -/
  def nat : Parser Char Nat :=
    List.digitsToNat <$> some digit

end Parser
