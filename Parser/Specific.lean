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

  /-- Parse an alphanumeric string. -/
  def alphaNumeric : Parser Char String :=
    List.asString <$> many (oneIf Char.isAlphanum)

end Parser
