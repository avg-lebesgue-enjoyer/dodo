/-
  **FILE:** `Dodo/Parsers.lean`
  **PURPOSE:** Provide parsers for `dodo` types.
-/

/- IMPORTS: -/

import Parser.Basic
import Dodo.Types



/- SECTION: Dates -/

/-- Parse a valid year. -/
def Parser.year : Parser Char Nat := nat.tokenSatisfying (2000 ≤ ·)
/-- Parse a valid month. -/
def Parser.month : Parser Char Nat := nat.tokenSatisfying (· ≤ 12)
/--
  Parse a day, validated assuming a month of "January".

  Not validated in the context of a known month. See also: `Parser.monthDay`.
-/
def Parser.day : Parser Char Nat := nat.tokenSatisfying (· ≤ 31)
/-- Parse a valid hour. -/
def Parser.hour : Parser Char Nat := nat.tokenSatisfying (· ≤ 23)
/-- Parse a valid minute. -/
def Parser.minute : Parser Char Nat := nat.tokenSatisfying (· ≤ 59)
/-- Parse `END_OF_DAY`. A successful `Unit` parse means that `END_OF_DAY` was found; failure is failure. -/
def Parser.endOfDay : Parser Char Unit := discard ∘ token ∘ exactlyString <| END_OF_DAY

/-- Parse a month and a day together, ensuring that the day is valid in context of the month. -/
def Parser.monthDay : Parser Char (Nat × Nat) := do
  let month ← .month
  let day ← .day
  if ¬ (month = 2 → day ≤ 28) then
    failure
  else if ¬ (month = 4 ∨ month = 6 ∨ month = 9 ∨ month = 11 → day ≤ 30) then
    failure
  else
    pure (month, day)

/-- Parse a valid `HourMinute`. -/
def Parser.hourMinute : Parser Char HourMinute := do
  (do
    let hour ← .hour
    let minute ← .minute
    pure <| HourMinute.hourMin hour minute
  ) <|> (do
    let _ ← Parser.endOfDay
    pure <| HourMinute.endOfDay)

/-- Parse a valid `Date`. -/
def Parser.date : Parser Char Date := do
  let year ← .year
  let (month, day) ← .monthDay
  let hourMinute ← .hourMinute
  pure { year := year, month := month, day := day, hourMinute := hourMinute : Date}
