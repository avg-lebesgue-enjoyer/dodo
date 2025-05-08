/-
  **FILE:** `Dodo/Types.lean`
  **PURPOSE:** Provide basic types for the `dodo` application.
-/

/- SECTION: Global constants -/

/-- The string representation of the `HourMinute.endOfDay` sentinel. -/
def END_OF_DAY : String := "e"



/- SECTION: `HourMinute`, `Date` -/

/--
  An hour-minute pair within a date, or the special sentinel `endOfDay`.

  `hourMin _ _ ≤ hourMin 23 59 < endOfDay`

  **GRAMMAR:**
    - HourMinute  ::= Hour Minute | e
    - Hour        ::= 00 | 01 | ⋯ | 23
    - Minute      ::= 00 | 01 | ⋯ | 59
-/
inductive HourMinute : Type where
  /--
    The end of the day. `hourMin _ _ < endOfDay`.
  -/
  | endOfDay : HourMinute
  /--
    An actual hour-minute pair. The hour is encoded in 24-hour time. As strings, both
    fields are encoded using two digits:
    - 0 ↦ `"00"`
    - ⋯
    - 23 ↦ `"23"`
    - ⋯
    - 59 ↦ `"59"`
  -/
  | hourMin (hour : Nat) (minute : Nat) : HourMinute

/--
  A date.

  Leap years don't exist because I couldn't be bothered to code them in, and frankly my
  life would be better if I reserved Feb. 29th for relaxation...

  **GRAMMAR:**
    - Date  ::= Year Month Day HourMinute
    - Year  ::= 2000 | 2001 | ⋯
    - Month ::= 01 | 02 | ⋯ | 12
    - Day   ::= 01 | 02 | ⋯ | 31

  **VALIDATION RULES:** (those not in the above grammar)
    - Day ≤ 31
    - Month = 2 → Day ≤ 28
    - Month ∈ {4, 6, 9, 11} → Day ≤ 30
-/
structure Date : Type where
  /-- The year. `2000 ≤ year`. -/
  year : Nat
  /--
    The month. As a string, encoded using two digits:
    - January ↦ `"01"`
    - ⋯
    - December ↦ `"12"`
  -/
  month : Nat
  /--
    The day. As a string, encoded using two digits:
    - First ↦ `"01"`
    - ⋯
    - Thirty-first ↦ `"31"`
  -/
  day : Nat
  /-- The hour-minute pair. -/
  hourMinute : HourMinute

instance HourMinute.instToString : ToString HourMinute where
  toString (hourMinute : HourMinute) : String :=
    match hourMinute with
    | .endOfDay => "e"
    | .hourMin hour minute => s!"{hour} {minute}"

instance Date.instToString : ToString Date where
  toString (date : Date) : String :=
    s!"{date.year} {date.month} {date.day} {date.hourMinute}"
