/-
  **FILE:** `Dodo/Types.lean`
  **PURPOSE:** Provide basic types for the `dodo` application.
-/

/- IMPORTS: -/

import Dodo.Help



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
    The end of the day. `hourMin _ _ < endOfDay`.
  -/
  | endOfDay : HourMinute
  deriving Ord

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
  deriving Ord

instance HourMinute.instToString : ToString HourMinute where
  toString (hourMinute : HourMinute) : String :=
    match hourMinute with
    | .endOfDay => "e"
    | .hourMin hour minute => s!"{hour.toString2} {minute.toString2}"

instance Date.instToString : ToString Date where
  toString (date : Date) : String :=
    s!"{date.year} {date.month.toString2} {date.day.toString2} {date.hourMinute}"

instance : Inhabited HourMinute where
  default : HourMinute := .endOfDay

instance : Inhabited Date where
  default : Date := { year := 2000, month := 1, day := 1, hourMinute := default }



/- SECTION: `ScreamLevel`, `Item`, `Block` -/

/-- Level of screaming. -/
inductive ScreamLevel : Type where
  | silent  : ScreamLevel
  | a       : ScreamLevel
  | aa      : ScreamLevel
  | aaa     : ScreamLevel
  | A       : ScreamLevel
  | AA      : ScreamLevel
  | AAA     : ScreamLevel
  deriving Ord

instance ScreamLevel.instToString : ToString ScreamLevel where
  toString
    | .silent => ""
    | .a      => "a"
    | .aa     => "aa"
    | .aaa    => "aaa"
    | .A      => "A"
    | .AA     => "AA"
    | .AAA    => "AAA"

/-- An item on the todo list. -/
structure Item : Type where
  /-- Brief description of the item. Must contain some non-whitespace character. -/
  title : String
  /-- A list of tags describing what the item is "for". E.g. a uni subject name. -/
  forTags : List String
  /-- A date by which the item must be complete. -/
  byDate : Date
  /-- A date on which to remind the user about the item. -/
  remindMeBy : Option Date := none
  /-- Level of screaming. -/
  screamLevel : ScreamLevel := .silent

-- FIXME: Change this later if needed
instance Item.instToString : ToString Item where
  toString item :=
    let base := s!"[ ] {item.title}\n····FOR: {item.forTags.foldr (· ++ " " ++ ·) ""}\n····BY:  {item.byDate}"
    let screamSuffix := match item.screamLevel with | .silent => "" | _ => s!"\n····SCREAM LEVEL: {item.screamLevel}"
    let remindSuffix := match item.remindMeBy with | .none => "" | .some date => s!"\n····REMIND ME: {date}"
    base ++ screamSuffix ++ remindSuffix

/-- The state of the todo list. -/
structure Dodo : Type where
  /-- Date when the todo list was last updated. -/
  lastWrittenDate : Date
  /-- The items currently held in the todo list. -/
  items : List Item

/-- Mostly for debugging. -/
instance Dodo.instToString : ToString Dodo where
  toString dodo :=
    s!"Last written date: {dodo.lastWrittenDate}\nThe rest of the dodo:\n{dodo.items.map ToString.toString |>.foldr (· ++ "\n" ++ ·) ""}"
