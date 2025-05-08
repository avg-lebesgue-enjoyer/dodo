/-
  **FILE:** `Parser/Help.lean`
  **PURPOSE:** Hold helper functions
-/

/- SECTION: `Char.toDigit` -/

/-- `'0' ↦ 0` etc. -/
partial def Char.toDigit : Char → Nat
  | '0' => 0
  | '1' => 1
  | '2' => 2
  | '3' => 3
  | '4' => 4
  | '5' => 5
  | '6' => 6
  | '7' => 7
  | '8' => 8
  | '9' => 9
  | _   => panic! "Trying to convert a non-digit to a digit!"

/-- Convert a list of digits to a `Nat`; e.g. `[1, 2, 3] ↦ 123`. -/
def List.digitsToNat (ds : List Nat) : Nat :=
  ds.reverse.foldr (fun d v => d + 10 * v) 0
