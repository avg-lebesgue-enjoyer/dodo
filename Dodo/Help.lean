/-
  **FILE:** `Dodo/Help.lean`
  **PURPOSE:** Provide generic helper functions
-/

/- SECTION: General `String` utilities -/

/-- Removes a trailing `'\n'`. -/
def List.removeNewline : List Char → List Char
  | [] => []
  | ['\n'] => []
  | (c :: cs) => c :: cs.removeNewline

/-- Removes a trailing `'\n'`. -/
def String.removeNewline : String → String :=
  List.asString ∘ List.removeNewline ∘ String.toList

/-- Write a `Nat` to a ≥2-character `String`, prepending a `'0'` if necessary. -/
def Nat.toString2 (n : Nat) : String :=
  if n ≤ 9 then s!"0{n}" else s!"{n}"
