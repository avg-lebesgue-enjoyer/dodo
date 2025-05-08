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
