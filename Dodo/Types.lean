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
  A day in a calendar year.

  Leap years don't exist because I couldn't be bothered to code them in, and frankly my
  life would be better if I reserved Feb. 29th for relaxation...

  **GRAMMAR:**
    - CalendarDay ::= Year Month Day
    - Year        ::= 2000 | 2001 | ⋯
    - Month       ::= 01 | 02 | ⋯ | 12
    - Day         ::= 01 | 02 | ⋯ | 31

  **VALIDATION RULES:** (those not in the above grammar)
    - Day ≤ 31
    - Month = 2 → Day ≤ 28
    - Month ∈ {4, 6, 9, 11} → Day ≤ 30
-/
structure CalendarDay : Type where
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
  deriving Ord

/--
  A date-time pair.

  Remember that leap years don't exist; see `CalendarDay`.

  **GRAMMAR:**
    - Date  ::= Day HourMinute
-/
structure Date extends CalendarDay : Type where
  /-- The hour-minute pair. -/
  hourMinute : HourMinute
  deriving Ord

/-- Convert a `CalendarDate` to a `Date` with the `.hourMinute` given by `.endOfDay`. -/
def CalendarDay.toEndOfDate (day : CalendarDay) : Date := ⟨day, .endOfDay⟩

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

/--
  Compare `Item`s lexicographicaly:
  - first by their `.byDate : Date`;
  - then by their `.screamLevel : ScreamLevel`;
  - then by their `.title : String`.
-/
instance Item.instOrd : Ord Item where
  compare i j :=
    match compare i.byDate j.byDate with
    | .lt => .lt
    | .gt => .gt
    | .eq =>
      match compare i.screamLevel j.screamLevel with
      | .lt => .lt
      | .gt => .gt
      | .eq => compare i.title j.title

/-- Necessary for `head!` to be used on an output of `List.splitBy`. -/
instance Item.instInhabited : Inhabited Item where
  default := { title := "<!> Logic error in program! Whoops!", forTags := [], byDate := default : Item }

/-- The state of the todo list. -/
structure Dodo : Type where
  /-- Date when the todo list was last updated. -/
  lastWrittenDate : Date
  /-- The items currently held in the todo list. -/
  items : List Item

section «simple transformations on `Dodo`s»

  /--
    Sort the `.items` in a `Dodo`.

    TODO: Add a guard to check whether the `.items` are already sorted
  -/
  def Dodo.sortItems : StateM Dodo Unit := do
    set { (←get) with
          items :=
            (←get).items.mergeSort
              (le := fun i j => compare i j == Ordering.lt ∨ compare i j == Ordering.eq)
        }

  /-- Check that `i` and `j` are due by the end of the same day. -/
  def Item.bySameDay (i j : Item) : Bool :=
    i.byDate.day == j.byDate.day

  /--
    Split the list of items by their date.

    REQUIRES: The `Dodo.items` are sorted.
  -/
  def Dodo.splitItemsByDate : StateM Dodo (List (List Item)) := do
    let items := (←get).items
    pure <| items.splitBy Item.bySameDay

  /--
    Get the list of items, split by their date, and with the inner lists tagged with the day by which they are due.

    REQUIRES: The `Dodo.items` are sorted.
  -/
  def Dodo.getDatedItems : StateM Dodo (List (CalendarDay × List Item)) := do
    pure <| (←splitItemsByDate).map (fun items => (items.head!.byDate.toCalendarDay, items))

  def Dodo.getEnumeratedDatedItems : StateM Dodo (List (CalendarDay × List (Nat × Item))) := do
    let enumerateInnerList (innerItems : List Item) : StateM Nat (List (Nat × Item)) := do
      let pureMe := innerItems.enumFrom (←get)
      set <| (←get) + innerItems.length
      pure pureMe
    let enumerate : List (CalendarDay × List Item) → StateM Nat (List (CalendarDay × List (Nat × Item))) :=
      List.foldlM -- This is needlessly slow
        (fun currentList (day, innerList) => do
          let enumeratedInnerList ← enumerateInnerList innerList
          pure <| currentList ++ [(day, enumeratedInnerList)]
        )
        []
    pure ∘ Prod.fst ∘ Id.run <| enumerate (←getDatedItems) 0

end «simple transformations on `Dodo`s»

section «printing `Dodo`s to strings»

  /-- `toString` here writes the `Dodo` in the format that we want to save to file. -/
  instance Dodo.instToString : ToString Dodo where
    toString dodo :=
      let action : StateM Dodo String := do
        sortItems
        let datedItems ← List.map (fun (c, is) => (c.toEndOfDate, is)) <$> getDatedItems
        pure <| datedItems.foldr (fun (date, items) restOfString => s!"-- <= {date}\n" ++ items.foldr (ToString.toString · ++ "\n" ++ ·) "\n" ++ restOfString) ""
      s!"{dodo |> action |>.fst}"

end «printing `Dodo`s to strings»
