/-
  **FILE:** `Dodo/Parsers.lean`
  **PURPOSE:** Provide parsers for `dodo` types.
-/

/- IMPORTS: -/

import Parser.Basic
import Dodo.Types



/- SECTION: Global constants -/

/-- Marker for end of file. -/
def EXTINCTION : String := "extinction"

/-- Empty checkbox seen in the file. Marks the beginning of an `Item`. -/
def EMPTY_CHECKBOX : String := "[ ]"
/-- Indentation level for sub-details of an `Item`. -/
def INDENTATION : String := "····"
/-- Token delimiting the tags of an `Item`. -/
def FOR : String := "FOR:"
/-- Token delimiting the "by date" of an `Item`. -/
def BY : String := "BY:"
/-- Token delimiting the "scream level" of an `Item`. -/
def SCREAM_LEVEL : String := "SCREAM LEVEL:"
/-- Token delimiting the "remind me date" of an `Item`. -/
def REMIND_ME : String := "REMIND ME:"

/-- Marker delimiting each `DueDay`. -/
def DUE_DAY_MARKER : String := "-- <="

/-- Marker at the top of the file delimiting when the todo list was last updated. -/
def WRITTEN : String := "Written:"



/- SECTION: Dates -/
-- Find associated grammars in `Dodo/Types.lean`.

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



/- SECTION: Parsers for the whole `dodo.md` file -/
section «`dodo.md` grammar» -- yes, this is only in a `section` so that I can fold it in my editor :)
  /-
    Since our parsers are set up generically (i.e. not *just* to process lists of *characters*), we can e.g. have parsers which
    read from a `List String`. This is captured by the types `Parser String α`.
    Before making it to these parsers, the data read from the `dodo.md` file will be split into a `List String`.

    **GRAMMAR:**
      Space denotes newline (i.e. different elements of the `List String` being parsed)
      "Quoted spaces" denote real spaces
      - `Dodo_Md`       ::= `Header` `DueDays` `End`
      - `Header`        ::= "Written: `Date`"
      - `DueDays`       ::= `DueDay` `DueDays`
                            | (potentially empty)
      - `DueDay`        ::= "-- <= `Date`" `Item`
                            | "-- <= `Date`" `Item` `Items`
      - `Items`         ::= `Item` `Items`
                            | (potentially empty)
      - `Item`          ::= "[ ] `Title`"               -- line breaks here are newlines
                            "····FOR: `Tag` `Tags`"
                            "····BY:  `Date`"
                            | "[ ] `Title`"
                              "····FOR: `Tag` `Tags`"
                              "····BY:  `Date`"
                              "····SCREAM LEVEL: `ScreamLevel`"
                            | "[ ] `Title`"
                              "····FOR: `Tag` `Tags`"
                              "····BY:  `Date`"
                              "····REMIND ME: `Date`"
                            | "[ ] `Title`"
                              "····FOR: `Tag` `Tags`"
                              "····BY:  `Date`"
                              "····SCREAM LEVEL: `ScreamLevel`"
                              "····REMIND ME: `Date`"
      - `Tags`          ::= "`Tag` `Tags`"
                            | (possibly empty)
      - `Tag`           ::= `Text`
      - `Title`         ::= `Text`
      - `Text`          ::= (any sequence of characters except `'\t'` and `'\n'`)
      - `Date`          ::= (as per `Dodo/Types.lean`'s `Date`)
      - `ScreamLevel`   ::= a | aa | aaa | A | AA | AAA
      - `End`           ::= extinction

    The `Item`s must all have a "FOR:" field. There is a special `Tag` to go here, called "uncategorised", which catches
    anything that the user hasn't categorised.
  -/
end «`dodo.md` grammar»

/-- Parse the `End` of file. -/
def Parser.end : Parser String Unit := discard <| Parser.tokenLine (Parser.exactlyLine EXTINCTION)

/-- Parse a non-`silent` `ScreamLevel`. -/
def Parser.screamLevel : Parser Char ScreamLevel := do
  (Functor.mapConst .aaa <| token (exactlyString "aaa"))
  <|> (Functor.mapConst .aa <| token (exactlyString "aa"))
  <|> (Functor.mapConst .a <| token (exactlyString "a"))
  <|> (Functor.mapConst .AAA <| token (exactlyString "AAA"))
  <|> (Functor.mapConst .AA <| token (exactlyString "AA"))
  <|> (Functor.mapConst .A <| token (exactlyString "A"))

/-- Parse some (non-empty) `Text`. -/
def Parser.text : Parser Char String := do
  List.asString <$> token (some (oneIf (fun c => c ≠ '\t' ∧ c ≠ '\n')))

/-- Parse a `Title`. -/
def Parser.title : Parser Char String := Parser.text

/-- Parse a `Tag`. -/
def Parser.tag : Parser Char String := text

/-- Parse some `Tags`. -/
def Parser.tags : Parser Char (List String) := some Parser.tag

/-- Parse a line containing the title of some `Item`. -/
def Parser.item.title : Parser String String :=
  tokenLine <| lineParsingVia <| token <| do let _ ← exactlyString "[ ]" ; Parser.title

/--
  Parse a modifying an `Item`'s title.

  Explicitly, this parses a line of the form `····<LABEL><INNERPARSER>`, where `label` matches the
  string token `<LABEL>` and the `innerParser` successfully parses `<INNERPARSER>`.
-/
def Parser.item.child (label : String) (innerParser : Parser Char α) : Parser String α :=
  tokenLine <| lineParsingVia <| (do -- Build one horizontal line...
    discard <| token (exactlyString INDENTATION)
    discard <| token (exactlyString label)
    innerParser
  )

/-- Parse a line containing the `FOR:` clause of some `Item`. -/
def Parser.item.for : Parser String (List String) :=
  Parser.item.child FOR (some (token Parser.tag))

/-- Parse a line containing the `BY:` clause of some `Item`. -/
def Parser.item.by : Parser String Date :=
  Parser.item.child BY (token Parser.date)

/-- Parse the `[ ] Title`, `FOR:` and `BY:` clauses of some `Item`. -/
def Parser.item.essentials : Parser String Item := do
  let title ← Parser.item.title
  let tags ← Parser.item.for
  let date ← Parser.item.by
  pure { title := title, forTags := tags, byDate := date }

/-- Parse a line containing the `SCREAM LEVEL:` clause of some `Item`. -/
def Parser.item.screamLevel : Parser String ScreamLevel :=
  Parser.item.child SCREAM_LEVEL (token Parser.screamLevel)

/-- Parse a line containing the `REMIND ME:` clause of some `Item`. -/
def Parser.item.remindMe : Parser String Date :=
  Parser.item.child REMIND_ME (token Parser.date)

/-- Parse an `Item`. -/
def Parser.item : Parser String Item := do
  let baseItem ← Parser.item.essentials
  let screamLevel ← Parser.item.screamLevel <|> pure .silent
  let remindMe ← (Option.some <$> Parser.item.remindMe) <|> pure .none
  pure { baseItem with screamLevel := screamLevel, remindMeBy := remindMe }

/-- Parse some (≥ 1) `Item`s separated onto new lines. -/
def Parser.items : Parser String (List Item) := some Parser.item

/-- Parse a `DueDay`. -/
def Parser.dueDay : Parser String (Date × List Item) := do
  let date ← tokenLine <| lineParsingVia <| (do
    discard <| token (exactlyString DUE_DAY_MARKER)
    token Parser.date
  )
  let items ← Parser.items
  pure (date, items)

/-- Parse all of the `DueDays`. -/
def Parser.dueDays : Parser String (List (Date × List Item)) := many Parser.dueDay

/-- Parse the header. -/
def Parser.header : Parser String Date :=
  tokenLine <| lineParsingVia (do
    discard <| token (exactlyString WRITTEN)
    token Parser.date
  )

/-- Parse the whole `dodo.md` file. -/
def Parser.dodo : Parser String Dodo := do
  let date ← Parser.header
  let items ← List.flatten <$> List.map Prod.snd <$> Parser.dueDays
  Parser.end
  pure { lastWrittenDate := date, items := items : Dodo }
