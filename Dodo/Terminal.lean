/-
  **FILE:** `Dodo/Terminal.lean`
  **PURPOSE:** Helper functions to write nicely to the terminal.
-/

/- IMPORTS: -/

import Dodo.Types



/- SECTION: Types and constants that shouldn't be touched -/
namespace Terminal

  /-- `abbrev EscapedSequence := String`. -/
  abbrev EscapedSequence := String

  /-- `abbrev Colour := String`. -/
  abbrev Colour := String
  namespace Colour
    def DARK_WHITE_FG   : Colour := "37"
    def BRIGHT_BLACK_FG : Colour := "90" -- lol ANSI name
    def RED_FG          : Colour := "91"
    def GREEN_FG        : Colour := "92"
    def YELLOW_FG       : Colour := "93"
    def BLUE_FG         : Colour := "94"
    def MAGENTA_FG      : Colour := "95"
    def CYAN_FG         : Colour := "96"
    def WHITE_FG        : Colour := "97"
  end Colour

end Terminal



/- SECTION: (Customisable) global constants -/
namespace Terminal.Colour

  /-- Colour to render `ScreamLevel`s. -/
  def SCREAM_LEVEL : Colour := RED_FG

  /-- Colour to render elapsed `"REMIND ME:"` dates. -/
  def ELAPSED_REMIND_ME : Colour := BLUE_FG

end Terminal.Colour



/- SECTION: Generic terminal utility -/
namespace Terminal

  /-- Escape character. -/
  def ESCAPE : String := "\x1b"
  /-- Open block of special control characters. -/
  def OPEN : String := "["
  /-- Close block of special control characters. -/
  def CLOSE : String := "m"

  /-- Reset terminal state. -/
  def RESET : String := s!"{ESCAPE}{OPEN}0{CLOSE}"

  /-- Write out the given control `sequence`. -/
  def writeControlSequence (sequence : EscapedSequence) : String :=
    s!"{ESCAPE}{OPEN}{sequence}"

  /-- Write some `content` enclosed in the context of a given escaped `sequence` of control characters. -/
  def withEscapeSequence (sequence : EscapedSequence) (content : String) :=
    s!"{ESCAPE}{OPEN}{sequence}{CLOSE}" ++ content ++ RESET

  /-- Write this string to clear the terminal. -/
  def CLEAR_TERMINAL : String :=
    writeControlSequence "2J" ++ writeControlSequence "1;1H"

  /-- Clear the terminal, and reset the cursor to the top left. -/
  def clearTerminal : IO Unit :=
    IO.print CLEAR_TERMINAL

  /-- Write some `content` in bold. -/
  def inBold (content : String) : String :=
    withEscapeSequence "1" content

end Terminal



/- SECTION: Colours -/
namespace Terminal

  /-- Format some `content` enclosed in the context of a given `foreground` and `background` colour. -/
  def withColour (foreground : Colour) (background : Option Colour := none) (content : String) : String :=
    match background with
    | none            => withEscapeSequence foreground content
    | some background => withEscapeSequence s!"{foreground};{background}" content

  /-- Format some `content` enclosed in the context of a given `foreground` colour (but no background colour). -/
  def withFGColour (foreground : Colour) (content : String) : String :=
    withColour foreground none content

  /-- Format a string for "subtle" display on the terminal. -/
  def subtle : String → String :=
    Terminal.withFGColour Terminal.Colour.BRIGHT_BLACK_FG

end Terminal



/- SECTION: Write structures to terminal -/
section «write structures to terminal»

  /-- Format `"····"` for the terminal. -/
  def Indentation.toTerminal : String :=
    Terminal.subtle "····"

  /-- Format the "due day" string for the terminal. -/
  def DueDay.toTerminal : String → String :=
    Terminal.withFGColour Terminal.Colour.GREEN_FG

  /--
    Format the `Item` for the terminal.

    - The `currentDate` is used to conditionally highlight the `"REMIND ME:"` dates based
      on whether they have elapsed.
    - The `index` is used to identify the element in the list.
  -/
  def Item.toTerminal (item : Item) (currentDate : Date) (index : Nat) : String :=
    let base :=
      s!"[{index}] " ++ Terminal.inBold s!"{item.title}" ++ "\n"
      ++ Indentation.toTerminal ++ s!"FOR: {item.forTags.foldr (· ++ " " ++ ·) ""}\n"
      ++ Indentation.toTerminal ++ s!"BY:  {item.byDate}"
    let screamSuffix :=
      match item.screamLevel with
      | .silent => ""
      | _       => "\n" ++ Indentation.toTerminal ++ "SCREAM LEVEL: " ++ Terminal.withFGColour Terminal.Colour.SCREAM_LEVEL s!"{item.screamLevel}"
    let remindSuffix :=
      match item.remindMeBy with
      | .none       => ""
      | .some date  =>
        let elapsed : Bool := compare date currentDate = .lt
        "\n"
        ++ Indentation.toTerminal
        ++ Terminal.withFGColour (if elapsed then Terminal.Colour.ELAPSED_REMIND_ME else Terminal.Colour.BRIGHT_BLACK_FG) (s!"REMIND ME: {date}")
    base ++ screamSuffix ++ remindSuffix

end «write structures to terminal»
