/-
  **FILE:** `Dodo/ApplicationLogic.lean`
  **PURPOSE:** Main application logic.
-/

/- IMPORTS: -/

import Dodo.Help
import Dodo.Terminal
import Dodo.Types
import Dodo.Parsers



/- SECTION: Global constants and abbreviations -/

/-- `abbrev HomeDirectory := System.FilePath` -/
abbrev HomeDirectory := System.FilePath

-- These are honourary constants...
/-- Directory for `dodo` to reserve as its own. -/
def DODO_DIRECTORY : ReaderM HomeDirectory System.FilePath := do pure <| (←read).join "Documents/dodo-test"
/-- File to read todo list from. -/
def DODO_FILE : ReaderM HomeDirectory System.FilePath := do pure <| (←DODO_DIRECTORY).join "dodo.md"
/-- File to write backup to. -/
def DODO_BACKUP_FILE : ReaderM HomeDirectory System.FilePath := do pure <| (←DODO_DIRECTORY).join "dodo-backup.md"

/-- Tin -/
abbrev EXIT_OK : UInt32 := 0
/-- Tin. Not bothered to do more... -/
abbrev EXIT_NOT_OK : UInt32 := 1

/-- Message for user when the file can't be parsed. -/
def FAILED_TO_PARSE : String := "dodo: Failed to parse `dodo.md`! You should probably fix it up manually..."



/- SECTION: Get home directory -/
section «get home directory»

  def FAILED_TO_READ_HOME_DIRECTORY : String :=
    "dodo: Failed to read your home directory by looking up `$HOME`!"

  /-- Get the home directory by looking up `$HOME`. Return `none` if it doesn't exist. -/
  def getHomeDirectory : IO (Option String) := do
    let h ← IO.getEnv "HOME"
    if h.isNone then do
      IO.eprintln FAILED_TO_READ_HOME_DIRECTORY
    pure h

end «get home directory»



/- SECTION: Get date -/
section «get date»

  def DATE_COMMAND : String := "date"
  def DATE_ARGUMENTS : Array String := #["+%Y %m %d %H %M"]

  /-- Get the current date. Return `none` on subtask failure. -/
  def getDate : IO (Option Date) := do
    let output ← IO.Process.output { cmd := DATE_COMMAND, args := DATE_ARGUMENTS }
    if output.exitCode ≠ 0 then
      return none
    pure <| Parser.date.runOnString output.stdout

end «get date»



/- SECTION: Get terminal dimensions -/
section «get terminal dimensions»

  def TERMINAL_LINES_COLUMNS_COMMAND : String := "tput"
  def TERMINAL_LINES_ARGUMENTS : Array String := #["lines"]
  def TERMINAL_COLUMNS_ARGUMENTS : Array String := #["cols"]

  /-- Get the terminal dimensions. Return `none` on subtask failure. -/
  def getTerminalDimensions : IO (Option (Nat × Nat)) := do
    let lines ← IO.Process.output { cmd := TERMINAL_LINES_COLUMNS_COMMAND, args := TERMINAL_LINES_ARGUMENTS }
    if lines.exitCode ≠ 0 then
      return none
    let cols ← IO.Process.output { cmd := TERMINAL_LINES_COLUMNS_COMMAND, args := TERMINAL_COLUMNS_ARGUMENTS }
    if cols.exitCode ≠ 0 then
      return none
    pure <| (·, ·) <$> Parser.nat.runOnString lines.stdout <*> Parser.nat.runOnString cols.stdout

end «get terminal dimensions»



/- SECTION: Back up file -/
section «back up file»

  def BACK_UP_COMMAND : String := "cp"
  def BACK_UP_ARGUMENTS : ReaderM HomeDirectory (Array String) :=
    do pure <| #[DODO_FILE (←read), DODO_BACKUP_FILE (←read)].map toString
  def FAILED_TO_BACK_UP : String :=
    "dodo: Failed to back up previous file! Here's a trace of the `cp` command's `stderr`:"

  /-- Make a backup of the `DODO_FILE` into the `DODO_BACKUP_FILE`. Return `none` on subtask failure. -/
  def makeBackup : ReaderT HomeDirectory IO (Option Unit) := do
    let output ← IO.Process.output { cmd := BACK_UP_COMMAND, args := BACK_UP_ARGUMENTS (←readThe HomeDirectory) }
    if output.exitCode ≠ 0 then
      let stderr ← IO.getStderr
      stderr.putStrLn FAILED_TO_BACK_UP
      stderr.putStrLn s!"{output.stderr}"
      return none
    return some ()

end «back up file»



/- SECTION: Read lines from file -/
section «read file to memory»

  /-- Read the whole file into memory at once. Perhaps not the best plan... -/
  def readFromFile : ReaderT HomeDirectory IO (List String) := do
    (·.splitOn "\n") <$> IO.FS.readFile (DODO_FILE (←readThe HomeDirectory)) -- could `throw`.

end «read file to memory»



/- SECTION: Write `Dodo` to file -/
section «write to file»

  /-- Collect together the string to be written to file. This depends on the current date, which may fail to be obtained. -/
  def Dodo.toOutputString (dodo : Dodo) : IO (Option String) := do
    match (←getDate) with
    | none      => pure none
    | some date => pure s!"{WRITTEN} {date}\n\n{dodo}{EXTINCTION}" -- `s!"{dodo}"` has a trailing `\n`

  /-- Write the given `Dodo` to file. Return `.none` on any error. -/
  def Dodo.writeToFile (dodo : Dodo) : ReaderT HomeDirectory IO (Option Unit) := do
    match (←dodo.toOutputString) with
    | none => pure none
    | some outputString =>
      IO.FS.writeFile (DODO_FILE (←readThe HomeDirectory)) outputString
      pure ()

end «write to file»



/- SECTION: Display `Dodo` data to terminal -/
section «display to terminal»

  /--
    Write the list of items to terminal.

    The `currentDate` is used to conditionally highlight the `"REMIND ME:"` dates.

    REQUIRES: `dodo.items` is sorted.
  -/
  def Dodo.writeToTerminal (dodo : Dodo) (currentDate : Date) : IO Unit := do
    let datedItems : List (Date × List Item) := (fun (c, is) => (c.toEndOfDate, is)) <$> Dodo.getDatedItems.run' dodo
    let mut index : Nat := (datedItems.map (fun (_, items) => items.length) |>.sum) - 1
    for (date, items) in datedItems.reverse do
      IO.println <| DueDay.toTerminal s!"-- <= {date}"
      for item in items.reverse do
        IO.println <| item.toTerminal currentDate index ++ "\n"
        index := index - 1

end «display to terminal»





/- TESTING:: -/

def test : IO UInt32 := do
  match (←getHomeDirectory) with
  | none => return EXIT_NOT_OK
  | some home =>
  try
    match (← Parser.dodo.run <$> readFromFile home) with
    | none =>
      IO.eprintln FAILED_TO_PARSE
      return EXIT_NOT_OK
    | some dodo =>
      match (← dodo.writeToFile home) with
      | none =>
        IO.eprintln "<!> Error writing dodo!"
        return EXIT_NOT_OK
      | some () =>
        IO.eprintln s!"<!> Writing was successful!"
        return EXIT_OK
  catch e =>
    IO.eprintln s!"dodo: Error caught by Lean T^T. Wasn't expected by me... Here it is ;~;!"
    IO.eprintln e.toString
    return EXIT_NOT_OK

-- #eval test
