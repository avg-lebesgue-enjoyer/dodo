/-
  **FILE:** `Dodo/Basic.lean`
  **PURPOSE:** Root of the `Dodo` module
  **MODULE CONTENTS:**
    The main "todo list" application logic
  **MODULE TODO LIST:**
    [ ] Write code to print a `Dodo` to a string in the format we would like it to go to file.
    [ ] Write code to actually write the `Dodo` to file. (including the "Written: `Date`" and "extinction")
    [ ] Write code to handle actions:
      [ ] (Print items to screen)
        [ ] (And print a copy of those things for which the "remind me" date is in the past)
        [ ] (Print stuff with pretty syntax highlighting!)
      [ ] Filter known items      -- `:t`itle, `:f`or, `:b`y, due `:a`fter, `:s`cream level, `:r`eset filtering
        [ ] (And only print what was filtered for)
      [ ] Add new item (`:+`, `:=`)
      [ ] Remove item (`:-`, `:_`)
        [ ] "Cursor" system to select between displayed items?
      [ ] Edit item (`:e`, `:~`)
      [ ] `:w`
      [ ] `:wq`
      [ ] `:qa!`
    [ ] Strengthen `Dodo/Types.lean` to make guarantees in the type system (such as "this date is valid")
-/

/- IMPORTS: -/

import Dodo.Types -- <!> DEBUG
import Dodo.Parsers -- <!> DEBUG
