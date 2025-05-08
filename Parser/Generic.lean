/-
  **FILE:** `Parser/Generic.lean`
  **PURPOSE:** Outline a generic `Parser` type, but with no particular examples.
-/

/- SECTION: `Parser` -/

/--
  The type of parsers which parse `α` from `List γ`s.

  `List Char` (`γ := Char`) is easier to deal with than `String`.
-/
def Parser (γ : Type) : Type → Type := StateT (List γ) (OptionT Id)

/-- Run the parser `p` on the string `s`. -/
def Parser.runOnString (p : Parser Char α) (s : String) : Option α :=
  Prod.fst <$> p s.toList

instance Parser.instMonad : Monad (Parser γ) := StateT.instMonad
instance Parser.instMonadStateOf : MonadStateOf (List γ) (Parser γ) where
  get := StateT.get
  set := StateT.set
  modifyGet := StateT.modifyGet
instance Parser.instAlternative : Alternative (Parser γ) := StateT.instAlternative



/- SECTION: Simple parsers -/
namespace Parser

  /-- The parser which always fails. -/
  def failure : Parser γ α := Alternative.failure

  /-- Parse a single `γ`. -/
  def one : Parser γ γ := do
    match (←get) with
    | [] =>
      failure
    | (c :: cs) =>
      set cs
      pure c

  /-- Parse one `γ`, but only if it satisfies `p`. -/
  def oneIf (p : γ → Bool) : Parser γ γ := do
    let c ← one
    if p c then
      pure c
    else
      failure

  /--
    Parse one `γ`, but only if it satisfies `p`.
    Also return a proof that the parsed `γ` satisfies `p`.
  -/
  def oneIf' (p : γ → Bool) : Parser γ ((c : γ) ×' p c) := do
    let c ← one
    if h : p c then
      pure ⟨c, h⟩
    else
      failure

  /-- Parse the character `c` (and fail on any other character). -/
  def exactlyChar [BEq γ] (c : γ) : Parser γ γ :=
    oneIf (· == c)

  /-- Parse the given `List γ` (and fail if this list is not seen). -/
  def exactlyList [BEq γ] : List γ → Parser γ (List γ) :=
    List.foldr
      ((· :: ·) <$> exactlyChar · <*> ·)
      (pure [])

  mutual
    /--
      Repeatedly parse using `p` until it fails; return a list of parsed results.
      *Only succeed if `p` succeeds at least once.*

      Do not propagate the failure.
    -/
    partial def some (p : Parser γ α) : Parser γ (List α) :=
      (· :: ·) <$> p <*> many p

    /--
      Repeatedly parse using `p` until it fails; return a list of parsed results.
      *Always succeeds, but may contain an empty returned list.*

      Do not propagate the failure.
    -/
    partial def many (p : Parser γ α) : Parser γ (List α) := do
      some p <|> pure []
  end

  /--
    Parse as many characters satisfying `p` as possible, and collect them into a
    list.
  -/
  def manyCharsSatisfying (p : γ → Bool) : Parser γ (List γ) :=
    many (oneIf p)

  /-- Parse elements satisfying `p` and discard them. -/
  def discardCharsSatisfying (p : γ → Bool) : Parser γ Unit :=
    discard <| manyCharsSatisfying p

  /--
    Parse a token using the given `tokenParser`, discarding `whitespace` on either
    side.
    The `whitespace` is encoded by a function `γ → Bool` rather than an explicit
    list to check for, because `Char.isWhitespace` is good here.
  -/
  def genericToken (whitespace : γ → Bool) (tokenParser : Parser γ α) : Parser γ α := do
    discardCharsSatisfying whitespace
    let a ← tokenParser
    discardCharsSatisfying whitespace
    pure a

  /--
    Parse a token using the given `tokenParser` (and `whitespace` predicate), and
    return it only if the token satisfies the given `predicate`.
  -/
  def genericTokenSatisfying (tokenParser : Parser γ α) (predicate : α → Bool) (whitespace : γ → Bool) : Parser γ α := do
    let a ← genericToken whitespace tokenParser
    if predicate a then
      pure a
    else
      failure

end Parser



/- SECTION: `γ := Char`-specific -/
namespace Parser

  /-- Parse the given `String` (and fail if this string is not seen). -/
  def exactlyString (s : String) : Parser Char String :=
    List.asString <$> exactlyList s.toList

  /--
    Parse a token using the given `tokenParser`, discarding whitespace on either
    side. Whitespace is determined using `Char.isWhitespace`.
  -/
  def token (tokenParser : Parser Char α) : Parser Char α :=
    tokenParser.genericToken Char.isWhitespace

  /--
    Parse a token using the given `tokenParser`, and return it only if the token
    satisfies the given `predicate`.
  -/
  def tokenSatisfying (tokenParser : Parser Char α) (predicate : α → Bool) : Parser Char α :=
    tokenParser.genericTokenSatisfying predicate Char.isWhitespace

end Parser
