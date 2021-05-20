{-# LANGUAGE OverloadedStrings #-}

-- to test run:  echo "\lambda(x.x) \Rightarrow \lambda{}-calc" | ./tex2text # = λ(x.x) ⇒ λ-calc
import Control.Monad as Monad
import qualified Data.ByteString as B
import Data.Either.Combinators
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.Text.IO as Text (putStr, putStrLn)
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Megaparsec as P
import Text.Megaparsec.Char as P

mapping :: [(Text, Text)]
mapping =
  -- Superscripts
  [ ("^0", "⁰"),
    ("^1", "¹"),
    ("^2", "²"),
    ("^3", "³"),
    ("^4", "⁴"),
    ("^5", "⁵"),
    ("^6", "⁶"),
    ("^7", "⁷"),
    ("^8", "⁸"),
    ("^9", "⁹"),
    ("^+", "⁺"),
    ("^-", "⁻"),
    ("^=", "⁼"),
    ("^(", "⁽"),
    ("^)", "⁾"),
    ("^n", "ⁿ"),
    ("^r", "ʳ"),
    -- Subscripts
    ("_0", "₀"),
    ("_1", "₁"),
    ("_2", "₂"),
    ("_3", "₃"),
    ("_4", "₄"),
    ("_5", "₅"),
    ("_6", "₆"),
    ("_7", "₇"),
    ("_8", "₈"),
    ("_9", "₉"),
    ("_+", "₊"),
    ("_-", "₋"),
    ("_=", "₌"),
    ("_(", "₍"),
    ("_)", "₎"),
    -- Arrows
    ("->", "→"),
    ("<--", "←"),
    ("<-->", "↔"),
    ("=>", "⇒"),
    ("<=", "⇐"),
    ("<=>", "⇔"),
    -- Symbols from mathematics and logic, LaTeX style
    ("forall", "∀"),
    ("exists", "∃"),
    ("neg", "¬"),
    ("in", "∈"),
    ("ni", "∋"),
    ("land", "∧"),
    ("lor", "∨"),
    ("empty", "∅"),
    ("prod", "∏"),
    ("sum", "∑"),
    ("le", "≤"),
    ("ge", "≥"),
    ("pm", "±"),
    ("subset", "⊂"),
    ("subseteq", "⊆"),
    ("supset", "⊃"),
    ("supseteq", "⊇"),
    ("setminus", "∖"),
    ("cap", "∩"),
    ("cup", "∪"),
    ("uplus", "⊎"),
    ("int", "∫"),
    ("therefore", "∴"),
    ("qed", "∎"),
    ("1", "𝟙"),
    ("N", "ℕ"),
    ("Z", "ℤ"),
    ("C", "ℂ"),
    ("Q", "ℚ"),
    ("R", "ℝ"),
    ("B", "𝔹"),
    ("E", "𝔼"),
    ("F", "𝔽"),
    ("to", "→"),
    ("mapsto", "↦"),
    ("infty", "∞"),
    ("cong", "≅"),
    ("=", "≡"),
    ("=:", "≕"),
    ("ne", "≠"),
    ("approx", "≈"),
    ("top", "⊤"),
    ("bot", "⊥"),
    ("perp", "⊥"),
    ("not", "̷"),
    ("ldots", "…"),
    ("cdots", "⋯"),
    ("cdot", "⋅"),
    ("circ", "◦"),
    ("times", "×"),
    ("oplus", "⊕"),
    ("langle", "⟨"),
    ("<", "⟨"),
    ("rangle", "⟩"),
    (">", "⟩"),
    ("=<>", "≡⟨⟩"),
    ("::", "∷"),
    (":=", "≔"),
    ("=?", "≟"),
    ("{{", "⦃"),
    ("}}", "⦄"),
    (">>", "≫"),
    (">>=", "≫="),
    ("<<", "≪"),
    ("=<<", "=≪"),
    -- Greek alphabet…
    ("alpha", "α"),
    ("beta", "β"),
    ("gamma", "γ"),
    ("delta", "δ"),
    ("epsilon", "ε"),
    ("zeta", "ζ"),
    ("eta", "η"),
    ("theta", "θ"),
    ("iota", "ι"),
    ("kappa", "κ"),
    ("lambda", "λ"),
    ("mu", "μ"),
    ("nu", "ν"),
    ("xi", "ξ"),
    ("omicron", "ο"),
    ("pi", "π"),
    ("rho", "ρ"),
    ("stigma", "ς"),
    ("sigma", "σ"),
    ("tau", "τ"),
    ("upsilon", "υ"),
    ("phi", "ϕ"),
    ("varphi", "φ"),
    ("chi", "χ"),
    ("psi", "ψ"),
    ("omega", "ω"),
    ("Alpha", "Α"),
    ("Beta", "Β"),
    ("Gamma", "Γ"),
    ("Delta", "Δ"),
    ("Epsilon", "Ε"),
    ("Zeta", "Ζ"),
    ("Eta", "Η"),
    ("Theta", "Θ"),
    ("Iota", "Ι"),
    ("Kappa", "Κ"),
    ("Lambda", "Λ"),
    ("Mu", "Μ"),
    ("Nu", "Ν"),
    ("Xi", "Ξ"),
    ("Omicron", "Ο"),
    ("Pi", "Π"),
    ("Rho", "Ρ"),
    ("Sigma", "Σ"),
    ("Tau", "Τ"),
    ("Upsilon", "Υ"),
    ("Phi", "Φ"),
    ("Chi", "Χ"),
    ("Psi", "Ψ"),
    ("Omega", "Ω"),
    -- musical
    ("#", "♯"),
    ("sharp", "♯"),
    ("flat", "♭"),
    -- smiley
    (":)", "☺"),
    ("XD", "😁"),
    (";)", "😉"),
    -- misc
    ("ell", "ℓ"),
    ("ae", "ä"),
    ("Ae", "Ä"),
    ("oe", "ö"),
    ("Oe", "Ö"),
    ("ue", "ü"),
    ("Ue", "Ü"),
    ("ss", "ß"),
    ("Ss", "ẞ"),
    (",", " "), -- Narrow No-Break Space
    ("~", " "), -- No-Break Space
    ("'", "́"), -- Combining Acute Accent, e. g. é
    ("^", "̂"), -- Combining Circumflex Accent, e. g. ê
    ("`", "̀"), -- Combining Grave Accent, e. g. è
    ("\"", "̈"), -- Combining Diaeresis
    ("--", "–"),
    ("---", "—"),
    ("prime", "′"),
    ("''", "″"),
    ("'''", "‴"),
    ("''''", "⁗"),
    ("apos", "ʼ"),
    ("degree", "°")
  ]

data Fragment
  = Plain Text
  | Escaped Text
  deriving (Show)

type Partition = [Fragment]

resolve :: [(Text, Text)] -> Fragment -> Text
resolve _ (Plain t) = t
resolve m (Escaped e) =
  fromMaybe ("\\" `Text.append` e) (M.lookup e $ M.fromList m)

type Parser = Parsec Void String

pPlain :: Parser Fragment
pPlain = Plain . Text.pack <$> manyTill anySingle (lookAhead (void (char '\\')) <|> eof)

-- | An escaped fragment is a word preceded by a backslash '\' and terminated by a space or "{}".
--  An optional {} allows an escaped term to be followed by plain text
-- without any whitespace in between. If the fragment is derminated by
-- ent is derminated byway.
pEscaped :: Parser Fragment
pEscaped = do
  fragment <-
    Escaped . Text.pack
      <$> (P.char '\\' >> P.manyTill anySingle (lookAhead (void (string "{}")) <|> lookAhead (void (P.char '\\')) <|> lookAhead (void spaceChar) <|> eof))
  void . P.observing $ string "{}"
  return fragment

pFragment :: Parser Fragment
pFragment = pEscaped <|> pPlain

pPartition :: Parser Partition
pPartition = do
  end <- atEnd
  if end
    then return []
    else (:) <$> pFragment <*> pPartition

parsePartition :: String -> Either Text Partition
parsePartition = mapLeft (Text.pack . errorBundlePretty) . P.parse pPartition ""

main :: IO ()
main = do
  args <- listToMaybe <$> getArgs
  case args of
    Nothing -> decodeInput
    Just "decode" -> decodeInput
    Just "print" -> printMap mapping
    Just cmd -> die $ "unknown command '" ++ cmd ++ "'"

printMap :: [(Text, Text)] -> IO ()
printMap [] = return ()
printMap ((k, v) : xs) = do
  Text.putStr k
  Text.putStr " "
  Text.putStrLn v
  printMap xs

decodeInput :: IO ()
decodeInput = do
  input <- getContents
  case parsePartition input of
    Left e -> Text.putStrLn e
    Right part ->
      B.putStr . encodeUtf8 . Text.concat $ map (resolve mapping) part
