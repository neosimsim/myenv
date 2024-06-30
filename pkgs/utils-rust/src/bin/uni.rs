use std::collections::HashMap;
use std::io::{stdin, stdout, Read, Write};

#[cfg(test)]
use pretty_assertions::assert_eq;

fn main() -> Result<(), String> {
    uni(&mut stdin(), &mut stdout()).map_err(|err| err.to_string())
}

fn keywords() -> HashMap<Vec<u8>, Vec<u8>> {
    let keywords = [
        // Superscripts
        ("^0", "⁰"),
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
        // Subscripts
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
        // Arrows
        ("->", "→"),
        ("<--", "←"),
        ("<-->", "↔"),
        ("=>", "⇒"),
        ("<=", "⇐"),
        ("<=>", "⇔"),
        // Symbols from mathematics and logic, LaTeX style
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
        ("ell", "ℓ"),
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
        // Greek alphabet…
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
        // musical
        ("#", "♯"),
        ("sharp", "♯"),
        ("flat", "♭"),
        // smiley
        (":)", "☺"),
        ("XD", "😁"),
        (";)", "😉"),
        // German umlauts and ß
        ("ae", "ä"),
        ("Ae", "Ä"),
        ("oe", "ö"),
        ("Oe", "Ö"),
        ("ue", "ü"),
        ("Ue", "Ü"),
        ("ss", "ß"),
        ("Ss", "ẞ"),
        // misc
        (",", " "), // Narrow No-Break Space
        ("~", " "), // No-Break Space
        ("'", "́"),  // Combining Acute Accent, e. g. é
        ("^", "̂"),  // Combining Circumflex Accent, e. g. ê
        ("`", "̀"),  // Combining Grave Accent, e. g. è
        ("\"", "̈"), // Combining Diaeresis
        ("--", "–"),
        ("---", "—"),
        ("prime", "′"),
        ("''", "″"),
        ("'''", "‴"),
        ("''''", "⁗"),
        ("apos", "ʼ"),
        ("degree", "°"),
        ("check", "✔"),
    ];
    keywords
        .into_iter()
        .map(|(k, v)| (k.into(), v.into()))
        .collect()
}

fn uni<T, W>(input: &mut T, writer: &mut W) -> Result<(), String>
where
    T: Read,
    W: Write,
{
    let mut buffer = [0; 1];
    let mut delim: Option<u8> = None;
    let keywords = keywords();
    loop {
        match delim {
            None => {
                let size = input.read(&mut buffer).map_err(|err| err.to_string())?;
                if size == 0 {
                    return Ok(());
                }
            }
            Some(d) => {
                buffer[0] = d;
                delim = None;
            }
        };
        if buffer == [b'\\'] {
            let (keyword, new_delim) = read_keyword(input)?;
            delim = new_delim;
            match keywords.get(&keyword) {
                None => {
                    writer.write(b"\\").map_err(|err| err.to_string())?;
                    writer.write(&keyword).map_err(|err| err.to_string())?;
                }
                Some(special) => {
                    writer.write(special).map_err(|err| err.to_string())?;
                }
            };
        } else {
            writer.write(&buffer).map_err(|err| err.to_string())?;
        }
    }
}

fn read_keyword<T>(input: &mut T) -> Result<(Vec<u8>, Option<u8>), String>
where
    T: Read,
{
    let mut keyword = Vec::new();
    let mut buffer = [0; 1];
    loop {
        let size = input.read(&mut buffer).map_err(|err| err.to_string())?;
        if size == 0 {
            return Ok((keyword, None));
        } else if buffer == *b"\\" || buffer == *b" " || buffer == *b"\n" {
            return Ok((keyword, Some(buffer[0])));
        } else if buffer == *b"{" {
            let size = input.read(&mut buffer).map_err(|err| err.to_string())?;
            if size == 0 {
                return Err(String::from("Expected closing '}'"));
            }
            if buffer != *b"}" {
                return Err(format!("Expected closing '}}' got '{}'", buffer[0]));
            }
            return Ok((keyword, None));
        } else {
            keyword.append(&mut buffer[..size].to_vec());
        }
    }
}

#[test]
fn test_read_keyword() {
    let mut input = "foo".as_bytes();
    assert_eq!(read_keyword(&mut input), Ok(("foo".into(), None)))
}

#[test]
fn test_read_keyword_backslash_delim() {
    let mut input = r"foo\bar".as_bytes();
    assert_eq!(read_keyword(&mut input), Ok(("foo".into(), Some(b'\\'))));
    let mut rest = String::new();
    input.read_to_string(&mut rest).unwrap();
    assert_eq!(rest, "bar")
}

#[test]
fn test_read_keyword_space_delim() {
    let mut input = "foo bar".as_bytes();
    assert_eq!(read_keyword(&mut input), Ok(("foo".into(), Some(b' '))));
    let mut rest = String::new();
    input.read_to_string(&mut rest).unwrap();
    assert_eq!(rest, "bar");
}

#[test]
fn test_read_keyword_newline_delim() {
    let mut input = "foo\nbar".as_bytes();
    assert_eq!(read_keyword(&mut input), Ok(("foo".into(), Some(b'\n'))));
    let mut rest = String::new();
    input.read_to_string(&mut rest).unwrap();
    assert_eq!(rest, "bar")
}

#[test]
fn test_read_keyword_braces_delim() {
    let mut input = "foo{}bar".as_bytes();
    assert_eq!(read_keyword(&mut input), Ok(("foo".into(), None)));
    let mut rest = String::new();
    input.read_to_string(&mut rest).unwrap();
    assert_eq!(rest, "bar")
}

#[test]
fn test_simple_uni() {
    let input = r"\^0";
    let mut writer = Vec::new();
    uni(&mut input.as_bytes(), &mut writer).unwrap();
    assert_eq!(String::from_utf8(writer).unwrap(), "⁰");
}

#[test]
fn test_uni_backslash_separated() {
    let input = r"\^0\^1";
    let mut writer = Vec::new();
    uni(&mut input.as_bytes(), &mut writer).unwrap();
    assert_eq!(String::from_utf8(writer).unwrap(), "⁰¹");
}
#[test]
fn test_uni_space_separated() {
    let input = r"\^0 \^1";
    let mut writer = Vec::new();
    uni(&mut input.as_bytes(), &mut writer).unwrap();
    assert_eq!(String::from_utf8(writer).unwrap(), "⁰ ¹");
}

#[test]
fn test_uni_text() {
    let input = r"
Superscripts
\^0
\^1
\^2
";
    let expected = r"
Superscripts
⁰
¹
²
";

    let mut writer = Vec::new();
    uni(&mut input.as_bytes(), &mut writer).unwrap();
    assert_eq!(String::from_utf8(writer).unwrap(), expected);
}

#[test]
fn test_uni() {
    let input = r"
Superscripts
\^0
\^1
\^2
\^3
\^4
\^5
\^6
\^7
\^8
\^9
\^+
\^-
\^=
\^(
\^)
\^n

Subscripts
\_0
\_1
\_2
\_3
\_4
\_5
\_6
\_7
\_8
\_9
\_+
\_-
\_=
\_(
\_)

Arrows
\->
\<--
\<-->
\=>
\<=
\<=>

Symbols from mathematics and logic, LaTeX style
\forall
\exists
\in
\ni
\empty
\prod
\sum
\le
\ge
\pm
\subset
\subseteq
\supset
\supseteq
\setminus
\cap
\cup
\int
\therefore
\qed
\1
\N
\Z
\C
\Q
\R
\E
\F
\to
\mapsto
\infty
\cong
\:=
\=:
\ne
\approx
\perp
\not
\ldots
\cdots
\cdot
\circ
\times
\oplus
\langle
\rangle

Greek alphabet…
\alpha
\beta
\gamma
\delta
\epsilon
\zeta
\eta
\theta
\iota
\kappa
\lambda
\mu
\nu
\xi
\omicron
\pi
\rho
\stigma
\sigma
\tau
\upsilon
\phi
\varphi
\chi
\psi
\omega

\Alpha
\Beta
\Gamma
\Delta
\Epsilon
\Zeta
\Eta
\Theta
\Iota
\Kappa
\Lambda
\Mu
\Nu
\Xi
\Omicron
\Pi
\Rho
\Sigma
\Tau
\Upsilon
\Phi
\Chi
\Psi
\Omega

Usage within text:
\lambda-Calc
\lambda{}-Calc
\lambdaCalc
\lambda{}Calc
\lambda\Lambda
\lambda \Lambda
Was macht das \lambda und wo kommt das \phi her?
";

    let expected = r"
Superscripts
⁰
¹
²
³
⁴
⁵
⁶
⁷
⁸
⁹
⁺
⁻
⁼
⁽
⁾
ⁿ

Subscripts
₀
₁
₂
₃
₄
₅
₆
₇
₈
₉
₊
₋
₌
₍
₎

Arrows
→
←
↔
⇒
⇐
⇔

Symbols from mathematics and logic, LaTeX style
∀
∃
∈
∋
∅
∏
∑
≤
≥
±
⊂
⊆
⊃
⊇
∖
∩
∪
∫
∴
∎
𝟙
ℕ
ℤ
ℂ
ℚ
ℝ
𝔼
𝔽
→
↦
∞
≅
≔
≕
≠
≈
⊥
̷
…
⋯
⋅
◦
×
⊕
⟨
⟩

Greek alphabet…
α
β
γ
δ
ε
ζ
η
θ
ι
κ
λ
μ
ν
ξ
ο
π
ρ
ς
σ
τ
υ
ϕ
φ
χ
ψ
ω

Α
Β
Γ
Δ
Ε
Ζ
Η
Θ
Ι
Κ
Λ
Μ
Ν
Ξ
Ο
Π
Ρ
Σ
Τ
Υ
Φ
Χ
Ψ
Ω

Usage within text:
\lambda-Calc
λ-Calc
\lambdaCalc
λCalc
λΛ
λ Λ
Was macht das λ und wo kommt das ϕ her?
";

    let mut writer = Vec::new();
    uni(&mut input.as_bytes(), &mut writer).unwrap();
    assert_eq!(String::from_utf8(writer).unwrap(), expected);
}
