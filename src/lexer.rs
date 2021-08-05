use logos::{skip, Lexer, Logos};


fn string(lex: &mut Lexer<Token>) -> String {
    lex.slice().into()
}

fn number(lex: &mut Lexer<Token>) -> Result<f64, std::num::ParseFloatError> {
    lex.slice().parse()
}


#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    // Single-character tokens
    #[token("(")] LeftParen,
    #[token(")")] RightParen,
    #[token("{")] LeftBrace,
    #[token("}")] RightBrace,
    #[token(",")] Comma,
    #[token(".")] Dot,
    #[token("-")] Minus,
    #[token("+")] Plus,
    #[token(";")] Semicolon,
    #[token("/")] Slash,
    #[token("*")] Star,

    // One or two character tokens
    #[token("!")] Bang,
    #[token("!=")] BangEqual,
    #[token("=")] Equal,
    #[token("==")] EqualEqual,
    #[token(">")] Greater,
    #[token(">=")] GreaterEqual,
    #[token("<")] Less,
    #[token("<=")] LessEqual,

    // Literals
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*", string)]
    Identifier(String),
    #[regex(r#""[^"]*""#, string)]
    String(String),
    #[regex(r"[0-9]+(\.[0-9]*)?", number)]
    Number(f64),

    // Keywords
    #[token("and")] And,
    #[token("class")] Class,
    #[token("else")] Else,
    #[token("false")] False,
    #[token("for")] For,
    #[token("fun")] Fun,
    #[token("if")] If,
    #[token("nil")] Nil,
    #[token("not")] Not,
    #[token("or")] Or,
    #[token("print")] Print,
    #[token("return")] Return,
    #[token("super")] Super,
    #[token("this")] This,
    #[token("true")] True,
    #[token("var")] Var,
    #[token("while")] While,

    #[error]
    #[regex(r"[ \t\n\r]+", skip)] // whitespace
    #[regex(r"//[^\n]*", skip)] // comments
    Error,
}
