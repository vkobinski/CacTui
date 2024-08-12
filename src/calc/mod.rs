pub mod parser;
pub mod scanner;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Identifier(String),
    Number(f32),
    Plus,
    Minus,
    Star,
    Slash,
    Comma,
    Dot,
    Colon,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    True,
    False,
    LeftParen,
    RightParen,
}
