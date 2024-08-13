use std::fmt::Display;

pub mod parser;
pub mod scanner;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Identifier(String),
    Number(f64),
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

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Token::Identifier(s) => s.to_string(),
            Token::Number(n) => n.to_string(),
            s => String::from(match s {
                Token::Plus => "+",
                Token::Minus => "-",
                Token::Star => "*",
                Token::Slash => "/",
                Token::Comma => ",",
                Token::Dot => ".",
                Token::Colon => ":",
                Token::Equal => "=",
                Token::Greater => ">",
                Token::GreaterEqual => ">=",
                Token::Less => "<",
                Token::LessEqual => "<=",
                Token::True => "TRUE",
                Token::False => "FALSE",
                Token::LeftParen => "(",
                Token::RightParen => ")",
                _ => unreachable!(),
            }),
        };

        f.write_str(string.as_str())
    }
}
