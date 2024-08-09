use core::panic;

#[derive(Clone, PartialEq, Debug)]
enum Token {
    Identifier(String),
    Plus,
    Minus,
    Star,
    Slash,
    Comma,
    Dot,
    True,
    False,
}

enum ParseError {
    SourceEnded,
}

struct Parser {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
}

impl Parser {
    fn new(source: String) -> Self {
        Self {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
        }
    }

    fn parse(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        return self.tokens.clone();
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        let token = match c {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            ',' => Token::Comma,
            '.' => Token::Dot,
            '"' => self.string(),
            c => {
                if c.is_digit(10) {
                    self.number()
                } else if c.is_alphabetic() {
                    self.identifier()
                }
            }
        };

        self.tokens.push(token);
    }

    fn string(&mut self) -> Token {
        let mut chars = vec![];

        while self.peek() != '"' && !self.is_at_end() {
            chars.push(self.advance());
        }

        Token::Identifier(chars.into_iter().collect())
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source.chars().nth(self.current).unwrap()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn is_at_end(&self) -> bool {
        if self.current >= self.source.len() {
            return true;
        }
        false
    }

    fn number(&self) -> Token {
        todo!()
    }

    fn identifier(&self) -> Token {
        todo!()
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;

    #[test]
    fn parse_string() {
        let tokens = Parser::new("\"Teste\"".to_string()).parse();
        let compare = Token::Identifier("Teste".to_string());
        assert_eq!(compare, *tokens.first().unwrap());
    }
}
