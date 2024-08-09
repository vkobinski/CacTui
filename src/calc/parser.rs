#[derive(Clone, PartialEq, Debug)]
enum Token {
    Identifier(String),
    Number(f32),
    Plus,
    Minus,
    Star,
    Slash,
    Comma,
    Dot,
    True,
    False,
    LeftParen,
    RightParen,
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
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '"' => self.string(),
            c => {
                if c.is_ascii_digit() {
                    self.number(c)
                } else {
                    self.identifier(c)
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

    fn number(&mut self, c: char) -> Token {
        let mut chars = vec![c];

        while self.peek().is_ascii_digit() || self.peek() == '.' {
            chars.push(self.advance());
        }

        let string_number: String = chars.into_iter().collect();

        Token::Number(string_number.parse().unwrap())
    }

    fn identifier(&mut self, c: char) -> Token {
        let mut chars = vec![c];

        while self.peek().is_alphanumeric() || self.peek() == '_' {
            chars.push(self.advance());
        }

        let identifier: String = chars.into_iter().collect::<String>().trim().to_string();

        match identifier {
            t if self.is_true(&t) => Token::True,
            f if self.is_false(&f) => Token::False,
            _ => Token::Identifier(identifier),
        }
    }

    fn is_true(&self, string: &String) -> bool {
        string == "TRUE"
    }
    fn is_false(&self, string: &String) -> bool {
        string == "FALSE"
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

    #[test]
    fn parse_number() {
        let tokens = Parser::new("20.32".to_string()).parse();
        let compare = Token::Number(20.32);
        assert_eq!(compare, *tokens.first().unwrap());
    }

    #[test]
    fn parse_true() {
        let tokens = Parser::new("TRUE".to_string()).parse();
        let compare = Token::True;
        assert_eq!(compare, *tokens.first().unwrap());
    }

    #[test]
    fn parse_false() {
        let tokens = Parser::new("FALSE".to_string()).parse();
        let compare = Token::False;
        assert_eq!(compare, *tokens.first().unwrap());
    }

    #[test]
    fn parse_function_call() {
        let source = "count_if(A1, A2)";
        let resp = vec![
            Token::Identifier("count_if".to_string()),
            Token::LeftParen,
            Token::Identifier("A1".to_string()),
            Token::Comma,
            Token::Identifier("A2".to_string()),
            Token::RightParen,
        ];

        let tokens = Parser::new(source.to_string()).parse();
        assert_eq!(resp, *tokens);
    }
}
