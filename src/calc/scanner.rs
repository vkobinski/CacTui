use super::Token;

enum ScanError {
    SourceEnded,
}

pub(crate) struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
        }
    }

    pub fn scan(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.clone()
    }

    fn scan_token(&mut self) {
        let mut c = self.advance();

        while c == ' ' {
            c = self.advance();
        }

        let token = match c {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            ',' => Token::Comma,
            '.' => Token::Dot,
            ':' => Token::Colon,
            '=' => Token::Equal,
            '>' if self.consume_if('=') => Token::GreaterEqual,
            '<' if self.consume_if('=') => Token::LessEqual,
            '>' => Token::Greater,
            '<' => Token::Less,
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

    fn consume_if(&mut self, c: char) -> bool {
        if self.source.chars().nth(self.current) == Some(c) {
            self.advance();
            true
        } else {
            false
        }
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
        string == "TRUE" || string == "1"
    }
    fn is_false(&self, string: &String) -> bool {
        string == "FALSE" || string == "2"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_string() {
        let tokens = Scanner::new("\"Teste\"".to_string()).scan();
        let compare = Token::Identifier("Teste".to_string());
        assert_eq!(compare, *tokens.first().unwrap());
    }

    #[test]
    fn scan_number() {
        let tokens = Scanner::new("20.32".to_string()).scan();
        let compare = Token::Number(20.32);
        assert_eq!(compare, *tokens.first().unwrap());
    }

    #[test]
    fn scan_less_equal() {
        let tokens = Scanner::new("<= A1".to_string()).scan();
        let compare = Token::LessEqual;
        assert_eq!(compare, *tokens.first().unwrap());
        assert_eq!(Token::Identifier("A1".to_string()), *tokens.get(1).unwrap());
    }

    #[test]
    fn scan_less() {
        let tokens = Scanner::new("<".to_string()).scan();
        let compare = Token::Less;
        assert_eq!(compare, *tokens.first().unwrap());
    }

    #[test]
    fn scan_greater_equal() {
        let tokens = Scanner::new(">=".to_string()).scan();
        let compare = Token::GreaterEqual;
        assert_eq!(compare, *tokens.first().unwrap());
    }

    #[test]
    fn scan_greater() {
        let tokens = Scanner::new(">".to_string()).scan();
        let compare = Token::Greater;
        assert_eq!(compare, *tokens.first().unwrap());
    }

    #[test]
    fn scan_true() {
        let tokens = Scanner::new("TRUE".to_string()).scan();
        let compare = Token::True;
        assert_eq!(compare, *tokens.first().unwrap());
    }

    #[test]
    fn scan_false() {
        let tokens = Scanner::new("FALSE".to_string()).scan();
        let compare = Token::False;
        assert_eq!(compare, *tokens.first().unwrap());
    }

    #[test]
    fn scan_sum() {
        let source = "A1 + A2";
        let resp = vec![
            Token::Identifier("A1".to_string()),
            Token::Plus,
            Token::Identifier("A2".to_string()),
        ];

        let tokens = Scanner::new(source.to_string()).scan();
        assert_eq!(resp, *tokens);
    }

    #[test]
    fn scan_entire() {
        let source = "mul(A1 + A2, 2)";
        let resp = vec![
            Token::Identifier("mul".to_string()),
            Token::LeftParen,
            Token::Identifier("A1".to_string()),
            Token::Plus,
            Token::Identifier("A2".to_string()),
            Token::Comma,
            Token::Number(2.0),
            Token::RightParen,
        ];

        let tokens = Scanner::new(source.to_string()).scan();
        assert_eq!(resp, *tokens);
    }

    #[test]
    fn scan_function_call() {
        let source = "count_if(A1, A2)";
        let resp = vec![
            Token::Identifier("count_if".to_string()),
            Token::LeftParen,
            Token::Identifier("A1".to_string()),
            Token::Comma,
            Token::Identifier("A2".to_string()),
            Token::RightParen,
        ];

        let tokens = Scanner::new(source.to_string()).scan();
        assert_eq!(resp, *tokens);
    }
}
