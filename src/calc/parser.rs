use std::{fmt::Display, iter::Peekable, vec::IntoIter};

use crate::Sheet;

use super::{interpreter::Interpreter, scanner::Scanner, Token};

#[derive(PartialEq, Debug, Clone)]
pub(crate) enum Expr {
    Range(Range),
    Call(Call),
    BinOp(BinOp),
    Closure(Closure),
    Literal(Literal),
}

impl Expr {}

type BExpr = Box<Expr>;

#[derive(PartialEq, Debug)]
pub enum ParseError {
    ExpectedToken(String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ParseError::ExpectedToken(t) => format!("Expected {} token.", t),
            //ParseError::UnexpectedEndOfInput => "Input ended before expected.".to_string(),
        };

        f.write_str(str.as_str())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Range {
    pub(crate) from: BExpr,
    pub(crate) to: BExpr,
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Call {
    pub(crate) callee: BExpr,
    pub(crate) args: Vec<BExpr>,
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct BinOp {
    pub(crate) left: BExpr,
    pub(crate) operator: Token,
    pub(crate) right: BExpr,
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Closure {
    pub(crate) args: Vec<BExpr>,
    pub(crate) body: BExpr,
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Unary {
    pub(crate) operator: Token,
    pub(crate) expr: BExpr,
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) enum Literal {
    Number(f64),
    Boolean(bool),
    Identifier(String),
}

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse_string(str: String) -> Result<BExpr, ParseError> {
        let tokens = Scanner::new(str).scan();
        Parser::new(tokens).parse()
    }

    pub fn interpret_string(str: String, sheet: &Sheet) -> Result<String, ParseError> {
        let code = Self::parse_string(str)?;

        let interpreter = Interpreter::new(sheet);
        let inter = interpreter.interpret(&code);

        match inter {
            Ok(ret) => Ok(ret.to_string()),
            Err(err) => Ok(err.to_string()),
        }
    }

    fn parse(&mut self) -> Result<BExpr, ParseError> {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> Result<BExpr, ParseError> {
        self.parse_range()
    }

    fn parse_range(&mut self) -> Result<BExpr, ParseError> {
        let expr = self.parse_binary()?;

        if self.match_token(&Token::Colon) {
            let to = self.parse_binary()?;
            Ok(Box::new(Expr::Range(Range { from: expr, to })))
        } else {
            Ok(expr)
        }
    }

    fn parse_binary(&mut self) -> Result<BExpr, ParseError> {
        let mut expr = self.parse_unary()?;

        while let Some(op) = self.match_binary_op() {
            self.advance();
            let right = self.parse_unary()?;
            expr = Box::new(Expr::BinOp(BinOp {
                left: expr,
                operator: op,
                right,
            }))
        }

        Ok(expr)
    }

    fn match_token(&mut self, token: &Token) -> bool {
        self.tokens
            .next_if(|t| t == token)
            .map_or_else(|| false, |_| true)
    }

    fn consume(&mut self, token: &Token) -> Result<(), ParseError> {
        self.tokens.next_if(|t| t == token).map_or_else(
            || Err(ParseError::ExpectedToken(token.to_string())),
            |_| Ok(()),
        )
    }

    fn parse_unary(&mut self) -> Result<BExpr, ParseError> {
        self.parse_primary()
    }

    fn match_binary_op(&mut self) -> Option<Token> {
        if let Some(token) = self.tokens.peek() {
            return match token {
                Token::Plus
                | Token::Minus
                | Token::Star
                | Token::Slash
                | Token::Equal
                | Token::Greater
                | Token::GreaterEqual
                | Token::Less
                | Token::LessEqual => Some(self.tokens.peek().unwrap().clone()),
                _ => None,
            };
        }

        None
    }

    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn parse_call(&mut self, callee: BExpr) -> Result<BExpr, ParseError> {
        let mut args = Vec::new();

        dbg!(callee.clone());

        if !self.match_token(&Token::RightParen) {
            loop {
                args.push(self.parse_expression()?);
                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }

        self.consume(&Token::RightParen)?;
        Ok(Box::new(Expr::Call(Call { callee, args })))
    }

    fn parse_closure(&mut self) -> Result<BExpr, ParseError> {
        self.consume(&Token::Bar)?;
        let mut args = Vec::new();

        if !self.match_token(&Token::Bar) {
            loop {
                args.push(self.parse_expression()?);
                if !self.match_token(&Token::Comma) {
                    break;
                }
                self.advance();
            }
        }

        self.consume(&Token::Bar)?;
        let expr = self.parse_expression()?;
        Ok(Box::new(Expr::Closure(Closure { body: expr, args })))
    }

    fn parse_primary(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr = match self.tokens.peek() {
            Some(Token::Bar) => self.parse_closure()?,
            Some(Token::Number(n)) => {
                let n = *n;
                self.advance();
                Box::new(Expr::Literal(Literal::Number(n)))
            }
            Some(Token::True) => {
                self.advance();
                Box::new(Expr::Literal(Literal::Boolean(true)))
            }
            Some(Token::False) => {
                self.advance();
                Box::new(Expr::Literal(Literal::Boolean(false)))
            }
            Some(Token::Identifier(s)) => {
                let s = s.clone();
                self.advance();
                Box::new(Expr::Literal(Literal::Identifier(s)))
            }
            Some(Token::LeftParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(&Token::RightParen)?;
                expr
            }
            token => {
                dbg!(token);
                return Err(ParseError::ExpectedToken("Expression".to_string()));
            }
        };

        while self.match_token(&Token::LeftParen) {
            expr = self.parse_call(expr)?;
        }

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {

    use crate::calc::{
        parser::{Call, Range},
        scanner::Scanner,
        Token,
    };

    use super::{BExpr, BinOp, Closure, Expr, Literal, ParseError, Parser};

    fn parse_string(str: String) -> Result<BExpr, ParseError> {
        let tokens = Scanner::new(str).scan();
        Parser::new(tokens).parse()
    }

    #[test]
    fn test_sum() {
        let expr = parse_string("(A1 + A2)".to_string()).unwrap();
        let expected = Expr::BinOp(BinOp {
            left: Box::new(Expr::Literal(Literal::Identifier("A1".to_string()))),
            operator: Token::Plus,
            right: Box::new(Expr::Literal(Literal::Identifier("A2".to_string()))),
        });

        assert_eq!(*expr, expected);
    }

    #[test]
    fn test_call() {
        let expr = parse_string("print((A1 + A2))".to_string()).unwrap();
        let expected = Expr::Call(Call {
            callee: Box::new(Expr::Literal(Literal::Identifier("print".to_string()))),
            args: vec![Box::new(Expr::BinOp(BinOp {
                left: Box::new(Expr::Literal(Literal::Identifier("A1".to_string()))),
                operator: Token::Plus,
                right: Box::new(Expr::Literal(Literal::Identifier("A2".to_string()))),
            }))],
        });

        assert_eq!(*expr, expected);
    }

    #[test]
    fn test_range() {
        let expr = parse_string("(A1:A10)".to_string()).unwrap();
        let expected = Expr::Range(Range {
            from: Box::new(Expr::Literal(Literal::Identifier("A1".to_string()))),
            to: Box::new(Expr::Literal(Literal::Identifier("A10".to_string()))),
        });

        assert_eq!(*expr, expected);
    }

    #[test]
    fn test_call_range() {
        let expr = parse_string("count(A1:A10)".to_string()).unwrap();
        let expected = Expr::Call(Call {
            callee: Box::new(Expr::Literal(Literal::Identifier("count".to_string()))),
            args: vec![Box::new(Expr::Range(Range {
                from: Box::new(Expr::Literal(Literal::Identifier("A1".to_string()))),
                to: Box::new(Expr::Literal(Literal::Identifier("A10".to_string()))),
            }))],
        });

        assert_eq!(*expr, expected);
    }

    #[test]
    fn test_closure() {
        let expr = Parser::parse_string("(|v| v > 2)".to_string()).unwrap();

        let expected = Expr::Closure(Closure {
            args: vec![Box::new(Expr::Literal(Literal::Identifier(
                "v".to_string(),
            )))],
            body: Box::new(Expr::BinOp(BinOp {
                left: Box::new(Expr::Literal(Literal::Identifier("v".to_string()))),
                operator: Token::Greater,
                right: Box::new(Expr::Literal(Literal::Number(2.0))),
            })),
        });

        assert_eq!(*expr, expected);
    }

    #[test]
    fn test_closure_call() {
        let expr = Parser::parse_string("count_if(|v| v > 2)".to_string()).unwrap();

        let expected = Expr::Call(Call {
            callee: Box::new(Expr::Literal(Literal::Identifier("count_if".to_string()))),
            args: vec![Box::new(Expr::Closure(Closure {
                args: vec![Box::new(Expr::Literal(Literal::Identifier(
                    "v".to_string(),
                )))],
                body: Box::new(Expr::BinOp(BinOp {
                    left: Box::new(Expr::Literal(Literal::Identifier("v".to_string()))),
                    operator: Token::Greater,
                    right: Box::new(Expr::Literal(Literal::Number(2.0))),
                })),
            }))],
        });

        assert_eq!(*expr, expected);
    }
}
