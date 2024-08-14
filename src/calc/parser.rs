use std::{collections::HashMap, fmt::Display, iter::Peekable, vec::IntoIter};

use ratatui::buffer::Cell;

use crate::{CellValue, Sheet};

use super::{scanner::Scanner, Token};

#[derive(PartialEq, Debug, Clone)]
enum Expr {
    Range(Range),
    Call(Call),
    BinOp(BinOp),
    Literal(Literal),
}

#[derive(PartialEq, Debug, Clone)]
enum Interpret {
    Number(f64),
    Text(String),
    Values(Vec<Interpret>),
    Bool(bool),
}

#[derive(PartialEq, Debug, Clone)]
enum InterpretError {
    BinOp,
    InvalidOperator,
    Range,
    Cell,
    FunctionNotExist,
    InvalidFunction,
    InvalidArgument,
}

impl Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            InterpretError::BinOp => "Could not parse Binary Operation",
            InterpretError::InvalidOperator => "Invalid Operator in Binary Operation",
            InterpretError::Range => "Invalid Range",
            InterpretError::Cell => "Blank cell",
            InterpretError::FunctionNotExist => "Function does not exist",
            InterpretError::InvalidFunction => "Trying to call a function with invalid name",
            InterpretError::InvalidArgument => "Invalid argument for function",
        })
    }
}

struct FunctionRegistry {
    functions: HashMap<String, Box<dyn Fn(&[Interpret]) -> Result<Interpret, InterpretError>>>,
}

impl FunctionRegistry {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    fn register<F>(&mut self, name: &str, func: F)
    where
        F: Fn(&[Interpret]) -> Result<Interpret, InterpretError> + 'static,
    {
        self.functions.insert(name.to_string(), Box::new(func));
    }

    fn call(&self, name: &str, args: &[Interpret]) -> Result<Interpret, InterpretError> {
        self.functions
            .get(name)
            .ok_or(InterpretError::FunctionNotExist)
            .and_then(|f| f(args))
    }
}

fn count(args: &[Interpret]) -> Result<Interpret, InterpretError> {
    let sum = args.iter().try_fold(0.0, |acc, arg| {
        if let Interpret::Number(n) = arg {
            Ok(acc + n)
        } else {
            Err(InterpretError::InvalidArgument)
        }
    })?;
    Ok(Interpret::Number(sum))
}
pub struct Interpreter {
    function_registry: FunctionRegistry,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut registry = FunctionRegistry::new();
        registry.register("count", count);
        Self {
            function_registry: registry,
        }
    }

    //fn interpret_call(&self, call: &Call) -> Result<Interpret, InterpretError> {
    //    let args = call
    //        .args
    //        .iter()
    //        .map(|arg| self.interpret(arg))
    //        .collect::<Result<Vec<_>, _>>()?;
    //    if let Expr::Literal(Literal::Identifier(id)) = &*call.callee {
    //        self.function_registry.call(id, &args)
    //    } else {
    //        Err(InterpretError::InvalidFunction)
    //    }
    //}
}

impl Display for Interpret {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Interpret::Number(n) => write!(f, "{}", n),
            Interpret::Text(t) => write!(f, "{}", t),
            Interpret::Values(v) => f.write_str(
                &v.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(","),
            ),
            Interpret::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl Expr {
    fn parse_literal(&self, lit: String) -> Option<(usize, usize)> {
        let mut start = lit.chars();
        let letter = start.next();
        let num = start.next();

        if let (Some(letter), Some(num)) = (letter, num) {
            if letter.is_ascii_uppercase() && letter.is_ascii_alphabetic() {
                let first = (letter as u32) - ('A' as u32);
                let second = num.to_digit(10).unwrap() - 1;

                return Some((first.try_into().unwrap(), second.try_into().unwrap()));
            }
        }

        None
    }

    fn interpret(&self, sheet: &Sheet, inter: &Interpreter) -> Result<Interpret, InterpretError> {
        match self {
            Expr::Range(r) => {
                let (from, to) = (r.from.clone(), r.to.clone());

                if let (
                    Expr::Literal(Literal::Identifier(from)),
                    Expr::Literal(Literal::Identifier(to)),
                ) = ((*from), *(to))
                {
                    let from = self.parse_literal(from);
                    let to = self.parse_literal(to);

                    if let (Some(from), Some(to)) = (from, to) {
                        if from.0 == to.0 {
                            let mut cells: Vec<f64> = vec![];

                            for i in (from.1)..(to.1 + 1) {
                                let cur = sheet.get(&(from.0, i)).unwrap();

                                if let CellValue::Number(n) = cur.val {
                                    cells.push(n);
                                }
                            }

                            return Ok(Interpret::Values(
                                cells.iter().map(|v| Interpret::Number(*v)).collect(),
                            ));
                        }
                    }
                }

                Err(InterpretError::Range)
            }
            Expr::Call(call) => {
                let calle = call.callee.clone();

                if let Expr::Literal(Literal::Identifier(id)) = *calle {
                    match id.as_str() {
                        "count" => {
                            let args = call.args.first().unwrap().interpret(sheet, &inter);

                            if let Ok(Interpret::Values(args)) = args {
                                return Ok(Interpret::Number(args.iter().fold(0.0, |l, arg| {
                                    if let Interpret::Number(a) = arg {
                                        return l + a;
                                    }

                                    l
                                })));
                            }
                        }
                        _ => return Err(InterpretError::FunctionNotExist),
                    }
                }

                Err(InterpretError::InvalidFunction)
            }
            Expr::BinOp(bin_op) => {
                let (left, op, right) = (
                    bin_op.left.clone(),
                    bin_op.operator.clone(),
                    bin_op.right.clone(),
                );

                let left = left.interpret(sheet, inter);
                let right = right.interpret(sheet, inter);

                if let (Ok(Interpret::Number(left)), Ok(Interpret::Number(right))) = (left, right) {
                    return Ok(match op {
                        Token::Plus | Token::Minus | Token::Star | Token::Slash => {
                            Interpret::Number(match op {
                                Token::Plus => left + right,
                                Token::Minus => left - right,
                                Token::Star => left * right,
                                Token::Slash => left / right,
                                _ => unreachable!(),
                            })
                        }
                        Token::Greater
                        | Token::GreaterEqual
                        | Token::Less
                        | Token::LessEqual
                        | Token::EqualEqual => Interpret::Bool(match op {
                            Token::Greater => left > right,
                            Token::GreaterEqual => left >= right,
                            Token::Less => left < right,
                            Token::LessEqual => left <= right,
                            Token::EqualEqual => left == right,
                            _ => unreachable!(),
                        }),
                        _ => return Err(InterpretError::InvalidOperator),
                    });
                }

                Err(InterpretError::BinOp)
            }
            Expr::Literal(lil) => match lil {
                Literal::Number(n) => Ok(Interpret::Number(*n)),
                Literal::Boolean(b) => Ok(Interpret::Bool(*b)),
                Literal::Identifier(i) => match self.parse_literal(i.to_string()) {
                    Some((a, b)) => sheet.get(&(a, b)).map_or_else(
                        || Err(InterpretError::Cell),
                        |c| {
                            if let CellValue::Number(n) = c.val {
                                Ok(Interpret::Number(n))
                            } else if let CellValue::Formula(_, s) = &c.val {
                                Ok(Interpret::Number(s.parse().unwrap()))
                            } else {
                                Err(InterpretError::Cell)
                            }
                        },
                    ),
                    None => Ok(Interpret::Text(i.to_string())),
                },
            },
        }
    }
}

type BExpr = Box<Expr>;

#[derive(PartialEq, Debug)]
pub enum ParseError {
    ExpectedToken(String),
    UnexpectedEndOfInput,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ParseError::ExpectedToken(t) => format!("Expected {} token.", t),
            ParseError::UnexpectedEndOfInput => "Input ended before expected.".to_string(),
        };

        f.write_str(str.as_str())
    }
}

#[derive(PartialEq, Debug, Clone)]
struct Range {
    from: BExpr,
    to: BExpr,
}

#[derive(PartialEq, Debug, Clone)]
struct Call {
    callee: BExpr,
    args: Vec<BExpr>,
}

#[derive(PartialEq, Debug, Clone)]
struct BinOp {
    left: BExpr,
    operator: Token,
    right: BExpr,
}

#[derive(PartialEq, Debug, Clone)]
struct Unary {
    operator: Token,
    expr: BExpr,
}

#[derive(PartialEq, Debug, Clone)]
enum Literal {
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

        let interpreter = Interpreter::new();

        let inter = (*code).interpret(sheet, &interpreter);

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

        if !self.match_token(&Token::RightParen) {
            loop {
                args.push(self.parse_expression()?);
                if !self.match_token(&Token::Comma) {
                    break;
                }
                self.advance();
            }
        }

        self.consume(&Token::RightParen)?;
        Ok(Box::new(Expr::Call(Call { callee, args })))
    }

    fn parse_primary(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut expr = match self.tokens.peek() {
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
            _ => return Err(ParseError::ExpectedToken("Expression".to_string())),
        };

        while self.match_token(&Token::LeftParen) {
            self.advance();

            expr = self.parse_call(expr)?;
        }

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        calc::{
            parser::{Call, Interpret, Interpreter, Range},
            scanner::Scanner,
            Token,
        },
        Cell, Sheet,
    };

    use super::{BExpr, BinOp, Expr, Literal, ParseError, Parser};

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
    fn test_range_inter() {
        let mut sheet = Sheet::new();

        sheet.insert(
            (0, 0),
            Cell {
                val: crate::CellValue::Number(10.0),
                format: crate::CellFormat {},
            },
        );

        sheet.insert(
            (0, 1),
            Cell {
                val: crate::CellValue::Number(20.0),
                format: crate::CellFormat {},
            },
        );

        let inter = Interpreter::new();

        let expr = parse_string("(A1:A2)".to_string())
            .unwrap()
            .interpret(&sheet, &inter)
            .unwrap()
            .to_string();
        let expected = String::from("10,20");
        assert_eq!(expr, expected);
    }
}
