use std::{collections::HashMap, fmt::Display};

use crate::{CellValue, Sheet};

use super::{
    parser::{Call, Expr, Literal},
    Token,
};

#[derive(PartialEq, Debug, Clone)]
pub enum Interpret {
    Number(f64),
    Text(String),
    Values(Vec<Interpret>),
    Bool(bool),
}

#[derive(PartialEq, Debug, Clone)]
pub enum InterpretError {
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

type CalcFunction = Box<dyn Fn(&[Interpret]) -> Result<Interpret, InterpretError>>;

struct FunctionRegistry {
    functions: HashMap<String, CalcFunction>,
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
    if let Some(Interpret::Values(val)) = args.first() {
        let sum = val.iter().try_fold(0.0, |acc, arg| {
            if let Interpret::Number(n) = arg {
                Ok(acc + n)
            } else {
                Err(InterpretError::InvalidArgument)
            }
        })?;
        return Ok(Interpret::Number(sum));
    }
    Err(InterpretError::InvalidArgument)
}
pub struct Interpreter<'a> {
    function_registry: FunctionRegistry,
    sheet: &'a Sheet,
}

impl<'a> Interpreter<'a> {
    pub fn new(sheet: &'a Sheet) -> Self {
        let mut registry = FunctionRegistry::new();
        registry.register("count", count);
        Self {
            function_registry: registry,
            sheet,
        }
    }

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

    pub fn interpret(&self, expr: &Expr) -> Result<Interpret, InterpretError> {
        match expr {
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
                                let cur = self.sheet.get(&(from.0, i)).unwrap();

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
            Expr::Call(call) => self.interpret_call(call),
            Expr::BinOp(bin_op) => {
                let (left, op, right) = (
                    bin_op.left.clone(),
                    bin_op.operator.clone(),
                    bin_op.right.clone(),
                );

                let left = self.interpret(&left);
                let right = self.interpret(&right);

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
                    Some((a, b)) => self.sheet.get(&(a, b)).map_or_else(
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

    fn interpret_call(&self, call: &Call) -> Result<Interpret, InterpretError> {
        let args = call
            .args
            .iter()
            .map(|arg| self.interpret(arg))
            .collect::<Result<Vec<_>, _>>()?;
        if let Expr::Literal(Literal::Identifier(id)) = &*call.callee {
            self.function_registry.call(id, &args)
        } else {
            Err(InterpretError::InvalidFunction)
        }
    }
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
