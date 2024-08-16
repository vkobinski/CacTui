use std::{collections::HashMap, fmt::Display};

use crate::{calc::parser::BinOp, CellValue, Sheet};

use super::{
    parser::{Call, Closure, Expr, Literal},
    Token,
};

#[derive(PartialEq, Debug, Clone)]
pub enum Interpret {
    Number(f64),
    Text(String),
    Values(Vec<Interpret>),
    Bool(bool),
    Closure(Closure),
}

impl Interpret {
    fn as_number(&self) -> f64 {
        if let Interpret::Number(num) = self {
            *num
        } else {
            panic!()
        }
    }

    fn as_text(&self) -> String {
        if let Interpret::Text(text) = self {
            text.to_string()
        } else {
            panic!()
        }
    }
    fn as_values(&self) -> Vec<Interpret> {
        if let Interpret::Values(values) = self {
            values.clone()
        } else {
            panic!()
        }
    }

    fn as_bool(&self) -> bool {
        if let Interpret::Bool(b) = self {
            *b
        } else {
            panic!()
        }
    }

    fn as_closure(&self) -> &Closure {
        if let Interpret::Closure(closure) = self {
            closure
        } else {
            panic!()
        }
    }

    fn get_type(&self) -> &'static str {
        match self {
            Interpret::Number(_) => "Number",
            Interpret::Text(_) => "Text",
            Interpret::Values(_) => "Range",
            Interpret::Bool(_) => "Bool",
            Interpret::Closure(_) => "Closure",
        }
    }
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
    ClosureBinOp,
    ClosureInvalidArg,
    ExpectedArg(String),
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
            InterpretError::ClosureBinOp => todo!(),
            InterpretError::ClosureInvalidArg => todo!(),
            InterpretError::ExpectedArg(_) => todo!(),
        })
    }
}

type CalcFunction = Box<dyn Fn(&[Interpret], &Interpreter) -> Result<Interpret, InterpretError>>;
struct CalcFunctionImplement {
    fun: CalcFunction,
    args_type: Vec<&'static str>,
}

struct FunctionRegistry {
    functions: HashMap<String, CalcFunctionImplement>,
}

impl FunctionRegistry {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    fn register<F>(&mut self, name: &str, func: F, args_types: Vec<&'static str>)
    where
        F: Fn(&[Interpret], &Interpreter) -> Result<Interpret, InterpretError> + 'static,
    {
        let fun_insert = CalcFunctionImplement {
            fun: Box::new(func),
            args_type: args_types,
        };
        self.functions.insert(name.to_string(), fun_insert);
    }

    fn call(
        &self,
        name: &str,
        args: &[Interpret],
        interpreter: &Interpreter,
    ) -> Result<Interpret, InterpretError> {
        let function = self
            .functions
            .get(name)
            .ok_or(InterpretError::FunctionNotExist)?;

        if function.args_type.len() != args.len() {
            return Err(InterpretError::InvalidArgument);
        }

        for (expected_type, arg) in function.args_type.iter().zip(args) {
            if *expected_type != arg.get_type() {
                return Err(InterpretError::ExpectedArg(expected_type.to_string()));
            }
        }

        (function.fun)(args, interpreter)
    }
}

fn count(args: &[Interpret], _interpreter: &Interpreter) -> Result<Interpret, InterpretError> {
    let sum = args
        .first()
        .unwrap()
        .as_values()
        .iter()
        .try_fold(0.0, |acc, arg| {
            if let Interpret::Number(n) = arg {
                Ok(acc + n)
            } else {
                Err(InterpretError::InvalidArgument)
            }
        })?;

    Ok(Interpret::Number(sum))
}

fn count_if(args: &[Interpret], interpreter: &Interpreter) -> Result<Interpret, InterpretError> {
    let sum = args
        .first()
        .unwrap()
        .as_values()
        .iter()
        .try_fold(0.0, |acc, arg| {
            let closure = args.get(1).unwrap();

            let val = interpreter.run_closure(closure.clone(), "".to_string(), arg.clone());

            match val {
                Ok(val) => {
                    if val.as_bool() {
                        return Ok(acc + arg.as_number());
                    }
                    Ok(acc)
                }
                Err(_) => Err(InterpretError::ClosureBinOp),
            }
        })?;

    Ok(Interpret::Number(sum))
}
pub struct Interpreter<'a> {
    function_registry: FunctionRegistry,
    sheet: &'a Sheet,
}

impl<'a> Interpreter<'a> {
    pub fn new(sheet: &'a Sheet) -> Self {
        let mut registry = FunctionRegistry::new();
        registry.register("count", count, vec!["Range"]);
        registry.register("count_if", count_if, vec!["Range", "Closure"]);
        Self {
            function_registry: registry,
            sheet,
        }
    }

    fn run_closure(
        &self,
        closure: Interpret,
        var_name: String,
        var_value: Interpret,
    ) -> Result<Interpret, InterpretError> {
        let closure = closure.as_closure();

        if let Expr::BinOp(body) = *closure.body.clone() {
            let new_bin_op = Expr::BinOp(BinOp {
                left: Box::new(Expr::Literal(Literal::Number(var_value.as_number()))),
                operator: body.operator,
                right: body.right,
            });

            return self.interpret(&new_bin_op);
        }

        Err(InterpretError::ClosureBinOp)
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
            Expr::Closure(closure) => Ok(Interpret::Closure(closure.clone())),
        }
    }

    fn interpret_call(&self, call: &Call) -> Result<Interpret, InterpretError> {
        let args = call
            .args
            .iter()
            .map(|arg| self.interpret(arg))
            .collect::<Result<Vec<_>, _>>()?;
        if let Expr::Literal(Literal::Identifier(id)) = &*call.callee {
            self.function_registry.call(id, &args, &self)
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
            Interpret::Closure(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::calc::interpreter::Interpreter;
    use crate::calc::parser::Parser;
    use crate::{Cell, Sheet};

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

        let inter = Interpreter::new(&sheet);

        let expr = Parser::parse_string("A1:A2".to_string()).unwrap();
        let expr = inter.interpret(&expr).unwrap().to_string();

        let expected = String::from("10,20");
        assert_eq!(expr, expected);
    }

    #[test]
    fn test_count() {
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

        let inter = Interpreter::new(&sheet);

        let expr = Parser::parse_string("count(A1:A2)".to_string()).unwrap();
        let expr = inter.interpret(&expr).unwrap().to_string();

        let expected = String::from("30");
        assert_eq!(expr, expected);
    }

    #[test]
    fn test_count_if() {
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

        let inter = Interpreter::new(&sheet);

        let expr = Parser::parse_string("count_if(A1:A2, |v| v > 10)".to_string()).unwrap();
        let expr = inter.interpret(&expr).unwrap().to_string();

        let expected = String::from("20");
        assert_eq!(expr, expected);
    }
}
