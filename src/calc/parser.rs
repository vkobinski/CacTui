use super::Token;

trait Expr {
    fn visit(&self) -> String;
}

type ExprRef = Box<dyn Expr>;

struct Range {
    from: ExprRef,
    to: ExprRef,
}

impl Expr for Range {
    fn visit(&self) -> String {
        todo!()
    }
}

struct Call {
    callee: ExprRef,
    args: Vec<ExprRef>,
}

impl Expr for Call {
    fn visit(&self) -> String {
        todo!()
    }
}

struct BinOp {
    left: ExprRef,
    operator: Token,
    right: ExprRef,
}

impl Expr for BinOp {
    fn visit(&self) -> String {
        todo!()
    }
}

struct Parser {
    tokens: Vec<Token>,
}

enum ParseError {
    ExpectedChar(char),
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    fn parse(&mut self) -> Result<ExprRef, ParseError> {
        todo!()
    }
}
