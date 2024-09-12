use crate::scanner::Token;

pub enum Declaration<'a> {
    Class(ClassDeclaration<'a>),
    Function(Function<'a>),
    Variable(VariableDeclaration<'a>),
    Statement(Statement<'a>),
}

pub enum Expression<'a> {
    Assign(AssignExpr<'a>),
    Binary(BinaryExpr<'a>),
    Unary(UnaryExpr<'a>),
    Call(CallExpr<'a>),
    Primary(Literal<'a>),
}

pub enum Statement<'a> {
    Expr(ExprStatement<'a>),
    For(ForStatement<'a>),
    If(IfStatement<'a>),
    Print(PrintStatement<'a>),
    Return(ReturnStatement<'a>),
    While(WhileStatement<'a>),
}

pub enum Literal<'a> {
    Number(Token<'a>),
    String(Token<'a>),
    Bool(Token<'a>),
    Nil(Token<'a>),
    This(Token<'a>),
    Identifier(Token<'a>),
    Group(Box<Expression<'a>>),
    Super(SuperExpr<'a>),
}

pub enum Operator {
    Add,
    Sub,
    Div,
    Mul,
    LT,
    GT,
    LE,
    GE,
    EQ,
    NE,
    And,
    Or,
}

pub struct Program<'a> {
    pub declarations: Vec<Declaration<'a>>,
}

pub struct Function<'a> {
    name: Token<'a>,
    parameters: Parameters<'a>,
    block: BlockStatement<'a>,
}

pub struct Parameters<'a> {
    values: Vec<Token<'a>>,
}

pub struct Arguments<'a> {
    values: Vec<Box<Expression<'a>>>,
}

pub struct VariableDeclaration<'a> {
    name: Token<'a>,
    assignment: Option<Box<Expression<'a>>>,
}

pub struct ClassDeclaration<'a> {
    name: Token<'a>,
    parent: Option<Token<'a>>,
    methods: Vec<Function<'a>>,
}

pub struct BlockStatement<'a> {
    declarations: Vec<Box<Declaration<'a>>>,
}

pub struct WhileStatement<'a> {
    condition: Box<Expression<'a>>,
    block: Box<Statement<'a>>,
}

pub struct PrintStatement<'a> {
    expr: Box<Expression<'a>>,
}

pub struct ReturnStatement<'a> {
    expr: Option<Box<Expression<'a>>>,
}

pub struct IfStatement<'a> {
    condition: Box<Expression<'a>>,
    true_block: Box<Statement<'a>>,
    false_block: Option<Box<Statement<'a>>>,
}

pub enum ForInit<'a> {
    VarDeclaration(VariableDeclaration<'a>),
    ExprStatement(ExprStatement<'a>),
}

pub struct ForStatement<'a> {
    init: ForInit<'a>,
    condition: Box<Expression<'a>>,
    change: Box<Expression<'a>>,
    block: Box<Statement<'a>>,
}

pub struct ExprStatement<'a> {
    expr: Box<Expression<'a>>,
}

pub struct SuperExpr<'a> {
    pub token: Token<'a>,
    pub ident: Token<'a>,
}

pub struct AssignExpr<'a> {
    call: Option<CallExpr<'a>>,
    token: Token<'a>,
    expr: Box<Expression<'a>>,
}

pub struct BinaryExpr<'a> {
    op: Operator,
    left: Box<Expression<'a>>,
    right: Box<Expression<'a>>,
}

pub struct UnaryExpr<'a> {
    op: Operator,
    expr: Box<Expression<'a>>,
}

pub struct CallExpr<'a> {
    callee: Box<Expression<'a>>,
    paren: Token<'a>,
    args: Arguments<'a>,
}
