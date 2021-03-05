use crate::Loc;

pub type Identifier = String;

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Variable(Identifier),
    Call {
        base: Box<Loc<Expression>>,
        args: Vec<(Option<Loc<Identifier>>, Loc<Expression>)>,
    },
    DotCall {
        base: Box<Loc<Expression>>,
        name: Loc<Identifier>,
        args: Vec<(Option<Loc<Identifier>>, Loc<Expression>)>,
    },
    InfixOp {
        op: InfixOp,
        args: Box<[Loc<Expression>; 2]>,
    },
    PrefixOp {
        op: PrefixOp,
        expr: Box<Loc<Expression>>,
    },
    Field {
        base: Box<Loc<Expression>>,
        name: Loc<Identifier>,
    },
    Index {
        base: Box<Loc<Expression>>,
        index: Box<Loc<Expression>>,
    },
    As {
        base: Box<Loc<Expression>>,
        ty: Loc<TypeReference>,
    },
}

pub type Block = Vec<Loc<Statement>>;

#[derive(Debug, Clone)]
pub enum Statement {
    Var {
        name: Loc<Identifier>,
        type_ref: Loc<TypeReference>,
        rhs: Option<Loc<Expression>>,
    },
    Becomes {
        // this needs to be checked to be a valid l-value
        lhs: Loc<Expression>,
        rhs: Loc<Expression>,
    },
    Return(Option<Loc<Expression>>),
    Break,
    Continue,
    Branch {
        // guaranteed to have length of at least 1
        branches: Vec<(Loc<Expression>, Block)>,
        else_: Option<Block>,
    },
}

#[derive(Debug, Clone)]
pub enum TypeReference {
    Primitive(Loc<PrimitiveType>),
    Named {
        name: Loc<Identifier>,
        generics: Vec<Loc<TypeReference>>,
    },
    Array {
        base: Box<Loc<TypeReference>>,
        size: Loc<usize>,
    },
    OpenArray {
        base: Box<Loc<TypeReference>>,
    },
}

#[derive(Debug, Clone)]
pub enum PrimitiveType {
    Bool,
    Int,
    UInt,
    Float,
    Double,

    BoolVec {
        components: VecSize,
    },
    IntVec {
        components: VecSize,
        vtype: Option<Loc<VecType>>,
        space: Option<Loc<Identifier>>,
    },
    UIntVec {
        components: VecSize,
        vtype: Option<Loc<VecType>>,
        space: Option<Loc<Identifier>>,
    },

    FloatVec {
        components: VecSize,
        vtype: Option<Loc<VecType>>,
        space: Option<Loc<Identifier>>,
    },
    DoubleVec {
        components: VecSize,
        vtype: Option<Loc<VecType>>,
        space: Option<Loc<Identifier>>,
    },

    FloatMat {
        cols: VecSize,
        rows: VecSize,
        transform: Option<(Loc<Identifier>, Loc<Identifier>)>,
    },
    DoubleMat {
        cols: VecSize,
        rows: VecSize,
        transform: Option<(Loc<Identifier>, Loc<Identifier>)>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VecSize {
    VS2,
    VS3,
    VS4,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VecType {
    Point,
    Vector,
    Colour,
}

#[derive(Debug, Copy, Clone)]
pub enum Literal {
    Integer(i128),
    Float(f64),
}

#[derive(Debug, Copy, Clone)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Neq,
}

#[derive(Debug, Copy, Clone)]
pub enum PrefixOp {
    Plus,
    Minus,
}
