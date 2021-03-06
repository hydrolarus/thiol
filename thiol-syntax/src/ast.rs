// SPDX-FileCopyrightText: 2021 The thiol developers
//
// SPDX-License-Identifier: EUPL-1.2

use crate::Loc;

pub type Identifier = String;

#[derive(Debug, Clone)]
pub struct File {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Loc<Identifier>,
    pub args: Vec<(Loc<Identifier>, Loc<TypeReference>)>,
    pub ret_type: Loc<TypeReference>,

    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Consts {
    pub vars: Vec<Loc<VariableDef>>,
}

#[derive(Debug, Clone)]
pub struct Types {
    pub types: Vec<Loc<TypeDefinition>>,
}

#[derive(Debug, Clone)]
pub struct TypeDefinition {
    pub name: Loc<Identifier>,
    pub generics: Vec<Loc<Identifier>>,
    pub rhs: Loc<TypeDefinitionRhs>,
}

#[derive(Debug, Clone)]
pub enum TypeDefinitionRhs {
    Alias(TypeReference),
    Record { fields: Vec<Loc<VariableDef>> },
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(Loc<Function>),
    Consts(Loc<Consts>),
    Types(Loc<Types>),
    Program(Loc<Program>),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub name: Loc<Identifier>,
    pub inputs: Vec<Loc<VariableDef>>,
    pub outputs: Vec<Loc<VariableDef>>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct VariableDef {
    pub attributes: Vec<Loc<Attribute>>,
    pub name: Loc<Identifier>,
    pub type_: Loc<TypeReference>,
    pub rhs: Option<Loc<Expression>>,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Loc<Identifier>,
    pub args: Vec<(Option<Loc<Identifier>>, Loc<Expression>)>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Variable(Identifier),
    PrimitiveTypeConstructor(PrimitiveType),
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
    For {
        iter_name: Loc<Identifier>,
        loop_type: ForLoopType,
        from: Loc<Expression>,
        to: Loc<Expression>,

        body: Block,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum ForLoopType {
    Up,
    Down,
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
