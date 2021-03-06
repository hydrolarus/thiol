// SPDX-FileCopyrightText: 2021 The thiol developers
//
// SPDX-License-Identifier: EUPL-1.2

use std::collections::HashMap;

use id_arena::{Arena, Id};

use thiol_syntax::FileLocation;

pub type Identifier = String;

#[derive(Default)]
pub struct Context {
    pub identifiers: Arena<Identifier>,
    pub type_defs: Arena<TypeDefinition>,
    pub type_refs: Arena<TypeReference>,
    pub functions: Arena<Function>,
    pub programs: Arena<Program>,
    pub attributes: Arena<Attribute>,
    pub variable_defs: Arena<VariableDef>,
    pub statements: Arena<Statement>,
    pub expressions: Arena<Expression>,
    pub prim_ops: Arena<PrimitiveOp>,

    pub identifier_fcs: HashMap<Id<Identifier>, FileLocation>,
    pub type_def_fcs: HashMap<Id<TypeDefinition>, FileLocation>,
    pub type_ref_fcs: HashMap<Id<TypeReference>, FileLocation>,
    pub function_fcs: HashMap<Id<Function>, FileLocation>,
    pub program_fcs: HashMap<Id<Program>, FileLocation>,
    pub attribute_fcs: HashMap<Id<Attribute>, FileLocation>,
    pub variable_def_fcs: HashMap<Id<VariableDef>, FileLocation>,
    pub statement_fcs: HashMap<Id<Statement>, FileLocation>,
    pub expression_fcs: HashMap<Id<Expression>, FileLocation>,
    pub prim_op_fcs: HashMap<Id<PrimitiveOp>, FileLocation>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub types: Vec<Id<TypeDefinition>>,
    pub consts: Vec<Id<VariableDef>>,
    pub functions: Vec<Id<Function>>,
    pub programs: Vec<Id<Program>>,
}

#[derive(Debug, Clone)]
pub struct TypeDefinition {
    pub name: Id<Identifier>,
    pub generics: Vec<Id<Identifier>>,
    pub rhs: Id<TypeDefinitionRhs>,
}

#[derive(Debug, Clone)]
pub enum TypeDefinitionRhs {
    Alias(Id<TypeReference>),
    Record { fields: Vec<Id<VariableDef>> },
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Id<Identifier>,
    pub args: Vec<(Id<Identifier>, Id<TypeReference>)>,
    pub ret_type: Id<TypeReference>,

    pub body: Vec<Id<Statement>>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub name: Id<Identifier>,
    pub inputs: Vec<Id<VariableDef>>,
    pub outputs: Vec<Id<VariableDef>>,
    pub body: Vec<Id<Statement>>,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Id<Identifier>,
    pub pos_args: Vec<Id<Expression>>,
    pub nam_args: Vec<(Id<Identifier>, Id<Expression>)>,
}

#[derive(Debug, Clone)]
pub struct VariableDef {
    pub attrs: Vec<Id<Attribute>>,
    pub name: Id<Identifier>,
    pub type_: Id<TypeReference>,
    pub rhs: Option<Id<Expression>>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Var(Id<VariableDef>),
    Becomes {
        lhs: Id<Expression>,
        rhs: Id<Expression>,
    },
    Return(Option<Id<Expression>>),
    Break,
    Continue,
    If {
        cond: Id<Expression>,
        then_body: Vec<Id<Statement>>,
        else_body: Vec<Id<Statement>>,
    },
    For {
        iter_name: Id<Identifier>,
        loop_type: ForLoopType,
        from: Id<Expression>,
        to: Id<Expression>,

        body: Vec<Id<Statement>>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum ForLoopType {
    Up,
    Down,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Id<Literal>),
    Variable(Id<Identifier>),
    PrimitiveOp(Id<PrimitiveOp>),
    Call {
        name: Id<Identifier>,
        pos_args: Vec<Id<Expression>>,
        nam_args: Vec<(Id<Identifier>, Id<Expression>)>,
    },
    Field {
        base: Id<Expression>,
        name: Id<Identifier>,
    },
    Index {
        base: Id<Expression>,
        index: Id<Expression>,
    },
    As {
        base: Id<Expression>,
        ty: Id<TypeReference>,
    },
}

#[derive(Debug, Clone)]
pub enum PrimitiveOp {
    Neg(Id<Expression>),
    Pos(Id<Expression>),

    Add(Id<Expression>, Id<Expression>),
    Sub(Id<Expression>, Id<Expression>),
    Mul(Id<Expression>, Id<Expression>),
    Div(Id<Expression>, Id<Expression>),
    Mod(Id<Expression>, Id<Expression>),

    Gt(Id<Expression>, Id<Expression>),
    Gte(Id<Expression>, Id<Expression>),
    Lt(Id<Expression>, Id<Expression>),
    Lte(Id<Expression>, Id<Expression>),
    Eq(Id<Expression>, Id<Expression>),
    Neq(Id<Expression>, Id<Expression>),

    Constructor {
        ty: PrimitiveType,
        pos_args: Vec<Id<Expression>>,
        nam_args: Vec<(Id<Identifier>, Id<Expression>)>,
    },
}

pub enum TypeReference {
    Primitive(PrimitiveType),
    OpenArray(Id<TypeReference>),
    Array {
        base: Id<TypeReference>,
        size: usize,
    },
    Named {
        name: Id<Identifier>,
        generics: Vec<Id<TypeReference>>,
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
        vtype: Option<Id<VecType>>,
        space: Option<Id<Identifier>>,
    },
    UIntVec {
        components: VecSize,
        vtype: Option<Id<VecType>>,
        space: Option<Id<Identifier>>,
    },

    FloatVec {
        components: VecSize,
        vtype: Option<Id<VecType>>,
        space: Option<Id<Identifier>>,
    },
    DoubleVec {
        components: VecSize,
        vtype: Option<Id<VecType>>,
        space: Option<Id<Identifier>>,
    },

    FloatMat {
        cols: VecSize,
        rows: VecSize,
        transform: Option<(Id<Identifier>, Id<Identifier>)>,
    },
    DoubleMat {
        cols: VecSize,
        rows: VecSize,
        transform: Option<(Id<Identifier>, Id<Identifier>)>,
    },
}

#[derive(Debug, Clone)]
pub enum VecType {
    Point,
    Vector,
    Colour,
}

#[derive(Debug, Clone)]
pub enum VecSize {
    VS2,
    VS3,
    VS4,
}

#[derive(Debug, Copy, Clone)]
pub enum Literal {
    Integer(i128),
    Float(f64),
}
