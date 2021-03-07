// SPDX-FileCopyrightText: 2021 The thiol developers
//
// SPDX-License-Identifier: EUPL-1.2

#![allow(clippy::redundant_closure_call)]

use crate::{
    lexer::{Token, TokenKind as TK},
    FileId, Loc,
};

use crate::ast;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub location: crate::FileLocation,
    pub expected_tokens: Vec<String>,
}

pub fn parse_file(file_id: FileId, input: &str) -> Result<ast::File, ParseError> {
    let toks = crate::lexer::tokenise(file_id, input).collect::<Vec<_>>();

    fn extract_expected_token(s: &str) -> String {
        s.trim_start_matches("[tok ! (TK :: ")
            .split(|c: char| !c.is_alphanumeric())
            .next()
            .unwrap()
            .to_string()
    }

    match parser::file(&toks) {
        Ok(val) => Ok(val),
        Err(err) => {
            let location = toks
                .get(err.location)
                .map(|t| t.loc)
                .unwrap_or(crate::FileLocation {
                    file: file_id,
                    start: input.len(),
                    end: input.len(),
                });
            let expected = err.expected.tokens().map(extract_expected_token).collect();
            Err(ParseError {
                location,
                expected_tokens: expected,
            })
        }
    }
}

macro_rules! tok {
    ($p:pat, $loc:ident) => {
        Token {
            value: $p,
            loc: $loc,
        }
    };
    ($p:pat) => {
        Token { value: $p, loc: _ }
    };
}

peg::parser! {
    grammar parser() for [Token] {

        //
        // file and item
        //

        pub rule file() -> ast::File
        = items:item()* ![_] {
            ast::File {
                items,
            }
        }

        rule item() -> ast::Item
        =
            func:function() {
                ast::Item::Function(func)
            }
        /   consts:consts() {
                ast::Item::Consts(consts)
            }
        /   types:types() {
                ast::Item::Types(types)
            }
        /   prog:program() {
                ast::Item::Program(prog)
            }

        //
        // Program
        //
        rule program() -> Loc<ast::Program>
        =
            [tok!(TK::Program, start)] name:identifier()
                inputs:program_inputs()?
                outputs:program_outputs()?
            [tok!(TK::Begin)]
                body:block()
            [tok!(TK::End, end)] {
                Loc::new(
                    start.merge(end),
                    ast::Program {
                        name,
                        inputs: inputs.unwrap_or_default(),
                        outputs: outputs.unwrap_or_default(),
                        body,
                    }
                )
            }

        rule program_inputs() -> Vec<Loc<ast::VariableDef>>
        = [tok!(TK::Input)] vars:variable_def()* { vars }

        rule program_outputs() -> Vec<Loc<ast::VariableDef>>
        = [tok!(TK::Output)] vars:variable_def()* { vars }

        //
        // function
        //

        pub rule function() -> Loc<ast::Function>
        =
            [tok!(TK::Function, start)] name:identifier() [tok!(TK::ParenOpen)]
                args:sep_trailing(<function_arg()>, <[tok!(TK::Comma)]>)
            [tok!(TK::ParenClose)] [tok!(TK::Returns)] ret_ty:type_reference()
            [tok!(TK::Begin)]
                body:block()
            [tok!(TK::End, end)] {
                Loc::new(
                    start.merge(end),
                    ast::Function {
                        name,
                        args,
                        ret_type: ret_ty,
                        body,
                    }
                )
            }

        rule function_arg() -> (Loc<ast::Identifier>, Loc<ast::TypeReference>)
        = name:identifier() [tok!(TK::Colon)] ty:type_reference() {
            (name, ty)
        }

        //
        // Consts
        //
        rule consts() -> Loc<ast::Consts>
        = [tok!(TK::Const, start)] vars:variable_def()+ {
            let end = vars.last().map(|l| l.loc).unwrap();
            Loc::new(start.merge(end), ast::Consts { vars })
        }

        //
        // Types
        //
        rule types() -> Loc<ast::Types>
        = [tok!(TK::Type, start)] tys:type_definition()+ {
            Loc::new(
                start.merge(tys.last().unwrap().loc),
                ast::Types {
                    types: tys,
                }
            )
        }

        rule type_definition() -> Loc<ast::TypeDefinition>
        =
            name:identifier() [tok!(TK::LessThan)]
                generics:sep_trailing(<identifier()>, <[tok!(TK::Comma)]>)
            [tok!(TK::GreaterThan)] [tok!(TK::Equals)] rhs:type_def_rhs() {
                Loc::new(
                    name.loc.merge(rhs.loc),
                    ast::TypeDefinition {
                        name,
                        generics,
                        rhs,
                    }
                )
            }
        /   name:identifier() [tok!(TK::Equals)] rhs:type_def_rhs() {
                Loc::new(
                    name.loc.merge(rhs.loc),
                    ast::TypeDefinition {
                        name,
                        generics: vec![],
                        rhs,
                    }
                )
            }

        rule type_def_rhs() -> Loc<ast::TypeDefinitionRhs>
        =
            [tok!(TK::Record, start)]
                fields:variable_def()*
            [tok!(TK::End, end)] {
                Loc::new(start.merge(end), ast::TypeDefinitionRhs::Record { fields })
            }
        /   ty_ref:type_reference() [tok!(TK::SemiColon, end)] {
                Loc::new(ty_ref.loc.merge(end), ast::TypeDefinitionRhs::Alias(ty_ref))
            }


        rule variable_def() -> Loc<ast::VariableDef>
        =
            attrs:attribute()* name:identifier() [tok!(TK::Colon)]
            type_:type_reference() [tok!(TK::SemiColon, end)] {
                let start = attrs.first().map(|l| l.loc).unwrap_or(name.loc);
                Loc::new(start.merge(end), ast::VariableDef {
                    attributes: attrs,
                    name,
                    type_,
                    rhs: None,
                })
            }
        /   attrs:attribute()* name:identifier() [tok!(TK::Colon)]
            type_:type_reference()
            [tok!(TK::Becomes)] rhs:expression() [tok!(TK::SemiColon, end)] {
                let start = attrs.first().map(|l| l.loc).unwrap_or(name.loc);
                Loc::new(start.merge(end), ast::VariableDef {
                    attributes: attrs,
                    name,
                    type_,
                    rhs: Some(rhs),
                })
            }

        rule attribute() -> Loc<ast::Attribute>
        =
            [tok!(TK::BracketOpen, start)] name:identifier() [tok!(TK::BracketClose, end)] {
                Loc::new(start.merge(end), ast::Attribute { name, args: vec![] })
            }
        /   [tok!(TK::BracketOpen, start)] name:identifier() [tok!(TK::ParenOpen)]
                args:arglist()
            [tok!(TK::ParenClose)] [tok!(TK::BracketClose, end)] {
                Loc::new(
                    start.merge(end),
                    ast::Attribute {
                        name,
                        args,
                    }
                )
            }

        //
        // Statement
        //

        pub rule statement() -> Loc<ast::Statement>
        =
            [tok!(TK::Var, start)] var_def:variable_def() {
                Loc::new(
                    start.merge(var_def.loc),
                    ast::Statement::Var(var_def.value),
                )
            }
        /   [tok!(TK::Var, start)] name:identifier() var_def:variable_def() {
                Loc::new(
                    start.merge(var_def.loc),
                    ast::Statement::Var(var_def.value)
                )
            }
            // expr-lhs := expr;
        /   lhs:expression_atom() [tok!(TK::Becomes)] rhs:expression() [tok!(TK::SemiColon, end)] {
                Loc::new(
                    lhs.loc.merge(end),
                    ast::Statement::Becomes {
                        lhs,
                        rhs,
                    },
                )
            }
        /   [tok!(TK::Return, start)] expr:expression() [tok!(TK::SemiColon, end)] {
                Loc::new(start.merge(end), ast::Statement::Return(Some(expr)))
            }
        /   [tok!(TK::Return, start)] [tok!(TK::SemiColon, end)] {
                Loc::new(start.merge(end), ast::Statement::Return(None))
            }
        /   [tok!(TK::Break, start)] [tok!(TK::SemiColon, end)] {
                Loc::new(start.merge(end), ast::Statement::Break)
            }
        /   [tok!(TK::Continue, start)] [tok!(TK::SemiColon, end)] {
                Loc::new(start.merge(end), ast::Statement::Continue)
            }
        /   [tok!(TK::If, start)] cond:expression() [tok!(TK::Then)]
                tb:block()
            eis:elseif_branch()*
            [tok!(TK::End, end)] {
                Loc::new(
                    start.merge(end),
                    ast::Statement::Branch {
                        branches: {
                            let mut v = Vec::with_capacity(1 + eis.len());
                            v.push((cond, tb));
                            v.extend(eis);
                            v
                        },
                        else_: None,
                    }
                )
            }
        /   [tok!(TK::If, start)] cond:expression() [tok!(TK::Then)]
                tb:block()
            eis:elseif_branch()*
            [tok!(TK::Else)]
                eb:block()
            [tok!(TK::End, end)] {
                Loc::new(
                    start.merge(end),
                    ast::Statement::Branch {
                        branches: {
                            let mut v = Vec::with_capacity(1 + eis.len());
                            v.push((cond, tb));
                            v.extend(eis);
                            v
                        },
                        else_: Some(eb),
                    }
                )
            }
        /   [tok!(TK::For, start)] iter:identifier() [tok!(TK::In)]
                from:expression_atom() [tok!(TK::To)] to:expression_atom()
            [tok!(TK::Do)] body:block() [tok!(TK::End, end)] {
                Loc::new(
                    start.merge(end),
                    ast::Statement::For {
                        iter_name: iter,
                        loop_type: ast::ForLoopType::Up,
                        from,
                        to,
                        body,
                    }
                )
            }
        /   [tok!(TK::For, start)] iter:identifier() [tok!(TK::In)]
                from:expression_atom() [tok!(TK::DownTo)] to:expression_atom()
            [tok!(TK::Do)] body:block() [tok!(TK::End, end)] {
                Loc::new(
                    start.merge(end),
                    ast::Statement::For {
                        iter_name: iter,
                        loop_type: ast::ForLoopType::Down,
                        from,
                        to,
                        body,
                    }
                )
            }

        rule elseif_branch() -> (Loc<ast::Expression>, ast::Block)
        =
            [tok!(TK::ElseIf)] cond:expression() [tok!(TK::Then, start)]
                b:block()
            &([tok!(TK::End)] / [tok!(TK::ElseIf)]) {
                (cond, b)
            }

        rule block() -> ast::Block
        = statement()*

        //
        // Expression
        //

        pub rule expression() -> Loc<ast::Expression> = precedence!{

            x:(@) [tok!(TK::Equals)] y:@ {
                Loc::new(
                    x.loc.merge(y.loc),
                    ast::Expression::InfixOp { op: ast::InfixOp::Eq, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::NotEquals)] y:@ {
                Loc::new(
                    x.loc.merge(y.loc),
                    ast::Expression::InfixOp { op: ast::InfixOp::Neq, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::LessThan)] y:@ {
                Loc::new(
                    x.loc.merge(y.loc),
                    ast::Expression::InfixOp { op: ast::InfixOp::Lt, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::LessThanEqual)] y:@ {
                Loc::new(
                    x.loc.merge(y.loc),
                    ast::Expression::InfixOp { op: ast::InfixOp::Lte, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::GreaterThan)] y:@ {
                Loc::new(
                    x.loc.merge(y.loc),
                    ast::Expression::InfixOp { op: ast::InfixOp::Gt, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::GreaterThanEqual)] y:@ {
                Loc::new(
                    x.loc.merge(y.loc),
                    ast::Expression::InfixOp { op: ast::InfixOp::Gte, args: Box::new([x, y]), },
                )
            }
            --
            x:(@) [tok!(TK::Plus, opl)] y:@ {
                Loc::new(
                    x.loc.merge(y.loc),
                    ast::Expression::InfixOp { op: ast::InfixOp::Add, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::Minus, opl)] y:@ {
                Loc::new(
                    x.loc.merge(y.loc),
                    ast::Expression::InfixOp { op: ast::InfixOp::Sub, args: Box::new([x, y]), },
                )
            }
            --
            x:(@) [tok!(TK::Star, opl)] y:@ {
                Loc::new(
                    x.loc.merge(y.loc),
                    ast::Expression::InfixOp { op: ast::InfixOp::Mul, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::Slash, opl)] y:@ {
                Loc::new(
                    x.loc.merge(y.loc),
                    ast::Expression::InfixOp { op: ast::InfixOp::Div, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::Mod, opl)] y:@ {
                Loc::new(
                    x.loc.merge(y.loc),
                    ast::Expression::InfixOp { op: ast::InfixOp::Mod, args: Box::new([x, y]), },
                )
            }
            --
            expr:@ [tok!(TK::As)] ty:type_reference() {
                Loc::new(expr.loc.merge(ty.loc), ast::Expression::As {
                    base: Box::new(expr),
                    ty,
                })
            }
            --
            [tok!(TK::Plus, opl)] arg:@ {
                Loc::new(opl.merge(arg.loc), ast::Expression::PrefixOp { op: ast::PrefixOp::Plus, expr: Box::new(arg) })
            }
            [tok!(TK::Minus, opl)] arg:@ {
                Loc::new(opl.merge(arg.loc), ast::Expression::PrefixOp { op: ast::PrefixOp::Minus, expr: Box::new(arg) })
            }
            --
            atom:expression_atom() { atom }
        }

        rule expression_atom() -> Loc<ast::Expression> = precedence!{
            base:@ [tok!(TK::BracketOpen)] idx:expression() [tok!(TK::BracketClose, loc)]
            {
                Loc::new(base.loc.merge(loc), ast::Expression::Index {
                    base: Box::new(base),
                    index: Box::new(idx),
                })
            }
            --
            base:@ [tok!(TK::Dot)] name:identifier()
            [tok!(TK::ParenOpen)] args:arglist() [tok!(TK::ParenClose, loc)]
            {
                Loc::new(base.loc.merge(loc), ast::Expression::DotCall {
                    base: Box::new(base),
                    name,
                    args,
                })
            }
            --
            base:@ [tok!(TK::ParenOpen)] args:arglist() [tok!(TK::ParenClose, loc)] {
                Loc::new(base.loc.merge(loc), ast::Expression::Call {
                    base: Box::new(base),
                    args,
                })
            }
            --
            base:@ [tok!(TK::Dot)] name:identifier() {
                Loc::new(
                    base.loc.merge(name.loc),
                    ast::Expression::Field { base: Box::new(base), name },
                )
            }
            --
            ident:identifier() {
                Loc::new(ident.loc, ast::Expression::Variable(ident.value))
            }
            l:literal() {
                Loc::new(l.loc, ast::Expression::Literal(l.value))
            }
            [tok!(TK::ParenOpen)] inner:expression() [tok!(TK::ParenClose)] {
                inner
            }
            prim:type_primitive() {
                Loc::new(
                    prim.loc,
                    ast::Expression::PrimitiveTypeConstructor(prim.value),
                )
            }
        }

        rule call_arg() -> (Option<Loc<ast::Identifier>>, Loc<ast::Expression>)
        =
            ident:identifier()
            [tok!(TK::Colon)]
            rhs:expression()
            {
                (Some(ident), rhs)
            }
        / e:expression() { (None, e) }

        rule arglist() -> Vec<(Option<Loc<ast::Identifier>>, Loc<ast::Expression>)>
        = args:sep_trailing(<call_arg()>, <[tok!(TK::Comma)]>)


        //
        // Type references
        //

        pub rule type_reference() -> Loc<ast::TypeReference>
        =
            [tok!(TK::ParenOpen)] inner:type_reference() [tok!(TK::ParenClose)] {
                inner
            }
        /   prim:type_primitive() {
                Loc::new(prim.loc, ast::TypeReference::Primitive(prim))
            }
        /   [tok!(TK::Array, al)] [tok!(TK::BracketOpen)]
                [tok!(TK::Integer(size), sl)]
            [tok!(TK::BracketOpen)] [tok!(TK::Of)] ty:type_reference() {
                Loc::new(
                    al.merge(ty.loc),
                    ast::TypeReference::Array {
                        base: Box::new(ty),
                        size: Loc::new(sl, size as usize),
                    },
                )
            }
        /   [tok!(TK::Array, al)] [tok!(TK::Of)] ty:type_reference() {
                Loc::new(
                    al.merge(ty.loc),
                    ast::TypeReference::OpenArray {
                        base: Box::new(ty),
                    },
                )
            }
        /   name:identifier() [tok!(TK::LessThan)]
                gens:sep_trailing(<type_reference()>, <[tok!(TK::Comma)]>)
            [tok!(TK::GreaterThan, end)] {
                Loc::new(
                    name.loc.merge(end),
                    ast::TypeReference::Named {
                        name,
                        generics: gens,
                    }
                )
            }
        /   name:identifier() {
                Loc::new(
                    name.loc,
                    ast::TypeReference::Named {
                        name,
                        generics: vec![],
                    }
                )
            }

        rule type_primitive() -> Loc<ast::PrimitiveType>
        =
            [tok!(TK::TyBool, loc)] { Loc::new(loc, ast::PrimitiveType::Bool) }
        /   [tok!(TK::TyInt, loc)] { Loc::new(loc, ast::PrimitiveType::Int) }
        /   [tok!(TK::TyUInt, loc)] { Loc::new(loc, ast::PrimitiveType::UInt) }
        /   [tok!(TK::TyFloat, loc)] { Loc::new(loc, ast::PrimitiveType::Float) }
        /   [tok!(TK::TyDouble, loc)] { Loc::new(loc, ast::PrimitiveType::Double) }

        /   [tok!(TK::TyBoolVec(n), loc)] { Loc::new(loc, ast::PrimitiveType::BoolVec { components: n }) }

        /   [tok!(TK::TyIntVec(n), loc)] annot:type_prim_vec_annot()? {
                let annot = annot.unwrap_or((None, None));
                Loc::new(loc, ast::PrimitiveType::IntVec {
                    components: n,
                    vtype: annot.0,
                    space: annot.1,
                })
            }
        /   [tok!(TK::TyUIntVec(n), loc)] annot:type_prim_vec_annot()? {
                let annot = annot.unwrap_or((None, None));
                Loc::new(loc, ast::PrimitiveType::UIntVec {
                    components: n,
                    vtype: annot.0,
                    space: annot.1,
                })
            }
        /   [tok!(TK::TyFloatVec(n), loc)] annot:type_prim_vec_annot()? {
                let annot = annot.unwrap_or((None, None));
                Loc::new(loc, ast::PrimitiveType::FloatVec {
                    components: n,
                    vtype: annot.0,
                    space: annot.1,
                })
            }
        /   [tok!(TK::TyDoubleVec(n), loc)] annot:type_prim_vec_annot()? {
                let annot = annot.unwrap_or((None, None));
                Loc::new(loc, ast::PrimitiveType::DoubleVec {
                    components: n,
                    vtype: annot.0,
                    space: annot.1,
                })
            }
        /   [tok!(TK::TyFloatMat((col, row)), loc)] annot:type_prim_mat_annot()? {
                Loc::new(loc, ast::PrimitiveType::FloatMat {
                    cols: col,
                    rows: row,
                    transform: annot,
                })
            }
        /   [tok!(TK::TyDoubleMat((col, row)), loc)] annot:type_prim_mat_annot()? {

                Loc::new(loc, ast::PrimitiveType::DoubleMat {
                    cols: col,
                    rows: row,
                    transform: annot,
                })
            }

        rule type_prim_vec_annot() -> (Option<Loc<ast::VecType>>, Option<Loc<ast::Identifier>>)
        =   [tok!(TK::Is)] ty:type_vec_type() [tok!(TK::In)] space:identifier() {
                (Some(ty), Some(space))
            }
        /   [tok!(TK::Is)] ty:type_vec_type() {
                (Some(ty), None)
            }
        /   [tok!(TK::In)] space:identifier() {
                (None, Some(space))
            }

        rule type_prim_mat_annot() -> (Loc<ast::Identifier>, Loc<ast::Identifier>)
        = [tok!(TK::From)] f:identifier() [tok!(TK::To)] t:identifier() {
            (f, t)
        }


        rule type_vec_type() -> Loc<ast::VecType>
        = [tok!(TK::Point, loc)] { Loc::new(loc, ast::VecType::Point) }
        / [tok!(TK::Vector, loc)] { Loc::new(loc, ast::VecType::Vector) }
        / [tok!(TK::Colour, loc)] { Loc::new(loc, ast::VecType::Colour) }

        //
        // Terminals
        //

        rule literal() -> Loc<ast::Literal>
        = [tok!(TK::Integer(x), loc)]
        {
            Loc::new(loc, ast::Literal::Integer(x as i128))
        } /
        [tok!(TK::Float(x), loc)]
        {
            Loc::new(loc, ast::Literal::Float(x))
        }

        rule identifier() -> Loc<ast::Identifier>
        = [tok!(TK::Identifier(i), loc)] { Loc::new(loc, i) }

        //
        // Utils
        //

        rule first<T, S>(x: rule<T>, sep: rule<S>) -> T
        = f:x() sep() { f }

        rule sep_trailing<T, S>(x: rule<T>, sep: rule<S>) -> Vec<T>
        =
            // non trailing (at least one)
            items:first(<x()>, <sep()>)* last:x() { let mut items = items; items.push(last); items }
            // trailing trailing (zero or more)
        /   items:first(<x()>, <sep()>)* { items }

    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::tokenise;

    fn check_expr_parses(input: &str) -> Loc<ast::Expression> {
        let toks = tokenise(0, input).collect::<Vec<_>>();
        parser::expression(&toks[..]).unwrap()
    }

    #[test]
    fn expr_literal() {
        check_expr_parses("12");
    }

    #[test]
    fn expr_infix() {
        let ast = check_expr_parses("12 + 30");
        let printed = format!("{:?}", ast);

        assert!(printed.contains("InfixOp"));
    }

    #[test]
    fn expr_prefix() {
        let ast = check_expr_parses("12 + -30");
        let printed = format!("{:?}", ast);

        assert!(printed.contains("PrefixOp"));
    }

    #[test]
    fn expr_call() {
        check_expr_parses("f()");
        check_expr_parses("f(1)");
        check_expr_parses("f(1,)");
        check_expr_parses("f(1,2)");
        check_expr_parses("f(1, y: 2)");
        check_expr_parses("f(1,2,)");
    }

    #[test]
    #[should_panic(expected = "ParseError")]
    fn expr_call_only_comma_fails() {
        check_expr_parses("f(,)");
    }

    #[test]
    fn expr_dot_call() {
        check_expr_parses("x.f()");
        check_expr_parses("x.f(1)");
        check_expr_parses("x.f(1,)");
        check_expr_parses("x.f(1,2)");
        check_expr_parses("x.f(1, y: 2)");
        check_expr_parses("x.f(1,2,)");

        let ast = check_expr_parses("x.f(1, 2)");
        let printed = format!("{:?}", ast);
        assert!(printed.contains("DotCall"));
        assert!(!printed.contains("Field"));
    }

    #[test]
    fn expr_field() {
        let ast = check_expr_parses("v.x");
        let printed = format!("{:?}", ast);

        assert!(printed.contains("Field"));
        assert!(!printed.contains("DotCall"));
    }

    #[test]
    fn expr_index() {
        let ast = check_expr_parses("x[1 + 2]");
        let printed = format!("{:?}", ast);

        assert!(printed.contains("Index"));
        assert!(printed.contains("InfixOp"));
    }

    #[test]
    fn expr_cast() {
        let ast = check_expr_parses("x as float4 is Point");
        let printed = format!("{:?}", ast);

        assert!(printed.contains("As"));
        assert!(printed.contains("\"x\""));
        assert!(printed.contains("FloatVec"));
        assert!(printed.contains("components: VS4"));
        assert!(printed.contains("Point"));
    }

    fn check_type_parses(input: &str) -> Loc<ast::TypeReference> {
        let toks = tokenise(0, input).collect::<Vec<_>>();
        match parser::type_reference(&toks[..]) {
            Ok(val) => val,
            Err(err) => {
                let idx = err.location;
                let err_tok = &toks[idx];
                panic!("Error at token {:?}: {}", err_tok, err.expected)
            }
        }
    }

    #[test]
    fn type_vec_no_annot() {
        let t = check_type_parses("float3");
        let printed = format!("{:?}", t);

        assert!(printed.contains("FloatVec"));
        assert!(printed.contains("components: VS3"));
    }

    #[test]
    fn type_mat_annot() {
        let t = check_type_parses("float4x4 from ObjectSpace to WorldSpace");
        let printed = format!("{:?}", t);

        assert!(printed.contains("FloatMat"));
        assert!(printed.contains("ObjectSpace"));
        assert!(printed.contains("WorldSpace"));
        assert!(printed.contains("cols: VS4"));
        assert!(printed.contains("rows: VS4"));
    }

    #[test]
    fn type_generic() {
        let t = check_type_parses("Texture<float4 is Point in WorldSpace>");
        let printed = format!("{:?}", t);

        assert!(printed.contains("Texture"));
        assert!(printed.contains("FloatVec"));
        assert!(printed.contains("WorldSpace"));
        assert!(printed.contains("components: VS4"));
        assert!(printed.contains("Point"));
    }

    fn check_statement_parses(input: &str) -> Loc<ast::Statement> {
        let toks = tokenise(0, input).collect::<Vec<_>>();
        match parser::statement(&toks[..]) {
            Ok(val) => val,
            Err(err) => {
                let idx = err.location;
                let err_tok = &toks[idx];
                panic!("Error at token {:?}: {}", err_tok, err.expected)
            }
        }
    }

    #[test]
    fn test_stmt_becomes() {
        let s = check_statement_parses("x := 12;");
        let printed = format!("{:?}", s);

        assert!(printed.contains("Becomes"));
        assert!(printed.contains("\"x\""));
        assert!(printed.contains("12"));
    }

    #[test]
    fn test_stmt_var_no_rhs() {
        let s = check_statement_parses("var x: int;");
        let printed = format!("{:?}", s);

        assert!(printed.contains("Var"));
        assert!(printed.contains("\"x\""));
        assert!(printed.contains("Int"));
        assert!(printed.contains("rhs: None"));
    }

    #[test]
    fn test_stmt_var_with_rhs() {
        let s = check_statement_parses("var x: int := 12;");
        let printed = format!("{:?}", s);

        assert!(printed.contains("Var"));
        assert!(printed.contains("\"x\""));
        assert!(printed.contains("Int"));
        assert!(printed.contains("12"));
    }

    #[test]
    fn test_stmt_ifs() {
        check_statement_parses(
            r#"
        if thing = other_thing() then
            var x: int;
            x := 12;
            return x;
        else
            return 10;
        end
        "#,
        );

        check_statement_parses(
            r#"
        if thing() then
            var x: int := 12;
            return x;
        end
        "#,
        );

        check_statement_parses(
            r#"
        if thing() = 4 then
            var x: int;
            x := 12;
            return x;
        elseif other_thing() then
            return 0;
        elseif boop then
            return boop_factor;
        end
        "#,
        );
    }

    #[test]
    fn test_stmt_for_loop() {
        check_statement_parses(
            r#"
        for i in 0 to 10 do
            x := x + i;
        end
        "#,
        );

        check_statement_parses(
            r#"
        for i in 10 downto 0 do
            x := x + i;
        end
        "#,
        );
    }

    fn check_file_parses(input: &str) -> ast::File {
        let toks = tokenise(0, input).collect::<Vec<_>>();
        match parser::file(&toks[..]) {
            Ok(val) => val,
            Err(err) => {
                let idx = err.location;
                let err_tok = &toks[idx];
                panic!("Error at token {:?}: {}", err_tok, err.expected)
            }
        }
    }

    #[test]
    fn test_function() {
        check_file_parses(
            r#"
        function incr(x: int) returns int
        begin
            var tmp: int := x + 1;
            // return tmp;
            result := tmp;
        end
        "#,
        );
    }

    #[test]
    fn test_const_decl() {
        check_file_parses("const TEST: float3 := float3(1, 1, 1);");
    }

    #[test]
    fn test_parses_example_file() {
        let src = r#"
const
    WATER_COLOUR: float3 := float3(0, 117 / 255, 242 / 255);
    Z_NEAR: float := 10;
    Z_FAR: float := 400;

type
    Uniforms = record
        view: float4x4;
        projection: float4x4;
        time_size_width: float;
        viewport_height: float;
    end

const
    [Uniform(set: 0, binding: 0)]
    UNIFORMS: Uniforms;

    [Uniform(set: 0, binding: 1)]
    REFLECTION: Texture<float4>;
    
    [Uniform(set: 0, binding: 2)]
    TERRAIN_DEPTH_TEX: Texture<float4>;
    
    [Uniform(set: 0, binding: 3)]
    COLOUR_SAMPLER: Sampler<float4>;


function to_linear_depth(depth: float) returns float
begin
    var z_n: float := 2 * depth - 1;
    result := 2 * Z_NEAR * Z_FAR / (Z_FAR + Z_NEAR - z_n * (Z_FAR - Z_NEAR));
end



program fragment
input
    [Position]
    frag_coord: float4;

    [Location(0)]
    water_screen_pos: float2;
    [Location(1)]
    fresnel: float;
    [Location(2)]
    light: float3;
output
    [Location(0)]
    colour: float4;
begin
    var reflection_colour: float3;
    reflection_colour := REFLECTION.sample(COLOUR_SAMPLER, water_screen_pos).xyz;
    
    var pixel_depth: float := to_linear_depth(frag_coord.z);
    
    var terrain_data: float4;
    terrain_data := TERRAIN_DEPTH_TEX.sample(
        sampler: COLOUR_SAMPLER,
        coord: frag_coord.xy / float2(UNIFORMS.time_size_width.w, UNIFORMS.viewport_height),
    );
    var terrain_depth: float := to_linear_depth(terrain_data.r);
    
    var dist: float := terrain_depth - pixel_depth;
    var clamped: float := smoothstep(lower: 0, upper: 1.5, value: dist).pow(4.8);
    
    colour.a := clamped * (1 - fresnel);
    
    var final_colour: float3 := light + reflection_colour;
    var depth_colour: float3 := mix(
        start: final_colour,
        end_: water_colour,
        value: smoothstep(lower: 1, upper: 5, value: dist) * 2,
    );
    
    colour.xyz := depth_colour;
end
        "#;

        check_file_parses(src);
    }
}
