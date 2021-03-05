#![allow(clippy::redundant_closure_call)]

use crate::lexer::{Token, TokenKind as TK};
use crate::{HasLoc, Loc};

use crate::ast;

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
        // Statement
        //

        pub rule statement() -> Loc<ast::Statement>
        =
            [tok!(TK::Var, start)] name:identifier() [tok!(TK::Colon)] ty:type_reference()
            [tok!(TK::Becomes)] rhs:expression()
            [tok!(TK::SemiColon, end)] {
                Loc::new(
                    start.merge(end),
                    ast::Statement::Var {
                        name,
                        type_ref: ty,
                        rhs: Some(rhs),
                    }
                )
            }
        /   [tok!(TK::Var, start)] name:identifier() [tok!(TK::Colon)] ty:type_reference()
            [tok!(TK::SemiColon, end)] {
                Loc::new(
                    start.merge(end),
                    ast::Statement::Var {
                        name,
                        type_ref: ty,
                        rhs: None,
                    }
                )
            }
            // expr-lhs := expr;
        /   lhs:expression_atom() [tok!(TK::Becomes)] rhs:expression() [tok!(TK::SemiColon, end)] {
                Loc::new(
                    lhs.loc().merge(end),
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
                    x.loc().merge(y.loc()),
                    ast::Expression::InfixOp { op: ast::InfixOp::Eq, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::NotEquals)] y:@ {
                Loc::new(
                    x.loc().merge(y.loc()),
                    ast::Expression::InfixOp { op: ast::InfixOp::Neq, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::LessThan)] y:@ {
                Loc::new(
                    x.loc().merge(y.loc()),
                    ast::Expression::InfixOp { op: ast::InfixOp::Lt, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::LessThanEqual)] y:@ {
                Loc::new(
                    x.loc().merge(y.loc()),
                    ast::Expression::InfixOp { op: ast::InfixOp::Lte, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::GreaterThan)] y:@ {
                Loc::new(
                    x.loc().merge(y.loc()),
                    ast::Expression::InfixOp { op: ast::InfixOp::Gt, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::GreaterThanEqual)] y:@ {
                Loc::new(
                    x.loc().merge(y.loc()),
                    ast::Expression::InfixOp { op: ast::InfixOp::Gte, args: Box::new([x, y]), },
                )
            }
            --
            x:(@) [tok!(TK::Plus, opl)] y:@ {
                Loc::new(
                    x.loc().merge(y.loc()),
                    ast::Expression::InfixOp { op: ast::InfixOp::Add, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::Minus, opl)] y:@ {
                Loc::new(
                    x.loc().merge(y.loc()),
                    ast::Expression::InfixOp { op: ast::InfixOp::Sub, args: Box::new([x, y]), },
                )
            }
            --
            x:(@) [tok!(TK::Star, opl)] y:@ {
                Loc::new(
                    x.loc().merge(y.loc()),
                    ast::Expression::InfixOp { op: ast::InfixOp::Mul, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::Slash, opl)] y:@ {
                Loc::new(
                    x.loc().merge(y.loc()),
                    ast::Expression::InfixOp { op: ast::InfixOp::Div, args: Box::new([x, y]), },
                )
            }
            x:(@) [tok!(TK::Mod, opl)] y:@ {
                Loc::new(
                    x.loc().merge(y.loc()),
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
                Loc::new(opl.merge(arg.loc()), ast::Expression::PrefixOp { op: ast::PrefixOp::Plus, expr: Box::new(arg) })
            }
            [tok!(TK::Minus, opl)] arg:@ {
                Loc::new(opl.merge(arg.loc()), ast::Expression::PrefixOp { op: ast::PrefixOp::Minus, expr: Box::new(arg) })
            }
            --
            atom:expression_atom() { atom }
        }

        rule expression_atom() -> Loc<ast::Expression> = precedence!{
            base:@ [tok!(TK::BracketOpen)] idx:expression() [tok!(TK::BracketClose, loc)]
            {
                Loc::new(base.loc().merge(loc), ast::Expression::Index {
                    base: Box::new(base),
                    index: Box::new(idx),
                })
            }
            --
            base:@ [tok!(TK::Dot)] name:identifier()
            [tok!(TK::ParenOpen)] args:arglist() [tok!(TK::ParenClose, loc)]
            {
                Loc::new(base.loc().merge(loc), ast::Expression::DotCall {
                    base: Box::new(base),
                    name,
                    args,
                })
            }
            --
            base:@ [tok!(TK::ParenOpen)] args:arglist() [tok!(TK::ParenClose, loc)] {
                Loc::new(base.loc().merge(loc), ast::Expression::Call {
                    base: Box::new(base),
                    args,
                })
            }
            --
            base:@ [tok!(TK::Dot)] name:identifier() {
                Loc::new(
                    base.loc().merge(name.loc()),
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
        // Types
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

        /   [tok!(TK::TyIntVec(n), loc)] annot:type_prim_vec_annot() {
                Loc::new(loc, ast::PrimitiveType::IntVec {
                    components: n,
                    vtype: annot.0,
                    space: annot.1,
                })
            }
        /   [tok!(TK::TyUIntVec(n), loc)] annot:type_prim_vec_annot() {
                Loc::new(loc, ast::PrimitiveType::UIntVec {
                    components: n,
                    vtype: annot.0,
                    space: annot.1,
                })
            }
        /   [tok!(TK::TyFloatVec(n), loc)] annot:type_prim_vec_annot() {
                Loc::new(loc, ast::PrimitiveType::FloatVec {
                    components: n,
                    vtype: annot.0,
                    space: annot.1,
                })
            }
        /   [tok!(TK::TyDoubleVec(n), loc)] annot:type_prim_vec_annot() {
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
        =
            [tok!(TK::Is)] ty:type_vec_type() [tok!(TK::In)] space:identifier() {
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
        let ast = check_expr_parses("x as float4 is point");
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
        let t = check_type_parses("Texture<float4 is point in WorldSpace>");
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
}
