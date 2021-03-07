// SPDX-FileCopyrightText: 2021 The thiol developers
//
// SPDX-License-Identifier: EUPL-1.2

use id_arena::Id;
use thiol_hir as hir;
use thiol_syntax::{ast, FileLocation, Loc};

pub fn lower(
    ctx: &mut hir::Context,
    file: &ast::File,
) -> core::result::Result<hir::Module, Vec<Error>> {
    let mut t = Translator {
        ctx,
        errs: Vec::new(),
    };

    let mut module = hir::Module::default();

    for item in &file.items {
        match item {
            ast::Item::Function(f) => {
                if let Ok(id) = t.function(f) {
                    module.functions.push(id);
                }
            }
            ast::Item::Consts(c) => {
                if let Ok(ids) = t.consts(c) {
                    module.consts.extend(ids);
                }
            }
            ast::Item::Types(tys) => {
                if let Ok(tys) = t.types(tys) {
                    module.types.extend(tys);
                }
            }
            ast::Item::Program(p) => {
                if let Ok(id) = t.program(p) {
                    module.programs.push(id);
                }
            }
        }
    }

    if t.errs.is_empty() {
        Ok(module)
    } else {
        Err(t.errs)
    }
}

type Result<T> = core::result::Result<T, ()>;

#[derive(Debug, Clone)]
pub enum Error {
    PositionalArgAfterNamedArg {
        item: FileLocation,
        named_arg: FileLocation,
        pos_arg: FileLocation,
    },
    CallOnNonFunction {
        item: FileLocation,
        base: FileLocation,
    },
    TypeConstructorInInvalidPosition {
        where_: FileLocation,
    },
}

struct Translator<'a> {
    ctx: &'a mut hir::Context,
    errs: Vec<Error>,
}

impl<'a> Translator<'a> {
    fn function(&mut self, f: &Loc<ast::Function>) -> Result<Id<hir::Function>> {
        let func = hir::Function {
            name: self.ident(&f.value.name),
            args: f
                .value
                .args
                .iter()
                .map(|(name, ty)| {
                    let name = self.ident(name);
                    let ty = self.type_reference(ty);
                    (name, ty)
                })
                .collect(),
            ret_type: self.type_reference(&f.value.ret_type),
            body: self.block(&f.value.body)?,
        };

        let id = self.ctx.functions.alloc(func);
        self.ctx.function_fcs.insert(id, f.loc);
        Ok(id)
    }

    fn program(&mut self, p: &Loc<ast::Program>) -> Result<Id<hir::Program>> {
        let prog = hir::Program {
            name: self.ident(&p.value.name),
            inputs: p
                .value
                .inputs
                .iter()
                .map(|var| self.variable_def(&var.value, var.loc))
                .collect::<Result<_>>()?,
            outputs: p
                .value
                .outputs
                .iter()
                .map(|var| self.variable_def(&var.value, var.loc))
                .collect::<Result<_>>()?,
            body: self.block(&p.value.body)?,
        };
        let id = self.ctx.programs.alloc(prog);
        self.ctx.program_fcs.insert(id, p.loc);
        Ok(id)
    }

    fn consts(&mut self, c: &Loc<ast::Consts>) -> Result<Vec<Id<hir::VariableDef>>> {
        c.value
            .vars
            .iter()
            .map(|v| self.variable_def(&v.value, v.loc))
            .collect()
    }

    fn types(&mut self, t: &Loc<ast::Types>) -> Result<Vec<Id<hir::TypeDefinition>>> {
        t.value
            .types
            .iter()
            .map(|t| self.type_definition(t))
            .collect()
    }

    fn type_definition(&mut self, t: &Loc<ast::TypeDefinition>) -> Result<Id<hir::TypeDefinition>> {
        let rhs = match &t.value.rhs.value {
            ast::TypeDefinitionRhs::Alias(ty) => {
                hir::TypeDefinitionRhs::Alias(self.type_reference(ty))
            }
            ast::TypeDefinitionRhs::Record { fields } => hir::TypeDefinitionRhs::Record {
                fields: fields
                    .iter()
                    .map(|v| self.variable_def(&v.value, v.loc))
                    .collect::<Result<_>>()?,
            },
        };
        let rhs_id = self.ctx.type_def_rhss.alloc(rhs);
        self.ctx.type_def_rhs_fcs.insert(rhs_id, t.value.rhs.loc);

        let def = hir::TypeDefinition {
            name: self.ident(&t.value.name),
            generics: t.value.generics.iter().map(|i| self.ident(i)).collect(),
            rhs: rhs_id,
        };
        let def_id = self.ctx.type_defs.alloc(def);
        self.ctx.type_def_fcs.insert(def_id, t.loc);

        Ok(def_id)
    }

    fn statement(&mut self, st: &Loc<ast::Statement>) -> Result<Id<hir::Statement>> {
        let stmt = match &st.value {
            ast::Statement::Var(v) => hir::Statement::Var(self.variable_def(v, st.loc)?),
            ast::Statement::Becomes { lhs, rhs } => {
                // TODO check if lhs is a valid l-value
                let lhs_id = self.expr(lhs)?;
                let rhs_id = self.expr(rhs)?;
                hir::Statement::Becomes {
                    lhs: lhs_id,
                    rhs: rhs_id,
                }
            }
            ast::Statement::Return(None) => hir::Statement::Return(None),
            ast::Statement::Return(Some(e)) => {
                let e = self.expr(e)?;
                hir::Statement::Return(Some(e))
            }
            ast::Statement::Break => hir::Statement::Break,
            ast::Statement::Continue => hir::Statement::Continue,
            ast::Statement::Branch { branches, else_ } => {
                // the `branches()` method calculates the Loc but does not add it to the top
                // most branch, the loc information from the AST is better, so it's added here.
                let (branch_stmt, _loc) = self.branches(branches, else_)?;
                branch_stmt
            }
            ast::Statement::For {
                iter_name,
                loop_type,
                from,
                to,
                body,
            } => {
                let iter_name = self.ident(iter_name);
                let loop_type = match loop_type {
                    ast::ForLoopType::Up => hir::ForLoopType::Up,
                    ast::ForLoopType::Down => hir::ForLoopType::Down,
                };
                let from = self.expr(from)?;
                let to = self.expr(to)?;

                let body = self.block(body)?;

                hir::Statement::For {
                    iter_name,
                    loop_type,
                    from,
                    to,
                    body,
                }
            }
        };
        let id = self.ctx.statements.alloc(stmt);
        self.ctx.statement_fcs.insert(id, st.loc);
        Ok(id)
    }

    fn variable_def(
        &mut self,
        v: &ast::VariableDef,
        loc: FileLocation,
    ) -> Result<Id<hir::VariableDef>> {
        let mut attrs = vec![];
        for attr in &v.attributes {
            let mut hir_attr = hir::Attribute {
                name: self.ident(&attr.value.name),
                pos_args: vec![],
                nam_args: vec![],
            };

            let mut named = false;
            for (name, expr) in &attr.value.args {
                let expr_id = self.expr(expr)?;
                if let Some(n) = name {
                    named = true;
                    let nam = self.ident(n);
                    hir_attr.nam_args.push((nam, expr_id));
                } else if named {
                    self.errs.push(Error::PositionalArgAfterNamedArg {
                        item: attr.loc,
                        named_arg: hir_attr
                            .nam_args
                            .last()
                            .map(|(l, e)| {
                                self.ctx.identifier_fcs[l].merge(self.ctx.expression_fcs[e])
                            })
                            .unwrap(),
                        pos_arg: expr.loc,
                    });
                    return Err(());
                } else {
                    hir_attr.pos_args.push(expr_id);
                }
            }

            let id = self.ctx.attributes.alloc(hir_attr);
            self.ctx.attribute_fcs.insert(id, attr.loc);
            attrs.push(id);
        }

        let name = self.ident(&v.name);
        let ty = self.type_reference(&v.type_);
        let rhs = if let Some(e) = &v.rhs {
            Some(self.expr(e)?)
        } else {
            None
        };

        let def = hir::VariableDef {
            attrs,
            name,
            type_: ty,
            rhs,
        };

        let id = self.ctx.variable_defs.alloc(def);
        self.ctx.variable_def_fcs.insert(id, loc);
        Ok(id)
    }

    fn expr(&mut self, e: &Loc<ast::Expression>) -> Result<Id<hir::Expression>> {
        let expr = match &e.value {
            ast::Expression::Literal(l) => {
                let lit = match l {
                    ast::Literal::Integer(i) => hir::Literal::Integer(*i),
                    ast::Literal::Float(f) => hir::Literal::Float(*f),
                };
                hir::Expression::Literal(lit)
            }
            ast::Expression::Variable(name) => {
                let id = self.ident_loc(name, e.loc);
                hir::Expression::Variable(id)
            }
            ast::Expression::PrimitiveTypeConstructor(_) => {
                self.errs
                    .push(Error::TypeConstructorInInvalidPosition { where_: e.loc });
                return Err(());
            }
            ast::Expression::Call { base, args } => {
                let mut pos_args = vec![];
                let mut nam_args = vec![];

                let mut named = false;

                for (nam, expr) in args {
                    let expr_id = self.expr(expr)?;

                    if let Some(n) = nam {
                        named = true;
                        let nam = self.ident(n);
                        nam_args.push((nam, expr_id));
                    } else if named {
                        self.errs.push(Error::PositionalArgAfterNamedArg {
                            item: e.loc,
                            named_arg: nam_args
                                .last()
                                .map(|(l, e)| {
                                    self.ctx.identifier_fcs[l].merge(self.ctx.expression_fcs[e])
                                })
                                .unwrap(),
                            pos_arg: expr.loc,
                        });
                        return Err(());
                    } else {
                        pos_args.push(expr_id);
                    }
                }

                match &base.value {
                    ast::Expression::Variable(v) => {
                        let name = self.ident_loc(v, base.loc);
                        hir::Expression::Call {
                            name,
                            pos_args,
                            nam_args,
                        }
                    }
                    ast::Expression::PrimitiveTypeConstructor(p) => {
                        let prim = hir::PrimitiveOp::Constructor {
                            ty: self.prim_type(p),
                            pos_args,
                            nam_args,
                        };

                        let prim_id = self.ctx.prim_ops.alloc(prim);
                        self.ctx.prim_op_fcs.insert(prim_id, base.loc);
                        hir::Expression::PrimitiveOp(prim_id)
                    }
                    _ => {
                        self.errs.push(Error::CallOnNonFunction {
                            item: e.loc,
                            base: base.loc,
                        });
                        return Err(());
                    }
                }
            }
            ast::Expression::DotCall { base, name, args } => {
                let name = self.ident(name);
                let mut pos_args = vec![self.expr(base)?];
                let mut nam_args = vec![];

                let mut named = false;

                for (nam, expr) in args {
                    let expr_id = self.expr(expr)?;

                    if let Some(n) = nam {
                        named = true;
                        let nam = self.ident(n);
                        nam_args.push((nam, expr_id));
                    } else if named {
                        self.errs.push(Error::PositionalArgAfterNamedArg {
                            item: e.loc,
                            named_arg: nam_args
                                .last()
                                .map(|(l, e)| {
                                    self.ctx.identifier_fcs[l].merge(self.ctx.expression_fcs[e])
                                })
                                .unwrap(),
                            pos_arg: expr.loc,
                        });
                        return Err(());
                    } else {
                        pos_args.push(expr_id);
                    }
                }

                hir::Expression::Call {
                    name,
                    pos_args,
                    nam_args,
                }
            }
            ast::Expression::InfixOp { op, args } => {
                let lhs = self.expr(&args[0])?;
                let rhs = self.expr(&args[1])?;

                let prim = match op {
                    ast::InfixOp::Add => hir::PrimitiveOp::Add(lhs, rhs),
                    ast::InfixOp::Sub => hir::PrimitiveOp::Sub(lhs, rhs),
                    ast::InfixOp::Mul => hir::PrimitiveOp::Mul(lhs, rhs),
                    ast::InfixOp::Div => hir::PrimitiveOp::Div(lhs, rhs),
                    ast::InfixOp::Mod => hir::PrimitiveOp::Mod(lhs, rhs),
                    ast::InfixOp::Gt => hir::PrimitiveOp::Gt(lhs, rhs),
                    ast::InfixOp::Gte => hir::PrimitiveOp::Gte(lhs, rhs),
                    ast::InfixOp::Lt => hir::PrimitiveOp::Lt(lhs, rhs),
                    ast::InfixOp::Lte => hir::PrimitiveOp::Lte(lhs, rhs),
                    ast::InfixOp::Eq => hir::PrimitiveOp::Eq(lhs, rhs),
                    ast::InfixOp::Neq => hir::PrimitiveOp::Neq(lhs, rhs),
                };

                let prim_id = self.ctx.prim_ops.alloc(prim);
                self.ctx
                    .prim_op_fcs
                    .insert(prim_id, args[0].loc.merge(args[1].loc));

                hir::Expression::PrimitiveOp(prim_id)
            }
            ast::Expression::PrefixOp { op, expr } => {
                let expr = self.expr(expr)?;
                let prim = match op {
                    ast::PrefixOp::Plus => hir::PrimitiveOp::Pos(expr),
                    ast::PrefixOp::Minus => hir::PrimitiveOp::Neg(expr),
                };

                let prim_id = self.ctx.prim_ops.alloc(prim);
                self.ctx.prim_op_fcs.insert(prim_id, e.loc);

                hir::Expression::PrimitiveOp(prim_id)
            }
            ast::Expression::Field { base, name } => {
                let base = self.expr(base)?;
                let name = self.ident(name);
                hir::Expression::Field { base, name }
            }
            ast::Expression::Index { base, index } => {
                let base = self.expr(base)?;
                let index = self.expr(index)?;
                hir::Expression::Index { base, index }
            }
            ast::Expression::As { base, ty } => {
                let base = self.expr(base)?;
                let ty = self.type_reference(ty);
                hir::Expression::As { base, ty }
            }
        };
        let id = self.ctx.expressions.alloc(expr);
        self.ctx.expression_fcs.insert(id, e.loc);
        Ok(id)
    }

    fn type_reference(&mut self, ty: &Loc<ast::TypeReference>) -> Id<hir::TypeReference> {
        let hir_ty = match &ty.value {
            ast::TypeReference::Primitive(prim) => {
                hir::TypeReference::Primitive(self.prim_type(&prim.value))
            }
            ast::TypeReference::Named { name, generics } => hir::TypeReference::Named {
                name: self.ident(&name),
                generics: generics.iter().map(|g| self.type_reference(g)).collect(),
            },
            ast::TypeReference::Array { base, size } => hir::TypeReference::Array {
                base: self.type_reference(base),
                size: size.value,
            },
            ast::TypeReference::OpenArray { base } => {
                hir::TypeReference::OpenArray(self.type_reference(base))
            }
        };
        let id = self.ctx.type_refs.alloc(hir_ty);
        self.ctx.type_ref_fcs.insert(id, ty.loc);
        id
    }

    fn ident(&mut self, ident: &Loc<ast::Identifier>) -> Id<hir::Identifier> {
        let id = self.ctx.identifiers.alloc(ident.value.clone());
        self.ctx.identifier_fcs.insert(id, ident.loc);
        id
    }

    fn ident_loc(&mut self, ident: &str, loc: FileLocation) -> Id<hir::Identifier> {
        let id = self.ctx.identifiers.alloc(ident.to_string());
        self.ctx.identifier_fcs.insert(id, loc);
        id
    }

    fn branches(
        &mut self,
        br: &[(Loc<ast::Expression>, ast::Block)],
        el: &Option<ast::Block>,
    ) -> Result<(hir::Statement, FileLocation)> {
        match br {
            [] => unreachable!(),
            [(cond, block)] => {
                let cond = self.expr(cond)?;
                let block = self.block(block)?;

                let (else_, end) = if let Some(else_) = el {
                    let else_block = self.block(else_)?;
                    let last = else_block.last().unwrap();
                    let loc = self.ctx.statement_fcs[last];
                    (else_block, loc)
                } else {
                    let last = block.last().unwrap();
                    let loc = self.ctx.statement_fcs[last];

                    (vec![], loc)
                };

                let loc = self.ctx.expression_fcs[&cond].merge(end);
                let st = hir::Statement::If {
                    cond,
                    then_body: block,
                    else_body: else_,
                };
                Ok((st, loc))
            }
            [(cond, block), rest @ ..] => {
                let (else_, else_loc) = self.branches(rest, el)?;
                let else_id = self.ctx.statements.alloc(else_);
                self.ctx.statement_fcs.insert(else_id, else_loc);

                let cond = self.expr(cond)?;
                let cond_loc = self.ctx.expression_fcs[&cond];

                let block = self.block(block)?;

                let loc = cond_loc.merge(else_loc);
                let st = hir::Statement::If {
                    cond,
                    then_body: block,
                    else_body: vec![else_id],
                };
                Ok((st, loc))
            }
        }
    }

    fn block(&mut self, block: &[Loc<ast::Statement>]) -> Result<Vec<Id<hir::Statement>>> {
        block.iter().map(|s| self.statement(s)).collect()
    }

    fn prim_type(&mut self, p: &ast::PrimitiveType) -> hir::PrimitiveType {
        match p {
            ast::PrimitiveType::Bool => hir::PrimitiveType::Bool,
            ast::PrimitiveType::Int => hir::PrimitiveType::Int,
            ast::PrimitiveType::UInt => hir::PrimitiveType::UInt,
            ast::PrimitiveType::Float => hir::PrimitiveType::Float,
            ast::PrimitiveType::Double => hir::PrimitiveType::Double,
            ast::PrimitiveType::BoolVec { components } => hir::PrimitiveType::BoolVec {
                components: vec_size(components),
            },
            ast::PrimitiveType::IntVec {
                components,
                vtype,
                space,
            } => hir::PrimitiveType::IntVec {
                components: vec_size(components),
                vtype: vtype.as_ref().map(|s| self.vec_type(s)),
                space: space.as_ref().map(|i| self.ident(i)),
            },
            ast::PrimitiveType::UIntVec {
                components,
                vtype,
                space,
            } => hir::PrimitiveType::UIntVec {
                components: vec_size(components),
                vtype: vtype.as_ref().map(|s| self.vec_type(s)),
                space: space.as_ref().map(|i| self.ident(i)),
            },
            ast::PrimitiveType::FloatVec {
                components,
                vtype,
                space,
            } => hir::PrimitiveType::FloatVec {
                components: vec_size(components),
                vtype: vtype.as_ref().map(|s| self.vec_type(s)),
                space: space.as_ref().map(|i| self.ident(i)),
            },
            ast::PrimitiveType::DoubleVec {
                components,
                vtype,
                space,
            } => hir::PrimitiveType::DoubleVec {
                components: vec_size(components),
                vtype: vtype.as_ref().map(|s| self.vec_type(s)),
                space: space.as_ref().map(|i| self.ident(i)),
            },
            ast::PrimitiveType::FloatMat {
                cols,
                rows,
                transform,
            } => hir::PrimitiveType::FloatMat {
                cols: vec_size(cols),
                rows: vec_size(rows),
                transform: transform
                    .as_ref()
                    .map(|(a, b)| (self.ident(a), self.ident(b))),
            },
            ast::PrimitiveType::DoubleMat {
                cols,
                rows,
                transform,
            } => hir::PrimitiveType::DoubleMat {
                cols: vec_size(cols),
                rows: vec_size(rows),
                transform: transform
                    .as_ref()
                    .map(|(a, b)| (self.ident(a), self.ident(b))),
            },
        }
    }

    fn vec_type(&mut self, ty: &Loc<ast::VecType>) -> Id<hir::VecType> {
        let hir_ty = match ty.value {
            ast::VecType::Point => hir::VecType::Point,
            ast::VecType::Vector => hir::VecType::Vector,
            ast::VecType::Colour => hir::VecType::Colour,
        };
        let id = self.ctx.vec_types.alloc(hir_ty);
        self.ctx.vec_type_fcs.insert(id, ty.loc);
        id
    }
}

fn vec_size(components: &ast::VecSize) -> hir::VecSize {
    match components {
        ast::VecSize::VS2 => hir::VecSize::VS2,
        ast::VecSize::VS3 => hir::VecSize::VS3,
        ast::VecSize::VS4 => hir::VecSize::VS4,
    }
}
