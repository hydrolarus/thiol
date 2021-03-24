// SPDX-FileCopyrightText: 2021 The thiol developers
//
// SPDX-License-Identifier: EUPL-1.2

use pretty::RcDoc as Doc;
use thiol_hir as hir;
use thiol_typeck as ty;
use ty::VecType;

pub(crate) fn dump_type_context<'a>(
    hir: &'a thiol_hir::Context,
    ctx: &'a thiol_typeck::Context,
) -> String {
    let printer = TypePrinter { _hir: hir, ty: ctx };
    let doc = lines(ctx.complete_types.iter().map(|(name, id)| {
        Doc::nil()
            .append(name)
            .append(" = ")
            .append(printer.print_type(*id))
    }));
    let mut v = Vec::new();
    doc.render(80, &mut v).unwrap();
    String::from_utf8_lossy(&v).to_string()
}

struct TypePrinter<'a> {
    _hir: &'a hir::Context,
    ty: &'a ty::Context,
}

impl<'a> TypePrinter<'a> {
    fn print_type(&self, id: ty::TypeId) -> Doc {
        let type_ = self.ty.types.get_by_right(&id).unwrap();

        match &type_ {
            ty::Type::Bool => Doc::text("bool"),
            ty::Type::Int => Doc::text("int"),
            ty::Type::UInt => Doc::text("uint"),
            ty::Type::Float => Doc::text("float"),
            ty::Type::Double => Doc::text("double"),
            ty::Type::BoolVec { components } => Doc::text("bool").append(comp_size(components)),
            ty::Type::IntVec {
                components,
                vtype,
                space,
            } => {
                let initial = Doc::text("int")
                    .append(comp_size(components))
                    .append(comp_type(vtype));

                if let Some(space) = space {
                    initial.append(format!("{{{}}}", space))
                } else {
                    initial
                }
            }
            ty::Type::UIntVec {
                components,
                vtype,
                space,
            } => {
                let initial = Doc::text("uint")
                    .append(comp_size(components))
                    .append(comp_type(vtype));

                if let Some(space) = space {
                    initial.append(format!("{{{}}}", space))
                } else {
                    initial
                }
            }
            ty::Type::FloatVec {
                components,
                vtype,
                space,
            } => {
                let initial = Doc::text("float")
                    .append(comp_size(components))
                    .append(comp_type(vtype));

                if let Some(space) = space {
                    initial.append(format!("{{{}}}", space))
                } else {
                    initial
                }
            }
            ty::Type::DoubleVec {
                components,
                vtype,
                space,
            } => {
                let initial = Doc::text("double")
                    .append(comp_size(components))
                    .append(comp_type(vtype));

                if let Some(space) = space {
                    initial.append(format!("{{{}}}", space))
                } else {
                    initial
                }
            }
            ty::Type::FloatMat {
                cols,
                rows,
                transform,
            } => {
                let initial = Doc::text("float")
                    .append(comp_size(cols))
                    .append("x")
                    .append(comp_size(rows));

                if let Some((from, to)) = transform {
                    initial.append(format!("{{{} ⇒ {}}}", from, to))
                } else {
                    initial
                }
            }
            ty::Type::DoubleMat {
                cols,
                rows,
                transform,
            } => {
                let initial = Doc::text("double")
                    .append(comp_size(cols))
                    .append("x")
                    .append(comp_size(rows));

                if let Some((from, to)) = transform {
                    initial.append(format!("{{{} ⇒ {}}}", from, to))
                } else {
                    initial
                }
            }
            ty::Type::Array { base, size } => Doc::text(format!("array[{}] of (", *size))
                .append(self.print_type(*base))
                .append(")"),
            ty::Type::OpenArray { base } => Doc::text("array of (")
                .append(self.print_type(*base))
                .append(")"),
            ty::Type::Record { fields } => {
                let fields = lines(fields.iter().map(|(name, id)| {
                    Doc::nil()
                        .append(name)
                        .append(" : ")
                        .append(self.print_type(*id))
                        .group()
                }));

                Doc::text("record")
                    .append(Doc::line().append(fields).group().nest(4))
                    .append(Doc::line())
                    .append("end")
                    .group()
            }
            ty::Type::Distinct { distinct_id, inner } => {
                Doc::text(format!("({}) ", distinct_id)).append(self.print_type(*inner))
            }
        }
    }
}

fn comp_size(s: &ty::types::VecSize) -> &'static str {
    match s {
        ty::VecSize::VS2 => "2",
        ty::VecSize::VS3 => "3",
        ty::VecSize::VS4 => "4",
    }
}

fn comp_type(t: &ty::types::VecType) -> &'static str {
    match t {
        VecType::Unknown => "",
        VecType::Point => "[Point]",
        VecType::Vector => "[Vector]",
        VecType::Colour => "[Colour]",
    }
}

fn lines<'a>(i: impl IntoIterator<Item = Doc<'a>>) -> Doc<'a> {
    Doc::nil()
        .append(Doc::intersperse(
            i.into_iter().map(|d| d.group()),
            Doc::hardline(),
        ))
        .group()
}
