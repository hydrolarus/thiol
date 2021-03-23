// SPDX-FileCopyrightText: 2021 The thiol developers
//
// SPDX-License-Identifier: EUPL-1.2

use std::collections::{BTreeMap, HashMap, HashSet};

use hir::{FileLocation, Identifier, TypeDefinition};
use thiol_hir::{self as hir, TypeReference};

use bimap::BiBTreeMap;
use id_arena::Id;

pub mod types;
pub use types::*;

pub enum Error {
    TypeRedefinition {
        previous_name: FileLocation,
        redefinition_name: FileLocation,
        redefinition: FileLocation,
    },

    GenericParamaterRedefinition {
        previous_name: FileLocation,
        redefinition: FileLocation,
    },

    RecursiveTypeDefinition {
        type_def: FileLocation,
        type_name: FileLocation,
        recurive_usages: Vec<FileLocation>,
    },
    MutuallyRecursiveTypeDefinitions {
        type_def_idents: Vec<FileLocation>,
    },

    UndefinedType {
        name: String,
        uses: Vec<FileLocation>,
    },

    HigherKindedGenericTypeUsed {
        loc: FileLocation,
        generic_name: Identifier,
    },
    MismatchedNumberGenericArgs {
        loc: FileLocation,
        given: usize,
        expected: usize,
        def_loc: FileLocation,
    },

    FieldRedefinition {
        previous_name: FileLocation,
        redefinition_name: FileLocation,
        item: FileLocation,
    },
}

pub fn type_check(
    ty_ctx: &mut Context,
    hir_ctx: &hir::Context,
    module: &hir::Module,
) -> Result<(), Vec<Error>> {
    let mut errs = vec![];

    // sort type definitions by dependency
    let mut tyname_to_node = HashMap::new();
    let mut g =
        petgraph::graph::Graph::<Option<Id<TypeDefinition>>, petgraph::graph::NodeIndex>::new();

    let mut deps = HashMap::new();

    for ty in &module.types {
        let ty_def = &hir_ctx.type_defs[*ty];
        let ty_name = &hir_ctx.identifiers[ty_def.name];

        let node = g.add_node(Some(*ty));

        // definition with the same name
        if let Some(prev_idx) = tyname_to_node.insert(ty_name.clone(), node) {
            let prev_id = g[prev_idx].unwrap();
            let prev_def = &hir_ctx.type_defs[prev_id];

            errs.push(Error::TypeRedefinition {
                previous_name: hir_ctx.identifier_fcs[&prev_def.name],
                redefinition_name: hir_ctx.identifier_fcs[&ty_def.name],
                redefinition: hir_ctx.type_def_fcs[ty],
            });
            continue;
        }
    }

    for ty in &module.types {
        let def = &hir_ctx.type_defs[*ty];
        let ty_name = &hir_ctx.identifiers[def.name];
        if let Err(err) = type_def_deps(hir_ctx, def, &mut deps) {
            errs.push(err);
            continue;
        }

        if let Some(usages) = deps.get(ty_name.as_str()) {
            errs.push(Error::RecursiveTypeDefinition {
                type_def: hir_ctx.type_def_fcs[ty],
                type_name: hir_ctx.identifier_fcs[&def.name],
                recurive_usages: usages.clone(),
            });
            continue;
        }

        let self_node = tyname_to_node[ty_name];

        for (name, uses) in deps.drain() {
            if let Some(id) = tyname_to_node.get(name) {
                g.add_edge(self_node, *id, Default::default());
            } else {
                errs.push(Error::UndefinedType {
                    name: name.to_string(),
                    uses,
                });
                continue;
            };
        }
    }

    if !errs.is_empty() {
        return Err(errs);
    }

    let groups = petgraph::algo::tarjan_scc(&g);

    for group in groups {
        if group.len() > 1 {
            errs.push(Error::MutuallyRecursiveTypeDefinitions {
                type_def_idents: group
                    .into_iter()
                    .map(|id| {
                        let id = g[id].unwrap();
                        hir_ctx.identifier_fcs[&hir_ctx.type_defs[id].name]
                    })
                    .collect(),
            });
            continue;
        }

        debug_assert_eq!(group.len(), 1);

        let id = g[group[0]].unwrap();

        if let Err(errors) = ty_ctx.add_type_definition(hir_ctx, id) {
            errs.extend(errors);
        }
    }

    if errs.is_empty() {
        Ok(())
    } else {
        Err(errs)
    }
}

#[derive(Default, Clone)]
pub struct Context {
    pub defs: BTreeMap<Identifier, Id<TypeDefinition>>,
    // generic types are *incomplete* before they are applied, but
    // non generic types will be able to be mapped directly to a type
    pub complete_types: BTreeMap<Identifier, TypeId>,

    pub types: BiBTreeMap<Type, TypeId>,
}

impl Context {
    fn add_type_definition(
        &mut self,
        ctx: &hir::Context,
        id: Id<TypeDefinition>,
    ) -> Result<(), Vec<Error>> {
        let def = &ctx.type_defs[id];
        let def_loc = ctx.type_def_fcs[&id];
        let name = &ctx.identifiers[def.name];

        let old = self.defs.insert(name.clone(), id);
        debug_assert!(old.is_none());

        // "complete" types (types without generics) can be stored separately and
        // already be translated (instead of only validated)
        if def.generics.is_empty() {
            let ty_id = match &ctx.type_def_rhss[def.rhs] {
                hir::TypeDefinitionRhs::Alias(id) => self
                    .ty_ref(ctx, *id, &Default::default())
                    .map_err(|err| vec![err])?,
                hir::TypeDefinitionRhs::Record { fields: field_ids } => {
                    let mut errs = vec![];
                    let mut fields_so_far = HashMap::new();

                    let mut fields = vec![];

                    for field in field_ids {
                        let var_def = &ctx.variable_defs[*field];
                        let var_name = &ctx.identifiers[var_def.name];
                        let var_fc = ctx.identifier_fcs[&var_def.name];

                        if let Some(prev) = fields_so_far.insert(var_name.as_str(), var_fc) {
                            errs.push(Error::FieldRedefinition {
                                item: def_loc,
                                previous_name: prev,
                                redefinition_name: var_fc,
                            });
                        }

                        match self.ty_ref(ctx, var_def.type_, &Default::default()) {
                            Ok(id) => {
                                fields.push((var_name.clone(), id));
                            }
                            Err(err) => {
                                errs.push(err);
                            }
                        }
                    }

                    if !errs.is_empty() {
                        return Err(errs);
                    }

                    self.add_or_get_type(Type::Record { fields })
                }
            };
            let old = self.complete_types.insert(name.clone(), ty_id);
            debug_assert!(old.is_none());
            Ok(())
        } else {
            let generics = def
                .generics
                .iter()
                .map(|id| ctx.identifiers[*id].as_str())
                .collect();

            let mut errs = vec![];
            match &ctx.type_def_rhss[def.rhs] {
                hir::TypeDefinitionRhs::Alias(id) => {
                    if let Err(err) = self.ty_validate_ref(ctx, *id, &generics) {
                        errs.push(err);
                    }
                }
                hir::TypeDefinitionRhs::Record { fields } => {
                    let mut fields_so_far = HashMap::new();

                    for field in fields {
                        let var_def = &ctx.variable_defs[*field];
                        let var_name = &ctx.identifiers[var_def.name];
                        let var_fc = ctx.identifier_fcs[&var_def.name];

                        if let Some(prev) = fields_so_far.insert(var_name.as_str(), var_fc) {
                            errs.push(Error::FieldRedefinition {
                                item: def_loc,
                                previous_name: prev,
                                redefinition_name: var_fc,
                            });
                        }

                        if let Err(err) = self.ty_validate_ref(ctx, var_def.type_, &generics) {
                            errs.push(err);
                        }
                    }
                }
            }
            if errs.is_empty() {
                Ok(())
            } else {
                Err(errs)
            }
        }
    }

    fn add_or_get_type(&mut self, ty: Type) -> TypeId {
        if let Some(id) = self.types.get_by_left(&ty) {
            *id
        } else {
            let next_id = TypeId(self.types.len());
            let res = self.types.insert(ty, next_id);
            debug_assert_eq!(res.did_overwrite(), false);
            next_id
        }
    }

    fn ty_named(
        &mut self,
        ctx: &hir::Context,
        loc: FileLocation,
        name: &str,
        generics: &[TypeId],
    ) -> Result<TypeId, Error> {
        // fast path: check if the type is a known complete type
        if let Some(ty) = self.complete_types.get(name) {
            if generics.is_empty() {
                return Ok(*ty);
            } else {
                // There was a complete type but this one uses generics!
                // Instead of duplicating the error handling logic we continue
                // on the non-complete path
            }
        }

        if let Some(id) = self.defs.get(name) {
            let def_loc = ctx.type_def_fcs[id];
            let def = &ctx.type_defs[*id];

            if def.generics.len() != generics.len() {
                Err(Error::MismatchedNumberGenericArgs {
                    loc,
                    expected: def.generics.len(),
                    given: generics.len(),
                    def_loc,
                })
            } else {
                let subst = def
                    .generics
                    .iter()
                    .map(|id| ctx.identifiers[*id].as_str())
                    .zip(generics.iter().copied())
                    .collect();

                let ty = match &ctx.type_def_rhss[def.rhs] {
                    hir::TypeDefinitionRhs::Alias(id) => {
                        return self.ty_ref(ctx, *id, &subst);
                    }
                    hir::TypeDefinitionRhs::Record { fields } => {
                        let mut record_fields = Vec::with_capacity(fields.len());
                        for field in fields {
                            let def = &ctx.variable_defs[*field];
                            let name = ctx.identifiers[def.name].clone();
                            let field_ty = self.ty_ref(ctx, def.type_, &subst)?;
                            record_fields.push((name, field_ty));
                        }
                        Type::Record {
                            fields: record_fields,
                        }
                    }
                };
                Ok(self.add_or_get_type(ty))
            }
        } else {
            Err(Error::UndefinedType {
                name: name.to_string(),
                uses: vec![loc],
            })
        }
    }

    fn ty_ref(
        &mut self,
        ctx: &hir::Context,
        id: Id<TypeReference>,
        subst: &HashMap<&str, TypeId>,
    ) -> Result<TypeId, Error> {
        use hir::PrimitiveType as PT;
        use TypeReference as TR;

        let ty_ref = &ctx.type_refs[id];

        let ty = match ty_ref {
            TR::Primitive(prim) => match prim {
                PT::Bool => Type::Bool,
                PT::Int => Type::Int,
                PT::UInt => Type::UInt,
                PT::Float => Type::Float,
                PT::Double => Type::Double,
                PT::BoolVec { components } => Type::BoolVec {
                    components: (*components).into(),
                },
                PT::IntVec {
                    components,
                    vtype,
                    space,
                } => Type::IntVec {
                    components: (*components).into(),
                    vtype: vtype.map(|ty| ctx.vec_types[ty]).into(),
                    space: space.map(|id| ctx.identifiers[id].clone()),
                },
                PT::UIntVec {
                    components,
                    vtype,
                    space,
                } => Type::UIntVec {
                    components: (*components).into(),
                    vtype: vtype.map(|ty| ctx.vec_types[ty]).into(),
                    space: space.map(|id| ctx.identifiers[id].clone()),
                },
                PT::FloatVec {
                    components,
                    vtype,
                    space,
                } => Type::FloatVec {
                    components: (*components).into(),
                    vtype: vtype.map(|ty| ctx.vec_types[ty]).into(),
                    space: space.map(|id| ctx.identifiers[id].clone()),
                },
                PT::DoubleVec {
                    components,
                    vtype,
                    space,
                } => Type::DoubleVec {
                    components: (*components).into(),
                    vtype: vtype.map(|ty| ctx.vec_types[ty]).into(),
                    space: space.map(|id| ctx.identifiers[id].clone()),
                },
                PT::FloatMat {
                    cols,
                    rows,
                    transform,
                } => Type::FloatMat {
                    cols: (*cols).into(),
                    rows: (*rows).into(),
                    transform: transform.map(|(from, to)| {
                        (ctx.identifiers[from].clone(), ctx.identifiers[to].clone())
                    }),
                },
                PT::DoubleMat {
                    cols,
                    rows,
                    transform,
                } => Type::DoubleMat {
                    cols: (*cols).into(),
                    rows: (*rows).into(),
                    transform: transform.map(|(from, to)| {
                        (ctx.identifiers[from].clone(), ctx.identifiers[to].clone())
                    }),
                },
            },
            TR::OpenArray(inner) => {
                let inner_id = self.ty_ref(ctx, *inner, subst)?;
                Type::OpenArray { base: inner_id }
            }
            TR::Array { base, size } => {
                let inner_id = self.ty_ref(ctx, *base, subst)?;
                Type::Array {
                    base: inner_id,
                    size: *size,
                }
            }
            TR::Named { name, generics } => {
                let loc = ctx.type_ref_fcs[&id];
                let mut gens = Vec::with_capacity(generics.len());
                for id in generics {
                    let id: TypeId = self.ty_ref(ctx, *id, subst)?;
                    gens.push(id);
                }

                let name = &ctx.identifiers[*name];

                if let Some(subst_id) = subst.get(name.as_str()) {
                    if !gens.is_empty() {
                        return Err(Error::HigherKindedGenericTypeUsed {
                            generic_name: name.clone(),
                            loc,
                        });
                    }
                    return Ok(*subst_id);
                } else {
                    return self.ty_named(ctx, loc, name, &gens);
                }
            }
        };

        Ok(self.add_or_get_type(ty))
    }

    /// Validate a type reference
    ///
    /// Used to check that a type definition is valid without having to instantiate
    /// generics
    fn ty_validate_ref(
        &self,
        ctx: &hir::Context,
        id: Id<TypeReference>,
        generics: &HashSet<&str>,
    ) -> Result<(), Error> {
        let ty_ref = &ctx.type_refs[id];

        match ty_ref {
            TypeReference::Primitive(_) => Ok(()),
            TypeReference::OpenArray(inner) => self.ty_validate_ref(ctx, *inner, generics),
            TypeReference::Array { base, size: _ } => self.ty_validate_ref(ctx, *base, generics),
            TypeReference::Named {
                name,
                generics: applied_gens,
            } => {
                let loc = ctx.type_ref_fcs[&id];
                let name_s = &ctx.identifiers[*name];
                if generics.contains(name_s.as_str()) {
                    if !applied_gens.is_empty() {
                        Err(Error::HigherKindedGenericTypeUsed {
                            generic_name: name_s.clone(),
                            loc,
                        })
                    } else {
                        Ok(())
                    }
                } else if let Some(def_id) = self.defs.get(name_s.as_str()) {
                    let def = &ctx.type_defs[*def_id];
                    let def_loc = ctx.type_def_fcs[def_id];
                    if def.generics.len() != applied_gens.len() {
                        Err(Error::MismatchedNumberGenericArgs {
                            loc,
                            expected: def.generics.len(),
                            given: applied_gens.len(),
                            def_loc,
                        })
                    } else {
                        for gen in applied_gens {
                            self.ty_validate_ref(ctx, *gen, generics)?;
                        }
                        Ok(())
                    }
                } else {
                    Err(Error::UndefinedType {
                        name: name_s.to_string(),
                        uses: vec![loc],
                    })
                }
            }
        }
    }
}

fn type_def_deps<'a>(
    ctx: &'a hir::Context,
    ty: &hir::TypeDefinition,
    deps: &mut HashMap<&'a str, Vec<FileLocation>>,
) -> Result<(), Error> {
    let mut generics = HashMap::new();

    for gen in &ty.generics {
        let name = &ctx.identifiers[*gen];
        let loc = ctx.identifier_fcs[gen];

        if let Some(prev) = generics.insert(name.as_str(), loc) {
            return Err(Error::GenericParamaterRedefinition {
                previous_name: prev,
                redefinition: loc,
            });
        }
    }

    let rhs = &ctx.type_def_rhss[ty.rhs];
    match rhs {
        hir::TypeDefinitionRhs::Alias(ty) => {
            type_ref_deps(ctx, *ty, deps);
        }
        hir::TypeDefinitionRhs::Record { fields } => {
            for field in fields {
                let def = &ctx.variable_defs[*field];
                type_ref_deps(ctx, def.type_, deps);
            }
        }
    }

    for (gen, _) in generics {
        deps.remove(gen);
    }

    Ok(())
}

fn type_ref_deps<'a>(
    ctx: &'a hir::Context,
    ty: Id<hir::TypeReference>,
    deps: &mut HashMap<&'a str, Vec<FileLocation>>,
) {
    let ty_ref = &ctx.type_refs[ty];
    match ty_ref {
        TypeReference::Primitive(_) => {}
        TypeReference::OpenArray(base) => type_ref_deps(ctx, *base, deps),
        TypeReference::Array { base, size: _ } => type_ref_deps(ctx, *base, deps),
        TypeReference::Named { name, generics } => {
            let usage_loc = ctx.identifier_fcs[name];
            let name = &ctx.identifiers[*name];

            let locs = deps.entry(name.as_str()).or_default();
            locs.push(usage_loc);

            for gen in generics {
                type_ref_deps(ctx, *gen, deps);
            }
        }
    }
}
