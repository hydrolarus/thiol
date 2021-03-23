// SPDX-FileCopyrightText: 2021 The thiol developers
//
// SPDX-License-Identifier: EUPL-1.2

use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    files::Files,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use thiol_syntax::parser;
use thiol_syntax::FileId;

use anyhow::bail;
use clap::Clap;
use std::path::PathBuf;

mod pretty_printing;

#[derive(Debug, Clap)]
#[clap(version = "0.1", author = "Tia")]
struct Arguments {
    #[clap(long)]
    parse_only: bool,

    #[clap(long)]
    dump_type_context: bool,

    /// Do not display colours in the terminal output
    #[clap(long)]
    no_colour: bool,

    file_paths: Vec<PathBuf>,
}

fn main() {
    match run() {
        Ok(()) => {}
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1)
        }
    }
}

fn run() -> anyhow::Result<()> {
    let args: Arguments = Arguments::parse();

    let mut hir_ctx = thiol_hir::Context::default();

    let mut files = codespan_reporting::files::SimpleFiles::new();

    for path in &args.file_paths {
        let content = std::fs::read_to_string(path)?;
        let id = files.add(path.display().to_string(), content);
        let src = files.source(id).unwrap();

        let ast = match parser::parse_file(id, src) {
            Ok(val) => val,
            Err(err) => {
                let diag = parse_error_to_diag(err);
                emit(!args.no_colour, &files, diag);

                bail!("Aborting due to previous error")
            }
        };

        let module = match thiol_ast_lowering::lower(&mut hir_ctx, &ast) {
            Ok(module) => module,
            Err(errs) => {
                for err in errs {
                    let diag = ast_lowering_error_to_diag(err);
                    emit(!args.no_colour, &files, diag);
                }
                bail!("aborting due to previous error");
            }
        };

        let mut ty_ctx = thiol_typeck::Context::default();
        match thiol_typeck::type_check(&mut ty_ctx, &hir_ctx, &module) {
            Ok(_) => {}
            Err(errs) => {
                for err in errs {
                    let diag = typeck_error_to_diag(err);
                    emit(!args.no_colour, &files, diag);
                }
                bail!("aboring due to previous error")
            }
        }

        if args.dump_type_context {
            println!("{}", pretty_printing::dump_type_context(&hir_ctx, &ty_ctx));
        }
    }

    if args.parse_only {
        return Ok(());
    }

    Ok(())
}

fn colour_choice(coloured: bool) -> ColorChoice {
    if coloured {
        ColorChoice::Auto
    } else {
        ColorChoice::Never
    }
}

fn emit<'a>(coloured: bool, files: &'a impl Files<'a, FileId = FileId>, diag: Diagnostic<FileId>) {
    let mut writer = StandardStream::stderr(colour_choice(coloured));
    let term_config = Config::default();

    codespan_reporting::term::emit(&mut writer, &term_config, files, &diag).unwrap();
}

fn parse_error_to_diag(err: thiol_syntax::parser::ParseError) -> Diagnostic<FileId> {
    let label =
        Label::primary(err.location.file, err.location.range()).with_message("Unexpected token");
    let mut notes = vec!["Expected one of the following token types:".to_string()];
    notes.extend(err.expected_tokens.into_iter().map(|s| format!(" - {}", s)));
    let note = notes.join("\n");

    Diagnostic::error()
        .with_message("parse error")
        .with_labels(vec![label])
        .with_notes(vec![note])
}

fn ast_lowering_error_to_diag(err: thiol_ast_lowering::Error) -> Diagnostic<FileId> {
    use thiol_ast_lowering::Error;

    match err {
        Error::PositionalArgAfterNamedArg {
            item: _,
            named_arg,
            pos_arg,
        } => {
            let prim = Label::primary(pos_arg.file, pos_arg.range())
                .with_message("positional argument used here");
            let other = Label::secondary(named_arg.file, named_arg.range())
                .with_message("named argument used here");

            Diagnostic::error()
                .with_message("positional argument used after named argument")
                .with_labels(vec![prim, other])
        }
        Error::CallOnNonFunction { item: _, base } => {
            let prim =
                Label::primary(base.file, base.range()).with_message("not a callable expression");
            Diagnostic::error()
                .with_message("function call on a non-callable value")
                .with_labels(vec![prim])
        }
        Error::TypeConstructorInInvalidPosition { where_ } => {
            let prim = Label::primary(where_.file, where_.range())
                .with_message("invalid position for type constructor");
            Diagnostic::error()
                .with_message("type constructor used in an invalid position")
                .with_labels(vec![prim])
        }
    }
}

fn typeck_error_to_diag(err: thiol_typeck::Error) -> Diagnostic<FileId> {
    match err {
        thiol_typeck::Error::MutuallyRecursiveTypeDefinitions { type_def_idents } => {
            let labels = type_def_idents
                .into_iter()
                .enumerate()
                .map(|(i, loc)| {
                    let style = if i == 0 {
                        LabelStyle::Primary
                    } else {
                        LabelStyle::Secondary
                    };

                    let message = if i == 0 {
                        "incomplete type due to a cyclic definition"
                    } else {
                        "type is part of a recursive cycle"
                    };

                    Label::new(style, loc.file, loc.range()).with_message(message)
                })
                .collect();

            Diagnostic::error()
                .with_message("mutually recursive type definitions")
                .with_labels(labels)
        }
        thiol_typeck::Error::RecursiveTypeDefinition {
            type_def,
            type_name,
            recurive_usages,
        } => {
            let message = "recursive type definition";
            let mut labels = vec![
                Label::primary(type_name.file, type_name.range())
                    .with_message("type has infinite size"),
                Label::secondary(type_def.file, type_def.range()),
            ];
            labels.extend(recurive_usages.into_iter().map(|loc| {
                Label::secondary(loc.file, loc.range()).with_message("recursive use here")
            }));
            Diagnostic::error()
                .with_message(message)
                .with_labels(labels)
        }
        thiol_typeck::Error::TypeRedefinition {
            previous_name,
            redefinition_name,
            redefinition,
        } => {
            let message = "type redefinition";
            let labels = vec![
                Label::primary(redefinition_name.file, redefinition_name.range())
                    .with_message("redefinition of type"),
                Label::secondary(redefinition.file, redefinition.range()),
                Label::secondary(previous_name.file, previous_name.range())
                    .with_message("first definition of type with the same name"),
            ];

            Diagnostic::error()
                .with_message(message)
                .with_labels(labels)
        }
        thiol_typeck::Error::UndefinedType { name, uses } => {
            let message = format!("type `{}` not defined", name);

            let labels = uses
                .into_iter()
                .enumerate()
                .map(|(i, loc)| {
                    if i == 0 {
                        Label::primary(loc.file, loc.range()).with_message("undefined type")
                    } else {
                        Label::secondary(loc.file, loc.range())
                            .with_message("another usage of an undefined type")
                    }
                })
                .collect();

            Diagnostic::error()
                .with_message(message)
                .with_labels(labels)
        }
        thiol_typeck::Error::HigherKindedGenericTypeUsed {
            loc,
            generic_name: _,
        } => {
            let label = Label::primary(loc.file, loc.range());
            Diagnostic::error()
                .with_message("higher kinded generics are not supported")
                .with_labels(vec![label])
        }
        thiol_typeck::Error::MismatchedNumberGenericArgs {
            loc,
            given,
            expected,
            def_loc,
        } => {
            let labels = vec![
                Label::primary(loc.file, loc.range()),
                Label::secondary(def_loc.file, def_loc.range()),
            ];
            Diagnostic::error()
                .with_message(format!(
                    "mismatched number of generic arguments: expected {} but found {}",
                    expected, given
                ))
                .with_labels(labels)
        }
        thiol_typeck::Error::FieldRedefinition {
            previous_name,
            redefinition_name,
            item,
        } => {
            let labels = vec![
                Label::primary(redefinition_name.file, redefinition_name.range())
                    .with_message("redefinition of field"),
                Label::secondary(previous_name.file, previous_name.range())
                    .with_message("previous definition of field with the same name"),
                Label::secondary(item.file, item.range()),
            ];
            Diagnostic::error()
                .with_message("field redefinition")
                .with_labels(labels)
        }
        thiol_typeck::Error::GenericParamaterRedefinition {
            previous_name,
            redefinition,
        } => {
            let labels = vec![
                Label::primary(redefinition.file, redefinition.range())
                    .with_message("redefinition of generic parameter"),
                Label::secondary(previous_name.file, previous_name.range())
                    .with_message("previous definition of generic paramater with the same name"),
            ];
            Diagnostic::error()
                .with_message("generic parameter redefinition")
                .with_labels(labels)
        }
    }
}
