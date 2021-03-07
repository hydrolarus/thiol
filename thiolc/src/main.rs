// SPDX-FileCopyrightText: 2021 The thiol developers
//
// SPDX-License-Identifier: EUPL-1.2

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
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

#[derive(Debug, Clap)]
#[clap(version = "0.1", author = "Tia")]
struct Arguments {
    #[clap(long)]
    parse_only: bool,

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

        let _module = match thiol_ast_lowering::lower(&mut hir_ctx, &ast) {
            Ok(module) => module,
            Err(errs) => {
                for err in errs {
                    let diag = ast_lowering_error_to_diag(err);
                    emit(!args.no_colour, &files, diag);
                }
                bail!("aborting due to previous error");
            }
        };
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
