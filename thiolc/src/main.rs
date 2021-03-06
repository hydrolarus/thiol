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

fn run() -> anyhow::Result<()> {
    let args: Arguments = Arguments::parse();

    let mut files = codespan_reporting::files::SimpleFiles::new();

    for path in &args.file_paths {
        let content = std::fs::read_to_string(path)?;
        let id = files.add(path.display().to_string(), content);
        let src = files.source(id).unwrap();

        match parser::parse_file(id, src) {
            Ok(_val) => {}
            Err(err) => {
                let label = Label::primary(id, err.location.start..err.location.end)
                    .with_message("Unexpected token");
                let mut notes = vec!["Expected one of the following token types:".to_string()];
                notes.extend(err.expected_tokens.into_iter().map(|s| format!(" - {}", s)));
                let note = notes.join("\n");

                let diag = Diagnostic::error()
                    .with_message("Parse error")
                    .with_labels(vec![label])
                    .with_notes(vec![note]);
                emit(!args.no_colour, &files, diag);

                bail!("Aborting due to previous error")
            }
        }
    }

    if args.parse_only {
        return Ok(());
    }

    Ok(())
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
