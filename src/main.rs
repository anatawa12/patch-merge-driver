use clap::{Parser, Subcommand};

mod install;
mod merge;
mod patch;
mod path;
mod util;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
#[clap(propagate_version = true)]
struct Command {
    #[clap(subcommand)]
    command: Commands,
    #[clap(flatten)]
    global: GlobalOptions,
}

#[derive(Parser)]
struct GlobalOptions {}

#[derive(Subcommand)]
enum Commands {
    Install(install::Options),
    Merge(merge::Options),
}

fn main() {
    let parsed: Command = Command::parse();
    match parsed.command {
        Commands::Install(ref opt) => install::main(opt),
        Commands::Merge(ref opt) => merge::main(opt),
    }
}
