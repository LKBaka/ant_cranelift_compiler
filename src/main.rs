use clap::Parser;

use crate::args::Args;

mod args;
mod compiler;

fn main() {
    let args = Args::parse();

    println!("args: {args:#?}")
}