mod args;
mod compiler;
mod monomorphizer;
mod traits;
mod value;

use std::{
    cell::RefCell,
    fs,
    path::PathBuf,
    rc::Rc, sync::{Arc, Mutex},
};

use crate::{compiler::{
    Compiler, compile_to_executable, create_target_isa, table::SymbolTable,
}, monomorphizer::Monomorphizer};

use ant_lexer::Lexer;
use ant_parser::{Parser, error::display_err};

use ant_type_checker::{
    TypeChecker,
    table::TypeTable,
};

use clap::Parser as ClapParser;

use crate::args::{Args, ARG};

fn compile(arg: Args) {
    unsafe { ARG = Some(arg.clone()) };
    
    let file_arc: Arc<str> = arg.file.clone().into();
    let file = PathBuf::from(arg.file);

    if !file.exists() {
        panic!("file is not exists: {}", file.to_string_lossy())
    }

    let file_content = fs::read_to_string(&file).expect("read file error");

    let mut lexer = Lexer::new(file_content, file_arc.clone());

    let tokens = lexer.get_tokens();

    if lexer.contains_error() {
        lexer.print_errors();
        println!();
        panic!("lexer error")
    }

    let mut parser = Parser::new(tokens);

    let program = match parser.parse_program() {
        Ok(it) => it,
        Err(err) => {
            display_err(&err);
            println!();
            panic!("parser error")
        }
    };

    let type_table = Arc::new(Mutex::new(TypeTable::new().init()));

    let mut checker = TypeChecker::new(type_table.clone());

    let mut typed_program = match checker.check_node(program) {
        Ok(it) => it,
        Err(err) => {
            println!("{err:#?}");
            println!();
            panic!("type checker error")
        }
    };

    let mut monomorphizer = Monomorphizer::new();
    match monomorphizer.monomorphize(&mut typed_program) {
        Ok(_) => (),
        Err(it) => {
            println!("{it}");
            println!();
            panic!("monomorphizer error")
        }
    }

    let compiler = Compiler::new(
        create_target_isa(),
        file_arc.clone(),
        Rc::new(RefCell::new(SymbolTable::new())),
        type_table.clone(),
    );

    let code = match compiler.compile_program(typed_program) {
        Ok(code) => code,
        Err(err) => {
            println!("{err}");
            println!();
            panic!("compiler error")
        }
    };

    #[cfg(windows)]
    let output_file_stem = ".exe";

    #[cfg(not(windows))]
    let output_file_stem = "";

    let output_path = if let Some(it) = arg.output {
        PathBuf::from(it)
    } else {
        file.parent().unwrap().join(PathBuf::from(
            file.file_stem().unwrap().to_string_lossy().to_string() + output_file_stem,
        ))
    };

    if !output_path.exists() {
        fs::create_dir_all(&output_path.parent().unwrap()).unwrap()
    }

    match compile_to_executable(&code, &output_path) {
        Ok(_) => (),
        Err(it) => println!("{it}"),
    }
}

fn main() {
    let args = Args::parse();

    compile(args);
}
