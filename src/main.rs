#![feature(pattern)]
#![feature(try_blocks)]
#![feature(box_patterns)]
#![feature(slice_concat_trait)]
#![feature(inherent_associated_types)]

mod ast;
mod tokens;
mod cs_compiler;
mod source;
mod debugging;

use std::error::Error;
use std::process::exit;
use crate::ast::code_printer::CodePrinter;
use crate::ast::parsing::parse_tokens;
use crate::cs_compiler::{compile_cs, CsCompilerArgs, CsCompilerWarn, run_exe, TargetType};
use crate::source::{Source};
use crate::source::SourceType::String;
use crate::tokens::tokenize::{str_to_num_lit, tokenize};

pub(crate) type Result<T> = core::result::Result<T, Box<dyn Error>>;

fn main() {
    main_compile_cs();
}

fn main_compile(){
    let source = Source::from_file("testing/test_project/src/b_module.lin".to_string()).expect("Could not read source file");
    let tokens = match tokenize(source) {
        Ok(tokens) => tokens,
        Err(e) => panic!("{}", e)
    };
    println!("{:#?}", tokens);
    let main = match parse_tokens(tokens, "test_project") {
        Ok(module) => module,
        Err(e) => {
            eprintln!("{}", e);
            exit(1)
        }
    };
    println!("{}", main.print());
}

fn main_compile_cs()  {
    let cs = CsCompilerArgs {
        framework_version: "3.5".to_string(),
        target: TargetType::Exe,
        optimized: false,
        debug_info: true,
        compiler_warn_level: CsCompilerWarn::All,
        symbols: vec![],
        main: "Module".to_string(),
        outfile: "../test_project.exe".to_string(),
        icon: None,
        sources: vec!["main.cs".to_string(), "a_module.cs".to_string(), "b_module.cs".to_string(), "c_module.cs".to_string(), ],
        project_root: "testing/test_project/target_mock/cs".to_string()
    };
    compile_cs(cs).expect("Error compiling c# code");
    run_exe("testing/test_project/target_mock/test_project.exe").expect("Error running exe");
}