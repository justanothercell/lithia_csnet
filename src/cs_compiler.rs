use std::env;
use std::process::{Command, ExitStatus};

use crate::Result;

// https://github.com/philippgille/docs-1/blob/master/docs/csharp/language-reference/compiler-options/command-line-building-with-csc-exe.md
#[derive(Debug, Clone)]
pub(crate) struct CsCompilerArgs {
    pub(crate) framework_version: String,
    pub(crate) target: TargetType,
    pub(crate) optimized: bool,
    pub(crate) debug_info: bool,
    pub(crate) compiler_warn_level: CsCompilerWarn,
    pub(crate) symbols: Vec<String>,
    pub(crate) main: String,
    pub(crate) outfile: String,
    pub(crate) icon: Option<String>,
    pub(crate) sources: Vec<String>,
    pub(crate) project_root: String
}

#[derive(Debug, Clone)]
pub(crate) enum TargetType {
    AppContainerExe,
    Exe,
    Library,
    Module,
    WinExe,
    WinMdObj
}

impl TargetType {
    pub(crate) const fn arg(&self) -> &'static str{
        match self {
            TargetType::AppContainerExe => "appcontainerexe",
            TargetType::Exe => "exe",
            TargetType::Library => "library",
            TargetType::Module => "module",
            TargetType::WinExe => "winexe",
            TargetType::WinMdObj => "winmdobj"
        }
    }
}

//https://github.com/philippgille/docs-1/blob/master/docs/csharp/language-reference/compiler-options/warn-compiler-option.md
#[derive(Debug, Clone)]
pub(crate) enum CsCompilerWarn {
    None,
    Some,
    Many,
    Most,
    All,
}

impl CsCompilerWarn {
    pub(crate) fn level(&self) -> u8{
        match self {
            CsCompilerWarn::None => 0,
            CsCompilerWarn::Some => 1,
            CsCompilerWarn::Many => 2,
            CsCompilerWarn::Most => 3,
            CsCompilerWarn::All => 4
        }
    }
}

pub(crate) fn compile_cs(cs: CsCompilerArgs) -> Result<()>{
    let mut command = Command::new("cmd");
    command.arg("/C")
        .arg(format!("C:\\Windows\\Microsoft.NET\\Framework\\v{}\\csc.exe", cs.framework_version))
        .arg("/nologo")
        .arg("/unsafe")
        .arg(format!("/target:{}", cs.target.arg()))
        .arg(format!("/warn:{}", cs.compiler_warn_level.level()))
        .arg(format!("/out:{}", cs.outfile))
        .arg(format!("/main:{}", cs.main));
    if cs.symbols.len() > 0 {
        command.arg(format!("/define:{}", cs.symbols.join(";")));
    }
    if cs.debug_info {
        command.arg("/debug");
    }
    if cs.optimized {
        command.arg("/optimize");
    }
    if let Some(icon) = cs.icon {
        command.arg(format!("/win32icon:{}", icon));
    }
    command.args(cs.sources);
    let current_dir = env::current_dir()?;
    println!("Compiling generated C# code...");
    env::set_current_dir(cs.project_root)?;
    let status = command.spawn().expect("Could not spawn process").wait()?;
    env::set_current_dir(current_dir)?;
    println!("Compilation finished with {}", status);
    Ok(())
}

pub(crate) fn run_exe(exe: &str) -> Result<ExitStatus>{
    Ok(Command::new(exe).spawn().expect("Could not spawn process").wait()?)
}