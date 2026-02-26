use std::fmt::Display;

use clap::{Parser, builder::Str};

#[derive(Parser, Debug, Clone)]
#[command(
    name = "TypedAntCompiler",
    version = "0.1.0",
    about = "TypedAnt Compiler",
    long_about = None
)]

pub struct Args {
    /// 输入文件路径
    #[arg(short, long)]
    pub file: String,

    /// 输出路径
    #[arg(short, long)]
    pub output: Option<String>,

    /// 优化级别 (0-3, s, z)
    #[arg(short = 'O', default_value = "0")]
    pub opt_level: OptLevelArg, 

    /// 包含调试信息
    #[arg(short = 'g', long = "debuginfo")]
    pub debug_info: bool,

    /// 欲链接的静态库文件
    #[arg(short = 'l', long = "link")]
    pub link_with: Vec<String>,

    /// 是否保留临时.o
    #[arg(long = "keep-cache")]
    pub keep_cache: bool,

    /// 目标工具链
    #[arg(short = 'T', long = "target-triple", default_value = "")]
    pub target_triple: String,

    /// 是否仅编译，不链接
    #[arg(short = 'c', long = "compile-only")]
    pub compile_only: bool,

    /// 脚本模式开关
    #[arg(long)]
    pub script_mode: bool,
}

#[derive(Debug, Clone)]
pub struct OptLevelArg(pub String);

impl std::str::FromStr for OptLevelArg {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" | "1" | "2" | "3" | "s" | "z" => Ok(OptLevelArg(s.to_string())),
            _ => Err(format!("无效的优化级别: {}. 可选值: 0, 1, 2, 3, s, z", s)),
        }
    }
}

impl OptLevelArg {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn is_optimized(&self) -> bool {
        self.0 != "0"
    }
}

impl Display for OptLevelArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "O{}", self.0)
    }
}

pub static mut ARG: Option<Args> = {
    None
};

pub fn read_arg() -> Option<Args> {
    unsafe { (*&raw const ARG).clone() }
}
