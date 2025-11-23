use clap::Parser;

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

    // 输出路径
    #[arg(short, long)]
    pub output: Option<String>,

    /// 优化级别 (0-3, s, z)
    #[arg(short = 'O', default_value = "0")]
    pub(crate) opt_level: OptLevelArg,
}

#[derive(Debug, Clone)]
pub struct OptLevelArg(String);

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