use clap::Parser;

#[derive(Parser, Debug, Clone)]
#[command(
    name = "TypedAntCompiler",
    version = "0.1.0",
    about = "TypedAnt Compiler",
    long_about = None
)]

pub struct Args {
    /// 输入文件路径（可选）
    #[arg(short, long)]
    pub(crate) file: String,
}
