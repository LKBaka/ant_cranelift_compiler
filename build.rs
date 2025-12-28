use std::fs;
use std::path::Path;
use std::env;

fn main() {
    // 获取构建配置：debug 或 release
    let profile = env::var("PROFILE").unwrap(); // debug / release
    let target_dir = env::var("CARGO_TARGET_DIR").unwrap_or_else(|_| "target".into());

    let target_include_dir = Path::new(&target_dir)
        .join(&profile)
        .join("include");

    let project_include_dir = Path::new("include");

    // 删除已存在的目标目录（保证干净）
    if target_include_dir.exists() {
        fs::remove_dir_all(&target_include_dir).unwrap();
    }

    // 递归复制整个 include 文件夹
    copy_dir_all(project_include_dir, &target_include_dir).unwrap();

    // 监控 include 变化，重新执行 build.rs
    println!("cargo:rerun-if-changed=include");
}

// 递归复制目录和文件
fn copy_dir_all(src: &Path, dst: &Path) -> std::io::Result<()> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let path = entry.path();
        let dest_path = dst.join(entry.file_name());
        if path.is_dir() {
            copy_dir_all(&path, &dest_path)?;
        } else {
            fs::copy(&path, &dest_path)?;
        }
    }
    Ok(())
}
