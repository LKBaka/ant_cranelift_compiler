# ant_cranelift_compiler

## 项目简介

`ant_cranelift_compiler` 是一个基于 Rust 的编译器项目，旨在使用 Cranelift 后端为 TypedAnt 代码生成高效的机器代码。该项目的主旨是提供一个 TypedAnt 的一个可用，好用的可执行文件编译器

## 项目结构

```
├── build.rs                       # 构建脚本
├── Cargo.toml                     # 项目配置文件
├── test_program.ant               # 示例测试程序
├── include/                       # 包含的 C 文件
│   └── arc.c
├── src/                           # 源代码目录
│   ├── args.rs                    # 命令行参数解析模块
│   ├── lib.rs                     # 库入口文件
│   ├── main.rs                    # 主程序入口
│   ├── traits.rs                  # 通用 trait 定义
│   ├── compiler/                  # 编译器核心模块
│   │   ├── compile_state_impl.rs  # 编译状态实现 
│   │   ├── compiler_impl.rs       # 编译器主体实现
│   │   ├── constants.rs           # 各种常量
│   │   ├── convert_type.rs        # 负责将各种类型转换到 cranelift 专有类型
│   │   ├── imm.rs                 # 与立即数相关
│   │   ├── mod.rs                 # CompileState 和 Compiler 定义
│   │   ├── table.rs               # 符号表
│   │   ├── arc/                   # 与 ARC 相关的模块
│   │   │   └── mod.rs
│   │   ├── handler/               # 表达式处理模块
│   │   │   ├── compile_infix.rs
│   │   │   └── mod.rs
│   ├── monomorphizer/             # 单态化相关模块
│       ├── mod.rs
│       └── test.rs
└── target/                        # 编译输出目录
```

## 功能特性

- **Cranelift 集成**：利用 Cranelift 提供高效的机器代码生成。
- **模块化设计**：代码结构清晰 (并非清晰)，便于扩展和维护。(并非便于扩展和维护)

## 快速开始

1. 克隆项目：

   ```bash
   git clone https://github.com/LKBaka/ant_cranelift_compiler
   cd ant_cranelift_compiler
   ```

2. 构建项目：

   ```bash
   cargo build
   ```

3. 运行测试程序：

   ```bash
   cargo run -- -f test_program.ant
   ```

## 如果你是 linux 用户

在快速开始第三步之前 你应当重新编译一遍 libarc.a
(在项目根目录下)

```bash
gcc -c ./include/arc.c
mv ./arc.o ./include/arc.o
ar rcs ./include/libarc.a ./include/arc.o
```

## 贡献指南

欢迎对本项目提出建议或贡献代码！请遵循以下步骤：
> [!NOTE]
> 若不会进行 git 操作可使用 GithubDesktop + Github 网页代替

1. Fork 本仓库。
2. 创建一个新的分支：

   ```bash
   git checkout -b feature/your-feature-name
   ```

3. 提交更改并推送到你的分支：

   ```bash
   git commit -m "描述你的更改"
   git push origin feature/your-feature-name
   ```

4. 创建一个 Pull Request。