use ant_type_checker::ty::{IntTy, Ty};
use cranelift::prelude::types;

use crate::compiler::imm::platform_width_to_int_type;

pub fn convert_type_to_cranelift_type(ty: &Ty) -> types::Type {
    match ty {
        Ty::IntTy(ty) => match ty {
            IntTy::USize => platform_width_to_int_type(),
            IntTy::ISize => platform_width_to_int_type(),
            IntTy::I64 => types::I64,
            IntTy::I32 => types::I32,
            IntTy::I16 => types::I16,
            IntTy::I8 => types::I8,
            IntTy::U64 => types::I64,
            IntTy::U32 => types::I32,
            IntTy::U16 => types::I16,
            IntTy::U8 => types::I8,
        }
        Ty::Bool => types::I8, // cranelift 没有比 i8 更小的类型了
        Ty::Struct { .. } => platform_width_to_int_type(),
        Ty::Str => platform_width_to_int_type(),
        Ty::Function { .. } => platform_width_to_int_type(), // 以函数指针处理
        Ty::Unit => types::I8,
        _ => todo!("impl ty {ty}"),
    }
}