use ant_ast::expr::IntValue;
use cranelift::prelude::{Imm64, Uimm64, types};

use crate::compiler::get_platform_width;

pub trait IntoImm {
    type ImmType;
    fn into_imm(self) -> Self::ImmType;
}

impl IntoImm for i64 {
    type ImmType = Imm64;
    fn into_imm(self) -> Imm64 {
        Imm64::new(self)
    }
}

impl IntoImm for u64 {
    type ImmType = Uimm64;
    fn into_imm(self) -> Uimm64 {
        Uimm64::new(self)
    }
}

pub fn platform_width_to_int_type() -> cranelift::prelude::Type {
    match get_platform_width() {
        64 => types::I64,
        32 => types::I32,
        16 => types::I16,
        _ => unreachable!(),
    }
}

pub fn int_value_to_int_type(value: &IntValue) -> cranelift::prelude::Type {
    match value {
        IntValue::I64(_it) => types::I64,
        IntValue::I32(_it) => types::I32,
        IntValue::I16(_it) => types::I16,
        IntValue::I8(_it) => types::I8,
        IntValue::U64(_it) => types::I64,
        IntValue::U32(_it) => types::I32,
        IntValue::U16(_it) => types::I16,
        IntValue::U8(_it) => types::I8,
        IntValue::ISize(_it) => platform_width_to_int_type(),
        IntValue::USize(_it) => platform_width_to_int_type(),
    }
}

pub fn int_value_to_imm(value: &IntValue) -> Imm64 {
    match value {
        IntValue::I64(it) => (*it).into_imm(),
        IntValue::I32(it) => (*it as i64).into_imm(),
        IntValue::I16(it) => (*it as i64).into_imm(),
        IntValue::I8(it) => (*it as i64).into_imm(),
        IntValue::U64(it) => (*it as i64).into_imm(),
        IntValue::U32(it) => (*it as i64).into_imm(),
        IntValue::U16(it) => (*it as i64).into_imm(),
        IntValue::U8(it) => (*it as i64).into_imm(),
        IntValue::ISize(it) => (*it as i64).into_imm(),
        IntValue::USize(it) => (*it as i64).into_imm(),
    }
}
