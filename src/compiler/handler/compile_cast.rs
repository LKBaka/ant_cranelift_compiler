use ant_type_checker::ty::{Ty, TyId};
use cranelift::prelude::{InstBuilder, IntCC, Value};

use crate::compiler::{
    CompileResult, Compiler, FunctionState, convert_type::convert_type_to_cranelift_type,
    get_platform_width,
};

pub fn compile_cast(
    state: &mut FunctionState,
    val: Value,
    src_id: TyId,
    dest_id: TyId,
) -> CompileResult<Value> {
    let tcx = state.tcx_ref();
    let src_ty = tcx.get(src_id);
    let dest_ty = tcx.get(dest_id);

    let dest_cranelift_ty = convert_type_to_cranelift_type(dest_ty);

    // 获取平台指针宽度
    let ptr_width = get_platform_width() as u32;

    let src_size = Compiler::get_type_size(state, src_ty, ptr_width)?;
    let dest_size = Compiler::get_type_size(state, dest_ty, ptr_width)?;

    match (src_ty, dest_ty) {
        // 整数与整数、指针与整数、指针与指针之间的转换
        (Ty::IntTy(_), Ty::IntTy(_))
        | (Ty::Ptr(_), Ty::IntTy(_))
        | (Ty::IntTy(_), Ty::Ptr(_))
        | (Ty::Ptr(_), Ty::Ptr(_)) => {
            if src_size > dest_size {
                // 大转小：截断高位 (例如 i64 -> i32)
                return Ok(state.builder.ins().ireduce(dest_cranelift_ty, val))
            } else if src_size < dest_size {
                // 小转大：需要区分符号位扩展还是零扩展
                if let Ty::IntTy(s_int) = src_ty {
                    if s_int.is_signed() {
                        // 有符号扩展 (例如 i32 -> i64)
                        return Ok(state.builder.ins().sextend(dest_cranelift_ty, val));
                    }

                    // 无符号扩展 (例如 u32 -> u64)
                    return Ok(state.builder.ins().uextend(dest_cranelift_ty, val));
                }

                // 指针在 Cranelift 里通常视为无符号，统一用 uextend
                return Ok(state.builder.ins().uextend(dest_cranelift_ty, val));
            }

            // 位宽相等：即便类型不同 (如 i64 as *u8)，在 cranelift 里都是 I64
            // 只要目标类型一致，直接返回或 bitcast
            Ok(val)
        }

        // 布尔值转换
        (Ty::Bool, Ty::IntTy(_)) => {
            if dest_size > 1 {
                Ok(state.builder.ins().uextend(dest_cranelift_ty, val))
            } else {
                Ok(val)
            }
        }

        // 整数转布尔 (非 0 即为 true)
        (Ty::IntTy(_), Ty::Bool) => {
            let src_ty = state.builder.func.dfg.value_type(val);

            let zero = state.builder.ins().iconst(src_ty, 0);
            let is_not_zero = state.builder.ins().icmp(IntCC::NotEqual, val, zero);

            Ok(state.builder.ins().uextend(dest_cranelift_ty, is_not_zero))
        }

        (lty, rty) => Err(format!("impl cast {} as {}", lty, rty)),
    }
}
