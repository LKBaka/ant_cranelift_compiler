use std::sync::Arc;

use ant_ast::expr::IntValue;
use ant_type_checker::{
    ty::Ty,
    typed_ast::{GetType, typed_expr::TypedExpression},
};
use cranelift::prelude::{InstBuilder, IntCC, Value, types};

use crate::compiler::{Compiler, FunctionState};

macro_rules! four_fundamental_operations {
    ($op:ident) => {
        (|state, x, y| state.builder.ins().$op(x, y)) as OpFunc
    };
}

macro_rules! cmp {
    ($op:expr) => {
        (|state, x, y| state.builder.ins().icmp($op, x, y)) as OpFunc
    };
}

macro_rules! const_eval_op {
    ($state:expr, $ty:expr, $op:expr) => {
        $state.builder.ins().iconst($ty, $op)
    };
}

pub fn compile_infix_iadd(state: &mut FunctionState<'_>, left: IntValue, right: IntValue) -> Value {
    match (left, right) {
        (IntValue::I64(l), IntValue::I64(r)) => const_eval_op!(state, types::I64, l + r),
        (IntValue::I32(l), IntValue::I32(r)) => const_eval_op!(state, types::I32, (l + r) as i64),
        (IntValue::I16(l), IntValue::I16(r)) => const_eval_op!(state, types::I16, (l + r) as i64),
        (IntValue::I8(l), IntValue::I8(r)) => const_eval_op!(state, types::I8, (l + r) as i64),
        (IntValue::U64(l), IntValue::U64(r)) => const_eval_op!(state, types::I64, (l + r) as i64),
        (IntValue::U32(l), IntValue::U32(r)) => const_eval_op!(state, types::I32, (l + r) as i64),
        (IntValue::U16(l), IntValue::U16(r)) => const_eval_op!(state, types::I16, (l + r) as i64),
        (IntValue::U8(l), IntValue::U8(r)) => const_eval_op!(state, types::I8, (l + r) as i64),
        _ => todo!(),
    }
}

pub fn compile_infix_isub(state: &mut FunctionState<'_>, left: IntValue, right: IntValue) -> Value {
    match (left, right) {
        (IntValue::I64(l), IntValue::I64(r)) => const_eval_op!(state, types::I64, l - r),
        (IntValue::I32(l), IntValue::I32(r)) => const_eval_op!(state, types::I32, (l - r) as i64),
        (IntValue::I16(l), IntValue::I16(r)) => const_eval_op!(state, types::I16, (l - r) as i64),
        (IntValue::I8(l), IntValue::I8(r)) => const_eval_op!(state, types::I8, (l - r) as i64),
        (IntValue::U64(l), IntValue::U64(r)) => const_eval_op!(state, types::I64, (l - r) as i64),
        (IntValue::U32(l), IntValue::U32(r)) => const_eval_op!(state, types::I32, (l - r) as i64),
        (IntValue::U16(l), IntValue::U16(r)) => const_eval_op!(state, types::I16, (l - r) as i64),
        (IntValue::U8(l), IntValue::U8(r)) => const_eval_op!(state, types::I8, (l - r) as i64),
        _ => todo!(),
    }
}

pub fn compile_infix_imul(state: &mut FunctionState<'_>, left: IntValue, right: IntValue) -> Value {
    match (left, right) {
        (IntValue::I64(l), IntValue::I64(r)) => const_eval_op!(state, types::I64, l * r),
        (IntValue::I32(l), IntValue::I32(r)) => const_eval_op!(state, types::I32, (l * r) as i64),
        (IntValue::I16(l), IntValue::I16(r)) => const_eval_op!(state, types::I16, (l * r) as i64),
        (IntValue::I8(l), IntValue::I8(r)) => const_eval_op!(state, types::I8, (l * r) as i64),
        (IntValue::U64(l), IntValue::U64(r)) => const_eval_op!(state, types::I64, (l * r) as i64),
        (IntValue::U32(l), IntValue::U32(r)) => const_eval_op!(state, types::I32, (l * r) as i64),
        (IntValue::U16(l), IntValue::U16(r)) => const_eval_op!(state, types::I16, (l * r) as i64),
        (IntValue::U8(l), IntValue::U8(r)) => const_eval_op!(state, types::I8, (l * r) as i64),
        _ => todo!(),
    }
}

pub fn compile_infix_ieq(state: &mut FunctionState<'_>, left: IntValue, right: IntValue) -> Value {
    match (left, right) {
        (IntValue::I64(l), IntValue::I64(r)) => const_eval_op!(state, types::I8, (l == r) as i64),
        (IntValue::I32(l), IntValue::I32(r)) => const_eval_op!(state, types::I8, (l == r) as i64),
        (IntValue::I16(l), IntValue::I16(r)) => const_eval_op!(state, types::I8, (l == r) as i64),
        (IntValue::I8(l), IntValue::I8(r)) => const_eval_op!(state, types::I8, (l == r) as i64),
        (IntValue::U64(l), IntValue::U64(r)) => const_eval_op!(state, types::I8, (l == r) as i64),
        (IntValue::U32(l), IntValue::U32(r)) => const_eval_op!(state, types::I8, (l == r) as i64),
        (IntValue::U16(l), IntValue::U16(r)) => const_eval_op!(state, types::I8, (l == r) as i64),
        (IntValue::U8(l), IntValue::U8(r)) => const_eval_op!(state, types::I8, (l == r) as i64),
        _ => todo!(),
    }
}

pub fn compile_infix_ineq(state: &mut FunctionState<'_>, left: IntValue, right: IntValue) -> Value {
    match (left, right) {
        (IntValue::I64(l), IntValue::I64(r)) => const_eval_op!(state, types::I8, (l != r) as i64),
        (IntValue::I32(l), IntValue::I32(r)) => const_eval_op!(state, types::I8, (l != r) as i64),
        (IntValue::I16(l), IntValue::I16(r)) => const_eval_op!(state, types::I8, (l != r) as i64),
        (IntValue::I8(l), IntValue::I8(r)) => const_eval_op!(state, types::I8, (l != r) as i64),
        (IntValue::U64(l), IntValue::U64(r)) => const_eval_op!(state, types::I8, (l != r) as i64),
        (IntValue::U32(l), IntValue::U32(r)) => const_eval_op!(state, types::I8, (l != r) as i64),
        (IntValue::U16(l), IntValue::U16(r)) => const_eval_op!(state, types::I8, (l != r) as i64),
        (IntValue::U8(l), IntValue::U8(r)) => const_eval_op!(state, types::I8, (l != r) as i64),
        _ => todo!(),
    }
}

pub fn compile_infix(
    state: &mut FunctionState<'_>,
    op: Arc<str>,
    left: &Box<TypedExpression>,
    right: &Box<TypedExpression>,
) -> Result<Value, String> {
    #[rustfmt::skip]
    let mut non_const_handler = |
        left: &Box<TypedExpression>,
        right: &Box<TypedExpression>,
        op: &str
    | {
        let lval = Compiler::compile_expr(state, &left)?;
        let rval = Compiler::compile_expr(state, &right)?;

        match (left.get_type(), right.get_type()) {
            (Ty::IntTy(_), Ty::IntTy(_)) => {
                type OpFunc = fn(&mut FunctionState<'_>, Value, Value) -> Value;

                let op_func: OpFunc = match op {
                    "+" => four_fundamental_operations!(iadd),
                    "-" => four_fundamental_operations!(isub),
                    "*" => four_fundamental_operations!(imul),
                    "/" => four_fundamental_operations!(fdiv),
                    ">" => cmp!(IntCC::SignedGreaterThan),
                    "<" => cmp!(IntCC::SignedLessThan),
                    "==" => cmp!(IntCC::Equal),
                    "!=" => cmp!(IntCC::NotEqual),
                    _ => todo!("todo op {op}"),
                };

                Ok(op_func(state, lval, rval))
            }

            (Ty::Bool, Ty::Bool) => {
                type OpFunc = fn(&mut FunctionState<'_>, Value, Value) -> Value;

                let op_func: OpFunc = match op {
                    "==" => cmp!(IntCC::Equal),
                    "!=" => cmp!(IntCC::NotEqual),
                    _ => todo!("todo op {op}"),
                };

                Ok(op_func(state, lval, rval))
            }

            (lty, rty) => todo!("impl {left} {op} {right}. left_ty: {lty}, right_ty: {rty}"),
        }
    };

    match (&**left, &**right, op.as_ref()) {
        (
            TypedExpression::Int { value: lval, .. },
            TypedExpression::Int { value: rval, .. },
            "+",
        ) => Ok(compile_infix_iadd(state, *lval, *rval)),
        (
            TypedExpression::Int { value: lval, .. },
            TypedExpression::Int { value: rval, .. },
            "-",
        ) => Ok(compile_infix_isub(state, *lval, *rval)),
        (
            TypedExpression::Int { value: lval, .. },
            TypedExpression::Int { value: rval, .. },
            "*",
        ) => Ok(compile_infix_imul(state, *lval, *rval)),
        (
            TypedExpression::Int { value: lval, .. },
            TypedExpression::Int { value: rval, .. },
            "==",
        ) => Ok(compile_infix_ieq(state, *lval, *rval)),
        (
            TypedExpression::Int { value: lval, .. },
            TypedExpression::Int { value: rval, .. },
            "!=",
        ) => Ok(compile_infix_ineq(state, *lval, *rval)),
        (_, _, op) => non_const_handler(left, right, op),
    }
}
