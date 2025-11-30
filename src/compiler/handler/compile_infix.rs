use std::rc::Rc;

use ant_ast::expr::IntValue;
use ant_type_checker::{
    ty::Ty,
    typed_ast::{GetType, typed_expr::TypedExpression},
};
use cranelift::prelude::{InstBuilder, IntCC, Value, types};

use crate::compiler::{Compiler, CompilerState};

pub fn compile_infix_iadd(state: &mut CompilerState<'_>, left: IntValue, right: IntValue) -> Value {
    match (left, right) {
        (IntValue::I64(l), IntValue::I64(r)) => state.builder.ins().iconst(types::I64, l + r),
        (IntValue::I32(l), IntValue::I32(r)) => {
            state.builder.ins().iconst(types::I32, (l + r) as i64)
        }
        (IntValue::I16(l), IntValue::I16(r)) => {
            state.builder.ins().iconst(types::I16, (l + r) as i64)
        }
        (IntValue::I8(l), IntValue::I8(r)) => state.builder.ins().iconst(types::I8, (l + r) as i64),
        (IntValue::U64(l), IntValue::U64(r)) => {
            state.builder.ins().iconst(types::I64, (l + r) as i64)
        }
        (IntValue::U32(l), IntValue::U32(r)) => {
            state.builder.ins().iconst(types::I32, (l + r) as i64)
        }
        (IntValue::U16(l), IntValue::U16(r)) => {
            state.builder.ins().iconst(types::I16, (l + r) as i64)
        }
        (IntValue::U8(l), IntValue::U8(r)) => state.builder.ins().iconst(types::I8, (l + r) as i64),
        _ => todo!(),
    }
}

pub fn compile_infix_isub(state: &mut CompilerState<'_>, left: IntValue, right: IntValue) -> Value {
    match (left, right) {
        (IntValue::I64(l), IntValue::I64(r)) => state.builder.ins().iconst(types::I64, l - r),
        (IntValue::I32(l), IntValue::I32(r)) => {
            state.builder.ins().iconst(types::I32, (l - r) as i64)
        }
        (IntValue::I16(l), IntValue::I16(r)) => {
            state.builder.ins().iconst(types::I16, (l - r) as i64)
        }
        (IntValue::I8(l), IntValue::I8(r)) => state.builder.ins().iconst(types::I8, (l - r) as i64),
        (IntValue::U64(l), IntValue::U64(r)) => {
            state.builder.ins().iconst(types::I64, (l - r) as i64)
        }
        (IntValue::U32(l), IntValue::U32(r)) => {
            state.builder.ins().iconst(types::I32, (l - r) as i64)
        }
        (IntValue::U16(l), IntValue::U16(r)) => {
            state.builder.ins().iconst(types::I16, (l - r) as i64)
        }
        (IntValue::U8(l), IntValue::U8(r)) => state.builder.ins().iconst(types::I8, (l - r) as i64),
        _ => todo!(),
    }
}

pub fn compile_infix_imul(state: &mut CompilerState<'_>, left: IntValue, right: IntValue) -> Value {
    match (left, right) {
        (IntValue::I64(l), IntValue::I64(r)) => state.builder.ins().iconst(types::I64, l * r),
        (IntValue::I32(l), IntValue::I32(r)) => {
            state.builder.ins().iconst(types::I32, (l * r) as i64)
        }
        (IntValue::I16(l), IntValue::I16(r)) => {
            state.builder.ins().iconst(types::I16, (l * r) as i64)
        }
        (IntValue::I8(l), IntValue::I8(r)) => state.builder.ins().iconst(types::I8, (l * r) as i64),
        (IntValue::U64(l), IntValue::U64(r)) => {
            state.builder.ins().iconst(types::I64, (l * r) as i64)
        }
        (IntValue::U32(l), IntValue::U32(r)) => {
            state.builder.ins().iconst(types::I32, (l * r) as i64)
        }
        (IntValue::U16(l), IntValue::U16(r)) => {
            state.builder.ins().iconst(types::I16, (l * r) as i64)
        }
        (IntValue::U8(l), IntValue::U8(r)) => state.builder.ins().iconst(types::I8, (l * r) as i64),
        _ => todo!(),
    }
}

pub fn compile_infix_ieq(state: &mut CompilerState<'_>, left: IntValue, right: IntValue) -> Value {
    match (left, right) {
        (IntValue::I64(l), IntValue::I64(r)) => {
            state.builder.ins().iconst(types::I8, (l == r) as i64)
        }
        (IntValue::I32(l), IntValue::I32(r)) => {
            state.builder.ins().iconst(types::I8, (l == r) as i64)
        }
        (IntValue::I16(l), IntValue::I16(r)) => {
            state.builder.ins().iconst(types::I8, (l == r) as i64)
        }
        (IntValue::I8(l), IntValue::I8(r)) => {
            state.builder.ins().iconst(types::I8, (l == r) as i64)
        }
        (IntValue::U64(l), IntValue::U64(r)) => {
            state.builder.ins().iconst(types::I8, (l == r) as i64)
        }
        (IntValue::U32(l), IntValue::U32(r)) => {
            state.builder.ins().iconst(types::I8, (l == r) as i64)
        }
        (IntValue::U16(l), IntValue::U16(r)) => {
            state.builder.ins().iconst(types::I8, (l == r) as i64)
        }
        (IntValue::U8(l), IntValue::U8(r)) => {
            state.builder.ins().iconst(types::I8, (l == r) as i64)
        }
        _ => todo!(),
    }
}

pub fn compile_infix(
    state: &mut CompilerState<'_>,
    op: Rc<str>,
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
                type OpFunc = fn(&mut CompilerState<'_>, Value, Value) -> Value;

                let op_func: OpFunc = match op {
                    "+" => (|state, x, y| state.builder.ins().iadd(x, y)) as OpFunc,
                    "-" => (|state, x, y| state.builder.ins().isub(x, y)) as OpFunc,
                    "*" => (|state, x, y| state.builder.ins().imul(x, y)) as OpFunc,
                    "/" => (|state, x, y| state.builder.ins().fdiv(x, y)) as OpFunc,
                    ">" => (|state, x, y| state.builder.ins().icmp(IntCC::SignedGreaterThan, x, y)) as OpFunc,
                    "<" => (|state, x, y| state.builder.ins().icmp(IntCC::SignedLessThan, x, y)) as OpFunc,
                    "==" => (|state, x, y| state.builder.ins().icmp(IntCC::Equal, x, y)) as OpFunc,
                    "!=" => {
                        (|state, x, y| state.builder.ins().icmp(IntCC::NotEqual, x, y)) as OpFunc
                    }
                    _ => todo!("todo op {op}"),
                };

                Ok(op_func(state, lval, rval))
            }

            (Ty::Bool, Ty::Bool) => {
                type OpFunc = fn(&mut CompilerState<'_>, Value, Value) -> Value;

                let op_func: OpFunc = match op {
                    "==" => (|state, x, y| state.builder.ins().icmp(IntCC::Equal, x, y)) as OpFunc,
                    "!=" => {
                        (|state, x, y| state.builder.ins().icmp(IntCC::NotEqual, x, y)) as OpFunc
                    }
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
        (_, _, op) => non_const_handler(left, right, op),
    }
}
