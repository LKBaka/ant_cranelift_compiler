use ant_ty::Ty;
use cranelift::prelude::{InstBuilder, Value};
use cranelift_codegen::ir::{self};
use cranelift_frontend::FunctionBuilder;

use crate::compiler::CompileState;

pub trait NoRepeatPush<T> {
    fn push_no_repeat(&mut self, item: T);
}

impl<T: Eq> NoRepeatPush<T> for Vec<T> {
    fn push_no_repeat(&mut self, item: T) {
        if !self.contains(&item) {
            self.push(item);
        }
    }
}

pub trait NeedGc {
    fn need_gc(&self) -> bool;
}

impl NeedGc for Ty {
    fn need_gc(&self) -> bool {
        match self {
            Ty::Trait { .. } => true,
            Ty::BigInt => true,
            Ty::Function { .. } => false,
            Ty::AppliedGeneric(_, _) => true,
            Ty::Struct { .. } => true,
            Ty::Ptr(_) => false,
            Ty::Infer(_) => false,
            Ty::InferInt(_) => false,
            Ty::Generic(_, _) => false,
            Ty::IntTy(_) => false,
            Ty::FloatTy(_) => false,
            Ty::Bool => false,
            Ty::Unit => false,
            Ty::Str => false,
            Ty::Unknown => false,
        }
    }
}

pub trait BuilderExtends {
    /// Jump.
    ///
    /// Unconditionally jump to a basic block, passing the specified
    /// block arguments. The number and types of arguments must match the
    /// destination block.
    ///
    /// Inputs:
    ///
    /// - block_call_label: Destination basic block
    /// - block_call_args: Block arguments
    #[allow(non_snake_case)]
    fn jump_if_reachable(&mut self, block_call_label: ir::Block, block_call_args: &[Value])
    where
        Self: Sized;
}

impl<'a> BuilderExtends for FunctionBuilder<'a> {
    fn jump_if_reachable(&mut self, block_call_label: ir::Block, block_call_args: &[Value])
    where
        Self: Sized,
    {
        if !self.is_unreachable() {
            self.ins().jump(block_call_label, block_call_args);
        }
    }
}

pub trait LiteralExprToConst {
    type ConstType;
    fn to_const<'a, 'b>(&self, state: &impl CompileState<'a, 'b>) -> Self::ConstType;
}

pub trait ToLeBytes {
    fn to_le_bytes(&self) -> Vec<u8>;
}

pub trait ToIntValue {
    type IntValueType;
    fn to_int_value(&self) -> Self::IntValueType;
}