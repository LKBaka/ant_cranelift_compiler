use ant_type_checker::ty::Ty;
use cranelift::prelude::{InstBuilder, Value};
use cranelift_codegen::ir::{self};
use cranelift_frontend::FunctionBuilder;

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
            Ty::Struct { .. } => true,
            Ty::Generic(_, _) => true,
            Ty::IntTy(_) => false,
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
    fn to_const(&self) -> Self::ConstType;
}

pub trait ToLeBytes {
    fn to_le_bytes(&self) -> Vec<u8>;
}