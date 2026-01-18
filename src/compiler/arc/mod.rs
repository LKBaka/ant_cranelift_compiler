use ant_type_checker::ty::Ty;

use cranelift::prelude::{InstBuilder, Value};
use cranelift_module::Module;

use crate::{compiler::FunctionState, traits::NeedGc};

impl<'a> FunctionState<'a> {
    #[inline]
    pub fn emit_retain(&mut self, val: Value) {
        let fref = self
            .module
            .declare_func_in_func(self.arc_retain, &mut self.builder.func);
        
        self.builder.ins().call(fref, &[val]);
    }

    #[inline]
    pub fn emit_release(&mut self, val: Value) {
        let fref = self
            .module
            .declare_func_in_func(self.arc_release, &mut self.builder.func);

        self.builder.ins().call(fref, &[val]);
    }

    #[inline]
    pub fn emit_alloc(&mut self, size: Value) -> Value {
        let fref = self
            .module
            .declare_func_in_func(self.arc_alloc, &mut self.builder.func);

        let call = self.builder.ins().call(fref, &[size]);
        self.builder.inst_results(call)[0]
    }

    #[inline]
    pub fn update_ptr(&mut self, ptr: Value, obj: Value) {
        self.emit_retain(ptr);
        self.emit_release(obj);
    }

    pub fn retain_if_needed(&mut self, val: Value, ty: &Ty) {
        if ty.need_gc() {
            self.emit_retain(val);
        }
    }

    pub fn release_if_needed(&mut self, val: Value, ty: &Ty) {
        if ty.need_gc() {
            self.emit_release(val);
        }
    }
}
