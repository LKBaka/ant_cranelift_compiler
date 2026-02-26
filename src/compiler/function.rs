use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use ant_type_checker::ty::TyId;
use ant_type_checker::{ty::Ty, typed_ast::typed_expr::TypedExpression};
use cranelift::prelude::{AbiParam, InstBuilder, Signature, Value};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::Module;

use crate::compiler::Compiler;
use crate::compiler::imm::platform_width_to_int_type;
use crate::compiler::table::SymbolTable;
use crate::compiler::{
    CompileResult, FunctionState, constants::CALL_CONV,
    convert_type::convert_type_to_cranelift_type,
};
use crate::traits::NeedGc;

pub fn mangle_func(name: &str, args: &[&Ty]) -> String {
    if args.is_empty() {
        return name.to_string();
    }

    let mut res = name.to_string();
    res.push_str("__");

    for arg in args {
        res.push_str(&format!("_{}", arg));
    }

    res
}

pub fn make_signature(param_types: &[&Ty], ret_ty: &Ty) -> Signature {
    let mut signature = Signature::new(CALL_CONV);

    signature.params = param_types
        .iter()
        .map(|ty| AbiParam::new(convert_type_to_cranelift_type(ty)))
        .collect::<Vec<AbiParam>>();

    if ret_ty != &Ty::Unit {
        signature
            .returns
            .push(AbiParam::new(convert_type_to_cranelift_type(ret_ty)));
    };

    signature
}

pub fn compile_function(
    state: &mut FunctionState,

    signature: Signature,
    params: &Vec<(Arc<str>, TyId)>,
    ret_ty: TyId,

    block_ast: &Box<TypedExpression>,
    func_name: &str,
) -> CompileResult<Value> {
    let func_id = state.function_map.get(func_name).cloned().map_or_else(
        || Err(format!("function {func_name} not in function map")),
        |it| Ok(it),
    )?;

    let mut ctx = state.module.make_context();
    ctx.func.signature = signature.clone();

    let func_ref = state
        .module
        .declare_func_in_func(func_id, &mut state.builder.func);

    let ref_val = state
        .builder
        .ins()
        .func_addr(platform_width_to_int_type(), func_ref);

    // 5. 定义外部作用域的符号
    let func_symbol = state.table.borrow_mut().define_func(&func_name);
    state.builder.declare_var(
        Variable::from_u32(func_symbol.var_index as u32),
        platform_width_to_int_type(),
    );
    state.builder.def_var(
        Variable::from_u32(func_symbol.var_index as u32),
        ref_val.clone(),
    );

    // 6. 创建新的编译上下文
    let mut func_builder_ctx = FunctionBuilderContext::new();
    let mut func_builder = FunctionBuilder::new(&mut ctx.func, &mut func_builder_ctx);

    let entry_block = func_builder.create_block();
    func_builder.append_block_params_for_function_params(entry_block);
    func_builder.switch_to_block(entry_block);
    func_builder.seal_block(entry_block);

    // 7. 创建函数内部的符号表
    let func_symbol_table = Rc::new(RefCell::new(SymbolTable::from_outer(state.table.clone())));

    // 8. 在函数内部符号表中也定义这个函数
    let inner_func_symbol = func_symbol_table.borrow_mut().define_func(&func_name);
    func_builder.declare_var(
        Variable::from_u32(inner_func_symbol.var_index as u32),
        platform_width_to_int_type(),
    );

    // 9. 在函数内部重新创建FuncRef和对应的Value
    let inner_func_ref = state
        .module
        .declare_func_in_func(func_id, &mut func_builder.func);
    let inner_ref_val = func_builder
        .ins()
        .func_addr(platform_width_to_int_type(), inner_func_ref);
    func_builder.def_var(
        Variable::from_u32(inner_func_symbol.var_index as u32),
        inner_ref_val, // 使用内部创建的Value
    );

    // 10. 声明参数变量
    for (i, (param_name, tyid)) in params.iter().enumerate() {
        let symbol = func_symbol_table.borrow_mut().define(&param_name);
        let cranelift_ty = convert_type_to_cranelift_type(state.tcx.get(*tyid));

        func_builder.declare_var(Variable::from_u32(symbol.var_index as u32), cranelift_ty);

        let param_value = func_builder.block_params(entry_block)[i];
        func_builder.def_var(Variable::from_u32(symbol.var_index as u32), param_value);
    }

    let ret_ty = state.tcx.get(ret_ty).clone();

    // 11. 编译函数体
    let mut func_state = FunctionState {
        builder: func_builder,
        module: state.module,
        table: func_symbol_table,
        tcx: state.tcx,
        function_map: state.function_map,
        data_map: state.data_map,
        generic_map: state.generic_map,
        compiled_generic_map: state.compiled_generic_map,
        target_isa: state.target_isa.clone(),

        arc_alloc: state.arc_alloc,
        arc_release: state.arc_release,
        arc_retain: state.arc_retain,

        terminated: false,
    };

    let result = Compiler::compile_expr(&mut func_state, block_ast)?;

    if !func_state.terminated {
        let symbols = state.table.borrow().map.clone();

        for (_, sym) in symbols {
            if sym.is_val && sym.symbol_ty.need_gc() {
                let var = Variable::from_u32(sym.var_index as u32);
                let val = func_state.builder.use_var(var);
                func_state.emit_release(val);
            }
        }

        if ret_ty != Ty::Unit {
            func_state.builder.ins().return_(&[result]);
        } else {
            func_state.builder.ins().return_(&[]);
        }
    }

    func_state.builder.finalize();

    match cranelift_codegen::verify_function(&ctx.func, &*state.target_isa) {
        Ok(_) => {}
        Err(errors) => {
            let mut msg = String::new();
            for e in errors.0.iter() {
                use std::fmt::Write;
                writeln!(msg, "verifier: {}", e).unwrap();
            }
            return Err(format!("verifier errors:\n{}", msg));
        }
    }

    state
        .module
        .define_function(func_id, &mut ctx)
        .map_or_else(|err| Err(err.to_string()), |_| Ok(()))?;
    state.module.clear_context(&mut ctx);

    return Ok(ref_val);
}
