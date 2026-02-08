use ant_token::token;
use ant_token::token_type::TokenType;
use ant_type_checker::ty::{Ty, TyId};
use ant_type_checker::ty_context::TypeContext;
use ant_type_checker::typed_ast::GetType;
use ant_type_checker::typed_ast::typed_expr::TypedExpression;
use ant_type_checker::typed_ast::typed_node::TypedNode;
use ant_type_checker::typed_ast::typed_stmt::TypedStatement;
use std::collections::HashMap;

use crate::traits::NoRepeatPush;

/// 泛型函数信息
#[derive(Debug, Clone)]
struct GenericFunctionInfo {
    expr: Box<TypedExpression>,
    param_names: Vec<String>,
}

/// 单态化器主结构
pub struct Monomorphizer<'a> {
    generic_functions: HashMap<String, GenericFunctionInfo>,
    instances: Vec<(String, Vec<TyId>)>,

    tcx: &'a mut TypeContext,
}

impl<'a> Monomorphizer<'a> {
    pub fn new(tcx: &'a mut TypeContext) -> Self {
        Self {
            generic_functions: HashMap::new(),
            instances: Vec::new(),
            tcx,
        }
    }

    /// 执行单态化：收集→替换→生成
    pub fn monomorphize(&mut self, node: &mut TypedNode) -> Result<(), String> {
        self.collect_generic_functions(node)?;
        self.collect_instances(node)?;
        self.generate_and_replace(node)?;

        Ok(())
    }

    fn collect_generic_functions(&mut self, node: &TypedNode) -> Result<(), String> {
        let TypedNode::Program { statements, .. } = node;

        for stmt in statements {
            self.collect_in_stmt(stmt);
        }

        Ok(())
    }

    fn collect_in_stmt(&mut self, stmt: &TypedStatement) {
        match stmt {
            TypedStatement::ExpressionStatement(expr) => {
                self.collect_in_expr(expr);
            }
            TypedStatement::Let { value, .. } => {
                self.collect_in_expr(value);
            }
            TypedStatement::Block { statements, .. } => {
                for s in statements {
                    self.collect_in_stmt(s);
                }
            }
            TypedStatement::While {
                condition, block, ..
            } => {
                self.collect_in_expr(condition);
                self.collect_in_stmt(block);
            }
            TypedStatement::Return { expr, .. } => {
                self.collect_in_expr(expr);
            }
            _ => {}
        }
    }

    fn collect_in_expr(&mut self, expr: &TypedExpression) {
        match expr {
            TypedExpression::Function {
                name,
                generics_params,
                ..
            } => {
                if !generics_params.is_empty()
                    && let Some(fn_name) = name
                {
                    let param_names: Vec<String> = generics_params
                        .iter()
                        .filter_map(|p| {
                            if let TypedExpression::Ident(ident, _) = &**p {
                                Some(ident.value.to_string())
                            } else {
                                None
                            }
                        })
                        .collect();

                    if param_names.is_empty() {
                        return;
                    }

                    self.generic_functions.insert(
                        fn_name.value.to_string(),
                        GenericFunctionInfo {
                            expr: Box::new(expr.clone()),
                            param_names,
                        },
                    );
                }
            }

            TypedExpression::Call { func, args, .. } => {
                self.collect_in_expr(func);
                for arg in args {
                    self.collect_in_expr(arg);
                }
            }

            TypedExpression::Infix { left, right, .. } => {
                self.collect_in_expr(left);
                self.collect_in_expr(right);
            }

            TypedExpression::If {
                condition,
                consequence,
                else_block,
                ..
            } => {
                self.collect_in_expr(condition);
                self.collect_in_expr(consequence);
                if let Some(e) = else_block {
                    self.collect_in_expr(e);
                }
            }

            _ => {}
        }
    }

    fn collect_instances(&mut self, node: &TypedNode) -> Result<(), String> {
        let TypedNode::Program { statements, .. } = node;
        for stmt in statements {
            self.collect_instances_in_stmt(stmt);
        }

        Ok(())
    }

    fn collect_instances_in_stmt(&mut self, stmt: &TypedStatement) {
        match stmt {
            TypedStatement::ExpressionStatement(expr) => {
                self.collect_instances_in_expr(expr);
            }
            TypedStatement::Let { value, .. } => {
                self.collect_instances_in_expr(value);
            }
            TypedStatement::Block { statements, .. } => {
                for s in statements {
                    self.collect_instances_in_stmt(s);
                }
            }
            TypedStatement::While {
                condition, block, ..
            } => {
                self.collect_instances_in_expr(condition);
                self.collect_instances_in_stmt(block);
            }
            TypedStatement::Return { expr, .. } => {
                self.collect_instances_in_expr(expr);
            }
            _ => {}
        }
    }

    fn collect_instances_in_expr(&mut self, expr: &TypedExpression) {
        match expr {
            TypedExpression::Call { func, args, .. } => {
                if let TypedExpression::Ident(ident, _) = &**func {
                    let func_name = ident.value.as_ref();
                    if self.generic_functions.contains_key(func_name) {
                        let arg_types: Vec<TyId> = args
                            .iter()
                            .map(|a| a.get_type())
                            .collect();
                        let key = (func_name.to_string(), arg_types);
                        self.instances.push_no_repeat(key);
                    }
                }
                self.collect_instances_in_expr(func);
                for arg in args {
                    self.collect_instances_in_expr(arg);
                }
            }
            TypedExpression::Infix { left, right, .. } => {
                self.collect_instances_in_expr(left);
                self.collect_instances_in_expr(right);
            }
            TypedExpression::If {
                condition,
                consequence,
                else_block,
                ..
            } => {
                self.collect_instances_in_expr(condition);
                self.collect_instances_in_expr(consequence);
                if let Some(e) = else_block {
                    self.collect_instances_in_expr(e);
                }
            }
            _ => {}
        }
    }

    fn generate_and_replace(&mut self, node: &mut TypedNode) -> Result<(), String> {
        let TypedNode::Program { statements, .. } = node;

        // 第一步：生成专门化函数（插入前）
        let mut new_stmts = Vec::new();
        for (fname, type_args) in self.instances.clone() {
            if let Some(gen_info) = self.generic_functions.get(&fname) {
                let type_str = type_args
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join("_");
                let mangled = format!("{}__mono_{}", fname, type_str);

                let mut spec_func = gen_info.expr.clone();

                // generic_param_name -> concrete_type
                let mut type_map = HashMap::new();
                for (param_name, concrete_ty) in gen_info.param_names.iter().zip(type_args.iter()) {
                    type_map.insert(param_name.clone(), concrete_ty.clone());
                }

                // 应用类型替换
                self.substitute_generics_in_expr(&mut spec_func, &type_map);

                // 改名+清空泛型参数，确保函数类型也替换到具体类型
                if let TypedExpression::Function {
                    name,
                    generics_params,
                    ty,
                    ..
                } = &mut *spec_func
                {
                    generics_params.clear();
                    *name = Some(token::Token {
                        value: mangled.into(),
                        token_type: TokenType::Ident,
                        line: 0,
                        column: 0,
                        file: "monomorphizer".into(),
                    });
                    let t = self.substitute_generic_ty(ty, &type_map);
                    *ty = t;
                }

                new_stmts.push(TypedStatement::ExpressionStatement(*spec_func));
            }
        }

        // 插入到程序起始处
        for stmt in new_stmts.into_iter().rev() {
            statements.insert(0, stmt);
        }

        // 第二步：替换调用点
        for stmt in statements.iter_mut() {
            self.replace_calls_in_stmt(stmt);
        }

        // 第三步：移除原始泛型函数
        statements.retain(|stmt| !Self::is_generic_def(stmt, &self.generic_functions));

        Ok(())
    }

    fn replace_calls_in_stmt(&mut self, stmt: &mut TypedStatement) {
        match stmt {
            TypedStatement::ExpressionStatement(expr) => {
                self.replace_calls_in_expr(expr);
            }

            TypedStatement::Let { value, .. } => {
                self.replace_calls_in_expr(value);
            }

            TypedStatement::Return { expr, .. } => {
                self.replace_calls_in_expr(expr);
            }

            TypedStatement::Block { statements, .. } => {
                for s in statements {
                    self.replace_calls_in_stmt(s);
                }
            }

            TypedStatement::While {
                condition, block, ..
            } => {
                self.replace_calls_in_expr(condition);
                self.replace_calls_in_stmt(block);
            }

            _ => {}
        }
    }

    fn replace_calls_in_expr(&mut self, expr: &mut TypedExpression) {
        match expr {
            TypedExpression::Call {
                func,
                args,
                func_ty,
                ..
            } => {
                if let TypedExpression::Ident(ident, _) = &mut **func {
                    if let Some(gen_info) = self.generic_functions.get(ident.value.as_ref()) {
                        // 构造实例化类型映射：泛型参数名 -> 实参类型
                        let arg_types: Vec<TyId> = args
                            .iter()
                            .map(|a| a.get_type())
                            .collect();
                        let mut type_map = HashMap::new();
                        for (param_name, concrete_ty) in
                            gen_info.param_names.iter().zip(arg_types.iter())
                        {
                            type_map.insert(param_name.clone(), concrete_ty.clone());
                        }

                        // 改名
                        let type_str = arg_types
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join("_");

                        let mangled = format!("{}__mono_{}", ident.value.as_ref(), type_str);
                        ident.value = mangled.into();

                        // 同步把调用表达式的 func_ty 从泛型替到具体类型
                        let t = self.substitute_generic_ty(func_ty, &type_map);
                        *func_ty = t;
                    }
                }

                // 递归处理子表达式
                self.replace_calls_in_expr(func);
                for arg in args {
                    self.replace_calls_in_expr(arg);
                }
            }

            TypedExpression::Infix { left, right, .. } => {
                self.replace_calls_in_expr(left);
                self.replace_calls_in_expr(right);
            }

            TypedExpression::If {
                condition,
                consequence,
                else_block,
                ..
            } => {
                self.replace_calls_in_expr(condition);
                self.replace_calls_in_expr(consequence);
                if let Some(e) = else_block {
                    self.replace_calls_in_expr(e);
                }
            }

            TypedExpression::Function { params, block, .. } => {
                for p in params {
                    self.replace_calls_in_expr(p);
                }
                self.replace_calls_in_expr(block);
            }

            TypedExpression::Block(_, stmts, ..) => {
                for s in stmts {
                    self.replace_calls_in_stmt(s);
                }
            }

            _ => {}
        }
    }

    fn is_generic_def(
        stmt: &TypedStatement,
        generic_functions: &HashMap<String, GenericFunctionInfo>,
    ) -> bool {
        if let TypedStatement::ExpressionStatement(TypedExpression::Function { name, .. }) = stmt {
            name.as_ref()
                .map(|n| generic_functions.contains_key(n.value.as_ref()))
                .unwrap_or(false)
        } else {
            false
        }
    }

    fn substitute_generics_in_expr(
        &mut self,
        expr: &mut TypedExpression,
        type_map: &HashMap<String, TyId>,
    ) {
        match expr {
            TypedExpression::Ident(_, ty) => {
                let t = self.substitute_generic_ty(ty, type_map);
                *ty = t;
            }

            TypedExpression::TypeHint(_, _, ty) => {
                let t = self.substitute_generic_ty(ty, type_map);
                *ty = t;
            }

            TypedExpression::Function {
                params, block, ty, ..
            } => {
                for param in params {
                    self.substitute_generics_in_expr(param, type_map);
                }
                self.substitute_generics_in_expr(block, type_map);
                let t = self.substitute_generic_ty(ty, type_map);
                *ty = t;
            }

            TypedExpression::Call {
                func,
                args,
                func_ty,
                ..
            } => {
                self.substitute_generics_in_expr(func, type_map);
                for arg in args {
                    self.substitute_generics_in_expr(arg, type_map);
                }
                let t = self.substitute_generic_ty(func_ty, type_map);
                *func_ty = t;
            }

            TypedExpression::Infix { left, right, .. } => {
                self.substitute_generics_in_expr(left, type_map);
                self.substitute_generics_in_expr(right, type_map);
            }

            TypedExpression::If {
                condition,
                consequence,
                else_block,
                ..
            } => {
                self.substitute_generics_in_expr(condition, type_map);
                self.substitute_generics_in_expr(consequence, type_map);
                if let Some(e) = else_block {
                    self.substitute_generics_in_expr(e, type_map);
                }
            }

            TypedExpression::Block(_, stmts, ty) => {
                for s in stmts {
                    self.substitute_generics_in_stmt(s, type_map);
                }

                let t = self.substitute_generic_ty(ty, type_map);
                *ty = t;
            }

            _ => {}
        }
    }

    fn substitute_generics_in_stmt(
        &mut self,
        stmt: &mut TypedStatement,
        type_map: &HashMap<String, TyId>,
    ) {
        match stmt {
            TypedStatement::ExpressionStatement(expr) => {
                self.substitute_generics_in_expr(expr, type_map);
            }

            TypedStatement::Let { value, .. } => {
                self.substitute_generics_in_expr(value, type_map);
            }

            TypedStatement::Return { expr, .. } => {
                self.substitute_generics_in_expr(expr, type_map);
            }

            TypedStatement::Block { statements, ty, .. } => {
                for s in statements {
                    self.substitute_generics_in_stmt(s, type_map);
                }

                let t = self.substitute_generic_ty(ty, type_map);
                *ty = t;
            }

            TypedStatement::While {
                condition, block, ..
            } => {
                self.substitute_generics_in_expr(condition, type_map);
                self.substitute_generics_in_stmt(block, type_map);
            }

            _ => {}
        }
    }

    fn substitute_generic_ty(&mut self, tyid: &TyId, type_map: &HashMap<String, TyId>) -> TyId {
        let ty = self.tcx.get(*tyid).clone();

        match ty {
            Ty::Generic(name, _) => type_map
                .get(name.as_ref())
                .cloned()
                .unwrap_or_else(|| *tyid),
            Ty::AppliedGeneric(name, args) => {
                let new_args: Vec<TyId> = args
                    .iter()
                    .map(|id| self.substitute_generic_ty(id, type_map))
                    .collect();

                self.tcx.alloc(Ty::AppliedGeneric(name.clone(), new_args))
            }

            Ty::Function {
                params_type,
                ret_type,
                is_variadic,
            } => {
                let ty = Ty::Function {
                    params_type: params_type
                        .iter()
                        .map(|t| self.substitute_generic_ty(&t, type_map))
                        .collect(),
                    ret_type: self.substitute_generic_ty(&ret_type, type_map),
                    is_variadic,
                };

                self.tcx.alloc(ty)
            }

            _ => *tyid,
        }
    }
}