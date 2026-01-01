use ant_token::token;
use ant_token::token_type::TokenType;
use ant_type_checker::ty::Ty;
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
pub struct Monomorphizer {
    generic_functions: HashMap<String, GenericFunctionInfo>,
    instances: Vec<(String, Vec<Ty>)>,
}

impl Monomorphizer {
    pub fn new() -> Self {
        Self {
            generic_functions: HashMap::new(),
            instances: Vec::new(),
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
            Self::collect_in_stmt(stmt, &mut self.generic_functions);
        }

        Ok(())
    }

    fn collect_in_stmt(
        stmt: &TypedStatement,
        generic_functions: &mut HashMap<String, GenericFunctionInfo>,
    ) {
        match stmt {
            TypedStatement::ExpressionStatement(expr) => {
                Self::collect_in_expr(expr, generic_functions);
            }
            TypedStatement::Let { value, .. } => {
                Self::collect_in_expr(value, generic_functions);
            }
            TypedStatement::Block { statements, .. } => {
                for s in statements {
                    Self::collect_in_stmt(s, generic_functions);
                }
            }
            TypedStatement::While {
                condition, block, ..
            } => {
                Self::collect_in_expr(condition, generic_functions);
                Self::collect_in_stmt(block, generic_functions);
            }
            TypedStatement::Return { expr, .. } => {
                Self::collect_in_expr(expr, generic_functions);
            }
            _ => {}
        }
    }

    fn collect_in_expr(
        expr: &TypedExpression,
        generic_functions: &mut HashMap<String, GenericFunctionInfo>,
    ) {
        match expr {
            TypedExpression::Function {
                name,
                generics_params,
                ..
            } => if !generics_params.is_empty() && let Some(fn_name) = name {
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

                generic_functions.insert(
                    fn_name.value.to_string(),
                    GenericFunctionInfo {
                        expr: Box::new(expr.clone()),
                        param_names,
                    },
                );
            }

            TypedExpression::Call { func, args, .. } => {
                Self::collect_in_expr(func, generic_functions);
                for arg in args {
                    Self::collect_in_expr(arg, generic_functions);
                }
            }
            
            TypedExpression::Infix { left, right, .. } => {
                Self::collect_in_expr(left, generic_functions);
                Self::collect_in_expr(right, generic_functions);
            }
            
            TypedExpression::If {
                condition,
                consequence,
                else_block,
                ..
            } => {
                Self::collect_in_expr(condition, generic_functions);
                Self::collect_in_expr(consequence, generic_functions);
                if let Some(e) = else_block {
                    Self::collect_in_expr(e, generic_functions);
                }
            }
            
            _ => {}
        }
    }

    fn collect_instances(&mut self, node: &TypedNode) -> Result<(), String> {
        let TypedNode::Program { statements, .. } = node;
        for stmt in statements {
            Self::collect_instances_in_stmt(stmt, &self.generic_functions, &mut self.instances);
        }

        Ok(())
    }

    fn collect_instances_in_stmt(
        stmt: &TypedStatement,
        generic_functions: &HashMap<String, GenericFunctionInfo>,
        instances: &mut Vec<(String, Vec<Ty>)>,
    ) {
        match stmt {
            TypedStatement::ExpressionStatement(expr) => {
                Self::collect_instances_in_expr(expr, generic_functions, instances);
            }
            TypedStatement::Let { value, .. } => {
                Self::collect_instances_in_expr(value, generic_functions, instances);
            }
            TypedStatement::Block { statements, .. } => {
                for s in statements {
                    Self::collect_instances_in_stmt(s, generic_functions, instances);
                }
            }
            TypedStatement::While {
                condition, block, ..
            } => {
                Self::collect_instances_in_expr(condition, generic_functions, instances);
                Self::collect_instances_in_stmt(block, generic_functions, instances);
            }
            TypedStatement::Return { expr, .. } => {
                Self::collect_instances_in_expr(expr, generic_functions, instances);
            }
            _ => {}
        }
    }

    fn collect_instances_in_expr(
        expr: &TypedExpression,
        generic_functions: &HashMap<String, GenericFunctionInfo>,
        instances: &mut Vec<(String, Vec<Ty>)>,
    ) {
        match expr {
            TypedExpression::Call { func, args, .. } => {
                if let TypedExpression::Ident(ident, _) = &**func {
                    let func_name = ident.value.as_ref();
                    if generic_functions.contains_key(func_name) {
                        let arg_types: Vec<Ty> = args.iter().map(|a| a.get_type()).collect();
                        let key = (func_name.to_string(), arg_types);
                        instances.push_no_repeat(key);
                    }
                }
                Self::collect_instances_in_expr(func, generic_functions, instances);
                for arg in args {
                    Self::collect_instances_in_expr(arg, generic_functions, instances);
                }
            }
            TypedExpression::Infix { left, right, .. } => {
                Self::collect_instances_in_expr(left, generic_functions, instances);
                Self::collect_instances_in_expr(right, generic_functions, instances);
            }
            TypedExpression::If {
                condition,
                consequence,
                else_block,
                ..
            } => {
                Self::collect_instances_in_expr(condition, generic_functions, instances);
                Self::collect_instances_in_expr(consequence, generic_functions, instances);
                if let Some(e) = else_block {
                    Self::collect_instances_in_expr(e, generic_functions, instances);
                }
            }
            _ => {}
        }
    }

    fn generate_and_replace(&mut self, node: &mut TypedNode) -> Result<(), String> {
        let TypedNode::Program { statements, .. } = node;

        // 第一步：生成专门化函数（插入前）
        let mut new_stmts = Vec::new();
        for (fname, type_args) in &self.instances {
            if let Some(gen_info) = self.generic_functions.get(fname) {
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
                Self::substitute_generics_in_expr(&mut spec_func, &type_map);

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
                    *ty = Self::substitute_generic_ty(ty, &type_map);
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
            Self::replace_calls_in_stmt(stmt, &self.generic_functions);
        }

        // 第三步：移除原始泛型函数
        statements.retain(|stmt| !Self::is_generic_def(stmt, &self.generic_functions));

        Ok(())
    }

    fn replace_calls_in_stmt(
        stmt: &mut TypedStatement,
        generic_functions: &HashMap<String, GenericFunctionInfo>,
    ) {
        match stmt {
            TypedStatement::ExpressionStatement(expr) => {
                Self::replace_calls_in_expr(expr, generic_functions);
            }
            
            TypedStatement::Let { value, .. } => {
                Self::replace_calls_in_expr(value, generic_functions);
            }
            
            TypedStatement::Return { expr, .. } => {
                Self::replace_calls_in_expr(expr, generic_functions);
            }
            
            TypedStatement::Block { statements, .. } => {
                for s in statements {
                    Self::replace_calls_in_stmt(s, generic_functions);
                }
            }
            
            TypedStatement::While {
                condition, block, ..
            } => {
                Self::replace_calls_in_expr(condition, generic_functions);
                Self::replace_calls_in_stmt(block, generic_functions);
            }
            
            _ => {}
        }
    }

    fn replace_calls_in_expr(
        expr: &mut TypedExpression,
        generic_functions: &HashMap<String, GenericFunctionInfo>,
    ) {
        match expr {
            TypedExpression::Call {
                func,
                args,
                func_ty,
                ..
            } => {
                if let TypedExpression::Ident(ident, _) = &mut **func {
                    if let Some(gen_info) = generic_functions.get(ident.value.as_ref()) {
                        // 构造实例化类型映射：泛型参数名 -> 实参类型
                        let arg_types: Vec<Ty> = args.iter().map(|a| a.get_type()).collect();
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
                        *func_ty = Self::substitute_generic_ty(func_ty, &type_map);
                    }
                }

                // 递归处理子表达式
                Self::replace_calls_in_expr(func, generic_functions);
                for arg in args {
                    Self::replace_calls_in_expr(arg, generic_functions);
                }
            }
            
            TypedExpression::Infix { left, right, .. } => {
                Self::replace_calls_in_expr(left, generic_functions);
                Self::replace_calls_in_expr(right, generic_functions);
            }
            
            TypedExpression::If {
                condition,
                consequence,
                else_block,
                ..
            } => {
                Self::replace_calls_in_expr(condition, generic_functions);
                Self::replace_calls_in_expr(consequence, generic_functions);
                if let Some(e) = else_block {
                    Self::replace_calls_in_expr(e, generic_functions);
                }
            }
            
            TypedExpression::Function { params, block, .. } => {
                for p in params {
                    Self::replace_calls_in_expr(p, generic_functions);
                }
                Self::replace_calls_in_expr(block, generic_functions);
            }
            
            TypedExpression::Block(_, stmts, ..) => {
                for s in stmts {
                    Self::replace_calls_in_stmt(s, generic_functions);
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

    fn substitute_generics_in_expr(expr: &mut TypedExpression, type_map: &HashMap<String, Ty>) {
        match expr {
            TypedExpression::Ident(_, ty) => {
                *ty = Self::substitute_generic_ty(ty, type_map);
            }
            
            TypedExpression::TypeHint(_, _, ty) => {
                *ty = Self::substitute_generic_ty(ty, type_map);
            }
            
            TypedExpression::Function {
                params, block, ty, ..
            } => {
                for param in params {
                    Self::substitute_generics_in_expr(param, type_map);
                }
                Self::substitute_generics_in_expr(block, type_map);
                *ty = Self::substitute_generic_ty(ty, type_map);
            }
            
            TypedExpression::Call {
                func,
                args,
                func_ty,
                ..
            } => {
                Self::substitute_generics_in_expr(func, type_map);
                for arg in args {
                    Self::substitute_generics_in_expr(arg, type_map);
                }
                *func_ty = Self::substitute_generic_ty(func_ty, type_map);
            }
            
            TypedExpression::Infix { left, right, .. } => {
                Self::substitute_generics_in_expr(left, type_map);
                Self::substitute_generics_in_expr(right, type_map);
            }
            
            TypedExpression::If {
                condition,
                consequence,
                else_block,
                ..
            } => {
                Self::substitute_generics_in_expr(condition, type_map);
                Self::substitute_generics_in_expr(consequence, type_map);
                if let Some(e) = else_block {
                    Self::substitute_generics_in_expr(e, type_map);
                }
            }

            TypedExpression::Block(_, stmts, ty) => {
                for s in stmts {
                    Self::substitute_generics_in_stmt(s, type_map);
                }
                *ty = Self::substitute_generic_ty(ty, type_map);
            }
            
            _ => {}
        }
    }

    fn substitute_generics_in_stmt(stmt: &mut TypedStatement, type_map: &HashMap<String, Ty>) {
        match stmt {
            TypedStatement::ExpressionStatement(expr) => {
                Self::substitute_generics_in_expr(expr, type_map);
            }
            
            TypedStatement::Let { value, .. } => {
                Self::substitute_generics_in_expr(value, type_map);
            }
            
            TypedStatement::Return { expr, .. } => {
                Self::substitute_generics_in_expr(expr, type_map);
            }
            
            TypedStatement::Block { statements, ty, .. } => {
                for s in statements {
                    Self::substitute_generics_in_stmt(s, type_map);
                }
                *ty = Self::substitute_generic_ty(ty, type_map);
            }
            
            TypedStatement::While {
                condition, block, ..
            } => {
                Self::substitute_generics_in_expr(condition, type_map);
                Self::substitute_generics_in_stmt(block, type_map);
            }
            
            _ => {}
        }
    }

    fn substitute_generic_ty(ty: &Ty, type_map: &HashMap<String, Ty>) -> Ty {
        match ty {
            Ty::Generic(name, _) => type_map
                .get(name.as_ref())
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            Ty::Function {
                params_type,
                ret_type,
                is_variadic,
            } => Ty::Function {
                params_type: params_type
                    .iter()
                    .map(|t| Self::substitute_generic_ty(t, type_map))
                    .collect(),
                ret_type: Box::new(Self::substitute_generic_ty(ret_type, type_map)),
                is_variadic: *is_variadic,
            },
           
            _ => ty.clone(),
        }
    }
}
