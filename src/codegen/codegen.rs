use std::collections::HashMap;
use inkwell::{builder::Builder, context::Context, execution_engine::{ExecutionEngine, JitFunction}, module::Module, types::BasicMetadataTypeEnum, values::{BasicMetadataValueEnum, BasicValueEnum, FloatValue, FunctionValue, PointerValue}, FloatPredicate};

use crate::ast_builder::*;

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub function: &'a ExprAST,
    
    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl <'a, 'ctx> Compiler <'a, 'ctx> {
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    fn create_entry_block_alloc(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.fn_value().get_first_basic_block().unwrap();
        match entry.get_first_instruction() {
            Some(first_instuction) => builder.position_before(&first_instuction),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name).unwrap()
    }

    pub fn build_load(&self, ptr: PointerValue<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
        self.builder.build_load(self.context.f64_type(), ptr, name).unwrap()
    }

    fn compile_expr(&mut self, expr: &ExprAST) -> Result<FloatValue<'ctx>, Box<dyn std::error::Error>> {
        match *expr {
            ExprAST::NumberExprAST(num) => Ok(self.context.f64_type().const_float(num)),
            ExprAST::VariableExprAST(name) => match self.variables.get(name) {
                Some(var) => Ok(self.build_load(*var, name.as_str().into_float_value())),
                None => Err("could not find a matching variable".into()),
            },
            ExprAST::BinaryExprAST(op, lhs, rhs) => if op == '=' {
                    let var_name = match lhs {
                        ExprAST::VariableExprAST(name) => name,
                        _ => return Err("Expected variable as left-hand operator of assignment".into())
                    };
                    let var_val = self.compile_expr(rhs)?;
                    let var = self.variables.get(var_name).ok_or("Undefined variable")?;
                    self.builder.build_store(*var, var_val).unwrap();
                    Ok(var_val)
                } else {
                    let l = self.compile_expr(lhs)?;
                    let r = self.compile_expr(rhs)?;

                    match op {
                        '+' => Ok(self.builder.build_float_add(lhs, rhs, "tmpadd").unwrap()),
                        '-' => Ok(self.builder.build_float_sub(lhs, rhs, "tmpsub").unwrap()),
                        '*' => Ok(self.builder.build_float_mul(lhs, rhs, "tmpmul").unwrap()),
                        '/' => Ok(self.builder.build_float_div(lhs, rhs, "tmpdiv").unwrap()),
                        '<' => Ok({
                            let cmp = self.builder.build_float_compare(FloatPredicate::ULT, lhs, rhs, "tmpcmp").unwrap();
                            self.builder.build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool").unwrap()
                        }),
                        _ => Err("undefined binary operator".into())

                    }
            },
            ExprAST::CallExprAST(callee, args) => match self.get_function(callee) {
                Some(func) => {
                    let mut compiled_args = Vec::with_capacity(args.len());
                    for arg in args {
                        compiled_args.push(self.compile_expr(arg)?);
                    }
                    let argsv: Vec<BasicMetadataValueEnum> = compiled_args.iter().by_ref().map(|&val| val.into()).collect();
                    match self.builder.build_call(func, argsv.as_slice(), "tmp").unwrap().try_as_basic_value().left() {
                        Some(value) => Ok(value.into_float_value()),
                        None => Err("invalid call produced"),
                    }
                },
                None => Err("unknown function"),
            },
            _ => Err("invalid function declaration".into())
        }
    }

    fn compile_prototype(&self, proto: &ExprAST) -> Result<FunctionValue<'ctx>, Box<dyn std::error::Error>> {
        if let ExprAST::PrototypeAST(name, args) = proto {
            let ret_type = self.context.f64_type();
            let args_types = std::iter::repeat(ret_type)
                .take(args.len())
                .map(|f| f.into())
                .collect::<Vec<BasicMetadataTypeEnum>>()
                .as_slice();
            let fn_type = self.context.f64_type().fn_type(args_types, false);
            let fn_val = self.module.add_function(name.as_str(), fn_type, None);
            for (i, arg) in fn_val.get_param_iter().enumerate() {
                arg.into_float_value().set_name(args[i].as_str());
            }

            Ok(fn_val)
        } else {
            Err("invalid compile_prototype call".into())
        }
    }
}
