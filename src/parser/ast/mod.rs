use std::fmt;
pub mod identifiers;
use identifiers::IdentifierLookup;
pub mod stmt;
use stmt::*;
pub mod expr;
use expr::*;

use crate::errors::ZeptwoError;

pub trait SizeAndIndex {
    fn determine_size_and_indexes(
        &mut self,
        stack_index: &mut usize,
        lookup: &mut IdentifierLookup,
    ) -> Result<usize, ZeptwoError>;
}

// #[derive(Debug, Default)]
// pub struct AST {
//     pub let_statements: Vec<Let>,
//     pub main_fn: ZeptwoFn,
//     pub fn_statements: Vec<ZeptwoFn>,
//     pub class_statements: Vec<Class>,
// }

// impl CheckTypes for AST {
//     fn check_types(&mut self) -> Result<(), ZeptwoError> {
//         let mut function_id_vec = FUNCTION_ID_VEC.lock().unwrap();
//         for function in function_id_vec.iter_mut() {
//             function.check_types()?;
//         }

//         self.main_fn.check_types()?;
//         for stmt in &mut self.fn_statements {
//             stmt.check_types()?;
//         }
//         Ok(())
//     }
// }

// impl SizeAndIndex for AST {
//     fn determine_size_and_indexes(
//         &mut self,
//         stack_index: &mut usize,
//     ) -> Result<usize, ZeptwoError> {
//         let mut function_id_vec = FUNCTION_ID_VEC.lock().unwrap();
//         for function in function_id_vec.iter_mut() {
//             *stack_index = 0;
//             function.determine_size_and_indexes(stack_index)?;
//         }
//         *stack_index = 0;
//         self.main_fn.determine_size_and_indexes(stack_index)?;

//         Ok(0) // TODO: remove
//     }
// }

// #[derive(Debug)]
// pub struct Let {}

// #[derive(Debug, Clone)]
// pub struct ZeptwoFn {
//     pub name: &'static str,
//     pub params: Vec<VariableID>,
//     pub body: Expr,
//     pub stack_size: Option<usize>,
//     pub return_type: ValType,
// }

// impl ZeptwoFn {
//     pub fn new(
//         name: &'static str,
//         params: Vec<VariableID>,
//         body: Expr,
//         return_type: ValType,
//     ) -> Self {
//         Self {
//             name,
//             params,
//             body,
//             stack_size: None,
//             return_type,
//         }
//     }
// }

// impl CheckTypes for ZeptwoFn {
//     fn check_types(&mut self) -> Result<(), ZeptwoError> {
//         if self.return_type != self.body.determine_type_and_opcode()? {
//             Err(ZeptwoError::parser_error(format!(
//                 "Expected function '{}' to return type '{}'",
//                 self.name, self.return_type
//             )))?;
//         }
//         Ok(())
//     }
// }

// impl SizeAndIndex for ZeptwoFn {
//     fn determine_size_and_indexes(
//         &mut self,
//         stack_index: &mut usize,
//     ) -> Result<usize, ZeptwoError> {
//         let mut total_size = 0;

//         for param in &mut self.params {
//             total_size += param.determine_size_and_indexes(stack_index)?;
//         }
//         total_size += self.body.determine_size_and_indexes(stack_index)?;

//         self.stack_size = Some(total_size);
//         println!("name: {}, size: {}", self.name, self.stack_size.unwrap());
//         Ok(total_size)
//     }
// }

// impl std::default::Default for ZeptwoFn {
//     fn default() -> Self {
//         Self {
//             name: "",
//             params: vec![],
//             body: Expr::Nothing,
//             stack_size: None,
//             return_type: ValType::Nul,
//         }
//     }
// }

// #[derive(Debug)]
// pub struct Class {}

pub enum Item {
    Stmt(Stmt),
    Expr(Expr),
}

// impl std::fmt::Display for Item {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Item::Stmt(s) => write!(f, "{}", s),
//             Item::Expr(e) => write!(f, "{}", e),
//         }
//     }
// }
