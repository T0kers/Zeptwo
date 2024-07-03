use crate::position::{Pos, Position};

use super::{expr::*, identifiers::*, *};

pub trait CheckTypes {
    fn check_types(&mut self, lookup: &mut IdentifierLookup) -> Result<(), ZeptwoError>;
}

#[derive(Debug, Clone)]
pub enum Stmt {
    While {
        condition: Expr,
        body: Vec<Stmt>,
        variable_storage: Option<usize>,
    },
    Let {
        var_id: VariableID,
        expr: Expr,
    },
    Return {
        expr: Expr,
    },
    Expr {
        expr: Expr,
    },
}

impl Position for Stmt {
    fn left_pos(&self, lookup: &IdentifierLookup) -> Pos {
        match self {
            Stmt::While { condition, .. } => condition.left_pos(lookup),
            Stmt::Let { var_id, .. } => var_id.left_pos(lookup),
            Stmt::Return { expr } => expr.left_pos(lookup),
            Stmt::Expr { expr } => expr.left_pos(lookup),
        }
    }
    fn right_pos(&self, lookup: &IdentifierLookup) -> Pos {
        match self {
            Stmt::While { body, .. } => body.last().unwrap().right_pos(lookup),
            Stmt::Let { expr, .. } => expr.right_pos(lookup),
            Stmt::Return { expr } => expr.right_pos(lookup),
            Stmt::Expr { expr } => expr.right_pos(lookup),
        }
    }
}

impl CheckTypes for Stmt {
    fn check_types(&mut self, lookup: &mut IdentifierLookup) -> Result<(), ZeptwoError> {
        match self {
            Self::While {
                condition, body, ..
            } => {
                let condition_type = condition.determine_type_and_opcode(lookup)?.clone();
                if let ValType::Bool = condition_type {
                } else {
                    Err(ZeptwoError::parser_error_at_line(
                        condition.right_pos(lookup),
                        format!(
                            "Expected type '{}' from condition. Found '{}'",
                            ValType::Bool,
                            condition_type
                        ),
                    ))?;
                }
                for stmt in body {
                    stmt.check_types(lookup)?;
                }
                Ok(())
            }
            Stmt::Let { var_id, expr } => {
                let expr_type = expr.determine_type_and_opcode(lookup)?;

                let var_type = var_id.get_variable_type(lookup);
                match var_type {
                    Some(t) => {
                        if t != expr_type {
                            return Err(ZeptwoError::Parser(
                                "Explicit type not matching with expression type.".to_string(),
                            )); // TODO: BETTER ERROR!!!
                        }
                    }
                    None => {
                        var_id.set_variable_type(Some(&expr_type), lookup);
                    }
                };

                Ok(())
            }
            Stmt::Return { expr, .. } => {
                expr.determine_type_and_opcode(lookup)?;
                Ok(())
            }
            Stmt::Expr { expr, .. } => {
                expr.determine_type_and_opcode(lookup)?;
                Ok(())
            }
        }
    }
}

impl SizeAndIndex for Stmt {
    fn determine_size_and_indexes(
        &mut self,
        stack_index: &mut usize,
        lookup: &mut IdentifierLookup,
    ) -> Result<usize, ZeptwoError> {
        match self {
            Stmt::While {
                condition,
                body,
                variable_storage,
            } => {
                let mut max_size = condition.determine_size_and_indexes(stack_index, lookup)?;
                let stack_index_start = *stack_index;
                for stmt in body {
                    let stmt_size = stmt.determine_size_and_indexes(stack_index, lookup)?;
                    max_size =
                        std::cmp::max(max_size, (*stack_index - stack_index_start) + stmt_size);
                }
                let var_storage = *stack_index - stack_index_start;
                *variable_storage = Some(var_storage);

                *stack_index = stack_index_start;
                Ok(max_size)
            }
            Stmt::Let { var_id, expr } => {
                let max_size = expr.determine_size_and_indexes(stack_index, lookup)?;
                var_id.determine_size_and_indexes(stack_index, lookup)?;
                println!(
                    "name: {}, index: {:?}, max_size: {}",
                    var_id.get_variable_info(lookup).name,
                    var_id.get_variable_info(lookup).stack_index,
                    max_size
                );
                Ok(max_size - expr.get_result_size(lookup) as usize) // subtracting expr_result_size, because the amount of space the variable uses on the stack is accounted for
            }
            Stmt::Return { expr, .. } => Ok(expr.determine_size_and_indexes(stack_index, lookup)?),
            Stmt::Expr { expr, .. } => Ok(expr.determine_size_and_indexes(stack_index, lookup)?),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}
