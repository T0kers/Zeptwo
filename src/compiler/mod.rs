use identifiers::{FunctionInfo, IdentifierLookup};

use crate::errors::ZeptwoError;
use crate::parser::ast::{
    expr::{Expr, OpCodeOp, ValTypeTrait},
    stmt::Stmt,
    *,
};

use crate::vm::bytecode::{BytecodeConstructer, FullOpCode};
use crate::{
    position::Position,
    vm::bytecode::Bytecode,
};

pub trait Compile {
    fn compile(
        &mut self,
        bytecode: &mut BytecodeConstructer,
        lookup: &mut IdentifierLookup,
    ) -> Result<(), ZeptwoError>;
}

pub struct Compiler {
    lookup: IdentifierLookup,
}

impl Compiler {
    pub fn new(lookup: IdentifierLookup) -> Self {
        Compiler { lookup }
    }
    pub fn compile(&mut self) -> Result<Bytecode, ZeptwoError> {
        let mut bytecode = BytecodeConstructer::new();
        bytecode.emit_main_call();
        let jump = bytecode.emit_jump(FullOpCode::Jump { offset: 0 });
        for i in 0..self.lookup.function_id_vec.len() {
            self.lookup.compile_function(&mut bytecode, i)?;
        }
        bytecode.patch_jump(jump)?;
        bytecode.add_opcode_same_pos(FullOpCode::Eof);
        Ok(bytecode.finish())
    }
}

impl IdentifierLookup {
    fn compile_function(
        &mut self,
        bytecode: &mut BytecodeConstructer,
        i: usize,
    ) -> Result<(), ZeptwoError> {
        self.function_id_vec[i].start_adress = Some(bytecode.next_instruction_index());
        if self.function_id_vec[i].is_main() {
            bytecode.patch_main_call(self.function_id_vec[i].start_adress.unwrap())?;
        }
        let mut body = self.function_id_vec[i].body.clone();
        body.compile(bytecode, self)?;
        self.function_id_vec[i].body = body;

        bytecode.add_opcode_same_pos(FullOpCode::Return);
        Ok(())
    }
}

impl Compile for FunctionInfo {
    fn compile(
        &mut self,
        bytecode: &mut BytecodeConstructer,
        lookup: &mut IdentifierLookup,
    ) -> Result<(), ZeptwoError> {
        self.start_adress = Some(bytecode.next_instruction_index());
        self.body.compile(bytecode, lookup)?;
        bytecode.add_opcode_same_pos(FullOpCode::Return);
        Ok(())
    }
}

impl Compile for Stmt {
    fn compile(
        &mut self,
        bytecode: &mut BytecodeConstructer,
        lookup: &mut IdentifierLookup,
    ) -> Result<(), ZeptwoError> {
        match self {
            Stmt::While {
                condition,
                body,
                variable_storage,
            } => {
                let loop_start = bytecode.next_instruction_index();
                condition.compile(bytecode, lookup)?;
                let exit_jump = bytecode.emit_jump(FullOpCode::PopJumpIfFalse { offset: 0 });

                for stmt in body {
                    stmt.compile(bytecode, lookup)?;
                }
                bytecode.pop_usize_same_line(variable_storage.unwrap());

                bytecode.emit_loop(loop_start)?;

                bytecode.patch_jump(exit_jump)?;
            }
            Stmt::Let { expr, .. } => {
                expr.compile(bytecode, lookup)?;
            }
            Stmt::Return { expr } => {
                expr.compile(bytecode, lookup)?;
                bytecode.add_opcode(FullOpCode::Return, self.left_pos(lookup))
            }
            Stmt::Expr { expr } => {
                expr.compile(bytecode, lookup)?;
                let expr_size = expr.get_result_size(lookup);
                if expr_size != 0 {
                    bytecode
                        .add_opcode(FullOpCode::Pop { amount: expr_size }, self.left_pos(lookup));
                }
            }
        }
        Ok(())
    }
}

impl Compile for Expr {
    fn compile(
        &mut self,
        bytecode: &mut BytecodeConstructer,
        lookup: &mut IdentifierLookup,
    ) -> Result<(), ZeptwoError> {
        match self {
            Expr::Block {
                body,
                value,
                variable_storage,
                data_type,
            } => {
                for stmt in body {
                    stmt.compile(bytecode, lookup)?;
                }
                value.compile(bytecode, lookup)?;

                let return_size = data_type.as_ref().unwrap().get_result_size(lookup);
                if return_size == 0 {
                    bytecode.pop_usize(variable_storage.unwrap(), value.left_pos(lookup));
                } else {
                    bytecode.value_pop_usize_same_line(return_size, variable_storage.unwrap());
                }
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                condition.compile(bytecode, lookup)?;

                let then_jump = bytecode.emit_jump(FullOpCode::PopJumpIfFalse { offset: 0 });

                then_branch.compile(bytecode, lookup)?;

                if let Some(else_branch) = else_branch {
                    let else_jump = bytecode.emit_jump(FullOpCode::Jump { offset: 0 });

                    bytecode.patch_jump(then_jump)?;

                    else_branch.compile(bytecode, lookup)?;

                    bytecode.patch_jump(else_jump)?;
                } else {
                    bytecode.patch_jump(then_jump)?;
                }
            }
            Expr::FnCall { callee, args } => {
                let mut args_size = 0;
                for arg in args {
                    arg.compile(bytecode, lookup)?;
                    args_size += arg.get_result_size(lookup);
                }
                let address = {
                    let address = callee.get_address(lookup).unwrap();
                    if address > u32::MAX as usize {
                        Err(ZeptwoError::compiler_error_at_line(
                            self.left_pos(lookup),
                            "Function addresss to too big.",
                        ))?
                    } else {
                        address as u32
                    }
                };
                bytecode.add_opcode_same_pos(FullOpCode::CallFn {
                    stack_offset_offset: args_size,
                    address,
                });
            }
            Expr::AmbiguousFnCall { .. } => unreachable!(),
            Expr::Assignment { var_id, node } => {
                node.compile(bytecode, lookup)?;
                let id = var_id.get_variable_info(lookup);
                let (Some(data_type), Some(stack_index)) = (id.data_type, id.stack_index) else {
                    Err(ZeptwoError::compiler_error_at_line(
                        lookup.var_pos(*var_id),
                        format!("Could not access identifier '{}'.", id.name),
                    ))?
                };
                bytecode.set_variable(
                    stack_index,
                    data_type.get_result_size(lookup),
                    lookup.var_pos(*var_id),
                )?;
            }
            Expr::Binary { lhs, op, rhs, .. } => {
                lhs.compile(bytecode, lookup)?;
                rhs.compile(bytecode, lookup)?;
                if let OpCodeOp::OpCode(o) = op {
                    bytecode.add_opcode(*o, lhs.right_pos(lookup));
                } else {
                    Err(ZeptwoError::compiler_error_at_line(
                        lhs.right_pos(lookup),
                        "Operator was not converted into opcode.",
                    ))?
                }
            }
            Expr::Unary { op, node, .. } => {
                node.compile(bytecode, lookup)?;
                if let OpCodeOp::OpCode(o) = op {
                    bytecode.add_opcode(*o, node.left_pos(lookup));
                } else {
                    Err(ZeptwoError::compiler_error_at_line(
                        node.left_pos(lookup),
                        "Operator was not converted into opcode.",
                    ))?
                }
            }
            Expr::Value { value, pos: line } => {
                bytecode.write_constant(value.clone(), *line)?;
            }
            Expr::Variable { id, pos: line } => {
                let id = id.get_variable_info(lookup);
                let (Some(data_type), Some(stack_index)) = (id.data_type, id.stack_index) else {
                    Err(ZeptwoError::compiler_error_at_line(
                        *line,
                        format!("Could not access identifier '{}'", id.name),
                    ))?
                };
                bytecode.get_variable(stack_index, data_type.get_result_size(lookup), *line)?;
            }
            Expr::Nothing => {
                println!("Compiling nothing expr, doing nothing.")
            }
        }
        Ok(())
    }
}
