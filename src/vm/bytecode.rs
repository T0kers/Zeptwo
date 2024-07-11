use std::collections::VecDeque;

use crate::{errors::{echo, ZeptwoError}, parser::ast::expr::Val, position::Pos};
use strum_macros::{Display, EnumDiscriminants, FromRepr};

#[derive(Debug, EnumDiscriminants, Copy, Clone, Display)]
#[strum_discriminants(name(OpCode), derive(FromRepr, Display), repr(u8))]
pub enum FullOpCode {
    ConstByte {
        byte: u8,
    },
    ConstWord {
        const_index: u16,
    },
    UsubInt,
    UsubFLt,
    BitnotInt,
    NotBool,
    AddInt,
    AddFlt,
    SubInt,
    SubFlt,
    MulInt,
    MulFlt,
    DivInt,
    DivFlt,
    EqWord,
    EqBool,
    RemInt,
    GetLocal {
        relative_index: u16,
        size: u16,
    },
    SetLocal {
        relative_index: u16,
        size: u16,
    },
    GetGlobal {
        relative_index: u16,
        size: u16,
    },
    SetGlobal {
        relative_index: u16,
        size: u16,
    },
    Jump {
        offset: u16,
    },
    PopJumpIfFalse {
        offset: u16,
    },
    Loop {
        offset: u16,
    },

    CallFn {
        stack_offset_offset: u16,
        address: u32,
    },
    Return,
    Pop {
        amount: u16,
    },
    ValuePop {
        data_size: u16,
        amount: u16,
    },
    Eof,
}

impl FullOpCode {
    fn to_u8_slice(self) -> Box<[u8]> {
        let mut result: Vec<u8> = vec![std::convert::Into::<OpCode>::into(self).into()];
        match self {
            FullOpCode::ConstByte { byte } => result.push(byte),
            FullOpCode::ConstWord { const_index } => {
                result.extend_from_slice(&const_index.to_ne_bytes())
            }
            FullOpCode::UsubInt
            | FullOpCode::UsubFLt
            | FullOpCode::BitnotInt
            | FullOpCode::NotBool
            | FullOpCode::AddInt
            | FullOpCode::AddFlt
            | FullOpCode::SubInt
            | FullOpCode::SubFlt
            | FullOpCode::MulInt
            | FullOpCode::MulFlt
            | FullOpCode::DivInt
            | FullOpCode::DivFlt
            | FullOpCode::EqWord
            | FullOpCode::EqBool
            | FullOpCode::RemInt => {}
            FullOpCode::GetLocal {
                relative_index,
                size,
            } => {
                result.extend_from_slice(&relative_index.to_ne_bytes());
                result.extend_from_slice(&size.to_ne_bytes());
            }
            FullOpCode::SetLocal {
                relative_index,
                size,
            } => {
                result.extend_from_slice(&relative_index.to_ne_bytes());
                result.extend_from_slice(&size.to_ne_bytes());
            }
            FullOpCode::GetGlobal {
                relative_index,
                size,
            } => {
                result.extend_from_slice(&relative_index.to_ne_bytes());
                result.extend_from_slice(&size.to_ne_bytes());
            }
            FullOpCode::SetGlobal {
                relative_index,
                size,
            } => {
                result.extend_from_slice(&relative_index.to_ne_bytes());
                result.extend_from_slice(&size.to_ne_bytes());
            }
            FullOpCode::Jump { offset } => {
                result.extend_from_slice(&offset.to_ne_bytes());
            }
            FullOpCode::PopJumpIfFalse { offset } => {
                result.extend_from_slice(&offset.to_ne_bytes());
            }
            FullOpCode::Loop { offset } => result.extend_from_slice(&offset.to_ne_bytes()),
            FullOpCode::CallFn {
                stack_offset_offset,
                address,
            } => {
                result.extend_from_slice(&stack_offset_offset.to_ne_bytes());
                result.extend_from_slice(&address.to_ne_bytes());
            }
            FullOpCode::Return => {}
            FullOpCode::Pop { amount } => result.extend_from_slice(&amount.to_ne_bytes()),
            FullOpCode::ValuePop { data_size, amount } => {
                result.extend_from_slice(&data_size.to_ne_bytes());
                result.extend_from_slice(&amount.to_ne_bytes());
            }
            FullOpCode::Eof => {}
        }
        result.into_boxed_slice()
    }
}

impl std::convert::Into<u8> for OpCode {
    fn into(self) -> u8 {
        self as u8
    }
}

#[derive(Clone, Copy)]
struct RuntimePosEncoding {
    amount: u32,
    pos: Pos,
}

pub struct JumpInfo {
    opcode_start: usize,
    jump_start: usize,
}

pub struct BytecodeConstructer {
    assembler: BytecodeAssembler,
    pub has_main_compiled: bool,
}

impl BytecodeConstructer {
    pub fn new() -> Self {
        Self {
            assembler: BytecodeAssembler::new(),
            has_main_compiled: false,
        }
    }
    pub fn finish(&mut self) -> Bytecode {
        self.assembler.finish()
    }
    pub fn add_opcode(&mut self, opcode: FullOpCode, pos: Pos) {
        self.assembler.add_opcode(opcode, pos);
    }
    pub fn add_opcode_same_pos(&mut self, opcode: FullOpCode) {
        self.add_opcode(opcode, self.assembler.last_pos());
    }

    pub fn write_constant(&mut self, value: Val, pos: Pos) -> Result<(), ZeptwoError> {
        let value_slice = value.to_u8_slice();
        match value_slice.len() {
            0 => Ok(()),
            1 => {
                self.add_opcode(
                    FullOpCode::ConstByte {
                        byte: value_slice[0],
                    },
                    pos,
                );
                Ok(())
            }
            8 => {
                let const_index = self.assembler.add_constant(value_slice)?;
                self.add_opcode(FullOpCode::ConstWord { const_index }, pos);
                Ok(())
            }
            _ => todo!(),
        }
    }
    pub fn pop_usize(&mut self, mut amount: usize, pos: Pos) {
        // TODO: add pop that supports larger argument, or compile into a loop.
        while amount > u16::MAX as usize {
            self.add_opcode(FullOpCode::Pop { amount: u16::MAX }, pos);
            amount -= u16::MAX as usize;
        }
        if amount != 0 {
            self.add_opcode(
                FullOpCode::Pop {
                    amount: amount as u16,
                },
                pos,
            );
        }
    }
    pub fn value_pop_usize_same_line(&mut self, data_size: u16, mut amount: usize) {
        while amount > u16::MAX as usize {
            // TODO: add pop that supports larger argument, or compile into a loop.
            self.add_opcode_same_pos(FullOpCode::ValuePop {
                data_size,
                amount: u16::MAX,
            });
            amount -= u16::MAX as usize;
        }
        if amount != 0 {
            self.add_opcode_same_pos(FullOpCode::ValuePop {
                data_size,
                amount: amount as u16,
            });
        }
    }
    pub fn pop_usize_same_line(&mut self, mut amount: usize) {
        while amount > u16::MAX as usize {
            // TODO: add pop that supports larger argument, or compile into a loop.
            self.add_opcode_same_pos(FullOpCode::Pop { amount: u16::MAX });
            amount -= u16::MAX as usize;
        }
        if amount != 0 {
            self.add_opcode_same_pos(FullOpCode::Pop {
                amount: amount as u16,
            });
        }
    }
    pub fn get_variable(
        &mut self,
        stack_index: usize,
        size: u16,
        is_global: bool,
        pos: Pos,
    ) -> Result<(), ZeptwoError> {
        if stack_index > u16::MAX as usize {
            Err(ZeptwoError::compiler_error_at_pos(
                pos,
                "Stack is too large. Can't access variable.",
            ))?;
        }
        if is_global {
            self.assembler.add_opcode(
                FullOpCode::GetGlobal {
                    relative_index: stack_index.try_into().unwrap(),
                    size,
                },
                pos,
            );
        }
        else {
            self.assembler.add_opcode(
                FullOpCode::GetLocal {
                    relative_index: stack_index.try_into().unwrap(),
                    size,
                },
                pos,
            );
        }
        Ok(())
    }
    pub fn set_variable(
        &mut self,
        stack_index: usize,
        size: u16,
        is_global: bool,
        pos: Pos,
    ) -> Result<(), ZeptwoError> {
        if stack_index > u16::MAX as usize {
            Err(ZeptwoError::compiler_error_at_pos(
                pos,
                "Stack is too large. Can't access variable.",
            ))?;
        }
        if is_global {
            self.assembler.add_opcode(
                FullOpCode::SetGlobal {
                    relative_index: stack_index.try_into().unwrap(),
                    size,
                },
                pos,
            );
        }
        else {
            self.assembler.add_opcode(
                FullOpCode::SetLocal {
                    relative_index: stack_index.try_into().unwrap(),
                    size,
                },
                pos,
            );
        }
        
        Ok(())
    }
    pub fn emit_main_call(&mut self) -> usize {
        let opcode_start = self.next_instruction_index();
        self.assembler.add_opcode(
            FullOpCode::CallFn {
                stack_offset_offset: 0,
                address: 0xFFFF,
            },
            Pos::new(0),
        );
        opcode_start
    }
    pub fn patch_main_call(&mut self, call_index: usize, addr: usize) -> Result<(), ZeptwoError> {
        let addr = {
            if addr > u32::MAX as usize {
                Err(ZeptwoError::compiler_error_at_pos(
                    Pos::new(0),
                    "Can't call main function.",
                ))?
            } else {
                addr as u32
            }
        };
        let mut instruction = self.assembler.get_instruction_at_byte_index(call_index);
        match &mut instruction.instruction {
            FullOpCode::CallFn {
                address, ..
            } => *address = addr,
            _ => unreachable!(),
        }
        self.assembler.update_bytecode(instruction)?;
        self.has_main_compiled = true;
        Ok(())
    }
    
    pub fn emit_jump(&mut self, instruction: FullOpCode) -> JumpInfo {
        let opcode_start = self.next_instruction_index();
        self.add_opcode_same_pos(instruction);
        let jump_start = self.next_instruction_index();
        JumpInfo { opcode_start, jump_start }
    }
    pub fn patch_jump(&mut self, jump_info: JumpInfo) -> Result<(), ZeptwoError> {
        let jump_size = self.next_instruction_index() - jump_info.jump_start;

        if jump_size > u16::MAX as usize {
            Err(ZeptwoError::compiler_error_at_pos(
                self.assembler.last_pos(),
                "Branch is too big to jump over.",
            ))?;
        }
        let mut instruction = self.assembler.get_instruction_at_byte_index(jump_info.opcode_start);

        match &mut instruction.instruction {
            FullOpCode::Jump { offset } => *offset = jump_size as u16,
            FullOpCode::PopJumpIfFalse { offset } => *offset = jump_size as u16,
            _ => unreachable!(),
        }
        self.assembler.update_bytecode(instruction)?;
        Ok(())
    }
    pub fn emit_loop(&mut self, loop_start: usize) -> Result<(), ZeptwoError> {
        let opcode_start = self.next_instruction_index();
        self.add_opcode_same_pos(FullOpCode::Loop { offset: 0xFFFF });

        let jump_offset = self.next_instruction_index() - loop_start;
        if jump_offset > u16::MAX as usize {
            Err(ZeptwoError::compiler_error_at_pos(
                self.assembler.last_pos(),
                "Loop body is too large.",
            ))?;
        }
        let mut instruction = self
            .assembler
            .get_instruction_at_byte_index(opcode_start);
        match &mut instruction.instruction {
            FullOpCode::Loop { offset } => *offset = jump_offset as u16,
            _ => unreachable!(),
        }
        self.assembler.update_bytecode(instruction)?;

        Ok(())
    }
    pub fn current_instruction_index(&mut self) -> usize {
        self.assembler.current_instruction_index()
    }
    pub fn next_instruction_index(&mut self) -> usize {
        self.assembler.next_instruction_index()
    }
}

impl Default for BytecodeConstructer {
    fn default() -> Self {
        BytecodeConstructer::new()
    }
}

struct BytecodeAssembler {
    opcode_queue: VecDeque<FullOpCode>,
    bytecode: Bytecode,
}

impl BytecodeAssembler {
    pub fn new() -> Self {
        Self {
            opcode_queue: VecDeque::new(),
            bytecode: Bytecode::new(),
        }
    }
    pub fn finish(&mut self) -> Bytecode {
        self.push_queue();
        self.bytecode.clone()
    }
    pub fn add_opcode(&mut self, opcode: FullOpCode, pos: Pos) {
        self.bytecode.code.extend_from_slice(&opcode.to_u8_slice());
        self.add_pos(pos);
    }
    fn push_queue(&mut self) {
        while let Some(op) = self.opcode_queue.pop_front() {
            self.bytecode.code.extend_from_slice(&op.to_u8_slice())
        }
    }
    fn add_constant(&mut self, value: Box<[u8]>) -> Result<u16, ZeptwoError> {
        let start: u16 = self.bytecode.constants.len() as u16;
        self.bytecode.constants.extend_from_slice(&value);
        if self.bytecode.constants.len() < u16::MAX as usize {
            Ok(start)
        } else {
            Err(ZeptwoError::compile_error("Too many constants defined."))
        }
    }
    pub fn current_instruction_index(&mut self) -> usize {
        self.push_queue();
        self.bytecode.code.len() - 1
    }
    pub fn next_instruction_index(&mut self) -> usize {
        self.push_queue();
        self.bytecode.code.len()
    }
    pub fn last_pos(&self) -> Pos {
        self.bytecode.poses.last().unwrap().pos
    }
    pub fn add_pos(&mut self, pos: Pos) {
        let len = self.bytecode.poses.len();
        if len > 0 {
            let last = &mut self.bytecode.poses[len - 1];
            if last.pos == pos {
                last.amount += 1;
                return;
            }
        }
        self.bytecode
            .poses
            .push(RuntimePosEncoding { amount: 1, pos });
    }
    pub fn get_instruction_at_byte_index(&mut self, mut index: usize) -> BytecodeInstruction {
        self.push_queue();
        BytecodeInstruction {
            index,
            instruction: self.bytecode.read_full_opcode(&mut index),
        }
    }
    pub fn update_bytecode(&mut self, instruction: BytecodeInstruction) -> Result<(), ZeptwoError> {
        assert_eq!(
            OpCode::from_repr(self.bytecode.code[instruction.index]).unwrap(),
            instruction.instruction.into()
        );
        let vec = instruction.instruction.to_u8_slice().to_vec();
        self.bytecode.code[instruction.index..instruction.index + vec.len()].copy_from_slice(vec.as_slice());
        Ok(())
    }
}

pub struct BytecodeInstruction {
    index: usize,
    pub instruction: FullOpCode,
}

#[derive(Clone)]
pub struct Bytecode {
    pub code: Vec<u8>,
    pub constants: Vec<u8>,
    poses: Vec<RuntimePosEncoding>,
}

impl Bytecode {
    pub fn new() -> Self {
        Bytecode {
            code: vec![],
            constants: vec![],
            poses: vec![],
        }
    }
    pub fn pos(&self, i: usize) -> Pos {
        let mut current_line: u32 = 0;
        for pos in &self.poses {
            current_line += pos.amount;
            if current_line as usize > i {
                return pos.pos;
            }
        }
        Pos::new(0)
    }
    pub fn read_full_opcode(&self, index: &mut usize) -> FullOpCode {
        *index += 1;
        echo(match OpCode::from_repr(self.code[*index - 1]).unwrap() {
            OpCode::ConstByte => FullOpCode::ConstByte {
                byte: self.read_u8(index),
            },
            OpCode::ConstWord => FullOpCode::ConstWord {
                const_index: self.read_u16(index),
            },
            OpCode::UsubInt => FullOpCode::UsubInt,
            OpCode::UsubFLt => FullOpCode::UsubFLt,
            OpCode::BitnotInt => FullOpCode::BitnotInt,
            OpCode::NotBool => FullOpCode::NotBool,
            OpCode::AddInt => FullOpCode::AddInt,
            OpCode::AddFlt => FullOpCode::AddFlt,
            OpCode::SubInt => FullOpCode::SubInt,
            OpCode::SubFlt => FullOpCode::SubFlt,
            OpCode::MulInt => FullOpCode::MulInt,
            OpCode::MulFlt => FullOpCode::MulFlt,
            OpCode::DivInt => FullOpCode::DivInt,
            OpCode::DivFlt => FullOpCode::DivFlt,
            OpCode::EqWord => FullOpCode::EqWord,
            OpCode::EqBool => FullOpCode::EqBool,
            OpCode::RemInt => FullOpCode::RemInt,
            OpCode::GetLocal => FullOpCode::GetLocal {
                relative_index: self.read_u16(index),
                size: self.read_u16(index),
            },
            OpCode::SetLocal => FullOpCode::GetLocal {
                relative_index: self.read_u16(index),
                size: self.read_u16(index),
            },
            OpCode::GetGlobal => FullOpCode::GetLocal {
                relative_index: self.read_u16(index),
                size: self.read_u16(index),
            },
            OpCode::SetGlobal => FullOpCode::GetLocal {
                relative_index: self.read_u16(index),
                size: self.read_u16(index),
            },
            OpCode::Jump => FullOpCode::Jump { offset: self.read_u16(index) },
            OpCode::PopJumpIfFalse => FullOpCode::PopJumpIfFalse { offset: self.read_u16(index) },
            OpCode::Loop => FullOpCode::Loop { offset: self.read_u16(index) },
            OpCode::CallFn => FullOpCode::CallFn { stack_offset_offset: self.read_u16(index), address: self.read_u32(index) },
            OpCode::Return => FullOpCode::Return,
            OpCode::Pop => FullOpCode::Pop { amount: self.read_u16(index) },
            OpCode::ValuePop => FullOpCode::ValuePop { data_size: self.read_u16(index), amount: self.read_u16(index) },
            OpCode::Eof => FullOpCode::Eof,
        })
    }
    pub fn read_u8(&self, index: &mut usize) -> u8 {
        *index += 1;
        self.code[*index - 1]
    }
    pub fn read_u16(&self, index: &mut usize) -> u16 {
        *index += 2;
        u16::from_ne_bytes(self.code[*index - 2..*index].try_into().unwrap())
    }
    pub fn read_u32(&self, index: &mut usize) -> u32 {
        *index += 4;
        u32::from_ne_bytes(self.code[*index - 4..*index].try_into().unwrap())
    }
    pub fn disassemble(&self) {
        println!("===== Bytecode =====");

        let mut i = 0;
        while i < self.code.len() {
            self.disassemble_instruction(&mut i);
        }

        println!("====================");
    }
    pub fn disassemble_instruction(&self, i: &mut usize) { // TODO: use read_full_opcode to dissasemble.
        print!("{:04} ", i);
        if *i > 0 && self.pos(*i) == self.pos(*i - 1) {
            print!("   | ");
        } else {
            print!("{:4} ", self.pos(*i));
        }
        match OpCode::from_repr(self.code[*i]).unwrap() {
            OpCode::ConstByte => self.u8_instruction("CONST_BYTE", i),
            OpCode::ConstWord => self.const_word_instruction("CONST_WORD", i),
            OpCode::UsubInt => Bytecode::simple_instruction("USUB_INT", i),
            OpCode::UsubFLt => Bytecode::simple_instruction("USUB_FLT", i),
            OpCode::BitnotInt => Bytecode::simple_instruction("BITNOT_INT", i),
            OpCode::NotBool => Bytecode::simple_instruction("NOT_BOOL", i),
            OpCode::AddInt => Bytecode::simple_instruction("ADD_INT", i),
            OpCode::AddFlt => Bytecode::simple_instruction("ADD_FLT", i),
            OpCode::SubInt => Bytecode::simple_instruction("SUB_INT", i),
            OpCode::SubFlt => Bytecode::simple_instruction("SUB_FLT", i),
            OpCode::MulInt => Bytecode::simple_instruction("MUL_INT", i),
            OpCode::MulFlt => Bytecode::simple_instruction("MUL_FLT", i),
            OpCode::DivInt => Bytecode::simple_instruction("DIV_INT", i),
            OpCode::DivFlt => Bytecode::simple_instruction("DIV_FLT", i),
            OpCode::RemInt => Bytecode::simple_instruction("REM_INT", i),
            OpCode::EqWord => Bytecode::simple_instruction("EQ_WORD", i),
            OpCode::EqBool => Bytecode::simple_instruction("EQ_BOOL", i),
            OpCode::GetLocal => self.u16_u16_instruction("GET_LOCAL", i),
            OpCode::SetLocal => self.u16_u16_instruction("SET_LOCAL", i),
            OpCode::GetGlobal => self.u16_u16_instruction("GET_GLOBAL", i),
            OpCode::SetGlobal => self.u16_u16_instruction("SET_GLOBAL", i),
            OpCode::PopJumpIfFalse => self.u16_instruction("POP_JUMP_IF_FALSE", i),
            OpCode::Jump => self.u16_instruction("JUMP", i),
            OpCode::Loop => self.u16_instruction("LOOP", i),
            OpCode::CallFn => self.u16_u32_instruction("CALL_FN", i),
            OpCode::Return => Bytecode::simple_instruction("RETURN", i),
            OpCode::Pop => self.u16_instruction("POP", i),
            OpCode::ValuePop => self.u16_u16_instruction("VALUE_POP", i),
            OpCode::Eof => Bytecode::simple_instruction("EOF", i),
        }
    }
    fn simple_instruction(name: &str, i: &mut usize) {
        println!("{}", name);
        *i += 1;
    }
    fn u8_instruction(&self, name: &str, i: &mut usize) {
        println!("{} {}", name, self.code[*i + 1]);
        *i += 2;
    }
    fn u16_instruction(&self, name: &str, i: &mut usize) {
        println!("{} {}", name, self.code[*i + 2] + self.code[*i + 1]);
        *i += 3;
    }
    fn u16_u16_instruction(&self, name: &str, i: &mut usize) {
        println!(
            "{} {} {}",
            name,
            ((self.code[*i + 2] as usize) << 8) | (self.code[*i + 1] as usize),
            ((self.code[*i + 4] as usize) << 8) | (self.code[*i + 3] as usize)
        );
        *i += 5;
    }
    fn u16_u32_instruction(&self, name: &str, i: &mut usize) {
        println!(
            "{} {} {}",
            name,
            ((self.code[*i + 2] as usize) << 8) | (self.code[*i + 1]) as usize,
            ((self.code[*i + 6] as usize) << 24)
                | ((self.code[*i + 5] as usize) << 16 | (self.code[*i + 4] as usize) << 8)
                | (self.code[*i + 3] as usize),
        );
        *i += 7;
    }
    fn const_word_instruction(&self, name: &str, i: &mut usize) {
        let index: usize = ((self.code[*i + 2] as usize) << 8) | (self.code[*i + 1] as usize);
        println!("{} {} {}", name, index, self.constants[index]);
        *i += 3;
    }
}

impl Default for Bytecode {
    fn default() -> Self {
        Bytecode::new()
    }
}
