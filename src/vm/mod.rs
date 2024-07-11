pub mod bytecode;

use self::bytecode::OpCode;
use bytecode::Bytecode;

use super::{errors::ZeptwoError, vmtypes::*};

#[derive(Debug, Clone, Copy)]
struct StackFrame {
    return_address: usize,
    stack_offset: usize,
}

pub struct VM {
    program: Bytecode,
    ip: usize,
    stack: [u8; VM::STACK_SIZE],
    stack_top: usize,
    call_stack: [StackFrame; VM::CALL_STACK_SIZE],
    call_stack_top: usize,
}

impl VM {
    const STACK_SIZE: usize = 256 * 16;
    const CALL_STACK_SIZE: usize = 64;
    pub fn new(program: Bytecode) -> Self {
        Self {
            program,
            ip: 0,
            stack: [0x00; VM::STACK_SIZE],
            stack_top: 0,
            call_stack: [StackFrame {
                return_address: 0,
                stack_offset: 0,
            }; VM::CALL_STACK_SIZE],
            call_stack_top: 0,
        }
    }
    pub fn run(&mut self) -> Result<(), ZeptwoError> {
        macro_rules! unary_operation {
            ($size:ident, $kind:ident, $op:tt) => {
                let a = $kind::from_ne_bytes(self.pop($size));
                self.push(($op a).to_ne_bytes());
            };
        }
        macro_rules! binary_operation {
            ($size:ident, $kind:ident, $op:tt) => {
                let b = $kind::from_ne_bytes(self.pop($size));
                let a = $kind::from_ne_bytes(self.pop($size));
                self.push((a $op b).to_ne_bytes());
            };
        }
        loop {
            println!("{:?}", &self.stack[0..self.stack_top]);
            let mut dissassemble_ip = self.ip;
            self.program.disassemble_instruction(&mut dissassemble_ip);
            match OpCode::from_repr(self.read_byte()).unwrap() {
                OpCode::ConstByte => {
                    let byte = self.read_byte();
                    self.push_byte(byte);
                }
                OpCode::ConstWord => {
                    let start = self.read_u16() as usize;
                    self.push_word(start);
                }
                OpCode::UsubInt => {
                    unary_operation!(VMINT_SIZE, VMInt, -);
                }
                OpCode::UsubFLt => {
                    unary_operation!(VMFLT_SIZE, VMFlt, -);
                }
                OpCode::BitnotInt => {
                    //TODO: FIX SAME OPCODe
                    unary_operation!(VMINT_SIZE, VMInt, !);
                }
                OpCode::NotBool => {
                    let a = self.pop_byte() != 0;
                    self.push_byte((!a) as u8);
                }
                OpCode::AddInt => {
                    binary_operation!(VMINT_SIZE, VMInt, +);
                }
                OpCode::SubInt => {
                    binary_operation!(VMINT_SIZE, VMInt, -);
                }
                OpCode::MulInt => {
                    binary_operation!(VMINT_SIZE, VMInt, *);
                }
                OpCode::DivInt => {
                    binary_operation!(VMINT_SIZE, VMInt, /);
                }
                OpCode::RemInt => {
                    binary_operation!(VMINT_SIZE, VMInt, %);
                }
                OpCode::AddFlt => {
                    binary_operation!(VMFLT_SIZE, VMFlt, +);
                }
                OpCode::SubFlt => {
                    binary_operation!(VMFLT_SIZE, VMFlt, -);
                }
                OpCode::MulFlt => {
                    binary_operation!(VMFLT_SIZE, VMFlt, *);
                }
                OpCode::DivFlt => {
                    binary_operation!(VMFLT_SIZE, VMFlt, /);
                }
                OpCode::EqWord => {
                    let b = VMInt::from_ne_bytes(self.pop_word());
                    let a = VMInt::from_ne_bytes(self.pop_word());
                    self.push_byte((a == b) as u8);
                }
                OpCode::EqBool => {
                    let b = self.pop_byte();
                    let a = self.pop_byte();
                    self.push_byte((a == b) as u8)
                }
                OpCode::GetLocal => {
                    let stack_index = (self.read_u16() as usize) + self.stack_offset();
                    let size = self.read_u16() as usize;
                    let var = self.stack[stack_index..stack_index + size].to_vec();
                    self.push_ref(&var);
                }
                OpCode::SetLocal => {
                    let stack_index = self.read_u16() as usize + self.stack_offset();
                    let size = self.read_u16() as usize;

                    let data = self.stack[self.stack_top - size..self.stack_top].to_vec();
                    self.stack[stack_index..stack_index + size].copy_from_slice(data.as_slice());
                }
                OpCode::GetGlobal => {
                    let stack_index = self.read_u16() as usize;
                    let size = self.read_u16() as usize;
                    let var = self.stack[stack_index..stack_index + size].to_vec();
                    self.push_ref(&var);
                }
                OpCode::SetGlobal => {
                    let stack_index = self.read_u16() as usize;
                    let size = self.read_u16() as usize;

                    let data = self.stack[self.stack_top - size..self.stack_top].to_vec();
                    self.stack[stack_index..stack_index + size].copy_from_slice(data.as_slice());
                }
                OpCode::Jump => {
                    let offset = self.read_u16() as usize;
                    self.ip += offset;
                }
                OpCode::PopJumpIfFalse => {
                    let offset = self.read_u16() as usize;
                    let byte = self.pop_byte();
                    println!("{}", byte);
                    if byte == 0 {
                        self.ip += offset;
                    }
                }
                OpCode::Loop => {
                    let offset = self.read_u16() as usize;
                    self.ip -= offset;
                }
                OpCode::CallFn => {
                    let stack_offset_offset = self.read_u16() as usize;
                    let address = self.read_u32() as usize;
                    match self.call_stack.get_mut(self.call_stack_top) {
                        Some(frame) => *frame = StackFrame {
                            return_address: self.ip,
                            stack_offset: self.stack_top - stack_offset_offset,
                        },
                        None => Err(ZeptwoError::runtime_error("Call stack overflow."))?,
                    }
                    self.call_stack_top += 1;
                    self.ip = address;
                }
                OpCode::Return => {
                    // TODO: make it work for early returns
                    self.call_stack_top -= 1;
                    let stack_frame = self.call_stack[self.call_stack_top];
                    self.ip = stack_frame.return_address;
                    //self.stack_top = stack_frame.stack_offset; FIXME.
                }
                OpCode::Pop => {
                    let amount = self.read_u16() as usize;
                    self.stack_top -= amount;
                }
                OpCode::ValuePop => {
                    let data_size = self.read_u16() as usize;
                    let amount = self.read_u16() as usize;

                    let data = self.pop_ref(data_size).to_vec();

                    self.stack_top -= amount;

                    self.push_ref(&data);
                }
                OpCode::Eof => {
                    let result = VMInt::from_ne_bytes(self.pop(VMINT_SIZE));
                    println!("{result}");
                    return Ok(());
                }
            }
        }
    }
    fn stack_offset(&self) -> usize {
        self.call_stack[self.call_stack_top - 1].stack_offset
    }
    fn push<const N: usize>(&mut self, bytes: [u8; N]) {
        self.stack[self.stack_top..self.stack_top + bytes.len()].copy_from_slice(&bytes);
        self.stack_top += bytes.len();
    }
    fn push_ref(&mut self, bytes: &[u8]) {
        self.stack[self.stack_top..self.stack_top + bytes.len()].copy_from_slice(bytes);
        self.stack_top += bytes.len();
    }
    fn push_byte(&mut self, byte: u8) {
        self.stack[self.stack_top] = byte;
        self.stack_top += 1;
    }
    fn push_word(&mut self, start: usize) {
        let slice = &self.program.constants[start..start + VMWORD_SIZE];
        self.stack[self.stack_top..self.stack_top + VMWORD_SIZE].copy_from_slice(slice);
        self.stack_top += VMWORD_SIZE;
    }
    fn pop<const N: usize>(&mut self, amount: usize) -> [u8; N] {
        let bytes = &self.stack[self.stack_top - amount..self.stack_top];
        self.stack_top -= amount;
        bytes.try_into().unwrap()
    }
    fn pop_ref(&mut self, amount: usize) -> &[u8] {
        let bytes = &self.stack[self.stack_top - amount..self.stack_top];
        self.stack_top -= amount;
        bytes
    }
    fn pop_word(&mut self) -> [u8; VMWORD_SIZE] {
        self.pop(VMWORD_SIZE)
    }
    fn pop_byte(&mut self) -> u8 {
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }
    fn read_byte(&mut self) -> u8 {
        let byte = self.program.code[self.ip];
        self.ip += 1;
        byte
    }
    fn read_u16(&mut self) -> u16 {
        let bytes = self.read_byte() as u16;
        bytes | ((self.read_byte() as u16) << 8)
    }
    fn read_u32(&mut self) -> u32 {
        let bytes = self.read_u16() as u32;
        bytes | ((self.read_u16() as u32) << 16)
    }

    pub fn runtime_error(&mut self, msg: &str) -> Result<(), ZeptwoError> {
        Err(ZeptwoError::runtime_error(format!(
            "{} [Line {}] in script.",
            msg,
            self.program.pos(self.ip - 1)
        )))
    }
}
