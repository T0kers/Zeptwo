
use super::identifiers::{DependencyID, FunctionID, IdentifierID, IdentifierLookup, VariableID};
use super::{identifiers::AmbiguousFunctionID, stmt::*};
use crate::vm::bytecode::FullOpCode;
use crate::{
    position::{Pos, Position},
    vmtypes::*,
    ZeptwoError,
};
use crate::{
    parser::SizeAndIndex,
    scanner::{Token, TokenKind},
};
use std::fmt;

pub trait ValTypeTrait {
    fn get_type(&self, lookup: &IdentifierLookup) -> Option<ValType>;
    fn determine_type_and_opcode(
        &mut self,
        lookup: &mut IdentifierLookup,
        dependencies: &mut Vec<DependencyID>
    ) -> Result<ValType, ZeptwoError>;
    fn get_result_size(&self, lookup: &IdentifierLookup) -> u16;
}

impl std::str::FromStr for ValType {
    type Err = ZeptwoError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(
            match s {
                "String" => ValType::Str,
                "Int" => ValType::Int,
                "Float" => ValType::Flt,
                "Bool" => ValType::Bool,
                "Nul" => ValType::Nul,
                _ => panic!(),
            }
        )
    }
}

impl fmt::Display for ValType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Func {
                params,
                return_type,
                ..
            } => write!(f, "fn ({}) -> {}", "fix", "this"), // TODO fix this
            Self::Str => write!(f, "String"),
            Self::Int => write!(f, "Int"),
            Self::Flt => write!(f, "Float"),
            Self::Bool => write!(f, "Bool"),
            Self::Nul => write!(f, "Nul"),
        }
    }
}

impl ValTypeTrait for ValType {
    fn get_type(&self, lookup: &IdentifierLookup) -> Option<ValType> {
        Some(self.clone())
    }
    fn determine_type_and_opcode(
        &mut self,
        lookup: &mut IdentifierLookup,
        _: &mut Vec<DependencyID>,
    ) -> Result<ValType, ZeptwoError> {
        Ok(self.get_type(lookup).unwrap())
    }
    fn get_result_size(&self, lookup: &IdentifierLookup) -> u16 {
        match self {
            ValType::Func { func_type, .. } => match func_type {
                FuncKind::Native => Func::Native { index: 0 },
                FuncKind::SingeOpCode => Func::SingeOpCode { opcode: 0 },
                FuncKind::ByteCode => Func::Bytecode { index: 0 },
            }
            .to_u8_slice()
            .len(),
            ValType::Str => VMSTRING_SIZE,
            ValType::Int => VMINT_SIZE,
            ValType::Flt => VMFLT_SIZE,
            ValType::Bool => VMBOOL_SIZE,
            ValType::Nul => VMNUL_SIZE,
        }
        .try_into()
        .unwrap()
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum ValType {
    Func {
        func_type: FuncKind,
        params: Vec<ValType>,
        return_type: Box<ValType>,
    },
    Str,
    Int,
    Flt,
    Bool,
    Nul,
}

impl ValType {
    fn from_native_index(index: u8) -> Self {
        todo!()
    }
    fn from_single_opcode(opcode: u8) -> Self {
        todo!()
    }
    fn from_bytecode_index(index: usize) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FuncKind {
    Native,
    SingeOpCode,
    ByteCode,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Func {
    Native {
        index: u8, // TODO: try changing into a function pointer
    },
    SingeOpCode {
        opcode: u8,
    },
    Bytecode {
        index: usize,
    },
}

impl Func {
    pub fn to_u8_slice(self) -> Box<[u8]> {
        match self {
            Func::Native { index } => Box::new(index.to_ne_bytes()),
            Func::SingeOpCode { opcode } => Box::new(opcode.to_ne_bytes()),
            Func::Bytecode { index } => Box::new(index.to_ne_bytes()),
        }
    }
}

impl ValTypeTrait for Func {
    fn get_type(&self, lookup: &IdentifierLookup) -> Option<ValType> {
        todo!()
    }
    fn determine_type_and_opcode(
        &mut self,
        lookup: &mut IdentifierLookup,
        dependencies: &mut Vec<DependencyID>,
    ) -> Result<ValType, ZeptwoError> {
        Ok(self.get_type(lookup).unwrap())
    }
    fn get_result_size(&self, lookup: &IdentifierLookup) -> u16 {
        self.get_type(lookup).unwrap().get_result_size(lookup)
    }
}

#[derive(Debug, Clone)]
pub enum Val {
    Func(Func),
    Str(VMString),
    Int(VMInt),
    Flt(VMFlt),
    Bool(VMBool),
    Nul,
}

impl Val {
    pub fn to_u8_slice(&self) -> Box<[u8]> {
        match self {
            Val::Func(f) => f.to_u8_slice(),
            Val::Str(s) => s.as_bytes().to_owned().into_boxed_slice(),
            Val::Int(i) => Box::new(i.to_ne_bytes()),
            Val::Flt(f) => Box::new(f.to_ne_bytes()),
            Val::Bool(b) => vec![*b as u8].into_boxed_slice(),
            Val::Nul => vec![].into_boxed_slice(),
        }
    }
}

impl ValTypeTrait for Val {
    fn get_type(&self, lookup: &IdentifierLookup) -> Option<ValType> {
        match self {
            Val::Func(f) => f.get_type(lookup),
            Val::Str(_) => Some(ValType::Str),
            Val::Int(_) => Some(ValType::Int),
            Val::Flt(_) => Some(ValType::Flt),
            Val::Bool(_) => Some(ValType::Bool),
            Val::Nul => Some(ValType::Nul),
        }
    }
    fn determine_type_and_opcode(
        &mut self,
        lookup: &mut IdentifierLookup,
        dependencies: &mut Vec<DependencyID>,
    ) -> Result<ValType, ZeptwoError> {
        Ok(self.get_type(lookup).unwrap())
    }
    fn get_result_size(&self, lookup: &IdentifierLookup) -> u16 {
        self.get_type(lookup).unwrap().get_result_size(lookup)
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Val::Func(f) => todo!(),
                Val::Str(s) => s.as_str().to_string(),
                Val::Int(i) => i.to_string(),
                Val::Flt(f) => f.to_string(),
                Val::Bool(b) => b.to_string(),
                Val::Nul => "nul".to_string(),
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    USub,
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::USub => "-",
                Self::Not => "!",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Add => "+",
                Self::Sub => "-",
                Self::Mul => "*",
                Self::Div => "/",
                Self::Mod => "%",
                Self::Eq => "==",
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OpCodeOp<T> {
    OpCode(FullOpCode),
    Op(T),
}

impl OpCodeOp<UnaryOp> {
    pub fn new_unary(token: Token) -> Self {
        OpCodeOp::Op(match token.kind {
            TokenKind::Minus => UnaryOp::USub,
            TokenKind::Exclamation => UnaryOp::Not,
            _ => panic!(),
        })
    }
}

impl OpCodeOp<BinaryOp> {
    pub fn new_binary(token: Token) -> Self {
        OpCodeOp::Op(match token.kind {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Sub,
            TokenKind::Star => BinaryOp::Mul,
            TokenKind::Slash => BinaryOp::Div,
            TokenKind::Percent => BinaryOp::Mod,
            TokenKind::EqualEqual => BinaryOp::Eq,
            _ => panic!(),
        })
    }
}

impl<T: fmt::Display> fmt::Display for OpCodeOp<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpCodeOp::Op(o) => write!(f, "{}", o),
            OpCodeOp::OpCode(o) => write!(f, "{}", o),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Block {
        body: Vec<Stmt>,
        variable_storage: Option<usize>,
        value: Box<Expr>,
        data_type: Option<ValType>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
        data_type: Option<ValType>,
    },
    FnCall {
        callee: FunctionID,
        args: Vec<Expr>,
        pos: Pos,
    },
    AmbiguousFnCall {
        callee: AmbiguousFunctionID,
        args: Vec<Expr>,
        pos: Pos,
    },
    Assignment {
        var_id: VariableID,
        node: Box<Expr>,
    },
    Unary {
        op: OpCodeOp<UnaryOp>,
        node: Box<Expr>,
        data_type: Option<ValType>,
    },
    Binary {
        lhs: Box<Expr>,
        op: OpCodeOp<BinaryOp>,
        rhs: Box<Expr>,
        data_type: Option<ValType>,
    },
    Value {
        value: Val,
        pos: Pos,
    },
    Variable {
        id: VariableID,
        pos: Pos,
    },
    UnresolvedIdentifier {
        pos: Pos,
        lexeme: &'static str,
        args: Option<Vec<Expr>>,
    },
    Nothing,
}

impl Expr {
    pub fn new_unary(op: Token, node: Expr) -> Result<Expr, ZeptwoError> {
        Ok(Expr::Unary {
            op: OpCodeOp::new_unary(op),
            node: Box::new(node),
            data_type: None,
        })
    }
    pub fn new_binary(lhs: Expr, op: Token, rhs: Expr) -> Result<Expr, ZeptwoError> {
        Ok(Expr::Binary {
            lhs: Box::new(lhs),
            op: OpCodeOp::new_binary(op),
            rhs: Box::new(rhs),
            data_type: None,
        })
    }
    pub fn is_block_with_nul_value(&self) -> bool {
        match self {
            Expr::Block { value, .. } => value.is_nul(),
            Expr::If {
                then_branch,
                else_branch,
                ..
            } => {
                if let Some(else_branch) = else_branch {
                    return then_branch.is_block_with_nul_value()
                        && else_branch.is_block_with_nul_value();
                }
                then_branch.is_block_with_nul_value()
            }
            _ => false,
        }
    }
    fn is_nul(&self) -> bool {
        matches!(
            self,
            Expr::Value {
                value: Val::Nul,
                ..
            }
        )
    }
}

impl Position for Expr {
    fn left_pos(&self, lookup: &IdentifierLookup) -> Pos {
        match self {
            Expr::Block { body, value, .. } => match body.first() {
                Some(expr) => expr.left_pos(lookup),
                None => value.left_pos(lookup),
            },
            Expr::If { condition, .. } => condition.left_pos(lookup),
            Expr::FnCall { args, pos, .. } => match args.first() {
                Some(arg) => {arg.left_pos(lookup)},
                None => *pos,
            },
            Expr::AmbiguousFnCall { args, pos, .. } => match args.first() {
                Some(arg) => {arg.left_pos(lookup)},
                None => *pos,
            },
            Expr::Assignment { node, .. } => node.left_pos(lookup),
            Expr::Unary { node, .. } => node.left_pos(lookup),
            Expr::Binary { lhs, .. } => lhs.left_pos(lookup),
            Expr::Value { pos, .. } => *pos,
            Expr::Variable { pos, .. } => *pos,
            Expr::UnresolvedIdentifier { pos, .. } => *pos,
            Expr::Nothing => panic!(),
        }
    }
    fn right_pos(&self, lookup: &IdentifierLookup) -> Pos {
        match self {
            Expr::Block { value, .. } => value.right_pos(lookup),
            Expr::If {
                then_branch,
                else_branch,
                ..
            } => {
                if let Some(else_branch) = else_branch {
                    else_branch.right_pos(lookup)
                } else {
                    then_branch.right_pos(lookup)
                }
            }
            Expr::FnCall { args, .. } => args.last().unwrap().right_pos(lookup),
            Expr::AmbiguousFnCall { args, .. } => args.last().unwrap().right_pos(lookup),
            Expr::Assignment { node, .. } => node.right_pos(lookup),
            Expr::Unary { node, .. } => node.right_pos(lookup),
            Expr::Binary { rhs, .. } => rhs.right_pos(lookup),
            Expr::Value { pos, .. } => *pos,
            Expr::Variable { pos, .. } => *pos,
            Expr::UnresolvedIdentifier { pos, .. } => *pos,
            Expr::Nothing => panic!(),
        }
    }
}

impl ValTypeTrait for Expr {
    fn get_type(&self, lookup: &IdentifierLookup) -> Option<ValType> {
        match self {
            Expr::Block { data_type, .. } => data_type.as_ref().cloned(),
            Expr::If { data_type, .. } => data_type.as_ref().cloned(),
            Expr::FnCall { callee, .. } => Some(callee.get_return_type(lookup)),
            Expr::AmbiguousFnCall { .. } => None,
            Expr::Assignment { node, .. } => node.get_type(lookup),
            Expr::Unary { data_type, .. } => data_type.as_ref().cloned(),
            Expr::Binary { data_type, .. } => data_type.as_ref().cloned(),
            Expr::Value { value, .. } => value.get_type(lookup),
            Expr::Variable { id, .. } => id.get_variable_type(lookup),
            Expr::UnresolvedIdentifier { .. } => None,
            Expr::Nothing => panic!(),
        }
    }
    fn determine_type_and_opcode(
        &mut self,
        lookup: &mut IdentifierLookup,
        dependencies: &mut Vec<DependencyID>,
    ) -> Result<ValType, ZeptwoError> {
        let check_type = self.get_type(lookup);
        match check_type {
            None => {
                match self {
                    Expr::Block {
                        body,
                        value,
                        data_type,
                        ..
                    } => {
                        for stmt in body {
                            stmt.check_types(lookup, dependencies)?;
                        }
                        *data_type = Some(value.determine_type_and_opcode(lookup, dependencies)?);
                    }
                    Expr::If {
                        condition,
                        then_branch,
                        else_branch,
                        data_type,
                    } => {
                        let condition_type = condition.determine_type_and_opcode(lookup, dependencies)?;
                        if let ValType::Bool = condition_type {
                        } else {
                            Err(ZeptwoError::parser_error_at_pos(
                                condition.right_pos(lookup),
                                format!(
                                    "Expected type '{}' from condition. Found '{}'",
                                    ValType::Bool,
                                    condition_type
                                ),
                            ))?;
                        }
                        let then_type = then_branch.determine_type_and_opcode(lookup, dependencies)?;
                        if let Some(else_branch) = else_branch {
                            let else_type = else_branch.determine_type_and_opcode(lookup, dependencies)?;
                            if then_type != else_type {
                                Err(ZeptwoError::parser_error_at_pos(then_branch.right_pos(lookup), format!("Then and else branch have incompatible types. Expected '{}' in else block, found '{}'", then_type, else_type)))?;
                            }
                        } else if let ValType::Nul = then_type {
                        } else {
                            Err(ZeptwoError::parser_error_at_pos(
                                then_branch.right_pos(lookup),
                                format!(
                                    "Expected type '{}' from then branch. Found '{}'",
                                    ValType::Nul,
                                    then_branch.get_type(lookup).unwrap()
                                ),
                            ))?;
                        }

                        *data_type = Some(then_type)
                    }
                    Expr::FnCall { .. } => unreachable!(),
                    Expr::AmbiguousFnCall { callee, args, pos: left_pos } => {
                        let mut types = vec![];
                        for arg in &mut args.clone() {
                            types.push(arg.determine_type_and_opcode(lookup, dependencies)?.clone());
                        }
                        match lookup.determine_function(*callee, &types) {
                            Some(func) => {
                                dependencies.push(DependencyID::Func(func));
                                *self = Expr::FnCall {
                                    callee: func,
                                    args: args.clone(),
                                    pos: *left_pos,
                                };
                            }
                            None => {
                                let callee_name = lookup.get_ambiguous_name(*callee);
                                Err(ZeptwoError::parser_error_at_pos(self.left_pos(lookup), format!("Wrong arguments parsed into function '{}', arguments recieved were: {}", callee_name, {
                                    let mut string = String::from("("); // TODO: turn into function thing
                                    let mut first = true;
                                    for t in types {
                                        if !first {
                                            string += ", "
                                        }
                                        string += t.to_string().as_str();
                                        first = false;
                                    }
                                    string + ")"
                                })))?
                            }
                        }
                    }
                    Expr::Assignment { var_id, node } => {
                        if let Some(var_type) = var_id.get_variable_type(lookup) {
                            let node_type = node.determine_type_and_opcode(lookup, dependencies)?;
                            if var_type != node_type {
                                Err(ZeptwoError::parser_error_at_pos(
                                    self.left_pos(lookup),
                                    format!(
                                        "Can't assign '{}' to variable with type '{}'.",
                                        node_type, var_type
                                    ),
                                ))?;
                            }
                        } else {
                            unreachable!()
                        }
                    }
                    Expr::Unary {
                        op,
                        node,
                        data_type,
                    } => {
                        let mut error = false;
                        let node_type = node.determine_type_and_opcode(lookup, dependencies)?;
                        if let OpCodeOp::Op(operation) = op {
                            match operation {
                                UnaryOp::USub => match node_type {
                                    ValType::Int => {
                                        *op = OpCodeOp::OpCode(FullOpCode::UsubInt);
                                        *data_type = Some(node_type.clone());
                                    }
                                    ValType::Flt => {
                                        *op = OpCodeOp::OpCode(FullOpCode::UsubFLt);
                                        *data_type = Some(node_type.clone());
                                    }
                                    _ => {
                                        error = true;
                                    }
                                },
                                UnaryOp::Not => match node_type {
                                    ValType::Int => {
                                        *op = OpCodeOp::OpCode(FullOpCode::BitnotInt);
                                        *data_type = Some(node_type.clone());
                                    }
                                    ValType::Bool => {
                                        *op = OpCodeOp::OpCode(FullOpCode::NotBool);
                                        *data_type = Some(node_type.clone());
                                    }
                                    _ => {
                                        error = true;
                                    }
                                },
                            }
                        }
                        if error {
                            return Err(ZeptwoError::parser_error_at_pos(
                                node.right_pos(lookup),
                                format!(
                                    "Unary operator '{}', not supported for type '{}'.",
                                    op, node_type
                                )
                                .as_str(),
                            ));
                        }
                    }
                    Expr::Binary {
                        lhs,
                        op,
                        rhs,
                        data_type,
                    } => {
                        let mut error = false;
                        let lhs_type = lhs.determine_type_and_opcode(lookup, dependencies)?;
                        let rhs_type = rhs.determine_type_and_opcode(lookup, dependencies)?;
                        if let OpCodeOp::Op(operation) = op {
                            match operation {
                                BinaryOp::Add => match (lhs_type.clone(), rhs_type.clone()) {
                                    (ValType::Int, ValType::Int) => {
                                        *op = OpCodeOp::OpCode(FullOpCode::AddInt);
                                        *data_type = Some(lhs_type.clone());
                                    }
                                    (ValType::Flt, ValType::Flt) => {
                                        *op = OpCodeOp::OpCode(FullOpCode::AddFlt);
                                        *data_type = Some(lhs_type.clone());
                                    }
                                    _ => {
                                        error = true;
                                    }
                                },
                                BinaryOp::Sub => match (lhs_type.clone(), rhs_type.clone()) {
                                    (ValType::Int, ValType::Int) => {
                                        *op = OpCodeOp::OpCode(FullOpCode::SubInt);
                                        *data_type = Some(lhs_type.clone());
                                    }
                                    (ValType::Flt, ValType::Flt) => {
                                        *op = OpCodeOp::OpCode(FullOpCode::SubFlt);
                                        *data_type = Some(lhs_type.clone());
                                    }
                                    _ => {
                                        error = true;
                                    }
                                },
                                BinaryOp::Mul => match (lhs_type.clone(), rhs_type.clone()) {
                                    (ValType::Int, ValType::Int) => {
                                        *op = OpCodeOp::OpCode(FullOpCode::RemInt);
                                        *data_type = Some(lhs_type.clone());
                                    }
                                    (ValType::Flt, ValType::Flt) => {
                                        *op = OpCodeOp::OpCode(FullOpCode::MulFlt);
                                        *data_type = Some(lhs_type.clone());
                                    }
                                    _ => {
                                        error = true;
                                    }
                                },
                                BinaryOp::Div => match (lhs_type.clone(), rhs_type.clone()) {
                                    (ValType::Int, ValType::Int) => {
                                        *op = OpCodeOp::OpCode(FullOpCode::DivInt);
                                        *data_type = Some(lhs_type.clone());
                                    }
                                    (ValType::Flt, ValType::Flt) => {
                                        *op = OpCodeOp::OpCode(FullOpCode::DivFlt);
                                        *data_type = Some(lhs_type.clone());
                                    }
                                    _ => {
                                        error = true;
                                    }
                                },
                                BinaryOp::Mod => {
                                    if let (ValType::Int, ValType::Int) =
                                        (lhs_type.clone(), rhs_type.clone())
                                    {
                                        *op = OpCodeOp::OpCode(FullOpCode::RemInt);
                                        *data_type = Some(ValType::Int);
                                    };
                                }
                                BinaryOp::Eq => match (lhs_type.clone(), rhs_type.clone()) {
                                    (ValType::Int, ValType::Int) | (ValType::Flt, ValType::Flt) => {
                                        *op = OpCodeOp::OpCode(FullOpCode::EqWord);
                                        *data_type = Some(ValType::Bool);
                                    }
                                    (ValType::Bool, ValType::Bool) => {
                                        *op = OpCodeOp::OpCode(FullOpCode::EqBool);
                                        *data_type = Some(ValType::Bool);
                                    }
                                    _ => {
                                        error = true;
                                    }
                                },
                            }
                            if error {
                                return Err(ZeptwoError::parser_error_at_pos(lhs.left_pos(lookup), format!("Binary operator '{}', not supported for operands of type '{}' and '{}'.", op, lhs_type.clone(), rhs_type.clone())));
                            }
                        }
                    }
                    Expr::Value { .. } => {}
                    Expr::Variable { .. } => {}
                    Expr::UnresolvedIdentifier { pos, lexeme, args } => {
                        match lookup.acquire_identifier_id(lexeme) {
                            Some(IdentifierID::Var(id)) => {
                                match args {
                                    Some(_) => {
                                        Err(ZeptwoError::parser_error_at_pos(*pos, format!("Can't call variable '{}'.", lexeme)))?
                                    }
                                    None => {
                                        *self = Expr::Variable { id, pos: *pos };
                                        self.determine_type_and_opcode(lookup, dependencies)?;
                                    },
                                }
                            },
                            Some(IdentifierID::Func(id)) => {
                                match args.clone() {
                                    Some(args) => {
                                        *self = Expr::AmbiguousFnCall { callee: id, args, pos: *pos };
                                        self.determine_type_and_opcode(lookup, dependencies)?;
                                    }
                                    None => Err(ZeptwoError::parser_error_at_pos(*pos, format!("Missing arguments to function '{}'.", lexeme)))?, // todo: make into expression.
                                }
                            },
                            None => Err(ZeptwoError::parser_error_at_pos(*pos, format!("Could not find definition to identifier '{}'.", lexeme)))?,
                        }
                    }
                    Expr::Nothing => panic!(),
                }
                match self.get_type(lookup) {
                    Some(v) => Ok(v),
                    None => Err(ZeptwoError::parser_error_at_pos(
                        self.right_pos(lookup),
                        "Could not determine expression type.",
                    )),
                }
            }
            Some(c) => Ok(c),
        }
    }
    fn get_result_size(&self, lookup: &IdentifierLookup) -> u16 {
        match self.get_type(lookup) {
            Some(ValType::Func {
                func_type,
                params,
                return_type,
            }) => todo!(),
            Some(ValType::Str) => VMSTRING_SIZE,
            Some(ValType::Int) => VMINT_SIZE,
            Some(ValType::Flt) => VMFLT_SIZE,
            Some(ValType::Bool) => VMBOOL_SIZE,
            Some(ValType::Nul) => VMNUL_SIZE,
            None => panic!(),
        }
        .try_into()
        .unwrap()
    }
}

impl SizeAndIndex for Expr {
    fn determine_size_and_indexes(
        &mut self,
        stack_index: &mut usize,
        lookup: &mut IdentifierLookup,
    ) -> Result<usize, ZeptwoError> {
        match self {
            Expr::Block {
                body,
                value,
                variable_storage,
                ..
            } => {
                let mut max_size = 0;
                let stack_index_start = *stack_index;
                for stmt in body {
                    let stmt_size = stmt.determine_size_and_indexes(stack_index, lookup)?;
                    max_size =
                        std::cmp::max(max_size, (*stack_index - stack_index_start) + stmt_size);
                }
                let value_size = value.determine_size_and_indexes(stack_index, lookup)?;
                let var_storage = *stack_index - stack_index_start;
                max_size = std::cmp::max(max_size, var_storage + value_size);

                match variable_storage {
                    Some(storage) => *storage += var_storage,
                    None => *variable_storage = Some(var_storage),
                }

                *stack_index = stack_index_start;
                Ok(max_size)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => Ok(std::cmp::max(
                std::cmp::max(
                    condition.determine_size_and_indexes(stack_index, lookup)?,
                    then_branch.determine_size_and_indexes(stack_index, lookup)?,
                ),
                if let Some(else_branch) = else_branch {
                    else_branch.determine_size_and_indexes(stack_index, lookup)?
                } else {
                    0
                },
            )),
            Expr::FnCall { callee, args, .. } => {
                for arg in args {
                    arg.determine_size_and_indexes(stack_index, lookup);
                }
                // TODO: Calculate size correctly !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                Ok(0)
            }
            Expr::AmbiguousFnCall { callee, args, .. } => unreachable!(),
            Expr::Assignment { node, .. } => {
                Ok(node.determine_size_and_indexes(stack_index, lookup)?)
            }
            Expr::Unary { node, .. } => Ok(node.determine_size_and_indexes(stack_index, lookup)?),
            Expr::Binary { lhs, rhs, .. } => {
                let mut max_size = lhs.determine_size_and_indexes(stack_index, lookup)?;
                max_size = std::cmp::max(
                    max_size,
                    lhs.get_result_size(lookup) as usize
                        + rhs.determine_size_and_indexes(stack_index, lookup)?,
                );
                Ok(max_size)
            }
            Expr::Value { value, .. } => Ok(value.get_result_size(lookup) as usize),
            Expr::Variable { id, .. } => Ok(id
                .get_variable_info(lookup)
                .data_type
                .unwrap()
                .get_result_size(lookup) as usize),
            Expr::UnresolvedIdentifier { .. } => unreachable!(),
            Expr::Nothing => Ok(0),
        }
    }
}

// TODO: reuse this in error messages.
// impl fmt::Display for Expr {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             Expr::Block { body, value, .. } => {
//                 write!(f, "{{ {} }}", {
//                     let mut body_string = String::new();
//                     for stmt in body {
//                         body_string += format!("{}", stmt).as_str();
//                     }
//                     body_string += format!("{}", value).as_str();
//                     body_string
//                 })
//             }
//             Expr::If {
//                 condition,
//                 then_branch,
//                 else_branch,
//                 ..
//             } => {
//                 if let Some(else_branch) = else_branch {
//                     write!(f, "if {} {} else {}", condition, then_branch, else_branch)
//                 } else {
//                     write!(f, "if {} {}", condition, then_branch)
//                 }
//             }
//             Expr::FnCall { callee, args } => {
//                 write!(f, "{}({})", callee.get_name(), {
//                     // TODO: MAKE FUNCTION to not repeat code
//                     let mut string = String::new();
//                     for arg in args {
//                         string += format!("{}", arg).as_str();
//                     }
//                     string
//                 })
//             }
//             Expr::AmbiguousFnCall { callee, args } => {
//                 write!(f, "{}({})", callee.get_name(), {
//                     // TODO: MAKE FUNCTION to not repeat code
//                     let mut string = String::new();
//                     for arg in args {
//                         string += format!("{}", arg).as_str();
//                     }
//                     string
//                 })
//             }
//             Expr::Assignment { var_id, node, .. } => {
//                 write!(f, "({} = {})", var_id.get_variable_name(), node)
//             }
//             Expr::Unary {
//                 op,
//                 node,
//                 data_type,
//             } => write!(f, "({} {}){:?}", op, node, data_type),
//             Expr::Binary {
//                 lhs,
//                 op,
//                 rhs,
//                 data_type,
//             } => write!(f, "({} {} {}){:?}", lhs, op, rhs, data_type),
//             Expr::Value { value, .. } => write!(f, "{} {:?}", value, value.get_type()),
//             Expr::Variable { id, .. } => {
//                 let var_info = id.get_variable_info();
//                 write!(f, "{} {:?}", var_info.name, var_info.data_type)
//             }
//             Expr::Nothing => write!(f, "nothing"),
//         }
//     }
// }
