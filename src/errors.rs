use crate::{
    position::Pos,
    scanner::{Token, TokenKind},
};

pub fn echo<T: std::fmt::Debug>(i: T) -> T {
    println!("{:?}", i);
    i
}

pub enum ZeptwoError {
    Scanner(String),
    Parser(String),
    Compiler(String),
    Runtime(String),
}

impl ZeptwoError {
    pub fn compile_error(msg: impl Into<String>) -> ZeptwoError {
        ZeptwoError::Compiler(msg.into())
    }

    pub fn parser_error(msg: impl Into<String>) -> ZeptwoError {
        ZeptwoError::Parser(msg.into())
    }

    pub fn runtime_error(msg: impl Into<String>) -> ZeptwoError {
        ZeptwoError::Compiler(msg.into())
    }

    pub fn compiler_error_at_line(pos: Pos, msg: impl Into<String>) -> ZeptwoError {
        ZeptwoError::Compiler(format!("[Line {}] {}", pos, msg.into()))
    }

    pub fn parser_error_at_line(pos: Pos, msg: impl Into<String>) -> ZeptwoError {
        ZeptwoError::Parser(format!("[{}] {}", pos, msg.into()))
    }

    pub fn parser_error_at_token(token: Token, msg: &str) -> ZeptwoError {
        ZeptwoError::Parser(format!(
            "[{}] Error {}: {}",
            token.pos,
            match token.kind {
                TokenKind::EOF => "at end".to_string(),
                _ => format!("at {}", token.lexeme),
            },
            msg
        ))
    }
}

impl std::fmt::Display for ZeptwoError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            ZeptwoError::Scanner(ref err) => write!(f, "{}", err),
            ZeptwoError::Parser(ref err) => write!(f, "{}", err),
            ZeptwoError::Compiler(ref err) => write!(f, "{}", err),
            ZeptwoError::Runtime(ref err) => write!(f, "{}", err),
        }
    }
}
