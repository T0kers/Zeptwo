pub mod ast;
use ast::{
    expr::{Expr, Val, ValType},
    stmt::{CheckTypes, Stmt},
    *,
};
use expr::ValTypeTrait;
use identifiers::{FunctionID, IdentifierID, IdentifierLookup};

use crate::{errors::ZeptwoError, position::Position};
use ast::identifiers::VariableID;

use super::scanner::{Scanner, Token, TokenKind};
use crate::position::Pos;

use std::{collections::HashMap, str::FromStr};
use strum::{EnumCount, IntoEnumIterator};
use strum_macros::{EnumCount as EnumCountMacro, EnumIter, FromRepr};

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, EnumCountMacro, EnumIter, FromRepr, Clone, Copy,
)]
pub enum Precedence {
    None,
    Assignment, // =
    Or,         // ||
    And,        // &&
    Equality,   // == !=
    Comparison, // < > <= >=
    BitwiseOr,  // |
    BitwiseXor, // ^
    BitwiseAnd, // &
    Term,       // + -
    Factor,     // * / %
    Unary,      // ! - ~
    Call,       // . ()
    Primary,
}

impl Precedence {
    pub fn one_higher(&self) -> Self {
        let mut idx = Precedence::iter().position(|x| x == *self).unwrap();

        idx = std::cmp::min(idx + 1, Precedence::COUNT - 1);

        Precedence::from_repr(idx).unwrap()
    }
}

type PrefixRuleFn = fn(&mut Parser, bool) -> Result<Expr, ZeptwoError>;
type InfixRuleFn = fn(&mut Parser, /*lhs of equation*/ Expr) -> Result<Expr, ZeptwoError>;
struct ParseRule {
    prefix: Option<PrefixRuleFn>,
    infix: Option<InfixRuleFn>,
    precedence: Precedence,
}

impl ParseRule {
    pub const fn new(
        prefix: Option<PrefixRuleFn>,
        infix: Option<InfixRuleFn>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

pub struct Parser {
    current: Token,
    previous: Token,
    scanner: Scanner,
    pub lookup: IdentifierLookup,
    had_error: bool,
}

impl Parser {
    pub fn new(source: &'static str) -> Self {
        Self {
            current: Token::new(TokenKind::Nul, "", Pos::new(0)),
            previous: Token::new(TokenKind::Nul, "", Pos::new(0)),
            scanner: Scanner::new(source),
            lookup: IdentifierLookup {
                variable_info_vec: vec![],
                ambiguous_function_vec: vec![],
                ambiguous_function_map: HashMap::new(),
                function_id_vec: vec![],
                scope_identifier_map: vec![],
            },
            had_error: false,
        }
    }
    pub fn parse(&mut self) -> Result<(), ZeptwoError> {
        self.advance()?;

        self.lookup.start_scope();
        while !self.compare(TokenKind::EOF)? {
            match self.declaration() {
                Ok(()) => (),
                Err(e) => self.synchronize(e),
            }
        }
        self.consume(TokenKind::EOF, "Expected end of tokens.")?;

        if self.had_error {
            Err(ZeptwoError::parser_error(
                "Stopping compilation due to previous errors.",
            ))?;
        }
        self.lookup.check_types()?;
        // self.ast.check_types()?;
        //self.ast.optimize()?;
        self.lookup.determine_size_and_indexes()?;
        // self.ast.determine_size_and_indexes(&mut stack_size)?;

        Ok(())
    }
    fn advance(&mut self) -> Result<(), ZeptwoError> {
        self.previous = self.current.clone();
        loop {
            let next_token = self.scanner.scan_token();
            if let TokenKind::Error = next_token.kind.clone() {
                self.silent_synchronize(); // error message has already been printed, so we do not need print anything here.
            } else {
                self.current = next_token;
                break Ok(());
            }
        }
    }
    fn consume(&mut self, kind: TokenKind, msg: &str) -> Result<(), ZeptwoError> {
        if self.current.kind == kind {
            self.advance()?;
            Ok(())
        } else {
            self.error_at_current(msg)
        }
    }
    fn compare(&mut self, kind: TokenKind) -> Result<bool, ZeptwoError> {
        if self.check(kind) {
            self.advance()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn declaration(&mut self) -> Result<(), ZeptwoError> {
        self.advance()?;
        match self.previous.kind.clone() {
            TokenKind::Let => self.let_declaration(),
            TokenKind::Fn => self.fn_declaration(),
            TokenKind::Class => self.class_declaration(),
            _ => self.error("Expected declaration."),
        }
    }

    fn let_declaration(&mut self) -> Result<(), ZeptwoError> {
        Ok(())
    }

    fn fn_declaration(&mut self) -> Result<(), ZeptwoError> {
        self.lookup.new_function();

        self.consume(TokenKind::Identifier, "Expected function name.")?;
        let name = self.previous.lexeme;

        let declared_pos = self.previous.pos;

        self.consume(TokenKind::LParen, "Expected '(' after function name.")?;
        let mut params = vec![];
        let mut parameter_size = 0;
        while self.compare(TokenKind::Identifier)? {
            let param_name = self.previous.lexeme;
            self.consume(TokenKind::Colon, "Expected parameter type.")?;
            let var_type = self.type_expression()?;
            parameter_size += var_type.get_result_size(&self.lookup) as usize;
            params.push(
                self.lookup
                    .new_variable(param_name, Some(var_type), self.previous.pos),
            );

            if !self.compare(TokenKind::Comma)? {
                break;
            }
        }
        self.consume(TokenKind::RParen, "Expected ')' after function name.")?;

        self.consume(TokenKind::Colon, "Expected ':' after parameters.")?;

        let return_type = self.type_expression()?;

        self.consume(TokenKind::LBrace, "Expected '{' before function body.")?;

        let block = Expr::Block {
            body: vec![],
            variable_storage: Some(parameter_size),
            value: Box::new(self.function_block()?),
            data_type: None,
        };
        self.lookup
            .define_function(name, params, block, return_type, declared_pos);
        Ok(())
    }

    fn class_declaration(&mut self) -> Result<(), ZeptwoError> {
        Ok(())
    }

    fn item(&mut self) -> Result<Item, ZeptwoError> {
        match self.current.kind.clone() {
            TokenKind::While => {
                self.advance()?;
                Ok(Item::Stmt(self.while_statement()?))
            }
            // TokenKind::For => {self.advance()?; self.for_statement()},
            // TokenKind::Return => {self.advance()?; self.return_statement()},
            TokenKind::Let => {
                self.advance()?;
                Ok(Item::Stmt(self.let_statement()?))
            }
            // TokenKind::Break => self.break_statement(),
            // TokenKind::Continue => self.continue_statement(),
            _ => {
                let expr = self.expression()?;
                if expr.is_block_with_nul_value() {
                    self.compare(TokenKind::Semicolon)?;
                    return Ok(Item::Stmt(Stmt::Expr { expr }));
                }
                if self.compare(TokenKind::Semicolon)? {
                    return Ok(Item::Stmt(Stmt::Expr { expr }));
                }
                Ok(Item::Expr(expr))
            }
        }
    }

    fn while_statement(&mut self) -> Result<Stmt, ZeptwoError> {
        let condition = self.expression()?;

        self.consume(TokenKind::LBrace, "Expected '{' after while condition.")?;

        let mut body = vec![];
        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::EOF) {
            match self.item()? {
                Item::Stmt(stmt) => body.push(stmt),
                Item::Expr(expr) => {
                    Err(ZeptwoError::parser_error_at_line(
                        expr.right_pos(&self.lookup),
                        "Expected statement.",
                    ))?;
                }
            }
        }
        self.consume(TokenKind::RBrace, "Expected '}' after while block.")?;
        Ok(Stmt::While {
            condition,
            body,
            variable_storage: None,
        })
    }

    fn let_statement(&mut self) -> Result<Stmt, ZeptwoError> {
        self.consume(TokenKind::Identifier, "Expected variable name.")?;
        let variable_name = self.previous.lexeme;
        let var_pos = self.previous.pos;

        let mut val_type = None;
        if self.compare(TokenKind::Colon)? {
            self.consume(TokenKind::Identifier, "Expected variable type after ':'.")?;
            val_type = Some(ValType::from_str(self.previous.lexeme)?);
        }

        self.consume(TokenKind::Equal, "Expected '=' after variable name.")?;

        let expr = self.expression()?;
        self.consume(TokenKind::Semicolon, "Expected ';' after let statement.")?;

        let var_id = self.lookup.new_variable(variable_name, val_type, var_pos);
        println!(
            "name: {}, index: {:?}",
            variable_name,
            var_id.get_variable_info(&self.lookup).stack_index
        );

        Ok(Stmt::Let { var_id, expr })
    }

    fn parse_precedence(&mut self, prec: Precedence) -> Result<Expr, ZeptwoError> {
        self.advance()?;

        let mut lhs = Expr::Nothing;

        let can_assign = prec <= Precedence::Assignment;
        if let Some(prefix_rule) = self.get_rule(&self.previous.kind).prefix {
            lhs = prefix_rule(self, can_assign)?;
        } else {
            self.error("Expected expression.")?;
        }
        while prec <= self.get_rule(&self.current.kind).precedence {
            self.advance()?;
            if let Some(infix_rule) = self.get_rule(&self.previous.kind).infix {
                lhs = infix_rule(self, lhs)?;
            }
        }
        if can_assign && self.compare(TokenKind::Equal)? {
            self.error("Invalid assignment target.")?;
        }
        Ok(lhs)
    }

    fn type_expression(&mut self) -> Result<ValType, ZeptwoError> {
        self.consume(TokenKind::Identifier, "Expected type.")?;
        ValType::from_str(self.previous.lexeme)
    }

    fn expression(&mut self) -> Result<Expr, ZeptwoError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn function_block(&mut self) -> Result<Expr, ZeptwoError> {
        self.scoped_block()
    }

    fn scoped_block(&mut self) -> Result<Expr, ZeptwoError> {
        self.lookup.start_scope();
        let block = self.block()?;
        self.lookup.end_scope();
        Ok(block)
    }

    fn block(&mut self) -> Result<Expr, ZeptwoError> {
        let mut body = vec![];
        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::EOF) {
            match self.item()? {
                Item::Stmt(stmt) => body.push(stmt),
                Item::Expr(expr) => {
                    self.consume(TokenKind::RBrace, "Expected '}' or ';' after expression.")?;
                    return Ok(Expr::Block {
                        body,
                        value: Box::new(expr),
                        variable_storage: None,
                        data_type: None,
                    });
                }
            }
        }
        self.consume(TokenKind::RBrace, "Expected '}' after block.")?;
        Ok(Expr::Block {
            body,
            value: Box::new(Expr::Value {
                value: Val::Nul,
                pos: self.previous.pos,
            }),
            variable_storage: None,
            data_type: None,
        })
    }

    fn if_expression(&mut self) -> Result<Expr, ZeptwoError> {
        let condition = Box::new(self.expression()?);
        self.consume(TokenKind::LBrace, "Expected '{' after condition.")?;
        let then_branch = Box::new(self.scoped_block()?);

        if self.compare(TokenKind::Else)? {
            self.consume(TokenKind::LBrace, "Expected '{' after 'else'.")?;
            let else_branch = Some(Box::new(self.scoped_block()?));
            Ok(Expr::If {
                condition,
                then_branch,
                else_branch,
                data_type: None,
            })
        } else {
            Ok(Expr::If {
                condition,
                then_branch,
                else_branch: None,
                data_type: None,
            })
        }
    }

    fn grouping(&mut self) -> Result<Expr, ZeptwoError> {
        let result = self.expression()?;
        self.consume(TokenKind::RParen, "Expected ')' after expression.")?;
        Ok(result)
    }
    fn binary(&mut self, lhs: Expr) -> Result<Expr, ZeptwoError> {
        let oper_kind = self.previous.kind.clone();
        let rule = self.get_rule(&oper_kind);
        Expr::new_binary(
            lhs,
            self.previous.clone(),
            self.parse_precedence(rule.precedence.one_higher())?,
        )
    }
    fn unary(&mut self) -> Result<Expr, ZeptwoError> {
        Expr::new_unary(
            self.previous.clone(),
            self.parse_precedence(Precedence::Unary)?,
        )
    }
    fn value(&mut self) -> Result<Expr, ZeptwoError> {
        Ok(Expr::Value {
            value: match self.previous.kind {
                TokenKind::Int => Val::Int(self.previous.lexeme.parse().unwrap()),
                TokenKind::Float => Val::Flt(self.previous.lexeme.parse().unwrap()),
                TokenKind::String => Val::Str(self.previous.lexeme.parse().unwrap()),
                TokenKind::True => Val::Bool(self.previous.lexeme.parse().unwrap()),
                TokenKind::False => Val::Bool(self.previous.lexeme.parse().unwrap()),
                TokenKind::Nul => Val::Nul,
                _ => panic!(),
            },
            pos: self.previous.pos,
        })
    }

    fn identifier(&mut self, can_assign: bool) -> Result<Expr, ZeptwoError> {
        let lexeme = self.previous.lexeme;
        let id = self
            .lookup
            .acquire_identifier_id(lexeme, self.previous.pos)?;
        match id {
            IdentifierID::Var(id) => {
                if can_assign && self.compare(TokenKind::Equal)? {
                    let expr = self.expression()?;
                    Ok(Expr::Assignment {
                        var_id: id,
                        node: Box::new(expr),
                    })
                } else {
                    Ok(Expr::Variable {
                        id,
                        pos: self.previous.pos,
                    })
                }
            }
            IdentifierID::Func(id) => {
                if self.compare(TokenKind::LParen)? {
                    let mut args = vec![];
                    while !self.compare(TokenKind::RParen)? {
                        args.push(self.expression()?);

                        if !self.compare(TokenKind::Comma)? {
                            self.consume(
                                TokenKind::RParen,
                                "Expected ')' after function arguments.",
                            )?;
                            break;
                        }
                    }
                    Ok(Expr::AmbiguousFnCall { callee: id, args })
                } else {
                    Ok(Expr::Variable {
                        id: todo!(), /*id.as_expr()*/
                        pos: self.previous.pos,
                    })
                }
            }
            IdentifierID::Unresolved() => todo!(),
        }
    }

    fn get_rule(&self, kind: &TokenKind) -> &ParseRule {
        static LPAREN_RULE: ParseRule =
            ParseRule::new(Some(|s, _| s.grouping()), None, Precedence::Call);

        static SCOPE_RULE: ParseRule =
            ParseRule::new(Some(|s, _| s.scoped_block()), None, Precedence::None);

        static IF_RULE: ParseRule =
            ParseRule::new(Some(|s, _| s.if_expression()), None, Precedence::None);

        static EQ_RULE: ParseRule =
            ParseRule::new(None, Some(|s, lhs| s.binary(lhs)), Precedence::Equality);

        static PLUS_RULE: ParseRule =
            ParseRule::new(None, Some(|s, lhs| s.binary(lhs)), Precedence::Term);
        static MINUS_RULE: ParseRule = ParseRule::new(
            Some(|s, _| s.unary()),
            Some(|s, lhs| s.binary(lhs)),
            Precedence::Term,
        );
        static UNARY_RULE: ParseRule =
            ParseRule::new(Some(|s, _| s.unary()), None, Precedence::None);
        static TERM_RULE: ParseRule =
            ParseRule::new(None, Some(|s, lhs| s.binary(lhs)), Precedence::Factor);

        static VALUE_RULE: ParseRule =
            ParseRule::new(Some(|s, _| s.value()), None, Precedence::None);

        static IDENTIFIER_RULE: ParseRule =
            ParseRule::new(Some(|s, b| s.identifier(b)), None, Precedence::None);

        static DEFAULT_RULE: ParseRule = ParseRule::new(None, None, Precedence::None);

        use TokenKind as TK;
        match kind {
            TK::LParen => &LPAREN_RULE,
            TK::LBrace => &SCOPE_RULE,
            TK::If => &IF_RULE,
            TK::EqualEqual => &EQ_RULE,
            TK::Plus => &PLUS_RULE,
            TK::Minus => &MINUS_RULE,
            TK::Exclamation | TK::Tilde => &UNARY_RULE,
            TK::Star | TK::Slash | TK::Rem => &TERM_RULE,
            TK::Int | TK::Float | TK::String | TK::True | TK::False | TK::Nul => &VALUE_RULE,
            TK::Identifier => &IDENTIFIER_RULE,
            _ => &DEFAULT_RULE,
        }
    }

    fn check(&mut self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }
    fn error_at_current(&self, msg: &str) -> Result<(), ZeptwoError> {
        Err(ZeptwoError::parser_error_at_token(
            self.current.clone(),
            msg,
        ))
    }
    fn error(&self, msg: &str) -> Result<(), ZeptwoError> {
        Err(ZeptwoError::parser_error_at_token(
            self.previous.clone(),
            msg,
        ))
    }
    fn synchronize(&mut self, e: ZeptwoError) {
        eprintln!("{}", e);
        self.silent_synchronize();
    }
    fn silent_synchronize(&mut self) {
        self.had_error = true;
        while self.current.kind != TokenKind::EOF {
            if self.previous.kind == TokenKind::Semicolon {
                return;
            }
            match self.current.kind {
                TokenKind::Return
                | TokenKind::Let
                | TokenKind::If
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Class => return,
                _ => match self.advance() {
                    Ok(_) => continue,
                    Err(e) => self.synchronize(e),
                },
            }
        }
    }
}
