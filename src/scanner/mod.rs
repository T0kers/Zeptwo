use crate::position::Pos;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // single character
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Plus,
    Minus,
    Slash,
    Rem,
    Tilde,
    Dot,
    Comma,
    Colon,
    Semicolon,

    // one or two characters
    Exclamation,
    ExclamationEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    GreaterGreater,
    Less,
    LessEqual,
    LessLess,
    And,
    AndAnd,
    Bar,
    BarBar,
    Carrot,
    CarrotCarrot,
    Star,
    StarStar,

    // literals
    Identifier,
    String,
    Int,
    Float,

    // keywords
    If,
    Else,
    For,
    While,
    Let,
    Fn,
    Class,
    Selv,
    Return,
    True,
    False,
    Nul,

    Error,
    EOF,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: &'static str,
    pub pos: Pos,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: &'static str, pos: Pos) -> Self {
        Self { kind, lexeme, pos }
    }
    pub fn new_identifier(lexeme: &'static str, pos: Pos) -> Self {
        let mut chars = lexeme.chars();
        let kind;
        if let Some(ch) = chars.nth(0) {
            kind = match ch {
                'c' => Token::check_keyword(chars.as_str(), "lass", TokenKind::Class),
                'i' => Token::check_keyword(chars.as_str(), "f", TokenKind::If),
                'e' => Token::check_keyword(chars.as_str(), "lse", TokenKind::Else),
                'f' => match chars.nth(0) {
                    Some('a') => Token::check_keyword(chars.as_str(), "lse", TokenKind::False),
                    Some('o') => Token::check_keyword(chars.as_str(), "r", TokenKind::For),
                    Some('n') => Token::check_keyword(chars.as_str(), "", TokenKind::Fn),
                    _ => TokenKind::Identifier,
                },
                'w' => Token::check_keyword(chars.as_str(), "hile", TokenKind::While),
                's' => Token::check_keyword(chars.as_str(), "elf", TokenKind::Selv),
                'l' => Token::check_keyword(chars.as_str(), "et", TokenKind::Let),
                'r' => Token::check_keyword(chars.as_str(), "eturn", TokenKind::Return),
                't' => Token::check_keyword(chars.as_str(), "rue", TokenKind::True),
                'n' => Token::check_keyword(chars.as_str(), "ul", TokenKind::Nul),
                _ => TokenKind::Identifier,
            };
        } else {
            kind = TokenKind::Identifier;
        }

        Self { kind, lexeme, pos }
    }
    fn check_keyword(rest: &str, check: &str, kind: TokenKind) -> TokenKind {
        if rest == check {
            kind
        } else {
            TokenKind::Identifier
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

pub struct Scanner {
    pub source: &'static str,
    start: usize,
    current: usize,
    pos: Pos,
}

impl Scanner {
    pub fn new(source: &'static str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            pos: Pos::new(1),
        }
    }
    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return Token::new(TokenKind::EOF, self.current_lexeme(), self.pos);
        }

        if let Some(ch) = self.advance() {
            match ch {
                '(' => Token::new(TokenKind::LParen, self.current_lexeme(), self.pos),
                ')' => Token::new(TokenKind::RParen, self.current_lexeme(), self.pos),
                '[' => Token::new(TokenKind::LBracket, self.current_lexeme(), self.pos),
                ']' => Token::new(TokenKind::RBracket, self.current_lexeme(), self.pos),
                '{' => Token::new(TokenKind::LBrace, self.current_lexeme(), self.pos),
                '}' => Token::new(TokenKind::RBrace, self.current_lexeme(), self.pos),
                '+' => Token::new(TokenKind::Plus, self.current_lexeme(), self.pos),
                '-' => Token::new(TokenKind::Minus, self.current_lexeme(), self.pos),
                '/' => Token::new(TokenKind::Slash, self.current_lexeme(), self.pos),
                '%' => Token::new(TokenKind::Rem, self.current_lexeme(), self.pos),
                '~' => Token::new(TokenKind::Tilde, self.current_lexeme(), self.pos),
                '.' => Token::new(TokenKind::Dot, self.current_lexeme(), self.pos),
                ',' => Token::new(TokenKind::Comma, self.current_lexeme(), self.pos),
                ':' => Token::new(TokenKind::Colon, self.current_lexeme(), self.pos),
                ';' => Token::new(TokenKind::Semicolon, self.current_lexeme(), self.pos),
                '!' => Token::new(
                    if self.compare('=') {
                        self.advance();
                        TokenKind::ExclamationEqual
                    } else {
                        TokenKind::Exclamation
                    },
                    self.current_lexeme(),
                    self.pos,
                ),
                '=' => Token::new(
                    if self.compare('=') {
                        self.advance();
                        TokenKind::EqualEqual
                    } else {
                        TokenKind::Equal
                    },
                    self.current_lexeme(),
                    self.pos,
                ),
                '<' => Token::new(
                    if self.compare('=') {
                        self.advance();
                        TokenKind::LessEqual
                    } else if self.compare('<') {
                        self.advance();
                        TokenKind::LessLess
                    } else {
                        TokenKind::Less
                    },
                    self.current_lexeme(),
                    self.pos,
                ),
                '>' => Token::new(
                    if self.compare('=') {
                        self.advance();
                        TokenKind::GreaterEqual
                    } else if self.compare('>') {
                        self.advance();
                        TokenKind::GreaterGreater
                    } else {
                        TokenKind::Greater
                    },
                    self.current_lexeme(),
                    self.pos,
                ),
                '&' => Token::new(
                    if self.compare('&') {
                        self.advance();
                        TokenKind::AndAnd
                    } else {
                        TokenKind::And
                    },
                    self.current_lexeme(),
                    self.pos,
                ),
                '|' => Token::new(
                    if self.compare('|') {
                        self.advance();
                        TokenKind::BarBar
                    } else {
                        TokenKind::Bar
                    },
                    self.current_lexeme(),
                    self.pos,
                ),
                '^' => Token::new(
                    if self.compare('^') {
                        self.advance();
                        TokenKind::CarrotCarrot
                    } else {
                        TokenKind::Carrot
                    },
                    self.current_lexeme(),
                    self.pos,
                ),
                '*' => Token::new(
                    if self.compare('*') {
                        self.advance();
                        TokenKind::StarStar
                    } else {
                        TokenKind::Star
                    },
                    self.current_lexeme(),
                    self.pos,
                ),
                '"' => {
                    while self.peek() != Some('"') && !self.is_at_end() {
                        if let Some('\n') = self.peek() {
                            self.pos.line_add(1);
                        }
                        self.advance();
                    }
                    if self.is_at_end() {
                        return self.error("Unterminated string.");
                    };
                    self.advance();
                    Token::new(TokenKind::String, self.current_lexeme(), self.pos)
                }
                ch if ch.is_ascii_digit() => {
                    while let Some(n) = self.peek() {
                        if n.is_ascii_digit() {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    if let Some('.') = self.peek() {
                        if let Some(ch) = self.peek_next() {
                            if ch.is_ascii_digit() {
                                self.advance();
                                self.advance();
                                while let Some(n) = self.peek() {
                                    if n.is_ascii_digit() {
                                        self.advance();
                                    } else {
                                        break;
                                    }
                                }
                                return Token::new(
                                    TokenKind::Float,
                                    self.current_lexeme(),
                                    self.pos,
                                );
                            }
                        }
                    }
                    Token::new(TokenKind::Int, self.current_lexeme(), self.pos)
                }
                ch if ch.is_alphabetic() || ch == '_' => {
                    while let Some(next) = self.peek() {
                        if next.is_alphanumeric() || next == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    Token::new_identifier(self.current_lexeme(), self.pos)
                }
                ch => self.error(format!("{} is not recognised character.", ch).as_str()),
            }
        } else {
            self.error("Could not read token.")
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            if let Some(ch) = self.peek() {
                match ch {
                    ' ' | '\r' | '\t' => {
                        self.advance();
                    }
                    '\n' => {
                        self.pos.line_add(1);
                        self.advance();
                    }
                    '/' => {
                        if self.peek_next() == Some('/') {
                            while !self.is_at_end() && self.peek() != Some('\n') {
                                self.advance();
                            }
                        } else {
                            return;
                        }
                    }
                    _ => return,
                }
            } else {
                return;
            }
        }
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(ch) = self.source[self.current..].chars().next() {
            self.current += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    fn current_lexeme(&self) -> &'static str {
        &self.source[self.start..self.current]
    }

    fn peek(&self) -> Option<char> {
        self.source[self.current..].chars().next()
    }

    fn peek_next(&self) -> Option<char> {
        self.source[self.current..].chars().nth(1)
    }

    fn compare(&self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        Some(expected) == self.peek()
    }

    fn is_at_end(&self) -> bool {
        self.source.len() <= self.current
    }

    fn error(&mut self, msg: &str) -> Token {
        println!(
            "[Line {}] Error at {}: {}",
            self.pos,
            &self.source[self.start..self.current],
            msg
        );
        Token::new(TokenKind::Error, self.current_lexeme(), self.pos)
    }
}
