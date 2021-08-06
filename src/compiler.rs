use crate::chunk::Chunk;
use crate::default;
use crate::lexer::{Lexer, Token, TokenKind};
use crate::opcode::OpCode;
use crate::span::FreeSpan;
use crate::value::Value;
use log::trace;
use std::num::ParseFloatError;
use std::{iter, mem};


#[derive(Debug)]
pub struct ParserError {
    pub span: FreeSpan,
    pub kind: ParserErrorKind,
}

#[derive(Debug)]
pub enum ParserErrorKind {
    UnexpectedToken {
        found: TokenKind,
        expected: TokenKind,
    },
    ExpectedExpressionStart {
        found: TokenKind,
    },
    InvalidNumberLiteral {
        token: String,
        cause: ParseFloatError,
    },
}

struct Parser<'ctx> {
    chunk: &'ctx mut Chunk,
    lex: &'ctx mut Lexer<'ctx>,

    errors: Vec<ParserError>,
    panicking: bool,

    previous: Token,
    current: Token,

    #[cfg(debug_assertions)]
    callstack: Vec<&'static str>,
}

pub fn compile(source: String) -> Result<Chunk, Vec<ParserError>> {
    let mut chunk = Chunk::new(source.clone());
    let mut lex = Lexer::new(&source);

    let current = lex.peek();
    let mut parser = Parser {
        chunk: &mut chunk,
        lex: &mut lex,
        errors: default(),
        panicking: false,
        previous: default(),
        current,
        #[cfg(debug_assertions)]
        callstack: default(),
    };

    parser.expression();
    parser.finish();

    if parser.errors.is_empty() {
        Ok(chunk)
    } else {
        Err(parser.errors)
    }
}

impl<'ctx> Parser<'ctx> {
    fn enter(&mut self, fun: &'static str) {
        #[cfg(debug_assertions)] {
            let indent = iter::repeat(' ').take(self.callstack.len()).collect::<String>();
            self.callstack.push(fun);
            trace!("{} + {}", indent, fun);
        }
    }
    fn leave(&mut self) {
        #[cfg(debug_assertions)] {
            let fun = self.callstack.pop().unwrap();
            let indent = iter::repeat(' ').take(self.callstack.len()).collect::<String>();
            trace!("{} - {}", indent, fun);
        }
    }

    fn error(&mut self, span: FreeSpan, kind: ParserErrorKind) {
        if !self.panicking {
            self.errors.push(ParserError { span, kind });
            self.panicking = true;
        }
    }

    fn advance(&mut self) {
        let next = self.lex.next();
        self.previous = mem::replace(&mut self.current, next);
    }

    fn consume(&mut self, kind: TokenKind) {
        if self.current.kind == kind {
            self.advance();
        } else {
            self.error(self.current.span, ParserErrorKind::UnexpectedToken {
                found: self.current.kind,
                expected: kind,
            });
        }
    }

    fn finish(&mut self) {
        self.consume(TokenKind::Eof);
        self.emit(OpCode::Return);
    }
}

impl<'ctx> Parser<'ctx> {
    /// Get currently compiling Chunk
    fn chunk(&mut self) -> &mut Chunk {
        self.chunk
    }

    fn emit(&mut self, opcode: OpCode) {
        let span = self.previous.span;
        self.chunk().write(opcode, span);
    }
}

impl<'ctx> Parser<'ctx> {
    fn parse_precedence(&mut self, precedence: Precedence) {
        self.enter("parse_precedence");

        self.advance();
        let rule = parser_rule(self.previous.kind);

        if let Some(prefix) = rule.prefix {
            prefix(self);
        } else {
            self.error(self.previous.span, ParserErrorKind::ExpectedExpressionStart {
                found: self.previous.kind,
            });
            return
        }

        while precedence <= parser_rule(self.current.kind).precedence {
            self.advance();
            let rule = parser_rule(self.previous.kind);
            if let Some(infix) = rule.infix {
                infix(self);
            } else {
                unreachable!();
            }
        }

        self.leave();
    }

    fn expression(&mut self) {
        self.enter("expression");

        self.parse_precedence(Precedence::ASSIGNMENT);

        self.leave();
    }

    /// `"("` `expression` `")"`
    fn grouping(&mut self) {
        self.enter("grouping");

        self.expression();
        self.consume(TokenKind::RightParen);

        self.leave();
    }

    /// `operator` `expression`
    fn unary(&mut self) {
        self.enter("unary");

        let operator = self.previous.kind;
        self.parse_precedence(Precedence::UNARY);
        match operator {
            TokenKind::Minus => {
                self.emit(OpCode::Negate);
            }
            _ => unreachable!(),
        }

        self.leave();
    }

    /// `expression` `operator` `expression`
    fn binary(&mut self) {
        self.enter("binary");

        let operator = self.previous.kind;
        let rule = parser_rule(operator);
        self.parse_precedence(rule.precedence + 1);

        match operator {
            TokenKind::Plus => self.emit(OpCode::Add),
            TokenKind::Minus => self.emit(OpCode::Subtract),
            TokenKind::Star => self.emit(OpCode::Multiply),
            TokenKind::Slash => self.emit(OpCode::Divide),
            _ => unreachable!(),
        }

        self.leave();
    }

    /// `number`
    fn number(&mut self) {
        self.enter("number");

        let span = self.previous.span.anchor(self.lex.source());
        let slice = span.slice();
        match slice.parse() {
            Ok(float) => {
                let index = self.chunk().insert_constant(Value { float });
                self.emit(OpCode::Constant { index });
            }
            Err(cause) => {
                let token = slice.to_owned();
                self.error(self.previous.span, ParserErrorKind::InvalidNumberLiteral {
                    token,
                    cause,
                });
            }
        }

        self.leave();
    }
}

mod precedence {
    use std::ops::Add;

    #[derive(PartialEq, Eq, PartialOrd, Ord)]
    pub struct Precedence(u8);

    /// Helper macro for defining Precedence `const`s with ascending values
    macro_rules! precedence {
        ( $( $ops:ident ),+ $(,)? ) => {
            impl Precedence {
                precedence!( @(0u8) $($ops)* );
            }
        };
        ( @($n:expr) ) => {};
        ( @($n:expr) $op:ident $( $ops:ident )* ) => {
            pub const $op: Precedence = Precedence($n);
            precedence!( @($n + 1u8) $($ops)* );
        };
    }

    impl Add<u8> for Precedence {
        type Output = Precedence;

        fn add(self, rhs: u8) -> Self::Output {
            Precedence(self.0 + rhs)
        }
    }

    precedence! {
        NONE,
        ASSIGNMENT,
        _OR,
        _AND,
        _EQUALITY,
        _COMPARISON,
        TERM,
        FACTOR,
        UNARY,
        _CALL,
        _PRIMARY,
    }
}
use precedence::Precedence;

struct ParserRule<'ctx> {
    prefix: Option<fn(&mut Parser<'ctx>)>,
    infix: Option<fn(&mut Parser<'ctx>)>,
    precedence: Precedence,
}

fn parser_rule<'ctx>(kind: TokenKind) -> ParserRule<'ctx> {
    macro_rules! parser_rules {
        ( $( $kind:ident $prefix:tt $infix:tt $precedence:tt ),* $(,)? ) => {
            match kind {
                $(
                    TokenKind::$kind => ParserRule {
                        prefix: parser_rules!( @fn $prefix ),
                        infix: parser_rules!( @fn $infix ),
                        precedence: parser_rules!( @prec $precedence ),
                    },
                )*
                _ => ParserRule {
                    prefix: None,
                    infix: None,
                    precedence: Precedence::NONE,
                }
            }
        };

        ( @fn _ ) => { None };
        ( @fn $fn:ident ) => { Some(Parser::$fn) };

        ( @prec _ ) => { Precedence::NONE };
        ( @prec $prec:ident ) => { Precedence::$prec };
    }

    parser_rules! {
        LeftParen       grouping    _           _,
        RightParen      _           _           _,
        LeftBrace       _           _           _, 
        RightBrace      _           _           _,
        Comma           _           _           _,
        Dot             _           _           _,
        Minus           unary       binary      TERM,
        Plus            _           binary      TERM,
        Semicolon       _           _           _,
        Slash           _           binary      FACTOR,
        Star            _           binary      FACTOR,
        Bang            _           _           _,
        BangEqual       _           _           _,
        Equal           _           _           _,
        EqualEqual      _           _           _,
        Greater         _           _           _,
        GreaterEqual    _           _           _,
        Less            _           _           _,
        LessEqual       _           _           _,
        Identifier      _           _           _,
        String          _           _           _,
        Number          number      _           _,
        And             _           _           _,
        Class           _           _           _,
        Else            _           _           _,
        False           _           _           _,
        For             _           _           _,
        Fun             _           _           _,
        If              _           _           _,
        Nil             _           _           _,
        Or              _           _           _,
        Print           _           _           _,
        Return          _           _           _,
        Super           _           _           _,
        This            _           _           _,
        True            _           _           _,
        Var             _           _           _,
        While           _           _           _,
        Error           _           _           _,
        Eof             _           _           _,
    }
}
