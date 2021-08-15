use crate::chunk::Chunk;
use crate::default;
use crate::lexer::{Lexer, Token, TokenKind};
use crate::object::ObjectRef;
use crate::opcode::OpCode;
use crate::span::FreeSpan;
use crate::string::String as RoxString;
use crate::value::Value;
use log::trace;
use std::mem;
use std::num::ParseFloatError;


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
    InvalidAssignmentTarget,
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

    while parser.current.kind != TokenKind::Eof {
        parser.declaration();
    }
    parser.finish();

    if parser.errors.is_empty() {
        Ok(chunk)
    } else {
        Err(parser.errors)
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

/// Helper macro for emitting a variable number of instructions or constants in a single
/// expression.
macro_rules! emit {
    ( $self:ident ) => {};
    ( $self:ident, ) => {};
    ( $self:ident, Constant ( $value:expr ) $($tt:tt)* ) => {{
        let index = $self.chunk().insert_constant($value);
        $self.emit(OpCode::Constant { index });
        emit!( $self $($tt)* );
    }};
    ( $self:ident, GetGlobal ( $value:expr ) $($tt:tt)* ) => {{
        let index = $self.chunk().insert_constant($value);
        $self.emit(OpCode::GetGlobal { index });
        emit!( $self $($tt)* );
    }};
    ( $self:ident, DefGlobal ( $value:expr ) $($tt:tt)* ) => {{
        let index = $self.chunk().insert_constant($value);
        $self.emit(OpCode::DefGlobal { index });
        emit!( $self $($tt)* );
    }};
    ( $self:ident, SetGlobal ( $value:expr ) $($tt:tt)* ) => {{
        let index = $self.chunk().insert_constant($value);
        $self.emit(OpCode::SetGlobal { index });
        emit!( $self $($tt)* );
    }};
    ( $self:ident, $opcode:ident $($tt:tt)* ) => {{
        $self.emit(OpCode::$opcode);
        emit!( $self $($tt)* );
    }};
}

impl<'ctx> Parser<'ctx> {
    fn enter(&mut self, fun: &'static str) {
        #[cfg(debug_assertions)] {
            let indent = " ".repeat(self.callstack.len());
            self.callstack.push(fun);
            trace!("{} + {}", indent, fun);
        }
    }

    fn leave(&mut self) {
        #[cfg(debug_assertions)] {
            let fun = self.callstack.pop().unwrap();
            let indent = " ".repeat(self.callstack.len());
            trace!("{} - {}", indent, fun);
        }
    }
}

impl<'ctx> Parser<'ctx> {
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
    }
}

impl<'ctx> Parser<'ctx> {
    fn parse_precedence(&mut self, precedence: Precedence) {
        self.enter("parse_precedence");

        self.advance();
        let rule = parser_rule(self.previous.kind);

        let can_assign = precedence <= Precedence::ASSIGNMENT;

        if let Some(prefix) = rule.prefix {
            prefix(self, can_assign);
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
                infix(self, can_assign);
            } else {
                unreachable!();
            }
        }

        if can_assign && self.current.kind == TokenKind::Equal {
            self.error(self.previous.span, ParserErrorKind::InvalidAssignmentTarget);
            return
        }

        self.leave();
    }

    fn identifier_constant(&mut self, token: Token) -> ObjectRef<RoxString> {
        assert_eq!(token.kind, TokenKind::Identifier);
        let span = token.span.anchor(self.lex.source());
        RoxString::new(span.slice())
    }

    fn expression(&mut self) {
        self.enter("expression");

        self.parse_precedence(Precedence::ASSIGNMENT);

        self.leave();
    }

    fn var_declaration(&mut self) {
        self.enter("var_declaration");

        self.consume(TokenKind::Var);
        self.consume(TokenKind::Identifier);

        let name = self.identifier_constant(self.previous);

        if self.current.kind == TokenKind::Equal {
            self.advance();
            self.expression();
        } else {
            emit!(self, Nil);
        }

        self.consume(TokenKind::Semicolon);

        // define_variable();
        emit!(self, DefGlobal(Value::Object(name.upcast())));

        self.leave();
    }

    fn expression_statement(&mut self) {
        self.enter("expression_statement");

        self.expression();
        self.consume(TokenKind::Semicolon);
        emit!(self, Pop);

        self.leave();
    }

    fn assert_statement(&mut self) {
        self.enter("assert_statement");

        self.consume(TokenKind::Assert);
        self.expression();
        self.consume(TokenKind::Semicolon);
        emit!(self, Assert);

        self.leave();
    }

    fn print_statement(&mut self) {
        self.enter("print_statement");

        self.consume(TokenKind::Print);
        self.expression();
        self.consume(TokenKind::Semicolon);
        emit!(self, Print);

        self.leave();
    }

    fn synchronize(&mut self) {
        self.enter("synchronize");

        self.panicking = false;

        while self.current.kind != TokenKind::Eof {
            if self.previous.kind == TokenKind::Semicolon {
                break;
            }
            if matches!(
                self.current.kind,
                TokenKind::Assert |
                TokenKind::Class |
                TokenKind::Fun |
                TokenKind::Var |
                TokenKind::For |
                TokenKind::If |
                TokenKind::While |
                TokenKind::Print |
                TokenKind::Return
            ) {
                break;
            }

            self.advance();
        }

        self.leave();
    }

    fn declaration(&mut self) {
        self.enter("declaration");

        match self.current.kind {
            TokenKind::Var => {
                self.var_declaration();
            }
            _ => {
                self.statement();
            }
        }

        if self.panicking {
            self.synchronize();
        }

        self.leave();
    }

    fn statement(&mut self) {
        self.enter("statement");

        match self.current.kind {
            TokenKind::Assert => {
                self.assert_statement();
            }
            TokenKind::Print => {
                self.print_statement();
            }
            _ => {
                self.expression_statement();
            }
        }

        self.leave();
    }

    /// `"("` `expression` `")"`
    fn grouping(&mut self, _: bool) {
        self.enter("grouping");

        self.expression();
        self.consume(TokenKind::RightParen);

        self.leave();
    }

    /// `<operator>` `<expression>`
    fn unary(&mut self, _: bool) {
        self.enter("unary");

        let operator = self.previous.kind;
        self.parse_precedence(Precedence::UNARY);
        match operator {
            TokenKind::Bang     => emit!(self, Not),
            TokenKind::Minus    => emit!(self, Negate),
            _ => unreachable!(),
        }

        self.leave();
    }

    /// `<expression>` `<operator>` `<expression>`
    fn binary(&mut self, _: bool) {
        self.enter("binary");

        let operator = self.previous.kind;
        let rule = parser_rule(operator);
        self.parse_precedence(rule.precedence + 1);

        match operator {
            TokenKind::BangEqual    => emit!(self, Equal, Not),
            TokenKind::EqualEqual   => emit!(self, Equal),
            TokenKind::Greater      => emit!(self, Greater),
            TokenKind::GreaterEqual => emit!(self, Less, Not),
            TokenKind::Less         => emit!(self, Less),
            TokenKind::LessEqual    => emit!(self, Greater, Not),
            TokenKind::Plus         => emit!(self, Add),
            TokenKind::Minus        => emit!(self, Subtract),
            TokenKind::Star         => emit!(self, Multiply),
            TokenKind::Slash        => emit!(self, Divide),
            _ => unreachable!(),
        }

        self.leave();
    }

    /// `<number>`
    fn number(&mut self, _: bool) {
        self.enter("number");

        let span = self.previous.span.anchor(self.lex.source());
        let slice = span.slice();
        match slice.parse() {
            Ok(float) => emit!(self, Constant(Value::Number(float))),
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

    /// `"\""` `<string>` `"\""`
    fn string(&mut self, _: bool) {
        self.enter("string");

        let span = self.previous.span.anchor(self.lex.source());
        let slice = span.slice()
            .strip_prefix('"').unwrap()
            .strip_suffix('"').unwrap();
        let string = RoxString::new(slice);

        emit!(self, Constant(Value::Object(string.upcast())));

        self.leave();
    }

    fn named_variable(&mut self, token: Token, can_assign: bool) {
        self.enter("named_variable");

        let name = self.identifier_constant(token);
        let name = Value::Object(name.upcast());

        if can_assign && self.current.kind == TokenKind::Equal {
            self.advance();
            self.expression();
            emit!(self, SetGlobal(name));
        } else {
            emit!(self, GetGlobal(name));
        }

        self.leave();
    }

    /// `<ident>`
    fn variable(&mut self, can_assign: bool) {
        self.enter("variable");

        self.named_variable(self.previous, can_assign);

        self.leave();
    }

    /// `"nil"` | `"true"` | `"false"`
    fn literal(&mut self, _: bool) {
        self.enter("literal");

        match self.previous.kind {
            TokenKind::Nil      => emit!(self, Nil),
            TokenKind::True     => emit!(self, True),
            TokenKind::False    => emit!(self, False),
            _ => unreachable!(),
        };

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
        EQUALITY,
        COMPARISON,
        TERM,
        FACTOR,
        UNARY,
        _CALL,
        _PRIMARY,
    }
}
use precedence::Precedence;

struct ParserRule<'ctx> {
    prefix: Option<fn(&mut Parser<'ctx>, bool)>,
    infix: Option<fn(&mut Parser<'ctx>, bool)>,
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
        Bang            unary       _           _,
        BangEqual       _           _           _,
        Equal           _           _           _,
        EqualEqual      _           binary      EQUALITY,
        Greater         _           binary      COMPARISON,
        GreaterEqual    _           binary      COMPARISON,
        Less            _           binary      COMPARISON,
        LessEqual       _           binary      COMPARISON,
        Identifier      variable    _           _,
        String          string      _           _,
        Number          number      _           _,
        And             _           _           _,
        Assert          _           _           _,
        Class           _           _           _,
        Else            _           _           _,
        False           literal     _           _,
        For             _           _           _,
        Fun             _           _           _,
        If              _           _           _,
        Nil             literal     _           _,
        Or              _           _           _,
        Print           _           _           _,
        Return          _           _           _,
        Super           _           _           _,
        This            _           _           _,
        True            literal     _           _,
        Var             _           _           _,
        While           _           _           _,
        Error           _           _           _,
        Eof             _           _           _,
    }
}
