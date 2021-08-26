use crate::chunk::Chunk;
use crate::default;
use crate::lexer::{Lexer, Token, TokenKind};
use crate::object::function::Function;
use crate::object::string::String as RoxString;
use crate::object::ObjectRef;
use crate::opcode::OpCode;
use crate::span::FreeSpan;
use crate::value::Value;
use log::trace;
use std::mem;
use std::num::ParseFloatError;

/// Dummy offset used for pre-patch jumps.
const DUMMY: u16 = u16::MAX;


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
    TooManyLocals,
    Shadowing,
}

struct Parser<'ctx, 'src> {
    chunk: &'ctx mut Chunk,
    lex: &'ctx mut Lexer<'src>,

    errors: Vec<ParserError>,
    panicking: bool,

    previous: Token,
    current: Token,

    locals: Vec<Local>,
    scope_depth: i32,

    #[cfg(debug_assertions)]
    callstack: Vec<&'static str>,
}

struct Local {
    name: Token,
    depth: i32,
}

#[allow(clippy::needless_lifetimes)]
pub fn compile<'src>(source: &'src str) -> Result<ObjectRef<Function>, Vec<ParserError>> {
    let mut chunk = Chunk::default();
    let mut lex = Lexer::new(source);

    let current = lex.peek();
    let mut parser = Parser {
        chunk: &mut chunk,
        lex: &mut lex,
        errors: default(),
        panicking: false,
        locals: default(),
        scope_depth: 0,
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
        Ok(Function::new(chunk, 0, ""))
    } else {
        Err(parser.errors)
    }
}

mod emit {
    use crate::opcode::OpCode;
    use super::Parser;

    pub struct PatchPlace {
        position: usize,
    }

    impl<'ctx, 'src> Parser<'ctx, 'src> {
        pub fn emit(&mut self, opcode: OpCode) -> Option<PatchPlace> {
            let span = self.previous.span;
            let position = self.chunk.write(opcode, span);
            matches!(opcode, OpCode::Jump { .. } | OpCode::JumpIfTrue { .. } | OpCode::JumpIfFalse { .. })
                .then(|| PatchPlace { position })
        }

        #[track_caller]
        pub fn patch_jump(&mut self, place: Option<PatchPlace>) {
            let place = place.expect("tried to patch an unpatchable instruction");
            self.chunk.patch_jump(place.position)
        }
    }

    pub fn one_patchplace(a: Option<PatchPlace>, b: Option<PatchPlace>) -> Option<PatchPlace> {
        assert!(a.is_none() || b.is_none(), "discarding a PatchPlace");
        a.or(b)
    }
}

/// Helper macro for emitting a variable number of instructions in a single expression.
macro_rules! emit {
    ( $self:ident $(,)? ) => {{
        None
    }};

    ( $self:ident, $opcode:ident { constant: $value:expr } $($tt:tt)* ) => {{
        let index = $self.chunk.insert_constant($value);
        emit::one_patchplace(
            $self.emit(OpCode::$opcode { index }),
            emit!( $self $($tt)* ),
        )
    }};
    ( $self:ident, $opcode:ident { $arg:ident $(: $value:expr)? } $($tt:tt)* ) => {{
        emit::one_patchplace(
            $self.emit(OpCode::$opcode { $arg $(: $value)? }),
            emit!( $self $($tt)* ),
        )
    }};
    ( $self:ident, $opcode:ident $($tt:tt)* ) => {{
        emit::one_patchplace(
            $self.emit(OpCode::$opcode),
            emit!( $self $($tt)* ),
        )
    }};
}

impl<'ctx, 'src> Parser<'ctx, 'src> {
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

impl<'ctx, 'src> Parser<'ctx, 'src> {
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

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        assert!(self.scope_depth > 0);
        self.scope_depth -= 1;

        let pop = self.locals
            // TODO this is neat and short but doesn't actually match that the elements are only
            //      ever removed from the back of the Vec
            .drain_filter(|loc| loc.depth > self.scope_depth)
            .count();
        (0..pop)
            .for_each(|_| { emit!(self, Pop); });
    }
}

impl<'ctx, 'src> Parser<'ctx, 'src> {
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
            self.leave();
            return;
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
            self.leave();
            return;
        }

        self.leave();
    }

    fn identifier_constant(&mut self, token: Token) -> ObjectRef<RoxString> {
        assert_eq!(token.kind, TokenKind::Identifier);
        let span = token.span.anchor(self.lex.source());
        RoxString::new(span.as_str())
    }

    fn add_local(&mut self, name: Token) {
        if self.locals.len() >= (u16::MAX as usize) {
            self.error(name.span, ParserErrorKind::TooManyLocals);
            return;
        }
        let tokslice = |token: Token| token.span.anchor(self.lex.source()).as_str();
        let is_shadowing = self.locals.iter()
            .rev()
            .take_while(|loc| loc.depth == self.scope_depth)
            .any(|loc| tokslice(loc.name) == tokslice(name));
        if is_shadowing {
            self.error(name.span, ParserErrorKind::Shadowing);
            return;
        }
        self.locals.push(Local { name, depth: self.scope_depth });
    }

    fn resolve_local(&mut self, name: Token) -> Option<u16> {
        let tokslice = |token: Token| token.span.anchor(self.lex.source()).as_str();
        self.locals.iter()
            .rposition(|loc| tokslice(loc.name) == tokslice(name))
            .map(|index| index as u16)
    }

    fn expression(&mut self) {
        self.enter("expression");

        self.parse_precedence(Precedence::ASSIGNMENT);

        self.leave();
    }

    fn block(&mut self) {
        self.enter("block");
        self.consume(TokenKind::LeftBrace);

        while !matches!(self.current.kind, TokenKind::RightBrace | TokenKind::Eof) {
            self.declaration();
        }
        self.consume(TokenKind::RightBrace);

        self.leave();
    }

    fn var_declaration(&mut self) {
        self.enter("var_declaration");

        self.consume(TokenKind::Var);
        self.consume(TokenKind::Identifier);

        let name = self.previous;

        if self.current.kind == TokenKind::Equal {
            self.advance();
            self.expression();
        } else {
            emit!(self, Nil);
        }

        self.consume(TokenKind::Semicolon);

        if self.scope_depth == 0 {
            // global variable
            let name = self.identifier_constant(name);
            emit!(self, DefGlobal { constant: Value::new_object(name) });
        } else {
            // local variable
            self.add_local(name);
        }

        self.leave();
    }

    fn expression_statement(&mut self) {
        self.enter("expression_statement");

        self.expression();
        self.consume(TokenKind::Semicolon);
        emit!(self, Pop);

        self.leave();
    }

    fn if_statement(&mut self) {
        self.enter("if_statement");

        self.consume(TokenKind::If);
        self.expression();

        let then_jump = emit!(self, JumpIfFalse { offset: DUMMY });
        emit!(self, Pop);
        self.block();

        let else_jump = emit!(self, Jump { offset: DUMMY });
        self.patch_jump(then_jump);
        emit!(self, Pop);

        if self.current.kind == TokenKind::Else {
            self.advance();
            self.block();
        }
        self.patch_jump(else_jump);

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

    fn while_statement(&mut self) {
        self.enter("while_statement");

        self.consume(TokenKind::While);
        let loop_start = self.chunk.code().len();
        self.expression();

        let exit_jump = emit!(self, JumpIfFalse { offset: DUMMY });
        emit!(self, Pop);
        self.block();

        let loop_end = self.chunk.code().len() + 3; // +3 for the encoded `Loop` instruction
        let offset = (loop_end - loop_start).try_into().expect("loop body too large");
        emit!(self, Loop { offset });

        self.patch_jump(exit_jump);
        emit!(self, Pop);

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
            TokenKind::If => {
                self.if_statement();
            }
            TokenKind::While => {
                self.while_statement();
            }
            TokenKind::LeftBrace => {
                self.begin_scope();
                self.block();
                self.end_scope();
            }
            _ => {
                self.expression_statement();
            }
        }

        self.leave();
    }

    /// <expr> )
    fn grouping(&mut self, _: bool) {
        self.enter("grouping");

        self.expression();
        self.consume(TokenKind::RightParen);

        self.leave();
    }

    /// <op> <expr>
    fn unary(&mut self, _: bool) {
        self.enter("unary");

        let operator = self.previous.kind;
        self.parse_precedence(Precedence::UNARY);
        match operator {
            TokenKind::Bang     => emit!(self, Not),
            TokenKind::Minus    => emit!(self, Negate),
            _ => unreachable!(),
        };

        self.leave();
    }

    /// <op> <expr>
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
        };

        self.leave();
    }

    /// and <expr>
    fn and(&mut self, _: bool) {
        self.enter("and");

        let end_jump = emit!(self, JumpIfFalse { offset: DUMMY });
        emit!(self, Pop);
        self.parse_precedence(Precedence::AND);
        self.patch_jump(end_jump);

        self.leave();
    }

    /// or <expr>
    fn or(&mut self, _: bool) {
        self.enter("or");

        let end_jump = emit!(self, JumpIfTrue { offset: DUMMY });
        emit!(self, Pop);
        self.parse_precedence(Precedence::OR);
        self.patch_jump(end_jump);

        self.leave();
    }

    /// <number>
    fn number(&mut self, _: bool) {
        self.enter("number");

        let span = self.previous.span.anchor(self.lex.source());
        let slice = span.as_str();
        match slice.parse() {
            Ok(float) => {
                emit!(self, Constant { constant: Value::new_number(float) });
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

    /// " <string> "
    fn string(&mut self, _: bool) {
        self.enter("string");

        let span = self.previous.span.anchor(self.lex.source());
        let slice = span.as_str()
            .strip_prefix('"').unwrap()
            .strip_suffix('"').unwrap();
        let string = RoxString::new(slice);

        emit!(self, Constant { constant: Value::new_object(string) });

        self.leave();
    }

    /// <ident>
    fn variable(&mut self, can_assign: bool) {
        self.enter("variable");

        if let Some(index) = self.resolve_local(self.previous) {
            if can_assign && self.current.kind == TokenKind::Equal {
                self.advance();
                self.expression();
                emit!(self, SetLocal { index });
            } else {
                emit!(self, GetLocal { index });
            }
        } else {
            let name = self.identifier_constant(self.previous);
            let name = Value::new_object(name);

            if can_assign && self.current.kind == TokenKind::Equal {
                self.advance();
                self.expression();
                emit!(self, SetGlobal { constant: name });
            } else {
                emit!(self, GetGlobal { constant: name });
            }
        };

        self.leave();
    }

    /// nil | true | false
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
        OR,
        AND,
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

struct ParserRule<'ctx, 'src> {
    prefix: Option<fn(&mut Parser<'ctx, 'src>, bool)>,
    infix: Option<fn(&mut Parser<'ctx, 'src>, bool)>,
    precedence: Precedence,
}

fn parser_rule<'ctx, 'src>(kind: TokenKind) -> ParserRule<'ctx, 'src> {
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
        And             _           and         AND,
        Assert          _           _           _,
        Class           _           _           _,
        Else            _           _           _,
        False           literal     _           _,
        For             _           _           _,
        Fun             _           _           _,
        If              _           _           _,
        Nil             literal     _           _,
        Or              _           or          OR,
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
