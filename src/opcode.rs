
/// Helper macro for defining u8 `const`s with unique values
macro_rules! opcodes {
    ( $( $ops:ident ),+ $(,)? ) => {
        opcodes!( @(0u8) $($ops)* );
    };
    ( @($n:expr) ) => {};
    ( @($n:expr) $op:ident $( $ops:ident )* ) => {
        const $op: u8 = $n;
        opcodes!( @($n + 1u8) $($ops)* );
    };
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OpCode {
    Constant { index: u16 },
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Return,
}

opcodes! {
    CONSTANT,
    NIL,
    TRUE,
    FALSE,
    EQUAL,
    GREATER,
    LESS,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NOT,
    NEGATE,
    RETURN,
}

impl OpCode {
    pub fn decode(code: &[u8]) -> Option<(OpCode, &[u8])> {
        Some(match code {
            [CONSTANT, x, y, rest @ .. ]  => {
                (OpCode::Constant { index: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [NIL, rest @ .. ]       => (OpCode::Nil, rest),
            [TRUE, rest @ .. ]      => (OpCode::True, rest),
            [FALSE, rest @ .. ]     => (OpCode::False, rest),
            [EQUAL, rest @ .. ]     => (OpCode::Equal, rest),
            [GREATER, rest @ .. ]   => (OpCode::Greater, rest),
            [LESS, rest @ .. ]      => (OpCode::Less, rest),
            [ADD, rest @ .. ]       => (OpCode::Add, rest),
            [SUBTRACT, rest @ .. ]  => (OpCode::Subtract, rest),
            [MULTIPLY, rest @ .. ]  => (OpCode::Multiply, rest),
            [DIVIDE, rest @ .. ]    => (OpCode::Divide, rest),
            [NOT, rest @ .. ]       => (OpCode::Not, rest),
            [NEGATE, rest @ .. ]    => (OpCode::Negate, rest),
            [RETURN, rest @ .. ]    => (OpCode::Return, rest),
            _ => return None,
        })
    }

    pub fn encode(self, code: &mut Vec<u8>) {
        match self {
            OpCode::Constant { index } => {
                code.push(CONSTANT);
                code.extend(index.to_le_bytes());
            }
            OpCode::Nil         => code.push(NIL),
            OpCode::True        => code.push(TRUE),
            OpCode::False       => code.push(FALSE),
            OpCode::Equal       => code.push(EQUAL),
            OpCode::Greater     => code.push(GREATER),
            OpCode::Less        => code.push(LESS),
            OpCode::Add         => code.push(ADD),
            OpCode::Subtract    => code.push(SUBTRACT),
            OpCode::Multiply    => code.push(MULTIPLY),
            OpCode::Divide      => code.push(DIVIDE),
            OpCode::Not         => code.push(NOT),
            OpCode::Negate      => code.push(NEGATE),
            OpCode::Return      => code.push(RETURN),
        }
    }
}
