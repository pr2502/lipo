use crate::chunk::ConstKey;


/// Helper macro for defining u8 `const`s with unique values
macro_rules! opcodes {
    ( $( $ops:ident ),+ $(,)? ) => {
        impl OpCode {
            opcodes!( @(0u8) $($ops)* );
        }
    };
    ( @($n:expr) ) => {};
    ( @($n:expr) $op:ident $( $ops:ident )* ) => {
        pub const $op: u8 = $n;
        opcodes!( @($n + 1u8) $($ops)* );
    };
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OpCode {
    Constant { key: ConstKey },
    Unit,
    True,
    False,
    Pop,
    GetLocal { slot: u16 },
    SetLocal { slot: u16 },
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Assert,
    Print,
    Jump { offset: u16 },
    JumpIfTrue { offset: u16 },
    JumpIfFalse { offset: u16 },
    Loop { offset: u16 },
    Call { args: u8 },
    Return,
}

opcodes! {
    CONSTANT,
    UNIT,
    TRUE,
    FALSE,
    POP,
    GET_LOCAL,
    SET_LOCAL,
    EQUAL,
    GREATER,
    LESS,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NOT,
    NEGATE,
    ASSERT,
    PRINT,
    JUMP,
    JUMP_IF_TRUE,
    JUMP_IF_FALSE,
    LOOP,
    CALL,
    RETURN,
}

impl OpCode {
    pub fn decode(code: &[u8]) -> Option<(OpCode, &[u8])> {
        Some(match code {
            [Self::CONSTANT, x, y, rest @ .. ]  => {
                (OpCode::Constant { key: ConstKey::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::UNIT, rest @ .. ]      => (OpCode::Unit, rest),
            [Self::TRUE, rest @ .. ]      => (OpCode::True, rest),
            [Self::FALSE, rest @ .. ]     => (OpCode::False, rest),
            [Self::POP, rest @ .. ]       => (OpCode::Pop, rest),
            [Self::GET_LOCAL, x, y, rest @ .. ] => {
                (OpCode::GetLocal { slot: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::SET_LOCAL, x, y, rest @ .. ] => {
                (OpCode::SetLocal { slot: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::EQUAL, rest @ .. ]     => (OpCode::Equal, rest),
            [Self::GREATER, rest @ .. ]   => (OpCode::Greater, rest),
            [Self::LESS, rest @ .. ]      => (OpCode::Less, rest),
            [Self::ADD, rest @ .. ]       => (OpCode::Add, rest),
            [Self::SUBTRACT, rest @ .. ]  => (OpCode::Subtract, rest),
            [Self::MULTIPLY, rest @ .. ]  => (OpCode::Multiply, rest),
            [Self::DIVIDE, rest @ .. ]    => (OpCode::Divide, rest),
            [Self::NOT, rest @ .. ]       => (OpCode::Not, rest),
            [Self::NEGATE, rest @ .. ]    => (OpCode::Negate, rest),
            [Self::ASSERT, rest @ .. ]    => (OpCode::Assert, rest),
            [Self::PRINT, rest @ .. ]     => (OpCode::Print, rest),
            [Self::JUMP, x, y, rest @ .. ] => {
                (OpCode::Jump { offset: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::JUMP_IF_TRUE, x, y, rest @ .. ] => {
                (OpCode::JumpIfTrue { offset: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::JUMP_IF_FALSE, x, y, rest @ .. ] => {
                (OpCode::JumpIfFalse { offset: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::LOOP, x, y, rest @ .. ] => {
                (OpCode::Loop { offset: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::CALL, x, rest @ .. ] => (OpCode::Call { args: *x }, rest),
            [Self::RETURN, rest @ .. ]    => (OpCode::Return, rest),
            _ => return None,
        })
    }

    pub fn encode(self, code: &mut Vec<u8>) {
        code.push(self.tag());
        match self {
            OpCode::Call { args: u8_arg } => {
                code.push(u8_arg);
            }
            OpCode::Constant { key: key_arg } => {
                code.extend(key_arg.to_le_bytes());
            }
            OpCode::GetLocal { slot: u16_arg } |
            OpCode::SetLocal { slot: u16_arg } |
            OpCode::Jump { offset: u16_arg } |
            OpCode::JumpIfTrue { offset: u16_arg } |
            OpCode::JumpIfFalse { offset: u16_arg } |
            OpCode::Loop { offset: u16_arg } => {
                code.extend(u16_arg.to_le_bytes());
            },
            _ => {}
        }
    }

    pub const fn tag(self) -> u8 {
        match self {
            OpCode::Constant { .. }     => Self::CONSTANT,
            OpCode::Unit                => Self::UNIT,
            OpCode::True                => Self::TRUE,
            OpCode::False               => Self::FALSE,
            OpCode::Pop                 => Self::POP,
            OpCode::GetLocal { .. }     => Self::GET_LOCAL,
            OpCode::SetLocal { .. }     => Self::SET_LOCAL,
            OpCode::Equal               => Self::EQUAL,
            OpCode::Greater             => Self::GREATER,
            OpCode::Less                => Self::LESS,
            OpCode::Add                 => Self::ADD,
            OpCode::Subtract            => Self::SUBTRACT,
            OpCode::Multiply            => Self::MULTIPLY,
            OpCode::Divide              => Self::DIVIDE,
            OpCode::Not                 => Self::NOT,
            OpCode::Negate              => Self::NEGATE,
            OpCode::Assert              => Self::ASSERT,
            OpCode::Print               => Self::PRINT,
            OpCode::Jump { .. }         => Self::JUMP,
            OpCode::JumpIfTrue { .. }   => Self::JUMP_IF_TRUE,
            OpCode::JumpIfFalse { .. }  => Self::JUMP_IF_FALSE,
            OpCode::Loop { .. }         => Self::LOOP,
            OpCode::Call { .. }         => Self::CALL,
            OpCode::Return              => Self::RETURN,
        }
    }
}
