
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
    Constant { index: u16 }, // indexes into the chunk constant pool
    Nil,
    True,
    False,
    Pop,
    GetLocal { index: u16 }, // indexes into the stack
    SetLocal { index: u16 }, // indexes into the stack
    GetGlobal { index: u16 }, // indexes into the chunk constant pool
    DefGlobal { index: u16 }, // indexes into the chunk constant pool
    SetGlobal { index: u16 }, // indexes into the chunk constant pool
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
    Return,
}

opcodes! {
    CONSTANT,
    NIL,
    TRUE,
    FALSE,
    POP,
    GET_LOCAL,
    SET_LOCAL,
    GET_GLOBAL,
    DEF_GLOBAL,
    SET_GLOBAL,
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
    RETURN,
}

impl OpCode {
    pub fn decode(code: &[u8]) -> Option<(OpCode, &[u8])> {
        Some(match code {
            [Self::CONSTANT, x, y, rest @ .. ]  => {
                (OpCode::Constant { index: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::NIL, rest @ .. ]       => (OpCode::Nil, rest),
            [Self::TRUE, rest @ .. ]      => (OpCode::True, rest),
            [Self::FALSE, rest @ .. ]     => (OpCode::False, rest),
            [Self::POP, rest @ .. ]       => (OpCode::Pop, rest),
            [Self::GET_LOCAL, x, y, rest @ .. ] => {
                (OpCode::GetLocal { index: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::SET_LOCAL, x, y, rest @ .. ] => {
                (OpCode::SetLocal { index: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::GET_GLOBAL, x, y, rest @ .. ] => {
                (OpCode::GetGlobal { index: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::DEF_GLOBAL, x, y, rest @ .. ] => {
                (OpCode::DefGlobal { index: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::SET_GLOBAL, x, y, rest @ .. ] => {
                (OpCode::SetGlobal { index: u16::from_le_bytes([*x, *y]) }, rest)
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
            [Self::RETURN, rest @ .. ]    => (OpCode::Return, rest),
            _ => return None,
        })
    }

    pub fn encode(self, code: &mut Vec<u8>) {
        code.push(self.tag());
        match self {
            OpCode::Constant { index: u16_arg } |
            OpCode::GetLocal { index: u16_arg } |
            OpCode::SetLocal { index: u16_arg } |
            OpCode::GetGlobal { index: u16_arg } |
            OpCode::DefGlobal { index: u16_arg } |
            OpCode::SetGlobal { index: u16_arg } |
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
            OpCode::Nil                 => Self::NIL,
            OpCode::True                => Self::TRUE,
            OpCode::False               => Self::FALSE,
            OpCode::Pop                 => Self::POP,
            OpCode::GetLocal { .. }     => Self::GET_LOCAL,
            OpCode::SetLocal { .. }     => Self::SET_LOCAL,
            OpCode::GetGlobal { .. }    => Self::GET_GLOBAL,
            OpCode::DefGlobal { .. }    => Self::DEF_GLOBAL,
            OpCode::SetGlobal { .. }    => Self::SET_GLOBAL,
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
            OpCode::Return              => Self::RETURN,
        }
    }
}
