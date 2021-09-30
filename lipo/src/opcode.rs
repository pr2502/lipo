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
    PopBlock { n: u8 },
    GetLocal { slot: u16 },
    SetLocal { slot: u16 },
    GetUpvalue { slot: u8 },
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
    Closure { fn_key: ConstKey, upvals: u8 },
    Return,
}

opcodes! {
    CONSTANT,
    UNIT,
    TRUE,
    FALSE,
    POP,
    POP_BLOCK,
    GET_LOCAL,
    SET_LOCAL,
    GET_UPVALUE,
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
    CLOSURE,
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
            [Self::POP_BLOCK, x, rest @ .. ] => (OpCode::PopBlock { n: *x }, rest),
            [Self::GET_LOCAL, x, y, rest @ .. ] => {
                (OpCode::GetLocal { slot: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::SET_LOCAL, x, y, rest @ .. ] => {
                (OpCode::SetLocal { slot: u16::from_le_bytes([*x, *y]) }, rest)
            }
            [Self::GET_UPVALUE, x, rest @ .. ] => {
                (OpCode::GetUpvalue { slot: *x }, rest)
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
            [Self::CLOSURE, x, y, z, rest @ .. ]  => {
                (OpCode::Closure { fn_key: ConstKey::from_le_bytes([*x, *y]), upvals: *z }, rest)
            }
            [Self::RETURN, rest @ .. ]    => (OpCode::Return, rest),
            _ => return None,
        })
    }

    pub fn encode(self, code: &mut Vec<u8>) {
        code.push(self.tag());
        match self {
            OpCode::Call { args: u8_arg } |
            OpCode::PopBlock { n: u8_arg } |
            OpCode::GetUpvalue { slot: u8_arg } => {
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
            OpCode::Closure { fn_key, upvals } => {
                code.extend(fn_key.to_le_bytes());
                code.push(upvals);
            }
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
            OpCode::PopBlock { .. }     => Self::POP_BLOCK,
            OpCode::GetLocal { .. }     => Self::GET_LOCAL,
            OpCode::SetLocal { .. }     => Self::SET_LOCAL,
            OpCode::GetUpvalue { .. }   => Self::GET_UPVALUE,
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
            OpCode::Closure { .. }      => Self::CLOSURE,
            OpCode::Return              => Self::RETURN,
        }
    }

    /// Encoded length
    #[allow(clippy::len_without_is_empty)]
    pub const fn len(self) -> usize {
        match self {
            // no arguments
            OpCode::Unit |
            OpCode::True |
            OpCode::False |
            OpCode::Pop |
            OpCode::Equal |
            OpCode::Greater |
            OpCode::Less |
            OpCode::Add |
            OpCode::Subtract |
            OpCode::Multiply |
            OpCode::Divide |
            OpCode::Not |
            OpCode::Negate |
            OpCode::Assert |
            OpCode::Print |
            OpCode::Return => 1,

            // one byte argument
            OpCode::Call { .. } |
            OpCode::PopBlock { .. } |
            OpCode::GetUpvalue { .. } => 2,

            // two byte argument
            OpCode::Constant { .. } |
            OpCode::GetLocal { .. } |
            OpCode::SetLocal { .. } |
            OpCode::Jump { .. } |
            OpCode::JumpIfTrue { .. } |
            OpCode::JumpIfFalse { .. } |
            OpCode::Loop { .. } |
            OpCode::Closure { .. } => 3,
        }
    }

    pub fn stack_effect(self) -> isize {
        match self {
            // return drops the whole stack frame,
            // it must be special cased in the stack tracking code
            OpCode::Return => unreachable!(),

            // doesn't modify the stack
            OpCode::Jump { .. } |
            OpCode::JumpIfTrue { .. } |
            OpCode::JumpIfFalse { .. } |
            OpCode::Loop { .. } => 0,

            // pop one push one
            OpCode::Not |
            OpCode::Negate => 0,

            // pushes to stack
            OpCode::Unit |
            OpCode::True |
            OpCode::False |
            OpCode::Constant { .. } |
            OpCode::GetLocal { .. } |
            OpCode::GetUpvalue { .. } => 1,

            // pops one element
            OpCode::Pop |
            OpCode::Assert |
            OpCode::Print |
            OpCode::SetLocal { .. } => -1,

            // binary ops, pop two push one
            OpCode::Equal |
            OpCode::Greater |
            OpCode::Less |
            OpCode::Add |
            OpCode::Subtract |
            OpCode::Multiply |
            OpCode::Divide => -1,

            // call, pops `args`, pushes one
            OpCode::Call { args } => -isize::from(args) + 1,

            // PopBlock pops N+1
            OpCode::PopBlock { n } => -isize::from(n) - 1,

            // closure, pops `upvals`, pushes one
            OpCode::Closure { upvals, .. } => -isize::from(upvals) + 1,
        }
    }
}
