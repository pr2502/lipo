
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


#[derive(Clone, Copy, Debug)]
pub enum OpCode {
    Constant { index: u16 },
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
}

opcodes! {
    CONSTANT,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NEGATE,
    RETURN,
}

impl OpCode {
    pub fn decode(code: &[u8]) -> Option<(OpCode, &[u8])> {
        Some(match code {
            [RETURN, rest @ .. ] => {
                (OpCode::Return, rest)
            }
            [ADD, rest @ .. ] => {
                (OpCode::Add, rest)
            }
            [SUBTRACT, rest @ .. ] => {
                (OpCode::Subtract, rest)
            }
            [MULTIPLY, rest @ .. ] => {
                (OpCode::Multiply, rest)
            }
            [DIVIDE, rest @ .. ] => {
                (OpCode::Divide, rest)
            }
            [NEGATE, rest @ .. ] => {
                (OpCode::Negate, rest)
            }
            [CONSTANT, x, y, rest @ .. ]  => {
                (OpCode::Constant { index: u16::from_le_bytes([*x, *y]) }, rest)
            }
            _ => return None,
        })
    }

    pub fn encode(self, code: &mut Vec<u8>) {
        match self {
            OpCode::Return => {
                code.push(RETURN);
            }
            OpCode::Add => {
                code.push(ADD);
            }
            OpCode::Subtract => {
                code.push(SUBTRACT);
            }
            OpCode::Multiply => {
                code.push(MULTIPLY);
            }
            OpCode::Divide => {
                code.push(DIVIDE);
            }
            OpCode::Negate => {
                code.push(NEGATE);
            }
            OpCode::Constant { index } => {
                code.push(CONSTANT);
                code.extend(index.to_le_bytes());
            }
        }
    }
}
