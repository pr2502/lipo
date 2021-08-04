
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
    Return,
}

opcodes! {
    CONSTANT,
    RETURN,
}

impl OpCode {
    pub fn parse(code: &[u8]) -> Option<(OpCode, &[u8])> {
        Some(match code {
            [RETURN, rest @ .. ] => {
                (OpCode::Return, rest)
            }
            [CONSTANT, x, y, rest @ .. ]  => {
                (OpCode::Constant { index: u16::from_le_bytes([*x, *y]) }, rest)
            }
            _ => return None,
        })
    }

    pub fn write(self, code: &mut Vec<u8>) {
        match self {
            OpCode::Return => {
                code.push(RETURN);
            }
            OpCode::Constant { index } => {
                code.push(CONSTANT);
                code.extend(index.to_le_bytes());
            }
        }
    }
}
