lipo_macro::define_opcodes! {
    //  Name        { args.. }                      pops        pushes
    [   Constant    { key: u16 },                   0,          1       ],
    [   Unit,                                       0,          1       ],
    [   True,                                       0,          1       ],
    [   False,                                      0,          1       ],
    [   Pop,                                        1,          0       ],
    [   PopBlock    { n: u8 },                      n + 1,      0       ],
    [   GetLocal    { slot: u16 },                  0,          1       ],
    [   SetLocal    { slot: u16 },                  1,          0       ],
    [   GetUpvalue  { slot: u8 },                   0,          1       ],
    [   GetTuple    { slot: u8 },                   1,          1       ],
    [   GetRecord   { name_key: u16 },              1,          1       ],
    [   Equal,                                      2,          1       ],
    [   Greater,                                    2,          1       ],
    [   Less,                                       2,          1       ],
    [   Add,                                        2,          1       ],
    [   Concat      { n: u8 },                      n + 2,      1       ],
    [   Subtract,                                   2,          1       ],
    [   Multiply,                                   2,          1       ],
    [   Divide,                                     2,          1       ],
    [   Not,                                        1,          1       ],
    [   Negate,                                     1,          1       ],
    [   MakeTuple   { len: u8 },                    len,        1       ],
    [   MakeRecord  { len: u8 },                    2 * len,    1       ],
    [   Assert,                                     1,          0       ],
    [   Print,                                      1,          0       ],
    [   Jump        { offset: u16 },                0,          0       ],
    [   JumpIfTrue  { offset: u16 },                0,          0       ],
    [   JumpIfFalse { offset: u16 },                0,          0       ],
    [   Loop        { offset: u16 },                0,          0       ],
    [   Call        { args: u8 },                   args + 1,   1       ],
    [   Closure     { fn_key: u16, upvals: u8 },    upvals,     1       ],
    [   Return,                                     unreachable!()      ],
}

#[allow(non_upper_case_globals)]
impl OpCode {
    pub(crate) const Jump_UNPATCHED: OpCode = OpCode::Jump { offset: u16::MAX };
    pub(crate) const JumpIfTrue_UNPATCHED: OpCode = OpCode::JumpIfTrue { offset: u16::MAX };
    pub(crate) const JumpIfFalse_UNPATCHED: OpCode = OpCode::JumpIfFalse { offset: u16::MAX };
}
