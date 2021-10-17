use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::token::{Brace, Bracket, Comma};
use syn::{Expr, Field, FieldsNamed, Ident, Type, bracketed};


pub struct Args {
    opcodes: Punctuated<Opcode, Comma>,
}

#[derive(Clone)]
struct Opcode {
    _bracket_token: Bracket,
    name: Ident,
    args: Option<FieldsNamed>,
    pops: (Comma, Expr),
    pushes: Option<(Comma, Expr)>,
}

enum ArgTy {
    U8,
    U16,
}

impl Parse for Args {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Args {
            opcodes: input.parse_terminated(Opcode::parse)?,
        })
    }
}

impl Parse for Opcode {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Opcode {
            _bracket_token: bracketed!(content in input),
            name: content.parse()?,
            args: if content.peek(Brace) {
                Some(content.parse()?)
            } else {
                None
            },
            pops: (content.parse()?, content.parse()?),
            pushes: if content.peek(Comma) {
                Some((content.parse()?, content.parse()?))
            } else {
                None
            },
        })
    }
}

impl Opcode {
    fn args(&self) -> impl Iterator<Item = &Field> + '_ {
        self.args.iter().flat_map(|fields| fields.named.iter())
    }
}

fn to_upper_snake_case(ident: &Ident) -> Ident {
    let mut acc = String::new();
    let mut prev = '_';
    for ch in ident.to_string().chars() {
        if ch.is_uppercase() && prev != '_' {
            acc.push('_');
        }
        acc.push(ch);
        prev = ch;
    }
    Ident::new(&acc.to_uppercase(), Span::call_site())
}

fn arg_ty(ty: &Type) -> ArgTy {
    match ty {
        Type::Path(ty) => {
            match ty.path.get_ident() {
                Some(ident) => {
                    if ident == "u8" { ArgTy::U8 }
                    else if ident == "u16" { ArgTy::U16 }
                    else { panic!("unhandled argument type {}", ident) }
                },
                _ => panic!("type not an Ident"),
            }
        },
        _ => panic!("type not a Type::Path"),
    }
}

fn unique_idents() -> impl Iterator<Item = Ident> {
    (0..).map(|i| Ident::new(&format!("i{}", i), Span::call_site()))
}

pub fn define_opcodes(args: Args) -> TokenStream {
    let opcodes = args.opcodes
        .iter()
        .cloned()
        .collect::<Vec<_>>();

    let variants = opcodes.iter()
        .map(|opcode| {
            let name = &opcode.name;
            let fields = &opcode.args;
            quote! { #name #fields }
        });

    let impl_constants = impl_constants(&opcodes);
    let impl_decode = impl_decode(&opcodes);
    let impl_encode = impl_encode(&opcodes);
    let impl_len = impl_len(&opcodes);
    let impl_stack_effect = impl_stack_effect(&opcodes);

    quote! {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum OpCode {
            #(#variants),*
        }

        impl OpCode {
            #impl_constants
        }

        impl OpCode {
            #impl_decode
            #impl_encode
            #impl_len
            #impl_stack_effect
        }
    }
}

fn impl_constants(opcodes: &[Opcode]) -> TokenStream {
    let constants = opcodes.iter()
        .enumerate()
        .map(|(i, opcode)| {
            let i = u8::try_from(i).expect("too many opcodes");
            let name = to_upper_snake_case(&opcode.name);
            quote! { pub const #name : u8 = #i; }
        });
    quote! {
        #(#constants)*
    }
}

fn impl_decode(opcodes: &[Opcode]) -> TokenStream {
    let decode_arms = opcodes.iter()
        .map(|opcode| {
            let name = &opcode.name;
            let const_name = to_upper_snake_case(name);

            let mut match_idents = unique_idents();
            let match_args = opcode.args()
                .map(|field| {
                    match arg_ty(&field.ty) {
                        ArgTy::U8 => {
                            let i0 = match_idents.next().unwrap();
                            quote! { #i0 }
                        },
                        ArgTy::U16 => {
                            let i0 = match_idents.next().unwrap();
                            let i1 = match_idents.next().unwrap();
                            quote! { #i0, #i1 }
                        },
                    }
                });

            let mut decode_idents = unique_idents();
            let decode_args = opcode.args()
                .map(|field| {
                    let name = field.ident.as_ref().unwrap();

                    match arg_ty(&field.ty) {
                        ArgTy::U8 => {
                            let i0 = decode_idents.next().unwrap();
                            quote! { #name : *#i0 }
                        },
                        ArgTy::U16 => {
                            let i0 = decode_idents.next().unwrap();
                            let i1 = decode_idents.next().unwrap();
                            quote! { #name : u16::from_le_bytes([*#i0, *#i1]) }
                        },
                    }
                });

            quote! {
                [OpCode::#const_name, #(#match_args,)* rest @ ..] => {
                    Some((OpCode::#name { #(#decode_args),* }, rest))
                }
            }
        });
    quote! {
        pub fn decode(code: &[u8]) -> Option<(OpCode, &[u8])> {
            match code {
                #(#decode_arms),*
                _ => None,
            }
        }
    }
}

fn impl_encode(opcodes: &[Opcode]) -> TokenStream {
    let encode_arms = opcodes.iter()
        .map(|opcode| {
            let name = &opcode.name;
            let const_name = to_upper_snake_case(name);

            let match_args = opcode.args()
                .map(|field| {
                    let name = field.ident.as_ref().unwrap();
                    quote! { #name }
                });

            let encode_args = opcode.args()
                .map(|field| {
                    let name = field.ident.as_ref().unwrap();

                    match arg_ty(&field.ty) {
                        ArgTy::U8 => quote! {
                            code.push(#name);
                        },
                        ArgTy::U16 => quote! {
                            code.extend(#name.to_le_bytes());
                        },
                    }
                });

            quote! {
                OpCode::#name { #(#match_args),* } => {
                    code.push(OpCode::#const_name);
                    #(#encode_args)*
                }
            }
        });
    quote! {
        pub fn encode(self, code: &mut Vec<u8>) {
            match self {
                #(#encode_arms),*
            }
        }
    }
}

fn impl_len(opcodes: &[Opcode]) -> TokenStream {
    let len_arms = opcodes.iter()
        .map(|opcode| {
            let name = &opcode.name;
            let len = opcode.args()
                .map(|field| {
                    match arg_ty(&field.ty) {
                        ArgTy::U8 => 1,
                        ArgTy::U16 => 2,
                    }
                })
                .sum::<usize>() + 1usize;
            quote! { OpCode::#name { .. } => #len }
        });
    quote! {
        /// Encoded length
        #[allow(clippy::len_without_is_empty)]
        pub const fn len(self) -> usize {
            match self {
                #(#len_arms),*
            }
        }
    }
}

fn impl_stack_effect(opcodes: &[Opcode]) -> TokenStream {
    let stack_effect_arms = opcodes.iter()
        .map(|opcode| {
            let name = &opcode.name;

            let match_args = opcode.args()
                .map(|field| {
                    let name = field.ident.as_ref().unwrap();
                    quote! { #name }
                });

            let pops_pushes = if let Some((_, pushes)) = &opcode.pushes {
                let (_, pops) = &opcode.pops;
                let map_args = opcode.args()
                    .map(|field| {
                        let name = field.ident.as_ref().unwrap();
                        quote! { let #name = usize::from(#name); }
                    })
                    .collect::<Vec<_>>();
                let map_args = map_args.as_slice();

                quote! {(
                    {
                        #(#map_args)*
                        #pops
                    },
                    {
                        #(#map_args)*
                        #pushes
                    },
                )}
            } else {
                let (_, expr) = &opcode.pops;
                quote! {
                    #expr
                }
            };

            quote! {
                OpCode::#name { #(#match_args),* } => {
                    #pops_pushes
                }
            }
        });
    quote! {
        pub fn stack_effect(self) -> (usize, usize) {
            #![allow(unused_variables)]
            match self {
                #(#stack_effect_arms),*
            }
        }
    }
}
