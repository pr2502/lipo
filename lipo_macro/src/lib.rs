use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use std::env;
use syn::spanned::Spanned;
use syn::{parse_macro_input, Data, DeriveInput, Error, Fields, Index};


fn lipo_crate() -> impl quote::ToTokens {
    if env::var("CARGO_PKG_NAME").unwrap() == "lipo" {
        quote! { crate }
    } else {
        // Assumes lipo is never renamed in `Cargo.toml`
        // this is probably the best we can do.
        quote! { ::lipo }
    }
}


/// Derive macro generating an impl of the trait `Object`.
///
/// # Examples
///
/// ```rust
/// use lipo::object::builtins::String;
/// use lipo::object::{Object, ObjectRef, Trace};
///
/// #[derive(Object, Trace, Debug)]
/// struct MyString<'alloc>(ObjectRef<'alloc, String>);
/// ```
///
/// Object doesn't support type or const generic parameters.
///
/// ```rust,compile_fail
/// use lipo::object::Object;
///
/// #[derive(Object)]
/// struct Bad<T>(T);
/// ```
#[proc_macro_derive(Object)]
pub fn derive_object(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let lipo = lipo_crate();
    let object_ident = input.ident;
    let typename = object_ident.to_string();

    let lifetimes = input.generics.lifetimes();
    let lifetimes = quote! { <#(#lifetimes),*> };

    let unwanted_generic_params =
        input.generics.type_params().any(|_| true) || input.generics.const_params().any(|_| true);
    if unwanted_generic_params {
        return Error::new_spanned(
            // FIXME why does this lose span information?
            input.generics.params,
            "Object cannot have type or const generic parameters",
        )
        .into_compile_error()
        .into();
    }

    let expanded = quote! {
        unsafe impl #lifetimes #lipo::__derive_object::DynObject for #object_ident #lifetimes {
            fn __vtable() -> &'static #lipo::__derive_object::ObjectVtable {

                static VTABLE: #lipo::__derive_object::ObjectVtable = #lipo::__derive_object::ObjectVtable {
                    typename: #typename,
                    drop: #lipo::__derive_object::drop::<#object_ident>,
                    mark: #lipo::__derive_object::mark::<#object_ident>,
                    debug_fmt: #lipo::__derive_object::debug_fmt::<#object_ident>,
                    partial_eq: #lipo::__derive_object::partial_eq::<#object_ident>,
                    hash_code: #lipo::__derive_object::hash_code::<#object_ident>,
                };

                &VTABLE
            }
        }

        impl #lifetimes #lipo::object::Object for #object_ident #lifetimes {}
    };

    TokenStream::from(expanded)
}


/// Derive macro generating an impl of the trait `Object`.
#[proc_macro_derive(Trace)]
pub fn derive_trace(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let lipo = lipo_crate();

    let ty_ident = input.ident;

    let lifetimes = input.generics.lifetimes();
    let lifetimes = quote! { <#(#lifetimes),*> };

    let unwanted_generic_params =
        input.generics.type_params().any(|_| true) || input.generics.const_params().any(|_| true);
    if unwanted_generic_params {
        return Error::new_spanned(
            // FIXME why does this lose span information?
            input.generics.params,
            "Object cannot have type or const generic parameters",
        )
        .into_compile_error()
        .into();
    }

    let mark_children = mark_children(&input.data);

    let expanded = quote! {
        unsafe impl #lifetimes #lipo::object::Trace for #ty_ident #lifetimes {
            fn mark(&self) {
                #mark_children
            }
        }
    };

    TokenStream::from(expanded)
}

fn mark_children(data: &Data) -> proc_macro2::TokenStream {
    let lipo = lipo_crate();

    match data {
        Data::Struct(data) => {
            match &data.fields {
                Fields::Named(fields) => {
                    let children = fields.named.iter().map(|f| {
                        let name = &f.ident;
                        quote_spanned! {f.span()=>
                            #lipo::__derive_trace::MaybeTrace::maybe_mark(&self.#name);
                        }
                    });
                    quote! { #(#children)* }
                }
                Fields::Unnamed(fields) => {
                    let children = fields.unnamed.iter().enumerate().map(|(i, f)| {
                        let index = Index::from(i);
                        quote_spanned! {f.span()=>
                            #lipo::__derive_trace::MaybeTrace::maybe_mark(&self.#index);
                        }
                    });
                    quote! { #(#children)* }
                }
                Fields::Unit => quote! {},
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}
