use proc_macro::TokenStream;
use quote::quote;
use std::env;
use syn::{parse_macro_input, DeriveInput, Error};


fn lipo_crate() -> impl quote::ToTokens {
    if env::var("CARGO_PKG_NAME").unwrap() == "lipo" {
        quote! { crate }
    } else {
        // Assumes lipo is never renamed in `Cargo.toml`
        // this is probably the best we can do.
        quote! { ::lipo }
    }
}


/// Derive macro generating an impl of  the trait `Object`.
///
/// # Examples
///
/// ```rust
/// use lipo::object::{Object, ObjectRef, Trace};
/// use lipo::object::builtins::String;
///
/// #[derive(Object, Debug)]
/// struct MyString<'alloc>(ObjectRef<'alloc, String>);
///
/// // For now trace has to be implemented manually.
/// unsafe impl<'alloc> Trace for MyString<'alloc> {
///     fn mark(&self) {
///         self.0.mark();
///     }
/// }
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
        unsafe impl #lifetimes #lipo::object::DynObject for #object_ident #lifetimes {
            fn __vtable() -> &'static #lipo::object::ObjectVtable {

                static VTABLE: #lipo::object::ObjectVtable = #lipo::object::ObjectVtable {
                    typename: #typename,
                    drop: #lipo::object::__derive::drop::<#object_ident>,
                    mark: #lipo::object::__derive::mark::<#object_ident>,
                    debug_fmt: #lipo::object::__derive::debug_fmt::<#object_ident>,
                    partial_eq: #lipo::object::__derive::partial_eq::<#object_ident>,
                    hash_code: #lipo::object::__derive::hash_code::<#object_ident>,
                };

                &VTABLE
            }
        }

        impl #lifetimes #lipo::object::Object for #object_ident #lifetimes {}
    };

    TokenStream::from(expanded)
}
