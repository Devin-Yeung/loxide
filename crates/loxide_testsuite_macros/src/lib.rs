use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Item, ItemMod, Lit, LitStr};

#[proc_macro_attribute]
pub fn pin(attr: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as ItemMod);
    let attr = parse_macro_input!(attr as Lit);
    let attr = if let Lit::Str(s) = attr {
        let changed = format!("dummy::{}", s.value());
        Lit::Str(LitStr::new(&changed, s.span()))
    } else {
        panic!("Expected String Literal")
    };

    if let Some(ref mut items) = input.content {
        for item in &mut items.1 {
            if let Item::Macro(ref mut macro_item) = item {
                let mac = &mut macro_item.mac;
                let tokens = mac.tokens.clone();
                mac.tokens = quote! {
                    #attr,
                    #tokens
                };
            }
        }
    }

    let output = quote! {
        #input
    };
    output.into()
}
