//! ```
//! macro_rules! gen_ident_ {
//!     ($ty:path, $e:expr, $i:ident) => {
//!         const $i: $ty = $e;
//!     };
//! }
//!
//! macro_rules! gen_ident {
//!     ($ty:path, $e:expr) => {
//!         mident::mident!{ gen_ident_!{$ty, $e, #rand} }
//!     };
//! }
//!
//! gen_ident!(u32, 123);
//! gen_ident!(u32, 123);
//!
//! macro_rules! concat_ident_ {
//!     ($ty:path, $e:expr, $i:ident) => {
//!         const $i: $ty = $e;
//!     };
//! }
//!
//! macro_rules! concat_ident {
//!     ($a:ident, $b:ident, $ty:path, $e:expr) => {
//!         mident::mident!{ concat_ident_!{$ty, $e, #concat($a $b _ #concat($b $a)) } }
//!     };
//! }
//!
//! concat_ident!(foo, bar, u32, 123);
//! const baz: u32 = foobar_barfoo;
//! ```

extern crate proc_macro;
use std::iter::Peekable;

use proc_macro::*;
use uuid::Uuid;

fn eval_mident_expr(
    pos: &mut Peekable<token_stream::IntoIter>,
    already_got_hash: bool,
) -> Option<String> {
    if !already_got_hash {
        let hash = pos.next()?;
        let hash = if let TokenTree::Punct(punct) = hash {
            punct
        } else {
            return None;
        };
        if !(hash.as_char() == '#' && hash.spacing() == Spacing::Alone) {
            return None;
        }
    }

    // #   mident_command (args)
    //   ^ must be here right now

    let cmd = pos.peek()?;
    let cmd = if let TokenTree::Ident(ident) = cmd {
        ident
    } else {
        return None;
    };
    let cmd = cmd.to_string();
    let cmd = cmd.as_str();

    // #   mident_command   (args)
    //   ^ cursor still here

    match cmd {
        "rand" => {
            pos.next();
            let new_id_str = format!("__{}", Uuid::new_v4().simple());
            Some(new_id_str)
        }

        "concat" => {
            pos.next();
            let group = pos.next().expect("#concat must be followed by a group");
            let group = if let TokenTree::Group(group) = group {
                group
            } else {
                panic!("#concat must be followed by a group")
            };

            let mut new_id_str = String::new();
            let mut toks_to_concat = Peekable::from(group.stream().into_iter().peekable());
            while let Some(tok) = toks_to_concat.peek() {
                match tok {
                    TokenTree::Ident(ident) => {
                        new_id_str.push_str(&ident.to_string());
                        toks_to_concat.next();
                    }
                    TokenTree::Punct(..) => {
                        let evaled = eval_mident_expr(&mut toks_to_concat, false)
                            .expect("can only #concat idents or mident commands");
                        new_id_str.push_str(&evaled);
                    }
                    _ => {
                        panic!("can only #concat idents or mident commands")
                    }
                }
            }
            return Some(new_id_str);
        }
        _ => None,
    }
}

struct MidentInner {
    input: Peekable<token_stream::IntoIter>,
}
impl From<TokenStream> for MidentInner {
    fn from(value: TokenStream) -> Self {
        Self {
            input: value.into_iter().peekable(),
        }
    }
}
impl Iterator for MidentInner {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.input.next()?;
        match tok {
            TokenTree::Group(group) => {
                // recurse
                let new_inner = TokenStream::from_iter(Self::from(group.stream()));
                return Some(TokenTree::Group(Group::new(group.delimiter(), new_inner)));
            }
            TokenTree::Punct(ref punct) => {
                if punct.as_char() == '#' && punct.spacing() == Spacing::Alone {
                    // a hash mark
                    if let Some(evaled) = eval_mident_expr(&mut self.input, true) {
                        let new_id = Ident::new(&evaled, Span::call_site());
                        return Some(TokenTree::Ident(new_id));
                    }
                }
            }
            _ => {}
        }

        Some(tok)
    }
}

#[proc_macro]
pub fn mident(input: TokenStream) -> TokenStream {
    dbg!(&input);
    let output = TokenStream::from_iter(MidentInner::from(input));
    dbg!(&output);
    output
}
