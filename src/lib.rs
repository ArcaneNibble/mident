//! ```
//! macro_rules! gen_ident_ {
//!     ($ty:path, $e:expr, $i:ident) => {
//!         const $i: $ty = $e;
//!     };
//! }
//!
//! macro_rules! rand_ident {
//!     ($ty:path, $e:expr) => {
//!         mident::mident!{ gen_ident_!{$ty, $e, #rand} }
//!     };
//! }
//!
//! macro_rules! upcase_ident {
//!     ($a:ident, $ty:path, $e:expr) => {
//!         mident::mident!{ gen_ident_!{$ty, $e, #upcase $a } }
//!     };
//! }
//!
//! upcase_ident!(foo, u32, 123);
//! const baz: u32 = FOO;
//!
//! rand_ident!(u32, 123);
//! rand_ident!(u32, 123);
//!
//! macro_rules! concat_ident {
//!     ($a:ident, $b:ident, $ty:path, $e:expr) => {
//!         mident::mident!{ gen_ident_!{$ty, $e, #concat($a $b _ #upcase #concat($b $a)) } }
//!     };
//! }
//!
//! concat_ident!(foo, bar, u32, 123);
//! const qux: u32 = foobar_BARFOO;
//!
//! macro_rules! flatten_ident {
//!     ($a:path, $ty:path, $e:expr) => {
//!         mident::mident!{ gen_ident_!{$ty, $e, #flatten_basename($a) } }
//!     };
//! }
//!
//! flatten_ident!(::FooStruct<'static, u32, 5, 'a'>, u32, 123);
//! const uwu: u32 = FooStruct_3C__27_static_2C_u32_2C_5_2C__27_a_27__3E_;
//!
//! macro_rules! ty_path_test {
//!     ($a:path, $e:expr) => {
//!         mident::mident!{
//!             const #rand: #ty_path($a) Vec<u32> = $e;
//!         }
//!     };
//! }
//!
//! ty_path_test!(::std::vec::FooStruct<'static, u32>, Vec::new());
//! ```

extern crate proc_macro;
use std::iter::Peekable;

use proc_macro::*;
use uuid::Uuid;

// either turn the group's contents into a peekable tokenstream, or
// unwrap one layer of Delimiter::None and then do the same
fn maybe_unwrap_group(group: Group) -> Peekable<token_stream::IntoIter> {
    let mut group_inner = group.stream().into_iter().peekable();
    if let Some(TokenTree::Group(group)) = group_inner.peek() {
        if group.delimiter() == Delimiter::None {
            group.stream().into_iter().peekable()
        } else {
            group_inner
        }
    } else {
        group_inner
    }
}

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

        "upcase" | "downcase" => {
            pos.next();
            let next = pos
                .peek()
                .expect("#{up|down}case must be followed by an arg");
            let mut new_id_str = match next {
                TokenTree::Ident(ident) => {
                    let s = ident.to_string();
                    pos.next();
                    s
                }
                TokenTree::Punct(..) => eval_mident_expr(pos, false)
                    .expect("can only #{up|down}case an ident or a mident command"),
                _ => {
                    panic!("can only #{{up|down}}case an ident or a mident command")
                }
            };
            if cmd == "upcase" {
                new_id_str.make_ascii_uppercase();
            } else {
                new_id_str.make_ascii_lowercase();
            }

            Some(new_id_str)
        }

        "concat" | "flatten" | "flatten_basename" => {
            pos.next();
            let group = pos
                .next()
                .expect("#flatten{_basename} must be followed by a group");
            let group = if let TokenTree::Group(group) = group {
                group
            } else {
                panic!("#flatten{{_basename}} must be followed by a group")
            };

            // attempt to unwrap one layer of macro blue paint
            let group_inner = maybe_unwrap_group(group);

            let mut new_id_str = String::new();
            let toks_to_concat = if cmd != "flatten_basename" {
                group_inner
            } else {
                // This is an ugly implementation. Find the last ':', but round-trip through a Vec
                let mut toks = group_inner.clone().collect::<Vec<_>>();
                let last_colon = toks.iter().rposition(|x| {
                    if let TokenTree::Punct(punct) = x {
                        punct.as_char() == ':'
                    } else {
                        false
                    }
                });
                if let Some(last_colon) = last_colon {
                    let new_toks = TokenStream::from_iter(toks.drain(last_colon + 1..));
                    new_toks.into_iter().peekable()
                } else {
                    group_inner
                }
            };
            fn flatten(
                cmd: &str,
                new_id_str: &mut String,
                mut toks: Peekable<token_stream::IntoIter>,
            ) {
                while let Some(tok) = toks.peek() {
                    match tok {
                        TokenTree::Ident(ident) => {
                            new_id_str.push_str(&ident.to_string());
                            toks.next();
                        }
                        TokenTree::Punct(punct) => {
                            let punct = punct.as_char();
                            let evaled = eval_mident_expr(&mut toks, false);
                            if let Some(evaled) = evaled {
                                new_id_str.push_str(&evaled);
                            } else {
                                if cmd == "concat" {
                                    panic!("can only #concat idents or mident commands")
                                }
                                new_id_str.push_str(&format!("_{:X}_", punct as i32));
                            }
                        }
                        TokenTree::Group(group) => {
                            if cmd == "concat" {
                                panic!("can only #concat idents or mident commands")
                            }
                            let (open, close) = match group.delimiter() {
                                Delimiter::Parenthesis => ("_28_", "_29_"),
                                Delimiter::Brace => ("_7B_", "_7D_"),
                                Delimiter::Bracket => ("_5B_", "_5D_"),
                                Delimiter::None => ("", ""),
                            };
                            let inner = group.stream().into_iter().peekable();
                            toks.next();
                            new_id_str.push_str(open);
                            flatten(cmd, new_id_str, inner);
                            new_id_str.push_str(close);
                        }
                        TokenTree::Literal(literal) => {
                            if cmd == "concat" {
                                panic!("can only #concat idents or mident commands")
                            }
                            for c in literal.to_string().chars() {
                                if c.is_ascii_alphanumeric() || c == '_' {
                                    new_id_str.push(c);
                                } else {
                                    new_id_str.push_str(&format!("_{:X}_", c as i32));
                                }
                            }
                            toks.next();
                        }
                    }
                }
            }
            flatten(cmd, &mut new_id_str, toks_to_concat);
            Some(new_id_str)
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

                    // recursive ident commands
                    if let Some(evaled) = eval_mident_expr(&mut self.input, true) {
                        let new_id = Ident::new(&evaled, Span::call_site());
                        return Some(TokenTree::Ident(new_id));
                    }

                    // other commands
                    if let TokenTree::Ident(ident) = self.input.peek()? {
                        if ident.to_string() == "ty_path" {
                            self.input.next();
                            let group = self
                                .input
                                .next()
                                .expect("#ty_path must be followed by a group");
                            let group = if let TokenTree::Group(group) = group {
                                group
                            } else {
                                panic!("#ty_path must be followed by a group")
                            };

                            // attempt to unwrap one layer of macro blue paint
                            let group_inner = maybe_unwrap_group(group);

                            // This is an ugly implementation. Find the last ':', but round-trip through a Vec
                            let mut toks = group_inner.collect::<Vec<_>>();
                            if let Some(last_colon) = toks.iter().rposition(|x| {
                                if let TokenTree::Punct(punct) = x {
                                    punct.as_char() == ':'
                                } else {
                                    false
                                }
                            }) {
                                let new_toks = TokenStream::from_iter(toks.drain(..last_colon + 1));
                                return Some(TokenTree::Group(Group::new(
                                    Delimiter::None,
                                    new_toks,
                                )));
                            } else {
                                return Some(TokenTree::Group(Group::new(
                                    Delimiter::None,
                                    TokenStream::new(),
                                )));
                            }
                        }
                    }
                }
            }
            _ => {}
        }

        Some(tok)
    }
}

/// Macro Identifier toolbox macro
#[proc_macro]
pub fn mident(input: TokenStream) -> TokenStream {
    TokenStream::from_iter(MidentInner::from(input))
}
