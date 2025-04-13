# Macro Identifier toolbox

This crate is a collection of small tools for constructing identifiers inside `macro_rules!`
declarative macros. This can help reduce unnecessary use of the `syn` and `quote` crates.

This crate was originally inspired by a combination of the `gensym` and `paste` crates.
However, unlike `gensym` it does not use `syn` nor `quote`, and it is dumber than `paste`.
This crate instead implements a very basic expression evaluator which can combine commands.

Inside the macro body, `#` is used to identify a `mident` command. Commands output
either an identifier or a stream of tokens. Those which output an identifier can be combined
or nested, but those which output a token stream cannot.

The following commands output an identifier:
* `#rand` -- will be replaced by `__{uuid_v4_simple}`
* `#upcase ident` -- converts ASCII letters to uppercase
* `#downcase ident` -- converts ASCII letters to lowercase
* `#concat(ident_1, ident_2, ...)` -- concatenates identifiers only
* `#flatten(path)` -- converts a type path into an identifier, replacing non-alphanumeric symbols with `_{HEX}_`
* `#flatten_basename(path)` -- same as `#flatten`, but strips everything before the last `:` (thus extracting only the type name)

The following commands output a token stream:
* `#ty_path(path)` -- removes everything after the last `:` (thus returning the path to a type but not the type itself).
  Returns an empty token stream if there is no `:` in the input.

## Note regarding type path manipulation

This crate only has visibility into _tokens_ and does not have any information about types.
It cannot tell whether two types would be considered equivalent or coerce-able.
When using the `#flatten` command in multiple places, the output will only be an exact match
if they are spelled exactly the same (e.g. no type aliases or even differing qualified paths).

# Examples
```
// Common macro which creates a variable
macro_rules! gen_ident_ {
    ($ty:path, $e:expr, $i:ident) => {
        const $i: $ty = $e;
    };
}

// Create a random identifier
macro_rules! rand_ident {
    ($ty:path, $e:expr) => {
        mident::mident!{ gen_ident_!{$ty, $e, #rand} }
    };
}
rand_ident!(u32, 123);
rand_ident!(u32, 123);

// Convert to lowercase
macro_rules! downcase_ident {
    ($a:ident, $ty:path, $e:expr) => {
        mident::mident!{ gen_ident_!{$ty, $e, #downcase $a } }
    };
}
downcase_ident!(FoO, u32, 123);
// The variable was created as `foo`
const baz: u32 = foo;

// Concatenate several pieces, while also using multiple mident operations
macro_rules! concat_ident {
    ($a:ident, $b:ident, $ty:path, $e:expr) => {
        mident::mident!{ gen_ident_!{$ty, $e, #concat($a $b _ #upcase #concat($b $a)) } }
    };
}
concat_ident!(foo, bar, u32, 123);
const qux: u32 = foobar_BARFOO;

// Example of using `flatten`
macro_rules! flatten_ident {
    ($a:path, $ty:path, $e:expr) => {
        mident::mident!{ gen_ident_!{$ty, $e, #flatten_basename($a) } }
    };
}
flatten_ident!(::some::long::path::to::FooStruct<'static, u32, 5, 'a'>, u32, 123);
const uwu: u32 = FooStruct_3C__27_static_2C_u32_2C_5_2C__27_a_27__3E_;

// Example of using `ty_path`
macro_rules! ty_path_test {
    ($a:path, $e:expr) => {
        mident::mident!{
            const #rand: #ty_path($a) Vec<u32> = $e;
        }
    };
}
ty_path_test!(::std::vec::this<'is, 'all, 'ignored, 'static, u32, (owo, uwu)>, Vec::new());
```
