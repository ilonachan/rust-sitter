//! Provides the proc-macros required to make Rust-Sitter work.
//! 
//! ## TODOs
//! 
//! ### Tokens
//! A whole rule tree can be marked as a TOKEN to make
//! Tree-Sitter match it normally, but only output it as one whole token.
//! As the grammar is built through Rust type tree structures, this requires
//! the user to define the type of the result in a way that this structure
//! must never be fulfilled.
//!   
//! Create a `LexedToken` type, which takes the grammar structure as a generic argument,
//! but in practice always evaluates to a `String` (or a type produced by a mapping function).
//! 
//! Once tokens are allowed, giving them the option to be "immediate" is trivial. But possibly
//! other leaf nodes could also benefit from that functionality.
//! 
//! ### Conflict Resolution & Dynamic Precedence
//! Tree-Sitter allows specifying a list of known conflict situations, i.e. sets of rules that might
//! be equally valid to match in some location. Normally this would cause the parser to abort, but
//! if the conflict is known about then it'll instead do the following:
//! - Try each of the possible branches.
//! - If exactly one works, match that. If none work, it's an error.
//! - If more than one branch could match, calculate their *dynamic precedences*
//!   and pick the one with the highest value.
//! 
//! The dynamic precedence values are attached to certain rules, and this is already implemented.
//! The bigger issue would be how to describe these conflicts... possibly just a simple macro call might work.
//! 
//! ### Relative Precedences
//! Rather than using numeric precedence to globally order all rules (where the default value is 0),
//! Tree-Sitter allows specifying known strings of relative comparisons by name. For example, we could
//! specify that [addition < multiplication] in terms of precedence, but we wouldn't be forced to make
//! claims about [multiplication <=> exponentiation] or such. Even more, we can *also* claim that
//! [exponentiation < multiplication], but this wouldn't force us to describe [addition <=> exponentiation].
//! 
//! The question is how this would be described in rust code... possibly just using a simple macro call
//! for each of these precedence chains.
//! 
//! ### External Scanners
//!   - Anonymous external tokens actually work already! The big thing to implement now will be a way
//!     to reference these tokens in the rest of the grammar, which should work exactly as referencing
//!     any other rule.
//!   - External tokens can still have regular rule definitions, and these will apply if the scanner
//!     doesn't match anything special itself. But how do we define these? The usual data tree model
//!     would require that structure to be filled at runtime... but if the external scanner does end
//!     up matching, no data beyond the returned token string can be provided! This problem might be
//!     similar to the Token rule described above, and could be solved similarly: By having a single
//!     field which can either just get the token, or get the data structure if applicable (which is
//!     potentially never, but this would define the data structure sufficiently).
//! 
//! 
//! 
//! ### Bonus things:
//! - Allow user to make field names and aliases, as well as to arbitrarily rename rules.
//!   These actions don't affect the way the API can be used, but they might make this library
//!   more broadly useful for generating readable Tree-Sitter `grammar.json` definitions.
//!   (full flexibility is unfortunately unachievable, however: field names are already used for
//!    technical workarounds.)
//! - Inline Rules: a parser technicality, where the specified rules never appear in the parse tree
//!   and are instead replaced by their children wherever they appear. This seems very similar to
//!   Hidden Rules, which seem to do the same thing? I'm probably misunderstanding something. Either way,
//!   the fact that these nodes don't appear in the syntax tree could be a massive problem, because that
//!   makes it very hard for Rust-Sitter to predictably parse the tree structure
//!   (perhaps this can be worked around, but is it worth it?)
//! - The Tree-Sitter generation program can also produce a `node-types.json` file, which seemingly
//!   describes all the rules used. Only rules that are actually used appear there, which excludes inlined
//!   or hidden rules; adding them to a `supertypes` list forces them to appear anyway.
//!   But none of this seems relevant to implement.

use proc_macro_error::proc_macro_error;
use quote::ToTokens;
use syn::{parse_macro_input, AttributeArgs, ItemMod};

mod expansion;
use expansion::*;

#[proc_macro_attribute]
/// Marks the top level AST node where parsing should start.
///
/// ## Example
/// ```ignore
/// #[rust_sitter::language]
/// pub struct Code {
///     ...
/// }
/// ```
pub fn language(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

/// Marks a type implementing the [`ExternalScanner`](rust_sitter::external_scanner::ExternalScanner)
/// trait to be hooked into the tree-sitter external scanning API. This allows complex
/// scanner logic to be written fully in Rust.
/// 
/// The first argument is any type which implements the interface, and which through that interface
/// will be instantiated and used as a scanner. The second argument is an Identifier to be used 
/// as the name of a dummy enum listing all external tokens (see [`external_token`]): when omitted
/// this defaults to `ExternalTokens`
///
/// ## Example
/// ```ignore
/// pub struct ExternalScanner {
///     ...
/// }
/// impl rust_sitter::ExternalScanner for ExternalScanner {
///     type Tokens = CustomTokens;
///     
///     fn new() -> Self { ... }
///     fn serialize(&mut self) -> Vec<u8> { ... }
///     fn deserialize(&mut self, buffer: &[u8]) { ... }
///     fn scan(&mut self, ...) -> Option<Self::Tokens> { ... }
/// }
/// 
/// rust_sitter::external_scanner!(ExternalScanner, CustomTokens);
/// ```
#[proc_macro]
pub fn external_scanner(
    // _attr: proc_macro::TokenStream,
    _item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    proc_macro::TokenStream::new()
}

#[proc_macro_attribute]
/// Mark a struct as an external token for this grammar.
/// This way these tokens can then be used in the regular token graph.
/// 
/// Providing a string argument makes this an _anonymous_ external token, and means
/// that wherever that string terminal occurs in the grammar, matching will first be
/// delegated to the external parser.
/// 
/// An external token can be a unit struct, or it can contain exactly one field of the following types:
/// - `String`: Return the external scanner result as a string, which is the best that can be gotten.
/// - `T`: The external scanner result is still returned as a string, but can optionally be run through a
///     `transform` function `Fn(String) -> T`.
/// - [`LexedToken<L, T=String>`](rust_sitter::LexedToken): Define an internal Tree-Sitter grammar structure
///     to be matched (as type `L`) and returned if the external scanner didn't match. If it did, this is like `String` or `T`.
///     (Note that in an anonymous external token, the Lexed type may only be `T` and
///     is also run through the `transform` function (or a separate one))
/// - One of the types above, wrapped in [`Spanned`].
/// 
/// These tokens are also collected in the order in which they're defined,
/// and their names are used to define variants of an auto-generated
/// [`ExternalTokens`](rust_sitter::external_scanner::ExternalTokens) enum.
/// The name of this enum is defined by the identifier used in the scanner
/// impl block annotated with [`external_scanner`].
///
/// ## Example
/// ```ignore
/// #[rust_sitter::external_token]
/// pub struct ExtToken1;
/// #[rust_sitter::external_token(transform=|v| v.parse().unwrap())]
/// pub struct ExtToken2(u32);
/// #[rust_sitter::external_token("+")]
/// pub struct ExtTokenPlus;
/// #[rust_sitter::external_token(transform=|v| v.parse().unwrap())]
/// pub struct ExtTokenComplex {
///     data: Spanned<LexedToken<ExistingToken, u64>>
/// };
/// 
/// // this will generate an enum like this (name taken from the scanner impl):
/// pub enum CustomTokens {
///     ExtToken1, ExtToken2, ExtTokenPlus, ExtTokenComplex
/// }
/// ```
pub fn external_token(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}


#[proc_macro_attribute]
/// This annotation marks a node as extra, which can safely be skipped while parsing.
/// This is useful for handling whitespace/newlines/comments.
///
/// ## Example
/// ```ignore
/// #[rust_sitter::extra]
/// struct Whitespace {
///     #[rust_sitter::leaf(pattern = r"\s")]
///     _whitespace: (),
/// }
/// ```
pub fn extra(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Marks the specified field as a Tree Sitter [word](https://tree-sitter.github.io/tree-sitter/creating-parsers#keywords),
/// which is useful when handling errors involving keywords. Only one field in the grammar can be marked as a word.
/// 
/// Essentially, the problem being solved is that keywords like `if` or `implements` could also
/// just as well be identifiers. Problems arise when things like "ifHello" should be matched: this could
/// be matched as the identifier "ifHello" (correct), *or* it could be the keyword "if" followed by the unrelated
/// token "Hello". Apparently Tree-Sitter will default to the latter, because it doesn't understand this relation
/// out of the box.
/// 
/// Marking a rule as a `word` has the following effects:
/// - Tree-Sitter will look at all string terminals, and check if they match the `word` rule. These are now _keywords_.
/// - Whenever a keyword is encountered, rather than just letting the specific keyword be matched and continue, the `word` rule
///   is matched instead. In the example case it means that the entire "ifHello" token is matched.
/// - If whatever was matched was actually just the keyword itself, all is well. Otherwise we don't consider that to be a match.
///   This is now correct behavior in our example.
/// 
/// Therefore, in most use cases the rule to be marked as `word` would likely be whatever identifier type would fit your keywords
/// if they weren't reserved. It makes conceptual sense that the parser would benefit from understanding this, and it also
/// seems to have performance benefits.
///
/// ## Example
/// ```ignore
/// #[rust_sitter::word]
/// Identifier(...)
/// ```
pub fn word(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Defines a field which matches a specific token in the source string.
/// The token can be defined by passing one of two arguments
/// - `text`: a string literal that will be exactly matched
/// - `pattern`: a regular expression that will be matched against the source string
///
/// If the resulting token needs to be converted into a richer type at runtime,
/// such as a number, then the `transform` argument can be used to specify a function
/// that will be called with the token's text.
///
/// The attribute can also be applied to a struct or enum variant with no fields.
///
/// ## Examples
///
/// Using the `leaf` attribute on a field:
/// ```ignore
/// struct Number(
///     #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
///     u32
/// )
/// ```
///
/// Using the attribute on a unit struct or unit enum variant:
/// ```ignore
/// #[rust_sitter::leaf(text = "9")]
/// struct BigDigit;
///
/// enum SmallDigit {
///     #[rust_sitter::leaf(text = "0")]
///     Zero,
///     #[rust_sitter::leaf(text = "1")]
///     One,
/// }
/// ```
///
pub fn leaf(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Defines a field that does not correspond to anything in the input string,
/// such as some metadata. Takes a single, unnamed argument, which is the value
/// used to populate the field at runtime.
///
/// ## Example
/// ```ignore
/// struct MyNode {
///    ...,
///    #[rust_sitter::skip(false)]
///    node_visited: bool
/// }
/// ```
/// 
/// Can also be used on structs and enums, to declare that they are not to be treated
/// as part of the grammar. This will mostly be useful for the struct underlying an
/// [`external_scanner`].
pub fn skip(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Defines a precedence level for a non-terminal, and optionally the associativity to be used
/// when conflicts arise.
///
/// This annotation can take a single unnamed parameter, which specifies the precedence level.
/// This is used to resolve conflicts with other non-terminals, so that the one with the higher
/// precedence will bind more tightly (appear lower in the parse tree).
///
/// The annotation can also take the named parameter `assoc` with possible values `left` | `right` | `dynamic`.
/// This is used to resolve conflicts of the same terminal matching in intersecting ways, by prefering
/// one matching direction over the other.
///
/// An example of left-associativity is subtraction: we expect `1 - 2 - 3` to be parsed as `(1 - 2) - 3`. For
/// right-associativity, an example might be exponentiation (possibly). Dynamic associativity refers to a
/// precedence resolution algorithm described by [Tree-Sitter](https://tree-sitter.github.io/tree-sitter/creating-parsers#the-grammar-dsl)
/// and [Bison docs](https://www.gnu.org/software/bison/manual/html_node/Generalized-LR-Parsing.html).
///
/// If left- or right-associativity is specified, the precedence can be omitted. In that case the default value
/// of 0 is used. However, it usually makes sense to specify a precedence anyway, to avoid conflicts between different terminals.
///
/// ## Example
/// ```ignore
/// #[rust_sitter::prec(1, assoc=left)]
/// Subtract(Box<Expr>, Box<Expr>)
/// ```
pub fn prec(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Defines a precedence level for a non-terminal that should be left-associative.
/// For example, with subtraction we expect 1 - 2 - 3 to be parsed as (1 - 2) - 3,
/// which corresponds to a left-associativity.
///
/// This annotation takes a single, unnamed parameter, which specifies the precedence level.
/// This is used to resolve conflicts with other non-terminals, so that the one with the higher
/// precedence will bind more tightly (appear lower in the parse tree).
///
/// This is an alternate syntax for `#[rust_sitter::prec(assoc=left, 1)]`.
/// Only one such attribute can be specified.
///
/// ## Example
/// ```ignore
/// #[rust_sitter::prec_left(1)]
/// Subtract(Box<Expr>, Box<Expr>)
/// ```
pub fn prec_left(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Defines a precedence level for a non-terminal that should be right-associative.
/// For example, with cons we could have 1 :: 2 :: 3 to be parsed as 1 :: (2 :: 3),
/// which corresponds to a right-associativity.
///
/// This annotation takes a single, unnamed parameter, which specifies the precedence level.
/// This is used to resolve conflicts with other non-terminals, so that the one with the higher
/// precedence will bind more tightly (appear lower in the parse tree).
///
/// This is an alternate syntax for `#[rust_sitter::prec(assoc=right, 1)]`.
/// Only one such attribute can be specified.
/// 
/// ## Example
/// ```ignore
/// #[rust_sitter::prec_right(1)]
/// Cons(Box<Expr>, Box<Expr>)
/// ```
pub fn prec_right(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// On `Vec<_>` typed fields, specifies a non-terminal that should be parsed in between the elements.
/// The [`rust_sitter::repeat`] annotation must be used on the field as well.
///
/// This annotation takes a single, unnamed argument, which specifies a field type to parse. This can
/// either be a reference to another type, or can be defined as a `leaf` field. Generally, the argument
/// is parsed using the same rules as an unnamed field of an enum variant.
///
/// ## Example
/// ```ignore
/// #[rust_sitter::delimited(
///     #[rust_sitter::leaf(text = ",")]
///     ()
/// )]
/// numbers: Vec<Number>
/// ```
pub fn delimited(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// On `Vec<_>` typed fields, specifies additional config for how the repeated elements should
/// be parsed. In particular, this annotation takes the following named arguments:
/// - `non_empty` - if this argument is `true`, then there must be at least one element parsed
///
/// ## Example
/// ```ignore
/// #[rust_sitter::repeat(non_empty = true)]
/// numbers: Vec<Number>
/// ```
pub fn repeat(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

/// Mark a module to be analyzed for a Rust Sitter grammar. Takes a single, unnamed argument, which
/// specifies the name of the grammar. This name must be unique across all Rust Sitter grammars within
/// a compilation unit.
#[proc_macro_error]
#[proc_macro_attribute]
pub fn grammar(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attrs: AttributeArgs = parse_macro_input!(attr);
    let module: ItemMod = parse_macro_input!(input);
    let expanded: ItemMod = expand_grammar(syn::parse_quote! {
        #[rust_sitter::grammar[#(#attrs),*]]
        #module
    });
    proc_macro::TokenStream::from(expanded.to_token_stream())
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::{Read, Write};
    use std::process::Command;

    use quote::ToTokens;
    use syn::parse_quote;
    use tempfile::tempdir;

    use super::expand_grammar;

    fn rustfmt_code(code: &str) -> String {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("temp.rs");
        let mut file = File::create(file_path.clone()).unwrap();

        writeln!(file, "{code}").unwrap();
        drop(file);

        Command::new("rustfmt")
            .arg(file_path.to_str().unwrap())
            .spawn()
            .unwrap()
            .wait()
            .unwrap();

        let mut file = File::open(file_path).unwrap();
        let mut data = String::new();
        file.read_to_string(&mut data).unwrap();
        drop(file);
        dir.close().unwrap();
        data
    }

    #[test]
    fn enum_transformed_fields() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub enum Expression {
                        Number(
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse::<i32>().unwrap())]
                            i32
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn enum_recursive() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub enum Expression {
                        Number(
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                            i32
                        ),
                        Neg(
                            #[rust_sitter::leaf(text = "-")]
                            (),
                            Box<Expression>
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn enum_prec_left() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub enum Expression {
                        Number(
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                            i32
                        ),
                        #[rust_sitter::prec_left(1)]
                        Sub(
                            Box<Expression>,
                            #[rust_sitter::leaf(text = "-")]
                            (),
                            Box<Expression>
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn struct_extra() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub enum Expression {
                        Number(
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] i32,
                        ),
                    }

                    #[rust_sitter::extra]
                    struct Whitespace {
                        #[rust_sitter::leaf(pattern = r"\s")]
                        _whitespace: (),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn grammar_unboxed_field() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub struct Language {
                        e: Expression,
                    }

                    pub enum Expression {
                        Number(
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v: &str| v.parse::<i32>().unwrap())]
                            i32
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn struct_repeat() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub struct NumberList {
                        numbers: Vec<Number>,
                    }

                    pub struct Number {
                        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                        v: i32
                    }

                    #[rust_sitter::extra]
                    struct Whitespace {
                        #[rust_sitter::leaf(pattern = r"\s")]
                        _whitespace: (),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn struct_optional() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub struct Language {
                        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                        v: Option<i32>,
                        t: Option<Number>,
                    }

                    pub struct Number {
                        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                        v: i32
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn enum_with_unamed_vector() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    pub struct Number {
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                            value: u32
                    }

                    #[rust_sitter::language]
                    pub enum Expr {
                        Numbers(
                            #[rust_sitter::repeat(non_empty = true)]
                            Vec<Number>
                        )
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn enum_with_named_field() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub enum Expr {
                        Number(
                                #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                                u32
                        ),
                        Neg {
                            #[rust_sitter::leaf(text = "!")]
                            _bang: (),
                            value: Box<Expr>,
                        }
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn spanned_in_vec() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    use rust_sitter::Spanned;

                    #[rust_sitter::language]
                    pub struct NumberList {
                        numbers: Vec<Spanned<Number>>,
                    }

                    pub struct Number {
                        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                        v: i32
                    }

                    #[rust_sitter::extra]
                    struct Whitespace {
                        #[rust_sitter::leaf(pattern = r"\s")]
                        _whitespace: (),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }
}
