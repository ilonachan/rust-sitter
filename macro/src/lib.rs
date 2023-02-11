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


#[proc_macro_attribute]
/// Marks an extra node that may appear anywhere in the language.
/// This can be used to, for example, accept whitespace and comments.
///
/// ## Example
/// ```ignore
/// #[rust_sitter::extra]
/// pub struct Whitespace {
///     ...
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
/// Number(
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
// TODO: how does this play together with the `token` directive from tree-sitter?
// TODO: where do immediate tokens fit in?
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
