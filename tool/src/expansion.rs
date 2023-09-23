use std::collections::HashSet;

use rust_sitter_common::{*, external_scanner::parse_external_tokens_args};
use serde_json::{json, Map, Value};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    *,
};

fn gen_field(
    path: String,
    leaf_type: Type,
    leaf_attrs: Vec<Attribute>,
    word_rule: &mut Option<String>,
    out: &mut Map<String, Value>,
) -> (Value, bool) {
    let leaf_attr = leaf_attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::leaf));

    if leaf_attrs
        .iter()
        .any(|attr| attr.path == syn::parse_quote!(rust_sitter::word))
    {
        if word_rule.is_some() {
            panic!("Multiple `word` rules specified");
        }

        *word_rule = Some(path.clone());
    }

    let leaf_params = leaf_attr.and_then(|a| {
        a.parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
            .ok()
    });

    let pattern_param = leaf_params.as_ref().and_then(|p| {
        p.iter()
            .find(|param| param.path == "pattern")
            .map(|p| p.expr.clone())
    });

    let text_param = leaf_params.as_ref().and_then(|p| {
        p.iter()
            .find(|param| param.path == "text")
            .map(|p| p.expr.clone())
    });

    let mut skip_over = HashSet::new();
    skip_over.insert("Spanned");
    skip_over.insert("Box");

    let (inner_type_vec, is_vec) = try_extract_inner_type(&leaf_type, "Vec", &skip_over);
    let (inner_type_option, is_option) = try_extract_inner_type(&leaf_type, "Option", &skip_over);

    if !is_vec && !is_option {
        if let Some(Expr::Lit(lit)) = pattern_param {
            if let Lit::Str(s) = &lit.lit {
                out.insert(
                    path.clone(),
                    json!({
                        "type": "PATTERN",
                        "value": s.value(),
                    }),
                );

                (
                    json!({
                        "type": "SYMBOL",
                        "name": path
                    }),
                    is_option,
                )
            } else {
                panic!("Expected string literal for pattern");
            }
        } else if let Some(Expr::Lit(lit)) = text_param {
            if let Lit::Str(s) = &lit.lit {
                out.insert(
                    path.clone(),
                    json!({
                        "type": "STRING",
                        "value": s.value(),
                    }),
                );

                (
                    json!({
                        "type": "SYMBOL",
                        "name": path
                    }),
                    is_option,
                )
            } else {
                panic!("Expected string literal for text");
            }
        } else {
            let symbol_name = if let Type::Path(p) = filter_inner_type(&leaf_type, &skip_over) {
                if p.path.segments.len() == 1 {
                    p.path.segments[0].ident.to_string()
                } else {
                    panic!("Expected a single segment path");
                }
            } else {
                panic!("Expected a path");
            };

            (
                json!({
                    "type": "SYMBOL",
                    "name": symbol_name,
                }),
                false,
            )
        }
    } else if is_vec {
        let (field_json, field_optional) = gen_field(
            path.clone(),
            inner_type_vec,
            leaf_attr.iter().cloned().cloned().collect(),
            word_rule,
            out,
        );

        let delimited_attr = leaf_attrs
            .iter()
            .find(|attr| attr.path == syn::parse_quote!(rust_sitter::delimited));

        let delimited_params =
            delimited_attr.and_then(|a| a.parse_args_with(FieldThenParams::parse).ok());

        let delimiter_json = delimited_params.map(|p| {
            gen_field(
                format!("{path}_vec_delimiter"),
                p.field.ty,
                p.field.attrs,
                word_rule,
                out,
            )
        });

        let repeat_attr = leaf_attrs
            .iter()
            .find(|attr| attr.path == syn::parse_quote!(rust_sitter::repeat));

        let repeat_params = repeat_attr.and_then(|a| {
            a.parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
                .ok()
        });

        let repeat_non_empty = repeat_params
            .and_then(|p| {
                p.iter()
                    .find(|param| param.path == "non_empty")
                    .map(|p| p.expr.clone())
            })
            .map(|e| e == syn::parse_quote!(true))
            .unwrap_or(false);

        let field_rule_non_optional = json!({
            "type": "FIELD",
            "name": format!("{path}_vec_element"),
            "content": field_json
        });

        let field_rule = if field_optional {
            json!({
                "type": "CHOICE",
                "members": [
                    {
                        "type": "BLANK"
                    },
                    field_rule_non_optional
                ]
            })
        } else {
            field_rule_non_optional
        };

        let vec_contents = if let Some((delimiter_json, delimiter_optional)) = delimiter_json {
            let delim_made_optional = if delimiter_optional {
                json!({
                    "type": "CHOICE",
                    "members": [
                        {
                            "type": "BLANK"
                        },
                        delimiter_json
                    ]
                })
            } else {
                delimiter_json
            };

            json!({
                "type": "SEQ",
                "members": [
                    field_rule,
                    {
                        "type": if field_optional {
                            "REPEAT1"
                        } else {
                            "REPEAT"
                        },
                        "content": {
                            "type": "SEQ",
                            "members": [
                                delim_made_optional,
                                field_rule,
                            ]
                        }
                    }
                ]
            })
        } else {
            json!({
                "type": "REPEAT1",
                "content": field_rule
            })
        };

        let contents_ident = format!("{path}_vec_contents");
        out.insert(contents_ident.clone(), vec_contents);

        (
            json!({
                "type": "SYMBOL",
                "name": contents_ident,
            }),
            !repeat_non_empty,
        )
    } else {
        // is_option
        let (field_json, field_optional) =
            gen_field(path, inner_type_option, leaf_attrs, word_rule, out);

        if field_optional {
            panic!("Option<Option<_>> is not supported");
        }

        (field_json, true)
    }
}

#[derive(Clone, PartialEq)]
enum AssocParam {
    Left,
    Right,
    Dynamic,
    None,
}
enum PrecArgs {
    Assoc(AssocParam),
    Unnamed(Expr),
    Unknown,
}

mod kw {
    syn::custom_keyword!(assoc);
    syn::custom_keyword!(left);
    syn::custom_keyword!(right);
    syn::custom_keyword!(dynamic);
}
impl Parse for PrecArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.parse::<kw::left>().is_ok() {
            return Ok(Self::Assoc(AssocParam::Left));
        } else if input.parse::<kw::right>().is_ok() {
            return Ok(Self::Assoc(AssocParam::Right));
        } else if input.parse::<kw::dynamic>().is_ok() {
            return Ok(Self::Assoc(AssocParam::Dynamic));
        } else if input.parse::<kw::assoc>().is_ok() {
            input.parse::<Token![=]>()?;
            if input.parse::<kw::left>().is_ok() {
                return Ok(Self::Assoc(AssocParam::Left));
            } else if input.parse::<kw::right>().is_ok() {
                return Ok(Self::Assoc(AssocParam::Right));
            } else if input.parse::<kw::dynamic>().is_ok() {
                return Ok(Self::Assoc(AssocParam::Dynamic));
            }
            return Ok(Self::Unknown);
        }
        if let Ok(expr) = input.parse::<Expr>() {
            return Ok(Self::Unnamed(expr));
        }
        return Ok(Self::Unknown);
    }
}

fn gen_struct_or_variant(
    path: String,
    attrs: Vec<Attribute>,
    fields: Fields,
    out: &mut Map<String, Value>,
    word_rule: &mut Option<String>,
) {
    fn gen_field_optional(
        path: &str,
        field: &Field,
        word_rule: &mut Option<String>,
        out: &mut Map<String, Value>,
        ident_str: String,
    ) -> Value {
        let (field_contents, is_option) = gen_field(
            format!("{path}_{ident_str}"),
            field.ty.clone(),
            field.attrs.clone(),
            word_rule,
            out,
        );

        let core = json!({
            "type": "FIELD",
            "name": ident_str,
            "content": field_contents
        });

        if is_option {
            json!({
                "type": "CHOICE",
                "members": [
                    {
                        "type": "BLANK"
                    },
                    core
                ]
            })
        } else {
            core
        }
    }

    let children = fields
        .iter()
        .enumerate()
        .filter_map(|(i, field)| {
            if field
                .attrs
                .iter()
                .any(|attr| attr.path == syn::parse_quote!(rust_sitter::skip))
            {
                None
            } else {
                let ident_str = field
                    .ident
                    .as_ref()
                    .map(|v| v.to_string())
                    .unwrap_or(format!("{i}"));

                Some(gen_field_optional(&path, field, word_rule, out, ident_str))
            }
        })
        .collect::<Vec<Value>>();

    let prec_attr = attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::prec));

    let prec_left_attr = attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::prec_left));

    let prec_right_attr = attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::prec_right));

    // allow using either of the attributes equally
    let (assoc_param, prec_param) = {
        if let Some(prec_attr) = prec_attr {
            if prec_left_attr.is_some() || prec_right_attr.is_some() {
                panic!("there can only be one precedence marker on each non-terminal")
            };

            let prec_params = prec_attr
                .parse_args_with(Punctuated::<PrecArgs, Token![,]>::parse_terminated)
                .ok();

            let assoc_param = prec_params
                .as_ref()
                .and_then(|p| {
                    p.iter().find_map(|param| match param {
                        PrecArgs::Assoc(assoc) => Some(assoc.clone()),
                        _ => None,
                    })
                })
                .unwrap_or(AssocParam::None);

            let prec_param = prec_params.as_ref().and_then(|p| {
                p.iter().find_map(|param| match param {
                    PrecArgs::Unnamed(expr) => Some(expr.clone()),
                    _ => None,
                })
            });

            if assoc_param == AssocParam::None && prec_param.is_none() {
                panic!("The prec attribute requires either the associativity or a precedence value to be specified")
            }

            (assoc_param, prec_param)
        } else if let Some(prec_left_attr) = prec_left_attr {
            if prec_right_attr.is_some() {
                panic!("there can only be one precedence marker on each non-terminal")
            };

            let prec_left_param = prec_left_attr.parse_args_with(Expr::parse).ok();

            (AssocParam::Left, prec_left_param)
        } else if let Some(prec_right_attr) = prec_right_attr {
            let prec_right_param = prec_right_attr.parse_args_with(Expr::parse).ok();

            (AssocParam::Right, prec_right_param)
        } else {
            (AssocParam::None, None)
        }
    };

    if assoc_param == AssocParam::Dynamic {
        panic!("Conflict checking not yet implemented");
    }

    let base_rule = match fields {
        Fields::Unit => {
            let dummy_field = Field {
                attrs: attrs.clone(),
                vis: Visibility::Inherited,
                ident: None,
                colon_token: None,
                ty: Type::Tuple(TypeTuple {
                    paren_token: Default::default(),
                    elems: Punctuated::new(),
                }),
            };
            gen_field_optional(&path, &dummy_field, word_rule, out, "unit".to_owned())
        }
        _ => json!({
            "type": "SEQ",
            "members": children
        }),
    };

    let rule = if assoc_param == AssocParam::None && prec_param.is_none() {
        base_rule
    } else {
        let rule_typelabel = match assoc_param {
            AssocParam::Left => "PREC_LEFT",
            AssocParam::Right => "PREC_RIGHT",
            AssocParam::Dynamic => "PREC_DYNAMIC",
            AssocParam::None => "PREC",
        };

        if let Some(Expr::Lit(lit)) = prec_param {
            match &lit.lit {
                Lit::Int(i) => json!({
                    "type": rule_typelabel,
                    "value": i.base10_parse::<u32>().unwrap(),
                    "content": base_rule
                }),
                // tree-sitter accepts strings in the precedence field,
                // this allows specifying far more complex inter-rule
                // precedence logic
                // but it'd require specifying more metadata elsewhere, and more consistency checks
                // Lit::Str(s) => json!({
                //     "type": "PREC",
                //     "value": s.value(),
                //     "content": seq_rule
                // }),
                _ => panic!("Expected integer literal for precedence"),
            }
        } else if assoc_param == AssocParam::Dynamic {
            panic!("For dynamic precedence, a precedence level is required");
        } else if assoc_param == AssocParam::None {
            panic!("Expected precedence parameter for the prec attribute");
        } else {
            json!({
                "type": rule_typelabel,
                "value": 0,
                "content": base_rule
            })
        }
    };

    out.insert(path, rule);
}

pub fn generate_grammar(module: &ItemMod) -> Value {
    let mut rules_map = Map::new();
    // for some reason, source_file must be the first key for things to work
    rules_map.insert("source_file".to_string(), json!({}));

    let mut extras_list = vec![];
    let mut externals_list = vec![];

    let grammar_name = module
        .attrs
        .iter()
        .find_map(|a| {
            if a.path == syn::parse_quote!(rust_sitter::grammar) {
                let grammar_name_expr = a.parse_args_with(Expr::parse).ok();
                if let Some(Expr::Lit(ExprLit {
                    attrs: _,
                    lit: Lit::Str(s),
                })) = grammar_name_expr
                {
                    Some(s.value())
                } else {
                    panic!("Expected string literal for grammar name");
                }
            } else {
                None
            }
        })
        .expect("Each grammar must have a name");

    let (_, contents) = module.content.as_ref().unwrap();

    let root_type = contents
        .iter()
        .find_map(|item| match item {
            Item::Enum(ItemEnum { ident, attrs, .. })
            | Item::Struct(ItemStruct { ident, attrs, .. }) => {
                if attrs
                    .iter()
                    .any(|attr| attr.path == syn::parse_quote!(rust_sitter::language))
                {
                    Some(ident.clone())
                } else {
                    None
                }
            }
            _ => None,
        })
        .expect("Each parser must have the root type annotated with `#[rust_sitter::language]`")
        .to_string();

    let has_external_scanner = contents.iter().any(|item| match item {
        Item::Macro(ItemMacro { ident: None, mac: Macro { path, .. }, .. }) => {
            path.clone() == syn::parse_quote!(rust_sitter::external_scanner)
        },
        _ => false
    });

    // Optionally locate the rule annotated with `#[rust_sitter::word]`.
    let mut word_rule = None;
    contents.iter().for_each(|c| {
        let (symbol, attrs) = match c {
            Item::Enum(e) => {
                if e.attrs
                    .iter()
                    .any(|a| a.path == syn::parse_quote!(rust_sitter::skip))
                {
                    return;
                }

                e.variants.iter().for_each(|v| {
                    gen_struct_or_variant(
                        format!("{}_{}", e.ident, v.ident),
                        v.attrs.clone(),
                        v.fields.clone(),
                        &mut rules_map,
                        &mut word_rule,
                    )
                });

                let mut members: Vec<Value> = vec![];
                e.variants.iter().for_each(|v| {
                    let variant_path = format!("{}_{}", e.ident.clone(), v.ident);
                    members.push(json!({
                        "type": "SYMBOL",
                        "name": variant_path
                    }))
                });

                let rule = json!({
                    "type": "CHOICE",
                    "members": members
                });

                rules_map.insert(e.ident.to_string(), rule);

                (e.ident.to_string(), e.attrs.clone())
            }

            Item::Struct(s) => {
                if s.attrs
                    .iter()
                    .any(|a| a.path == syn::parse_quote!(rust_sitter::skip))
                {
                    return;
                }

                if s.attrs
                    .iter()
                    .any(|a| a.path == syn::parse_quote!(rust_sitter::external_token))
                {
                    // TODO: add the necessary stuff for external token fallbacks
                } else {
                    gen_struct_or_variant(
                        s.ident.to_string(),
                        s.attrs.clone(),
                        s.fields.clone(),
                        &mut rules_map,
                        &mut word_rule,
                    );
                }

                (s.ident.to_string(), s.attrs.clone())
            }

            _ => return,
        };

        if attrs
            .iter()
            .any(|a| a.path == syn::parse_quote!(rust_sitter::extra))
        {
            extras_list.push(json!({
                "type": "SYMBOL",
                "name": symbol
            }));
        }

        if let Some(attr) = attrs
            .iter()
            .find(|a| a.path == syn::parse_quote!(rust_sitter::external_token))
        {
            let text_arg = parse_external_tokens_args(attr);
            if let Some(text) = text_arg {
                externals_list.push(json!({
                    "type": "STRING",
                    "value": text
                }));
            } else {
                externals_list.push(json!({
                    "type": "SYMBOL",
                    "name": symbol
                }));
            }
        }
    });

    rules_map.insert(
        "source_file".to_string(),
        rules_map.get(&root_type).unwrap().clone(),
    );

    if has_external_scanner {
        externals_list.push(json!({
            "type": "SYMBOL",
            "name": "__ERROR_SENTINEL__"
        }));
        json!({
            "name": grammar_name,
            "word": word_rule,
            "rules": rules_map,
            "extras": extras_list,
            "externals": externals_list,
        })
    } else {
        json!({
            "name": grammar_name,
            "word": word_rule,
            "rules": rules_map,
            "extras": extras_list,
        })
    }
}
