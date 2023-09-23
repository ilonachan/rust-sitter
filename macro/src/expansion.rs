use std::collections::HashSet;

use proc_macro2::Span;
use proc_macro_error::emit_error;
use quote::{quote, ToTokens};
use rust_sitter_common::{*, external_scanner::parse_external_tokens_args};
use syn::{parse::{Parse, ParseStream, Parser}, punctuated::Punctuated, *, spanned::Spanned};

fn is_sitter_attr(attr: &Attribute) -> bool {
    let ident = &attr.path.segments.iter().next().unwrap().ident;
    ident == "rust_sitter"
}

enum ParamOrField {
    Param(Expr),
    Field(FieldValue),
}

impl ToTokens for ParamOrField {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            ParamOrField::Param(expr) => expr.to_tokens(tokens),
            ParamOrField::Field(field) => field.to_tokens(tokens),
        }
    }
}

fn gen_field(path: String, ident_str: String, leaf: Field, out: &mut Vec<Item>) {
    let extract_ident = Ident::new(&format!("extract_{path}"), Span::call_site());
    let leaf_type = leaf.ty;

    let leaf_attr = leaf
        .attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::leaf));
    
    check_for_duplicate_prec_attribute(&leaf.attrs);
    
    let leaf_params = leaf_attr.and_then(|a| {
        a.parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
            .ok()
    });

    let transform_param = leaf_params.as_ref().and_then(|p| {
        p.iter()
            .find(|param| param.path == "transform")
            .map(|p| p.expr.clone())
    });

    let mut skip_over = HashSet::new();
    skip_over.insert("Spanned");
    skip_over.insert("Box");

    let (leaf_stmts, leaf_expr): (Vec<Stmt>, Expr) = match transform_param {
        Some(closure) => {
            let mut non_leaf = HashSet::new();
            non_leaf.insert("Spanned");
            non_leaf.insert("Box");
            non_leaf.insert("Option");
            non_leaf.insert("Vec");
            let wrapped_leaf_type = wrap_leaf_type(&leaf_type, &non_leaf);

            (
                vec![],
                syn::parse_quote!(<#wrapped_leaf_type as rust_sitter::Extract<_>>::extract(node, source, *last_idx, Some(&#closure))),
            )
        }
        None => (
            vec![],
            syn::parse_quote!(<#leaf_type as rust_sitter::Extract<_>>::extract(node, source, *last_idx, None)),
        ),
    };

    out.push(syn::parse_quote! {
        #[allow(non_snake_case)]
        #[allow(clippy::unused_unit)]
        fn #extract_ident(cursor_opt: &mut Option<rust_sitter::tree_sitter::TreeCursor>, source: &[u8], last_idx: &mut usize) -> #leaf_type {
            #(#leaf_stmts)*

            if let Some(cursor) = cursor_opt.as_mut() {
                loop {
                    let n = cursor.node();
                    if let Some(name) = cursor.field_name() {
                        if name == #ident_str {
                            let node: Option<rust_sitter::tree_sitter::Node> = Some(n);
                            let out = #leaf_expr;

                            if !cursor.goto_next_sibling() {
                                *cursor_opt = None;
                            };

                            *last_idx = n.end_byte();

                            return out;
                        } else {
                            let node: Option<rust_sitter::tree_sitter::Node> = None;
                            return #leaf_expr;
                        }
                    } else {
                        *last_idx = n.end_byte();
                    }

                    if !cursor.goto_next_sibling() {
                        let node: Option<rust_sitter::tree_sitter::Node> = None;
                        return #leaf_expr;
                    }
                }
            } else {
                let node: Option<rust_sitter::tree_sitter::Node> = None;
                return #leaf_expr;
            }
        }
    });
}

fn gen_struct_or_variant(
    path: String,
    fields: Fields,
    variant_ident: Option<Ident>,
    containing_type: Ident,
    container_attrs: Vec<Attribute>,
    out: &mut Vec<Item>,
) {
    if fields == Fields::Unit {
        let dummy_field = Field {
            attrs: container_attrs,
            vis: Visibility::Inherited,
            ident: None,
            colon_token: None,
            ty: Type::Verbatim(quote!(())), // unit type.
        };
        gen_field(format!("{path}_unit"), "unit".to_owned(), dummy_field, out);
    } else {
        fields.iter().enumerate().for_each(|(i, field)| {
            let ident_str = field
                .ident
                .as_ref()
                .map(|v| v.to_string())
                .unwrap_or(format!("{i}"));

            if !field
                .attrs
                .iter()
                .any(|attr| attr.path == syn::parse_quote!(rust_sitter::skip))
            {
                gen_field(
                    format!("{}_{}", path, ident_str),
                    ident_str,
                    field.clone(),
                    out,
                );
            }
        });
    }

    let extract_ident = Ident::new(&format!("extract_{path}"), Span::call_site());

    let mut have_named_field = false;

    let children_parsed = fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let expr = if let Some(skip_attrs) = field
                .attrs
                .iter()
                .find(|attr| attr.path == syn::parse_quote!(rust_sitter::skip))
            {
                skip_attrs.parse_args::<syn::Expr>().unwrap()
            } else {
                let ident_str = field
                    .ident
                    .as_ref()
                    .map(|v| v.to_string())
                    .unwrap_or(format!("{i}"));

                let ident = Ident::new(&format!("extract_{path}_{ident_str}"), Span::call_site());

                syn::parse_quote! {
                    #ident(&mut cursor, source, &mut last_idx)
                }
            };

            if field.ident.is_none() {
                ParamOrField::Param(expr)
            } else {
                let field_name = field.ident.as_ref().unwrap();
                have_named_field = true;
                ParamOrField::Field(FieldValue {
                    attrs: vec![],
                    member: Member::Named(field_name.clone()),
                    colon_token: Some(Token![:](Span::call_site())),
                    expr,
                })
            }
        })
        .collect::<Vec<ParamOrField>>();

    let construct_name = match variant_ident {
        Some(ident) => quote! {
            #containing_type::#ident
        },
        None => quote! {
            #containing_type
        },
    };

    let construct_expr = {
        match &fields {
            Fields::Unit => {
                let ident = Ident::new(&format!("extract_{path}_unit"), Span::call_site());
                quote! {
                    {
                        #ident(&mut cursor, source, &mut last_idx);
                        #construct_name
                    }
                }
            }
            Fields::Named(_) => quote! {
                #construct_name {
                    #(#children_parsed),*
                }
            },
            Fields::Unnamed(_) => quote! {
                #construct_name(
                    #(#children_parsed),*
                )
            },
        }
    };

    out.push(syn::parse_quote! {
        #[allow(non_snake_case)]
        fn #extract_ident(node: rust_sitter::tree_sitter::Node, source: &[u8]) -> #containing_type {
            let mut last_idx = node.start_byte();
            let mut parent_cursor = node.walk();
            let mut cursor = if parent_cursor.goto_first_child() {
                Some(parent_cursor)
            } else {
                None
            };

            #construct_expr
        }
    });
}

pub fn expand_grammar(input: ItemMod) -> ItemMod {
    let grammar_name = input
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

    let (brace, new_contents) = input.content.unwrap();

    let root_type = new_contents
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
        .expect("Each parser must have the root type annotated with `#[rust_sitter::language]`");
    
    let extscanner_impl = new_contents.iter().filter_map(|item| match item {
        Item::Macro(ItemMacro { ident: None, mac, .. }) => {
            let Macro { path, tokens, .. } = mac;
             
            if path.clone() == syn::parse_quote!(rust_sitter::external_scanner) {
                let tokens: proc_macro::TokenStream = tokens.clone().into();
                fn parse_input(input: ParseStream) -> syn::Result<(Type,Option<Ident>)> {
                    let ty: Type = input.parse()?;

                    let id: Option<Ident> = if let Some(_) = input.parse::<Option<Token![,]>>()? {
                        input.parse()?
                    } else { None };
                    let _: Option<Token![,]> = input.parse()?;
                    if !input.is_empty() {
                        return Err(input.error("Stream too long"));
                    }
                    Ok((ty,id))
                }
                
                match parse_input.parse(tokens) {
                    Ok((ty,id)) => Some((ty,id,mac.clone())),
                    Err(err) => {
                        emit_error!(err.span(), err.to_string());
                        None
                    }
                }
            } else {
                None
            }  
        },
        _ => None
    }).fold(None, |acc, el| {
        if acc.is_none() {
            Some(el)
        } else {
            emit_error!(el.2, "Only one external scanner can be registered for any grammar");
            acc
        }
    });

    let mut extscanner_tokens = vec![];

    let mut transformed: Vec<Item> = new_contents
        .iter()
        .cloned()
        .flat_map(|c| match c {
            Item::Enum(mut e) => {
                if e.attrs.iter().any(|attr| attr.path == syn::parse_quote!(rust_sitter::skip)) {
                    vec![Item::Enum(e)]
                } else {
                    let mut impl_body = vec![];
                    e.variants.iter().for_each(|v| {
                        check_for_duplicate_prec_attribute(&v.attrs);
                        
                        gen_struct_or_variant(
                            format!("{}_{}", e.ident, v.ident),
                            v.fields.clone(),
                            Some(v.ident.clone()),
                            e.ident.clone(),
                            v.attrs.clone(),
                            &mut impl_body,
                        )
                    });

                    let match_cases: Vec<Arm> = e
                        .variants
                        .iter()
                        .map(|v| {
                            let variant_path = format!("{}_{}", e.ident, v.ident);
                            let extract_ident =
                                Ident::new(&format!("extract_{variant_path}"), Span::call_site());
                            syn::parse_quote! {
                                #variant_path => return #extract_ident(n, source)
                            }
                        })
                        .collect();

                    e.attrs.retain(|a| !is_sitter_attr(a));
                    e.variants.iter_mut().for_each(|v| {
                        v.attrs.retain(|a| !is_sitter_attr(a));
                        v.fields.iter_mut().for_each(|f| {
                            f.attrs.retain(|a| !is_sitter_attr(a));
                        });
                    });

                    let enum_name = &e.ident;
                    let extract_impl: Item = syn::parse_quote! {
                        impl rust_sitter::Extract<#enum_name> for #enum_name {
                            type LeafFn = ();

                            #[allow(non_snake_case)]
                            fn extract(node: Option<rust_sitter::tree_sitter::Node>, source: &[u8], _last_idx: usize, _leaf_fn: Option<&Self::LeafFn>) -> Self {
                                let node = node.unwrap();
                                #(#impl_body)*

                                let mut cursor = node.walk();
                                assert!(cursor.goto_first_child());
                                loop {
                                    let n = cursor.node();
                                    match n.kind() {
                                        #(#match_cases),*,
                                        _ => if !cursor.goto_next_sibling() {
                                            panic!("Could not find a child corresponding to any enum branch")
                                        }
                                    }
                                }
                            }
                        }
                    };

                    vec![Item::Enum(e), extract_impl]
                }
            }

            Item::Struct(mut s) => {
                if s.attrs.iter().any(|attr| attr.path == syn::parse_quote!(rust_sitter::skip)) {
                    vec![Item::Struct(s)]
                } else if let Some(attr) = s.attrs.iter().find(|attr| attr.path == syn::parse_quote!(rust_sitter::external_token)) {
                    if let syn::Fields::Unit = s.fields {
                        // TODO: relax requirements
                        extscanner_tokens.push((s.ident.clone(), attr.clone()));
                        if parse_external_tokens_args(attr).is_some() {
                            s.attrs.push(syn::parse_quote!(#[allow(dead_code)]))
                        }

                        // TODO: add an impl Extract block, so the node can be used in the tree
                        // probably reuse logic from the general case
                        // (this could follow a similar pattern as for standard leaf unit structs)
                        // let struct_name = &s.ident;
                        // let extract_impl: Item = syn::parse_quote! {
                        //     impl rust_sitter::Extract<#struct_name> for #struct_name {
                        //         type LeafFn = ();
    
                        //         #[allow(non_snake_case)]
                        //         fn extract(node: Option<rust_sitter::tree_sitter::Node>, source: &[u8], last_idx: usize, _leaf_fn: Option<&Self::LeafFn>) -> Self {
                        //             let node = node.unwrap();
                        //             #(#impl_body)*
                        //             #extract_ident(node, source)
                        //         }
                        //     }
                        // };
                        vec![Item::Struct(s)]
                    } else {
                        emit_error!(s.fields, "External tokens can't have associated data, as this could not be populated during parsing.");
                        vec![Item::Struct(s)]
                    }
                } else {
                    let mut impl_body = vec![];

                    check_for_duplicate_prec_attribute(&s.attrs);

                    gen_struct_or_variant(
                        s.ident.to_string(),
                        s.fields.clone(),
                        None,
                        s.ident.clone(),
                        s.attrs.clone(),
                        &mut impl_body,
                    );

                    s.attrs.retain(|a| !is_sitter_attr(a));
                    s.fields.iter_mut().for_each(|f| {
                        f.attrs.retain(|a| !is_sitter_attr(a));
                    });

                    let struct_name = &s.ident;
                    let extract_ident =
                        Ident::new(&format!("extract_{struct_name}"), Span::call_site());

                    let extract_impl: Item = syn::parse_quote! {
                        impl rust_sitter::Extract<#struct_name> for #struct_name {
                            type LeafFn = ();

                            #[allow(non_snake_case)]
                            fn extract(node: Option<rust_sitter::tree_sitter::Node>, source: &[u8], last_idx: usize, _leaf_fn: Option<&Self::LeafFn>) -> Self {
                                let node = node.unwrap();
                                #(#impl_body)*
                                #extract_ident(node, source)
                            }
                        }
                    };

                    vec![Item::Struct(s), extract_impl]
                }
            }

            o => vec![o],
        })
        .collect();

    let tree_sitter_ident = Ident::new(&format!("tree_sitter_{grammar_name}"), Span::call_site());

    transformed.push(syn::parse_quote! {
        extern "C" {
            fn #tree_sitter_ident() -> rust_sitter::tree_sitter::Language;
        }
    });

    transformed.push(syn::parse_quote! {
        pub fn language() -> rust_sitter::tree_sitter::Language {
            unsafe { #tree_sitter_ident() }
        }
    });

    transformed.push(syn::parse_quote! {
        pub fn parse(input: &str) -> core::result::Result<#root_type, Vec<rust_sitter::errors::ParseError>> {
            let mut parser = rust_sitter::tree_sitter::Parser::new();
            parser.set_language(language()).unwrap();
            let tree = parser.parse(input, None).unwrap();
            let root_node = tree.root_node();

            if root_node.has_error() {
                let mut errors = vec![];
                rust_sitter::errors::collect_parsing_errors(
                    &root_node,
                    input.as_bytes(),
                    &mut errors,
                );

                Err(errors)
            } else {
                use rust_sitter::Extract;
                Ok(<#root_type as rust_sitter::Extract<_>>::extract(Some(root_node), input.as_bytes(), 0, None))
            }
        }
    });

    if let Some((scanner_ty, enum_ident, _attr)) = extscanner_impl {
        let enum_ident = enum_ident.unwrap_or(syn::parse_quote!(ExternalTokens));
        
        let ident_create = Ident::new(&format!("tree_sitter_{grammar_name}_external_scanner_create"), Span::call_site());
        let ident_destroy = Ident::new(&format!("tree_sitter_{grammar_name}_external_scanner_destroy"), Span::call_site());
        let ident_serialize = Ident::new(&format!("tree_sitter_{grammar_name}_external_scanner_serialize"), Span::call_site());
        let ident_deserialize = Ident::new(&format!("tree_sitter_{grammar_name}_external_scanner_deserialize"), Span::call_site());
        let ident_scan = Ident::new(&format!("tree_sitter_{grammar_name}_external_scanner_scan"), Span::call_site());
        
        
        transformed.push(syn::parse_quote! {
            #[no_mangle]
            extern "C" fn #ident_create() -> *mut libc::c_void {
                unsafe {rust_sitter::external_scanner::create::<#scanner_ty>()}
            }
        });
        transformed.push(syn::parse_quote! {
            #[no_mangle]
            extern "C" fn #ident_destroy(payload: *mut libc::c_void) {
                unsafe {rust_sitter::external_scanner::destroy::<#scanner_ty>(payload);}
            }
        });
        transformed.push(syn::parse_quote! {
            #[no_mangle]
            extern "C" fn #ident_serialize(
                payload: *mut libc::c_void,
                buffer: *mut libc::c_char,
            ) -> libc::c_uint {
                unsafe {rust_sitter::external_scanner::serialize::<#scanner_ty>(payload, buffer)}
            }
        });
        transformed.push(syn::parse_quote! {
            #[no_mangle]
            extern "C" fn #ident_deserialize(
                payload: *mut libc::c_void,
                buffer: *const libc::c_char,
                length: libc::c_uint,
            ) {
                unsafe {rust_sitter::external_scanner::deserialize::<#scanner_ty>(payload, buffer, length)}
            }
        });
        transformed.push(syn::parse_quote! {
            #[no_mangle]
            extern "C" fn #ident_scan(
                payload: *mut libc::c_void,
                lexer: *mut rust_sitter::external_scanner::_TSLexer,
                valid_symbols: *const bool,
            ) -> bool {
                unsafe {rust_sitter::external_scanner::scan::<#scanner_ty>(payload, lexer, valid_symbols)}
            }
        });
        

        let mut enum_variants: Punctuated<Variant, Token![,]> = Punctuated::new();
        let mut token_list_index_match_arms: Vec<Arm> = vec![];
        let mut from_index_match_arms: Vec<Arm> = vec![];



        for (id,(variant, _)) in extscanner_tokens.iter().enumerate() {
            let id = id as u16;
            enum_variants.push(Variant{ attrs: vec![], ident: variant.clone(), fields: Fields::Unit, discriminant: None });
            token_list_index_match_arms.push(syn::parse_quote!{
                #enum_ident::#variant => #id,
            });
            from_index_match_arms.push(syn::parse_quote!{
                #id => Some(#enum_ident::#variant),
            });
        }

        let enum_code: ItemEnum = syn::parse_quote!{
            pub enum #enum_ident {
                #enum_variants
            }
        };
        let token_count = extscanner_tokens.len() as u16;
        // LitInt::new(&extscanner_tokens.len().to_string(), Span::call_site())
        let impl_code: ItemImpl = syn::parse_quote!{
            impl rust_sitter::external_scanner::ExternalTokens for #enum_ident {
                fn token_count() -> u16 {
                    #token_count
                }
            
                fn token_list_index(&self) -> u16 {
                    match self {
                        #(#token_list_index_match_arms)*
                    }
                }
            
                fn from_index(index: u16) -> Option<Self> {
                    match index {
                        #(#from_index_match_arms)*
                        _ => None
                    }
                }
            }
        };

        transformed.push(Item::Enum(enum_code));
        transformed.push(Item::Impl(impl_code));
        
    } else if extscanner_tokens.len() > 0 {
        for (_,attr) in extscanner_tokens {
            emit_error!(attr, "Can't mark a unit struct as an External Token if no External Scanner is present")
        }
    }

    let mut filtered_attrs = input.attrs;
    filtered_attrs.retain(|a| !is_sitter_attr(a));
    ItemMod {
        attrs: filtered_attrs,
        vis: input.vis,
        mod_token: input.mod_token,
        ident: input.ident,
        content: Some((brace, transformed)),
        semi: input.semi,
    }
}

fn check_for_duplicate_prec_attribute(attrs: &[Attribute]) {
    
    let prec_attrs: Vec<&Attribute> = attrs.iter().filter(|attr| {
        attr.path == syn::parse_quote!(rust_sitter::prec)
            || attr.path == syn::parse_quote!(rust_sitter::prec_left)
            || attr.path == syn::parse_quote!(rust_sitter::prec_right)
    }).collect();

    if prec_attrs.len() > 1 {
        for attr in &prec_attrs[1..] {
            emit_error!(attr.span(), "Only one precedence annotation is allowed at each non-terminal");
        }
    }
    
}
