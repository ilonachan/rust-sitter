use syn::{parse::{Parse, ParseStream}, *, punctuated::Punctuated};

use crate::NameValueExpr;

pub enum ExternalTokensArgs {
    Text(LitStr, bool),
    Unknown,
}
impl Parse for ExternalTokensArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(NameValueExpr {
            path,
            expr:
                Expr::Lit(ExprLit {
                    lit: Lit::Str(expr),
                    ..
                }),
            ..
        }) = input.parse()
        {
            if path == "text" {
                return Ok(Self::Text(expr, true));
            }
            return Ok(Self::Unknown);
        }
        if let Ok(expr) = input.parse::<LitStr>() {
            return Ok(Self::Text(expr, false));
        }
        return Ok(Self::Unknown);
    }
}

pub fn parse_external_tokens_args(attr: &Attribute) -> Option<String> {
    if let Ok(args) =
        attr.parse_args_with(Punctuated::<ExternalTokensArgs, Token![,]>::parse_terminated)
    {
        let mut text_arg = None;
        for arg in args {
            if let ExternalTokensArgs::Text(val, _) = arg {
                if text_arg.is_some() {
                    panic!("Multiple `text` arguments supplied for external token")
                }
                text_arg = Some(val.value());
            }
        }
        text_arg
    } else {
        None
    }
}