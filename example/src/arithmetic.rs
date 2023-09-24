#[rust_sitter::grammar("arithmetic")]
pub mod grammar {
    #[rust_sitter::language]
    #[derive(PartialEq, Eq, Debug)]
    pub enum Expression {
        Number(#[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] i32),
        #[rust_sitter::prec(left,1)]
        Sub(
            Box<Expression>,
            #[rust_sitter::leaf(text = "-")] (),
            Box<Expression>,
        ),
        #[rust_sitter::prec(left,2)]
        Mul(
            Box<Expression>,
            #[rust_sitter::leaf(text = "*")] (),
            Box<Expression>,
        ),
    }

    #[rust_sitter::extra]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s")]
        _whitespace: (),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use grammar::Expression;

    #[wasm_bindgen_test::wasm_bindgen_test]
    #[test]
    fn successful_parses() {
        assert_eq!(grammar::parse("1", None).unwrap().0, Expression::Number(1));

        assert_eq!(grammar::parse(" 1", None).unwrap().0, Expression::Number(1));

        assert_eq!(
            grammar::parse("1 - 2", None).unwrap().0,
            Expression::Sub(
                Box::new(Expression::Number(1)),
                (),
                Box::new(Expression::Number(2))
            )
        );

        assert_eq!(
            grammar::parse("1 - 2 - 3", None).unwrap().0,
            Expression::Sub(
                Box::new(Expression::Sub(
                    Box::new(Expression::Number(1)),
                    (),
                    Box::new(Expression::Number(2))
                )),
                (),
                Box::new(Expression::Number(3))
            )
        );

        assert_eq!(
            grammar::parse("1 - 2 * 3", None).unwrap().0,
            Expression::Sub(
                Box::new(Expression::Number(1)),
                (),
                Box::new(Expression::Mul(
                    Box::new(Expression::Number(2)),
                    (),
                    Box::new(Expression::Number(3))
                ))
            )
        );

        assert_eq!(
            grammar::parse("1 * 2 * 3", None).unwrap().0,
            Expression::Mul(
                Box::new(Expression::Mul(
                    Box::new(Expression::Number(1)),
                    (),
                    Box::new(Expression::Number(2))
                )),
                (),
                Box::new(Expression::Number(3))
            )
        );

        assert_eq!(
            grammar::parse("1 * 2 - 3", None).unwrap().0,
            Expression::Sub(
                Box::new(Expression::Mul(
                    Box::new(Expression::Number(1)),
                    (),
                    Box::new(Expression::Number(2))
                )),
                (),
                Box::new(Expression::Number(3))
            )
        );
    }

    #[test]
    fn failed_parses() {
        insta::assert_debug_snapshot!(grammar::parse("1 + 2", None));
        insta::assert_debug_snapshot!(grammar::parse("1 - 2 -", None));
        insta::assert_debug_snapshot!(grammar::parse("a1", None));
        insta::assert_debug_snapshot!(grammar::parse("1a", None));
    }
}
