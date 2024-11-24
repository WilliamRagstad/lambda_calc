#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{parse_prog, run, Expr};

    #[test]
    fn test_parse() {
        let input = "x = y; λx. x y; x y;";
        let exprs = parse_prog(input);
        assert_eq!(
            &exprs,
            &[
                Expr::Assignment("x".to_string(), Box::new(Expr::Variable("y".to_string()))),
                Expr::Abstraction(
                    "x".to_string(),
                    Box::new(Expr::Application(
                        Box::new(Expr::Variable("x".to_string())),
                        Box::new(Expr::Variable("y".to_string()))
                    ))
                ),
                Expr::Application(
                    Box::new(Expr::Variable("x".to_string())),
                    Box::new(Expr::Variable("y".to_string()))
                )
            ]
        );
    }

    #[test]
    fn test_multi_app() {
        let input = "λx. λy. λz. ((x y) z);";
        let exprs = parse_prog(input);
        assert_eq!(
            &exprs,
            &[Expr::Abstraction(
                "x".to_string(),
                Box::new(Expr::Abstraction(
                    "y".to_string(),
                    Box::new(Expr::Abstraction(
                        "z".to_string(),
                        Box::new(Expr::Application(
                            Box::new(Expr::Application(
                                Box::new(Expr::Variable("x".to_string())),
                                Box::new(Expr::Variable("y".to_string()))
                            )),
                            Box::new(Expr::Variable("z".to_string()))
                        ))
                    ))
                ))
            )]
        );
    }

    #[test]
    fn test_eval() {
        let mut env = HashMap::new();
        let input = "x = y; λx. x y; x y;";
        run(input.to_string(), &mut env);
    }
}
