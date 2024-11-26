#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{eval, parse_prog, Term};

    #[test]
    fn test_parse() {
        let input = "x = y; λx. (x y); x y;";
        let terms = parse_prog(input);
        assert_eq!(
            &terms,
            &[
                Term::Assignment("x".to_string(), Box::new(Term::Variable("y".to_string()))),
                Term::Abstraction(
                    "x".to_string(),
                    Box::new(Term::Application(
                        Box::new(Term::Variable("x".to_string())),
                        Box::new(Term::Variable("y".to_string()))
                    ))
                ),
                Term::Application(
                    Box::new(Term::Variable("x".to_string())),
                    Box::new(Term::Variable("y".to_string()))
                )
            ]
        );
    }

    #[test]
    fn test_multi_app() {
        let input = "λx. λy. λz. ((x y) z);";
        let terms = parse_prog(input);
        assert_eq!(
            &terms,
            &[Term::Abstraction(
                "x".to_string(),
                Box::new(Term::Abstraction(
                    "y".to_string(),
                    Box::new(Term::Abstraction(
                        "z".to_string(),
                        Box::new(Term::Application(
                            Box::new(Term::Application(
                                Box::new(Term::Variable("x".to_string())),
                                Box::new(Term::Variable("y".to_string()))
                            )),
                            Box::new(Term::Variable("z".to_string()))
                        ))
                    ))
                ))
            )]
        );
    }

    #[test]
    fn test_eval() {
        let mut env = HashMap::new();
        let input = "x = λx. (x y); x y;";
        let terms = parse_prog(input);
        let mut terms = terms.into_iter();
        let first = terms.next().expect("No terms found");
        let result = terms.fold(eval(&first, &mut env, false), |_, term| {
            eval(&term, &mut env, false)
        });
        assert_eq!(
            result,
            Term::Application(
                Box::new(Term::Variable("y".to_string())),
                Box::new(Term::Variable("y".to_string()))
            )
        );
    }
}
