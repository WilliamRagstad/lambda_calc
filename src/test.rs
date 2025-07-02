#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        eval::{eval_expr, inline_vars},
        parser::{parse_prog, Expr, Term},
        PRINT_NONE,
    };

    impl Expr {
        fn term(&self) -> &Term {
            match self {
                Expr::Assignment(_, term) => term,
                Expr::Term(term) => term,
            }
        }
    }

    #[test]
    fn test_parse() {
        let input = "x = y; λx. (x y); x y;";
        let terms = parse_prog(input);
        assert_eq!(
            &terms,
            &[
                Expr::Assignment("x".to_string(), Term::Variable("y".to_string())),
                Expr::Term(Term::Abstraction(
                    "x".to_string(),
                    Box::new(Term::Application(
                        Box::new(Term::Variable("x".to_string())),
                        Box::new(Term::Variable("y".to_string()))
                    ))
                )),
                Expr::Term(Term::Application(
                    Box::new(Term::Variable("x".to_string())),
                    Box::new(Term::Variable("y".to_string()))
                ))
            ]
        );
    }

    #[test]
    fn test_multi_app() {
        let input = "λx. λy. λz. ((x y) z);";
        let terms = parse_prog(input);
        assert_eq!(
            &terms,
            &[Expr::Term(Term::Abstraction(
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
            ))]
        );
    }

    #[test]
    fn test_eval() {
        let mut env = HashMap::new();
        let input = "x = λx. (x y); x y;";
        let prog = parse_prog(input);
        assert_eq!(prog.len(), 2);
        eval_expr(&prog[0], &mut env, false, PRINT_NONE);
        let result = eval_expr(&prog[1], &mut env, false, PRINT_NONE);
        assert_eq!(
            result,
            Term::Application(
                Box::new(Term::Variable("y".to_string())),
                Box::new(Term::Variable("y".to_string()))
            )
        );
    }

    /// We should be able to have recursive function definitions
    /// and inline them in one step at a time without any issues.
    #[test]
    fn test_inline_vars_one_step() {
        let mut env = HashMap::new();
        let input = "A = λx. (A x); A y;";
        let expected = "(λx. (A x)) y";
        let prog = parse_prog(input);
        let binding = parse_prog(expected).pop().unwrap();
        let prog_expected = binding.term();
        assert_eq!(prog.len(), 2);
        eval_expr(&prog[0], &mut env, false, PRINT_NONE);
        let inlined = inline_vars(prog[1].term(), &env);
        assert_eq!(&inlined, prog_expected);
    }
}
