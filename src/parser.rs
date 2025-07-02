use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

/// Lambda calculus parser using pest
#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct LambdaCalcParser;

/// AST for our extended lambda calculus program
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assignment(String, Term),
    Term(Term),
}

/// A program is a list of expressions
pub type Program = Vec<Expr>;

/// AST for lambda calculus
///
/// See https://en.wikipedia.org/wiki/Lambda_calculus#Definition.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Variable(String),
    Abstraction(String, Box<Term>),
    Application(Box<Term>, Box<Term>),
}

/// Parse a top-level program into a list of terms
pub fn parse_prog(input: &str) -> Program {
    /// Transform a Pest pair into our own AST Expr node format
    fn parse_term(pair: Pair<Rule>) -> Term {
        match pair.as_rule() {
            Rule::variable => Term::Variable(pair.as_str().to_string()),
            Rule::abstraction => {
                let mut inner = pair.into_inner();
                let param = inner.next().unwrap().as_str().to_string();
                let body = parse_term(inner.next().unwrap());
                Term::Abstraction(param, Box::new(body))
            }
            // Rule::application => {
            //     let mut inner = pair.into_inner();
            //     let lhs = parse_term(inner.next().unwrap());
            //     let rhs = parse_term(inner.next().unwrap());
            //     Term::Application(Box::new(lhs), Box::new(rhs))
            // }
            // rhs is one or more terms
            Rule::application => {
                // Syntax sugar: (e1 e2 e3 ...) -> (e1 (e2 (e3 ...)))
                // Previous (e1 e2) was only allowed
                let mut inner = pair.into_inner();
                let mut lhs = parse_term(inner.next().unwrap());
                for rhs in inner {
                    lhs = Term::Application(Box::new(lhs), Box::new(parse_term(rhs)));
                }
                lhs
            }
            r => unreachable!("Rule {:?} not expected", r),
        }
    }

    let mut prog = Program::new();
    let pairs = match LambdaCalcParser::parse(Rule::program, input) {
        Ok(pairs) => pairs,
        Err(e) => {
            eprintln!("{}", e);
            return prog;
        }
    };
    for pair in pairs {
        match pair.as_rule() {
            Rule::EOI => break,
            Rule::assignment => {
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap().as_str().to_string();
                let term = parse_term(inner.next().unwrap());
                prog.push(Expr::Assignment(name, term));
            }
            // Parse a lambda calculus term
            _ => prog.push(Expr::Term(parse_term(pair))),
        }
    }
    prog
}
