use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
};

use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

mod test;

const DARK_GRAY: &str = "\x1b[90m";
const YELLOW: &str = "\x1b[33m";
const RESET: &str = "\x1b[0m";
/// Lambda calculus parser using pest
#[derive(Parser)]
#[grammar = "grammar.pest"]
struct LambdaCalcParser;

/// AST for lambda calculus
///
/// See https://en.wikipedia.org/wiki/Lambda_calculus#Definition.
#[derive(Debug, Clone, PartialEq)]
enum Term {
    Variable(String),
    Assignment(String, Box<Term>),
    Abstraction(String, Box<Term>),
    Application(Box<Term>, Box<Term>),
}

/// Parse a top-level program into a list of terms
fn parse_prog(input: &str) -> Vec<Term> {
    /// Transform a Pest pair into our own AST Expr node format
    fn parse_term(pair: Pair<Rule>) -> Term {
        match pair.as_rule() {
            Rule::variable => Term::Variable(pair.as_str().to_string()),
            Rule::assignment => {
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap().as_str().to_string();
                let term = parse_term(inner.next().unwrap());
                Term::Assignment(name, Box::new(term))
            }
            Rule::abstraction => {
                let mut inner = pair.into_inner();
                let param = inner.next().unwrap().as_str().to_string();
                let body = parse_term(inner.next().unwrap());
                Term::Abstraction(param, Box::new(body))
            }
            Rule::application => {
                let mut inner = pair.into_inner();
                let lhs = parse_term(inner.next().unwrap());
                let rhs = parse_term(inner.next().unwrap());
                Term::Application(Box::new(lhs), Box::new(rhs))
            }
            r => unreachable!("Rule {:?} not expected", r),
        }
    }

    let mut terms = Vec::new();
    let pairs = match LambdaCalcParser::parse(Rule::program, input) {
        Ok(pairs) => pairs,
        Err(e) => {
            eprintln!("{}", e);
            return terms;
        }
    };
    for pair in pairs {
        if let Rule::EOI = pair.as_rule() {
            break;
        }
        terms.push(parse_term(pair));
    }
    terms
}

/// Substitute a variable in a term with another term
/// This is used in β-reduction.
///
/// See https://en.wikipedia.org/wiki/Lambda_calculus#Substitution.
fn substitute(term: &Term, var: &str, value: &Term) -> Term {
    match term {
        // var[var := value] = value
        Term::Variable(v) if v == var => value.clone(),
        // x[var := value] = x   (x != var)
        Term::Variable(_) => term.clone(),
        // (e1 e2)[var := value] = (e1[var := value]) (e2[var := value])
        Term::Application(e1, e2) => Term::Application(
            Box::new(substitute(e1, var, value)),
            Box::new(substitute(e2, var, value)),
        ),
        // (λx. e)[var := value] = λx. e  (x == var)
        Term::Abstraction(s, _) if s == var => term.clone(), // Bound variable, no substitution needed
        // (λx. e)[var := value] = λx. e  (x in free_vars(value))
        Term::Abstraction(s, body) if free_vars(value).contains(s) => {
            // Avoid variable capture collisions by generating a fresh variable name
            let mut s_new = s.clone();
            while free_vars(value).contains(&s_new) {
                s_new.push('\'');
            }
            let new_body = substitute(&rename_var(body, s, &s_new), var, value);
            Term::Abstraction(s_new, Box::new(new_body))
        }
        // (λx. e)[var := value] = λx. e[var := value]  (x != var and x not in free_vars(value))
        Term::Abstraction(s, body) => {
            // Substitute inside the abstraction's body
            Term::Abstraction(s.clone(), Box::new(substitute(body, var, value)))
        }
        _ => unreachable!(),
    }
}

/// Collect free variables in a term
///
/// See https://en.wikipedia.org/wiki/Lambda_calculus#Free_and_bound_variables.
fn free_vars(term: &Term) -> HashSet<String> {
    match term {
        // free_vars(x) = {x}
        Term::Variable(s) => {
            let mut set = HashSet::new();
            set.insert(s.clone());
            set
        }
        // free_vars(λx. e) = free_vars(e) - {x}
        Term::Abstraction(s, body) => {
            let mut set = free_vars(body);
            set.remove(s);
            set
        }
        // free_vars(e1 e2) = free_vars(e1) + free_vars(e2)
        Term::Application(e1, e2) => {
            let mut set = free_vars(e1);
            set.extend(free_vars(e2));
            set
        }
        _ => unreachable!(),
    }
}

// Rename a variable in a term
fn rename_var(term: &Term, old_var: &str, new_var: &str) -> Term {
    match term {
        Term::Variable(s) if s == old_var => Term::Variable(new_var.to_string()),
        Term::Variable(_) => term.clone(),
        Term::Abstraction(s, body) if s == old_var => Term::Abstraction(
            new_var.to_string(),
            Box::new(rename_var(body, old_var, new_var)),
        ),
        Term::Abstraction(s, body) => {
            Term::Abstraction(s.clone(), Box::new(rename_var(body, old_var, new_var)))
        }

        Term::Application(e1, e2) => Term::Application(
            Box::new(rename_var(e1, old_var, new_var)),
            Box::new(rename_var(e2, old_var, new_var)),
        ),
        _ => unreachable!(),
    }
}

// Perform beta reduction on a lambda calculus term
fn beta_reduce(term: &Term) -> Term {
    match term {
        Term::Variable(_) => term.clone(),
        Term::Abstraction(var, body) => Term::Abstraction(var.clone(), Box::new(beta_reduce(body))),
        Term::Application(e1, e2) => {
            if let Term::Abstraction(var, body) = e1.borrow() {
                substitute(body, var, e2)
            } else {
                Term::Application(Box::new(beta_reduce(e1)), Box::new(beta_reduce(e2)))
            }
        }
        _ => unreachable!(),
    }
}

/// Evaluate a term in the given environment
/// by applying beta reduction until the term is in normal form
fn eval(term: &Term, env: &mut HashMap<String, Term>, verbose: bool) -> Term {
    fn reduce_to_normal_form(term: &Term, verbose: bool) -> Term {
        let mut term = term.clone();
        loop {
            let next = beta_reduce(&term);
            if next == term {
                return term;
            }
            term = next;
            if verbose {
                println!("{}", pretty_print(&term));
            }
        }
    }
    // Do the actual work
    if let Term::Assignment(name, val) = term {
        // Explicitly DON'T apply beta reduction here!
        // We want recursive combinators to not be evaluated until they are used
        let val = *val.clone();
        env.insert(name.clone(), val.clone());
        val
    } else {
        if verbose {
            println!("{}", pretty_print(term));
        }
        reduce_to_normal_form(term, verbose)
    }
}

/// Inline variables in a term using the given environment
fn inline_vars(term: &Term, env: &HashMap<String, Term>) -> Term {
    match &term {
        Term::Variable(v) => env.get(v).cloned().unwrap_or(term.clone()),
        Term::Assignment(name, val) => {
            Term::Assignment(name.clone(), Box::new(inline_vars(val, env)))
        }
        Term::Abstraction(param, body) => {
            Term::Abstraction(param.clone(), Box::new(inline_vars(body, env)))
        }
        Term::Application(f, x) => {
            Term::Application(Box::new(inline_vars(f, env)), Box::new(inline_vars(x, env)))
        }
    }
}

/// Run the given input program in the given environment
fn run(input: String, env: &mut HashMap<String, Term>, verbose: bool) {
    let terms = parse_prog(input.replace("\r", "").trim());
    if terms.is_empty() {
        return;
    }
    let mut result = terms[0].clone();
    for (i, term) in terms.iter().enumerate() {
        let term = inline_vars(term, env);
        result = eval(&term, env, verbose);
        if verbose && i < terms.len() - 1 {
            println!("{DARK_GRAY}------------------{RESET}");
        }
    }
    if !verbose {
        println!("{}", pretty_print(&result));
    }
}

/// Pretty print a term
fn pretty_print(term: &Term) -> String {
    fn print_term(term: &Term, top: bool) -> String {
        match term {
            Term::Variable(v) => v.clone(),
            Term::Assignment(name, val) => format!(
                "{}{DARK_GRAY} = {RESET}{}{DARK_GRAY};{RESET}",
                name,
                print_term(val, false)
            ),
            Term::Abstraction(param, body) => {
                let body = if matches!(**body, Term::Application(_, _)) {
                    format!(
                        "{DARK_GRAY}({RESET}{}{DARK_GRAY}){RESET}",
                        print_term(body, false)
                    )
                } else {
                    print_term(body, false)
                };
                format!("{YELLOW}λ{RESET}{}{DARK_GRAY}.{RESET}{}", param, body)
            }
            Term::Application(f, x) => {
                let lhs = if matches!(**f, Term::Variable(_)) {
                    print_term(f, false)
                } else {
                    format!(
                        "{DARK_GRAY}({RESET}{}{DARK_GRAY}){RESET}",
                        print_term(f, false)
                    )
                };
                let rhs = if matches!(**x, Term::Variable(_)) {
                    print_term(x, false)
                } else {
                    format!(
                        "{DARK_GRAY}({RESET}{}{DARK_GRAY}){RESET}",
                        print_term(x, false)
                    )
                };
                if top {
                    format!("{DARK_GRAY}({RESET}{} {}{DARK_GRAY}){RESET}", lhs, rhs)
                } else {
                    format!("{} {}", lhs, rhs)
                }
            }
        }
    }
    print_term(term, true)
}

fn main() {
    let mut env = HashMap::new();
    // If one argument is given, read that file, otherwise run REPL
    let mut args: Vec<String> = std::env::args().collect();
    // Remove --verbose flag if present
    let mut verbose = false;
    args.retain(|x| {
        match x.as_str() {
            "--help" | "-h" => help(),
            "--verbose" | "-v" => verbose = true,
            _ => return true,
        }
        false
    });
    if args.len() == 2 {
        run(
            std::fs::read_to_string(&args[1]).unwrap(),
            &mut env,
            verbose,
        );
    } else {
        use std::io::Write;
        loop {
            print!("> ");
            std::io::stdout().flush().unwrap();
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            run(input, &mut env, verbose);
        }
    }
}

fn help() -> ! {
    println!("Lambda calculus interpreter");
    println!("Usage: lambda [options] [file]");
    println!();
    println!("Options:");
    println!("  -h, --help     Print this help message");
    println!("  -v, --verbose  Print debug information");
    println!("  [file]         File to read lambda calculus program from");
    println!();
    println!("If no file is given, the program will run in REPL mode");
    std::process::exit(0);
}
