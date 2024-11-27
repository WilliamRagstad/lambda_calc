use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    io,
};

use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

mod print;
mod test;

/// Lambda calculus parser using pest
#[derive(Parser)]
#[grammar = "grammar.pest"]
struct LambdaCalcParser;

/// AST for our extended lambda calculus program
#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Assignment(String, Term),
    Term(Term),
}

/// A program is a list of expressions
type Program = Vec<Expr>;

/// Environment mapping variable names to terms
type Env = HashMap<String, Term>;

/// AST for lambda calculus
///
/// See https://en.wikipedia.org/wiki/Lambda_calculus#Definition.
#[derive(Debug, Clone, PartialEq)]
enum Term {
    Variable(String),
    Abstraction(String, Box<Term>),
    Application(Box<Term>, Box<Term>),
}

/// Parse a top-level program into a list of terms
fn parse_prog(input: &str) -> Program {
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
            Rule::application => {
                let mut inner = pair.into_inner();
                let lhs = parse_term(inner.next().unwrap());
                let rhs = parse_term(inner.next().unwrap());
                Term::Application(Box::new(lhs), Box::new(rhs))
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
        term = next;
        if verbose {
            println!("{}", print::term(&term));
        }
        reduce_to_normal_form(term, verbose)
    }
}

/// Inline variables in a term using the given environment
fn inline_vars(term: &Term, env: &HashMap<String, Term>) -> Term {
    match &term {
        Term::Variable(v) => env.get(v).cloned().unwrap_or(term.clone()),
        Term::Abstraction(param, body) => {
            Term::Abstraction(param.clone(), Box::new(inline_vars(body, env)))
        }
        Term::Application(f, x) => {
            Term::Application(Box::new(inline_vars(f, env)), Box::new(inline_vars(x, env)))
        }
    }
}

fn eval_expr(expr: &Expr, env: &mut Env, verbose: bool) -> Term {
    match expr {
        Expr::Assignment(name, val) => {
            if verbose {
                println!("{} = {};", print::var(name), print::term(val));
            }
            // Explicitly DON'T apply beta reduction here!
            // We want recursive combinators to not be evaluated until they are used
            env.insert(name.clone(), val.clone());
            val.clone()
        }
        Expr::Term(term) => {
            let term = inline_vars(term, env);
            if verbose {
                println!("{}", print::term(&term));
        }
    }
}

/// Run the given input program in the given environment
fn eval_prog(input: String, env: &mut Env, verbose: bool) {
    let terms: Program = parse_prog(input.replace("\r", "").trim());
    for (i, expr) in terms.iter().enumerate() {
        let term = eval_expr(expr, env, verbose);
        if matches!(expr, Expr::Assignment(_, _)) {
            continue;
        }
        if verbose {
            // Print all terms and their reduction steps
            // println!("{}", print::term(&term));
            if i < terms.len() - 1 {
                print::line(20);
            }
        }
        if !verbose && i == terms.len() - 1 {
            // Always print the last term if not in verbose mode
            println!("{}", print::term(&term));
        }
    }
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
        eval_prog(
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
            let args: Vec<&str> = input.trim().split(' ').collect::<Vec<&str>>();
            match *args.first().unwrap_or(&"") {
                ":q" | ":quit" => break,
                ":env" => {
                    for (name, term) in &env {
                        println!("{} = {}", name, print::term(term));
                    }
                    continue;
                }
                ":clear" => {
                    env.clear();
                    continue;
                }
                ":load" => {
                    let Some(file) = args.get(1) else {
                        eprintln!("Usage: :load <file>");
                        continue;
                    };
                    if let io::Result::Ok(content) = std::fs::read_to_string(file) {
                        eval_prog(content, &mut env, verbose);
                    } else {
                        eprintln!("Error reading file");
                    }
                    continue;
                }
                ":help" => {
                    println!("Commands:");
                    println!("  :q, :quit      Quit the program");
                    println!("  :env           Print the current environment");
                    println!("  :clear         Clear the current environment");
                    println!("  :load <file>   Load a file into the environment");
                    println!("  :help          Print this help message");
                    continue;
                }
                _ => {}
            }
            eval_prog(input, &mut env, verbose);
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
