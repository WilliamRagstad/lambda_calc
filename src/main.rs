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
#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Variable(String),
    Assignment(String, Box<Expr>),
    Abstraction(String, Box<Expr>),
    Application(Box<Expr>, Box<Expr>),
}

/// Parse a top-level program into a list of expressions
fn parse_prog(input: &str) -> Vec<Expr> {
    /// Transform a Pest pair into our own AST Expr node format
    fn parse_expr(pair: Pair<Rule>) -> Expr {
        match pair.as_rule() {
            Rule::variable => Expr::Variable(pair.as_str().to_string()),
            Rule::assignment => {
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap().as_str().to_string();
                let expr = parse_expr(inner.next().unwrap());
                Expr::Assignment(name, Box::new(expr))
            }
            Rule::abstraction => {
                let mut inner = pair.into_inner();
                let param = inner.next().unwrap().as_str().to_string();
                let body = parse_expr(inner.next().unwrap());
                Expr::Abstraction(param, Box::new(body))
            }
            Rule::application => {
                let mut inner = pair.into_inner();
                let lhs = parse_expr(inner.next().unwrap());
                let rhs = parse_expr(inner.next().unwrap());
                Expr::Application(Box::new(lhs), Box::new(rhs))
            }
            r => unreachable!("Rule {:?} not expected", r),
        }
    }

    let pairs = LambdaCalcParser::parse(Rule::program, input).unwrap_or_else(|e| panic!("{}", e));
    let mut exprs = Vec::new();
    for pair in pairs {
        if let Rule::EOI = pair.as_rule() {
            break;
        }
        exprs.push(parse_expr(pair));
    }
    exprs
}

/// Substitute a variable in an expression with another expression
/// This is used for beta reduction.
///
/// See https://en.wikipedia.org/wiki/Lambda_calculus#Substitution.
fn substitute(expr: &Expr, var: &str, value: &Expr) -> Expr {
    match expr {
        // var[var := value] = value
        Expr::Variable(v) if v == var => value.clone(),
        // x[var := value] = x   (x != var)
        Expr::Variable(_) => expr.clone(),
        // (e1 e2)[var := value] = (e1[var := value]) (e2[var := value])
        Expr::Application(e1, e2) => Expr::Application(
            Box::new(substitute(e1, var, value)),
            Box::new(substitute(e2, var, value)),
        ),
        // (λx. e)[var := value] = λx. e  (x == var)
        Expr::Abstraction(s, _) if s == var => expr.clone(), // Bound variable, no substitution needed
        // (λx. e)[var := value] = λx. e  (x in free_vars(value))
        Expr::Abstraction(s, body) if free_vars(value).contains(s) => {
            // Avoid variable capture by renaming
            let s_new = fresh_var(s);
            let new_body = substitute(&rename_var(body, s, &s_new), var, value);
            Expr::Abstraction(s_new, Box::new(new_body))
        }
        // (λx. e)[var := value] = λx. e[var := value]  (x != var and x not in free_vars(value))
        Expr::Abstraction(s, body) => {
            // Substitute inside the abstraction's body
            Expr::Abstraction(s.clone(), Box::new(substitute(body, var, value)))
        }
        _ => unreachable!(),
    }
}

/// Collect free variables in an expression
///
/// See https://en.wikipedia.org/wiki/Lambda_calculus#Free_and_bound_variables.
fn free_vars(expr: &Expr) -> HashSet<String> {
    match expr {
        // free_vars(x) = {x}
        Expr::Variable(s) => {
            let mut set = HashSet::new();
            set.insert(s.clone());
            set
        }
        // free_vars(λx. e) = free_vars(e) - {x}
        Expr::Abstraction(s, body) => {
            let mut set = free_vars(body);
            set.remove(s);
            set
        }
        // free_vars(e1 e2) = free_vars(e1) + free_vars(e2)
        Expr::Application(e1, e2) => {
            let mut set = free_vars(e1);
            set.extend(free_vars(e2));
            set
        }
        _ => unreachable!(),
    }
}

// Generate a fresh variable name to avoid name collisions
fn fresh_var(s: &str) -> String {
    format!("{}'", s)
}

// Rename a variable in an expression
fn rename_var(expr: &Expr, old_var: &str, new_var: &str) -> Expr {
    match expr {
        Expr::Variable(s) => {
            if s == old_var {
                Expr::Variable(new_var.to_string())
            } else {
                Expr::Variable(s.clone())
            }
        }
        Expr::Abstraction(s, body) => {
            let param = if s == old_var {
                new_var.to_string()
            } else {
                s.clone()
            };
            Expr::Abstraction(param, Box::new(rename_var(body, old_var, new_var)))
        }
        Expr::Application(e1, e2) => Expr::Application(
            Box::new(rename_var(e1, old_var, new_var)),
            Box::new(rename_var(e2, old_var, new_var)),
        ),
        _ => unreachable!(),
    }
}

// Perform beta reduction on a lambda calculus expression
fn beta_reduce(expr: &Expr) -> Expr {
    match expr {
        Expr::Variable(_) => expr.clone(),
        Expr::Abstraction(var, body) => Expr::Abstraction(var.clone(), Box::new(beta_reduce(body))),
        Expr::Application(e1, e2) => {
            if let Expr::Abstraction(var, body) = e1.borrow() {
                beta_reduce(&substitute(body, var, e2))
            } else {
                Expr::Application(Box::new(beta_reduce(e1)), Box::new(beta_reduce(e2)))
            }
        }
        _ => unreachable!(),
    }
}

/// Evaluate an expression in the given environment
/// by applying beta reduction until the expression is in normal form
fn eval(expr: &Expr, env: &mut HashMap<String, Expr>) -> Expr {
    fn reduce_to_normal_form(expr: &Expr) -> Expr {
        let mut expr = expr.clone();
        loop {
            let next = beta_reduce(&expr);
            if next == expr {
                return expr;
            }
            expr = next;
        }
    }
    // Do the actual work
    let expr = inline_vars(expr, env);
    if let Expr::Assignment(name, val) = expr {
        let val = reduce_to_normal_form(&val);
        env.insert(name.clone(), val.clone());
        val
    } else {
        reduce_to_normal_form(&expr)
    }
}

/// Inline variables in an expression using the given environment
fn inline_vars(expr: &Expr, env: &HashMap<String, Expr>) -> Expr {
    match &expr {
        Expr::Variable(v) => env.get(v).cloned().unwrap_or(expr.clone()),
        Expr::Assignment(name, val) => {
            Expr::Assignment(name.clone(), Box::new(inline_vars(val, env)))
        }
        Expr::Abstraction(param, body) => {
            Expr::Abstraction(param.clone(), Box::new(inline_vars(body, env)))
        }
        Expr::Application(f, x) => {
            Expr::Application(Box::new(inline_vars(f, env)), Box::new(inline_vars(x, env)))
        }
    }
}

/// Pretty print an expression
fn pretty_print(expr: &Expr) -> String {
    fn print_expr(expr: &Expr, top: bool) -> String {
        match expr {
            Expr::Variable(v) => v.clone(),
            Expr::Assignment(name, val) => format!(
                "{}{DARK_GRAY} = {RESET}{}{DARK_GRAY};{RESET}",
                name,
                print_expr(val, false)
            ),
            Expr::Abstraction(param, body) => {
                let body = if matches!(**body, Expr::Application(_, _)) {
                    format!(
                        "{DARK_GRAY}({RESET}{}{DARK_GRAY}){RESET}",
                        print_expr(body, false)
                    )
                } else {
                    print_expr(body, false)
                };
                format!("{YELLOW}λ{RESET}{}{DARK_GRAY}.{RESET}{}", param, body)
            }
            Expr::Application(f, x) => {
                let lhs = if matches!(**f, Expr::Variable(_)) {
                    print_expr(f, false)
                } else {
                    format!(
                        "{DARK_GRAY}({RESET}{}{DARK_GRAY}){RESET}",
                        print_expr(f, false)
                    )
                };
                let rhs = if matches!(**x, Expr::Variable(_)) {
                    print_expr(x, false)
                } else {
                    format!(
                        "{DARK_GRAY}({RESET}{}{DARK_GRAY}){RESET}",
                        print_expr(x, false)
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
    print_expr(expr, true)
}

/// Run the given input program in the given environment
fn run(input: String, env: &mut HashMap<String, Expr>) {
    let exprs = parse_prog(input.replace("\r", "").trim());
    println!(
        "{}",
        exprs
            .iter()
            .map(|e| inline_vars(e, env))
            .map(|e| pretty_print(&e))
            .collect::<Vec<_>>()
            .join("\n")
    );
    let mut exprs = exprs.into_iter();
    let first = exprs.next().expect("No expression found");
    let result = exprs.fold(eval(&first, env), |_, expr| eval(&expr, env));
    println!(
        "{DARK_GRAY}------------------{RESET}\n{}\n",
        pretty_print(&result)
    );
}

fn main() {
    let mut env = HashMap::new();
    // If one argument is given, read that file, otherwise run REPL
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 2 {
        run(std::fs::read_to_string(&args[1]).unwrap(), &mut env);
    } else {
        use std::io::Write;
        loop {
            print!("> ");
            std::io::stdout().flush().unwrap();
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            run(input, &mut env);
        }
    }
}
