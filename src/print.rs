use crate::Term;

const DARK_GRAY: &str = "\x1b[90m";
const YELLOW: &str = "\x1b[33m";
const CYAN: &str = "\x1b[36m";
const GREEN: &str = "\x1b[32m";
const PINK: &str = "\x1b[35m";
const ITALIC: &str = "\x1b[3m";
const RESET: &str = "\x1b[0m";

pub fn line(len: usize) {
    println!("{}{}{}", DARK_GRAY, "-".repeat(len), RESET);
}

pub fn var(v: &str) -> String {
    match v {
        // booleans
        "true" => format!("{CYAN}{ITALIC}true{RESET}"),
        "false" => format!("{CYAN}{ITALIC}false{RESET}"),
        // function names
        _ if char::is_uppercase(v.chars().next().unwrap()) => {
            format!("{PINK}{}{RESET}", v)
        }
        // digits
        _ if v.chars().all(char::is_numeric) => {
            format!("{GREEN}{}{RESET}", v)
        }
        // variable names
        _ => format!("{ITALIC}{}{RESET}", v),
    }
}

/// Pretty print a term
pub fn term(t: &Term) -> String {
    match t {
        Term::Variable(v) => var(v),
        Term::Abstraction(param, body) => {
            let body = if matches!(**body, Term::Application(_, _)) {
                format!("{DARK_GRAY}({RESET}{}{DARK_GRAY}){RESET}", term(body))
            } else {
                term(body)
            };
            format!("{YELLOW}λ{RESET}{}{DARK_GRAY}.{RESET}{}", var(param), body)
        }
        Term::Application(f, x) => {
            let lhs = if matches!(**f, Term::Variable(_)) {
                term(f)
            } else {
                format!("{DARK_GRAY}({RESET}{}{DARK_GRAY}){RESET}", term(f))
            };
            let rhs = if matches!(**x, Term::Variable(_)) {
                term(x)
            } else {
                format!("{DARK_GRAY}({RESET}{}{DARK_GRAY}){RESET}", term(x))
            };
            format!("{DARK_GRAY}({RESET}{} {}{DARK_GRAY}){RESET}", lhs, rhs)
        }
    }
}
