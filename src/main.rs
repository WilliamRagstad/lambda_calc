use std::collections::HashMap;

mod eval;
mod parser;
mod print;
mod test;

use eval::{eval_prog, PrinterFn};
use parser::Term;

pub const PRINT_NONE: PrinterFn = |_| {};
pub const PRINT_OUT: PrinterFn = |t| println!("{}", t);
pub const PRINT_DBG: PrinterFn = |t| {
    println!("{}", t);
    print::pause("Paused: Enter to step");
};

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
            PRINT_OUT,
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
                ":cls" | ":clear" => {
                    print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
                    continue;
                }
                ":env" => {
                    if args.len() == 2 && args[1] == "clear" {
                        env.clear();
                    } else {
                        for (name, term) in &env {
                            println!("{} = {}", name, print::term(term));
                        }
                    }
                    continue;
                }
                ":std" => {
                    eval_prog(
                        include_str!("./std.lc").into(),
                        &mut env,
                        verbose,
                        PRINT_OUT,
                    );
                    continue;
                }
                ":load" => {
                    let Some(file) = args.get(1) else {
                        eprintln!("Usage: :load <file>");
                        continue;
                    };
                    if let std::io::Result::Ok(content) = std::fs::read_to_string(file) {
                        eval_prog(content, &mut env, verbose, PRINT_OUT);
                    } else {
                        eprintln!("Error reading file");
                    }
                    continue;
                }
                ":dbg" => {
                    // Step through the program evaluation
                    let input = args[1..].join(" ");
                    eval_prog(input, &mut env, verbose, PRINT_DBG);
                    continue;
                }
                ":help" => {
                    println!("Commands:");
                    println!("  :q, :quit      Quit the program");
                    println!("  :cls, :clear   Clear the screen");
                    println!("  :env           Print the current environment");
                    println!("  :env clear     Clear the current environment");
                    println!("  :load <file>   Load a file into the environment");
                    println!("  :std           Load the standard library");
                    println!("  :dbg <prog>    Step through the evaluation");
                    println!("  :help          Print this help message");
                    continue;
                }
                cmd if cmd.starts_with(":") => {
                    eprintln!("Unknown command: {}, try :help", cmd);
                    continue;
                }
                _ => {}
            }
            eval_prog(input, &mut env, verbose, PRINT_OUT);
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
