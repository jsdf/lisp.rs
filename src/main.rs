extern crate lisp_rs;

use lisp_rs::lisp::Env;

use std::io;
use std::io::prelude::*;

fn main() {
    let mut env = Env::standard();

    loop {
        print!("lisp.rs> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin().read_line(&mut input)
            .ok()
            .expect("Failed to read line");

        match env.read_eval(&input) {
            Ok(val) => println!("=> {}", val),
            Err(err) => println!("{}", err),
        }
    }
}
