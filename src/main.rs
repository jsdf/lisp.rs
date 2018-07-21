extern crate lisp;
extern crate linenoise;

use lisp::{Env, EnvCell};

use std::rc::Rc;

fn main() {
    let mut env = Env::standard();

    linenoise::history_load("history");

    while let Some(input) = linenoise::input("lisp.rs> ") {
        linenoise::history_add(&input);

        match &*input {
            "env" => println!("{}", *env.borrow()),
            input => read_eval_print(&mut env, input),
        }
    }

    linenoise::history_save("history");

    println!("Bye bye!");
}

fn read_eval_print(env: &mut Rc<EnvCell>, input: &str) {
    match input.parse() {
        Ok(val) => match Env::eval(env, val) {
            Ok(val) => println!("=> {}", val),
            Err(err) => println!("while evaluating: {}", err),
        },
        Err(err) => {
            println!("while reading: {}", err);
            return;
        },
    };


}
