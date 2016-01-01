extern crate lisp;
extern crate linenoise;

use lisp::Env;

fn main() {
    let mut env = Env::standard();

    linenoise::history_load("history");
    while let Some(input) = linenoise::input("lisp.rs> ") {
        let val = match input.parse() {
            Ok(val) => val,
            Err(err) => {
                println!("while reading: {}", err);
                continue;
            },
        };

        linenoise::history_add(&input);

        match Env::eval(&mut env, val) {
            Ok(val) => println!("=> {}", val),
            Err(err) => println!("while evaluating: {}", err),
        }
    }
    linenoise::history_save("history");

    println!("Bye bye!");
}
