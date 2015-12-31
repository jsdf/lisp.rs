extern crate lisp;
extern crate linenoise;

use lisp::Env;

fn main() {
    let mut env = Env::standard();

    while let Some(input) = linenoise::input("lisp.rs> ") {
        match env.read_eval(&input) {
            Ok(val) => {
                linenoise::history_add(&input);
                println!("=> {}", val)
            },
            Err(err) => println!("{}", err),
        }
    }
    println!("Bye bye!");
}
