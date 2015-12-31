use std::collections::HashMap;
use std::f64::consts;
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

#[derive(Debug,Clone)]
enum Val {
    List(Vec<Val>),
    Number(f64),
    Symbol(String),
    // Callable(Proc),
}

type EnvRef = Rc<Option<Env>>;

#[derive(Debug,Clone)]
struct Proc {
    params: Vec<Val>,
    body: Val,
    env: EnvRef,
}

impl Proc {
    fn new(params: Vec<Val>, body: Val, env: EnvRef) -> Proc {
        Proc {
            params: params,
            body: body,
            env: env,
        }
    }

    fn call(&self, args: Vec<Val>) -> Val {
        if args.len() != self.params.len() {
            panic!("incorrect number of args for func, expected {}, got {}", self.params.len(), args.len());
        } else {
            let mut local_env = Env::new(self.env.clone());
            for param in &self.params {
                let param_name = match *param {
                    Val::Symbol(ref x) => x.clone(),
                    _ => panic!("param names must be symbols"),
                };
                local_env.define(&param_name, args[0].clone()); // TODO: optimise
            }
            let local_env_ref: EnvRef = Rc::new(Some(local_env));
            eval(self.body.clone(), local_env_ref)
        }
    }
}

#[derive(Debug,Clone)]
struct Env {
    vars: HashMap<String, Val>,
    parent: EnvRef,
}

impl Env {
    fn new(parent: EnvRef) -> Env {
        Env {
            vars: HashMap::new(),
            parent: parent,
        }
    }

    fn access(&self, var_name: &str) -> Val {
        match self.vars.get(var_name) {
            Some(x) => x.clone(),
            None => {
                match *self.parent {
                    Some(ref parent) => parent.access(&var_name),
                    None => panic!("can't access undefined variable '{}'", var_name),
                }
            },
        }
    }

    fn define(&mut self, var_name: &str, val: Val) {
        match self.vars.insert(var_name.to_owned(), val) {
            Some(_) => panic!("can't define variable '{}', already defined in this scope", var_name),
            None => (),
        }
    }

    fn assign(&mut self, var_name: &str, val: Val) {
        match self.vars.get_mut(var_name) {
            Some(x) => { *x = val; },
            None => panic!("can't assign to undefined variable '{}'", var_name),
        }
    }
}

// lifetimes:
// expressions are consumed by eval
// vals are copied when storing to and accessing from the environment

pub fn run() {
    let global_env = standard_env();
    read_eval_print_loop(global_env);
}

fn read_eval_print_loop(env: EnvRef) -> ! {
    loop {
        print!("lisp.rs> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin().read_line(&mut input)
            .ok()
            .expect("Failed to read line");

        read_eval_print(input.trim(), env.clone());
    }
}

fn read_eval_print(program: &str, env: EnvRef) {
    let program_result = eval(parse(program), env.clone());
    print!("=> ");
    print_val(&program_result);
}

fn format_list(list: &Vec<Val>) -> String {
    let formatted_items: Vec<String> = list.iter().map(|item| format_val(&item)).collect();

    format!("({})", formatted_items.join(" "))
}

fn format_val(val: &Val) -> String {
    match *val {
        Val::List(ref x) => format_list(&x),
        Val::Number(ref x) => format!("{}", x),
        Val::Symbol(ref x) => format!("{}", x),
    }
}

fn print_val(val: &Val) {
    println!("{}", format_val(&val));
}

// def tokenize(chars):
//     "Convert a string of characters into a list of tokens."
//     return chars.replace('(', ' ( ').replace(')', ' ) ').split()

fn tokenize(chars: &str) -> Vec<String> {
    chars
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|s| s.to_string())
        .collect()
}

// def parse(program):
//     "Read a Scheme expression from a string."
//     return read_from_tokens(tokenize(program))

fn parse(program: &str) -> Val {
    let mut tokens = tokenize(program);
    read_from_tokens(&mut tokens)
}

// def read_from_tokens(tokens):
//     "Read an expression from a sequence of tokens."
//     if len(tokens) == 0:
//         raise SyntaxError('unexpected EOF while reading')
//     token = tokens.pop(0)
//     if '(' == token:
//         L = []
//         while tokens[0] != ')':
//             L.append(read_from_tokens(tokens))
//         tokens.pop(0) # pop off ')'
//         return L
//     elif ')' == token:
//         raise SyntaxError('unexpected )')
//     else:
//         return atom(token)

fn read_from_tokens(tokens: &mut Vec<String>) -> Val {
    if tokens.len() == 0 {
        panic!("unexpected EOF while reading");
    }
    let token = tokens.remove(0);
    // println!("reading token: '{}'", token);
    if "(".to_string() == token {
        let mut list: Vec<Val> = Vec::new();
        while tokens[0] != ")".to_string() {
            list.push(read_from_tokens(tokens));
        }
        tokens.remove(0); // pop off ")"
        Val::List(list)
    } else if ")".to_string() == token {
        panic!("unexpected ')' while reading");
    } else {
        atom(token)
    }
}

// def atom(token):
//     "Numbers become numbers; every other token is a symbol."
//     try: return int(token)
//     except ValError:
//         try: return float(token)
//         except ValError:
//             return Symbol(token)

fn atom(token: String) -> Val {
    // TODO: separate int/float types?
    let number = token.parse::<f64>();
    match number {
        Ok(x) => Val::Number(x),
        Err(_) => Val::Symbol(token),
    }
}

fn symbol_true() -> Val {
    Val::Symbol("#t".to_string())
}

fn symbol_false() -> Val {
    Val::Symbol("#f".to_string())
}

fn standard_env() -> EnvRef {
    let null_env_ref: EnvRef = Rc::new(None);
    let mut env = Env::new(null_env_ref);
    env.define(&("pi".to_string()), Val::Number(consts::PI));
    Rc::new(Some(env))
}

// def eval(x, env=global_env):
//     "Evaluate an expression in an environment."
//     if isinstance(x, Symbol):      # variable reference
//         return env[x]
//     elif not isinstance(x, List):  # constant literal
//         return x                
//     elif x[0] == 'quote':          # (quote exp)
//         (_, exp) = x
//         return exp
//     elif x[0] == 'if':             # (if test conseq alt)
//         (_, test, conseq, alt) = x
//         exp = (conseq if eval(test, env) else alt)
//         return eval(exp, env)
//     elif x[0] == 'define':         # (define var exp)
//         (_, var, exp) = x
//         env[var] = eval(exp, env)
//     else:                          # (proc arg...)
//         proc = eval(x[0], env)
//         args = [eval(arg, env) for arg in x[1:]]
//         return proc(*args)

fn eval(val: Val, env: EnvRef) -> Val {
    // println!("eval {:?}", &val);
    let result = match val {
        Val::Symbol(x) => Rc::try_unwrap(env).unwrap().unwrap().access(&x),
        Val::Number(_) => val,
        Val::List(list) => {
            let mut args = list;
            let proc_name = args.remove(0);

            match proc_name {
                Val::Symbol(symbol) => {
                    match symbol.trim() {
                        "quote" => args.remove(0),
                        "if" => {
                            let test = args.remove(0);
                            let conseq = args.remove(0);
                            let alt = args.remove(0);
                            let test_result = eval(test, env.clone());
                            let exp = if !is_false(test_result) { conseq } else { alt };
                            eval(exp, env.clone())
                        },
                        // "lambda" => {
                        //     let params = match args.remove(0) {
                        //         Val::List(x) => x,
                        //         _ => panic!("expected params to be a list"),
                        //     };
                        //     let body = args.remove(0);
                        //     Val::Callable(Proc::new(params, body, env.clone()))
                        // },
                        "define" => {
                            let var = args.remove(0);
                            let exp = args.remove(0);
                            let var_name = match var {
                                Val::Symbol(ref x) => x,
                                _ => panic!("first arg to define must be a symbol"),
                            };
                            let exp_result = eval(exp, env.clone());
                            Rc::try_unwrap(env).unwrap().unwrap().define(&var_name, exp_result);
                            symbol_false()
                        },
                        // otherwise, call procedure
                        _ => {
                            let evaluated_args: Vec<Val> = args.iter()
                                .map(|arg| eval(arg.clone(), env.clone()))
                                .collect();
                            call_proc(&symbol, evaluated_args)
                        },
                    }
                },
                _ => panic!("unknown list form"),
            }
        },
    };
    // println!("eval result {:?}", &result);
    result
}

fn call_proc(proc_name: &str, mut args: Vec<Val>) -> Val {
    match proc_name.trim() {
        "+" => apply_arithmetic(args, add),
        "-" => apply_arithmetic(args, sub),
        "*" => apply_arithmetic(args, mul),
        "/" => apply_arithmetic(args, div),
        ">" => apply2(args, gt),
        "<" => apply2(args, lt),
        ">=" => apply2(args, gte),
        "<=" => apply2(args, lte),
        "=" => apply2(args, eq),
        "not" => apply1(args, not),
        "list" => Val::List(args),
        "begin" => {
            match args.pop() {
                Some(x) => x,
                None => symbol_false(),
            }
        },
        _ => panic!("unknown proc"),
    }
}

fn apply_arithmetic<F: Fn(f64, f64) -> f64>(args: Vec<Val>, operator: F) -> Val {
    let mut accumulated: f64 = 0f64;
    for (i, arg) in args.iter().enumerate() {
        accumulated = match *arg {
            Val::Number(operand) => {
                if i == 0 { operand } else { operator(accumulated, operand) }
            },
            _ => panic!("args to arithmetic functions must be Numbers"),
        };
    };
    Val::Number(accumulated)
}

fn add(a: f64, b: f64) -> f64 {
    a + b
}

fn sub(a: f64, b: f64) -> f64 {
    a - b
}

fn mul(a: f64, b: f64) -> f64 {
    a * b
}

fn div(a: f64, b: f64) -> f64 {
    a / b
}

fn apply1<F: Fn(Val) -> Val>(args: Vec<Val>, func: F) -> Val {
    if args.len() != 1 {
        panic!("incorrect number of args for func, expected 1, got {}", args.len());
    } else {
        func(args[0].clone())
    }
}

fn apply2<F: Fn(Val, Val) -> Val>(args: Vec<Val>, func: F) -> Val {
    if args.len() != 2 {
        panic!("incorrect number of args for func, expected 2, got {}", args.len());
    } else {
        func(args[0].clone(), args[1].clone())
    }
}

fn bool_to_symbol(x: bool) -> Val {
    if x { symbol_true() } else { symbol_false() }
}

fn extract_number(val: Val) -> f64 {
    match val {
        Val::Number(x) => x,
        _ => panic!("expected a Number"),
    }
}

fn extract_symbol(val: Val) -> String {
    match val {
        Val::Symbol(x) => x,
        _ => panic!("expected a Symbol"),
    }
}

fn gt(a: Val, b: Val) -> Val {
    bool_to_symbol(extract_number(a) > extract_number(b))
}

fn lt(a: Val, b: Val) -> Val {
    bool_to_symbol(extract_number(a) < extract_number(b))
}

fn gte(a: Val, b: Val) -> Val {
    bool_to_symbol(extract_number(a) >= extract_number(b))
}

fn lte(a: Val, b: Val) -> Val {
    bool_to_symbol(extract_number(a) <= extract_number(b))
}

fn eq(a: Val, b: Val) -> Val {
    match a {
        Val::Symbol(_) => bool_to_symbol(extract_symbol(a) == extract_symbol(b)),
        Val::Number(_) => bool_to_symbol(extract_number(a) == extract_number(b)),
        Val::List(_) => panic!("equality operator not implemented for List :("),
    }
}

fn is_false(val: Val) -> bool {
    match val {
        Val::Symbol(x) => x == "#f",
        _ => false,
    }
}

fn not(val: Val) -> Val {
    let boolean_value = !is_false(val);
    bool_to_symbol(!boolean_value)
}
