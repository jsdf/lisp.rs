use itertools::Itertools;

use std::collections::HashMap;
use std::f64::consts;
use std::fmt;
use std::io;
use std::io::prelude::*;
use std::ops::*;
use std::rc::Rc;

#[derive(Debug, Clone)]
enum Val {
    List(Vec<Val>),
    Number(f64),
    Symbol(String),
    // Callable(Proc),
}

impl<'a> From<&'a str> for Val {
    fn from(x: &'a str) -> Val {
        Val::Symbol(x.to_string())
    }
}

impl From<bool> for Val {
    fn from(x: bool) -> Val {
        if x { Val::from("#t") } else { Val::from("#f") }
    }
}

type EvalResult<T> = Result<T, String>;

impl Val {
    fn extract_number(&self) -> EvalResult<f64> {
        match *self {
            Val::Number(x) => Ok(x),
            ref val => Err(format!("expected a Number, found '{}'", val)),
        }
    }

    fn gt(a: &Val, b: &Val) -> EvalResult<Val> {
        let a = try!(a.extract_number());
        let b = try!(b.extract_number());

        Ok(Val::from(a > b))
    }

    fn lt(a: &Val, b: &Val) -> EvalResult<Val> {
        let a = try!(a.extract_number());
        let b = try!(b.extract_number());

        Ok(Val::from(a < b))
    }

    fn gte(a: &Val, b: &Val) -> EvalResult<Val> {
        let a = try!(a.extract_number());
        let b = try!(b.extract_number());

        Ok(Val::from(a >= b))
    }

    fn lte(a: &Val, b: &Val) -> EvalResult<Val> {
        let a = try!(a.extract_number());
        let b = try!(b.extract_number());

        Ok(Val::from(a <= b))
    }

    fn eq(a: &Val, b: &Val) -> EvalResult<Val> {
        fn eq_bool(a: &Val, b: &Val) -> EvalResult<bool> {
            match (a, b) {
                (&Val::Symbol(ref a), &Val::Symbol(ref b)) => Ok(a == b),
                (&Val::Number(a), &Val::Number(b)) => Ok(a == b),
                (&Val::List(ref a), &Val::List(ref b)) if a.len() != b.len() => Ok(false),
                (&Val::List(ref a), &Val::List(ref b)) => {
                    <_>::zip(a.iter(), b.iter())
                        .map(|(a, b)| eq_bool(a, b))
                        .fold_results(true, |acc, x| acc && x)
                },
                (a, b) => Err(format!("incompatible types passed to eq, found {} and {}", a, b))
            }
        }

        eq_bool(a, b).map(Val::from)
    }

    fn is_false(&self) -> bool {
        match *self {
            Val::Symbol(ref x) => x == "#f",
            _ => false,
        }
    }

    fn not(&self) -> Val {
        Val::from(self.is_false())
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Val::List(ref xs) => write!(f, "({})", xs.iter().format(" ", |x, f| f(x))),
            Val::Number(ref x) => write!(f, "{}", x),
            Val::Symbol(ref x) => write!(f, "{}", x),
        }
    }
}

type EnvRef = Rc<Option<Env>>;

#[derive(Debug, Clone)]
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

    fn call(&self, args: Vec<Val>) -> EvalResult<Val> {
        if args.len() != self.params.len() {
            Err(format!("incorrect number of args for func, expected {}, got {}", self.params.len(), args.len()))
        } else {
            let mut local_env = Env::new(self.env.clone());
            for param in &self.params {
                let param_name = match *param {
                    Val::Symbol(ref x) => x.clone(),
                    _ => return Err(format!("param names must be symbols")),
                };
                local_env.define(&param_name, args[0].clone()); // TODO: optimise
            }
            let local_env_ref: EnvRef = Rc::new(Some(local_env));
            eval(self.body.clone(), local_env_ref)
        }
    }
}

#[derive(Debug, Clone)]
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
    match eval(parse(program), env.clone()) {
        Ok(val) => println!("=> {}", val),
        Err(msg) => println!("error: {}", msg),
    }
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
        while tokens[0] != ")" {
            list.push(read_from_tokens(tokens));
        }
        tokens.remove(0); // pop off ")"
        Val::List(list)
    } else if ")" == token {
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

fn standard_env() -> EnvRef {
    let null_env_ref: EnvRef = Rc::new(None);
    let mut env = Env::new(null_env_ref);
    env.define("pi", Val::Number(consts::PI));
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

fn eval(val: Val, env: EnvRef) -> EvalResult<Val> {
    match val {
        Val::Symbol(x) => Ok(Rc::try_unwrap(env).unwrap().unwrap().access(&x)),
        Val::Number(_) => Ok(val),
        Val::List(list) => {
            let mut args = list;
            let proc_name = args.remove(0);

            match proc_name {
                Val::Symbol(symbol) => {
                    match symbol.trim() {
                        "quote" => Ok(args.remove(0)),
                        "if" => {
                            let test = args.remove(0);
                            let conseq = args.remove(0);
                            let alt = args.remove(0);
                            let test_result = try!(eval(test, env.clone()));
                            let exp = if !test_result.is_false() { conseq } else { alt };
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
                                _ => return Err("first arg to define must be a symbol".to_string()),
                            };
                            let exp_result = try!(eval(exp, env.clone()));
                            Rc::try_unwrap(env).unwrap().unwrap().define(&var_name, exp_result);
                            Ok(Val::from(false))
                        },
                        // otherwise, call procedure
                        _ => {
                            let evaluated_args = try! {
                                args.iter()
                                    .map(|arg| eval(arg.clone(), env.clone()))
                                    .fold_results(Vec::with_capacity(args.len()), |mut acc, x| { acc.push(x); acc })
                            };
                            call_proc(&symbol, evaluated_args)
                        },
                    }
                },
                _ => Err("unknown list form".to_string()),
            }
        },
    }
}

fn call_proc(proc_name: &str, mut args: Vec<Val>) -> EvalResult<Val> {
    match proc_name.trim() {
        "+" => apply_arithmetic(args, f64::add),
        "-" => apply_arithmetic(args, f64::sub),
        "*" => apply_arithmetic(args, f64::mul),
        "/" => apply_arithmetic(args, f64::div),
        ">" => apply2(args, Val::gt),
        "<" => apply2(args, Val::lt),
        ">=" => apply2(args, Val::gte),
        "<=" => apply2(args, Val::lte),
        "=" => apply2(args, Val::eq),
        "not" => apply1(args, |x| Ok(x.not())),
        "list" => Ok(Val::List(args)),
        "begin" => {
            match args.pop() {
                Some(x) => Ok(x),
                None => Ok(Val::from(false)),
            }
        },
        _ => Err(format!("unknown proc '{}'", proc_name)),
    }
}

fn apply_arithmetic<F: Fn(f64, f64) -> f64>(args: Vec<Val>, operator: F) -> EvalResult<Val> {
    let mut accumulated: f64 = 0f64;
    for (i, arg) in args.iter().enumerate() {
        accumulated = match *arg {
            Val::Number(operand) => {
                if i == 0 { operand } else { operator(accumulated, operand) }
            },
            _ => return Err(format!("args to arithmetic functions must be Numbers, found {}", arg)),
        };
    }
    Ok(Val::Number(accumulated))
}

fn apply1<F: Fn(&Val) -> EvalResult<Val>>(args: Vec<Val>, func: F) -> EvalResult<Val> {
    if args.len() != 1 {
        Err(format!("incorrect number of args for func, expected 1, got {}", args.len()))
    } else {
        func(&args[0])
    }
}

fn apply2<F: Fn(&Val, &Val) -> EvalResult<Val>>(args: Vec<Val>, func: F) -> EvalResult<Val> {
    if args.len() != 2 {
        Err(format!("incorrect number of args for func, expected 2, got {}", args.len()))
    } else {
        func(&args[0], &args[1])
    }
}
