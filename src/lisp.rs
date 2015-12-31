use itertools::Itertools;

use std::collections::HashMap;
use std::f64::consts;
use std::fmt;
use std::io;
use std::io::prelude::*;
use std::ops::*;
use std::rc::Rc;
use std::str::FromStr;

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

#[derive(Debug, Clone)]
struct Proc {
    params: Vec<Val>,
    body: Val,
    env: Rc<Env>,
}

impl Proc {
    fn new(params: Vec<Val>, body: Val, env: Rc<Env>) -> Proc {
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
            let mut local_env = Env::new(Some(self.env.clone()));
            for param in &self.params {
                let param_name = match *param {
                    Val::Symbol(ref x) => x.clone(),
                    _ => return Err(format!("param names must be symbols")),
                };
                local_env.define(param_name, args[0].clone()); // TODO: optimise
            }
            local_env.eval(self.body.clone())
        }
    }
}

#[derive(Debug, Clone)]
struct Env {
    vars: HashMap<String, Val>,
    parent: Option<Rc<Env>>,
}

impl Env {
    fn new(parent: Option<Rc<Env>>) -> Env {
        Env {
            vars: HashMap::new(),
            parent: parent,
        }
    }

    fn standard() -> Env {
        let mut env = Env::new(None);
        env.define("pi".to_string(), Val::Number(consts::PI));
        env
    }

    fn access(&self, var_name: &str) -> Option<&Val> {
        self.vars.get(var_name).or_else(|| {
            self.parent.as_ref().and_then(|parent| parent.access(&var_name))
        })
    }

    fn define(&mut self, var_name: String, val: Val) {
        match self.vars.insert(var_name, val) {
            Some(var_name) => panic!("can't define variable '{}', already defined in this scope", var_name),
            None => (),
        }
    }

    fn assign(&mut self, var_name: &str, val: Val) {
        match self.vars.get_mut(var_name) {
            Some(x) => { *x = val; },
            None => panic!("can't assign to undefined variable '{}'", var_name),
        }
    }

    fn eval(&mut self, val: Val) -> EvalResult<Val> {
        match val {
            Val::Symbol(x) => match self.access(&x) {
                Some(value) => Ok(value.clone()),
                None => Err(format!("can't access undefined variable '{}'", x)),
            },
            Val::Number(_) => Ok(val),
            Val::List(list) => {
                let mut args = list.into_iter();
                match args.next() {
                    Some(Val::Symbol(symbol)) => {
                        match symbol.trim() {
                            "quote" => Ok(args.next().unwrap()),
                            "if" => {
                                let test = args.next().unwrap();
                                let conseq = args.next().unwrap();
                                let alt = args.next().unwrap();
                                let test_result = try!(self.eval(test));
                                let exp = if !test_result.is_false() { conseq } else { alt };
                                self.eval(exp)
                            },
                            // "lambda" => {
                            //     let params = match args.remove(0) {
                            //         Val::List(x) => x,
                            //         _ => panic!("expected params to be a list"),
                            //     };
                            //     let body = args.remove(0);
                            //     Val::Callable(Proc::new(params, body, self.clone()))
                            // },
                            "define" => {
                                let var = args.next().unwrap();
                                let exp = args.next().unwrap();
                                let var_name = match var {
                                    Val::Symbol(name) => name,
                                    _ => return Err("first arg to define must be a symbol".to_string()),
                                };
                                let exp_result = try!(self.eval(exp));
                                self.define(var_name, exp_result);
                                Ok(Val::from(false))
                            },
                            // otherwise, call procedure
                            _ => {
                                let evaluated_args = try! {
                                    args.map(|arg| self.eval(arg))
                                        .fold_results(vec![], |mut acc, x| { acc.push(x); acc })
                                };
                                call_proc(&symbol, evaluated_args)
                            },
                        }
                    },
                    Some(_) | None => Err("unknown list form".to_string()),
                }
            },
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

fn read_eval_print_loop(mut env: Env) -> ! {
    loop {
        print!("lisp.rs> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin().read_line(&mut input)
            .ok()
            .expect("Failed to read line");

        match read_eval(&input, &mut env) {
            Ok(val) => println!("=> {}", val),
            Err(err) => println!("{}", err),
        }
    }
}

#[derive(Debug, Clone)]
enum ReplError {
    Parse(String),
    Eval(String),
}

impl fmt::Display for ReplError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ReplError::Parse(ref msg) => write!(f, "while reading: {}", msg),
            ReplError::Eval(ref msg) => write!(f, "while evaluating: {}", msg),
        }
    }
}

fn read_eval(program: &str, env: &mut Env) -> Result<Val, ReplError> {
    let val = try!(program.parse().map_err(ReplError::Parse));

    env.eval(val).map_err(ReplError::Eval)
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

type ParseResult<T> = Result<T, String>;

impl FromStr for Val {
    type Err = String;

    // def parse(program):
    //     "Read a Scheme expression from a string."
    //     return read_from_tokens(tokenize(program))
    fn from_str(src: &str) -> ParseResult<Val> {
        let mut tokens = tokenize(src);
        read_from_tokens(&mut tokens)
    }
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

fn read_from_tokens(tokens: &mut Vec<String>) -> ParseResult<Val> {
    if tokens.len() == 0 {
        return Err("unexpected EOF".to_string());
    }
    let token = tokens.remove(0);
    // println!("reading token: '{}'", token);
    if "(".to_string() == token {
        let mut list: Vec<Val> = Vec::new();
        while tokens[0] != ")" {
            list.push(try!(read_from_tokens(tokens)));
        }
        tokens.remove(0); // pop off ")"
        Ok(Val::List(list))
    } else if ")" == token {
        Err("unexpected ')'".to_string())
    } else {
        Ok(atom(token))
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
    let number = token.parse();
    match number {
        Ok(x) => Val::Number(x),
        Err(_) => Val::Symbol(token),
    }
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
    args.iter()
        .map(Val::extract_number)
        .fold_results(0.0, operator)
        .map(Val::Number)
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
