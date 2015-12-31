use itertools::Itertools;

use std::collections::HashMap;
use std::f64::consts;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

pub type Intrinsic = fn(Vec<Val>) -> EvalResult<Val>;

#[derive(Debug, Clone)]
pub enum Val {
    List(Vec<Val>),
    Number(f64),
    Symbol(String),
    // Callable(Proc),
    Intrinsic(Intrinsic),
}

impl<'a> From<&'a str> for Val {
    fn from(x: &'a str) -> Val {
        Val::Symbol(x.to_string())
    }
}
impl From<f64> for Val {
    fn from(x: f64) -> Val {
        Val::Number(x)
    }
}

impl From<bool> for Val {
    fn from(x: bool) -> Val {
        if x { Val::from("#t") } else { Val::from("#f") }
    }
}

pub type EvalResult<T> = Result<T, String>;

impl Val {
    fn extract_number(&self) -> EvalResult<f64> {
        match *self {
            Val::Number(x) => Ok(x),
            ref val => Err(format!("expected a Number, found '{}'", val)),
        }
    }

    fn is_false(&self) -> bool {
        match *self {
            Val::Symbol(ref x) => x == "#f",
            _ => false,
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Val::List(ref xs) => write!(f, "({})", xs.iter().format(" ", |x, f| f(x))),
            Val::Number(ref x) => write!(f, "{}", x),
            Val::Symbol(ref x) => write!(f, "{}", x),
            Val::Intrinsic(_) => write!(f, "<intrinsic>"),
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
pub struct Env {
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

    pub fn standard() -> Env {
        let mut env = Env::new(None);
        env.define("pi", Val::Number(consts::PI));
        env.define("+", Val::Intrinsic(intrinsics::add));
        env.define("-", Val::Intrinsic(intrinsics::sub));
        env.define("*", Val::Intrinsic(intrinsics::mul));
        env.define("/", Val::Intrinsic(intrinsics::div));
        env.define(">", Val::Intrinsic(intrinsics::gt));
        env.define("<", Val::Intrinsic(intrinsics::lt));
        env.define(">=", Val::Intrinsic(intrinsics::ge));
        env.define("<=", Val::Intrinsic(intrinsics::le));
        env.define("=", Val::Intrinsic(intrinsics::eq));
        env.define("not", Val::Intrinsic(intrinsics::not));
        env.define("list", Val::Intrinsic(intrinsics::list));
        env
    }

    fn access(&self, var_name: &str) -> Option<&Val> {
        self.vars.get(var_name).or_else(|| {
            self.parent.as_ref().and_then(|parent| parent.access(&var_name))
        })
    }

    fn define<S: Into<String>>(&mut self, var_name: S, val: Val) {
        match self.vars.insert(var_name.into(), val) {
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

    pub fn eval(&mut self, val: Val) -> EvalResult<Val> {
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
                            symbol => {
                                if let Some(&Val::Intrinsic(f)) = self.access(symbol) {
                                    args.map(|arg| self.eval(arg))
                                        .fold_results(vec![], |mut acc, x| { acc.push(x); acc })
                                        .and_then(f)
                                } else {
                                    Err(format!("the function '{}' was not defined", symbol))
                                }
                            },
                        }
                    },
                    Some(_) | None => Err("unknown list form".to_string()),
                }
            },
            Val::Intrinsic(f) => {
                Ok(Val::Intrinsic(f))
            },
        }
    }

    pub fn read_eval(&mut self, src: &str) -> Result<Val, ReplError> {
        let val = try!(src.parse().map_err(ReplError::Parse));

        self.eval(val).map_err(ReplError::Eval)
    }
}

#[derive(Debug, Clone)]
pub enum ReplError {
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

mod intrinsics {
    use itertools::Itertools;

    use lisp::{EvalResult, Val};

    use std::ops::*;

    fn foldf64<F: Fn(f64, f64) -> f64>(args: Vec<Val>, acc: f64, f: F) -> EvalResult<Val> {
        args.iter()
            .map(Val::extract_number)
            .fold_results(acc, f)
            .map(Val::from)
    }

    fn apply1<F: Fn(&Val) -> EvalResult<Val>>(args: Vec<Val>, f: F) -> EvalResult<Val> {
        if args.len() != 1 {
            Err(format!("incorrect number of args for func, expected 1, got {}", args.len()))
        } else {
            f(&args[0])
        }
    }

    fn apply2<F: Fn(&Val, &Val) -> EvalResult<Val>>(args: Vec<Val>, f: F) -> EvalResult<Val> {
        if args.len() != 2 {
            Err(format!("incorrect number of args for func, expected 2, got {}", args.len()))
        } else {
            f(&args[0], &args[1])
        }
    }

    fn apply2f64<F: Fn(f64, f64) -> EvalResult<Val>>(args: Vec<Val>, f: F) -> EvalResult<Val> {
        if args.len() != 2 {
            Err(format!("incorrect number of args for func, expected 2, got {}", args.len()))
        } else {
            f(try!(args[0].extract_number()),
              try!(args[1].extract_number()))
        }
    }

    pub fn add(args: Vec<Val>) -> EvalResult<Val> {
        foldf64(args, 0.0, f64::add)
    }

    pub fn sub(args: Vec<Val>) -> EvalResult<Val> {
        foldf64(args, 0.0, f64::sub)
    }

    pub fn mul(args: Vec<Val>) -> EvalResult<Val> {
        foldf64(args, 1.0, f64::mul)
    }

    pub fn div(args: Vec<Val>) -> EvalResult<Val> {
        foldf64(args, 1.0, f64::div)
    }

    pub fn gt(args: Vec<Val>) -> EvalResult<Val> {
        apply2f64(args, |a, b| Ok(Val::from(a > b)))
    }

    pub fn lt(args: Vec<Val>) -> EvalResult<Val> {
        apply2f64(args, |a, b| Ok(Val::from(a < b)))
    }

    pub fn ge(args: Vec<Val>) -> EvalResult<Val> {
        apply2f64(args, |a, b| Ok(Val::from(a >= b)))
    }

    pub fn le(args: Vec<Val>) -> EvalResult<Val> {
        apply2f64(args, |a, b| Ok(Val::from(a <= b)))
    }

    pub fn eq(args: Vec<Val>) -> EvalResult<Val> {
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

        apply2(args, |a, b| eq_bool(a, b).map(Val::from))
    }

    pub fn not(args: Vec<Val>) -> EvalResult<Val> {
        apply1(args, |x| Ok(Val::from(x.is_false())))
    }

    pub fn list(args: Vec<Val>) -> EvalResult<Val> {
        Ok(Val::List(args))
    }
}
