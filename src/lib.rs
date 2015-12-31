extern crate itertools;

use itertools::Itertools;

use std::collections::HashMap;
use std::f64::consts;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

mod intrinsics;

pub type Intrinsic = fn(Vec<Val>) -> EvalResult<Val>;

#[derive(Debug, Clone)]
pub enum Val {
    Bool(bool),
    Number(f64),
    Symbol(String),
    List(Vec<Val>),
    Closure(Closure),
    Intrinsic(Intrinsic),
}

pub type EvalResult<T> = Result<T, String>;

impl Val {
    fn extract_number(&self) -> EvalResult<f64> {
        match *self {
            Val::Number(x) => Ok(x),
            ref val => Err(format!("expected a Number, found '{}'", val)),
        }
    }

    fn extract_symbol(&self) -> EvalResult<&str> {
        match *self {
            Val::Symbol(ref x) => Ok(&x),
            ref val => Err(format!("expected a Number, found '{}'", val)),
        }
    }

    fn is_false(&self) -> bool {
        match *self {
            Val::Bool(false) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Number(x) => write!(f, "{}", x),
            Val::Symbol(ref x) => write!(f, "{}", x),
            Val::List(ref xs) => write!(f, "({})", xs.iter().format(" ", |x, f| f(x))),
            Val::Closure(ref cl) => write!(f, "(lambda ({}) ...)", cl.params.iter().format(" ", |param, f| f(param))),
            Val::Intrinsic(_) => write!(f, "<intrinsic>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    params: Vec<String>,
    body: Box<Val>,
    parent: Rc<Env>,
}

impl Closure {
    fn new(params: Vec<String>, body: Val, parent: Rc<Env>) -> Closure {
        Closure {
            params: params,
            body: Box::new(body),
            parent: parent,
        }
    }

    fn call(&self, args: Vec<Val>) -> EvalResult<Val> {
        if args.len() != self.params.len() {
            Err(format!("incorrect number of args for func, expected {}, got {}", self.params.len(), args.len()))
        } else {
            let mut local_env = Env::new(Some(self.parent.clone()));
            for (ident, val) in <_>::zip(self.params.iter(), args.into_iter()) {
                local_env.define(&ident[..], val);
            }
            local_env.eval((*self.body).clone())
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
        env.define("id", Val::Closure(Closure::new(vec!["x".to_string()],
                                                   Val::Symbol("x".to_string()),
                                                   Rc::new(Env::new(None)))));
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

    fn eval_args<I: IntoIterator<Item = Val>>(&mut self, args: I) -> EvalResult<Vec<Val>> {
        args.into_iter()
            .map(|arg| self.eval(arg))
            .fold_results(vec![], |mut acc, x| { acc.push(x); acc })
    }

    pub fn eval(&mut self, val: Val) -> EvalResult<Val> {
        match val {
            Val::Bool(_) => Ok(val),
            Val::Number(_) => Ok(val),
            Val::Symbol(x) => match self.access(&x) {
                Some(value) => Ok(value.clone()),
                None => Err(format!("can't access undefined variable '{}'", x)),
            },
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
                            "lambda" => {
                                let params = match args.next().unwrap() {
                                    Val::List(x) => try! {
                                        x.iter()
                                            .map(Val::extract_symbol)
                                            .fold_results(vec![], |mut acc, x| { acc.push(x.to_string()); acc })
                                    },
                                    _ => return Err("expected params to be a list".to_string()),
                                };
                                let body = args.next().unwrap();
                                Ok(Val::Closure(Closure::new(params, body, Rc::new(self.clone())))) // Ack!
                            },
                            "define" => {
                                let var = args.next().unwrap();
                                let exp = args.next().unwrap();
                                let var_name = match var {
                                    Val::Symbol(name) => name,
                                    _ => return Err("first arg to define must be a symbol".to_string()),
                                };
                                let exp_result = try!(self.eval(exp));
                                self.define(var_name, exp_result);
                                Ok(Val::Bool(false))
                            },
                            // otherwise, call procedure
                            symbol => {
                                let args = try!(self.eval_args(args));
                                match self.access(symbol) {
                                    Some(&Val::Closure(ref cl)) => cl.call(args),
                                    Some(&Val::Intrinsic(f)) => f(args),
                                    Some(val) => Err(format!("expected '{}' to be a function, but found '{}'", symbol, val)),
                                    None => Err(format!("the function '{}' was not defined", symbol)),
                                }
                            },
                        }
                    },
                    // TODO: anonymous closures
                    Some(_) | None => Err("unknown list form".to_string()),
                }
            },
            Val::Closure(_) => Ok(val),
            Val::Intrinsic(_) => Ok(val),
        }
    }
}

type ParseResult<T> = Result<T, String>;

impl FromStr for Val {
    type Err = String;

    fn from_str(src: &str) -> ParseResult<Val> {
        Parser::new(src).parse_value()
    }
}

struct Parser<'a> {
    src: &'a str,
    byte_pos: usize,
    current: &'a str,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Parser<'a> {
        Parser { src: src, byte_pos: 0, current: src }
    }

    fn is_eof(&self) -> bool {
        self.byte_pos >= self.src.len()
    }

    fn peek(&self) -> Option<char> {
        self.current.chars().next()
    }

    fn advance_bytes(&mut self, n: usize) {
        self.byte_pos += n;

        if self.is_eof() {
            self.byte_pos = self.src.len();
            self.current = "";
        } else {
            self.current = &self.current[n..];
        }
    }

    fn eat_char(&mut self, ch: char) -> bool {
        if self.current.starts_with(ch) {
            self.advance_bytes(ch.len_utf8());
            true
        } else {
            false
        }
    }

    fn consume_while<F: Fn(char) -> bool>(&mut self, f: F) -> &str {
        let char_index = self.current.char_indices()
            .skip_while(|&(_, ch)| f(ch))
            .next();

        match char_index {
            Some((pos, _)) => {
                let slice = &self.current[..pos];
                self.advance_bytes(pos);
                slice
            },
            None => "",
        }
    }

    fn skip_while<F: Fn(char) -> bool>(&mut self, f: F) -> bool {
        let char_index = self.current.char_indices()
            .skip_while(|&(_, ch)| f(ch))
            .next();

        match char_index {
            Some((pos, _)) => { self.advance_bytes(pos); true },
            None => false,
        }
    }

    fn skip_whitespace(&mut self) -> bool {
        self.skip_while(char::is_whitespace)
    }

    fn parse_value(&mut self) -> ParseResult<Val> {
        self.skip_whitespace();
        if self.eat_char('(') {
            self.parse_list()
        } else {
            match self.peek() {
                Some(')') => Err("unexpected `)`".to_string()),
                Some(_) => self.parse_atom(),
                None => Err("unexpected EOF".to_string()),
            }
        }
    }

    fn parse_list(&mut self) -> ParseResult<Val> {
        let mut list = vec![];
        loop {
            if self.is_eof() {
                return Err("unexpected EOF".to_string());
            }

            if self.eat_char(')') {
                return Ok(Val::List(list));
            }

            list.push(try!(self.parse_value()));

            self.skip_whitespace();
        }
    }

    fn parse_atom(&mut self) -> ParseResult<Val> {
        let atom = self.consume_while(|ch| ch != ')' && !ch.is_whitespace());

        match atom {
            "#t" => Ok(Val::Bool(true)),
            "#f" => Ok(Val::Bool(false)),
            // TODO: separate int/float types?
            atom => match atom.parse() {
                Ok(x) => Ok(Val::Number(x)),
                Err(_) => Ok(Val::Symbol(atom.to_string())),
            },
        }
    }
}
