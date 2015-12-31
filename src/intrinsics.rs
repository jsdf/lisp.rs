use itertools::Itertools;

use {EvalResult, Val};

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
