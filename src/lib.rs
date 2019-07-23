// https://www.codewars.com/kata/symbolic-differentiation-of-prefix-expressions
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Debug, PartialEq)]
enum Term {
    Lit(f64),
    Var,
    Add(Box<Term>, Box<Term>),
    Sub(Box<Term>, Box<Term>),
    Mul(Box<Term>, Box<Term>),
    Div(Box<Term>, Box<Term>),
    Pow(Box<Term>, Box<Term>),
    Sin(Box<Term>),
    Cos(Box<Term>),
    Tan(Box<Term>),
    Exp(Box<Term>),
    Ln(Box<Term>),
}
impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit(l) => write!(f, "{}", l),
            Var => write!(f, "x"),
            Add(t1, t2) => write!(f, "(+ {} {})", t1, t2),
            Sub(t1, t2) => write!(f, "(- {} {})", t1, t2),
            Mul(t1, t2) => write!(f, "(* {} {})", t1, t2),
            Div(t1, t2) => write!(f, "(/ {} {})", t1, t2),
            Pow(t1, t2) => write!(f, "(^ {} {})", t1, t2),
            Cos(t) => write!(f, "(cos {})", t),
            Sin(t) => write!(f, "(sin {})", t),
            Tan(t) => write!(f, "(tan {})", t),
            Exp(t) => write!(f, "(exp {})", t),
            Ln(t) => write!(f, "(ln {})", t),
        }
    }
}
use Term::*;

fn diff_term(term: Term) -> Term {
    match term {
        Lit(_) => Lit(0.0),
        Var => Lit(1.0),
        Add(t1, t2) => Add(diff_box(t1), diff_box(t2)),
        Sub(t1, t2) => Sub(diff_box(t1), diff_box(t2)),
        // Product rule
        Mul(t1, t2) => Add(
            Box::new(Mul(diff_box(t1.clone()), t2.clone())),
            Box::new(Mul(t1, diff_box(t2))),
        ),
        // Quotient rule
        Div(t1, t2) => Div(
            Box::new(Sub(
                Box::new(Mul(diff_box(t1.clone()), t2.clone())),
                Box::new(Mul(t1, diff_box(t2.clone()))),
            )),
            Box::new(Pow(t2, Box::new(Lit(2.0)))),
        ),
        Pow(t1, t2) => match *t2 {
            Lit(l) => Mul(
                t2,
                Box::new(Mul(diff_box(t1.clone()), Box::new(Pow(t1, Box::new(Lit(l - 1.0)))))),
            ),
            _ => panic!("Non-literal exponent"),
        },
        Sin(t) => Mul(diff_box(t.clone()), Box::new(Cos(t))),
        Cos(t) => Mul(
            diff_box(t.clone()),
            Box::new(Mul(Box::new(Lit(-1.0)), Box::new(Sin(t)))),
        ),
        Tan(t) => Mul(
            diff_box(t.clone()),
            Box::new(Add(
                Box::new(Lit(1.0)),
                Box::new(Pow(Box::new(Tan(t)), Box::new(Lit(2.0)))),
            )),
        ),
        Exp(t) => Mul(diff_box(t.clone()), Box::new(Exp(t))),
        Ln(t) => Div(diff_box(t.clone()), t),
    }
}
fn diff_box(t: Box<Term>) -> Box<Term> {
    Box::new(diff_term(*t))
}

fn simplify(term: Term) -> Term {
    match term {
        Lit(l) => Lit(l),
        Var => Var,
        Add(t1, t2) => match (*simplify_box(t1), *simplify_box(t2)) {
            (Lit(0.0), t) => t,
            (t, Lit(0.0)) => t,
            (Lit(x), Lit(y)) => Lit(x + y),
            (t1, t2) => Add(Box::new(t1), Box::new(t2)),
        },
        Sub(t1, t2) => match (*simplify_box(t1), *simplify_box(t2)) {
            (t, Lit(0.0)) => t,
            (Lit(x), Lit(y)) => Lit(x - y),
            (t1, t2) => Sub(Box::new(t1), Box::new(t2)),
        },
        Mul(t1, t2) => match (*simplify_box(t1), *simplify_box(t2)) {
            (Lit(0.0), _) => Lit(0.0),
            (_, Lit(0.0)) => Lit(0.0),
            (Lit(1.0), t) => t,
            (t, Lit(1.0)) => t,
            (Lit(x), Lit(y)) => Lit(x * y),
            (t1, t2) => Mul(Box::new(t1), Box::new(t2)),
        },
        Div(t1, t2) => match (*simplify_box(t1), *simplify_box(t2)) {
            (t, Lit(1.0)) => t,
            (Lit(x), Lit(y)) => Lit(x / y),
            (t1, t2) => Div(Box::new(t1), Box::new(t2)),
        },
        Pow(t1, t2) => match (*simplify_box(t1), *simplify_box(t2)) {
            (Lit(0.0), Lit(0.0)) => Lit(1.0),
            (_, Lit(0.0)) => Lit(1.0),
            (t, Lit(1.0)) => t,
            (Lit(x), Lit(y)) if y > 0.0 => Lit(x.powf(y)),
            (t1, t2) => Pow(Box::new(t1), Box::new(t2)),
        },
        Sin(t) => Sin(simplify_box(t)),
        Cos(t) => Cos(simplify_box(t)),
        Tan(t) => Tan(simplify_box(t)),
        Exp(t) => Exp(simplify_box(t)),
        Ln(t) => Ln(simplify_box(t)),
    }
}
fn simplify_box(t: Box<Term>) -> Box<Term> {
    Box::new(simplify(*t))
}

fn parse(input: &mut Peekable<Chars>) -> Result<Term, String> {
    match input.next() {
        None => Err(String::from("Ran out of input")),
        Some(' ') => parse(input), // Transparently ignore spaces
        Some('x') => Ok(Var),
        Some(c @ '0'..='9') => Ok(Lit(parse_u32(input, c) as f64)),
        Some('+') => parse_box(input).and_then(|t1| parse_box(input).and_then(|t2| Ok(Add(t1, t2)))),
        Some('-') => match input.next() {
            // Got a digit, we're parsing something like `-234`
            Some(c) if c.is_digit(10) => Ok(Lit(-(parse_u32(input, c) as f64))),
            // Not an immediate digit, try to parse the subtraction operator
            Some(_) => parse_box(input).and_then(|t1| parse_box(input).and_then(|t2| Ok(Sub(t1, t2)))),
            None => Err(String::from("EOF after `-`")),
        },
        Some('*') => parse_box(input).and_then(|t1| parse_box(input).and_then(|t2| Ok(Mul(t1, t2)))),
        Some('/') => parse_box(input).and_then(|t1| parse_box(input).and_then(|t2| Ok(Div(t1, t2)))),
        Some('^') => parse_box(input).and_then(|t1| parse_box(input).and_then(|t2| Ok(Pow(t1, t2)))),
        Some('c') => parse_str(input, "os").and(parse_box(input).and_then(|t| Ok(Cos(t)))),
        Some('s') => parse_str(input, "in").and(parse_box(input).and_then(|t| Ok(Sin(t)))),
        Some('t') => parse_str(input, "an").and(parse_box(input).and_then(|t| Ok(Tan(t)))),
        Some('e') => parse_str(input, "xp").and(parse_box(input).and_then(|t| Ok(Exp(t)))),
        Some('l') => parse_str(input, "n").and(parse_box(input).and_then(|t| Ok(Ln(t)))),
        Some('(') => parse(input).and_then(|result| parse_str(input, ")").and(Ok(result))),
        Some(c) => Err(format!("Unknown start of token: {}", c)),
    }
}
fn parse_str(input: &mut Peekable<Chars>, s: &str) -> Result<(), String> {
    for c in s.chars() {
        let ci = input.next();
        if ci != Some(c) {
            let ci_str = ci.map_or(String::from("None"), |c| c.to_string());
            return Err(format!("Got {}, expected {} when matching {}", ci_str, c, s));
        }
    }
    Ok(())
}
fn parse_u32(input: &mut Peekable<Chars>, first_digit: char) -> u32 {
    let mut val = first_digit.to_digit(10).unwrap();
    loop {
        match input.peek().and_then(|c| c.to_digit(10)) {
            None => break,
            Some(d) => {
                val = 10 * val + d;
                input.next();
            }
        }
    }
    return val;
}
fn parse_box(input: &mut Peekable<Chars>) -> Result<Box<Term>, String> {
    parse(input).map(Box::new)
}

pub fn diff(expr: &str) -> String {
    let ref mut chars = expr.chars().peekable();
    match parse(chars) {
        Ok(t) => simplify(diff_term(t)).to_string(),
        Err(err) => err,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diff_term_lit() {
        assert_eq!(diff_term(Lit(4.0)), Lit(0.0));
        assert_eq!(diff_term(Lit(-132823.0)), Lit(0.0));
    }
    #[test]
    fn diff_term_var() {
        assert_eq!(diff_term(Var), Lit(1.0));
    }
    #[test]
    fn diff_term_add() {
        assert_eq!(
            diff_term(Add(Box::new(Lit(5.0)), Box::new(Var))),
            Add(Box::new(Lit(0.0)), Box::new(Lit(1.0)))
        );
    }

    #[test]
    fn simplify_simple() {
        assert_eq!(simplify(Mul(Box::new(Lit(0.0)), Box::new(Var))), Lit(0.0));
        assert_eq!(simplify(Mul(Box::new(Lit(1.0)), Box::new(Var))), Var);
        assert_eq!(simplify(Mul(Box::new(Lit(2.0)), Box::new(Lit(17.0)))), Lit(34.0));
        assert_eq!(simplify(Div(Box::new(Lit(8.0)), Box::new(Lit(2.0)))), Lit(4.0));
        assert_eq!(simplify(Div(Box::new(Lit(7.0)), Box::new(Lit(2.0)))), Lit(3.5));
        assert_eq!(simplify(Add(Box::new(Lit(0.0)), Box::new(Var))), Var);
    }

    #[test]
    fn test_simple() {
        assert_eq!(diff("5"), "0");
        assert_eq!(diff("x"), "1");
        assert_eq!(diff("5"), "0");
        assert_eq!(diff("(+ x x)"), "2");
        assert_eq!(diff("(- x x)"), "0");
        assert_eq!(diff("(* x 2)"), "2");
        assert_eq!(diff("(/ x 2)"), "0.5");
        assert_eq!(diff("(^ x 2)"), "(* 2 x)");
        assert_eq!(diff("(cos x)"), "(* -1 (sin x))");
        assert_eq!(diff("(sin x)"), "(cos x)");
        assert_eq!(diff("(tan x)"), "(+ 1 (^ (tan x) 2))");
        assert_eq!(diff("(exp x)"), "(exp x)");
        assert_eq!(diff("(ln x)"), "(/ 1 x)");
        assert_eq!(diff("(+ x (+ x x))"), "3");
        assert_eq!(diff("(- (+ x x) x)"), "1");
        assert_eq!(diff("(* 2 (+ x 2))"), "2");
        assert_eq!(diff("(/ 2 (+ 1 x))"), "(/ -2 (^ (+ 1 x) 2))");
        assert_eq!(diff("(cos (+ x 1))"), "(* -1 (sin (+ x 1)))");
        assert_eq!(diff("(sin (+ x 1))"), "(cos (+ x 1))");
        assert_eq!(diff("(sin (* 2 x))"), "(* 2 (cos (* 2 x)))");
        assert_eq!(diff("(tan (* 2 x))"), "(* 2 (+ 1 (^ (tan (* 2 x)) 2)))");
        assert_eq!(diff("(exp (* 2 x))"), "(* 2 (exp (* 2 x)))");
        assert_eq!(diff(&diff("(sin x)")), "(* -1 (sin x))");
        assert_eq!(diff(&diff("(exp x)")), "(exp x)");
    }
}
