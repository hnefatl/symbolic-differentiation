// https://www.codewars.com/kata/symbolic-differentiation-of-prefix-expressions

#[derive(Clone, Debug, PartialEq)]
enum Term {
    Lit(i32),
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
use Term::*;

fn diff(term: Term) -> Term {
    match term {
        Lit(_) => Lit(0),
        Var => Lit(1),
        Add(t1, t2) => Add(diff_box(t1), diff_box(t2)),
        Sub(t1, t2) => Sub(diff_box(t1), diff_box(t2)),
        // Product rule
        Mul(t1, t2) => Add(Box::new(Mul(diff_box(t1.clone()), t2.clone())), Box::new(Mul(t1, diff_box(t2)))),
        // Quotient rule
        Div(t1, t2) => Div(
            Box::new(Sub(Box::new(Mul(diff_box(t1.clone()), t2.clone())), Box::new(Mul(t1, diff_box(t2.clone()))))),
            Box::new(Pow(t2, Box::new(Lit(2))))
        ),
        Pow(t1, t2) => match *t2 {
            Lit(l) => Mul(t2, Box::new(Mul(diff_box(t1.clone()), Box::new(Pow(t1, Box::new(Lit(l - 1))))))),
            _ => panic!("Non-literal exponent"),
        },
        Sin(t) => Mul(diff_box(t.clone()), Box::new(Cos(t))),
        Cos(t) => Mul(Box::new(Lit(-1)), Box::new(Mul(diff_box(t.clone()), Box::new(Sin(t))))),
        Tan(t) => diff(Div(Box::new(Sin(t.clone())), Box::new(Cos(t)))),
        Exp(t) => Mul(diff_box(t.clone()), Box::new(Exp(t))),
        Ln(t) => Div(diff_box(t.clone()), t),
    }
}
fn diff_box(t: Box<Term>) -> Box<Term> {
    Box::new(diff(*t))
}

fn simplify(term: Term) -> Term {
    match term {
        Lit(l) => Lit(l),
        Var => Var,
        Add(t1, t2) => match (*simplify_box(t1), *simplify_box(t2)) {
            (Lit(0), t) => t,
            (t, Lit(0)) => t,
            (Lit(x), Lit(y)) => Lit(x + y),
            (t1, t2) => Add(Box::new(t1), Box::new(t2))
        },
        Sub(t1, t2) => match (*simplify_box(t1), *simplify_box(t2)) {
            (t, Lit(0)) => t,
            (Lit(x), Lit(y)) => Lit(x - y),
            (t1, t2) => Sub(Box::new(t1), Box::new(t2))
        },
        Mul(t1, t2) => match (*simplify_box(t1), *simplify_box(t2)) {
            (Lit(1), t) => t,
            (t, Lit(1)) => t,
            (Lit(x), Lit(y)) => Lit(x * y),
            (t1, t2) => Mul(Box::new(t1), Box::new(t2))
        },
        Div(t1, t2) => match (*simplify_box(t1), *simplify_box(t2)) {
            (t, Lit(1)) => t,
            (Lit(x), Lit(y)) if x % y == 0 => Lit(x / y),
            (t1, t2) => Div(Box::new(t1), Box::new(t2))
        },
        Pow(t1, t2) => match (*simplify_box(t1), *simplify_box(t2)) {
            (Lit(0), Lit(0)) => Lit(1),
            (t, Lit(0)) => Lit(1),
            (t, Lit(1)) => t,
            (Lit(x), Lit(y)) if y > 0 => Lit(x.pow(y as u32)),
            (t1, t2) => Pow(Box::new(t1), Box::new(t2))
        },
        _ => term
    }
}
fn simplify_box(t: Box<Term>) -> Box<Term> {
    Box::new(simplify(*t))
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diff_lit() {
        assert_eq!(diff(Lit(4)), Lit(0));
        assert_eq!(diff(Lit(-132823)), Lit(0));
    }
    #[test]
    fn diff_var() {
        assert_eq!(diff(Var), Lit(1));
    }
    #[test]
    fn diff_add() {
        assert_eq!(diff(Add(Box::new(Lit(5)), Box::new(Var))), Add(Box::new(Lit(0)), Box::new(Lit(1))));
    }
}