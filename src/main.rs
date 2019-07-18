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
        Mul(t1, t2) => Add(Box::new(Mul(diff_box(t1), t2)), Box::new(Mul(t1, diff_box(t2)))),
        // Quotient rule
        Div(t1, t2) => Div(
            Box::new(Sub(Box::new(Mul(diff_box(t1), t2)), Box::new(Mul(t1, diff_box(t2))))),
            Box::new(Pow(t2, Box::new(Lit(2))))
        ),
        Pow(t1, t2) => match *t2 {
            Lit(l) => Mul(t2, Box::new(Mul(diff_box(t1), Box::new(Pow(t1, Box::new(Lit(l - 1))))))),
            _ => panic!("Non-literal exponent"),
        },
        Sin(t) => Mul(diff_box(t), Box::new(Cos(t))),
        Cos(t) => Mul(Box::new(Lit(-1)), Box::new(Mul(diff_box(t), Box::new(Sin(t))))),
        Tan(t) => diff(Div(Box::new(Sin(t)), Box::new(Cos(t)))),
        Exp(t) => Mul(diff_box(t), Box::new(term)),
        Ln(t) => Div(diff_box(t), t),
    }
}
fn diff_box(t: Box<Term>) -> Box<Term> {
    Box::new(diff(*t))
}

fn main() {
    println!("Hello, world!");
}
