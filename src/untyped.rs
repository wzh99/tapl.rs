pub use crate::util::*;

#[derive(Debug)]
pub enum Term {
    Var(i32), // variable (represented by de Bruijn index)
    Abs(Rc<Term>), // lambda abstraction
    App(Rc<Term>, Rc<Term>) // application of lambda to value
}

impl Term {
    fn is_value(&self) -> bool {
        if let Term::Abs(_) = self { true } else { false }
    }
}

/// Top level shift
fn shift(d: i32, t: &Rc<Term>) -> Rc<Term> {
    // Recursive call to closure is forbidden in Rust, so the recursive function is 
    // implemented separately, instead of nestedly.
    shift_walk(d, 0, t)
}

/// Recursively walk the AST to shift
fn shift_walk(d: i32, c: i32, t: &Rc<Term>) -> Rc<Term> {
    match t.as_ref() {
        Term::Var(x) => 
            if *x >= c { rc(Term::Var(*x + d)) } else { t.clone() }
        Term::Abs(t1) => rc(Term::Abs(shift_walk(d, c + 1, t1))),
        Term::App(t1, t2) =>
            rc(Term::App(shift_walk(d, c, t1), shift_walk(d, c, t2)))
    }
}

fn subst_top(s: &Rc<Term>, t: &Rc<Term>) -> Rc<Term> {
    shift(-1, &subst(0, &shift(1, s), t))
}

/// Substitution function
fn subst(j: i32, s: &Rc<Term>, t: &Rc<Term>) -> Rc<Term> {
    subst_walk(j, s, 0, t)
}

/// Recursively walk the AST to substitute
fn subst_walk(j: i32, s: &Rc<Term>, c: i32, t: &Rc<Term>) -> Rc<Term> {
    match t.as_ref() {
        Term::Var(x) => if *x == j + c { shift(c, s) } else { t.clone() }
        Term::Abs(t1) => rc(Term::Abs(subst_walk(j, s, c + 1, t1))),
        Term::App(t1, t2) => 
            rc(Term::App(subst_walk(j, s, c, t1), subst_walk(j, s, c, t2)))
    }
}

pub fn eval(t: &Rc<Term>) -> Rc<Term> {
    match t.as_ref() {
        Term::App(t1, t2) => {
            if let Term::Abs(t12) = t1.as_ref() { // t1 is a value
                if t2.is_value() {
                    eval(&subst_top(t2, t12)) 
                } else { 
                    eval(&rc(Term::App(t1.clone(), eval(t2))))
                }
            } else { // t1 is not a value
                eval(&rc(Term::App(eval(t1), t2.clone())))
            }
        }
        _ => t.clone()
    }
}

#[test]
fn test() {

}
