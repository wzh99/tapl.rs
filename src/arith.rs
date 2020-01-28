pub use crate::util::*;

#[derive(Debug)]
pub enum Term {
    True,
    False,
    If(Rc<Term>, Rc<Term>, Rc<Term>),
    Zero,
    Succ(Rc<Term>),
    Pred(Rc<Term>),
    IsZero(Rc<Term>)
}

impl Term {
    fn is_numeric(&self) -> bool {
        match self {
            Term::Zero => true,
            Term::Succ(t1) => t1.is_numeric(),
            _ => false
        }
    }
}

pub fn eval(t: &Rc<Term>) -> Rc<Term> {
    match t.as_ref() {
        Term::If(t1, t2, t3) =>
            match t1.as_ref() {
                Term::True => eval(t2),
                Term::False => eval(t3),
                _ => eval(&rc(Term::If(eval(t1), t2.clone(), t3.clone())))
            }
        Term::Succ(t1) => rc(Term::Succ(eval(t1))),
        Term::Pred(t1) => {
            match t1.as_ref() {
                Term::Zero => rc(Term::Zero),
                Term::Succ(nv1) if nv1.is_numeric() => nv1.clone(),
                _ => eval(&rc(Term::Pred(eval(t1))))
            }
        }
        Term::IsZero(t1) => {
            match t1.as_ref() {
                Term::Zero => rc(Term::True),
                Term::Succ(nv1) if nv1.is_numeric() => rc(Term::False),
                _ => eval(&rc(Term::IsZero(eval(t1))))
            }
        }
        _ => t.clone()
    }
}

#[test]
fn test() {
    let input = // if true then iszero pred pred succ 0 else false
        rc(Term::If(
            rc(Term::True),
            rc(Term::IsZero(
                rc(Term::Pred(
                    rc(Term::Pred(
                        rc(Term::Succ(
                            rc(Term::Zero)
                        ))
                    ))
                ))
            )),
            rc(Term::False)
        ));
    println!("{:?}", input);
    let result = eval(&input);
    println!("{:?}", result); // expect true
}