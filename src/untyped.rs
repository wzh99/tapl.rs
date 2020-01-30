pub use crate::util::*;

#[derive(Debug)]
pub enum Term {
    Var(isize), // variable (represented by de Bruijn index)
    Abs(Rc<Term>), // lambda abstraction
    App(Rc<Term>, Rc<Term>) // application of lambda to value
}

trait VarMap {
    fn map_var(&self, t: &Rc<Term>, c: isize) -> Rc<Term>;
}

fn map_var(on_var: &dyn VarMap, t: &Rc<Term>) -> Rc<Term> {
    map_var_walk(on_var, t, 0)
}

fn map_var_walk(on_var: &dyn VarMap, t: &Rc<Term>, c: isize) -> Rc<Term> {
    match t.as_ref() {
        Term::Var(_) => on_var.map_var(t, c),
        Term::Abs(t1) => rc(Term::Abs(map_var_walk(on_var, t1, c + 1))),
        Term::App(t1, t2) =>
            rc(Term::App(map_var_walk(on_var, t1, c), map_var_walk(on_var, t2, c)))
    }
}

fn shift(d: isize, t: &Rc<Term>) -> Rc<Term> {
    struct Shift { d: isize }
    impl VarMap for Shift {
        fn map_var(&self, t: &Rc<Term>, c: isize) -> Rc<Term> {
            if let Term::Var(x) = t.as_ref() {
                if *x >= c { rc(Term::Var(*x + self.d)) } else { t.clone() }
            } else {
                panic!()
            }
        }
    }
    map_var(&Shift{d}, t)
}

fn subst_top(s: &Rc<Term>, t: &Rc<Term>) -> Rc<Term> {
    shift(-1, &subst(0, &shift(1, s), t))
}

fn subst(j: isize, s: &Rc<Term>, t: &Rc<Term>) -> Rc<Term> {
    struct Subst { j: isize, s: Rc<Term> }
    impl VarMap for Subst {
        fn map_var(&self, t: &Rc<Term>, c: isize) -> Rc<Term> {
            if let Term::Var(x) = t.as_ref() {
                if *x == self.j + c { shift(c, &self.s) } else { t.clone() }
            } else {
                panic!()
            }
        }
    }
    map_var(&Subst{j, s: s.clone()}, t)
}

pub fn eval(t: &Rc<Term>) -> Rc<Term> {
    match eval_once(t).as_ref() {
        Some(r) => eval(r),
        None => t.clone()
    }
}

fn eval_once(t: &Rc<Term>) -> Option<Rc<Term>> {
    // The implementation here pay special attention to free variables.
    // A free variable is not a value, but it is in normal form. Any attempt to evaluate an
    // application with variables will cause infinite recursion in this function.
    if let Term::App(t1, t2) = t.as_ref() {
        match t1.as_ref() {
            Term::Abs(t12) =>
                match t2.as_ref() {
                    Term::Abs(_) => Some(subst_top(t2, t12)),
                    Term::App(_, _) => Some(rc(Term::App(t1.clone(), eval_once(t2)?))),
                    Term::Var(_) => None
                }
            Term::App(_, _) => Some(rc(Term::App(eval_once(t1)?, t2.clone()))),
            Term::Var(_) => Some(rc(Term::App(t1.clone(), eval_once(t2)?)))
        }
    } else {
        None
    }
}

#[test]
fn test() {
    let input = // (lambda. 1 0 2) lambda. 0
        rc(Term::App(
            rc(Term::Abs(
                rc(Term::App(
                    rc(Term::App(
                        rc(Term::Var(1)),
                        rc(Term::Var(0))
                    )),
                    rc(Term::Var(2))
                ))
            )),
            rc(Term::Abs(
                rc(Term::Var(0))
            ))
        ));
    println!("{:?}", input);
    let result = eval(&input);
    println!("{:?}", result); // expect: 0 (lambda. 0) 1
}
