pub use crate::util::*;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Type {
    Unit,
    Bool,
    Nat,
    String,
    Arr(Rc<Type>, Rc<Type>),
    Record(Vec<(String, Rc<Type>)>),
    Variant(Vec<(String, Rc<Type>)>)
}

#[derive(Debug)]
pub enum Term {
    // Lambda Calculus
    Var(isize),
    Abs(Rc<Type>, Rc<Term>), // lambda $x: T. t1
    App(Rc<Term>, Rc<Term>), // t1 t2
    // Unit
    Unit,
    // Bool
    True,
    False,
    // Nat
    Zero,
    IsZero(Rc<Term>),
    Pred(Rc<Term>),
    Succ(Rc<Term>),
    // String
    String(String),
    // Conditional
    If(Rc<Term>, Rc<Term>, Rc<Term>),
    // Ascription
    Ascribe(Rc<Term>, Rc<Type>), // t1 as T
    // Let Binding
    Let(Rc<Term>, Rc<Term>), // let $x = t1 in t2
    // Record
    Record(Vec<(String, Rc<Term>)>), // { l_i = t_i }
    Proj(Rc<Term>, String), // t.l_i
    // Variant
    Tag(String, Rc<Term>, Rc<Type>), // <l = t> as T
    Case(Rc<Term>, Vec<(String, Rc<Term>)>), // case t of <l_i = $x_i> => t_i
    // General Recursion
    Fix(Rc<Term>), // fix t
}

impl Term {
    fn is_numeric(&self) -> bool {
        match self {
            Term::Zero => true,
            Term::Succ(t1) => t1.is_numeric(),
            _ => false
        }
    }

    fn is_val(&self) -> bool {
        if self.is_numeric() { return true }
        match self {
            Term::Abs(_, _) => true,
            Term::Unit | Term::True | Term::False | Term::String(_) => true,
            Term::Record(fields) =>
                fields.iter().all(|(_, t)| t.is_val()),
            Term::Tag(_, t, _) => t.is_val(),
            _ => false
        }
    }
}

fn map_var<F>(f: &F, t: &Rc<Term>) -> Rc<Term> where
    F: Fn(&Rc<Term>, isize) -> Rc<Term> {
    walk(f, t, 0)
}

fn walk<F>(f: &F, t: &Rc<Term>, c: isize) -> Rc<Term> where
    F: Fn(&Rc<Term>, isize) -> Rc<Term> {
    match t.as_ref() {
        Term::Var(_) => f(t, c),
        Term::Abs(param_type, t1) =>
            rc(Term::Abs(param_type.clone(), walk(f, t1, c + 1))),
        Term::App(t1, t2) =>
            rc(Term::App(walk(f, t1, c), walk(f, t2, c))),
        Term::IsZero(t1) => rc(Term::IsZero(walk(f, t1, c))),
        Term::Pred(t1) => rc(Term::Pred(walk(f, t1, c))),
        Term::Succ(t1) => rc(Term::Succ(walk(f, t1, c))),
        Term::If(t1, t2, t3) =>
            rc(Term::If(walk(f, t1, c), walk(f, t2, c), walk(f, t3, c))),
        Term::Ascribe(t1, type_t1) =>
            rc(Term::Ascribe(walk(f, t1, c), type_t1.clone())),
        Term::Let(t1, t2) =>
            rc(Term::Let(walk(f, t1, c), walk(f, t2, c + 1))),
        Term::Record(fields) =>
            rc(Term::Record(fields.iter()
                .map(|(l, ti)| (l.clone(), walk(f, ti, c)))
                .collect()
            )),
        Term::Proj(t1, l) =>
            rc(Term::Proj(walk(f, t1, c), l.clone())),
        Term::Tag(l, t1, type_t1) =>
            rc(Term::Tag(l.clone(), walk(f, t1, c), type_t1.clone())),
        Term::Case(t1, cases) =>
            rc(Term::Case(walk(f, t1, c), cases.iter()
                .map(|(l, ti)| (l.clone(), walk(f, ti, c + 1)))
                .collect()
            )),
        Term::Fix(t1) => rc(Term::Fix(walk(f, t1, c))),
        _ => t.clone()
    }
}

fn shift(d: isize, t: &Rc<Term>) -> Rc<Term> {
    map_var(&|t, c| {
        if let Term::Var(x) = t.as_ref() {
            if *x >= c { rc(Term::Var(*x + d)) } else { t.clone() }
        } else {
            panic!()
        }
    }, t)
}

fn subst_top(s: &Rc<Term>, t: &Rc<Term>) -> Rc<Term> {
    shift(-1, &subst(0, &shift(1, s), t))
}

fn subst(j: isize, s: &Rc<Term>, t: &Rc<Term>) -> Rc<Term> {
    map_var(&|t, c| {
        if let Term::Var(x) = t.as_ref() {
            if *x == j + c { shift(c, s) } else { t.clone() }
        } else {
            panic!()
        }
    }, t)
}

pub fn eval(t: &Rc<Term>) -> Rc<Term> {
    match eval_once(t).as_ref() {
        Some(r) => eval(r),
        None => t.clone()
    }
}

fn eval_once(t: &Rc<Term>) -> Option<Rc<Term>> {
    match t.as_ref() {
        // Lambda calculus
        Term::App(v1, v2) if v1.is_val() && v2.is_val() =>
            if let Term::Abs(_, t12) = v1.as_ref() {
                Some(subst_top(v2, t12))
            } else { None }
        Term::App(v1, t2) if v1.is_val() =>
            Some(rc(Term::App(v1.clone(), eval_once(t2)?))),
        Term::App(t1, t2) =>
            Some(rc(Term::App(eval_once(t1)?, t2.clone()))),
        // Nat
        Term::Succ(t1) => Some(rc(Term::Succ(eval_once(t1)?))),
        Term::Pred(t1) =>
            match t1.as_ref() {
                Term::Zero => Some(t1.clone()),
                Term::Succ(nv) if nv.is_numeric() => Some(nv.clone()),
                _ => Some(rc(Term::Pred(eval_once(t1)?)))
            }
        Term::IsZero(t1) =>
            match t1.as_ref() {
                Term::Zero => Some(rc(Term::True)),
                Term::Succ(nv) if nv.is_numeric() => Some(rc(Term::False)),
                _ => Some(rc(Term::IsZero(eval_once(t1)?)))
            }
        // Conditional
        Term::If(t1, t2, t3) =>
            match t1.as_ref() {
                Term::True => Some(t2.clone()),
                Term::False => Some(t3.clone()),
                _ => Some(rc(Term::If(eval_once(t1)?, t2.clone(), t3.clone())))
            }
        // Ascription
        Term::Ascribe(v1, _) if v1.is_val() => Some(v1.clone()),
        Term::Ascribe(t1, type_t1) =>
            Some(rc(Term::Ascribe(eval_once(t1)?, type_t1.clone()))),
        // Let Binding
        Term::Let(v1, t2) if v1.is_val() => Some(subst_top(v1, t2)),
        Term::Let(t1, t2) =>
            Some(rc(Term::Let(eval_once(t1)?, t2.clone()))),
        // Record
        Term::Record(fields) =>
            Some(rc(Term::Record(
                map_any(fields, |(l, t)| {
                    if !t.is_val() { Some((l.clone(), eval_once(t)?)) } else { None }
                })?
            ))),
        Term::Proj(v1, l) if v1.is_val() =>
            if let Term::Record(fields) = v1.as_ref() {
                assoc(fields, l).map(|ti| ti.clone())
            } else { None }
        Term::Proj(t1, l) =>
            Some(rc(Term::Proj(eval_once(t1)?, l.clone()))),
        // Variant
        Term::Tag(l, t1, type_t) =>
            Some(rc(Term::Tag(l.clone(), eval_once(t1)?, type_t.clone()))),
        Term::Case(v1, cases) if v1.is_val() =>
            if let Term::Tag(l, v11, _) = v1.as_ref() {
                assoc(cases, l).map(|ti| subst_top(v11, ti))
            } else { None }
        Term::Case(t1, cases) =>
            Some(rc(Term::Case(eval_once(t1)?, cases.clone()))),
        // General Recursion
        Term::Fix(v1) if v1.is_val() =>
            if let Term::Abs(_, t12) = v1.as_ref() {
                Some(subst_top(t, t12))
            } else { None }
        Term::Fix(t1) => Some(rc(Term::Fix(eval_once(t1)?))),
        _ => None
    }
}

#[test]
fn test() {
}