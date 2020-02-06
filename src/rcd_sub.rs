pub use crate::util::*;
use std::ops::Deref;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Type {
    Top,
    Record(Vec<(String, Rc<Type>)>),
    Arr(Rc<Type>, Rc<Type>)
}

impl Type {
    fn is_subtype_of(&self, sup: &Type) -> bool {
        self == sup || match (self, sup) {
            // S-Rcd
            (Type::Record(field_sub), Type::Record(field_sup)) =>
                field_sup.iter().all(|(li, sup_i)| {
                    match assoc(field_sub, li) {
                        Some(sub_j) => sub_j.is_subtype_of(sup_i),
                        None => false
                    }
                }
            ),
            // S-Top
            (_, Type::Top) => true,
            // S-Arrow
            (Type::Arr(sub_param, sub_res), Type::Arr(sup_param, sup_res)) =>
                sup_param.is_subtype_of(sub_param) && sub_res.is_subtype_of(sup_res),
            _ => false
        }
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Var(isize),
    Abs(Rc<Type>, Rc<Term>),
    App(Rc<Term>, Rc<Term>),
    Record(Vec<(String, Rc<Term>)>),
    Proj(Rc<Term>, String)
}

impl Term {
    fn is_val(&self) -> bool {
        match self {
            Term::Abs(_, _) => true,
            Term::Record(fields) => fields.iter().all(|(_, ti)| ti.is_val()),
            _ => false
        }
    }
}

pub type TypeResult = Result<Rc<Type>, String>;

macro_rules! err {
    ($msg: expr) => { Err(String::from($msg)) };
}

pub fn type_of(t: &Rc<Term>, ctx: &Vec<Rc<Type>>) -> TypeResult {
    match t.deref() {
        // T-Var
        Term::Var(i) => match ctx.get(*i as usize) {
            Some(tp) => Ok(tp.clone()),
            None => err!("cannot find type of variable in current context")
        }
        // T-Abs
        Term::Abs(type_param, expr) => {
            let type_t1 = type_of(expr, &cons(type_param.clone(), ctx))?;
            Ok(rc(Type::Arr(type_param.clone(), type_t1)))
        }
        // TA-App
        Term::App(abs, arg) =>
            if let Type::Arr(type_param, type_ret) = &*type_of(abs, ctx)? {
                if type_of(arg, ctx)?.is_subtype_of(type_param) {
                    Ok(type_ret.clone())
                } else { err!("parameter type mismatch") }
            } else { err!("arrow type expected") }
        // T-Rcd
        Term::Record(fields) => {
            let type_field = try_map(fields, |(l, ti)| {
                type_of(ti, ctx).map(|ty| (l.clone(), ty))
            })?;
            Ok(rc(Type::Record(type_field)))
        }
        // T-Proj
        Term::Proj(t1, l) => if let Type::Record(fields) = &*type_of(t1, ctx)? {
            match assoc(&fields, l) {
                Some(type_ti) => Ok(type_ti.clone()),
                None => err!(format!("label {} not found", l))
            }
        } else { err!("expect record type") }
    }
}

fn map_var<F>(f: &F, t: &Rc<Term>) -> Rc<Term> where
    F: Fn(&Rc<Term>, isize) -> Rc<Term> {
    walk(f, t, 0)
}

fn walk<F>(f: &F, t: &Rc<Term>, c: isize) -> Rc<Term> where
    F: Fn(&Rc<Term>, isize) -> Rc<Term> {
    match t.deref() {
        Term::Var(_) => f(t, c),
        Term::Abs(param_type, t1) =>
            rc(Term::Abs(param_type.clone(), walk(f, t1, c + 1))),
        Term::App(t1, t2) =>
            rc(Term::App(walk(f, t1, c), walk(f, t2, c))),
        Term::Record(fields) => rc(Term::Record(
            fields.iter().map(|(l, ti)| (l.clone(), walk(f, ti, c))).collect()
        )),
        Term::Proj(t1, l) => rc(Term::Proj(walk(f, t1, c), l.clone())),
    }
}

fn shift(d: isize, t: &Rc<Term>) -> Rc<Term> {
    map_var(&|t, c| if let Term::Var(x) = t.deref() {
        if *x >= c { rc(Term::Var(*x + d)) } else { t.clone() }
    } else {
        panic!()
    }, t)
}

fn subst_top(s: &Rc<Term>, t: &Rc<Term>) -> Rc<Term> {
    shift(-1, &subst(0, &shift(1, s), t))
}

fn subst(j: isize, s: &Rc<Term>, t: &Rc<Term>) -> Rc<Term> {
    map_var(&|t, c| if let Term::Var(x) = t.deref() {
        if *x == j + c { shift(c, s) } else { t.clone() }
    } else {
        panic!()
    }, t)
}

pub fn eval(t: &Rc<Term>) -> Rc<Term> {
    match eval_once(t) {
        Some(r) => eval(&r),
        None => t.clone()
    }
}

fn eval_once(t: &Rc<Term>) -> Option<Rc<Term>> {
    match t.deref() {
        // Lambda calculus
        Term::App(v1, v2) if v1.is_val() && v2.is_val() =>
            if let Term::Abs(_, expr) = v1.deref() {
                Some(subst_top(v2, expr))
            } else { None }
        Term::App(v1, t2) if v1.is_val() =>
            Some(rc(Term::App(v1.clone(), eval_once(t2)?))),
        Term::App(t1, t2) => Some(rc(Term::App(eval_once(t1)?, t2.clone()))),
        // Record
        Term::Record(fields) => Some(rc(Term::Record(
            map_any(fields, |(l, t)| {
                if !t.is_val() { Some((l.clone(), eval_once(t)?)) } else { None }
            })?
        ))),
        Term::Proj(v1, l) if v1.is_val() => if let Term::Record(fields) = v1.deref() {
            assoc(fields, l).map(|ti| ti.clone())
        } else { None }
        Term::Proj(t1, l) => Some(rc(Term::Proj(eval_once(t1)?, l.clone()))),
        _ => None
    }
}
