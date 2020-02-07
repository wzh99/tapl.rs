pub use crate::util::*;
use std::ops::Deref;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Type {
    Top,
    Bool,
    Record(Vec<(String, Rc<Type>)>),
    Arr(Rc<Type>, Rc<Type>)
}

impl Type {
    fn is_subtype_of(&self, sup: &Type) -> bool {
        // S-Refl
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
    True,
    False,
    If(Rc<Term>, Rc<Term>, Rc<Term>),
    Record(Vec<(String, Rc<Term>)>),
    Proj(Rc<Term>, String)
}

impl Term {
    fn is_val(&self) -> bool {
        match self {
            Term::Abs(_, _) => true,
            Term::True | Term::False => true,
            Term::Record(fields) => fields.iter().all(|(_, ti)| ti.is_val()),
            _ => false
        }
    }
}

fn join(type_s: &Rc<Type>, type_t: &Rc<Type>) -> Rc<Type> {
    if type_s == type_t { type_s.clone() } else {
        match (type_s.deref(), type_t.deref()) {
            (Type::Arr(param_s, res_s), Type::Arr(param_t, res_t)) =>
                match meet(param_s, param_t) {
                    Some(param) => rc(Type::Arr(param, join(res_s, res_t))),
                    None => rc(Type::Top)
                }
            (Type::Record(field_s), Type::Record(field_t)) => {
                let common_field = field_s.iter().filter_map(|(li, type_i)| {
                    Some((li.clone(), join(type_i, assoc(field_t, li)?)))
                });
                rc(Type::Record(common_field.collect()))
            }
            _ => rc(Type::Top)
        }
    }
}

fn meet(type_s: &Rc<Type>, type_t: &Rc<Type>) -> Option<Rc<Type>> {
    if type_s == type_t { Some(type_s.clone()) } else {
        match (type_s.deref(), type_t.deref()) {
            (Type::Arr(param_s, res_s), Type::Arr(param_t, res_t)) =>
                Some(rc(Type::Arr(join(param_s, param_t), meet(res_s, res_t)?))),
            (Type::Record(field_s), Type::Record(field_t)) => {
                let common_labels: Vec<&String> = field_s.iter().filter_map(|(li, _)| {
                    assoc(field_t, li).map(|_| li)
                }).collect();
                let all_labels = append(
                    &field_s.iter().map(|(li, _)| li).collect(),
                    &field_t.iter().filter_map(|(li, _)| {
                        match assoc(field_s, li) {
                            Some(_) => None,
                            None => Some(li)
                        }
                    }).collect()
                );
                let all_fields = try_map(&all_labels, |li| {
                    if common_labels.iter().find(|lj| *li == **lj).is_some() {
                        Ok(((*li).clone(), meet(assoc(field_s, li).unwrap(),
                                                assoc(field_t, li).unwrap())?
                        ))
                    } else {
                        match assoc(field_s, li) {
                            Some(type_i) => Ok(((*li).clone(), type_i.clone())),
                            None => Ok(((*li).clone(), assoc(field_t, li).unwrap().clone()))
                        }
                    }
                })?;
                Some(rc(Type::Record(all_fields)))
            }
            _ => None
        }
    }
}

pub type TypeResult = Result<Rc<Type>, String>;

macro_rules! err {
    ($msg: expr) => { Err(String::from($msg)) };
}

pub fn type_of(t: &Rc<Term>, ctx: &Vec<Rc<Type>>) -> TypeResult {
    match t.deref() {
        // Lambda Calculus
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

        // Boolean
        // T-True, T-False
        Term::True | Term::False => Ok(rc(Type::Bool)),

        // Conditional
        // TA-If
        Term::If(guard, true_arm, false_arm) =>
            if let Type::Bool = type_of(guard, ctx)?.deref() {
                Ok(join(&type_of(true_arm, ctx)?, &type_of(false_arm, ctx)?))
            } else { err!("guard of conditional is not a boolean") }

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
        // Lambda Calculus
        Term::Var(_) => f(t, c),
        Term::Abs(param_type, t1) =>
            rc(Term::Abs(param_type.clone(), walk(f, t1, c + 1))),
        Term::App(t1, t2) =>
            rc(Term::App(walk(f, t1, c), walk(f, t2, c))),

        // Conditional
        Term::If(t1, t2, t3) =>
            rc(Term::If(walk(f, t1, c), walk(f, t2, c), walk(f, t3, c))),

        // Record
        Term::Record(fields) => rc(Term::Record(
            fields.iter().map(|(l, ti)| (l.clone(), walk(f, ti, c))).collect()
        )),
        Term::Proj(t1, l) => rc(Term::Proj(walk(f, t1, c), l.clone())),

        _ => t.clone()
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
        // E-AppAbs
        Term::App(v1, v2) if v1.is_val() && v2.is_val() =>
            if let Term::Abs(_, expr) = v1.deref() {
                Some(subst_top(v2, expr))
            } else { None }
        // E-App2
        Term::App(v1, t2) if v1.is_val() =>
            Some(rc(Term::App(v1.clone(), eval_once(t2)?))),
        // E-App1
        Term::App(t1, t2) => Some(rc(Term::App(eval_once(t1)?, t2.clone()))),

        // Conditional
        Term::If(t1, t2, t3) => match t1.deref() {
            // E-IfTrue
            Term::True => Some(t2.clone()),
            // E-IfFalse
            Term::False => Some(t3.clone()),
            // E-If
            _ => Some(rc(Term::If(eval_once(t1)?, t2.clone(), t3.clone())))
        }

        // Record
        // E-Rcd
        Term::Record(fields) => Some(rc(Term::Record(
            map_any(fields, |(l, t)| {
                if !t.is_val() { Some((l.clone(), eval_once(t)?)) } else { None }
            })?
        ))),
        // E-ProjRcd
        Term::Proj(v1, l) if v1.is_val() => if let Term::Record(fields) = v1.deref() {
            assoc(fields, l).map(|ti| ti.clone())
        } else { None }
        // E-Proj
        Term::Proj(t1, l) => Some(rc(Term::Proj(eval_once(t1)?, l.clone()))),

        _ => None
    }
}

#[test]
fn test() {
    let test_cases = vec![
        /// Case 0
        /// ((lambda x: {a: {}, b: Bool}. {c=x.b, d=true}) {a={e=false}, b=false, f={}}).c
        /// Type: Bool
        /// Result: false
        rc(Term::Proj( // (
            rc(Term::App( // (
                rc(Term::Abs( // lambda x:
                    rc(Type::Record( // {
                        vec![
                            ("a".to_string(), rc(Type::Record(vec![]))), // a:{}
                            ("b".to_string(), rc(Type::Bool)) // b: Bool
                        ]
                    )), // }.
                    rc(Term::Record( // {
                        vec![
                            ("c".to_string(), rc(Term::Proj( // c=
                                rc(Term::Var(0)), // x.
                                "b".to_string() // b
                            ))),
                            ("d".to_string(), rc(Term::True)) // d=true
                        ]
                    ))
                )),
                rc(Term::Record( // {
                    vec![
                        ("a".to_string(), rc(Term::Record( // a={
                            vec![
                                ("e".to_string(), rc(Term::False)) // e=false
                            ]
                        ))), // },
                        ("b".to_string(), rc(Term::False)), // b=false,
                        ("f".to_string(), rc(Term::Record(vec![]))) // f={}
                    ]
                )) // }
            )),
            "c".to_string() // .c
        )),

        /// Case 1
        /// if true then lambda x: {a: {}, b: Bool}. {c=true, d={}}
        ///         else lambda y: {e: Bool, a: {}}. {c=true, f={}}
        /// Type: {a: {}, b: Bool, e: Bool} -> {c: Bool}
        /// Result: lambda x: {a: {}, b: Bool}. {c=true, d={}}
        rc(Term::If( // if
            rc(Term::True), // true then
            rc(Term::Abs( // lambda x:
                rc(Type::Record( // {
                    vec![
                        ("a".to_string(), rc(Type::Record(vec![]))), // a: {}
                        ("b".to_string(), rc(Type::Bool)) // b: Bool
                    ]
                )), // }.
                rc(Term::Record( // {
                    vec![
                        ("c".to_string(), rc(Term::True)), // c=true,
                        ("d".to_string(), rc(Term::Record(vec![]))) // d={}
                    ]
                ))
            )),
            rc(Term::Abs( // lambda y:
                rc(Type::Record( // {
                    vec![
                        ("e".to_string(), rc(Type::Bool)), // e: Bool,
                        ("a".to_string(), rc(Type::Record(vec![]))) // a: {}
                    ]
                )), // }.
                rc(Term::Record( // {
                    vec![
                        ("c".to_string(), rc(Term::True)), // c=true
                        ("f".to_string(), rc(Term::Record(vec![]))) // f={}
                    ]
                ))
            ))
        ))
    ];

    for i in 0..test_cases.len() {
        let term = &test_cases[i];
        print!("{}\t", i);
        match type_of(term, &vec![]) {
            Ok(ty) => println!("{:?}: {:?}", eval(term), ty),
            Err(msg) => println!("{}", msg)
        }
    }
}
