pub use crate::util::*;
use std::ops::Deref;

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
    // Boolean
    True,
    False,
    // Natural Number
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
    match t.deref() {
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
        Term::Tag(l, t1, type_tag) =>
            rc(Term::Tag(l.clone(), walk(f, t1, c), type_tag.clone())),
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
        if let Term::Var(x) = t.deref() {
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
        if let Term::Var(x) = t.deref() {
            if *x == j + c { shift(c, s) } else { t.clone() }
        } else {
            panic!()
        }
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
        // Natural Number
        Term::Succ(t1) => Some(rc(Term::Succ(eval_once(t1)?))),
        Term::Pred(t1) => match t1.deref() {
            Term::Zero => Some(t1.clone()),
            Term::Succ(nv) if nv.is_numeric() => Some(nv.clone()),
            _ => Some(rc(Term::Pred(eval_once(t1)?)))
        }
        Term::IsZero(t1) => match t1.deref() {
            Term::Zero => Some(rc(Term::True)),
            Term::Succ(nv) if nv.is_numeric() => Some(rc(Term::False)),
            _ => Some(rc(Term::IsZero(eval_once(t1)?)))
        }
        // Conditional
        Term::If(t1, t2, t3) => match t1.deref() {
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
        Term::Let(t1, t2) => Some(rc(Term::Let(eval_once(t1)?, t2.clone()))),
        // Record
        Term::Record(fields) => Some(rc(Term::Record(
            map_any(fields, |(l, t)| {
                if !t.is_val() { Some((l.clone(), eval_once(t)?)) } else { None }
            })?
        ))),
        Term::Proj(v1, l) if v1.is_val() =>
            if let Term::Record(fields) = v1.deref() {
                assoc(fields, l).map(|ti| ti.clone())
            } else { None }
        Term::Proj(t1, l) => Some(rc(Term::Proj(eval_once(t1)?, l.clone()))),
        // Variant
        Term::Tag(l, t1, type_t) =>
            Some(rc(Term::Tag(l.clone(), eval_once(t1)?, type_t.clone()))),
        Term::Case(v1, cases) if v1.is_val() =>
            if let Term::Tag(l, v11, _) = v1.deref() {
                assoc(cases, l).map(|ti| subst_top(v11, ti))
            } else { None }
        Term::Case(t1, cases) => Some(rc(Term::Case(eval_once(t1)?, cases.clone()))),
        // General Recursion
        Term::Fix(v1) if v1.is_val() =>
            if let Term::Abs(_, t12) = v1.deref() {
                Some(subst_top(t, t12))
            } else { None }
        Term::Fix(t1) => Some(rc(Term::Fix(eval_once(t1)?))),
        _ => None
    }
}

type TypeResult = Result<Rc<Type>, String>;

macro_rules! err {
    () => { Err(String::from("")) };
    ($msg: expr) => { Err(String::from($msg)) };
}

pub fn type_of(t: &Rc<Term>, ctx: &Vec<Rc<Type>>) -> TypeResult {
    match t.deref() {
        // Lambda Calculus
        Term::Var(i) => match ctx.get(*i as usize) {
            Some(tp) => Ok(tp.clone()),
            None => err!("cannot find type of variable in current context")
        }
        Term::Abs(type_param, expr) => {
            let type_t1 = type_of(expr, &cons(type_param.clone(), ctx))?;
            Ok(rc(Type::Arr(type_param.clone(), type_t1)))
        }
        Term::App(abs, arg) =>
            if let Type::Arr(type_param, type_ret) = &*type_of(abs, ctx)? {
                if type_param == &type_of(arg, ctx)? {
                    Ok(type_ret.clone())
                } else { err!("parameter type mismatch") }
            } else { err!("not an arrow type") }
        // Unit
        Term::Unit => Ok(rc(Type::Unit)),
        // Boolean
        Term::True | Term::False => Ok(rc(Type::Bool)),
        // Natural Number
        Term::Zero => Ok(rc(Type::Nat)),
        Term::IsZero(t1) => if let Type::Nat = *type_of(t1, ctx)? {
            Ok(rc(Type::Bool))
        } else { err!("argument of is_zero is not a number") }
        Term::Pred(t1) => if let Type::Nat = *type_of(t1, ctx)? {
            Ok(rc(Type::Nat))
        } else { err!("argument of pred is not a number") }
        Term::Succ(t1) => if let Type::Nat = *type_of(t1, ctx)? {
            Ok(rc(Type::Nat))
        } else { err!("argument of succ is not a number") }
        // String
        Term::String(_) => Ok(rc(Type::String)),
        // Conditional
        Term::If(guard, true_arm, false_arm) => {
            if let Type::Bool = *type_of(guard, ctx)? {
                let type_true = type_of(true_arm, ctx)?;
                if type_true == type_of(false_arm, ctx)? {
                    Ok(type_true)
                } else { err!("arms of conditional have different types") }
            } else { err!("guard of conditional is not a boolean") }
        }
        // Ascription
        Term::Ascribe(t1, type_as) =>
            if &type_of(t1, ctx)? == type_as {
                Ok(type_as.clone())
            } else { err!("body of as term does not have the expected type") }
        // Let Binding
        Term::Let(t1, t2) => {
            let type_t1 = type_of(t1, ctx)?;
            let new_ctx = cons(type_t1, ctx);
            type_of(t2, &new_ctx)
        },
        // Record
        Term::Record(fields) => {
            let type_field = try_map(fields, |(l, ti)| {
                type_of(ti, ctx).map(|ty| (l.clone(), ty))
            })?;
            Ok(rc(Type::Record(type_field)))
        }
        Term::Proj(t1, l) =>
            if let Type::Record(fields) = &*type_of(t1, ctx)? {
                match assoc(&fields, l) {
                    Some(type_ti) => Ok(type_ti.clone()),
                    None => err!(format!("label {} not found", l))
                }
            } else { err!("expect record type") }
        // Variant
        Term::Tag(l, t1, type_tag) =>
            if let Type::Variant(fields) = type_tag.deref() {
                let type_expect = assoc(fields, l)
                    .ok_or(format!("label {} not found", l))?;
                let type_t1 = type_of(t1, ctx)?;
                if type_expect.deref() == type_t1.deref() {
                    Ok(type_tag.clone())
                } else { err!("field does not have expected type") }
            } else { err!("annotation is not variant type") }
        Term::Case(t1, cases) =>
            if let Type::Variant(type_cases) = &*type_of(t1, ctx)? {
                let type_cases = try_map(cases, |(l, ti)| {
                    match assoc(type_cases, l) {
                        Some(type_case) => type_of(ti, &cons(type_case.clone(), ctx)),
                        None => err!(format!("label {} not in type", l))
                    }
                })?;
                let type_head = type_cases.first()
                    .ok_or("cannot use case on an empty variant")?;
                if type_cases.iter().all(|type_ti| type_ti == type_head) {
                    Ok(type_head.clone())
                } else { err!("cases do not have the same type") }
            } else { err!("expect variant type") }
        // General Recursion
        Term::Fix(t1) =>
            if let Type::Arr(type_param, type_res) = &*type_of(t1, ctx)? {
                if type_param == type_res {
                    Ok(type_param.clone())
                } else { err!("result type is not compatible with parameter type") }
            } else { err!("arrow type expected") }
    }
}

#[test]
fn test() {
    fn gen_nat(x: usize) -> Rc<Term> {
        fn gen_rec(t: Rc<Term>, y: usize) -> Rc<Term> {
            if y == 0 { return t } else { gen_rec(rc(Term::Succ(t)), y - 1) }
        }
        gen_rec(rc(Term::Zero), x)
    }

    let test_cases = vec![
        /// Case 0
        /// {number=0, truth=is_zero(succ(0))}.truth
        /// Type: Bool
        /// Result: false
        rc(Term::Proj(
            rc(Term::Record(vec![ // {
                ("number".to_string(), rc(Term::Zero)), // number=0
                ("truth".to_string(), rc(Term::IsZero( // truth=is_zero(
                    rc(Term::Succ( // succ(
                        rc(Term::Zero) // 0
                    )) // )
                ))) // )
            ])), // }
            "truth".to_string() // .truth
        )),

        /// Case 1
        /// case <number=0> as <truth: Bool, number: Nat> {
        ///     <truth=x> => x,
        ///     <number=y> => is_zero(y)
        /// }
        /// Type: Bool
        /// Result: true
        rc(Term::Case( // case
            rc(Term::Tag( // <
                "number".to_string(), // number=
                rc(Term::Zero), // 0>
                rc(Type::Variant(vec![ // <
                    ("truth".to_string(), rc(Type::Bool)), // truth: Bool
                    ("number".to_string(), rc(Type::Nat)) // number: Nat
                ])) // >
            )), // {
            vec![
                ("truth".to_string(), rc(Term::Var(0))), // <truth=x> => x
                ("number".to_string(), rc(Term::IsZero( // <number=y> => is_zero(
                    rc(Term::Var(0)) // y
                ))) // )
            ]
        )), // }

        /// Case 2
        /// let is_even = fix lambda ie: Nat -> Bool.
        ///     lambda x: Nat.
        ///         if is_zero(x)
        ///         then true
        ///         else if is_zero(pred(x))
        ///         then false
        ///         else ie(pred(pred(x)));
        /// is_even(3)
        /// Type: Bool
        /// Result: false
        rc(Term::Let( // let is_even =
            rc(Term::Fix( // fix
                rc(Term::Abs( // lambda ie:
                    rc(Type::Arr(
                        rc(Type::Nat), // Nat ->
                        rc(Type::Bool) // Bool
                    )),
                    rc(Term::Abs( // lambda x:
                        rc(Type::Nat), // Nat
                        rc(Term::If( // if
                            rc(Term::IsZero( // is_zero(
                                rc(Term::Var(0)) // x
                            )), // )
                            rc(Term::True), // then true
                            rc(Term::If( // else if
                                rc(Term::IsZero( // is_zero(
                                    rc(Term::Pred( // pred(
                                        rc(Term::Var(0)) // x
                                    )) // )
                                )), // )
                                rc(Term::False), // then false
                                rc(Term::App( // else
                                    rc(Term::Var(1)), // ie(
                                    rc(Term::Pred( // pred(
                                        rc(Term::Pred( // pred(
                                            rc(Term::Var(0)) // x
                                        )) // )
                                    )) // )
                                )) // )
                            )) // end if
                        )) // end if
                    )) // end lambda
                ))// end lambda
            )), // end fix
            rc(Term::App(
                rc(Term::Var(0)), // is_even(
                gen_nat(3) // 3 // or succ(succ(succ(0)))
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
