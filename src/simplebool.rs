pub use crate::util::*;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Type {
    Bool,
    Arr(Rc<Type>, Rc<Type>)
}

#[derive(Eq, PartialEq, Debug)]
pub enum Term {
    True,
    False,
    Var(usize),
    If(Rc<Term>, Rc<Term>, Rc<Term>),
    Abs(Type, Rc<Term>),
    App(Rc<Term>, Rc<Term>)
}

pub fn type_of(t: &Rc<Term>, ctx: &Vec<Type>) -> Type {
    match t.as_ref() {
        Term::True | Term::False => Type::Bool,
        Term::Var(i) => ctx.get(*i)
            .expect("cannot find type assumption for free variable").clone(),
        Term::If(guard, true_arm, false_arm) => {
            if type_of(guard, ctx) != Type::Bool {
                panic!("guard of conditional is not a boolean")
            }
            let type_true = type_of(true_arm, ctx);
            if type_true != type_of(false_arm, ctx) {
                panic!("arms of conditional have different types")
            }
            type_true
        }
        Term::Abs(type_t1, t2) => {
            let mut new_ctx = (*ctx).clone();
            new_ctx.insert(0, type_t1.clone());
            let type_t2 = type_of(t2, &new_ctx);
            Type::Arr(rc(type_t1.clone()), rc(type_t2))
        }
        Term::App(t1, t2) => {
            let type_t1 = type_of(t1, ctx);
            let type_t2 = type_of(t2, ctx);
            match type_t1 {
                Type::Arr(type_t11, type_t12) =>
                    if *type_t11 == type_t2 {
                        (*type_t12).clone()
                    } else {
                        panic!("parameter type mismatch")
                    }
                _ => panic!("not an arrow type")
            }
        }
    }
}

#[test]
fn test() {
    // (if true then lambda. lambda. 1 else lambda. lambda. 0) true
    let input =
        rc(Term::App(
            rc(Term::If(
                rc(Term::True),
                rc(Term::Abs(
                    Type::Bool,
                    rc(Term::Abs(
                        Type::Bool,
                        rc(Term::Var(1))
                    ))
                )),
                rc(Term::Abs(
                    Type::Bool,
                    rc(Term::Abs(
                        Type::Bool,
                        rc(Term::Var(0))
                    ))
                ))
            )),
            rc(Term::True)
        ));
    println!("{:?}", input);
    let type_input = type_of(&input, &vec![]);
    println!("{:?}", type_input) // expect: Bool -> Bool
}