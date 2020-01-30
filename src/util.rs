pub use std::rc::Rc;

/// Shortcut for Rc::new
#[inline]
pub fn rc<T>(term: T) -> Rc<T> {
    Rc::new(term)
}

/// In discipline, no mutable variables should appear in a functional programming paradigm.
/// However, the insert method on Vec<T> only accept mutable references. So this function is
/// used to hide this mutable variable and make the whole program looks functional in general.
#[inline]
pub fn cons<T: Clone>(elem: T, v: &Vec<T>) -> Vec<T> {
    let mut new_vec = (*v).clone();
    new_vec.insert(0, elem);
    new_vec
}
