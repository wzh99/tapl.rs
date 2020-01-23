pub use std::rc::Rc;

#[inline]
pub fn rc<T>(term: T) -> Rc<T> {
    Rc::new(term)
}