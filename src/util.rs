pub use std::rc::Rc;

/// Shortcut for Rc::new
#[inline]
pub fn rc<T>(term: T) -> Rc<T> {
    Rc::new(term)
}

/// Put an element to the front of a list.
#[inline]
pub fn cons<T>(elem: T, v: &Vec<T>) -> Vec<T> where T: Clone {
    let mut new_vec = (*v).clone();
    new_vec.insert(0, elem);
    new_vec
}

/// Possibly map elements in a `Vec` to another. Return None if none of the elements are
/// actually mapped
#[inline]
pub fn map_any<T, F>(v: &Vec<T>, mut f: F) -> Option<Vec<T>> where
    T: Clone, F: FnMut(&T) -> Option<T> {
    let mut mapped = false;
    let new_map = v.iter()
        .map(|elem| {
            match f(elem) {
                Some(r) => { mapped = true; r }
                None => elem.clone()
            }
        }).collect();
    if mapped { Some(new_map) } else { None }
}

/// Find associated value of a given key in a `Vec<K, V>`
#[inline]
pub fn assoc<'a, K, V>(vec: &'a Vec<(K, V)>, key: &K) -> Option<&'a V> where
    K: PartialEq {
    vec.iter().find(|(k, _)| k == key).map(|(_, v) | v)
}