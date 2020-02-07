pub use std::rc::Rc;

/// Abbreviation for Rc::new
#[inline]
pub fn rc<T>(term: T) -> Rc<T> { Rc::new(term) }

/// Put an element to the front of a vector.
#[inline]
pub fn cons<T>(elem: T, v: &Vec<T>) -> Vec<T> where T: Clone {
    let mut new_vec = (*v).clone();
    new_vec.insert(0, elem);
    new_vec
}

/// Find associated value of a given key in a `Vec<K, V>`
#[inline]
pub fn assoc<'a, K, V>(vec: &'a Vec<(K, V)>, key: &K) -> Option<&'a V> where
    K: PartialEq {
    vec.iter().find(|(k, _)| k == key).map(|(_, v) | v)
}

/// Append elements in the second `Vec` to the first one
#[inline]
pub fn append<T>(v1: &Vec<T>, v2: &Vec<T>) -> Vec<T> where T: Clone {
    let mut res = v1.clone();
    res.append(&mut v2.clone());
    res
}

/// Try to map all elements in a `Vec` to another, possibly of different types.
/// Return `Err` if some error occurred during mapping.
#[inline]
pub fn try_map<T, R, E, F>(v: &Vec<T>, f: F) -> Result<Vec<R>, E> where
    F: Fn(&T) -> Result<R, E> {
    let mut res = Vec::with_capacity(v.len());
    for x in v { res.push(f(x)?) }
    Ok(res)
}

/// Possibly map elements in a `Vec` to another of the same type.
/// Return `None` if none of the elements are actually mapped
#[inline]
pub fn map_any<T, F>(v: &Vec<T>, f: F) -> Option<Vec<T>> where
    T: Clone, F: Fn(&T) -> Option<T> {
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
