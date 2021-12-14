#![allow(dead_code)]

pub fn compare_f64 (f1:f64, f2:f64, error:f64) -> bool
{
    (f1 - f2).abs() < error
}

pub fn slice_to_string<T> (slice:&[T]) -> String where T:std::fmt::Display
{
    let mut s = String::from("[");
    let l = slice.len();
    for (index, item) in slice.iter().enumerate() {
        if index < l - 1 {
            s = format!("{}{}, ", s, item);
        } else {
            s = format!("{}{}", s, item);
        }
    }
    format!("{}]", s)
}

pub fn slice_to_string_debug<T> (slice:&[T]) -> String where T:std::fmt::Debug
{
    let mut s = String::from("[");
    let l = slice.len();
    for (index, item) in slice.iter().enumerate() {
        if index < l - 1 {
            s = format!("{}{:?}, ", s, item);
        } else {
            s = format!("{}{:?}", s, item);
        }
    }
    format!("{}]", s)
}