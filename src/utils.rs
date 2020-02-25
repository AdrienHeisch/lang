pub fn compare_floats (f1:f32, f2:f32, error:f32) -> bool
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