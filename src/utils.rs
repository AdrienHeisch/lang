pub fn compare_floats (f1:f32, f2:f32, error:f32) -> bool
{
    (f1 - f2).abs() < error
}