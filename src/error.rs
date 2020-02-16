#[derive(Debug)]
pub struct Error
{
    pub msg: String,
    pub from: usize,
    pub to: usize
}