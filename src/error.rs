#[derive(Debug)]
pub struct Error //TODO error handling
{
    pub msg: String,
    pub from: usize,
    pub to: usize
}