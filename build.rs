fn main ()
{
    println!("cargo:rustc-cfg=lang_benchmark");
    // println!("cargo:rustc-cfg=lang_debug_info"); //(in compiler only)
    // println!("cargo:rustc-cfg=lang_panic_on_error");
    // println!("cargo:rustc-cfg=lang_ignore_parse_errors");
}