fn main() {
    // println!("cargo:rustc-cfg=lang_benchmark");
    // println!("cargo:rustc-cfg=lang_use_vm");
    println!("cargo:rustc-cfg=lang_panic_on_error");
    // println!("cargo:rustc-cfg=lang_ignore_parse_errors");
    println!("cargo:rustc-cfg=lang_print_lexer_output");
    println!("cargo:rustc-cfg=lang_print_parser_output");
    println!("cargo:rustc-cfg=lang_print_interpreter");
    println!("cargo:rustc-cfg=lang_print_vm_compiler");
    println!("cargo:rustc-cfg=lang_print_vm_interpreter");
    println!("cargo:rustc-cfg=lang_test_interpreter");
    // println!("cargo:rustc-cfg=lang_test_vm_compiler");
    // println!("cargo:rustc-cfg=lang_test_vm_interpreter");
}
