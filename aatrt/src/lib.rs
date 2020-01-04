#[no_mangle]
pub extern "C" fn exit(code: i32) {
    std::process::exit(code);
}
