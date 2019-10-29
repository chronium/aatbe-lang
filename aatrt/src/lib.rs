#[no_mangle]
pub extern fn exit(code: i32) {
  std::process::exit(code);
}
