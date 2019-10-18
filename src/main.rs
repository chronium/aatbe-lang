#![feature(box_syntax)]

mod parser;

use parser::aatbe_parser;

fn main() {
  println!(
    "{:#?}",
    aatbe_parser::expr(
      "
{
    fn puts str -> i32
    fn printf (*u8, ...) -> i32

    fn swap (x: any, y: any) -> (any, any)
        = (y, x)

    swap(a, b)
    printf(\"Hello %s\n\", \"World!\")
}
        "
    )
  );
}
