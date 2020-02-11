pub mod atom;
pub mod expr;
pub mod mangle_v1;
pub mod module;
pub mod scope;
pub mod unit;

pub use expr::*;
pub use module::{AatbeModule, ValueTypePair};
pub use scope::Scope;
pub use unit::CodegenUnit;

pub type GenRes = Result<ValueTypePair, CompileError>;

pub enum CompileError {
    ExpectedType {
        expected_ty: String,
        found_ty: String,
        value: String,
    },
    MismatchedArguments {
        function: String,
        expected_ty: String,
        found_ty: String,
    },
    UnaryMismatch {
        op: String,
        expected_ty: String,
        found_ty: String,
        value: String,
    },
    BinaryMismatch {
        op: String,
        types: (String, String),
        values: (String, String),
    },
    OpMismatch {
        op: String,
        types: (String, String),
        values: (String, String),
    },
    AssignMismatch {
        expected_ty: String,
        found_ty: String,
        value: String,
        var: String,
    },
    StoreMismatch {
        expected_ty: String,
        found_ty: String,
        value: String,
        lval: String,
    },
    NotIndexable {
        ty: String,
        lval: String,
    },
    Handled,
}
