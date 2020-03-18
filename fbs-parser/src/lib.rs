mod lexer;

#[doc(inline)]
pub use lexer::{
    Attribute, Enum, Error, FileExtension, FileIdentifier, Ident, Include, Namespace, Root, Schema,
    Struct, Table, Type, Union, parse
};
