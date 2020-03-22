mod lexer;

#[doc(inline)]
pub use lexer::{
    ident, parse_path, Attribute, Enum, EnumValue, Error, FileExtension, FileIdentifier, Ident,
    Include, Namespace, Primitive, RootType, Schema, SchemaType, Struct, Table, Type, Union,
    UnionValue,
};
