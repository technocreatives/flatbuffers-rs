use indexmap::IndexMap;

peg::parser! {
    grammar fbs_parser() for str {
        rule ___() = [' ' | '\n']+
        rule __() = [' ' | '\n']*
        rule _() = [' ']+

        pub rule schema() -> SchemaItems
            = s:schema0()* { SchemaItems { items: s } }
        rule schema0() -> SchemaItem
            = ([' ' | '\n'] / comment())* s:schema1() ([' ' | '\n'] / comment())* { s }
        rule schema1() -> SchemaItem
            = i:include() { SchemaItem::Include(i) }
            / n:namespace() { SchemaItem::Namespace(n) }
            / t:table_or_struct() { t }
            / e:enum() { SchemaItem::Enum(e) }
            / u:union() { SchemaItem::Union(u) }
            / r:root() { SchemaItem::Root(r) }
            / f:file_extension() { SchemaItem::FileExtension(f) }
            / f:file_identifier() { SchemaItem::FileIdentifier(f) }
            / a:attribute() { SchemaItem::Attribute(a) }
            // / r:rpc()
        rule comment() -> String
            = "//" s:$((!['\n'][_])*) "\n" { s.trim().to_string() }
        rule attribute() -> Attribute
            = "attribute" _ s:string_literal() __ semi_eol() { Attribute(s) }
        rule file_extension() -> FileExtension
            = "file_extension" _ s:string_literal() __ semi_eol() { FileExtension(s) }
        rule file_identifier() -> FileIdentifier
            = "file_identifier" _ s:string_literal() __ semi_eol() { FileIdentifier(s) }

        rule semi_eol()
            = ";" (__ comment() __)*

        rule table_or_struct() -> SchemaItem
            = x:$("table" / "struct") _ i:ident() " "* m:metadata()? " "* "{" __ f:field()* __ "}" {
                if x == "table" {
                    SchemaItem::Table(Table {
                        name: i,
                        metadata: m,
                        fields: f,
                    })
                } else {
                    SchemaItem::Struct(Struct {
                        name: i,
                        metadata: m,
                        fields: f,
                    })
                }
            }

        rule type_ident() -> Type
            = "bool" { Type::Bool }
            / "byte" { Type::Byte }
            / "ubyte" { Type::Ubyte }
            / "short" { Type::Short }
            / "ushort" { Type::Ushort }
            / "int" { Type::Int }
            / "uint" { Type::Uint }
            / "float" { Type::Float }
            / "long" { Type::Long }
            / "ulong" { Type::Ulong }
            / "double" { Type::Double }
            / "int8" { Type::Int8 }
            / "uint8" { Type::Uint8 }
            / "int16" { Type::Int16 }
            / "uint16" { Type::Uint16 }
            / "int32" { Type::Int32 }
            / "uint32" { Type::Uint32 }
            / "int64" { Type::Int64 }
            / "uint64" { Type::Uint64 }
            / "float32" { Type::Float32 }
            / "float64" { Type::Float64 }
            / "string" { Type::String }
            / "[" i:type_ident() "]" { Type::Vector(Box::new(i)) }
            / i:ident() { Type::Ident(i) }

        rule enum_value() -> EnumValue
            = i:ident() [' ']* "=" [' ']* v:integer() { EnumValue { name: i, value: Some(v) } }
            / i:ident() { EnumValue { name: i, value: None } }

        rule enum() -> Enum
            = "enum" _ i:ident() [' ']* ":" [' ']* t:type_ident() " "* m:metadata()? "{" __ v:enum_value() ** (__ "," __) __ ","? __ "}" {
                Enum {
                    name: i,
                    ty: t,
                    metadata: m,
                    values: v
                }
            }
        rule union_value() -> UnionValue
            = i:ident() [' ']* ":" [' ']* t:type_ident() { UnionValue { ty: t, name: Some(i) } }
            / t:type_ident() { UnionValue { ty: t, name: None } }

        rule union() -> Union
            = "union" _ i:ident() " "* m:metadata()? "{" __ v:union_value() ** (__ "," __) __ ","? __ "}" {
                Union {
                    name: i,
                    metadata: m,
                    values: v
                }
            }

        rule digit()
            = ['0'..='9']

        rule xdigit()
            = ['0'..='9' | 'a'..='f' | 'A'..='F']

        rule ident() -> Ident
            = v:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { Ident(v.to_string()) }

        rule integer() -> Integer
            = x:$(['-' | '+']? digit()+) { Integer::Decimal(x.to_string()) }
            / x:$(['-' | '+']?"0"['x' | 'X'] xdigit()+) { Integer::Hex(x.to_string()) }

        rule float() -> Float
            = x:$(['-' | '+']? ("." digit()+ / digit()+ "." digit()* / digit()+) (['e'|'E']['-' | '+']? digit()+)?) { Float::Decimal(x.to_string()) }
            / x:$(['-' | '+']? "0"['x'|'X'] ("." xdigit()+ / xdigit()+ "." xdigit()* / xdigit()+) (['p'|'P']['-' | '+']? digit()+)?) { Float::Hex(x.to_string()) }
            / x:$(['-' | '+']? ("nan" / "inf" / "infinity")) { Float::Special(x.to_string()) }
        rule bool() -> bool
            = "true" { true }
            / "false" { false }

        rule scalar() -> Scalar
            = i:integer() { Scalar::Integer(i) }
            / f:float() { Scalar::Float(f) }

        rule literal() -> Literal
            = s:scalar() { Literal::Scalar(s) }
            / s:string_literal() { Literal::String(s) }
            / b:bool() { Literal::Bool(b) }
            / i:ident() { Literal::Ident(i) }

        rule single_value() -> SingleValue
            = s:scalar() { SingleValue::Scalar(s) }
            / s:string_literal() { SingleValue::String(s) }

        rule metadata() -> Metadata
            = "(" __ o:metadata0() ** (__ "," __) __ ")" {
                let mut m = IndexMap::new();
                for (k, v) in o {
                    m.insert(k, v);
                }
                Metadata(m)
            }
        rule metadata0() -> (Ident, Option<SingleValue>)
            = i:ident() s:metadata1()? {
                (i, s)
            }
        rule metadata1() -> SingleValue
            = [' ']* ":" [' ']* s:single_value() { s }

        rule field() -> Field
            = i:ident() [' ']* ":" [' ']* t:type_ident() l:field0()? m:field1()? semi_eol() __ {
                Field {
                    name: i,
                    ty: t,
                    default_value: l,
                    metadata: m,
                }
            }
        rule field0() -> Literal
            = _ "=" _ l:literal() { l }
        rule field1() -> Metadata
            = _ m:metadata() { m }

        rule string_literal() -> String
            = "\"" s:$((!['"'][_])*) "\"" { s.to_string() }

        rule include() -> Include
            = "include" _ path:string_literal() __ semi_eol() { Include { path } }

        rule namespace() -> Namespace
            = "namespace" _ i:ident() ** "." __ semi_eol() { Namespace(i) }

        rule root() -> Root
            = "root_type" _ i:ident() __ semi_eol() { Root(i) }

        rule value() -> Value
            = s:string_literal() { Value::Single(SingleValue::String(s)) }

        rule object() -> IndexMap<Ident, Value>
            = "{" __ o:object0() ** (__ "," __) __ ","? __ "}" {
                let mut m = IndexMap::new();
                for (k, v) in o {
                    m.insert(k, v);
                }
                m
            }
        rule object0() -> (Ident, Value)
            = i:ident() [' ']* ":" [' ']* v:value() { (i, v) }

    }
}

#[derive(Debug, Clone)]
pub struct Attribute(String);

#[derive(Debug, Clone)]
pub struct FileExtension(String);

#[derive(Debug, Clone)]
pub struct FileIdentifier(String);

#[derive(Debug, Clone)]
pub struct Enum {
    name: Ident,
    ty: Type,
    metadata: Option<Metadata>,
    values: Vec<EnumValue>,
}

#[derive(Debug, Clone)]
pub struct EnumValue {
    name: Ident,
    value: Option<Integer>,
}

#[derive(Debug, Clone)]
pub struct Union {
    name: Ident,
    metadata: Option<Metadata>,
    values: Vec<UnionValue>,
}

#[derive(Debug, Clone)]
pub struct UnionValue {
    name: Option<Ident>,
    ty: Type,
}

#[derive(Debug, Clone)]
pub struct Field {
    name: Ident,
    ty: Type,
    default_value: Option<Literal>,
    metadata: Option<Metadata>,
}

#[derive(Debug, Clone)]
pub struct Metadata(IndexMap<Ident, Option<SingleValue>>);

#[derive(Debug, Clone)]
pub struct Struct {
    name: Ident,
    metadata: Option<Metadata>,
    fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Table {
    name: Ident,
    metadata: Option<Metadata>,
    fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Bool,
    Byte,
    Ubyte,
    Short,
    Ushort,
    Int,
    Uint,
    Float,
    Long,
    Ulong,
    Double,
    Int8,
    Uint8,
    Int16,
    Uint16,
    Int32,
    Uint32,
    Int64,
    Uint64,
    Float32,
    Float64,
    String,
    Vector(Box<Type>),
    Ident(Ident),
}

#[derive(Debug, Clone)]
pub struct SchemaItems {
    pub(crate) items: Vec<SchemaItem>,
}

#[derive(Debug, Clone)]
pub enum SchemaType {
    Table(Table),
    Struct(Struct),
    Union(Union),
    Enum(Enum),
}

#[derive(Debug, Clone, Default)]
pub struct Schema {
    includes: Vec<Include>,
    namespace: Option<Namespace>,
    attributes: Vec<Attribute>,
    types: Vec<SchemaType>,
    root_type: Option<Root>,
    file_identifier: Option<FileIdentifier>,
    file_extension: Option<FileExtension>,
}

use std::convert::TryFrom;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Error parsing input")]
    FailedParsing(#[from] peg::error::ParseError<peg::str::LineCol>),

    #[error("Duplicate namespace")]
    DuplicateNamespace,

    #[error("Duplicate root type")]
    DuplicateRootType,

    #[error("Duplicate file identifier")]
    DuplicateFileIdentifier,

    #[error("Duplicate file extension")]
    DuplicateFileExtension,
}

impl TryFrom<SchemaItems> for Schema {
    type Error = Error;

    fn try_from(other: SchemaItems) -> Result<Self, Self::Error> {
        let mut schema = Schema::default();

        for item in other.items.into_iter() {
            match item {
                SchemaItem::Include(include) => {
                    schema.includes.push(include);
                }
                SchemaItem::Namespace(ns) => {
                    if schema.namespace.is_some() {
                        return Err(Error::DuplicateNamespace);
                    }
                    schema.namespace = Some(ns);
                }
                SchemaItem::Table(table) => {
                    schema.types.push(SchemaType::Table(table));
                }
                SchemaItem::Struct(struct_) => {
                    schema.types.push(SchemaType::Struct(struct_));
                }
                SchemaItem::Root(root) => {
                    if schema.root_type.is_some() {
                        return Err(Error::DuplicateRootType);
                    }

                    schema.root_type = Some(root);
                }
                SchemaItem::Union(union_) => {
                    schema.types.push(SchemaType::Union(union_));
                }
                SchemaItem::Enum(enum_) => {
                    schema.types.push(SchemaType::Enum(enum_));
                }
                SchemaItem::Attribute(attr) => {
                    schema.attributes.push(attr);
                }
                SchemaItem::FileExtension(ext) => {
                    if schema.file_extension.is_some() {
                        return Err(Error::DuplicateFileExtension);
                    }

                    schema.file_extension = Some(ext);
                }
                SchemaItem::FileIdentifier(id) => {
                    if schema.file_identifier.is_some() {
                        return Err(Error::DuplicateFileIdentifier);
                    }

                    schema.file_identifier = Some(id);
                }
            }
        }

        Ok(schema)
    }
}

#[derive(Debug, Clone)]
pub enum SchemaItem {
    Include(Include),
    Namespace(Namespace),
    Table(Table),
    Struct(Struct),
    Root(Root),
    Union(Union),
    Enum(Enum),
    Attribute(Attribute),
    FileExtension(FileExtension),
    FileIdentifier(FileIdentifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Single(SingleValue),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SingleValue {
    String(String),
    Scalar(Scalar),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Scalar(Scalar),
    String(String),
    Bool(bool),
    Ident(Ident),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Scalar {
    Integer(Integer),
    Float(Float),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Integer {
    Decimal(String),
    Hex(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Float {
    Decimal(String),
    Hex(String),
    Special(String),
}

#[derive(Debug, Clone)]
pub struct Root(Ident);

#[derive(Debug, Clone)]
pub struct Namespace(Vec<Ident>);

#[derive(Debug, Clone)]
pub struct Include {
    path: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke() {
        let x = parse(
            r#"
include "ghostman.fbs";
namespace Pahkat;

union Package {
    Descriptor,
    Synthetic,
    Redirect,
}

union Version {
    Semantic,
    Timestamp,
    Unknown,
}

table Release {
    version: string (key);
}

table Descriptor {
    id: string (key); // Reference to parent.package_keys
    name_keys: [string]; // Reference to parent.known_languages
    name_values: [string];
    description_keys: [string]; // Reference to parent.known_languages
    description_values: [string];
    tags: [string]; // Reference to parent.known_tags
    release: [Release];
}

table Synthetic {

}

table Packages {
    packages_keys: [string];
    packages_values: [Package];
    known_tags: [string];
    known_languages: [string];
}

root_type Packages;
"#,
        );

        println!("{:#?}", x.unwrap());
    }

    #[test]
    fn smoke2() {
        let x = parse(
            r#"table PointPosition { x:uint; y:uint; }
table MarkerPosition {}
union Position {
Start  :  MarkerPosition  ,
Point : PointPosition ,
Finish :MarkerPosition ,  
Angery: bool,
}"#,
        );

        println!("{:#?}", x.unwrap());
    }
}

pub fn parse(s: &str) -> Result<Schema, Error> {
    let out = fbs_parser::schema(s)?;
    Schema::try_from(out)
}
