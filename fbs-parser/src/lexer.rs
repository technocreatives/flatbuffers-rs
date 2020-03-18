use indexmap::IndexMap;

peg::parser! {
    grammar fbs_parser() for str {
        rule ___() = [' ' | '\n']+
        rule __() = [' ' | '\n']*
        rule _() = [' ']+

        pub rule schema() -> Schema<'input>
            = s:schemaT(<{ Schema::default() }>) { s }
        rule schemaT(schema: Schema<'input>) -> Schema<'input>
            = schema0(<{&mut schema}>)*
        rule schema0(schema: &mut Schema<'input>)
            = ([' ' | '\n'] / comment())* schema1(<{&mut schema}>) ([' ' | '\n'] / comment())*
        rule schema1(schema: &mut Schema<'input>)
            // = i:include() { SchemaType::Include(i) }
            // / n:namespace() { SchemaType::Namespace(n) }
            = t:table_or_struct() { schema.types.push(t); }
            / e:enum() { schema.types.push(SchemaType::Enum(e)); }
            / u:union() { schema.types.push(SchemaType::Union(u)); }
            // / r:root() { SchemaType::Root(r) }
            // / f:file_extension() { SchemaType::FileExtension(f) }
            // / f:file_identifier() { SchemaType::FileIdentifier(f) }
            // / a:attribute() { SchemaType::Attribute(a) }
            // / r:rpc()
        rule comment() -> &'input str
            = "//" s:$((!['\n'][_])*) "\n" { s.trim() }
        rule attribute() -> Attribute<'input>
            = "attribute" _ s:string_literal() __ semi_eol() { Attribute(s) }
        rule file_extension() -> FileExtension<'input>
            = "file_extension" _ s:string_literal() __ semi_eol() { FileExtension(s) }
        rule file_identifier() -> FileIdentifier<'input>
            = "file_identifier" _ s:string_literal() __ semi_eol() { FileIdentifier(s) }

        rule semi_eol()
            = ";" (__ comment() __)*

        rule table_or_struct() -> SchemaType<'input>
            = x:$("table" / "struct") _ i:ident() " "* m:metadata()? " "* "{" __ f:field()* __ "}" {
                if x == "table" {
                    SchemaType::Table(Table {
                        name: i,
                        metadata: m,
                        fields: f,
                    })
                } else {
                    SchemaType::Struct(Struct {
                        name: i,
                        metadata: m,
                        fields: f,
                    })
                }
            }

        rule type_ident() -> Type<'input>
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

        rule enum_value() -> EnumValue<'input>
            = i:ident() [' ']* "=" [' ']* v:integer() { EnumValue { name: i, value: Some(v) } }
            / i:ident() { EnumValue { name: i, value: None } }

        rule enum() -> Enum<'input>
            = "enum" _ i:ident() [' ']* ":" [' ']* t:type_ident() " "* m:metadata()? "{" __ v:enum_value() ** (__ "," __) __ ","? __ "}" {
                Enum {
                    name: i,
                    ty: t,
                    metadata: m,
                    values: v
                }
            }
        rule union_value() -> UnionValue<'input>
            = i:ident() [' ']* ":" [' ']* t:type_ident() { UnionValue { ty: t, name: Some(i) } }
            / t:type_ident() { UnionValue { ty: t, name: None } }

        rule union() -> Union<'input>
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

        rule ident() -> Ident<'input>
            = v:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { Ident(v) }

        rule integer() -> Integer<'input>
            = x:$(['-' | '+']? digit()+) { Integer::Decimal(x) }
            / x:$(['-' | '+']?"0"['x' | 'X'] xdigit()+) { Integer::Hex(x) }

        rule float() -> Float<'input>
            = x:$(['-' | '+']? ("." digit()+ / digit()+ "." digit()* / digit()+) (['e'|'E']['-' | '+']? digit()+)?) { Float::Decimal(x) }
            / x:$(['-' | '+']? "0"['x'|'X'] ("." xdigit()+ / xdigit()+ "." xdigit()* / xdigit()+) (['p'|'P']['-' | '+']? digit()+)?) { Float::Hex(x) }
            / x:$(['-' | '+']? ("nan" / "inf" / "infinity")) { Float::Special(x)}
        rule bool() -> bool
            = "true" { true }
            / "false" { false }

        rule scalar() -> Scalar<'input>
            = i:integer() { Scalar::Integer(i) }
            / f:float() { Scalar::Float(f) }

        rule literal() -> Literal<'input>
            = s:scalar() { Literal::Scalar(s) }
            / s:string_literal() { Literal::String(s) }
            / b:bool() { Literal::Bool(b) }
            / i:ident() { Literal::Ident(i) }

        rule single_value() -> SingleValue<'input>
            = s:scalar() { SingleValue::Scalar(s) }
            / s:string_literal() { SingleValue::String(s) }

        rule metadata() -> Metadata<'input>
            = "(" __ o:metadata0() ** (__ "," __) __ ")" {
                let mut m = IndexMap::new();
                for (k, v) in o {
                    m.insert(k, v);
                }
                Metadata(m)
            }
        rule metadata0() -> (Ident<'input>, Option<SingleValue<'input>>)
            = i:ident() s:metadata1()? {
                (i, s)
            }
        rule metadata1() -> SingleValue<'input>
            = [' ']* ":" [' ']* s:single_value() { s }

        rule field() -> Field<'input>
            = i:ident() [' ']* ":" [' ']* t:type_ident() l:field0()? m:field1()? semi_eol() __ {
                Field {
                    name: i,
                    ty: t,
                    default_value: l,
                    metadata: m,
                }
            }
        rule field0() -> Literal<'input>
            = _ "=" _ l:literal() { l }
        rule field1() -> Metadata<'input>
            = _ m:metadata() { m }

        rule string_literal() -> &'input str
            = "\"" s:$((!['"'][_])*) "\"" { s }

        rule include() -> Include<'input>
            = "include" _ path:string_literal() __ semi_eol() { Include { path } }

        rule namespace() -> Namespace<'input>
            = "namespace" _ i:ident() ** "." __ semi_eol() { Namespace(i) }

        rule root() -> Root<'input>
            = "root_type" _ i:ident() __ semi_eol() { Root(i) }

        rule value() -> Value<'input>
            = s:string_literal() { Value::Single(SingleValue::String(s)) }

        rule object() -> IndexMap<Ident<'input>, Value<'input>>
            = "{" __ o:object0() ** (__ "," __) __ ","? __ "}" {
                let mut m = IndexMap::new();
                for (k, v) in o {
                    m.insert(k, v);
                }
                m
            }
        rule object0() -> (Ident<'input>, Value<'input>)
            = i:ident() [' ']* ":" [' ']* v:value() { (i, v) }

    }
}

#[derive(Debug, Clone)]
pub struct Attribute<'a>(&'a str);

#[derive(Debug, Clone)]
pub struct FileExtension<'a>(&'a str);

#[derive(Debug, Clone)]
pub struct FileIdentifier<'a>(&'a str);

#[derive(Debug, Clone)]
pub struct Enum<'a> {
    name: Ident<'a>,
    ty: Type<'a>,
    metadata: Option<Metadata<'a>>,
    values: Vec<EnumValue<'a>>,
}

#[derive(Debug, Clone)]
pub struct EnumValue<'a> {
    name: Ident<'a>,
    value: Option<Integer<'a>>,
}

#[derive(Debug, Clone)]
pub struct Union<'a> {
    name: Ident<'a>,
    metadata: Option<Metadata<'a>>,
    values: Vec<UnionValue<'a>>,
}

#[derive(Debug, Clone)]
pub struct UnionValue<'a> {
    name: Option<Ident<'a>>,
    ty: Type<'a>,
}

#[derive(Debug, Clone)]
pub struct Field<'a> {
    name: Ident<'a>,
    ty: Type<'a>,
    default_value: Option<Literal<'a>>,
    metadata: Option<Metadata<'a>>,
}

#[derive(Debug, Clone)]
pub struct Metadata<'a>(IndexMap<Ident<'a>, Option<SingleValue<'a>>>);

#[derive(Debug, Clone)]
pub struct Struct<'a> {
    name: Ident<'a>,
    metadata: Option<Metadata<'a>>,
    fields: Vec<Field<'a>>,
}

#[derive(Debug, Clone)]
pub struct Table<'a> {
    name: Ident<'a>,
    metadata: Option<Metadata<'a>>,
    fields: Vec<Field<'a>>,
}

#[derive(Debug, Clone)]
pub enum Type<'a> {
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
    Vector(Box<Type<'a>>),
    Ident(Ident<'a>),
}

// #[derive(Debug, Clone)]
// pub struct Schema<'a> {
//     pub(crate) items: Vec<SchemaType<'a>>,
// }


#[derive(Debug, Clone)]
pub enum SchemaType<'a> {
    Table(Table<'a>),
    Struct(Struct<'a>),
    Union(Union<'a>),
    Enum(Enum<'a>),
}

#[derive(Debug, Clone, Default)]
pub struct Schema<'a> {
    includes: Vec<Include<'a>>,
    namespace: Option<Namespace<'a>>,
    attributes: Vec<Attribute<'a>>,
    types: Vec<SchemaType<'a>>,
    root_type: Option<Root<'a>>,
    file_identifier: Option<FileIdentifier<'a>>,
    file_extension: Option<FileExtension<'a>>,
}

use std::convert::TryFrom;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Duplicate namespace")]
    DuplicateNamespace,

    #[error("Duplicate root type")]
    DuplicateRootType,

    #[error("Duplicate file identifier")]
    DuplicateFileIdentifier,

    #[error("Duplicate file extension")]
    DuplicateFileExtension,
}

// impl<'a> TryFrom<lexer::Schema<'a>> for Schema<'a> {
//     type Error = Error;

//     fn try_from(other: lexer::Schema<'a>) -> Result<Self, Self::Error> {
//         let mut schema = Schema::default();
        
//         for item in other.items.into_iter() {
//             use lexer::SchemaType;

//             match item {
//                 SchemaType::Include(include) => { schema.includes.push(include); },
//                 SchemaType::Namespace(ns) => {
//                     if !schema.namespace.is_some() {
//                         return Err(Error::DuplicateNamespace);
//                     }
//                     schema.namespace = Some(ns);
//                 },
//                 SchemaType::Table(table) => { schema.types.push(SchemaType::Table(table)); },
//                 SchemaType::Struct(struct_) => { schema.types.push(SchemaType::Struct(struct_)); }
//                 SchemaType::Root(root) => {
//                     if schema.root_type.is_some() {
//                         return Err(Error::DuplicateRootType);
//                     }

//                     schema.root_type = Some(root);
//                 }
//                 SchemaType::Union(union_) => { schema.types.push(SchemaType::Union(union_)); }
//                 SchemaType::Enum(enum_) => { schema.types.push(SchemaType::Enum(enum_)); }
//                 SchemaType::Attribute(attr) => { schema.attributes.push(attr); }
//                 SchemaType::FileExtension(ext) => {
//                     if schema.file_extension.is_some() {
//                         return Err(Error::DuplicateFileExtension);
//                     }

//                     schema.file_extension = Some(ext);
//                 }
//                 SchemaType::FileIdentifier(id) => {
//                     if schema.file_identifier.is_some() {
//                         return Err(Error::DuplicateFileIdentifier);
//                     }

//                     schema.file_identifier = Some(id);
//                 }
//             }
//         }

//         Ok(schema)
//     }
// }


// #[derive(Debug, Clone)]
// pub enum SchemaType<'a> {
//     Include(Include<'a>),
//     Namespace(Namespace<'a>),
//     Table(Table<'a>),
//     Struct(Struct<'a>),
//     Root(Root<'a>),
//     Union(Union<'a>),
//     Enum(Enum<'a>),
//     Attribute(Attribute<'a>),
//     FileExtension(FileExtension<'a>),
//     FileIdentifier(FileIdentifier<'a>),
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident<'a>(&'a str);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value<'a> {
    Single(SingleValue<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SingleValue<'a> {
    String(&'a str),
    Scalar(Scalar<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal<'a> {
    Scalar(Scalar<'a>),
    String(&'a str),
    Bool(bool),
    Ident(Ident<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Scalar<'a> {
    Integer(Integer<'a>),
    Float(Float<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Integer<'a> {
    Decimal(&'a str),
    Hex(&'a str),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Float<'a> {
    Decimal(&'a str),
    Hex(&'a str),
    Special(&'a str),
}

#[derive(Debug, Clone)]
pub struct Root<'a>(Ident<'a>);

#[derive(Debug, Clone)]
pub struct Namespace<'a>(Vec<Ident<'a>>);

#[derive(Debug, Clone)]
pub struct Include<'a> {
    path: &'a str,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke() {
        let x = fbs_parser::schema(
            r#"
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
        let x = fbs_parser::schema(
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
