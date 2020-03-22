use indexmap::IndexMap;
use std::convert::{TryFrom, TryInto};

#[derive(Debug, Default)]
pub(crate) struct Env {
    pub known_types: IndexMap<Ident, SchemaType>,
    pub last_enum_value: isize,
}

peg::parser! {
    pub(crate) grammar fbs_parser(env: &mut Env) for str {
        rule ___() = ([' '] / nl())+
        rule __() = ([' '] / nl())*
        rule _() = [' ']+

        rule nl() = ("\r\n" / "\n")

        pub(crate) rule schema() -> SchemaItems
            = s:schema0()* { SchemaItems { items: s } }
        rule schema0() -> SchemaItem
            = (([' '] / nl()) / comment())* s:schema1() (([' '] / nl()) / comment())* {
                if let SchemaItem::SchemaType(ref ty) = &s {
                    env.known_types.insert(ty.name().clone(), ty.clone());
                }
                s
            }
        rule schema1() -> SchemaItem
            = i:include() { SchemaItem::Include(i) }
            / n:namespace() { SchemaItem::Namespace(n) }
            / t:table_or_struct() { t }
            / e:enum() { SchemaItem::SchemaType(SchemaType::Enum(e)) }
            / u:union() { SchemaItem::SchemaType(SchemaType::Union(u)) }
            / r:root() { SchemaItem::RootType(r) }
            / f:file_extension() { SchemaItem::FileExtension(f) }
            / f:file_identifier() { SchemaItem::FileIdentifier(f) }
            / a:attribute() { SchemaItem::Attribute(a) }
            // / r:rpc()
        rule comment() -> String
            = "//" s:$((!nl()[_])*) nl() { s.trim().to_string() }
        rule attribute() -> Attribute
            = "attribute" _ s:string_literal() __ semi_eol() { Attribute(s) }
        rule file_extension() -> FileExtension
            = "file_extension" _ s:string_literal() __ semi_eol() { FileExtension(s) }
        rule file_identifier() -> FileIdentifier
            = "file_identifier" _ s:string_literal() __ semi_eol() {? FileIdentifier::try_from(&*s) }

        rule semi_eol()
            = ";" (__ comment() __)*

        rule table_or_struct() -> SchemaItem
            = x:$("table" / "struct") _ i:ident() " "* m:metadata()? " "* "{" __ f:field()* __ "}" {
                if x == "table" {
                    SchemaItem::SchemaType(SchemaType::Table(Table {
                        name: i,
                        metadata: m,
                        fields: f,
                    }))
                } else {
                    SchemaItem::SchemaType(SchemaType::Struct(Struct {
                        name: i,
                        metadata: m,
                        fields: f,
                    }))
                }
            }

        rule type_ident0() -> Type
            = i:ident() {?
                match env.known_types.get(&i) {
                    Some(v) => Ok(Type::SchemaType(Box::new((*v).to_owned()))),
                    None => Err("invalid known type"),
                }
            }

        rule type_ident() -> Type
            = "bool" { Type::Primitive(Primitive::Bool) }
            / ("byte" / "int8") { Type::Primitive(Primitive::Int8) }
            / ("ubyte" / "uint8") { Type::Primitive(Primitive::Uint8) }
            / ("short" / "int16") { Type::Primitive(Primitive::Int16) }
            / ("ushort" / "uint16") { Type::Primitive(Primitive::Uint16) }
            / ("int" / "int32") { Type::Primitive(Primitive::Int32) }
            / ("uint" / "uint32") { Type::Primitive(Primitive::Uint32) }
            / ("long" / "int64") { Type::Primitive(Primitive::Int64) }
            / ("ulong"/ "uint64") { Type::Primitive(Primitive::Uint64) }
            / ("float" / "float32") { Type::Primitive(Primitive::Float32) }
            / ("double" / "float64") { Type::Primitive(Primitive::Float64) }
            / "string" { Type::String }
            / "[" i:type_ident() "]" { Type::Vector(Box::new(i)) }
            / i:type_ident0() { i }

        rule enum_value() -> EnumValue
            = i:ident() [' ']* "=" [' ']* v:integer() {
                env.last_enum_value = v;
                EnumValue { name: i, value: v }
            }
            / i:ident() {?
                match env.last_enum_value.checked_add(1) {
                    Some(v) => {
                        env.last_enum_value = v;
                        Ok(EnumValue { name: i, value: v })
                    },
                    None => {
                        Err("hit isize limit for enum variants")
                    }
                }
            }

        rule enum() -> Enum
            = "enum" _ i:ident() [' ']* ":" [' ']* t:type_ident() " "* m:metadata()? "{" __ v:enum_value() ** (__ "," __) __ ","? __ "}" {?
                env.last_enum_value = 0;

                match t {
                    Type::Primitive(p) => Ok(Enum {
                        name: i,
                        ty: p,
                        metadata: m,
                        values: v
                    }),
                    _ => Err("invalid type")
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

        pub(crate) rule ident() -> Ident
            = v:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { Ident(v.to_string()) }

        rule integer() -> isize
            = x:$(['-' | '+']? digit()+) {? x.parse::<isize>().map_err(|_| "invalid decimal integer") }
            / x:$(['-' | '+']?"0"['x' | 'X'] xdigit()+) {? x.parse::<isize>().map_err(|_| "invalid hexadecimal integer") }

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

        rule root() -> RootType
            = "root_type" _ i:ident() __ semi_eol() { RootType(i) }

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
pub struct FileIdentifier(pub [u8; 4]);

impl<'a> TryFrom<&'a str> for FileIdentifier {
    type Error = &'static str;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        let v: [u8; 4] = value
            .as_bytes()
            .try_into()
            .map_err(|_| "file identifier must be 4 bytes")?;
        Ok(FileIdentifier(v))
    }
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: Ident,
    pub ty: Primitive,
    pub metadata: Option<Metadata>,
    pub values: Vec<EnumValue>,
}

impl Enum {
    #[inline(always)]
    pub fn table_field_size(&self) -> usize {
        self.ty.table_field_size()
    }

    #[inline(always)]
    pub fn value(&self, input: isize) -> Option<&EnumValue> {
        self.values.iter().find(|x| x.value == input)
    }
}

#[derive(Debug, Clone)]
pub struct EnumValue {
    pub name: Ident,
    pub value: isize,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub name: Ident,
    pub metadata: Option<Metadata>,
    pub values: Vec<UnionValue>,
}

#[derive(Debug, Clone)]
pub struct UnionValue {
    pub name: Option<Ident>,
    pub ty: Type,
}

impl Union {
    #[inline(always)]
    pub fn table_field_size(&self) -> usize {
        // Byte for union tag plus the offset size
        1 + IOFFSET_SIZE
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Ident,
    pub ty: Type,
    pub default_value: Option<Literal>,
    pub metadata: Option<Metadata>,
}

#[derive(Debug, Clone)]
pub struct Metadata(pub IndexMap<Ident, Option<SingleValue>>);

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Ident,
    pub metadata: Option<Metadata>,
    pub fields: Vec<Field>,
}

impl Struct {
    #[inline]
    pub fn table_field_size(&self) -> usize {
        self.fields
            .iter()
            .fold(0, |acc, cur| acc + cur.ty.table_field_size())
    }

    #[inline]
    pub fn offset(&self, ident: &Ident) -> Option<(usize, &Field)> {
        let mut offset = 0usize;

        for field in self.fields.iter() {
            let size = field.ty.table_field_size();

            if &field.name == ident {
                return Some((offset, field));
            }

            offset = offset.checked_add(size)?;
        }

        None
    }
}

#[derive(Debug, Clone)]
pub struct Table {
    pub name: Ident,
    pub metadata: Option<Metadata>,
    pub fields: Vec<Field>,
}

impl Table {
    pub fn id(&self, ident: &Ident) -> Option<(usize, &Field)> {
        let mut index = 0usize;

        for field in self.fields.iter() {
            if field.ty.is_union() {
                // Handle the hidden tag field which gets its own id
                index = index.checked_add(1)?;
            }

            // TODO: check metadata for id fields
            if &field.name == ident {
                return Some((index, field));
            }

            index = index.checked_add(1)?;
        }

        let index = self.fields.iter().position(|x| &x.name == ident)?;
        Some((index, &self.fields[index]))
    }
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Bool,
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
}

#[derive(Debug, Clone)]
pub enum Type {
    String,
    Primitive(Primitive),
    FixedArray(Primitive, usize),
    Vector(Box<Type>),
    SchemaType(Box<SchemaType>),
}

const UOFFSET_SIZE: usize = 4;
const IOFFSET_SIZE: usize = 4;

impl Type {
    #[inline]
    pub fn table_field_size(&self) -> usize {
        match self {
            Type::Primitive(p) => p.table_field_size(),
            Type::FixedArray(ty, count) => ty.table_field_size() * count,
            Type::SchemaType(ty) => ty.table_field_size(),
            Type::String | Type::Vector(_) => UOFFSET_SIZE,
        }
    }

    #[inline(always)]
    pub fn is_union(&self) -> bool {
        if let Type::SchemaType(ty) = self {
            match **ty {
                SchemaType::Union(_) => true,
                _ => false,
            }
        } else {
            false
        }
    }
}

impl Primitive {
    #[inline]
    pub fn table_field_size(&self) -> usize {
        match self {
            Primitive::Bool | Primitive::Uint8 | Primitive::Int8 => 1,
            Primitive::Int16 | Primitive::Uint16 => 2,
            Primitive::Float32 | Primitive::Int32 | Primitive::Uint32 => 4,
            Primitive::Float64 | Primitive::Int64 | Primitive::Uint64 => 8,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct SchemaItems {
    pub(crate) items: Vec<SchemaItem>,
}

#[derive(Debug, Clone)]
pub enum SchemaType {
    Table(Table),
    Struct(Struct),
    Union(Union),
    Enum(Enum),
}

impl SchemaType {
    #[inline]
    pub fn table_field_size(&self) -> usize {
        match self {
            SchemaType::Table(_) => IOFFSET_SIZE,
            SchemaType::Struct(struct_) => struct_.table_field_size(),
            SchemaType::Union(union_) => union_.table_field_size(),
            SchemaType::Enum(enum_) => enum_.table_field_size(),
        }
    }
}

impl SchemaType {
    pub fn name(&self) -> &Ident {
        match self {
            SchemaType::Table(t) => &t.name,
            SchemaType::Struct(t) => &t.name,
            SchemaType::Union(t) => &t.name,
            SchemaType::Enum(t) => &t.name,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Schema {
    pub includes: Vec<Include>,
    pub namespace: Option<Namespace>,
    pub attributes: Vec<Attribute>,
    pub types: Vec<SchemaType>,
    pub root_type: Option<RootType>,
    pub file_identifier: Option<FileIdentifier>,
    pub file_extension: Option<FileExtension>,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Include not found")]
    IncludeNotFound(#[source] std::io::Error),

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
                SchemaItem::SchemaType(ty) => {
                    schema.types.push(ty);
                }
                SchemaItem::RootType(root) => {
                    if schema.root_type.is_some() {
                        return Err(Error::DuplicateRootType);
                    }

                    schema.root_type = Some(root);
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
pub(crate) enum SchemaItem {
    Include(Include),
    Namespace(Namespace),
    SchemaType(SchemaType),
    RootType(RootType),
    Attribute(Attribute),
    FileExtension(FileExtension),
    FileIdentifier(FileIdentifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

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
    Integer(isize),
    Float(Float),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Float {
    Decimal(String),
    Hex(String),
    Special(String),
}

#[derive(Debug, Clone)]
pub struct RootType(pub Ident);

#[derive(Debug, Clone)]
pub struct Namespace(pub Vec<Ident>);

#[derive(Debug, Clone)]
pub struct Include {
    pub path: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke() {
        let x = parse_path(std::path::Path::new("tests/test.fbs"));

        println!("{:#?}", x);
    }

    #[test]
    fn smoke2() {
        let x = parse_path(std::path::Path::new("tests/test2.fbs"));

        println!("{:#?}", x);
    }
}

fn parse(s: &str) -> Result<Schema, Error> {
    let out = fbs_parser::schema(s, &mut Env::default())?;
    Schema::try_from(out)
}

#[inline]
pub fn ident(s: &str) -> Result<Ident, peg::error::ParseError<peg::str::LineCol>> {
    fbs_parser::ident(s, &mut Env::default())
}

pub fn parse_path(base_path: &std::path::Path) -> Result<Vec<Schema>, Error> {
    let s = std::fs::read_to_string(&base_path).map_err(Error::IncludeNotFound)?;
    let schema = parse(&s)?;

    let mut schemas: Vec<Schema> = schema
        .includes
        .iter()
        .map(|x| dbg!(parse_path(&base_path.parent().unwrap().join(&x.path))))
        .collect::<Result<Vec<Vec<Schema>>, Error>>()?
        .into_iter()
        .flatten()
        .collect();

    schemas.push(schema);
    Ok(schemas)
}
