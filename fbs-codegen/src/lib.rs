mod types;

use std::borrow::Cow;
use std::convert::{TryFrom, TryInto};
use std::path::Path;

use fbs_parser::{ident, parse_path, Schema, SchemaType};
use types::*;

macro_rules! value {
    ($buf:expr, Bool => bool, $offset:expr) => {{
        assert!($offset != 0);
        Value::Bool($buf[$offset as usize] != 0)
    }};
    ($buf:expr, $fty:ident => $ty:ty, $offset:expr) => {{
        assert!($offset != 0);
        let offset = $offset as usize;
        let v = <$ty>::from_le_bytes(
            $buf[offset..offset + std::mem::size_of::<$ty>()]
                .try_into()
                .unwrap(),
        );
        Value::$fty(v)
    }};
}

#[derive(Debug, thiserror::Error)]
pub enum SchemaError {
    #[error("No root declaration found for schema")]
    MissingRootDecl,

    #[error("No root type found for schema")]
    MissingRootType,
}

#[derive(Debug, thiserror::Error)]
pub enum WalkError {
    #[error("Schema error")]
    Schema(#[from] SchemaError),

    #[error("Data too short, got {0} bytes")]
    DataTooShort(usize),

    #[error("Invalid file identifier: expected `{expected:?}`, found: `{found:?}`")]
    InvalidFileIdentifier { expected: [u8; 4], found: [u8; 4] },

    #[error("Buffer overflow: length {length}, but requested offset {offset}")]
    BufferOverflow { length: usize, offset: uoffset },

    #[error("Received a root type that cannot be handled: {0:?}")]
    CannotHandleRootType(SchemaType),
}

pub struct Parser<'s, 'b> {
    schema: std::borrow::Cow<'s, Schema>,
    buf: std::borrow::Cow<'b, [u8]>,
    buf_len: u32,
}

trait SchemaExt {
    fn root_type(&self) -> Result<&SchemaType, SchemaError>;
}

impl SchemaExt for Schema {
    #[inline]
    fn root_type(&self) -> Result<&SchemaType, SchemaError> {
        let ident = &self
            .root_type
            .as_ref()
            .ok_or_else(|| SchemaError::MissingRootDecl)?
            .0;
        self.types
            .iter()
            .find(|x| x.name() == ident)
            .ok_or_else(|| SchemaError::MissingRootType)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum WalkerInitError {
    #[error("Buffer too large: offset type size {offset_size}, buffer length {len}, maximum length: {max_len}")]
    BufferTooLarge {
        offset_size: usize,
        len: usize,
        max_len: usize,
    },
}

#[derive(Debug)]
struct VTable<'a> {
    buf: &'a [u8],
    len: u16,
}

impl<'a> VTable<'a> {
    pub(crate) fn new(offset: uoffset, buf: &'a [u8]) -> Result<VTable<'a>, ValueError> {
        const MIN_VTABLE_LEN: u32 = std::mem::size_of::<u16>() as u32;

        if (offset + MIN_VTABLE_LEN) as usize >= buf.len() {
            return Err(ValueError::BufferOverflow {
                length: buf.len(),
                offset,
            });
        }

        let a = offset as usize;
        let b = (offset + MIN_VTABLE_LEN) as usize;
        let vtable_len = u16::from_le_bytes(buf[a..b].try_into().unwrap());

        Ok(VTable {
            buf: &buf[a..vtable_len as usize],
            len: vtable_len,
        })
    }

    #[inline(always)]
    pub fn len(&self) -> u16 {
        self.len
    }

    #[inline(always)]
    pub fn table_len(&self) -> u16 {
        if self.len >= 4 {
            u16::from_le_bytes(self.buf[2..4].try_into().unwrap())
        } else {
            0
        }
    }

    #[inline(always)]
    pub fn table_offset(&self, id: usize) -> Option<voffset> {
        const ID_START_OFFSET: usize = 4;
        const TWO_BYTES: usize = 2;

        let id_offset = id * TWO_BYTES + ID_START_OFFSET;
        let id_offset = match u16::try_from(id_offset) {
            Ok(v) => v as usize,
            Err(_) => return None,
        };

        if id_offset > self.len() as usize - TWO_BYTES {
            return None;
        }

        let v = voffset::from_le_bytes(self.buf[id_offset..id_offset + 2].try_into().unwrap());

        if v == 0 {
            None
        } else {
            Some(v)
        }
    }
}

#[derive(Debug)]
pub struct Table<'a> {
    data: &'a [u8],
    offset: uoffset,
    buf: &'a [u8],
    vtable: VTable<'a>,
    schema: &'a fbs_parser::Table,
}

#[derive(Debug)]
pub struct Struct<'a> {
    buf: &'a [u8],
    schema: &'a fbs_parser::Struct,
}

impl<'a> Struct<'a> {
    pub(crate) fn new(
        offset: ioffset,
        schema_struct: &'a fbs_parser::Struct,
        buf: &'a [u8],
    ) -> Result<Struct<'a>, ValueError> {
        let end = offset as usize + schema_struct.table_field_size();
        Ok(Struct {
            buf: &buf[offset as usize..end],
            schema: schema_struct,
        })
    }

    pub fn get(&self, ident: &fbs_parser::Ident) -> Result<Option<Value>, ValueError> {
        let (offset, schema_field) = self
            .schema
            .offset(ident)
            .ok_or_else(|| ValueError::KeyNotFound(ident.clone()))?;

        use fbs_parser::Type;

        let v = match &schema_field.ty {
            Type::Primitive(ty) => primitive(self.buf, ty, offset as uoffset),
            Type::FixedArray(_, _) => todo!(),
            Type::SchemaType(ty) => match &**ty {
                SchemaType::Struct(ty) => {
                    let struct_ = Some(Struct::new(offset as i32, &ty, self.buf)?);
                    Value::Struct(struct_)
                }
                SchemaType::Enum(ty) => enum_value(self.buf, ty, offset as uoffset)?,
                _ => return Err(ValueError::InvalidStructField),
            },
            _ => return Err(ValueError::InvalidStructField),
        };

        Ok(Some(v))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ValueError {
    #[error("Key not found in schema: {0:?}")]
    KeyNotFound(fbs_parser::Ident),

    #[error("Buffer overflow: length {length}, but requested offset {offset}")]
    BufferOverflow { length: usize, offset: uoffset },

    #[error("Invalid struct field")]
    InvalidStructField,

    #[error("Could not convert byte slice to UTF-8 string")]
    InvalidUtf8(#[from] std::str::Utf8Error),

    #[error("Could not convert enum value to isize integer")]
    EnumConvertError(#[from] TryFromValueError),

    #[error("Could not find value for given integer value `{0}`")]
    InvalidEnumValue(isize),
}

#[inline(always)]
fn primitive<'a>(buf: &'a [u8], prim: &fbs_parser::Primitive, offset: uoffset) -> Value<'a> {
    use fbs_parser::Primitive;

    match prim {
        Primitive::Bool => value!(buf, Bool => bool, offset),
        Primitive::Uint8 => value!(buf, U8 => u8, offset),
        Primitive::Int8 => value!(buf, I8 => i8, offset),
        Primitive::Uint16 => value!(buf, U16 => u16, offset),
        Primitive::Int16 => value!(buf, I16 => i16, offset),
        Primitive::Uint32 => value!(buf, U32 => u32, offset),
        Primitive::Int32 => value!(buf, I32 => i32, offset),
        Primitive::Uint64 => value!(buf, U64 => u64, offset),
        Primitive::Int64 => value!(buf, I64 => i64, offset),
        Primitive::Float32 => value!(buf, F32 => f32, offset),
        Primitive::Float64 => value!(buf, F64 => f64, offset),
    }
}

#[inline(always)]
fn enum_value<'a>(
    buf: &'a [u8],
    ty: &'a fbs_parser::Enum,
    offset: uoffset,
) -> Result<Value<'a>, ValueError> {
    let prim = primitive(buf, &ty.ty, offset);
    let value: isize = prim.try_into()?;
    let enum_value = ty
        .value(value)
        .ok_or_else(|| ValueError::InvalidEnumValue(value))?;
    Ok(Value::Enum(ty, enum_value))
}

impl<'a> Table<'a> {
    pub(crate) fn new(
        offset: uoffset,
        schema_table: &'a fbs_parser::Table,
        buf: &'a [u8],
    ) -> Result<Table<'a>, ValueError> {
        const MIN_TABLE_LEN: u32 = std::mem::size_of::<uoffset>() as u32;

        if (offset + MIN_TABLE_LEN) as usize >= buf.len() {
            return Err(ValueError::BufferOverflow {
                length: buf.len(),
                offset: offset as u32,
            });
        }

        let a = offset as usize;
        let b = (offset + MIN_TABLE_LEN) as usize;

        let vtable_ioffset = ioffset::from_le_bytes(buf[a..b].try_into().unwrap());
        let vtable_offset = (offset as ioffset - vtable_ioffset) as uoffset;
        let vtable = VTable::new(vtable_offset, buf)?;

        println!("{:?}", vtable);

        let table_end = offset as usize + vtable.table_len() as usize;

        if table_end >= buf.len() {
            return Err(ValueError::BufferOverflow {
                length: buf.len(),
                offset: offset as u32,
            });
        }

        Ok(Table {
            data: buf,
            offset,
            buf: &buf[offset as usize..table_end],
            vtable,
            schema: schema_table,
        })
    }

    #[inline(always)]
    fn offset(&self, offset: voffset) -> Option<ioffset> {
        assert!(offset != 0);

        let offset = offset as usize;

        let s = self.data[offset..offset + std::mem::size_of::<ioffset>()]
            .try_into()
            .unwrap();
        let v = ioffset::from_le_bytes(s);
        if v != 0 {
            Some(v)
        } else {
            None
        }
    }

    pub fn get(&self, ident: &fbs_parser::Ident) -> Result<Option<Value>, ValueError> {
        let (id, schema_field) = self
            .schema
            .id(ident)
            .ok_or_else(|| ValueError::KeyNotFound(ident.clone()))?;
        let table_field_offset = match self.vtable.table_offset(id) {
            Some(v) => v,
            None => return Ok(None),
        };

        use fbs_parser::Type;

        let v = match &schema_field.ty {
            Type::Vector(ty) => {
                // Check if type is a union, because it is, we're gonna have a bad time.
                if ty.is_union() {
                    // HAHAHHA
                    todo!()
                }

                let offset = match self.offset(table_field_offset) {
                    Some(v) => v,
                    None => return Ok(Some(Value::Vector(None))),
                };
                let offset = ((self.offset as ioffset) - offset) as usize;
                let size_end = offset + std::mem::size_of::<uoffset>();
                let len =
                    u32::from_le_bytes(self.data[offset..size_end].try_into().unwrap()) as usize;
                let slice = &self.data[size_end..size_end + len];

                Value::Vector(Some(Vector::new(slice, ty)))
            }
            Type::String => {
                let offset = match self.offset(table_field_offset) {
                    Some(v) => v,
                    None => return Ok(Some(Value::String(None))),
                };
                let offset = ((self.offset as ioffset) - offset) as usize;
                let size_end = offset + std::mem::size_of::<uoffset>();
                let len =
                    u32::from_le_bytes(self.data[offset..size_end].try_into().unwrap()) as usize;
                let slice = &self.data[size_end..size_end + len];
                let s = std::str::from_utf8(slice)?;

                Value::String(Some(s))
            }
            Type::Primitive(ty) => primitive(self.buf, ty, table_field_offset as uoffset),
            Type::FixedArray(_, _) => todo!(),
            Type::SchemaType(ty) => match &**ty {
                SchemaType::Table(ty) => {
                    let table = match self.offset(table_field_offset) {
                        Some(relative_offset) => {
                            let offset = (self.offset as ioffset + relative_offset) as uoffset;
                            Some(Table::new(offset, &ty, self.data)?)
                        }
                        None => None,
                    };
                    Value::Table(table)
                }
                SchemaType::Struct(ty) => {
                    let struct_ = match self.offset(table_field_offset) {
                        Some(offset) => Some(Struct::new(offset, &ty, self.data)?),
                        None => None,
                    };
                    Value::Struct(struct_)
                }
                SchemaType::Enum(ty) => enum_value(self.buf, ty, table_field_offset as uoffset)?,
                SchemaType::Union(ty) => {
                    let union_tag_offset = match self.vtable.table_offset(id - 1) {
                        Some(v) => self.offset + v as uoffset,
                        None => return Ok(None),
                    };

                    let union_tag = self.buf[union_tag_offset as usize];

                    if union_tag == 0 {
                        return Ok(None);
                    }

                    let union_value = &ty.values[union_tag as usize];

                    let value = match &union_value.ty {
                        Type::SchemaType(ty) => match &**ty {
                            SchemaType::Table(ty) => {
                                let table = match self.offset(table_field_offset) {
                                    Some(relative_offset) => {
                                        let offset = (self.offset as ioffset + relative_offset) as uoffset;
                                        Some(Table::new(offset, ty, self.data)?)
                                    }
                                    None => None,
                                };
                                Value::Table(table)
                            },
                            _ => todo!()
                        },
                        _ => todo!()
                    };

                    Value::Union(ty, union_value, Box::new(value))
                }
            },
        };

        Ok(Some(v))
    }
}

#[derive(Debug)]
pub struct Vector<'a> {
    buf: &'a [u8],
    ty: &'a fbs_parser::Type,
}

impl<'a> Vector<'a> {
    pub fn new(buf: &'a [u8], ty: &'a fbs_parser::Type) -> Vector<'a> {
        Vector { buf, ty }
    }
}

#[derive(Debug)]
pub enum Value<'a> {
    Table(Option<Table<'a>>),
    Struct(Option<Struct<'a>>),
    Enum(&'a fbs_parser::Enum, &'a fbs_parser::EnumValue),
    Union(
        &'a fbs_parser::Union,
        &'a fbs_parser::UnionValue,
        Box<Value<'a>>,
    ),
    Vector(Option<Vector<'a>>),
    // UnionVector(Option<UnionVector<'a>>),
    String(Option<&'a str>),
    Bool(bool),
    F32(f32),
    F64(f64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

#[derive(Debug, thiserror::Error)]
pub enum TryFromValueError {
    #[error("Could not parse value to integer")]
    TryFromInt(#[from] std::num::TryFromIntError),

    #[error("Unsupported value: {0:?}")]
    UnsupportedValue(String),
}

#[inline(always)]
fn try_from_value_isize_impl<'a>(
    value: &Value<'a>,
) -> Result<Option<isize>, std::num::TryFromIntError> {
    match *value {
        Value::I8(v) => isize::try_from(v).map_err(|e| e.into()),
        Value::U8(v) => isize::try_from(v).map_err(|e| e.into()),
        Value::I16(v) => isize::try_from(v).map_err(|e| e.into()),
        Value::U16(v) => isize::try_from(v).map_err(|e| e.into()),
        Value::I32(v) => isize::try_from(v).map_err(|e| e.into()),
        Value::U32(v) => isize::try_from(v).map_err(|e| e.into()),
        Value::I64(v) => isize::try_from(v).map_err(|e| e.into()),
        Value::U64(v) => isize::try_from(v).map_err(|e| e.into()),
        _ => return Ok(None),
    }
    .map(|x| Some(x))
}

impl<'a> TryFrom<Value<'a>> for isize {
    type Error = TryFromValueError;

    fn try_from(value: Value<'a>) -> Result<Self, Self::Error> {
        match try_from_value_isize_impl(&value) {
            Ok(Some(v)) => Ok(v),
            Ok(None) => Err(TryFromValueError::UnsupportedValue(format!("{:?}", &value))),
            Err(e) => Err(e.into()),
        }
    }
}

impl<'s, 'b> Parser<'s, 'b> {
    pub fn new<S: Into<Cow<'s, Schema>>, B: Into<Cow<'b, [u8]>>>(
        schema: S,
        buf: B,
    ) -> Result<Parser<'s, 'b>, WalkerInitError> {
        let buf = buf.into();
        let schema = schema.into();

        let buf_len = i32::try_from(buf.len()).map_err(|_| WalkerInitError::BufferTooLarge {
            offset_size: std::mem::size_of::<ioffset>(),
            len: buf.len(),
            max_len: ioffset::max_value() as usize,
        })? as u32;

        Ok(Parser {
            schema,
            buf,
            buf_len,
        })
    }

    pub fn walk(&self) -> Result<Value, WalkError> {
        let (root_offset, root_type) = self.read_root_offset()?;
        println!("root offset: {:?}, type: {:?}", &root_offset, &root_type);

        let file_identifier = self.read_file_identifier()?;
        println!("file identifier: {:?}", &file_identifier);

        let schema_table = match root_type {
            SchemaType::Table(table) => table,
            t => return Err(WalkError::CannotHandleRootType(t.clone())),
        };

        println!("schema table: {:?}", &schema_table);

        let root_table = Table::new(root_offset, &schema_table, &*self.buf).unwrap();

        // let i = ident("foo").unwrap();

        // let value = root_table.get(&i).unwrap().unwrap();

        Ok(Value::Table(Some(root_table)))
    }

    fn read_root_offset(&self) -> Result<(uoffset, &SchemaType), WalkError> {
        if self.buf.len() < 4 {
            return Err(WalkError::DataTooShort(self.buf.len()));
        }
        let root_type = self.schema.root_type()?;
        let s = &self.buf[0..4];
        Ok((uoffset::from_le_bytes(s.try_into().unwrap()), root_type))
    }

    fn read_file_identifier(&self) -> Result<Option<[u8; 4]>, WalkError> {
        let expected = match self.schema.file_identifier.as_ref() {
            None => return Ok(None),
            Some(v) => v.0,
        };

        if self.buf.len() < 8 {
            return Err(WalkError::DataTooShort(self.buf.len()));
        }

        let found = self.buf[4..8].try_into().unwrap();

        if found != expected {
            return Err(WalkError::InvalidFileIdentifier { expected, found });
        }

        Ok(Some(found))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn smoke() {}
}
