use fbs_parser::{
    Enum, EnumValue, Field, Ident, Schema, SchemaType, Struct, Table, Union, UnionValue,
};
use indexmap::IndexMap;
use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{prelude::*, Cursor};

#[derive(Debug)]
pub struct UnionItem<'s> {
    pub schema: &'s Union,
    pub repr: &'s UnionValue,
    pub value: Option<Box<Value<'s>>>,
}

#[derive(Debug)]
pub struct EnumItem<'s> {
    pub schema: &'s Enum,
    pub value: &'s EnumValue,
}

#[derive(Debug)]
pub struct TableItem<'s> {
    pub schema: &'s Table,
    pub values: Option<IndexMap<&'s Field, Value<'s>>>,
}

#[derive(Debug, thiserror::Error)]
enum BytesError {
    #[error("vtable too large")]
    VTableTooLarge,
}

impl<'s> TableItem<'s> {
    fn to_bytes<W: Write, X: Write>(
        &self,
        refs: &mut HashMap<&'s Field, uoffset>,
        main: &mut W,
        heap: &mut X,
    ) -> Result<usize, ()> {
        let values = match self.values.as_ref() {
            Some(v) => v,
            None => return vec![],
        };

        // vtable fields
        let mut vtable_len_in_bytes = 0u16;
        let mut vtable_table_len_in_bytes = 0u16;
        let mut vtable_data: Vec<u16> = vec![0; self.schema.fields.len()];

        let mut table_vtable_offset: i32 = 0;
        let mut table = vec![];

        let mut max_id = 0u16;
        let mut cur = 0usize;

        for (id, field) in self.schema.ids() {
            let value = match values.get(field) {
                Some(v) => v,
                None => continue,
            };

            max_id = std::cmp::max(max_id, id);

            if field.ty.is_union() {
                // TODO, id - 1 and crimes
            }

            v.to_bytes()

            // vtable_data[id] = ;
        }

        // Truncate the vtable to the last id that had an actual value specified
        vtable_data.truncate()
    }
}

#[derive(Debug)]
pub struct StructItem<'s> {
    pub schema: &'s Struct,
    pub values: Option<IndexMap<&'s Field, Value<'s>>>,
}

#[derive(Debug)]
pub struct VectorItem<'s> {
    pub schema: &'s SchemaType,
    pub values: Option<Vec<Value<'s>>>,
}

#[derive(Debug)]
pub struct UnionVectorItem<'s> {
    pub schema: &'s Union,
    pub values: Option<Vec<(&'s UnionValue, Value<'s>)>>,
}

#[derive(Debug)]
pub enum Value<'s> {
    Table(TableItem<'s>),
    Struct(StructItem<'s>),
    Enum(EnumItem<'s>),
    Union(UnionItem<'s>),
    Vector(VectorItem<'s>),
    UnionVector(UnionVectorItem<'s>),
    String(Option<String>),
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

impl<'s> Value<'s> {
    #[inline(always)]
    pub fn make_table(schema: &'s Table, values: IndexMap<&'s Field, Value<'s>>) -> Value<'s> {
        Value::Table(TableItem {
            schema,
            values: Some(values),
        })
    }

    #[inline(always)]
    pub fn make_struct(schema: &'s Struct, values: IndexMap<&'s Field, Value<'s>>) -> Value<'s> {
        Value::Struct(StructItem {
            schema,
            values: Some(values),
        })
    }

    #[inline(always)]
    pub fn make_enum(schema: &'s Enum, value: &'s EnumValue) -> Value<'s> {
        Value::Enum(EnumItem { schema, value })
    }

    #[inline(always)]
    pub fn make_union(schema: &'s Union, repr: &'s UnionValue, value: Value<'s>) -> Value<'s> {
        Value::Union(UnionItem {
            schema,
            repr,
            value: Some(Box::new(value)),
        })
    }

    #[inline(always)]
    pub fn make_vector(schema: &'s SchemaType, values: Vec<Value<'s>>) -> Value<'s> {
        Value::Vector(VectorItem {
            schema,
            values: Some(values),
        })
    }

    #[inline(always)]
    pub fn make_union_vector(
        schema: &'s Union,
        values: Vec<(&'s UnionValue, Value<'s>)>,
    ) -> Value<'s> {
        Value::UnionVector(UnionVectorItem {
            schema,
            values: Some(values),
        })
    }
}

pub struct Builder<'s> {
    schema: Cow<'s, Schema>,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Write error")]
    WriteError(#[from] std::io::Error),
}

use crate::types::{ioffset, uoffset, voffset};
use byteorder::{LittleEndian, WriteBytesExt};

impl<'s> Builder<'s> {
    pub fn new<S: Into<Cow<'s, Schema>>>(schema: S) -> Builder<'s> {
        let schema = schema.into();
        Builder { schema }
    }

    // #[inline]
    // fn to_writer_inner<W: Write>(&self, writer: &mut W, value: &Value<'s>) -> Result<usize, Error> {

    // }

    pub fn to_writer<W: Write + Seek>(
        &self,
        mut writer: W,
        value: &Value<'s>,
    ) -> Result<(), Error> {
        // Resolve root type or bail
        let root_type = self.schema.root_type().unwrap();
        let root_table = root_type.to_table().unwrap();

        // Check for file identifier
        let file_id = self.schema.file_identifier.as_ref();

        match file_id {
            Some(file_id) => {
                // root table offset
                writer.write_u32::<LittleEndian>(8)?;

                // file id
                writer.write(&file_id.0)?;
            }
            None => {
                // root table offset
                writer.write_u32::<LittleEndian>(4)?;
            }
        };

        Ok(())

        // Self::to_writer_inner(&mut writer, value)
    }

    pub fn to_vec(&self, value: &Value<'s>) -> Result<Vec<u8>, Error> {
        let mut out = Cursor::new(Vec::with_capacity(1024));
        self.to_writer(&mut out, value)?;
        Ok(out.into_inner())
    }
}

// #[cfg(test)]
// mod test {
//     use super::*;

//     #[test]
//     fn smoke() {
//         Builder::new()
//     }
// }
