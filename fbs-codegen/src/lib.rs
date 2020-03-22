use std::path::Path;

use fbs_parser::{parse_path, Schema};

pub struct Generator {
    schemas: Vec<Schema>,
}

#[derive(Debug, thiserror::Error)]
pub enum GenerateError {
    
}

impl Generator {
    pub fn new(path: &Path) -> Result<Generator, fbs_parser::Error> {
        let schemas = parse_path(path)?;
        Ok(Generator { schemas })
    }

    pub fn generate(&mut self) -> Result<(), GenerateError> {
        Ok(())
    } 
}
