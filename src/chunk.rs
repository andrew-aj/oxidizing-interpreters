use crate::bytecode::OpCode;
use crate::value;

pub struct Chunk {
    code: Vec<(OpCode, usize)>,
    constants: Vec<value::Value>,
}

impl Chunk {
    pub fn add_constant(&mut self, value: value::Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn write_chunk(&mut self, code: &(OpCode, usize)) {
        self.code.push(code.clone());
    }
}
