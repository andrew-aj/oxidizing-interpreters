use crate::bytecode::OpCode;
use crate::value;

#[derive(Default, Clone)]
pub struct Chunk {
    pub code: Vec<(OpCode, usize)>,
    pub constants: Vec<value::Value>,
}

impl Chunk {
    pub fn add_constant(&mut self, value: value::Value) -> usize {
        if let Some(index) = self.find_constant(value.clone()) {
            index
        } else {
            self.constants.push(value);
            self.constants.len() - 1
        }
    }

    pub fn find_constant(&mut self, value: value::Value) -> Option<usize> {
        if let value::Value::Number(val) = value {
            self.constants.iter().position(|i| {
                if let value::Value::Number(num) = i {
                    *num == val
                } else {
                    false
                }
            })
        } else if let value::Value::String(val) = value {
            self.constants.iter().position(|i| {
                if let value::Value::String(s) = i {
                    *s == val
                } else {
                    false
                }
            })
        } else {
            None
        }
    }

    pub fn write_chunk(&mut self, code: (OpCode, usize)) {
        self.code.push(code);
    }
}
