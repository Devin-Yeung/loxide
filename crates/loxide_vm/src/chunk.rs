#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
}

pub enum OpCode {
    Return,
}

impl From<OpCode> for u8 {
    fn from(val: OpCode) -> Self {
        match val {
            OpCode::Return => 0,
        }
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk { code: Vec::new() }
    }

    pub fn write(&mut self, op_code: OpCode) {
        self.code.push(op_code.into());
    }
}
