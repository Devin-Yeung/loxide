#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
}

pub enum OpCode {
    Return,
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        match self {
            OpCode::Return => 0,
        }
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
