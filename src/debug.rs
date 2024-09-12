use crate::bytecode::OpCode;
use crate::chunk::Chunk;

type Instr = (OpCode, usize);

impl Chunk {
    pub fn disassemble_chunk(&self, name: &str) {
        println!("== {} ==", name);

        for code in &self.code {
            self.disassemble_instruction(&code);
        }
    }

    fn constant_instruction(&self, name: &str, constant: usize) {
        println!(
            "{:<16} {} '{}'",
            name, constant, self.constants[constant]
        );
    }

    fn invoke_instruction(&self, name: &str, constant: usize, args: usize) {
        println!(
            "{:<16} ({} args) {} '{}'",
            name, args, constant, self.constants[constant]
        )
    }

    fn simple_instruction(&self, name: &str) {
        println!("{}", name)
    }

    fn byte_instruction(&self, name: &str, slot: usize) {
        println!("{:<16} {}", name, slot)
    }

    fn jump_instruction(&self, name: &str, sign: i8, offset: usize) {
        println!("{:<16} -> {}", name, offset as isize * sign as isize)
    }

    fn disassemble_instruction(&self, code: &Instr) {
        print!("{:0>5} ", code.1);

        match &code.0 {
            OpCode::Constant(x) => self.constant_instruction("OP_CONSTANT", *x),
            OpCode::Nil => self.simple_instruction("OP_NIL"),
            OpCode::True => self.simple_instruction("OP_TRUE"),
            OpCode::False => self.simple_instruction("OP_FALSE"),
            OpCode::Pop => self.simple_instruction("OP_POP"),
            OpCode::GetLocal(x) => self.byte_instruction("OP_GET_LOCAL", *x),
            OpCode::SetLocal(x) => self.byte_instruction("OP_SET_LOCAL", *x),
            OpCode::GetGlobal(x) => self.constant_instruction("OP_GET_GLOBAL", *x),
            OpCode::DefineGlobal(x) => self.constant_instruction("OP_DEFINE_GLOBAL", *x),
            OpCode::SetGlobal(x) => self.constant_instruction("OP_SET_GLOBAL", *x),
            OpCode::GetUpvalue(x) => self.byte_instruction("OP_GET_UPVALUE", *x),
            OpCode::SetUpvalue(x) => self.byte_instruction("OP_SET_UPVALUE", *x),
            OpCode::GetProperty(x) => self.constant_instruction("OP_GET_PROPERTY", *x),
            OpCode::SetProperty(x) => self.constant_instruction("OP_SET_PROPERTY", *x),
            OpCode::GetSuper(x) => self.constant_instruction("OP_GET_SUPER", *x),
            OpCode::Equal => self.simple_instruction("OP_EQUAL"),
            OpCode::Greater => self.simple_instruction("OP_GREATER"),
            OpCode::Less => self.simple_instruction("OP_LESS"),
            OpCode::Add => self.simple_instruction("OP_ADD"),
            OpCode::Subtract => self.simple_instruction("OP_SUBTRACT"),
            OpCode::Multiply => self.simple_instruction("OP_MULTIPLY"),
            OpCode::Divide => self.simple_instruction("OP_DIVIDE"),
            OpCode::Not => self.simple_instruction("OP_NOT"),
            OpCode::Negate => self.simple_instruction("OP_NEGATE"),
            OpCode::Print => self.simple_instruction("OP_PRINT"),
            OpCode::Jump(x) => self.jump_instruction("OP_JUMP", 1, *x),
            OpCode::JumpIfFalse(x) => self.jump_instruction("OP_JUMP_IF_FALSE", 1, *x),
            OpCode::Loop(x) => self.jump_instruction("OP_LOOP", -1, *x),
            OpCode::Call(x) => self.byte_instruction("OP_CALL", *x as usize),
            OpCode::Invoke(name, args) => self.invoke_instruction("OP_INVOKE", *name, *args as usize),
            OpCode::SuperInvoke(name, args) => {
                self.invoke_instruction("OP_SUPER_INVOKE", *name, *args as usize)
            }
            OpCode::Closure(x, vec) => {}
            OpCode::CloseUpvalue => self.simple_instruction("OP_CLOSE_UPVALUE"),
            OpCode::Return => self.simple_instruction("OP_RETURN"),
            OpCode::Class(x) => self.constant_instruction("OP_CLASS", *x),
            OpCode::Inherit => self.simple_instruction("OP_INHERIT"),
            OpCode::Method(x) => self.constant_instruction("OP_METHOD", *x),
        };
    }
}
