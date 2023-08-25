use crate::utils::Ref;
use crate::chunk::Chunk;

use std::collections::HashMap;

pub enum Value {
    Boolean(bool),
    Number(f64),
    BoundMethod(Ref<BoundMethod>),
    Class(Ref<Class>),
    Closure(Ref<Closure>),
    Function(Ref<Function>),
    Instance(Ref<Instance>),
    Native(Ref<Native>),
    String(Ref<String>),
    Nil,
}

pub struct BoundMethod {
    receiver: Ref<Value>,
    method: Ref<Closure>,
}

pub struct Class {
    name: Ref<String>,
    methods: HashMap<String, Ref<Value>>,
}

pub struct Closure {
    function: Ref<Function>,
    upvalues: Vec<Ref<Upvalue>>,
}

pub struct Function {
    arity: u8,
    chunk: Chunk,
    name: Ref<String>,
}

pub struct Instance {
    class: Ref<Class>,
    fields: HashMap<String, Ref<Value>>,
}

type NativeFunction = fn(&[Ref<Value>]) -> Result<Ref<Value>, String>;

pub struct Native {
    function: NativeFunction,
}

pub enum Upvalue {
    Open(Ref<Value>),
    Closed(Value),
}
