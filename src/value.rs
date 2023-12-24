use crate::chunk::Chunk;
use crate::utils::Ref;
use crate::utils::RefCreate;

use std::collections::HashMap;

#[derive(Clone)]
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

impl Value {
    pub fn str_to_value(text: &str) -> Value {
        Value::String(Ref::create(text.to_string()))
    }

    pub fn num_to_value(num: f64) -> Value {
        Value::Number(num)
    }
}

pub struct BoundMethod {
    receiver: Ref<Value>,
    method: Ref<Closure>,
}

pub struct Class {
    name: Ref<String>,
    methods: HashMap<String, Ref<Value>>,
}

#[derive(Clone, Default)]
pub struct Closure {
    pub function: Ref<Function>,
    pub upvalues: Vec<Ref<Upvalue>>,
}

#[derive(Clone, Default)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Ref<String>,
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
