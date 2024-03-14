use crate::chunk::Chunk;
use crate::utils::Ref;
use crate::utils::RefCreate;

use std::collections::HashMap;
use std::fmt;

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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Boolean(x) => write!(f, "{}", x),
            Value::Number(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "{}", *x.borrow()),
            Value::Nil => write!(f, "nil"),
            Value::BoundMethod(x) => write!(f, "{}", *x.borrow()),
            Value::Class(x) => write!(f, "{}", *x.borrow()),
            Value::Closure(x) => write!(f, "{}", *x.borrow()),
            Value::Function(x) => write!(f, "{}", *x.borrow()),
            Value::Instance(x) => write!(f, "{}", *x.borrow()),
            Value::Native(x) => write!(f, "{}", *x.borrow()),
        }
    }
}

pub struct BoundMethod {
    receiver: Ref<Value>,
    method: Ref<Closure>,
}

impl fmt::Display for BoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", *self.method.borrow())
    }
}

pub struct Class {
    name: Ref<String>,
    methods: HashMap<String, Ref<Value>>,
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", *self.name.borrow())
    }
}

#[derive(Clone, Default)]
pub struct Closure {
    pub function: Ref<Function>,
    pub upvalues: Vec<Ref<Upvalue>>,
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let func = self.function.borrow();
        write!(f, "{}", *func)
    }
}

#[derive(Clone, Default)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Ref<String>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = self.name.borrow();
        if *name == "" {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {}>", *name)
        }
    }
}

pub struct Instance {
    class: Ref<Class>,
    fields: HashMap<String, Ref<Value>>,
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} instance", *self.class.borrow())
    }
}

type NativeFunction = fn(&[Ref<Value>]) -> Result<Ref<Value>, String>;

pub struct Native {
    function: NativeFunction,
}

impl fmt::Display for Native {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

pub enum Upvalue {
    Open(Ref<Value>),
    Closed(Value),
}

impl fmt::Display for Upvalue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "upvalue")
    }
}
