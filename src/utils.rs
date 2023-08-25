use std::rc::Rc;
use std::cell::RefCell;

pub type Ref<T> = Rc<RefCell<T>>;
