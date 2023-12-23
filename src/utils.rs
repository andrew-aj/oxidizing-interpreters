use std::cell::RefCell;
use std::rc::Rc;

pub type Ref<T> = Rc<RefCell<T>>;

pub trait RefCreate<T> {
    fn create(data: T) -> Ref<T>;
}

impl<T: Clone> RefCreate<T> for Ref<T> {
    fn create(data: T) -> Ref<T> {
        Rc::new(RefCell::new(data.clone()))
    }
}
