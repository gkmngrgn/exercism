use std::marker::PhantomData;
use std::ptr::null_mut;

mod pre_implemented;

struct Node<T> {
    next: *mut Node<T>,
    prev: *mut Node<T>,
    element: T,
}

pub struct LinkedList<T> {
    first: *mut Node<T>,
    last: *mut Node<T>,
    length: usize,
}

pub struct Cursor<'a, T> {
    list: &'a mut LinkedList<T>,
    node: *mut Node<T>,
}

pub struct Iter<'a, T> {
    lifetime: PhantomData<&'a T>,
    node: *const Node<T>,
}

impl<T> LinkedList<T> {
    pub fn new() -> Self {
        Self {
            first: null_mut(),
            last: null_mut(),
            length: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn cursor_front(&mut self) -> Cursor<'_, T> {
        let node = self.first;
        Cursor { list: self, node }
    }

    pub fn cursor_back(&mut self) -> Cursor<'_, T> {
        let node = self.last;
        Cursor { list: self, node }
    }

    pub fn iter(&self) -> Iter<'_, T> {
        let node = self.first;
        Iter {
            lifetime: PhantomData,
            node,
        }
    }
}

impl<T> Drop for LinkedList<T> {
    fn drop(&mut self) {
        loop {
            if let None = self.cursor_front().take() {
                break;
            }
        }
    }
}

impl<T> Cursor<'_, T> {
    pub fn peek_mut(&mut self) -> Option<&mut T> {
        unsafe {
            if self.node.is_null() {
                return None;
            }
            Some(&mut (*self.node).element)
        }
    }

    pub fn next(&mut self) -> Option<&mut T> {
        unsafe {
            if (*self.node).next.is_null() {
                return None;
            }
            let element = &mut (*(*self.node).next).element;
            self.node = (*self.node).next;
            Some(element)
        }
    }

    pub fn prev(&mut self) -> Option<&mut T> {
        unsafe {
            if (*self.node).prev.is_null() {
                return None;
            }
            let element = &mut (*(*self.node).prev).element;
            self.node = (*self.node).prev;
            Some(element)
        }
    }

    pub fn take(&mut self) -> Option<T> {
        unsafe {
            if self.node.is_null() {
                return None;
            }
            let node = Box::from_raw(self.node);
            if node.prev.is_null() {
                if node.next.is_null() {
                    self.node = null_mut();
                    self.list.first = null_mut();
                    self.list.last = null_mut();
                } else {
                    self.node = node.next;
                    self.list.first = node.next;
                    (*node.next).prev = null_mut();
                }
            } else {
                if node.next.is_null() {
                    self.node = node.prev;
                    self.list.last = node.prev;
                    (*node.prev).next = null_mut();
                } else {
                    self.node = node.next;
                    (*node.prev).next = node.next;
                    (*node.next).prev = node.prev;
                }
            }
            self.list.length -= 1;
            Some(node.element)
        }
    }

    pub fn insert_after(&mut self, element: T) {
        unsafe {
            if self.list.first.is_null() {
                let node = Box::into_raw(Box::new(Node {
                    next: null_mut(),
                    prev: null_mut(),
                    element,
                }));
                self.list.first = node;
                self.list.last = node;
            } else {
                let node = Box::into_raw(Box::new(Node {
                    next: (*self.node).next,
                    prev: self.node,
                    element,
                }));
                if !(*node).next.is_null() {
                    (*(*node).next).prev = node;
                } else {
                    self.list.last = node;
                }
                (*(*node).prev).next = node;
            }
            self.list.length += 1;
        }
    }

    pub fn insert_before(&mut self, element: T) {
        unsafe {
            if self.list.first.is_null() {
                let node = Box::into_raw(Box::new(Node {
                    next: null_mut(),
                    prev: null_mut(),
                    element,
                }));
                self.list.first = node;
                self.list.last = node;
            } else {
                let node = Box::into_raw(Box::new(Node {
                    next: self.node,
                    prev: (*self.node).prev,
                    element,
                }));
                (*(*node).next).prev = node;
                if !(*node).prev.is_null() {
                    (*(*node).prev).next = node;
                } else {
                    self.list.first = node;
                }
            }
            self.list.length += 1;
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        unsafe {
            if self.node.is_null() {
                return None;
            }
            let element = &(*self.node).element;
            self.node = (*self.node).next;
            Some(element)
        }
    }
}
