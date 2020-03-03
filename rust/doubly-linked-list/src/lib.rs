mod pre_implemented;

use std::ptr::NonNull;

type MutNode<T> = Option<NonNull<Node<T>>>;

struct Node<T> {
    next: MutNode<T>,
    prev: MutNode<T>,
    data: T,
}

impl<T> Node<T> {
    fn new(data: T) -> Self {
        Self {
            data,
            next: None,
            prev: None,
        }
    }
}

pub struct LinkedList<T> {
    front: MutNode<T>,
    back: MutNode<T>,
    length: usize,
}

impl<T> LinkedList<T> {
    pub fn new() -> Self {
        Self {
            front: None,
            back: None,
            length: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn cursor_front(&mut self) -> Cursor<'_, T> {
        let node = self.front;
        Cursor { list: self, node }
    }

    pub fn cursor_back(&mut self) -> Cursor<'_, T> {
        let node = self.back;
        Cursor { list: self, node }
    }

    pub fn iter(&self) -> Iter<'_, T> {
        let node = self.front;
        Iter { list: self, node }
    }
}

pub struct Cursor<'a, T> {
    list: &'a mut LinkedList<T>,
    node: MutNode<T>,
}

impl<T> Cursor<'_, T> {
    pub fn peek_mut(&mut self) -> Option<&mut T> {
        unsafe { self.node.map(|node| &mut (*node.as_ptr()).data) }
    }

    pub fn next(&mut self) -> Option<&mut T> {
        unsafe { self.node = self.node.and_then(|node| node.as_ref().next) };
        self.peek_mut()
    }

    pub fn prev(&mut self) -> Option<&mut T> {
        unsafe { self.node = self.node.and_then(|node| node.as_ref().prev) };
        self.peek_mut()
    }

    pub fn take(&mut self) -> Option<T> {
        if self.list.length == 0 {
            return None;
        }
        self.list.length -= 1;

        unsafe {
            let node = self.node.unwrap();
            let next;
            if let Some(mut n) = node.as_ref().next {
                n.as_mut().prev = node.as_ref().prev;
                next = node.as_ref().next;
            } else if let Some(mut n) = node.as_ref().prev {
                n.as_mut().next = node.as_ref().next;
                next = node.as_ref().prev;
            } else {
                return None;
            }
            if self.node == self.list.front {
                self.list.front = node.as_ref().next;
            } else if self.node == self.list.back {
                self.list.back = node.as_ref().prev;
            }
            let data = Box::from_raw(self.node.take().unwrap().as_ptr()).data;
            self.node = next;
            Some(data)
        }
    }

    pub fn insert_after(&mut self, _element: T) {
        unimplemented!()
    }

    pub fn insert_before(&mut self, _element: T) {
        unimplemented!()
    }
}

pub struct Iter<'a, T> {
    list: &'a LinkedList<T>,
    node: MutNode<T>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        unsafe {
            self.node.map(|node| {
                self.node = node.as_ref().next;
                &(*node.as_ptr()).data
            })
        }
    }
}
