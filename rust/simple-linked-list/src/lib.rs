struct Node<T> {
    value: T,
    next: Option<Box<Node<T>>>,
}

pub struct SimpleLinkedList<T> {
    head: Option<Box<Node<T>>>,
    length: usize,
}

impl<T> SimpleLinkedList<T> {
    pub fn new() -> Self {
        Self {
            head: None,
            length: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn push(&mut self, _element: T) {
        self.head = Some(Box::new(Node {
            value: _element,
            next: self.head.take(),
        }));
        self.length += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        match self.head.take() {
            Some(node) => {
                self.head = node.next;
                self.length -= 1;
                Some(node.value)
            }
            None => None,
        }
    }

    pub fn peek(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.value)
    }
}

impl<T: Clone> SimpleLinkedList<T> {
    pub fn rev(&self) -> SimpleLinkedList<T> {
        let mut linked_list = Self::new();
        let mut current_node = &self.head;
        while let Some(node) = current_node {
            linked_list.push(node.value.clone());
            current_node = &node.next;
        }
        linked_list
    }
}

impl<'a, T: Clone> From<&'a [T]> for SimpleLinkedList<T> {
    fn from(_item: &[T]) -> Self {
        let mut linked_list = Self::new();
        for node in _item {
            linked_list.push(node.clone())
        }
        linked_list
    }
}

impl<T> Into<Vec<T>> for SimpleLinkedList<T> {
    fn into(self) -> Vec<T> {
        let mut nodes = vec![];
        let mut current_node = self;
        while let Some(node) = current_node.pop() {
            nodes.insert(0, node);
        }
        nodes
    }
}
