use std::collections::VecDeque;

pub struct CircularBuffer<T> {
    capacity: usize,
    field: VecDeque<T>,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    EmptyBuffer,
    FullBuffer,
}

impl<T> CircularBuffer<T> {
    pub fn new(capacity: usize) -> Self {
        Self {
            capacity: capacity,
            field: VecDeque::with_capacity(capacity),
        }
    }

    pub fn write(&mut self, element: T) -> Result<(), Error> {
        if self.field.len() == self.capacity {
            return Err(Error::FullBuffer);
        }
        self.field.push_back(element);
        Ok(())
    }

    pub fn read(&mut self) -> Result<T, Error> {
        if self.field.len() == 0 {
            return Err(Error::EmptyBuffer);
        }
        Ok(self.field.pop_front().unwrap())
    }

    pub fn clear(&mut self) {
        self.field.clear();
    }

    pub fn overwrite(&mut self, element: T) {
        if self.field.len() == self.capacity {
            self.field.pop_front();
        }
        self.write(element).unwrap()
    }
}
