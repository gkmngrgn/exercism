#[derive(Debug)]
pub struct CustomSet<T> {
    list: Vec<T>,
}

impl<T: Clone + PartialEq> CustomSet<T> {
    pub fn new(input: &[T]) -> Self {
        let list = input.to_vec();
        Self { list: list }
    }

    pub fn contains(&self, element: &T) -> bool {
        self.list.contains(element)
    }

    pub fn add(&mut self, element: T) {
        if !self.list.contains(&element) {
            self.list.push(element);
        }
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        let len = self.list.len();
        len == 0 || other.list.windows(len).any(|w| w == self.list.as_slice())
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.is_empty() || other.is_empty() || self.list.iter().all(|i| !other.contains(i))
    }

    pub fn intersection(&self, other: &Self) -> Self {
        let list: Vec<T> = self
            .list
            .iter()
            .cloned()
            .filter(|i| other.contains(i))
            .collect();
        Self { list: list }
    }

    pub fn difference(&self, other: &Self) -> Self {
        let list: Vec<T> = self
            .list
            .iter()
            .cloned()
            .filter(|i| !other.contains(i))
            .collect();
        Self { list: list }
    }

    pub fn union(&self, other: &Self) -> Self {
        let list: Vec<T> = self.list.iter().cloned().collect();
        let mut custom_set = Self { list: list };
        for item in other.list.iter().cloned() {
            custom_set.add(item);
        }
        custom_set
    }
}

impl<T: PartialEq> PartialEq for CustomSet<T> {
    fn eq(&self, other: &Self) -> bool {
        self.list.len() == other.list.len() && self.list.iter().all(|i| other.list.contains(i))
    }
}
