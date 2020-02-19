use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    name: String,
    attrs: HashMap<String, String>,
}

impl Node {
    pub fn new(name: &str) -> Node {
        Node {
            name: String::from(name),
            attrs: hashmap![],
        }
    }

    pub fn with_attrs(self, attrs: &[(&str, &str)]) -> Self {
        Node {
            attrs: attrs
                .iter()
                .map(|(k, v)| (String::from(*k), String::from(*v)))
                .collect(),
            ..self
        }
    }

    pub fn get_attr(&self, attr: &str) -> Option<&str> {
        self.attrs.get(attr).map(String::as_str)
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
}
