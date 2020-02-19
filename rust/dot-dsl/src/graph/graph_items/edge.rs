use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Edge {
    start: String,
    end: String,
    attrs: HashMap<String, String>,
}

impl Edge {
    pub fn new(start: &str, end: &str) -> Edge {
        Edge {
            start: String::from(start),
            end: String::from(end),
            attrs: hashmap![],
        }
    }

    pub fn with_attrs(self, attrs: &[(&str, &str)]) -> Self {
        Edge {
            attrs: attrs
                .iter()
                .map(|(k, v)| (String::from(*k), String::from(*v)))
                .collect(),
            ..self
        }
    }
}
