pub mod graph_items;

use graph_items::edge::Edge;
use graph_items::node::Node;
use std::collections::HashMap;

pub struct Graph {
    pub nodes: Vec<Node>,
    pub edges: Vec<Edge>,
    pub attrs: HashMap<String, String>,
}

impl Graph {
    pub fn new() -> Self {
        Graph {
            nodes: vec![],
            edges: vec![],
            attrs: hashmap![],
        }
    }

    pub fn with_nodes(self, nodes: &[Node]) -> Self {
        Graph {
            nodes: Vec::from(nodes),
            ..self
        }
    }

    pub fn with_edges(self, edges: &[Edge]) -> Self {
        Graph {
            edges: Vec::from(edges),
            ..self
        }
    }

    pub fn with_attrs(self, attrs: &[(&str, &str)]) -> Self {
        Graph {
            attrs: attrs
                .iter()
                .map(|(k, v)| (String::from(*k), String::from(*v)))
                .collect(),
            ..self
        }
    }

    pub fn get_node(&self, name: &str) -> Option<&Node> {
        self.nodes.iter().find(|n| n.get_name() == name)
    }
}
