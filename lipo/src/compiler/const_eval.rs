use fxhash::FxHashMap as HashMap;
use petgraph::graph::{Graph, NodeIndex};
use petgraph::Directed;

use crate::name::Name;
use crate::value::Value;

pub struct ConstEval<'alloc> {
    graph: Graph<Name<'alloc>, (), Directed, u32>,
    resolved: HashMap<ConstId, Value<'alloc>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ConstId(NodeIndex<u32>);

impl<'alloc> ConstEval<'alloc> {
    pub fn new() -> Self {
        ConstEval {
            graph: Graph::new(),
            resolved: HashMap::default(),
        }
    }

    pub fn resolve(&self, id: ConstId) -> Value<'alloc> {
        *self.resolved.get(&id).expect("BUG: unresolved constant")
    }

    pub fn add_const(&mut self, name: Name<'alloc>) -> ConstId {
        let id = self.graph.add_node(name);
        ConstId(id)
    }

    pub fn set_const_value(&mut self, id: ConstId, value: Value<'alloc>) {
        if self.resolved.insert(id, value).is_some() {
            unreachable!("BUG: Const value resolved multiple times");
        }
    }

    pub fn add_const_value(&mut self, name: Name<'alloc>, value: Value<'alloc>) -> ConstId {
        let id = self.add_const(name);
        self.set_const_value(id, value);
        id
    }

    pub fn dependency(&mut self, depends: ConstId, on: ConstId) {
        self.graph.add_edge(on.0, depends.0, ());
    }

    pub fn resolve_all(&mut self) {
        // todo
    }

    pub fn print(&mut self) {
        use petgraph::dot::{Config, Dot};

        let dot = Dot::with_config(&self.graph, &[Config::EdgeNoLabel]);
        println!("{dot:?}");
    }
}
