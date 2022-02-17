use fxhash::FxHashMap as HashMap;
use petgraph::graph::{Graph, NodeIndex};
use petgraph::{algo, Directed};

use crate::builtins::Function;
use crate::name::Name;
use crate::value::Value;
use crate::{Alloc, ObjectRef, VM};

#[rustfmt::skip]
// TODO we have to differentiate between runtime dependencies and const_eval
// dependencies. for now all dependencies are considered cons_eval dependencies and mutually
// recursive functions will fail to compile because of a dependency cycle which doesn't actually
// exist until runtime (when it's fine).
//
// 1. const_eval dependency
//     ```
//     const a = ();
//     const b = a;
//     ```
//
// 2. runtime dependency
//     ```
//     const a = ();
//     fn b() { a }
//     ```
//
// 3. also runtime dependency, equivalent to 2.
//     ```
//     const a = ();
//     const b = fn() a;
//     ```
//
// 4. a runtime dependency promoted to a const_eval dependency
//     ```
//     const a = ();
//     const b = fn() a;
//     const c = b();
//     ```
pub struct ConstEval<'alloc> {
    /// dependency graph between constants
    graph: Graph<Name<'alloc>, (), Directed, u32>,
    /// constants which were immediately resolved to a Value or after their code has been evaluated
    resolved: HashMap<ConstId, Value<'alloc>>,
    /// compiled code that evaluates to a constant with a function to map the return value
    code: HashMap<ConstId, (ObjectRef<'alloc, Function<'alloc>>, ValueMap<'alloc>)>,
}

type ValueMap<'alloc> = Box<dyn FnOnce(Value<'alloc>, &Alloc<'_, 'alloc>) -> Value<'alloc>>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ConstId(NodeIndex<u32>);

impl<'alloc> ConstEval<'alloc> {
    pub fn new() -> Self {
        ConstEval {
            graph: Graph::new(),
            resolved: HashMap::default(),
            code: HashMap::default(),
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
            unreachable!("BUG: Const value sete multiple times");
        }
    }

    pub fn set_const_code(&mut self, id: ConstId, code: ObjectRef<'alloc, Function<'alloc>>) {
        self.set_const_code_map(id, code, |v, _| v)
    }

    pub fn set_const_code_map(
        &mut self,
        id: ConstId,
        code: ObjectRef<'alloc, Function<'alloc>>,
        f: impl FnOnce(Value<'alloc>, &Alloc<'_, 'alloc>) -> Value<'alloc> + 'static,
    ) {
        if self.code.insert(id, (code, Box::new(f))).is_some() {
            unreachable!("BUG: Const code set multiple times");
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

    pub fn resolve_all(&mut self, alloc: &Alloc<'_, 'alloc>) {
        self.print_dot();

        let toposort = algo::toposort(&self.graph, None)
            .expect("const_eval error: dependency cycle in constants detected");

        for id in toposort.into_iter().map(ConstId) {
            if self.resolved.contains_key(&id) {
                continue;
            }
            let Some((func, map)) = self.code.remove(&id) else {
                eprintln!("const_eval error: constant {id:?} doesn't have either a value or code associated");
                continue;
            };
            func.chunk.resolve_constants(&*self);
            let vm = VM::new(func, alloc);
            let value = vm.run().expect("const_eval error");
            let value = map(value, alloc);
            self.resolved.insert(id, value);
        }
    }

    fn print_dot(&mut self) {
        use petgraph::dot::{Config, Dot};

        let dot = Dot::with_config(&self.graph, &[Config::EdgeNoLabel]);
        println!("{dot:?}");
    }
}
