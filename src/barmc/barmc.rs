use super::{
    badconfigs::BadConfigs,
    transitionsystem::{ConfigLangTrait, ConfigTrait, OperationIdTrait, TransitionSystem},
};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::collections::{BinaryHeap, HashSet};

pub enum BARMCResult<Config: ConfigTrait> {
    Closed,
    BadPath(Vec<Config>),
}

type NodeId = usize;

#[derive(Clone, Debug, Serialize, Deserialize)]
enum DependsOn<OperationId> {
    Nexts(Vec<(OperationId, NodeId)>),
    Abstract(Vec<NodeId>),
}

impl<OperationId: OperationIdTrait> DependsOn<OperationId> {
    fn get_dependent_nodes(&self) -> Vec<NodeId> {
        match self {
            DependsOn::Nexts(nexts) => nexts.iter().map(|(_, n)| *n).collect(),
            DependsOn::Abstract(nexts) => nexts.clone(),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Node<T: TransitionSystem> {
    id: NodeId,
    bad: Option<(T::Config, Option<NodeId>)>,
    seq: T::ConfigLang,
    depends_on: Option<DependsOn<T::OperationId>>,
    referenced_by: HashSet<NodeId>,
    abstracted: bool,
}

impl<T: TransitionSystem> Node<T> {
    fn new(id: NodeId, seq: T::ConfigLang) -> Self {
        Self {
            id,
            bad: None,
            seq,
            depends_on: None,
            abstracted: false,
            referenced_by: HashSet::new(),
        }
    }
    fn get_dependent_nodes(&self) -> Vec<NodeId> {
        match &self.depends_on {
            Some(dep) => dep.get_dependent_nodes(),
            None => vec![],
        }
    }
}

pub struct BARMC<T: TransitionSystem> {
    bad: BadConfigs<T::Config>,

    pub nodes: Vec<Node<T>>,

    reachable_frontier: BinaryHeap<(i32, NodeId)>, // (priority, id)
    pub reachable: HashSet<NodeId>,

    starts: HashSet<NodeId>,

    transition_system: T,
    abstractor:
        Box<dyn Fn(&T::ConfigLang, &dyn Fn(&T::ConfigLang) -> Option<T::Config>) -> T::ConfigLang>,
}

impl<'a, T: TransitionSystem> BARMC<T> {
    pub fn new(
        transition_system: T,
        abstractor: Box<
            dyn Fn(&T::ConfigLang, &dyn Fn(&T::ConfigLang) -> Option<T::Config>) -> T::ConfigLang,
        >,
    ) -> Self {
        let mut g = Self {
            bad: BadConfigs::new(),
            nodes: vec![],
            reachable_frontier: BinaryHeap::new(),
            reachable: HashSet::new(),
            starts: HashSet::new(),
            transition_system,
            abstractor,
        };
        for i in g.transition_system.init_states() {
            g.seq_to_node(i);
        }
        g.starts = g.nodes.iter().map(|n| n.id).collect();

        g.recompute_start_component();
        println!(
            "bad_states: {:?}\ninit_states: {:?}",
            g.bad.bads.len(),
            g.nodes.len(),
        );
        g
    }

    fn is_starts_bad(&self) -> bool {
        self.get_start_bad().is_some()
    }
    fn get_start_bad(&self) -> Option<&T::Config> {
        self.starts
            .iter()
            .find_map(|id| self.nodes[*id].bad.as_ref().map(|(c, _)| c))
    }

    fn find_abstraction_node_for(&self, node: &Node<T>) -> Option<NodeId> {
        let mut searched: HashSet<(usize, bool)> = HashSet::new();
        let mut que: Vec<(usize, bool)> = Vec::new();
        que.push((node.id, false));
        searched.insert((node.id, false));
        while !que.is_empty() {
            let (cur, next_rel) = que.pop().unwrap();
            if !self.reachable.contains(&cur) {
                continue;
            }
            let cur_node = self.get_node(cur).unwrap();
            if next_rel && cur_node.bad.is_none() {
                if cur_node.seq.includes(&node.seq) {
                    return Some(cur);
                }
            }
            for n in cur_node.referenced_by.iter() {
                let seen_next = if let Some(DependsOn::Nexts { .. }) = self.nodes[*n].depends_on {
                    true
                } else {
                    false
                };
                let next = (*n, seen_next);
                if !searched.contains(&next) {
                    searched.insert(next);
                    que.push(next);
                }
            }
        }

        for other_id in self
            .reachable
            .iter()
            .filter(|id| !searched.contains(&(**id, true)))
        {
            let other = self.get_node(*other_id).unwrap();
            if !other.bad.is_some() && node.id != other.id && other.seq.includes(&node.seq) {
                return Some(other.id);
            }
        }
        None
        //None
    }

    // まあ有用っぽいけど遅い
    fn find_concretize_node_for(&self, node: &Node<T>) -> Vec<NodeId> {
        return vec![];
        self.reachable_frontier
            .iter()
            .map(|(_, id)| id)
            .filter(|other_id| {
                let other = self.get_node(**other_id).unwrap();
                return !other.bad.is_some()
                    && node.id != other.id
                    && node.seq.includes(&other.seq);
            })
            .cloned()
            .collect_vec()
    }

    fn seq_to_node(&'a mut self, seq: T::ConfigLang) -> &'a Node<T> {
        let id = self.nodes.len();
        let node = Node::new(id, seq);
        self.add_node(node);
        //self.nodes[id].seq.debug_this(format!("{:?}", id).as_str());
        return self.get_node(id).unwrap();
    }

    fn get_bad_path(&self, id: NodeId) -> Vec<T::Config> {
        assert!(self.nodes[id].bad.is_some());

        let mut ret = vec![];
        let mut cur = id;
        loop {
            let (bad, bad_from) = self.nodes[cur].bad.clone().unwrap();
            ret.push(bad);
            if let Some(from) = bad_from {
                cur = from;
            } else {
                break;
            }
        }
        ret
    }

    fn is_reachable(&self, id: NodeId) -> bool {
        self.reachable.contains(&id)
    }

    fn add_node_to_start_component(&mut self, root_id: NodeId) {
        if self.reachable.contains(&root_id) {
            return;
        }
        self.reachable.insert(root_id);
        if self.get_node(root_id).unwrap().depends_on.is_none() {
            self.reachable_frontier
                .push((self.nodes[root_id].referenced_by.len() as i32, root_id));
        }
        for dep in self.get_node(root_id).unwrap().get_dependent_nodes() {
            self.add_node_to_start_component(dep);
        }
    }

    fn recompute_start_component(&mut self) {
        self.reachable.clear();
        self.reachable_frontier.clear();
        let starts = self.starts.clone();
        for s in starts {
            self.add_node_to_start_component(s);
        }
    }

    fn gen_nexts(&mut self, id: NodeId) -> DependsOn<T::OperationId> {
        println!("gen_nexts {:?}", id);
        let nexts: Vec<(<T as TransitionSystem>::OperationId, usize)> = self
            .transition_system
            .nexts(&self.nodes[id].seq)
            .into_iter()
            .map(|(opid, seq)| (opid.clone(), self.seq_to_node(seq).id))
            .collect();
        println!("nexts {:?}", nexts.len());
        DependsOn::Nexts(nexts)
    }

    fn one_step_concretize(&mut self, node_id: NodeId) {
        let new_dep = if let Some(abs) = self.find_abstraction_node_for(&self.nodes[node_id]) {
            DependsOn::Abstract(vec![abs])
        } else {
            if !self.nodes[node_id].abstracted {
                if let Some(dep) = self.create_abstraction(&self.nodes[node_id].seq.clone()) {
                    dep
                } else {
                    self.gen_nexts(node_id)
                }
            } else {
                self.gen_nexts(node_id)
            }
        };
        self.set_node_dependency(node_id, Some(new_dep));
    }
    fn add_bad(&mut self, conf: &T::Config) {
        if !self.bad.add_config(conf) {
            return;
        }
        let not_bad_nodes = self
            .nodes
            .iter()
            .filter(|n| n.bad.is_none())
            .map(|n| n.id)
            .collect_vec();
        not_bad_nodes.iter().for_each(|&id| {
            if self.nodes[id].seq.accept(&conf) {
                self.notify_node_is_bad(id, conf.clone(), None);
            }
        });
    }
    fn is_contains_bad(&self, seq: &T::ConfigLang) -> Option<T::Config> {
        let res = self.bad.is_contains_bad(seq);
        if res.is_some() {
            return res;
        }
        self.transition_system.find_bad_state(seq)
    }
    fn add_node(&mut self, node: Node<T>) {
        if self.get_node(node.id).is_some() {
            return;
        }
        let id = node.id;
        self.nodes.push(node);

        if self.nodes[id].depends_on.is_none() {
            self.add_node_to_start_component(id);
        }
        if let Some(badconfig) = self.is_contains_bad(&self.nodes[id].seq) {
            println!("reached bad {:?}", id);
            self.notify_node_is_bad(id, badconfig, None);
        } else {
            let concs: Vec<usize> = self.find_concretize_node_for(&self.nodes[id]);
            for conc in concs {
                self.set_node_dependency(conc, Some(DependsOn::Abstract(vec![id])));
            }
        }
    }
    fn pop_frontier_node(&'a mut self) -> Option<&'a Node<T>> {
        loop {
            let cur = self.reachable_frontier.pop();
            if cur.is_none() {
                return None;
            }
            let (_, id) = cur.unwrap();
            if self.nodes[id].depends_on.is_none() {
                return self.get_node(id);
            }
        }
    }
    fn get_node(&'a self, id: NodeId) -> Option<&'a Node<T>> {
        self.nodes.get(id)
    }

    fn set_node_dependency(&mut self, id: NodeId, depends_on: Option<DependsOn<T::OperationId>>) {
        if let Some(DependsOn::Nexts(nexts)) = depends_on.clone() {
            for (opid, next) in nexts {
                if let Some((nextbad, _)) = self.nodes[next].bad.clone() {
                    // assert!(!self.nodes[id].seq.accept(&nextbad));
                    // self.nodes[id].seq.debug_this("cur");
                    //assert!(self.nodes[next].seq.accept(&nextbad));
                    //self.nodes[next].seq.debug_this("nex");
                    let prev =
                        self.transition_system
                            .prev_within(&nextbad, &self.nodes[id].seq, opid);
                    if let Some(prev) = prev {
                        self.notify_node_is_bad(id, prev.clone(), Some(next));
                    } else {
                        self.set_node_dependency(id, None);
                    }
                    return;
                }
            }
        }

        for dep in self.nodes[id].get_dependent_nodes() {
            self.nodes[dep].referenced_by.remove(&id);
        }
        println!("set {:?} {:?}", id, depends_on);
        self.nodes[id].depends_on = depends_on;

        for dep in self.nodes[id].get_dependent_nodes() {
            self.nodes[dep].referenced_by.insert(id);
            self.add_node_to_start_component(dep);
        }
    }
    fn clear_node_dependency(&mut self, id: NodeId) {
        for dep in self.nodes[id].get_dependent_nodes() {
            self.nodes[dep].referenced_by.remove(&id);
        }
        self.nodes[id].depends_on = None;
        if self.is_reachable(id) {
            self.recompute_start_component();
        }
    }

    fn create_abstraction(&mut self, seq: &T::ConfigLang) -> Option<DependsOn<T::OperationId>> {
        let newseq = (self.abstractor)(seq, &|seq| self.is_contains_bad(seq));
        let newnodeid = self.seq_to_node(newseq).id;
        self.nodes[newnodeid].abstracted = true;
        if seq.is_equal(&self.nodes[newnodeid].seq) {
            return None;
        } else {
            assert!(*seq != self.nodes[newnodeid].seq);
            return Some(DependsOn::Abstract(vec![newnodeid]));
        }
    }

    // set and propagate
    fn notify_node_is_bad(&mut self, id: NodeId, badconfig: T::Config, bad_from: Option<NodeId>) {
        assert!(
            self.nodes[id].seq.accept(&badconfig),
            "{:?} {}",
            id,
            badconfig
        );
        println!("notify {:?} {:?}", id, badconfig);
        println!("referenced_by {:?}", self.nodes[id].referenced_by);

        if self.nodes[id].bad.is_some() {
            return;
        }
        self.nodes[id].bad = Some((badconfig.clone(), bad_from));
        self.add_bad(&badconfig);

        for ref_by in self.nodes[id].referenced_by.clone() {
            println!("check_ref {:?} {:?}", ref_by, badconfig);
            if self.nodes[ref_by].depends_on.is_none() {
                continue;
            }
            let depends_on = self.nodes[ref_by].depends_on.clone().unwrap();
            self.clear_node_dependency(ref_by);

            if self.nodes[ref_by].bad.is_some() {
                continue;
            }
            match depends_on {
                DependsOn::Nexts(nexts) => {
                    let opid = nexts
                        .iter()
                        .find(|(_, n)| *n == id)
                        .map(|(opid, _)| opid)
                        .unwrap()
                        .clone();
                    let prevconf = self.transition_system.prev_within(
                        &badconfig,
                        &self.nodes[ref_by].seq,
                        opid,
                    );
                    if let Some(prevconf) = prevconf {
                        self.notify_node_is_bad(ref_by, prevconf.clone(), Some(id));
                    } else {
                        self.set_node_dependency(ref_by, None);
                    }
                }
                DependsOn::Abstract(_) => {
                    if self.nodes[ref_by].seq.accept(&badconfig) {
                        self.notify_node_is_bad(ref_by, badconfig.clone(), Some(id));
                    }
                }
            }
        }
    }

    fn get_invariant(&mut self) -> Vec<Node<T>> {
        self.recompute_start_component();
        assert!(self
            .reachable
            .iter()
            .all(|&id| !self.nodes[id].bad.is_some() && self.nodes[id].depends_on.is_some()));

        self.reachable
            .iter()
            .map(|id| self.nodes[*id].clone())
            .collect()
    }

    fn find_most_abstract(&self, node: &Node<T>) -> NodeId {
        let mut current = node;
        loop {
            match &current.depends_on {
                Some(DependsOn::Abstract(ids)) => {
                    if ids.len() == 1 {
                        current = &self.nodes[ids[0]];
                    } else {
                        break;
                    }
                }
                _ => {
                    break;
                }
            }
        }
        current.id
    }

    fn get_graph_dot(&self) -> String {
        let mut ret = "digraph G {\n".to_string();
        for id in self.reachable.iter() {
            let node = &self.nodes[*id];
            ret += &format!(
                "{} [label=\"{},{}\", style=\"filled\", fillcolor=\"{}\"]\n",
                node.id,
                node.id,
                node.seq.get_dot_label(),
                if node.bad.is_some() {
                    "red"
                } else if node.depends_on.is_none() {
                    "yellow"
                } else {
                    "white"
                }
            );
            let is_abstract_dep = match &node.depends_on {
                Some(DependsOn::Abstract(_)) => true,
                _ => false,
            };
            for dep in node.get_dependent_nodes() {
                ret += &format!(
                    "{} -> {} [style=\"{}\"]\n",
                    node.id,
                    dep,
                    if is_abstract_dep { "dotted" } else { "solid" },
                );
            }
        }
        for start in self.starts.iter() {
            ret += &format!("start -> {} [style=\"solid\"]\n", start);
            ret += &format!("start [label=\"\", shape=\"point\"]\n");
        }
        ret += "}\n";
        ret
    }

    fn print_graph_dot(&self) {
        let dot = self.get_graph_dot();
        std::fs::write("graph.dot", dot).unwrap();
    }

    pub fn step(&mut self) -> bool {
        self.recompute_start_component();
        self.print_graph_dot();
        if let Some(id) = { self.pop_frontier_node().map(|n| n.id) } {
            println!("step {:?}", id);
            if let Some((bad, _)) = &self.nodes[id].bad {
                println!("front bad: {:?}", bad);
                self.notify_node_is_bad(id, bad.clone(), None);
            } else {
                println!("one_step_concretize {:?}", id);
                self.one_step_concretize(id);
            }
        }
        println!(
            "reachable: {:?} frontier: {:?}",
            self.reachable.len(),
            self.reachable_frontier.len(),
        );
        return self.reachable_frontier.is_empty() || self.is_starts_bad();
    }

    pub fn step_until_stop(&mut self) -> BARMCResult<T::Config> {
        while !self.step() {}
        self.recompute_start_component();
        self.print_graph_dot();

        if self.is_starts_bad() {
            for s in self.starts.iter() {
                if self.nodes[*s].bad.is_some() {
                    return BARMCResult::BadPath(self.get_bad_path(*s));
                }
            }
            panic!();
        } else {
            println!("closed");
            // save state as json
            let nodes: Vec<Node<T>> = self
                .reachable
                .iter()
                .map(|n| self.nodes[*n].clone())
                .collect_vec();
            serde_json::to_writer(std::fs::File::create("graph.json").unwrap(), &nodes).unwrap();

            return BARMCResult::Closed;
        }
    }
}
