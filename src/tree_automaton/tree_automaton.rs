use itertools::Itertools;
use maplit::{hashmap, hashset};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet, VecDeque}, io, iter::once, sync::atomic::AtomicU32
};
use std::{fmt::Display, io::Write};

use crate::trs::{
    term::{GroundTerm, SymbolTrait, Term},
    trs::Fun,
};

pub type State = u32;

static mut STATE_COUNTER: AtomicU32 = AtomicU32::new(0);
pub fn new_state() -> State {
    unsafe { STATE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst) }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub struct Transition<F: SymbolTrait> {
    pub children: Vec<State>,
    pub parent: State,
    pub symbol: F,
    //EPSのとき、暗黙にchildrenは1だという仮定をおいてる。気持ち悪いかもしれないが、結局他のSymbolに関してもArityを型で制約はしてないので、まあ一緒じゃないか
}
impl<F: SymbolTrait> Transition<F> {
    pub fn new(children: Vec<State>, parent: State, symbol: F) -> Self {
        Self {
            children,
            parent,
            symbol,
        }
    }
}

pub fn create_wildcard_transitions<F: SymbolTrait>(
    state: State,
    symbols: &HashSet<Fun<F>>,
) -> Vec<Transition<F>> {
    symbols
        .iter()
        .map(|s| Transition {
            children: (0..s.arity).map(|_| state.clone()).collect(),
            parent: state,
            symbol: s.symbol.clone(),
        })
        .collect()
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TreeAutomaton<F: SymbolTrait> {
    pub states: HashSet<State>,
    pub top_states: HashSet<State>, // accepting states for bottom-up evaluation
    pub bottom_states: HashMap<State, HashSet<F>>, // accepting states for top-down evaluation
    pub transitions_topdown: HashMap<State, Vec<Transition<F>>>,
    pub transitions_bottomup: HashMap<State, Vec<(Transition<F>, usize)>>,
    /*
        仮想的に、EmptyTerm というものがあるとして、arity0のシンボルは、arity1の、EmptyTermだけを子にもつものとして解釈する.
        これによって、bottom_statesはただのSetで良い(EmptyTermに対応する状態が入る)
    */
}

impl<F: SymbolTrait + Display> TreeAutomaton<F> {
    pub fn to_dot(&self) -> String {
        // ハイパーエッジは、中間ノードを用意して表現する
        let mut buffer: io::Cursor<Vec<u8>> = io::Cursor::new(Vec::new());
        buffer.write_all(b"digraph G {\n").unwrap();
        for state in &self.states {
            let is_top = self.top_states.contains(state);
            let shape = if is_top { "doublecircle" } else { "circle" };
            write!(
                buffer,
                "{} [label=\"{}\", shape=\"{}\"];\n",
                state, state, shape
            )
            .unwrap();
        }
        for transition in self.transitions_topdown.values().flatten() {
            if transition.children.len() == 1 {
                write!(
                    buffer,
                    "{} -> {} [label=\"{}\"];\n",
                    transition.children[0], transition.parent, transition.symbol
                )
                .unwrap();
            } else {
                let intermidiate_node_id =
                    "___intermidiate_node__".to_string() + new_state().to_string().as_str();
                for (arg_idx, ch) in transition.children.iter().enumerate() {
                    write!(
                        buffer,
                        "{} -> {} [label=\"[{}]\"];\n",
                        ch, intermidiate_node_id, arg_idx
                    )
                    .unwrap();
                }
                write!(buffer, "{} [shape=point];\n", intermidiate_node_id).unwrap();
                write!(
                    buffer,
                    "{} -> {} [label=\"{}\"];\n",
                    intermidiate_node_id, transition.parent, transition.symbol
                )
                .unwrap();
            }
        }

        write!(buffer, "}}\n").unwrap();
        return String::from_utf8(buffer.into_inner()).unwrap();
    }

    pub fn save_dot(&self, path: &str) {
        use std::fs::File;
        let mut file = File::create(path).unwrap();
        file.write_all(self.to_dot().as_bytes()).unwrap();
    }
}

impl<F: SymbolTrait> TreeAutomaton<F> {
    pub fn empty() -> Self {
        Self {
            states: HashSet::new(),
            top_states: HashSet::new(),
            bottom_states: HashMap::new(),
            transitions_topdown: HashMap::new(),
            transitions_bottomup: HashMap::new(),
        }
    }
    pub fn new(transitions: Vec<Transition<F>>, top_states: HashSet<State>) -> Self {
        let transitions = transitions
            .into_iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        let mut states: HashSet<_> = top_states.clone();

        let mut bottom_states = HashMap::new();
        for transition in &transitions {
            if transition.children.len() == 0 {
                bottom_states
                    .entry(transition.parent)
                    .or_insert(HashSet::new())
                    .insert(transition.symbol.clone());
            }
        }

        let mut transitions_topdown = HashMap::new();
        let mut transitions_bottomup = HashMap::new();
        for transition in transitions {
            states.insert(transition.parent);
            transitions_topdown
                .entry(transition.parent)
                .or_insert(Vec::new())
                .push(transition.clone());
            for (i, child) in transition.children.iter().enumerate() {
                states.insert(*child);
                transitions_bottomup
                    .entry(*child)
                    .or_insert(Vec::new())
                    .push((transition.clone(), i));
            }
        }

        Self {
            states,
            top_states,
            bottom_states,
            transitions_topdown,
            transitions_bottomup,
        }
    }

    pub fn construct_singleton_aut_ground_term(ground_term: &GroundTerm<F>) -> Self {
        let term = ground_term.to_term_with_varmap(&hashmap! {});
        let funcs = ground_term.extract_funcs();
        Self::construct_singleton_aut_term(&term, &funcs)
    }

    // F(A x) xのところは任意のタームが入る感じの言語
    pub fn construct_singleton_aut_term(term: &Term<F>, funcs: &HashSet<Fun<F>>) -> Self {
        if let Term::Variable { .. } = term {
            return Self::universal_aut(funcs);
        }
        //assert!(term.is_linear());

        let mut transitions = vec![];
        let top_state = new_state();

        let mut stack = vec![(term, top_state)];

        let universal_state = new_state();
        let universal_transitions = create_wildcard_transitions(universal_state, funcs);
        transitions.extend(universal_transitions);

        while let Some((node, parent)) = stack.pop() {
            match node {
                Term::Variable { .. } => {}
                Term::Function { symbol, children } => {
                    if children.len() == 0 {
                        transitions.push(Transition {
                            children: vec![],
                            parent,
                            symbol: symbol.clone(),
                        });
                    } else {
                        let children_states = children
                            .iter()
                            .map(|c| match c {
                                Term::Variable { .. } => universal_state,
                                Term::Function { .. } => new_state(),
                            })
                            .collect::<Vec<_>>();
                        for (child, child_state) in children.iter().zip(children_states.iter()) {
                            stack.push((child, *child_state));
                        }
                        transitions.push(Transition {
                            children: children_states,
                            parent,
                            symbol: symbol.clone(),
                        });
                    }
                }
            }
        }
        Self::new(transitions, hashset![top_state]).reduce_size()
    }

    pub fn reachable_states_from_top(&self) -> HashSet<State> {
        let mut reachable = HashSet::new();
        let mut stack = self.top_states.iter().cloned().collect::<Vec<_>>();
        while let Some(state) = stack.pop() {
            if reachable.insert(state) {
                if let Some(transitions) = self.transitions_topdown.get(&state) {
                    for transition in transitions {
                        stack.extend(transition.children.iter().cloned());
                    }
                }
            }
        }
        reachable
    }
    pub fn reachable_states_from_bottom(&self) -> HashSet<State> {
        let mut reachable = HashSet::new();
        let mut stack = self.bottom_states.keys().cloned().collect::<Vec<_>>();
        while let Some(state) = stack.pop() {
            if reachable.insert(state) {
                if let Some(transitions) = self.transitions_bottomup.get(&state) {
                    for transition in transitions {
                        if transition.0.children.iter().all(|c| reachable.contains(c)) {
                            stack.push(transition.0.parent);
                        }
                    }
                }
            }
        }
        reachable
    }

    pub fn reduce_size(&self) -> Self {
        self.trim_unreachable_states()
    }

    pub fn trim_unreachable_states(&self) -> Self {
        let reachable_top = self.reachable_states_from_top();
        let reachable_bottom = self.reachable_states_from_bottom();
        let states = reachable_top
            .intersection(&reachable_bottom)
            .cloned()
            .collect();
        let top_states = self.top_states.intersection(&states).cloned().collect();
        let transitions = self
            .transitions_topdown
            .values()
            .flatten()
            .cloned()
            .filter(|t| {
                states.contains(&t.parent)
                    && t.children.iter().all(|c| states.contains(c))
                    && states.contains(&t.parent)
            })
            .collect();
        Self::new(transitions, top_states)
    }

    pub fn evaluate(&self, tree: &GroundTerm<F>) -> bool {
        self.evaluate_topdown(tree)
    }
    fn evaluate_topdown(&self, node: &GroundTerm<F>) -> bool {
        fn dfs<F: SymbolTrait>(
            node: &GroundTerm<F>,
            automaton: &TreeAutomaton<F>,
            memo: &mut HashMap<(*const GroundTerm<F>, State), bool>,
            state: State,
        ) -> bool {
            let key = (node as *const GroundTerm<F>, state);
            if let Some(&result) = memo.get(&key) {
                return result;
            }
            for t in automaton.transitions_topdown.get(&state).unwrap_or(&vec![]) {
                if t.symbol != node.symbol {
                    continue;
                }
                if node.children.len() == 0 {
                    memo.insert(key, true);
                    return true;
                } else if t.children.len() == node.children.len()
                    && node
                        .children
                        .iter()
                        .zip(t.children.iter())
                        .all(|(n, c)| dfs(n, automaton, memo, *c))
                {
                    memo.insert(key, true);
                    return true;
                }
            }
            false
        }

        let mut memo = HashMap::new();
        for &state in &self.top_states {
            if dfs(node, self, &mut memo, state) {
                return true;
            }
        }
        false
    }
    pub fn union(&self, other: &Self) -> Self {
        let top_states = self
            .top_states
            .union(&other.top_states)
            .cloned()
            .collect::<HashSet<_>>();
        let transitions = self
            .transitions_topdown
            .values()
            .flatten()
            .chain(other.transitions_topdown.values().flatten())
            .cloned()
            .collect::<Vec<_>>();
        return Self::new(transitions, top_states);
    }
    pub fn product(&self, other: &Self) -> (Vec<Transition<F>>, HashMap<(State, State), State>) {
        //product construction
        let states_map: HashMap<(State, State), State> = self
            .states
            .iter()
            .cartesian_product(other.states.iter())
            .map(|(s1, s2)| ((*s1, *s2), new_state()))
            .collect();
        let transitions = self
            .transitions_topdown
            .values()
            .flatten()
            .cartesian_product(other.transitions_topdown.values().flatten())
            .filter(|(t1, t2)| t1.symbol == t2.symbol)
            .map(|(t1, t2)| Transition {
                children: t1
                    .children
                    .iter()
                    .zip(t2.children.iter())
                    .map(|(c1, c2)| states_map[&(*c1, *c2)])
                    .collect(),
                parent: states_map[&(t1.parent, t2.parent)],
                symbol: t1.symbol.clone(),
            })
            .collect_vec();
        (transitions, states_map)
    }
    pub fn intersect(&self, other: &Self) -> Self {
        let (transitions, states_map) = self.product(other);
        let top_states = self
            .top_states
            .iter()
            .cartesian_product(other.top_states.iter())
            .map(|(s1, s2)| states_map[&(*s1, *s2)])
            .collect();
        Self::new(transitions, top_states).reduce_size()
    }

    pub fn is_empty(&self) -> bool {
        self.reachable_states_from_bottom()
            .intersection(&self.reachable_states_from_top())
            .count()
            == 0
    }

    pub fn is_equivalent_to(&self, other: &Self) -> bool {
        self.is_subset_of(other) && other.is_subset_of(self)
    }

    fn symbol_to_bottom_states(&self) -> HashMap<F, HashSet<State>> {
        let mut ret = HashMap::new();
        for (state, symbols) in self.bottom_states.iter() {
            for symbol in symbols.iter() {
                ret.entry(symbol.clone())
                    .or_insert(HashSet::new())
                    .insert(*state);
            }
        }
        ret
    }

    // check L(other) \subseteq L(self)
    pub fn includes(&self, other: &Self) -> bool {
        other.is_subset_of(self)
    }
    // check L(A) \subseteq L(B). Antichain based algorithm
    pub fn is_subset_of(&self, other: &Self) -> bool {
        let mut chain: HashMap<State, Vec<HashSet<State>>> = HashMap::new();
        let mut todos: VecDeque<(State, HashSet<State>)> = VecDeque::new();

        let add_to_chain = |chain: &mut HashMap<State, Vec<HashSet<State>>>,
                            a: State,
                            bs: HashSet<State>|
         -> bool {
            if let Some(old_bss) = chain.get_mut(&a) {
                if old_bss.iter().any(|old_bs| old_bs.is_subset(&bs)) {
                    return false;
                }
                old_bss.retain(|old_bs| !bs.is_subset(old_bs));
            }
            chain.entry(a).or_insert(Vec::new()).push(bs);
            true
        };

        let post_pairs = |chain: &HashMap<State, Vec<HashSet<State>>>,
                          a: State,
                          bs: &HashSet<State>|
         -> Vec<(State, HashSet<State>)> {
            let emp_vec = vec![];
            let mut ret = vec![];
            for t_a in self.transitions_bottomup.get(&a).unwrap_or(&vec![]) {
                let a_parent = t_a.0.parent;

                let applicable_args_b = t_a
                    .0
                    .children
                    .iter()
                    .enumerate()
                    .map(|(child_idx, c)| {
                        if child_idx == t_a.1 {
                            bs.clone()
                        } else {
                            chain
                                .get(c)
                                .unwrap_or(&vec![])
                                .iter()
                                .cloned()
                                .fold(HashSet::new(), |acc, x| acc.union(&x).cloned().collect())
                        }
                    })
                    .collect::<Vec<_>>();

                let b_parents = bs
                    .iter()
                    .flat_map(|b| other.transitions_bottomup.get(b).unwrap_or(&emp_vec))
                    .filter(|(t_b, _)| {
                        t_b.symbol == t_a.0.symbol
                            && t_b
                                .children
                                .iter()
                                .zip(applicable_args_b.iter())
                                .all(|(c, b_candidates)| b_candidates.contains(c))
                    })
                    .map(|(t_b, _)| t_b.parent)
                    .collect::<HashSet<_>>();

                ret.push((a_parent, b_parents));
            }

            ret
        };

        let self_sym_to_bottoms = self.symbol_to_bottom_states();
        let otrs_sym_to_bottoms = other.symbol_to_bottom_states();
        if !self_sym_to_bottoms
            .keys()
            .all(|s| otrs_sym_to_bottoms.contains_key(s))
        {
            return false;
        }
        let inits = self_sym_to_bottoms.values().flatten().map(|self_bottom| {
            let keys = self.bottom_states.get(self_bottom).unwrap();
            let otrs_bottoms = keys
                .iter()
                .flat_map(|k| otrs_sym_to_bottoms.get(k).unwrap())
                .cloned()
                .collect::<HashSet<_>>();
            (*self_bottom, otrs_bottoms.clone())
        });
        for (s, states) in inits {
            if self.top_states.contains(&s) && other.top_states.is_disjoint(&states) {
                return false;
            }
            if add_to_chain(&mut chain, s, states.clone()) {
                todos.push_back((s, states));
            }
        }
        while let Some((a, bs)) = todos.pop_front() {
            for (a, bs) in post_pairs(&chain, a, &bs) {
                if self.top_states.contains(&a) && other.top_states.is_disjoint(&bs) {
                    return false;
                }
                if add_to_chain(&mut chain, a, bs.clone()) {
                    todos.push_back((a, bs));
                }
            }
        }

        true
    }

    pub fn universal_aut(symbols: &HashSet<Fun<F>>) -> Self {
        let main_state = new_state();
        let transitions = create_wildcard_transitions(main_state, symbols);
        Self::new(transitions, hashset! {main_state})
    }

    pub fn state_language(&self, s: State) -> TreeAutomaton<F> {
        let mut ret = self.clone();
        assert!(ret.states.contains(&s));
        ret.top_states = hashset![s];
        ret
    }

    pub fn refresh_all_states(&self) -> TreeAutomaton<F> {
        let rename_map: HashMap<State, State> =
            self.states.iter().map(|s| (*s, new_state())).collect();
        let transitions = self
            .transitions_topdown
            .values()
            .flatten()
            .map(|t| Transition {
                children: t.children.iter().map(|c| rename_map[c]).collect(),
                parent: rename_map[&t.parent],
                symbol: t.symbol.clone(),
            })
            .collect();

        let top_states = self.top_states.iter().map(|s| rename_map[s]).collect();
        Self::new(transitions, top_states)
    }

    // topdown manner
    pub fn substitute_state_tree(
        &self,
        s: State,
        aut: &TreeAutomaton<F>,
        preserve_original_behaviour: bool,
    ) -> TreeAutomaton<F> {
        let aut = aut.refresh_all_states();
        let transitions = self
            .transitions_topdown
            .values()
            .flatten()
            .flat_map(|t| {
                let children_combinations = t
                    .children
                    .iter()
                    .map(|c| {
                        if *c == s {
                            if preserve_original_behaviour {
                                aut.top_states.iter().chain(once(c)).cloned().collect_vec()
                            } else {
                                aut.top_states.iter().cloned().collect_vec()
                            }
                        } else {
                            vec![*c]
                        }
                    })
                    .multi_cartesian_product();
                children_combinations.map(move |children| Transition {
                    children,
                    parent: t.parent,
                    symbol: t.symbol.clone(),
                })
            })
            .chain(aut.transitions_topdown.values().flatten().cloned())
            .collect();

        let top_states = self
            .top_states
            .iter()
            .flat_map(|top| {
                if *top == s && !preserve_original_behaviour {
                    aut.top_states.iter().cloned().collect_vec()
                } else {
                    vec![*top]
                }
            })
            .collect();
        Self::new(transitions, top_states)
    }

    pub fn merge_states(&self, states: &[State]) -> TreeAutomaton<F> {
        if states.len() == 0 {
            return self.clone();
        }
        let merged_state = states[0];
        let transitions = self
            .transitions_topdown
            .values()
            .flatten()
            .cloned()
            .map(|t| Transition {
                children: t
                    .children
                    .iter()
                    .map(|c| if states.contains(c) { merged_state } else { *c })
                    .collect(),
                parent: if states.contains(&t.parent) {
                    merged_state
                } else {
                    t.parent
                },
                symbol: t.symbol.clone(),
            })
            .collect();
        let top_states = self
            .top_states
            .iter()
            .map(|s| if states.contains(s) { merged_state } else { *s })
            .collect();
        Self::new(transitions, top_states)
    }

    pub fn get_example(&self) -> Option<GroundTerm<F>> {
        self.get_example_from_state(&self.top_states)
    }
    pub fn get_example_from_state(&self, states: &HashSet<State>) -> Option<GroundTerm<F>> {
        fn aux<F: SymbolTrait>(
            aut: &TreeAutomaton<F>,
            state: State,
            memo: &mut HashMap<State, Option<GroundTerm<F>>>,
        ) -> Option<GroundTerm<F>> {
            if let Some(ret) = memo.get(&state) {
                return ret.clone();
            }
            memo.insert(state, None);

            for t in aut.transitions_topdown.get(&state).unwrap_or(&vec![]) {
                let mut children_ret = vec![];
                let mut failed = false;
                for c in t.children.iter() {
                    if let Some(child_ret) = aux(aut, *c, memo) {
                        children_ret.push(child_ret);
                    } else {
                        failed = true;
                        break;
                    }
                }
                if !failed {
                    let ret = Some(GroundTerm {
                        symbol: t.symbol.clone(),
                        children: children_ret,
                    });
                    memo.insert(state, ret.clone());
                    return ret;
                }
            }
            None
        }
        let mut memo = HashMap::new();
        for state in states {
            if let Some(ret) = aux(self, *state, &mut memo) {
                return Some(ret);
            }
        }
        None
    }

    // stateを子として含むような例を返し、子に該当する部分をvarで表現
    // ex: G(A x)
    pub fn get_upward_example(&self, target_state: State, var: u32) -> Option<Term<F>> {
        let aut = self.trim_unreachable_states();
        aut.save_dot("aut.dot");
        let mut cur_term: Term<F> = Term::Variable { symbol: var };
        let mut cur_state = target_state;
        let mut reached = HashSet::new();
        reached.insert(cur_state);
        while !aut.top_states.contains(&cur_state) {
            let mut found = false;
            for t in aut.transitions_bottomup.get(&cur_state).unwrap_or(&vec![]) {
                let t = &t.0;
                if reached.contains(&t.parent) {
                    continue;
                }
                let mut children = vec![];
                println!("t: {:?}", t);
                for c in t.children.iter() {
                    if *c == cur_state {
                        children.push(cur_term.clone());
                    } else {
                        children.push(
                            aut.get_example_from_state(&hashset![*c])
                                .unwrap()
                                .to_term_with_varmap(&hashmap! {}),
                        );
                    }
                }
                cur_term = Term::Function {
                    symbol: t.symbol.clone(),
                    children,
                };
                cur_state = t.parent;
                reached.insert(cur_state);
                found = true;
                break;
            }
            if !found {
                return None;
            }
        }
        Some(cur_term)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn t(from: &[State], to: State, symbol: &str) -> Transition<String> {
        Transition {
            children: from.to_vec(),
            parent: to,
            symbol: symbol.to_string(),
        }
    }
    #[test]
    fn test_tree_automaton_accept() {
        /*
            a () -> q_1
            b () -> q_2
            g (q_1) -> q_3
            f (q_3 q_2) -> q_4
        */
        let t1 = GroundTerm::from_string("(f (g a) b)").unwrap();
        let a = TreeAutomaton::construct_singleton_aut_ground_term(&t1);

        let t2 = GroundTerm::from_string("(f b (g a))").unwrap();
        let funcs = t1.extract_funcs();
        assert!(a.evaluate(&t1));
        assert!(!a.evaluate(&t2));
        assert!(!a.evaluate(&GroundTerm::from_string("(b (g a))").unwrap()));
        assert!(!a.evaluate(&GroundTerm::from_string("(a)").unwrap()));

        let e_aut = TreeAutomaton::universal_aut(&funcs);
        assert!(e_aut.evaluate(&GroundTerm::from_string("(a)").unwrap()));
        assert!(e_aut.evaluate(&GroundTerm::from_string("(b)").unwrap()));
        assert!(e_aut.evaluate(&GroundTerm::from_string("(f a b)").unwrap()));
        assert!(e_aut.evaluate(&GroundTerm::from_string("(f (f a b) b)").unwrap()));
        assert!(e_aut.intersect(&a).evaluate(&t1));
        assert!(!e_aut.intersect(&a).evaluate(&t2));
    }

    #[test]
    fn test_union() {
        let t1 = GroundTerm::from_string("(f (g a) b)").unwrap();
        let t2 = GroundTerm::from_string("(f b (g a))").unwrap();
        let a1 = TreeAutomaton::construct_singleton_aut_ground_term(&t1);
        let a2 = TreeAutomaton::construct_singleton_aut_ground_term(&t2);
        let a3 = a1.union(&a2);
        assert!(a3.evaluate(&t1));
        assert!(a3.evaluate(&t2));
        assert!(!a3.is_empty());
    }

    #[test]
    fn test_intersection() {
        let t1 = GroundTerm::from_string("(f (g a) b)").unwrap();
        let t2 = GroundTerm::from_string("(f b (g a))").unwrap();
        let a1 = TreeAutomaton::construct_singleton_aut_ground_term(&t1);
        let a2 = TreeAutomaton::construct_singleton_aut_ground_term(&t2);
        let a3 = a1.intersect(&a2);
        assert!(a3.is_empty());
    }

    #[test]
    fn test_inclusion_identity() {
        let t1 = GroundTerm::from_string("(f (g a) b)").unwrap();
        let a = TreeAutomaton::construct_singleton_aut_ground_term(&t1);
        assert!(a.is_subset_of(&a));
    }

    #[test]
    fn test_inclusion_positive() {
        let t1 = GroundTerm::from_string("(f (g a) b)").unwrap();
        let funcs = t1.extract_funcs();

        let a = TreeAutomaton::construct_singleton_aut_ground_term(&t1);
        let b = TreeAutomaton::universal_aut(&funcs);

        assert!(a.is_subset_of(&b));
    }

    #[test]
    fn test_inclusion_negative() {
        let t1 = GroundTerm::from_string("(f (g a) b)").unwrap();
        let a = TreeAutomaton::construct_singleton_aut_ground_term(&t1);

        let t2 = GroundTerm::from_string("(g b)").unwrap();
        let b = TreeAutomaton::construct_singleton_aut_ground_term(&t2);
        assert!(!a.is_subset_of(&b));
    }

    /// 言語が互いに交わらない場合
    #[test]
    fn test_inclusion_disjoint() {
        let t1 = GroundTerm::from_string("(f (g a) b)").unwrap();
        let a = TreeAutomaton::construct_singleton_aut_ground_term(&t1);

        let t2 = GroundTerm::from_string("(b)").unwrap();
        let b = TreeAutomaton::construct_singleton_aut_ground_term(&t2);

        a.save_dot("a.dot");
        b.save_dot("b.dot");
        println!("a: {:?}", a);
        println!("b: {:?}", b);
        assert!(!a.is_subset_of(&b));
        assert!(!b.is_subset_of(&a));
    }

    /// nondeterministic な経路を持つ場合の包含チェック
    #[test]
    fn test_inclusion_multiple_paths() {
        // 自動機 A： nondeterministic な遷移を持ち、(f a b) あるいは (f b a) を受理する
        let a = TreeAutomaton::new(
            vec![
                // bottom 状態 0 から nondeterministically "a" と "b" に行く
                t(&[], 1, "a"),
                t(&[], 2, "b"),
                t(&[], 3, "a"),
                t(&[], 4, "b"),
                // 2 通りの構成
                t(&[1, 4], 5, "f"),
                t(&[3, 2], 5, "f"),
            ],
            HashSet::from([5]),
        );
        // 自動機 B： 決定的に (f a b) を受理する
        let b = TreeAutomaton::new(
            vec![t(&[], 1, "a"), t(&[], 2, "b"), t(&[1, 2], 3, "f")],
            HashSet::from([3]),
        );
        // ここでは、A の受理する全ての項が B にも含まれると仮定する（設計次第ですが）
        assert!(a.is_subset_of(&b));
    }

    /// 和集合による包含チェック：  
    /// ２つの自動機 A1, A2 の和集合が、全体を受理する B に含まれることを確認
    #[test]
    fn test_inclusion_union() {
        let a1 = TreeAutomaton::new(vec![t(&[], 1, "a")], HashSet::from([1]));
        let a2 = TreeAutomaton::new(vec![t(&[], 2, "b")], HashSet::from([2]));
        // 和集合をとる（既存の union メソッドを利用）
        let a_union = a1.union(&a2);
        // B は "a" と "b" の両方を受理する自動機
        let b = TreeAutomaton::new(vec![t(&[], 1, "a"), t(&[], 2, "b")], HashSet::from([1, 2]));
        assert!(a_union.is_subset_of(&b));
    }

    #[test]
    fn test_merge_inclusion() {
        let t1 = GroundTerm::from_string("(f (g a) b)").unwrap();
        let a = TreeAutomaton::construct_singleton_aut_ground_term(&t1);
        let a2 = a.merge_states(
            &a.states
                .iter()
                .take(2)
                .cloned()
                .collect::<Vec<_>>()
                .as_slice(),
        );
        assert!(a.is_subset_of(&a2));
        assert!(!a2.is_subset_of(&a));
    }
}
