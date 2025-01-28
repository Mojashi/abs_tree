use std::{
    collections::{HashMap, HashSet},
    sync::atomic::AtomicU32,
};
use crate::term::{GroundTerm, SymbolTrait};

type State = u32;

static mut STATE_COUNTER: AtomicU32 = AtomicU32::new(0);
pub fn new_state() -> State {
    unsafe { STATE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst) }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Transition<F: SymbolTrait> {
    pub children: Vec<State>,
    pub parent: State,
    pub symbol: F,
}

#[derive(Clone)]
pub struct TreeAutomaton<F: SymbolTrait> {
    states: HashSet<State>,
    top_states: HashSet<State>, // accepting states for bottom-up evaluation
    bottom_states: HashSet<State>, // accepting states for top-down evaluation
    transitions_topdown: HashMap<State, Vec<Transition<F>>>,
    transitions_bottomup: HashMap<State, Vec<Transition<F>>>,
}

impl<F: SymbolTrait> TreeAutomaton<F> {
    pub fn new(
        transitions: Vec<Transition<F>>,
        top_states: HashSet<State>,
        bottom_states: HashSet<State>,
    ) -> Self {
        let transitions = transitions
            .into_iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        let mut states = HashSet::new();
        let mut transitions_topdown = HashMap::new();
        let mut transitions_bottomup = HashMap::new();
        for transition in transitions {
            states.insert(transition.parent);
            transitions_topdown
                .entry(transition.parent)
                .or_insert(Vec::new())
                .push(transition.clone());
            for child in transition.children {
                states.insert(child);
                transitions_bottomup
                    .entry(child)
                    .or_insert(Vec::new())
                    .push(Transition {
                        children: vec![],
                        parent: transition.parent,
                        symbol: transition.symbol.clone(),
                    });
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
        let mut stack = self.bottom_states.iter().collect::<Vec<_>>();
        while let Some(&state) = stack.pop() {
            if reachable.insert(state) {
                if let Some(transitions) = self.transitions_bottomup.get(&state) {
                    for transition in transitions {
                        stack.push(&transition.parent);
                    }
                }
            }
        }
        reachable
    }

    pub fn trim_unreachable_states(&mut self) {
        let reachable_top = self.reachable_states_from_top();
        let reachable_bottom = self.reachable_states_from_bottom();
        self.states
            .retain(|state| reachable_top.contains(state) || reachable_bottom.contains(state));
        self.transitions_topdown
            .retain(|state, _| reachable_top.contains(state));
        self.transitions_bottomup
            .retain(|state, _| reachable_bottom.contains(state));
    }

    pub fn add_transition(&mut self, transition: Transition<F>) {
        self.states.insert(transition.parent);
        self.transitions_topdown
            .entry(transition.parent)
            .or_insert(Vec::new())
            .push(transition.clone());
        for child in transition.children {
            self.states.insert(child);
            self.transitions_bottomup
                .entry(child)
                .or_insert(Vec::new())
                .push(Transition {
                    children: vec![],
                    parent: transition.parent,
                    symbol: transition.symbol.clone(),
                });
        }
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
            println!("dfs: {:?} {:?}", node, state);
            let key = (node as *const GroundTerm<F>, state);
            if let Some(&result) = memo.get(&key) {
                return result;
            }
            for t in automaton.transitions_topdown.get(&state).unwrap_or(&vec![]) {
                if t.children.len() == 0 {
                    if t.symbol == node.symbol {
                        memo.insert(key, true);
                        return true;
                    }
                } else if t.symbol == node.symbol && t.children.len() == node.children.len() {
                    if node
                        .children
                        .iter()
                        .zip(t.children.iter())
                        .all(|(n, c)| dfs(n, automaton, memo, *c))
                    {
                        memo.insert(key, true);
                        return true;
                    }
                }
            }
            memo.insert(key, true);
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tree_automaton() {
        let ground_term = GroundTerm::from_string("(f (g a) b)").unwrap();
        /*
            a (q_0) -> q_1
            b (q_0) -> q_2
            g (q_1) -> q_3
            f (q_3 q_2) -> q_4
        */
        let states = vec![
            new_state(),
            new_state(),
            new_state(),
            new_state(),
            new_state(),
        ];
        let mut a = TreeAutomaton::new(
            vec![],
            HashSet::from([states[4]]),
            HashSet::from([states[0]]),
        );
        a.add_transition(Transition {
            children: vec![states[0]],
            parent: states[1],
            symbol: "a".to_string(),
        });
        a.add_transition(Transition {
            children: vec![states[0]],
            parent: states[2],
            symbol: "b".to_string(),
        });
        a.add_transition(Transition {
            children: vec![states[1]],
            parent: states[3],
            symbol: "g".to_string(),
        });
        a.add_transition(Transition {
            children: vec![states[3], states[2]],
            parent: states[4],
            symbol: "f".to_string(),
        });
        assert!(a.evaluate(&ground_term));
    }
}
