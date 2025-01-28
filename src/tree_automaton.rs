use std::sync::atomic::AtomicU32;

use crate::term::{SymbolTrait, Term};

type State = u32;

static mut STATE_COUNTER: AtomicU32 = AtomicU32::new(0);
pub fn new_state() -> State {
    unsafe { STATE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst) }
}

#[derive(Clone)]
pub struct Transition<F: SymbolTrait> {
    pub children: Vec<State>,
    pub parent: State,
    pub symbol: F,
}

#[derive(Clone)]
pub struct TreeAutomaton<F: SymbolTrait> {
    states: Vec<State>,
    transitions: Vec<Transition<F>>,
}

impl <F: SymbolTrait> TreeAutomaton<F> {
    pub fn new() -> Self {
        TreeAutomaton {
            states: Vec::new(),
            transitions: Vec::new(),
        }
    }

    pub fn add_state(&mut self, state: State) {
        self.states.push(state);
    }

    pub fn add_transition(&mut self, children: Vec<State>, parent: State, symbol: F) {
        self.transitions.push(Transition {
            children,
            parent,
            symbol,
        });
    }

    // pub fn evaluate(&self, tree: &Term<F>) -> bool {
    //     self.evaluate_node_on_state(tree, 0)
    // }

    // fn evaluate_node_on_state(&self, node: &Term<F>, state: State) -> bool {
    //     for transition in &self.transitions {
    //         if transition.from_states.contains(&state) && transition.symbol == node.symbol {
    //             return node.children.iter().all(|child| {
    //                 self.evaluate_node_on_state(child, transition.to_state)
    //             });
    //         }
    //     }
    //     false
    // }

}
