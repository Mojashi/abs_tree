use crate::{
    barmc::{badconfigs::BadConfigs, union_find::UnionFind},
    tree_automaton::tree_automaton::{State, TreeAutomaton},
    trs::term::{GroundTerm, SymbolTrait},
};
use itertools::Itertools;
use rand::seq::SliceRandom;

pub fn create_abstraction_merge_as_can(
    lang: &TreeAutomaton<u32>,
    is_contains_bad: &dyn Fn(&TreeAutomaton<u32>) -> Option<GroundTerm<u32>>,
) -> TreeAutomaton<u32> {
    if is_contains_bad(lang).is_some() {
        println!("bad abstraction");
    }

    let befstates = lang.states.len();
    let mut combs = lang
        .states
        .iter()
        .cloned()
        .tuple_combinations()
        .collect_vec();
    combs.shuffle(&mut rand::thread_rng());

    println!("p {:?} {:?}", lang.states.len(), combs.len());

    let mut ds = UnionFind::<State>::new();
    let mut bad_merges: Vec<(State, State)> = vec![];

    let mut lang: TreeAutomaton<u32> = lang.clone();
    for (s, t) in combs.iter() {
        if ds.connected(s, t) {
            continue;
        }
        if bad_merges.iter().any(|(a, b)| {
            ds.connected(&s, a) && ds.connected(&t, b) || ds.connected(&s, b) && ds.connected(&t, a)
        }) {
            continue;
        }
        let s_contents = ds.find_contents(s);
        let t_contents = ds.find_contents(t);
        let s = lang
            .states
            .iter()
            .find(|a| s_contents.contains(*a))
            .cloned()
            .unwrap();
        let t = lang
            .states
            .iter()
            .find(|a| t_contents.contains(*a))
            .cloned()
            .unwrap();

        let try_lang = lang.merge_states(&[s, t]);
        let represent_state = if try_lang.states.contains(&s) {
            &s
        } else {
            &t
        };
        assert!(try_lang.states.contains(represent_state));

        if is_contains_bad(&try_lang).is_none() {
            lang = try_lang;
            ds.merge(&s, &t);
        } else {
            bad_merges.push((s, t));
        }
    }
    println!("abs {:?} -> {:?} ", befstates, lang.states.len());
    return lang.clone();
}

pub fn non_abstractor<F: SymbolTrait>(
    seq: &TreeAutomaton<F>,
    _: &BadConfigs<GroundTerm<u32>>,
) -> TreeAutomaton<F> {
    seq.clone()
}
