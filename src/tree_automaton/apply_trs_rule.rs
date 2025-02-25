use itertools::Itertools;
use maplit::{hashmap, hashset};
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    iter::Map,
};

use crate::{
    tree_automaton::tree_automaton::{new_state, Transition, TreeAutomaton},
    trs::{
        term::{GroundTerm, SymbolTrait, Term},
        trs::{Fun, Rule},
    },
};

use super::tree_automaton::State;

type Assignment = HashMap<u32, HashSet<State>>;
#[derive(Debug)]
struct PatternMatchResult {
    pub position: State,
    pub assignment: Assignment,
}

impl<F: SymbolTrait> TreeAutomaton<F> {
    pub fn is_univ_state(&self, state: State, symbols: &HashSet<Fun<F>>) -> bool {
        symbols.iter().all(|f| {
            self.transitions_topdown
                .get(&state)
                .unwrap_or(&vec![])
                .iter()
                .any(|t| {
                    t.symbol == f.symbol
                        && t.children.iter().all(|c| *c == state)
                        && t.parent == state
                })
        })
    }
    pub fn apply_trs_rule(
        &self,
        rule: &Rule<F>,
        symbols: &HashSet<Fun<F>>,
    ) -> Vec<TreeAutomaton<F>> {
        //println!("apply_trs_rule: {}", rule);
        //assert!(rule.is_right_linear());
        let univ_aut = TreeAutomaton::universal_aut(symbols);

        fn map_rhs<F: SymbolTrait>(
            rhs: &Term<F>,
            state_map: &HashMap<u32, TreeAutomaton<F>>,
            univ_aut: &TreeAutomaton<F>,
        ) -> TreeAutomaton<F> {
            match rhs {
                Term::Variable { symbol } => state_map
                    .get(symbol)
                    .map(|c| c.clone())
                    .unwrap_or(univ_aut.clone()),
                Term::Function {
                    symbol: f,
                    children,
                } => {
                    let mut transitions = vec![];
                    let mut children_top_states = vec![];
                    for child in children {
                        let child_aut = map_rhs(child, state_map, univ_aut);
                        transitions
                            .extend(child_aut.transitions_topdown.values().flatten().cloned());
                        children_top_states.push(*child_aut.top_states.iter().next().unwrap());
                    }
                    let top = new_state();
                    transitions.push(Transition {
                        children: children_top_states,
                        parent: top,
                        symbol: f.clone(),
                    });
                    TreeAutomaton::new(transitions, hashset![top])
                }
            }
        }
        let match_result = self.pattern_match_states(&rule.lhs);
        //println!("match_result: {:?}", match_result);
        let match_result = match_result
            .into_iter()
            .flat_map(|m| self.match_result_to_trees(m))
            .collect_vec();

        let mut ret = self.clone();
        let mut applied = false;
        for (position, assignment) in match_result {
            let assignment_aut = map_rhs(&rule.rhs, &assignment, &univ_aut);
            if assignment_aut.is_empty() {
                continue;
            }
            if self.is_univ_state(position, symbols) {
                continue;
            }
            applied = true;
            ret = ret.substitute_state_tree(position, &assignment_aut, true);
        }
        if !applied {
            return vec![];
        }
        ret = ret.reduce_size();
        if ret.is_subset_of(self) {
            vec![]
        } else {
            vec![ret]
        }
    }
    fn match_result_to_trees(
        &self,
        m: PatternMatchResult,
        // マッチ結果の木を実際にintersectionしてみて、どれかが空になるなら、そのマッチングは失敗である
    ) -> Option<(State, HashMap<u32, TreeAutomaton<F>>)> {
        let mut ret = HashMap::new();
        let mut stateset_tree_map: HashMap<BTreeSet<State>, TreeAutomaton<F>> = HashMap::new();
        for (k, v) in m.assignment.iter() {
            let lang = stateset_tree_map
                .entry(v.iter().cloned().collect())
                .or_insert_with(|| {
                    v.iter()
                        .map(|s| self.state_language(*s))
                        // このオートマトンに対してパターンマッチしている以上、何かしらの制約があるはずなのでReduce
                        .reduce(|a, b| a.intersect(&b).clone())
                        .unwrap()
                        .reduce_size()
                        .refresh_all_states()
                });
            if lang.is_empty() {
                return None;
            }
            ret.insert(*k, lang.clone());
        }
        Some((m.position, ret))
    }
    fn pattern_match_states(&self, pattern: &Term<F>) -> Vec<PatternMatchResult> {
        /*
        pattern: (f ( g x ) y x)
        output : [
            {x -> [q1,q3], y -> [q5]}, // q1とq3の言語のintersectionがXで、Yはq5の言語
            {x -> ..., y -> ...},
            {x -> ..., y -> ...}
        ]
        */

        // トップダウンでvalidな状態を探す

        self.states
            .iter()
            .flat_map(|&position| {
                let mut assignment: Vec<Assignment> = self.pattern_match_aux(position, &pattern);
                assignment.into_iter().map(move |a| PatternMatchResult {
                    position: position.clone(),
                    assignment: a,
                })
            })
            .collect_vec()
    }

    fn pattern_match_aux(&self, state: State, pattern: &Term<F>) -> Vec<Assignment> {
        fn intersect_assignment(a: Assignment, b: Assignment) -> Assignment {
            let keys = a.keys().chain(b.keys()).cloned().collect::<HashSet<_>>();
            keys.into_iter()
                .map(|k| {
                    if !a.contains_key(&k) {
                        (k, b[&k].clone())
                    } else if !b.contains_key(&k) {
                        (k, a[&k].clone())
                    } else {
                        (k, a[&k].union(&b[&k]).cloned().collect())
                    }
                })
                .collect()
        }
        fn prune_redudant(dnf: &mut Vec<Assignment>) {
            // 包含関係にあるものを削除する
            let l = dnf.len();
            if l == 0 {
                return;
            }
            for i in l - 1..0 {
                for j in 0..i {
                    if i == j {
                        continue;
                    }
                    if dnf[i].iter().all(|(k, v)| dnf[j][k].is_subset(&v)) {
                        dnf.remove(i);
                        break;
                    }
                }
            }
        }

        fn intersect_dnf(a: Vec<Assignment>, b: Vec<Assignment>) -> Vec<Assignment> {
            a.into_iter()
                .cartesian_product(b)
                .map(|(a, b)| intersect_assignment(a, b))
                .collect()
        }

        let r = match pattern {
            Term::Variable { symbol } => {
                vec![hashmap! {symbol.clone() => hashset!{state}}]
            }
            Term::Function { symbol, children } => {
                // 子供それぞれについて再帰的にパターンマッチを行う
                let mut ret = Vec::new();
                for applicable_t in self
                    .transitions_topdown
                    .get(&state)
                    .unwrap_or(&vec![])
                    .iter()
                    .filter(|t| t.symbol == *symbol)
                {
                    let mut child_results = vec![hashmap! {}];
                    assert_eq!(applicable_t.children.len(), children.len());
                    for (term_ch, state_ch) in children.iter().zip(applicable_t.children.iter()) {
                        let r = self.pattern_match_aux(*state_ch, term_ch);
                        child_results = intersect_dnf(child_results, r);
                    }
                    ret.extend(child_results);
                }
                prune_redudant(&mut ret);
                ret
            }
        };
        r
    }

    pub fn union_all(auts: &[TreeAutomaton<F>]) -> TreeAutomaton<F> {
        let mut transitions = vec![];
        let mut top_states = HashSet::new();
        for aut in auts {
            transitions.extend(aut.transitions_topdown.values().flatten().cloned());
            top_states.extend(aut.top_states.iter().cloned());
        }
        TreeAutomaton::new(transitions, top_states)
    }

    //頭からマッチングして交差部分を一つ返す
    pub fn find_intersection_with_term(&self, term: &Term<F>) -> Option<GroundTerm<F>> {
        for &top in self.top_states.iter() {
            let patterns = self
                .pattern_match_aux(top, term)
                .into_iter()
                .map(|assignment| PatternMatchResult {
                    position: top,
                    assignment,
                });
            let patterns = patterns
                .into_iter()
                .flat_map(|a| self.match_result_to_trees(a));

            for (pos, assign) in patterns {
                let mut failed = false;
                let mut concrete_assign = HashMap::new();
                for (k, v) in assign.iter() {
                    if let Some(e) = v.get_example() {
                        concrete_assign.insert(*k, e);
                    } else {
                        failed = true;
                        break;
                    }
                }
                if failed {
                    continue;
                }
                let g_term = term.subst_ground(&concrete_assign);
                let var = 0;
                let upward_ex = self.get_upward_example(pos, var);
                if let Some(upward_ex) = upward_ex {
                    let mut var_map = HashMap::new();
                    var_map.insert(var, g_term);
                    let term = upward_ex.subst_ground(&var_map);
                    return Some(term);
                }
            }
        }
        None
    }

    // 部分木としてTermを含んでいたら、一つ具体的な例を返す
    // 効率悪そう
    pub fn find_intersection_with_subterm(&self, term: &Term<F>) -> Option<GroundTerm<F>> {
        for &top in self.states.iter() {
            let patterns = self
                .pattern_match_aux(top, term)
                .into_iter()
                .map(|assignment| PatternMatchResult {
                    position: top,
                    assignment,
                })
                .into_iter()
                .collect_vec();
            //println!("patterns: {:?}", patterns);
            let patterns = patterns
                .into_iter()
                .flat_map(|a| self.match_result_to_trees(a));

            for (pos, assign) in patterns {
                //println!("pos: {:?}, assign: {:?}", pos, assign);
                let mut failed = false;
                let mut concrete_assign = HashMap::new();
                for (k, v) in assign.iter() {
                    if let Some(e) = v.get_example() {
                        concrete_assign.insert(*k, e);
                    } else {
                        failed = true;
                        break;
                    }
                }
                if failed {
                    continue;
                }
                let g_term = term.subst_ground(&concrete_assign);
                let var = 0;
                let upward_ex = self.get_upward_example(pos, var);
                if let Some(upward_ex) = upward_ex {
                    let mut var_map = HashMap::new();
                    var_map.insert(var, g_term);
                    let term = upward_ex.subst_ground(&var_map);
                    return Some(term);
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::trs::term::GroundTerm;

    use super::*;

    #[test]
    fn test_trs_rule_apply() {
        let term1 = GroundTerm::from_string("(F (G A) B)").unwrap();
        let term2 = GroundTerm::from_string("(H (G A) B)").unwrap();
        let symbols: HashSet<Fun<String>> = term1
            .extract_funcs()
            .union(&term2.extract_funcs())
            .cloned()
            .collect();

        let rule = Rule::from_string("(F x y) -> (H x y)").unwrap();
        //println!("rule: {:?}", rule);

        let aut1 = TreeAutomaton::construct_singleton_aut_ground_term(&term1);
        let aut2 = TreeAutomaton::union_all(&aut1.apply_trs_rule(&rule, &symbols));

        aut1.save_dot("test_trs_rule_apply_before.dot");
        aut2.save_dot("test_trs_rule_apply.dot");

        assert!(aut2.evaluate(&term2));
        assert!(aut2.evaluate(&term1));

        let aut3 = TreeAutomaton::construct_singleton_aut_ground_term(&term2).union(&aut1);
        assert!(aut2.is_equivalent_to(&aut3));
    }
}
