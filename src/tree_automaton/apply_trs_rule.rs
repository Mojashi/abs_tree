use itertools::Itertools;
use maplit::{hashmap, hashset};
use std::{
    collections::{HashMap, HashSet},
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
    // TODO: DNFをやめて、全部一つのオートマトンの中に入れて、複数箇所に同時に適用する操作を表現した方が良い
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
            .map(|m| self.match_result_to_trees(m))
            .collect_vec();

        let mut ret = self.clone();
        for (position, assignment) in match_result {
            let assignment_aut = map_rhs(&rule.rhs, &assignment, &univ_aut);
            ret = ret
                .substitute_state_tree(position, &assignment_aut, true)
                .reduce_size();
            //ここで、assignment_autを使わないようなTreeの中には、ruleが適用可能なものも含まれており、これはつまり、apply_trs_ruleは0~1回の適用を許している
        }
        vec![ret]
    }
    fn match_result_to_trees(
        &self,
        m: PatternMatchResult,
    ) -> (State, HashMap<u32, TreeAutomaton<F>>) {
        (
            m.position,
            m.assignment
                .iter()
                .map(|(k, v)| {
                    (
                        *k,
                        v.iter()
                            .map(|s| self.state_language(*s))
                            // このオートマトンに対してパターンマッチしている以上、何かしらの制約があるはずなのでReduce
                            .reduce(|a, b| a.intersect(&b).clone())
                            .unwrap(),
                    )
                })
                .collect::<HashMap<_, _>>(),
        )
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
            let patterns = patterns.into_iter().map(|a| self.match_result_to_trees(a));

            for (pos, assign) in patterns {
                let mut subst_aut = self.state_language(pos).trim_unreachable_states();
                let mut failed = false;
                for (k, v) in assign.iter() {
                    if let Some(e) = v.get_example() {
                        let t = Self::construct_singleton_aut_ground_term(&e);
                        subst_aut = subst_aut.substitute_state_tree(*k, &t, false);
                    } else {
                        failed = true;
                        break;
                    }
                }
                if failed {
                    continue;
                }
                if let Some(ex) = subst_aut.get_example() {
                    let var = 0;
                    let upward_ex = self.get_upward_example(pos, var);
                    if let Some(upward_ex) = upward_ex {
                        let mut var_map = HashMap::new();
                        var_map.insert(var, ex);
                        let term = upward_ex.subst_ground(&var_map);
                        return Some(term);
                    }
                }
            }
        }
        None
    }

    // 部分木としてTermを含んでいたら、一つ具体的な例を返す
    // 効率悪そう
    pub fn find_intersection_with_subterm(&self, term: &Term<F>) -> Option<GroundTerm<F>> {
        println!("find_intersection_with_term: {}", term);

        let patterns = self.pattern_match_states(term);
        let patterns = patterns.into_iter().map(|a| self.match_result_to_trees(a));
        for (pos, assign) in patterns {
            let mut subst_aut = self.state_language(pos).trim_unreachable_states();
            let mut failed = false;
            for (k, v) in assign.iter() {
                if let Some(e) = v.get_example() {
                    let t = Self::construct_singleton_aut_ground_term(&e);
                    subst_aut = subst_aut.substitute_state_tree(*k, &t, false);
                } else {
                    failed = true;
                    break;
                }
            }
            if failed {
                continue;
            }
            if let Some(ex) = subst_aut.get_example() {
                let var = 0;
                let upward_ex = self.get_upward_example(pos, var);
                if let Some(upward_ex) = upward_ex {
                    let mut var_map = HashMap::new();
                    var_map.insert(var, ex);
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
        println!("rule: {:?}", rule);

        let aut1 = TreeAutomaton::construct_singleton_aut_ground_term(&term1);
        let aut2 = TreeAutomaton::union_all(&aut1.apply_trs_rule(&rule, &symbols));

        aut1.save_dot("test_trs_rule_apply_before.dot");
        aut2.save_dot("test_trs_rule_apply.dot");

        assert!(aut2.evaluate(&term2));
        assert!(!aut2.evaluate(&term1));

        let aut3 = TreeAutomaton::construct_singleton_aut_ground_term(&term2);
        assert!(aut2.is_equivalent_to(&aut3));

        let aut4 = TreeAutomaton::union_all(&aut2.apply_trs_rule(&rule, &symbols));
        assert!(aut4.is_empty());
    }
}
