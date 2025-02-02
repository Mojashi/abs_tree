use std::collections::HashSet;

use crate::{
    barmc::transitionsystem::{ConfigLangTrait, ConfigTrait, TransitionSystem},
    tree_automaton::tree_automaton::TreeAutomaton,
    trs::{ictrs::ITRS, term::GroundTerm, trs::Fun},
};

impl ConfigTrait for GroundTerm<u32> {}
impl ConfigLangTrait for TreeAutomaton<u32> {
    type Config = GroundTerm<u32>;
    fn accept(&self, _seq: &GroundTerm<u32>) -> bool {
        self.evaluate(_seq)
    }
    fn includes(&self, _seq: &Self) -> bool {
        self.includes(_seq)
    }
    fn get_dot_label(&self) -> String {
        let ex = self
            .get_example()
            .map(|t| t.to_string())
            .unwrap_or_else(|| "None".to_string());
        format!("states:{:?}; ex:{}", self.states.len(), ex)
    }
    fn debug_this(&self, name: &str) {
        self.save_dot(format!("{}.dot", name).as_str());
    }
}

#[derive(Clone)]
pub struct TransitionSystemITRS {
    itrs: ITRS<u32>,
    funs: HashSet<Fun<u32>>,
}

impl TransitionSystemITRS {
    pub fn new(itrs: ITRS<u32>) -> Self {
        let funs = itrs.funs().into_iter().collect();
        Self { itrs, funs }
    }
}

impl TransitionSystem for TransitionSystemITRS {
    type Config = GroundTerm<u32>;
    type ConfigLang = TreeAutomaton<u32>;
    type OperationId = usize;

    fn init_states(&self) -> Vec<Self::ConfigLang> {
        vec![TreeAutomaton::construct_singleton_aut_term(
            &self.itrs.query.lhs,
            &self.funs,
        )]
    }

    fn find_bad_state(&self, c: &Self::ConfigLang) -> Option<Self::Config> {
        c.find_intersection_with_term(&self.itrs.query.rhs)
    }

    fn nexts(&self, conf: &Self::ConfigLang) -> Vec<(usize, Self::ConfigLang)> {
        self.itrs
            .trs
            .rules
            .iter()
            .enumerate()
            .flat_map(|(opid, r)| {
                conf.apply_trs_rule(r, &self.funs)
                    .into_iter()
                    .map(move |res| (opid, res))
            })
            .collect()
    }

    fn prev_within(
        &self,
        c: &Self::Config,
        within: &Self::ConfigLang,
        opid: Self::OperationId,
    ) -> Option<Self::Config> {
        //println!("prev_within: c:{}\nrule:{}", c, self.itrs.trs.rules[opid]);
        let reversed_rule = &self.itrs.trs.rules[opid].reversed();

        let aut = TreeAutomaton::construct_singleton_aut_ground_term(c);
        
        let res = aut.apply_trs_rule(reversed_rule, &self.funs);
        
        // within.save_dot("within.dot");
        // aut.save_dot("aut.dot");
        // TreeAutomaton::union_all(&res)
        //     .trim_unreachable_states()
        //     .save_dot("res.dot");
    
        res.iter()
            .flat_map(|r| r.intersect(within).get_example())
            .next()
    }
}
