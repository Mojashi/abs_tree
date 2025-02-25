use crate::trs::term::SymbolTrait;
use crate::{
    barmc::transitionsystem::{ConfigLangTrait, ConfigTrait, TransitionSystem},
    tree_automaton::tree_automaton::TreeAutomaton,
    trs::{
        ictrs::ITRS,
        term::GroundTerm,
        trs::{Fun, Rule},
    },
};
use itertools::Itertools;
use maplit::hashmap;
use std::collections::HashSet;

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

    fn remove_element(&self, c: &Self::Config) -> Self {
        self.remove_element(c)
    }
}

#[derive(Clone)]
pub struct TransitionSystemITRS {
    itrs: ITRS<u32>,
    original_funs: HashSet<Fun<u32>>,
    //funs: HashSet<Fun<u32>>,
    aux_rule_idx: usize, // for query
}

impl TransitionSystemITRS {
    pub fn new(mut itrs: ITRS<u32>) -> Self {
        let start_fun = u32::new();
        itrs.trs.rules.push(Rule {
            lhs: crate::trs::term::Term::Function {
                symbol: start_fun,
                children: vec![],
            },
            rhs: itrs.query.lhs.clone(),
        });
        let aux_rule_idx = itrs.trs.rules.len() - 1;
        itrs.query.lhs = crate::trs::term::Term::Function {
            symbol: start_fun,
            children: vec![],
        };

        let funs = itrs.funs().into_iter().chain(vec![Fun {
            symbol: start_fun,
            arity: 0,
            condition_intermidiate_fun: false,
        }]).chain(itrs.trs.intermidiate_funs.to_vec()).collect::<HashSet<_>>();
        let original_funs = funs
            .clone();
            // .into_iter()
            // .filter(|f| f.condition_intermidiate_fun == false)
            // .collect::<HashSet<_>>();
        Self {
            itrs,
            original_funs,
            //funs,
            aux_rule_idx,
        }
    }
}

impl TransitionSystem for TransitionSystemITRS {
    type Config = GroundTerm<u32>;
    type ConfigLang = TreeAutomaton<u32>;
    type OperationId = usize;

    fn init_states(&self) -> Vec<Self::ConfigLang> {
        vec![TreeAutomaton::construct_singleton_aut_term(
            &self.itrs.query.lhs.clone(),
            &self.original_funs,
        )]
    }

    fn find_bad_state(&self, c: &Self::ConfigLang) -> Option<Self::Config> {
        c.find_intersection_with_term(&self.itrs.query.rhs)
    }

    fn nexts(&self, conf: &Self::ConfigLang) -> Vec<(usize, Self::ConfigLang)> {
        //assert!(conf.assert_valid_funs(&self.funs));
        self.itrs
            .trs
            .rules
            .iter()
            .enumerate()
            .flat_map(|(opid, r)| {
                conf.apply_trs_rule(r, &self.original_funs)
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
        println!("prev_within: c:{}\nrule:{}", c, self.itrs.trs.rules[opid]);
        let reversed_rule = &self.itrs.trs.rules[opid].reversed();
        let matches = c.match_term_with_subterm(&reversed_rule.lhs);
        if matches.is_empty() {
            println!("no match");
            return None;
        }
        //within.save_dot("within.dot");
        for (m, pos) in matches {
            assert!(m.keys().collect::<HashSet<_>>() == reversed_rule.lhs.vars().iter().collect());
            println!("pos: {:?}", pos);
            for (k, v) in m.iter() {
                println!("m: {} -> {}", k, v);
            }
            let m = m
                .into_iter()
                .map(|(k, v)| (k as u32, v.to_term_with_varmap(&hashmap! {})))
                .collect();
            let res = reversed_rule.rhs.subst_term(&m);
            println!("res1: {}", res);
            let res = c.subst_term_with_pos(&pos.into_iter().collect_vec(), &res);
            println!("res2: {}", res);
            if let Some(ret) = within.find_intersection_with_term(&res) {
                return Some(ret);
            }
        }
        //assert!(false);
        None
    }
}
