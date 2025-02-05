use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display},
};

use super::{
    term::{SymbolTrait, Term},
    trs::{crule_to_rule, ConditionalRule, Fun, Rule, CTRS, TRS},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Query<F: SymbolTrait> {
    pub cond: Vec<Rule<F>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ICTRS<F: SymbolTrait> {
    pub ctrs: CTRS<F>,
    pub query: Query<F>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ITRS<F: SymbolTrait> {
    pub trs: TRS<F>,
    pub query: Rule<F>, // is lhs -> rhs NON-reachable?
}

impl<F: SymbolTrait> ICTRS<F> {
    pub fn to_itrs(&self) -> ITRS<F> {
        let mut trs = self.ctrs.to_trs();
        if trs.funs.iter().all(|f| f.arity != 0) {
            let symbol = F::new();
            trs.funs.push(Fun {
                symbol,
                arity: 0,
                condition_intermidiate_fun: false,
            });
        }

        let arity_zero_fun = trs.funs.iter().find(|f| f.arity == 0).unwrap();

        let mut main_rule = Rule {
            lhs: Term::Function {
                symbol: arity_zero_fun.symbol.clone(),
                children: vec![],
            },
            rhs: Term::Function {
                symbol: arity_zero_fun.symbol.clone(),
                children: vec![],
            },
        };
        let mut conds = self.query.cond.clone();
        // let pairtial_ground_cond = self
        //     .query
        //     .cond
        //     .iter()
        //     .enumerate()
        //     .find(|c| c.1.rhs.is_ground() || c.1.lhs.is_ground());
        // if let Some((p_idx, p)) = pairtial_ground_cond {
        //     conds.remove(p_idx);
        //     main_rule = p.clone();
        // } 

        let query_num_conds = conds.len();
        if query_num_conds == 0 {
            return ITRS {
                trs,
                query: main_rule,
            };
        }
        let intermidiate_fun = trs.get_or_insert_condition_intermidiate_func(query_num_conds);

        let query_crule = ConditionalRule {
            lhs: main_rule.lhs.clone(),
            rhs: main_rule.rhs.clone(),
            conds,
        };

        let query = crule_to_rule(query_crule, intermidiate_fun);
        let query = Rule {
            lhs: query.rhs.clone(),
            rhs: main_rule.rhs.clone(),
        };

        ITRS { trs, query }
    }
}

impl<F: SymbolTrait> ITRS<F> {
    pub fn funs(&self) -> Vec<Fun<F>> {
        self.trs.funs.clone()
    }
    pub fn rename_symbols_to_u32(&self) -> (ITRS<u32>, HashMap<F, u32>) {
        let (trs, sym_map) = self.trs.rename_symbols_to_u32();
        (
            ITRS {
                trs,
                query: Rule {
                    lhs: self.query.lhs.rename_symbols(&sym_map),
                    rhs: self.query.rhs.rename_symbols(&sym_map),
                },
            },
            sym_map,
        )
    }
}

impl<F: SymbolTrait + Display> fmt::Display for Query<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[Query]\n")?;
        for c in &self.cond {
            write!(f, "{}", c)?;
        }
        Ok(())
    }
}
impl<F: SymbolTrait + Display> fmt::Display for ICTRS<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n{}", self.ctrs, self.query)
    }
}

impl<F: SymbolTrait + Display> fmt::Display for ITRS<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n[Query]\n{}", self.trs, self.query)
    }
}
