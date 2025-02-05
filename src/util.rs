use std::collections::HashMap;

use itertools::Itertools;

use crate::{
    abstractor::create_abstraction_merge_as_can,
    barmc::barmc::{BARMCResult, BARMC},
    trs::{
        ictrs::{ICTRS, ITRS},
        term::SymbolTrait,
        trs::Fun,
    },
    ts_impl::TransitionSystemITRS,
};

impl<F: SymbolTrait> ICTRS<F> {
    pub fn solve(&self) -> bool {
        let (mut itrs, sym_map) = self.to_itrs().rename_symbols_to_u32();
        let mut sym_map_rev = sym_map
            .iter()
            .map(|(k, v)| (*v, k.clone()))
            .collect::<HashMap<_, _>>();

        let has_zero_arity_funs = itrs.trs.funs.iter().any(|f| f.arity == 0);
        if !has_zero_arity_funs {
            let symbol = u32::new();
            itrs.trs.funs.push(Fun {
                symbol,
                arity: 0,
                condition_intermidiate_fun: false,
            });
            sym_map_rev.insert(symbol, F::new());
        }
        let is_query_rhs_ground = itrs.query.rhs.is_ground();
        //assert!(is_query_rhs_ground);

        println!("itrs: {}", itrs);
        let ts = TransitionSystemITRS::new(itrs);

        let merge_abstractor = Box::new(create_abstraction_merge_as_can);
        let mut barmc = BARMC::new(ts, merge_abstractor);
        match barmc.step_until_stop() {
            BARMCResult::Closed => true,
            BARMCResult::BadPath(badpath) => {
                println!("symbol map: {:?}", sym_map_rev);
                let badpath = badpath
                    .into_iter()
                    .map(|g_term| g_term.map(&|s| sym_map_rev[&s].clone()))
                    .collect_vec();
                println!("Bad path:");
                for term in badpath.iter() {
                    println!("{}", term);
                }
                false
            }
        }
    }
}
