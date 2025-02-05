use std::collections::HashMap;

use itertools::Itertools;

use crate::{
    abstractor::create_abstraction_merge_as_can,
    barmc::barmc::{BARMCResult, BARMC},
    trs::{ictrs::ICTRS, term::SymbolTrait, trs::Fun},
    ts_impl::TransitionSystemITRS,
};

impl<F: SymbolTrait> ICTRS<F> {
    pub fn solve(&self) -> bool {
        let (itrs, sym_map) = self.to_itrs().rename_symbols_to_u32();
        let sym_map_rev = sym_map
            .iter()
            .map(|(k, v)| (*v, k.clone()))
            .collect::<HashMap<_, _>>();

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
                    .skip(1)
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
