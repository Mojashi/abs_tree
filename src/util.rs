use crate::{
    abstractor::create_abstraction_merge_as_can,
    barmc::barmc::BARMC,
    trs::{
        ictrs::{ICTRS, ITRS},
        term::SymbolTrait,
    },
    ts_impl::TransitionSystemITRS,
};

impl<F: SymbolTrait> ICTRS<F> {
    pub fn solve(&self) -> bool {
        let itrs: ITRS<u32> = self.to_itrs().rename_symbols_to_u32();
        let is_query_rhs_ground = itrs.query.rhs.is_ground();
        //assert!(is_query_rhs_ground);

        let ts = TransitionSystemITRS::new(itrs);

        let merge_abstractor = Box::new(create_abstraction_merge_as_can);
        let mut barmc = BARMC::new(ts, merge_abstractor);
        barmc.step_until_stop()
    }
}
