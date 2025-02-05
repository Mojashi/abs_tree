use std::fmt::Display;

use serde::{Deserialize, Serialize};

pub trait ConfigTrait:
    Clone + Eq + std::hash::Hash + std::fmt::Debug + Serialize + for<'a> Deserialize<'a> + Display
{
}
pub trait ConfigLangTrait: Clone + std::fmt::Debug + Serialize + for<'a> Deserialize<'a> + PartialEq + Eq {
    type Config: ConfigTrait;
    fn accept(&self, c: &Self::Config) -> bool;
    fn includes(&self, c: &Self) -> bool;
    fn get_dot_label(&self) -> String;
    fn debug_this(&self, name: &str);
    fn is_equal(&self, c: &Self) -> bool {
        self.includes(c) && c.includes(self)
    }
}
pub trait OperationIdTrait: Clone + Eq + std::hash::Hash + std::fmt::Debug + Serialize + for<'a> Deserialize<'a> {
}

impl OperationIdTrait for usize {}

pub trait TransitionSystem: Clone {
    type ConfigLang: ConfigLangTrait<Config = Self::Config>;
    type Config: ConfigTrait;
    type OperationId: OperationIdTrait;

    fn init_states(&self) -> Vec<Self::ConfigLang>;
    fn find_bad_state(&self, c: &Self::ConfigLang) -> Option<Self::Config>;
    fn nexts(&self, conf: &Self::ConfigLang) -> Vec<(Self::OperationId, Self::ConfigLang)>;
    fn prev_within(
        &self,
        c: &Self::Config,
        within: &Self::ConfigLang,
        applied_operation: Self::OperationId,
    ) -> Option<Self::Config>;
}
