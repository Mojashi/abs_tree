use std::collections::HashSet;

use super::transitionsystem::{ConfigLangTrait, ConfigTrait};

pub struct BadConfigs<Config: ConfigTrait> {
    pub bads: HashSet<Config>,
}

impl<Config: ConfigTrait> BadConfigs<Config> {
    pub fn new() -> Self {
        Self {
            bads: HashSet::new(),
        }
    }
    pub fn add_config(&mut self, conf: &Config) -> bool {
        self.bads.insert(conf.clone())
    }
    pub fn is_contains_bad(&self, seq: &impl ConfigLangTrait<Config = Config>) -> Option<Config> {
        self.bads.iter().find(|s| seq.accept(s)).cloned()
    }
}
