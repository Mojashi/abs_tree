use std::sync::atomic::AtomicU32;

pub trait SymbolTrait: Clone + Eq + std::hash::Hash {
    fn new() -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term<F: SymbolTrait> {
    Variable { symbol: u32 },
    Function { symbol: F, children: Vec<Term<F>> },
}

impl<F: SymbolTrait> Term<F> {
    pub fn children(&self) -> Vec<&Term<F>> {
        match self {
            Term::Variable { .. } => vec![],
            Term::Function { children, .. } => children.iter().collect(),
        }
    }
}

static mut SYMBOL_COUNTER: AtomicU32 = AtomicU32::new(0);
pub fn new_symbol() -> u32 {
    unsafe { SYMBOL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst) }
}

impl SymbolTrait for String {
    fn new() -> Self {
        new_symbol().to_string()
    }
}
impl SymbolTrait for u32 {
    fn new() -> Self {
        new_symbol() as u32
    }
}
