use std::{collections::HashMap, sync::atomic::AtomicU32};

use crate::sexp::SExpression;

pub trait SymbolTrait: Clone + Eq + std::hash::Hash + std::fmt::Debug {
    fn new() -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GroundTerm<F: SymbolTrait> {
    pub symbol: F,
    pub children: Vec<GroundTerm<F>>,
}

impl GroundTerm<String> {
    pub fn to_string(&self) -> String {
        if self.children.is_empty() {
            self.symbol.clone()
        } else {
            format!(
                "({} {})",
                self.symbol,
                self.children.iter().map(|c| c.to_string()).collect::<Vec<_>>().join(" ")
            )
        }
    }
    pub fn from_string(s: &str) -> Result<Self, &'static str> {
        let sexp = SExpression::parse_sexp(s)?.0;
        Self::from_sexp(&sexp)
    }
    pub fn from_sexp(sexp: &SExpression) -> Result<Self, &'static str> {
        if sexp.children.is_empty() {
            Ok(GroundTerm {
                symbol: sexp.name.clone(),
                children: vec![],
            })
        } else {
            Ok(GroundTerm {
                symbol: sexp.name.clone(),
                children: sexp
                    .children
                    .iter()
                    .map(|c| Self::from_sexp(c))
                    .collect::<Result<Vec<_>, _>>()?,
            })
        }
    }
}

#[test]
fn test_parse_term() {
    let s = "(f (g a) b)";
    let t = GroundTerm::from_string(s).unwrap();
    assert_eq!(t.to_string(), s);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term<F: SymbolTrait> {
    Variable { symbol: u32 },
    Function { symbol: F, children: Vec<Term<F>> },
}

impl<F: SymbolTrait> Term<F> {
    pub fn is_ground(&self) -> bool {
        match self {
            Term::Variable { .. } => false,
            Term::Function { children, .. } => children.iter().all(|c| c.is_ground()),
        }
    }
    pub fn to_ground(&self) -> Option<GroundTerm<F>> {
        match self {
            Term::Variable { .. } => None,
            Term::Function { symbol, children } => Some(GroundTerm {
                symbol: symbol.clone(),
                children: children.iter().map(|c| c.to_ground().unwrap()).collect(),
            }),
        }
    }
    pub fn size(&self) -> usize {
        match self {
            Term::Variable { .. } => 1,
            Term::Function { children, .. } => 1 + children.iter().map(|c| c.size()).sum::<usize>(),
        }
    }

    pub fn children(&self) -> Vec<&Term<F>> {
        match self {
            Term::Variable { .. } => vec![],
            Term::Function { children, .. } => children.iter().collect(),
        }
    }

    pub fn map<V: SymbolTrait>(&self, f: &impl Fn(F) -> V) -> Term<V> {
        match self {
            Term::Variable { symbol } => Term::Variable { symbol: *symbol },
            Term::Function { symbol, children } => Term::Function {
                symbol: f(symbol.clone()),
                children: children.iter().map(|c| c.map(f)).collect(),
            },
        }
    }

    pub fn rename_symbols<G: SymbolTrait>(&self, s_map: &HashMap<F, G>) -> Term<G> {
        self.map(&|s| s_map.get(&s).unwrap().clone())
    }
}

static mut SYMBOL_COUNTER: AtomicU32 = AtomicU32::new(100);
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
