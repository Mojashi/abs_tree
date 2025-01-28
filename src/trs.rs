use std::fmt;

use crate::term::{SymbolTrait, Term};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rule<F: SymbolTrait> {
    pub lhs: Term<F>,
    pub rhs: Term<F>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TRS<F: SymbolTrait> {
    pub rules: Vec<Rule<F>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cond<F: SymbolTrait> {
    pub lhs: Term<F>,
    pub rhs: Term<F>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConditionalRule<F: SymbolTrait> {
    pub lhs: Term<F>,
    pub rhs: Term<F>,
    pub conds: Vec<Cond<F>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fun<F: SymbolTrait> {
    pub symbol: F,
    pub arity: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CTRS<F: SymbolTrait> {
    pub funs: Vec<Fun<F>>,
    pub rules: Vec<ConditionalRule<F>>,
}

impl<F: SymbolTrait> CTRS<F> {
    pub fn to_trs(&self) -> TRS<F> {
        let max_num_conds: usize = self.rules.iter().map(|r| r.conds.len()).max().unwrap();
        let intermidiate_symbols = (0..max_num_conds).map(|_| F::new()).collect::<Vec<F>>();

        let mut rules = Vec::new();
        for r in self.rules.iter() {
            if r.conds.len() == 0 {
                rules.push(Rule {
                    lhs: r.lhs.clone(),
                    rhs: r.rhs.clone(),
                });
            } else {
                let i_sym = intermidiate_symbols[r.conds.len() - 1].clone();
                let lhs = r.lhs.clone();
                let rhs = Term::Function {
                    symbol: i_sym,
                    children: vec![r.lhs.clone(), r.rhs.clone()]
                        .into_iter()
                        .chain(
                            r.conds
                                .iter()
                                .flat_map(|c| vec![c.lhs.clone(), c.rhs.clone()]),
                        )
                        .collect(),
                };
                rules.push(Rule { lhs, rhs });
            }
        }

        for (i, i_sym) in intermidiate_symbols.iter().enumerate() {
            let i = i + 1;
            // i_sym(0,1,2,2) -> 1
            let lhs = Term::Function {
                symbol: i_sym.clone(),
                children: vec![
                    Term::Variable { symbol: 0 as u32 },
                    Term::Variable { symbol: 1 as u32 },
                ]
                .into_iter()
                .chain((0..(i * 2)).map(|j| Term::Variable {
                    symbol: (j / 2 + 2) as u32,
                }))
                .collect(),
            };
            let rhs = Term::Variable { symbol: i as u32 };
            rules.push(Rule { lhs, rhs });
        }

        TRS { rules }
    }
}

impl fmt::Display for Term<String> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Variable { symbol } => write!(f, "v{}", symbol),
            Term::Function { symbol, children } => {
                write!(f, "{}(", symbol)?;
                for (i, child) in children.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", child)?;
                }
                write!(f, ")")
            }
        }
    }
}
impl fmt::Display for TRS<String> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for rule in &self.rules {
            println!("{} -> {} ", rule.lhs, rule.rhs);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::term::SymbolTrait;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    struct TestSymbol(String);

    impl SymbolTrait for TestSymbol {
        fn new() -> Self {
            TestSymbol("test".to_string())
        }
    }

    #[test]
    fn test_to_ctrs() {}
}
