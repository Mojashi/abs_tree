use crate::term::{SymbolTrait, Term};
use itertools::Itertools;
use std::{collections::HashMap, fmt::{self, Display}};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rule<F: SymbolTrait> {
    pub lhs: Term<F>,
    pub rhs: Term<F>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TRS<F: SymbolTrait> {
    pub funs: Vec<Fun<F>>,
    pub rules: Vec<Rule<F>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConditionalRule<F: SymbolTrait> {
    pub lhs: Term<F>,
    pub rhs: Term<F>,
    pub conds: Vec<Rule<F>>,
}

fn gen_conditional_intermidiate_rule<F: SymbolTrait>(f: Fun<F>) -> Rule<F> {
    let lhs = Term::Function {
        symbol: f.symbol,
        children: vec![
            Term::Variable { symbol: 0 as u32 },
            Term::Variable { symbol: 1 as u32 },
        ]
        .into_iter()
        .chain((0..(f.arity * 2)).map(|j| Term::Variable {
            symbol: (j / 2 + 2) as u32,
        }))
        .collect(),
    };
    let rhs = Term::Variable { symbol: 1 as u32 };
    Rule { lhs, rhs }
}

pub fn crule_to_rule<F: SymbolTrait>(
    crule: ConditionalRule<F>,
    intermidiate_fun: Fun<F>,
) -> Rule<F> {
    if crule.conds.len() == 0 {
        Rule {
            lhs: crule.lhs.clone(),
            rhs: crule.rhs.clone(),
        }
    } else {
        let lhs = crule.lhs.clone();
        let rhs = Term::Function {
            symbol: intermidiate_fun.symbol,
            children: vec![crule.lhs.clone(), crule.rhs.clone()]
                .into_iter()
                .chain(
                    crule
                        .conds
                        .iter()
                        .flat_map(|c| vec![c.lhs.clone(), c.rhs.clone()]),
                )
                .collect(),
        };
        Rule { lhs, rhs }
    }
}

impl<F: SymbolTrait> ConditionalRule<F> {
    pub fn to_trs_rule(&self) -> Rule<F> {
        Rule {
            lhs: self.lhs.clone(),
            rhs: self.rhs.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fun<F: SymbolTrait> {
    pub symbol: F,
    pub arity: usize,
    pub condition_intermidiate_fun: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CTRS<F: SymbolTrait> {
    pub funs: Vec<Fun<F>>,
    pub rules: Vec<ConditionalRule<F>>,
}

impl<F: SymbolTrait> CTRS<F> {
    pub fn to_trs(&self) -> TRS<F> {
        let max_num_conds: usize = self.rules.iter().map(|r| r.conds.len()).max().unwrap();
        let intermidiate_functions = (1..=max_num_conds)
            .map(|arity| Fun {
                symbol: F::new(),
                arity,
                condition_intermidiate_fun: true,
            })
            .collect_vec();

        let mut rules = Vec::new();
        for r in self.rules.iter() {
            if r.conds.len() == 0 {
                rules.push(Rule {
                    lhs: r.lhs.clone(),
                    rhs: r.rhs.clone(),
                });
            } else {
                let i_fun = intermidiate_functions[r.conds.len() - 1].clone();
                rules.push(crule_to_rule(r.clone(), i_fun));
            }
        }

        for i_fun in intermidiate_functions.iter() {
            rules.push(gen_conditional_intermidiate_rule(i_fun.clone()));
        }
        TRS {
            funs: self
                .funs
                .iter()
                .cloned()
                .chain(intermidiate_functions)
                .collect(),
            rules,
        }
    }
}

impl<F: SymbolTrait> TRS<F> {
    pub fn get_or_insert_condition_intermidiate_func(&mut self, arity: usize) -> Fun<F> {
        if let Some(f) = self
            .funs
            .iter()
            .find(|f| f.condition_intermidiate_fun && f.arity == arity)
            .cloned()
        {
            f
        } else {
            let f = Fun {
                symbol: F::new(),
                arity,
                condition_intermidiate_fun: true,
            };
            self.funs.push(f.clone());
            f
        }
    }

    pub fn rename_symbols_to_u32(&self) -> (TRS<u32>, HashMap<F, u32>) {
        let mut funs = Vec::new();
        let mut fun_map: HashMap<F, u32> = HashMap::new();
        for f in self.funs.iter() {
            let new_f = Fun {
                symbol: fun_map
                    .entry(f.symbol.clone())
                    .or_insert_with(u32::new)
                    .clone(),
                arity: f.arity,
                condition_intermidiate_fun: f.condition_intermidiate_fun,
            };
            funs.push(new_f);
        }

        let mut rules = Vec::new();
        for r in self.rules.iter() {
            let new_rule = Rule {
                lhs: r.lhs.rename_symbols(&fun_map),
                rhs: r.rhs.rename_symbols(&fun_map),
            };
            rules.push(new_rule);
        }

        (TRS { funs, rules }, fun_map)
    }
}

impl<F: SymbolTrait + Display> fmt::Display for Term<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Variable { symbol } => write!(f, "v_{}", symbol),
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
impl<F: SymbolTrait + Display> fmt::Display for Rule<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.lhs, self.rhs)
    }
}
impl<F: SymbolTrait + Display> fmt::Display for ConditionalRule<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.lhs, self.rhs)?;
        if self.conds.len() != 0 {
            write!(f, " | ")?;
        }
        for (i, cond) in self.conds.iter().enumerate() {
            if i != 0 {
                write!(f, " ∧ ")?;
            }
            write!(f, "{}", cond)?;
        }
        Ok(())
    }
}

impl<F: SymbolTrait + Display> fmt::Display for TRS<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[funs]\n")?;
        for fun in self.funs.iter() {
            write!(f, "fun {} {}", fun.symbol, fun.arity)?;
            if fun.condition_intermidiate_fun {
                write!(f, " *intermidiate")?;
            }
            write!(f, "\n")?;
        }
        write!(f, "[rules]\n")?;
        for (i, rule) in self.rules.iter().enumerate() {
            if i != 0 {
                write!(f, "\n")?;
            }
            write!(f, "{}", rule)?;
        }
        Ok(())
    }
}

impl<F: SymbolTrait + Display> fmt::Display for CTRS<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[funs]\n")?;
        for fun in self.funs.iter() {
            write!(f, "fun {} {}", fun.symbol, fun.arity)?;
            if fun.condition_intermidiate_fun {
                write!(f, " *intermidiate")?;
            }
            write!(f, "\n")?;
        }
        write!(f, "[rules]\n")?;
        for (i, rule) in self.rules.iter().enumerate() {
            if i != 0 {
                write!(f, "\n")?;
            }
            write!(f, "{}", rule)?;
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
