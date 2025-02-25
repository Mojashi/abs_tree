use itertools::Itertools;
use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use super::term::{GroundTerm, SymbolTrait, Term};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rule<F: SymbolTrait> {
    pub lhs: Term<F>,
    pub rhs: Term<F>,
}

impl Rule<String> {
    // uppercase -> function, lowercase -> variable
    // (F x y -> G x y)
    pub fn from_string(s: &str) -> Result<Self, &'static str> {
        let (lhs, rhs) = s.split("->").collect_tuple().ok_or("Invalid rule")?;
        let lhs = GroundTerm::from_string(lhs.trim())?;
        let rhs = GroundTerm::from_string(rhs.trim())?;
        let (l_funcs, r_funcs) = (lhs.extract_funcs(), rhs.extract_funcs());
        let var_symbols = l_funcs
            .union(&r_funcs)
            .filter(|f| f.symbol.chars().next().unwrap().is_lowercase())
            .collect::<Vec<_>>();
        let mut symbol_map = HashMap::new();
        for (i, f) in var_symbols.iter().enumerate() {
            symbol_map.insert(f.symbol.clone(), i as u32);
        }
        let lhs = lhs.to_term_with_varmap(&symbol_map);
        let rhs = rhs.to_term_with_varmap(&symbol_map);
        Ok(Rule { lhs, rhs })
    }
}

impl<F> Rule<F>
where
    F: SymbolTrait,
{
    pub fn rename_symbols(&self, fun_map: &HashMap<F, u32>) -> Rule<u32> {
        Rule {
            lhs: self.lhs.rename_symbols(fun_map),
            rhs: self.rhs.rename_symbols(fun_map),
        }
    }
    pub fn is_right_linear(&self) -> bool {
        self.rhs.is_linear()
    }
    pub fn is_left_linear(&self) -> bool {
        self.lhs.is_linear()
    }

    pub fn is_right_vars_contained_in_left(&self) -> bool {
        self.rhs.vars().is_subset(&self.lhs.vars())
    }

    pub fn reversed(&self) -> Rule<F> {
        Rule {
            lhs: self.rhs.clone(),
            rhs: self.lhs.clone(),
        }
    }

    pub fn apply(&self, term: &GroundTerm<F>) -> Vec<GroundTerm<F>> {
        term.match_term(&self.lhs)
            .into_iter()
            .map(move |m| self.rhs.subst_ground(&m))
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TRS<F: SymbolTrait> {
    pub funs: Vec<Fun<F>>,
    pub rules: Vec<Rule<F>>,
    pub intermidiate_funs: CondIntermidiateFuns<F>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConditionalRule<F: SymbolTrait> {
    pub lhs: Term<F>,
    pub rhs: Term<F>,
    pub conds: Vec<Rule<F>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CondIntermidiateFuns<F: SymbolTrait> {
    true_fun: Fun<F>,
    and_fun: Fun<F>,
    eq_fun: Fun<F>,
    pair_fun: Fun<F>,
    cond_fun: Fun<F>,
}
impl <F: SymbolTrait> CondIntermidiateFuns<F> {
    pub fn new() -> Self {
        CondIntermidiateFuns {
            true_fun: Fun {
                symbol: F::new(),
                arity: 0,
                condition_intermidiate_fun: true,
            },
            and_fun: Fun {
                symbol: F::new(),
                arity: 2,
                condition_intermidiate_fun: true,
            },
            eq_fun: Fun {
                symbol: F::new(),
                arity: 2,
                condition_intermidiate_fun: true,
            },
            pair_fun: Fun {
                symbol: F::new(),
                arity: 2,
                condition_intermidiate_fun: true,
            },
            cond_fun: Fun {
                symbol: F::new(),
                arity: 2,
                condition_intermidiate_fun: true,
            },
        }
    }
    pub fn to_vec(&self) -> Vec<Fun<F>> {
        vec![
            self.true_fun.clone(),
            self.and_fun.clone(),
            self.eq_fun.clone(),
            self.pair_fun.clone(),
            self.cond_fun.clone(),
        ]
    }
}
pub fn crule_to_rule<F: SymbolTrait>(
    crule: ConditionalRule<F>,
    intermidiate_fun: &CondIntermidiateFuns<F>,
) -> Rule<F> {
    if crule.conds.len() == 0 {
        Rule {
            lhs: crule.lhs.clone(),
            rhs: crule.rhs.clone(),
        }
    } else {
        let eq_terms = crule
            .conds
            .iter()
            .map(|c| Term::Function {
                symbol: intermidiate_fun.eq_fun.symbol.clone(),
                children: vec![c.lhs.clone(), c.rhs.clone()],
            })
            .collect::<Vec<_>>();
        let and_term = eq_terms.iter().fold(
            Term::Function {
                symbol: intermidiate_fun.true_fun.symbol.clone(),
                children: vec![],
            },
            |acc, eq| Term::Function {
                symbol: intermidiate_fun.and_fun.symbol.clone(),
                children: vec![acc, eq.clone()],
            },
        );
        Rule {
            lhs: Term::Function {
                symbol: intermidiate_fun.cond_fun.symbol.clone(),
                children: vec![
                    Term::Function {
                        symbol: intermidiate_fun.pair_fun.symbol.clone(),
                        children: vec![crule.lhs.clone(), crule.rhs.clone()],
                    },
                    and_term,
                ],
            },
            rhs: crule.rhs.clone(),
        }
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

// l -> c(d(l,r),and(eq(a,b),eq(c,d)))
// c(d(l,r),true) -> r
// and(true,true)->true
// eq(a,a)->true

impl<F: SymbolTrait> CTRS<F> {
    pub fn to_trs(&self) -> TRS<F> {
        let true_fun = Fun {
            symbol: F::new(),
            arity: 0,
            condition_intermidiate_fun: true,
        };
        let and_fun = Fun {
            symbol: F::new(),
            arity: 2,
            condition_intermidiate_fun: true,
        };
        let eq_fun = Fun {
            symbol: F::new(),
            arity: 2,
            condition_intermidiate_fun: true,
        };
        let pair_fun = Fun {
            symbol: F::new(),
            arity: 2,
            condition_intermidiate_fun: true,
        };
        let cond_fun = Fun {
            symbol: F::new(),
            arity: 2,
            condition_intermidiate_fun: true,
        };
        let mut rules = Vec::new();
        rules.push(Rule {
            lhs: Term::Function {
                symbol: eq_fun.symbol.clone(),
                children: vec![Term::Variable { symbol: 0 }, Term::Variable { symbol: 0 }],
            },
            rhs: Term::Function {
                symbol: true_fun.symbol.clone(),
                children: vec![],
            },
        });
        rules.push(Rule {
            lhs: Term::Function {
                symbol: and_fun.symbol.clone(),
                children: vec![
                    Term::Function {
                        symbol: true_fun.symbol.clone(),
                        children: vec![],
                    },
                    Term::Function {
                        symbol: true_fun.symbol.clone(),
                        children: vec![],
                    },
                ],
            },
            rhs: Term::Function {
                symbol: true_fun.symbol.clone(),
                children: vec![],
            },
        });
        rules.push(Rule {
            lhs: Term::Function {
                symbol: cond_fun.symbol.clone(),
                children: vec![
                    Term::Function {
                        symbol: pair_fun.symbol.clone(),
                        children: vec![Term::Variable { symbol: 0 }, Term::Variable { symbol: 1 }],
                    },
                    Term::Function {
                        symbol: true_fun.symbol.clone(),
                        children: vec![],
                    },
                ],
            },
            rhs: Term::Variable { symbol: 1 },
        });
        let intermidiate_funs = CondIntermidiateFuns {
            true_fun,
            and_fun,
            eq_fun,
            pair_fun,
            cond_fun,
        };


        for r in self.rules.iter() {
            if r.conds.len() == 0 {
                rules.push(Rule {
                    lhs: r.lhs.clone(),
                    rhs: r.rhs.clone(),
                });
            } else {
                rules.push(crule_to_rule(r.clone(), &intermidiate_funs));
            }
        }

        TRS {
            funs: self.funs.clone(),
            rules,
            intermidiate_funs,
        }
    }
}

impl<F: SymbolTrait> TRS<F> {
    pub fn rename_symbols_to_u32(&self) -> (TRS<u32>, HashMap<F, u32>) {
        let mut funs = Vec::new();
        let mut intermidiate_funs = CondIntermidiateFuns {
            true_fun: Fun {
                symbol: u32::new(),
                arity: 0,
                condition_intermidiate_fun: true,
            },
            and_fun: Fun {
                symbol: u32::new(),
                arity: 2,
                condition_intermidiate_fun: true,
            },
            eq_fun: Fun {
                symbol: u32::new(),
                arity: 2,
                condition_intermidiate_fun: true,
            },
            pair_fun: Fun {
                symbol: u32::new(),
                arity: 2,
                condition_intermidiate_fun: true,
            },
            cond_fun: Fun {
                symbol: u32::new(),
                arity: 2,
                condition_intermidiate_fun: true,
            },
        };
        let mut fun_map: HashMap<F, u32> = HashMap::new();
        fun_map.insert(
            self.intermidiate_funs.true_fun.symbol.clone(),
            intermidiate_funs.true_fun.symbol.clone(),
        );
        fun_map.insert(
            self.intermidiate_funs.and_fun.symbol.clone(),
            intermidiate_funs.and_fun.symbol.clone(),
        );
        fun_map.insert(
            self.intermidiate_funs.eq_fun.symbol.clone(),
            intermidiate_funs.eq_fun.symbol.clone(),
        );
        fun_map.insert(
            self.intermidiate_funs.pair_fun.symbol.clone(),
            intermidiate_funs.pair_fun.symbol.clone(),
        );
        fun_map.insert(
            self.intermidiate_funs.cond_fun.symbol.clone(),
            intermidiate_funs.cond_fun.symbol.clone(),
        );
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

        (
            TRS {
                funs,
                rules,
                intermidiate_funs,
            },
            fun_map,
        )
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
