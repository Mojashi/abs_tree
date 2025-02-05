use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::{self, write, Display},
    iter::once,
    sync::atomic::AtomicU32,
};

use maplit::hashmap;
use serde::{Deserialize, Serialize};

use super::{sexp::SExpression, trs::Fun};

pub trait SymbolTrait: Clone + Eq + std::hash::Hash + std::fmt::Debug + Display {
    fn new() -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct GroundTerm<F: SymbolTrait> {
    pub symbol: F,
    pub children: Vec<GroundTerm<F>>,
}

type TermPos = VecDeque<usize>;

impl GroundTerm<String> {
    pub fn to_string(&self) -> String {
        if self.children.is_empty() {
            self.symbol.clone()
        } else {
            format!(
                "({} {})",
                self.symbol,
                self.children
                    .iter()
                    .map(|c| c.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
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

impl<F: SymbolTrait> GroundTerm<F> {
    pub fn extract_funcs(&self) -> HashSet<Fun<F>> {
        let mut ret = HashSet::new();
        for c in self.children.iter() {
            ret.extend(c.extract_funcs());
        }
        ret.insert(Fun {
            symbol: self.symbol.clone(),
            arity: self.children.len(),
            condition_intermidiate_fun: false,
        });
        ret
    }

    pub fn to_term_with_varmap(&self, var_map: &HashMap<F, u32>) -> Term<F> {
        match var_map.get(&self.symbol) {
            Some(&v) => Term::Variable { symbol: v },
            None => Term::Function {
                symbol: self.symbol.clone(),
                children: self
                    .children
                    .iter()
                    .map(|c| c.to_term_with_varmap(var_map))
                    .collect(),
            },
        }
    }

    pub fn subst_term_with_pos(&self, pos: &[usize], t: &Term<F>) -> Term<F> {
        if pos.is_empty() {
            t.clone()
        } else {
            let cur = pos[0];
            let rest = &pos[1..];
            let children = self
                .children
                .iter()
                .enumerate()
                .map(|(idx, c)| {
                    if idx == cur {
                        c.subst_term_with_pos(rest, t)
                    } else {
                        c.to_term_with_varmap(&hashmap! {})
                    }
                })
                .collect();
            Term::Function {
                symbol: self.symbol.clone(),
                children,
            }
        }
    }

    // subtermにもマッチする
    pub fn match_term_with_subterm(
        &self,
        t: &Term<F>,
    ) -> Vec<(HashMap<u32, GroundTerm<F>>, TermPos)> {
        self.children
            .iter()
            .enumerate()
            .flat_map(|(children_idx, c)| {
                c.match_term_with_subterm(t)
                    .into_iter()
                    .map(move |(a, mut p)| {
                        p.push_front(children_idx.clone());
                        (a, p)
                    })
            })
            .chain(self.match_term(t).into_iter().map(|a| (a, VecDeque::new())))
            .collect()
    }

    pub fn match_term(&self, t: &Term<F>) -> Option<HashMap<u32, GroundTerm<F>>> {
        match t {
            Term::Variable { symbol } => Some(hashmap! {*symbol => self.clone()}),
            Term::Function { symbol, children } => {
                if self.symbol != *symbol || self.children.len() != children.len() {
                    return None;
                }

                let mut match_here: HashMap<u32, GroundTerm<F>> = HashMap::new();
                for (c, t) in self.children.iter().zip(children.iter()) {
                    let r2 = c.match_term(t);
                    if r2.is_none() {
                        return None;
                    }
                    for r2map in r2.iter() {
                        for (k, v) in r2map.iter() {
                            if match_here.contains_key(k) {
                                if match_here[k] != *v {
                                    return None;
                                }
                            } else {
                                match_here.insert(*k, v.clone());
                            }
                        }
                    }
                }
                Some(match_here)
            }
        }
    }

    pub fn map<V: SymbolTrait>(&self, f: &impl Fn(F) -> V) -> GroundTerm<V> {
        GroundTerm {
            symbol: f(self.symbol.clone()),
            children: self.children.iter().map(|c| c.map(f)).collect(),
        }
    }
}

impl<F: SymbolTrait> fmt::Display for GroundTerm<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.symbol,
            self.children
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

#[test]
fn test_extract_funcs() {
    let s = "(f (g a) b)";
    let t = GroundTerm::from_string(s).unwrap();
    let funcs = t.extract_funcs();
    assert_eq!(
        funcs,
        HashSet::from([
            Fun {
                symbol: "f".to_string(),
                arity: 2,
                condition_intermidiate_fun: false
            },
            Fun {
                symbol: "g".to_string(),
                arity: 1,
                condition_intermidiate_fun: false
            },
            Fun {
                symbol: "a".to_string(),
                arity: 0,
                condition_intermidiate_fun: false
            },
            Fun {
                symbol: "b".to_string(),
                arity: 0,
                condition_intermidiate_fun: false
            },
        ])
    );
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

    pub fn is_linear(&self) -> bool {
        fn is_linear_rec<F: SymbolTrait>(t: &Term<F>, vars: &mut HashSet<u32>) -> bool {
            match t {
                Term::Variable { symbol } => vars.insert(*symbol),
                Term::Function { children, .. } => children.iter().all(|c| is_linear_rec(c, vars)),
            }
        }
        let mut vars = HashSet::new();
        is_linear_rec(self, &mut vars)
    }

    pub fn vars(&self) -> HashSet<u32> {
        fn vars_rec<F: SymbolTrait>(t: &Term<F>, vars: &mut HashSet<u32>) {
            match t {
                Term::Variable { symbol } => {
                    vars.insert(*symbol);
                }
                Term::Function { children, .. } => {
                    for c in children {
                        vars_rec(c, vars);
                    }
                }
            }
        }
        let mut vars = HashSet::new();
        vars_rec(self, &mut vars);
        vars
    }

    pub fn subst_ground(&self, s_map: &HashMap<u32, GroundTerm<F>>) -> GroundTerm<F> {
        match self {
            Term::Variable { symbol } => s_map[symbol].clone(),
            Term::Function { symbol, children } => GroundTerm {
                symbol: symbol.clone(),
                children: children.iter().map(|c| c.subst_ground(s_map)).collect(),
            },
        }
    }
    pub fn subst_term(&self, s_map: &HashMap<u32, Term<F>>) -> Term<F> {
        match self {
            Term::Variable { symbol } => s_map.get(symbol).unwrap_or(self).clone(),
            Term::Function { symbol, children } => Term::Function {
                symbol: symbol.clone(),
                children: children.iter().map(|c| c.subst_term(s_map)).collect(),
            },
        }
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

#[test]
fn test_match() {
    let gt = GroundTerm::from_string("(104 (102) (102) (101 (103 (102)) (102)) (103 (102)))").unwrap();
    let pat = GroundTerm::from_string("(104 (102) (102) (101 v_0 v_0) (103 v_0))")
        .unwrap()
        .to_term_with_varmap(&hashmap! {"v_0".to_string() => 0});

    assert_eq!(gt.match_term_with_subterm(&pat), vec![]);
}
