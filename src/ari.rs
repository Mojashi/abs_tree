use std::collections::HashMap;
use crate::{
    ictrs::{Query, ICTRS}, sexp::SExpression, term::Term, trs::{ConditionalRule, Fun, Rule, CTRS}
};
/*
   CTRS ::= ( format CTRS cond-type ) fun* rule*
cond-type ::= oriented | join | semi-equational
     rule ::= ( rule term term cond* )
     cond ::= ( = term term )
  INF ::= iTRS | iCTRS
 iTRS ::= ( format TRS :problem infeasibility ) fun* rule* query
iCTRS ::= ( format CTRS cond-type :problem infeasibility ) fun* rule* query
query ::= ( infeasible? cond+ )
 cond ::= ( = term term )
 */

/* Example
(format CTRS oriented :problem infeasibility)
(fun 0 0)
(fun cons 2)
(fun false 0)
(fun insert 2)
(fun lte 2)
(fun nil 0)
(fun ordered 1)
(fun s 1)
(fun true 0)
(rule (lte 0 n) true)
(rule (lte (s m) 0) false)
(rule (lte (s m) (s n)) (lte m n))
(rule (insert nil m) (cons m nil))
(rule (insert (cons n l) m) (cons m (cons n l)) (= (lte m n) true))
(rule (insert (cons n l) m) (cons n (insert l m)) (= (lte m n) false))
(rule (ordered nil) true)
(rule (ordered (cons m nil)) true)
(rule (ordered (cons m (cons n l))) (ordered (cons n l)) (= (lte m n) true))
(rule (ordered (cons m (cons n l))) false (= (lte m n) false))
(infeasible? (= (lte x3 x1) true) (= (lte x3 x1) false))
*/


pub fn parse_file(file: &str) -> Result<ICTRS<String>, &'static str> {
    let s = std::fs::read_to_string(file).unwrap();
    parse_lines(s.lines())
}
pub fn parse_lines(lines: std::str::Lines<'_>) -> Result<ICTRS<String>, &'static str> {
    let mut fun: Vec<Fun<String>> = Vec::new();
    let mut vars_map: HashMap<String, u32> = HashMap::new();
    let mut rule: Vec<ConditionalRule<String>> = Vec::new();
    let mut query: Query<String> = Query { cond: Vec::new() };

    fn map_sexp_to_term(
        sexp: &SExpression,
        funs: &Vec<Fun<String>>,
        vars_map: &mut HashMap<String, u32>,
    ) -> Term<String> {
        if let Some(fun) = funs.iter().find(|f| f.symbol == sexp.name) {
            assert_eq!(fun.arity, sexp.children.len());
            return Term::Function {
                symbol: sexp.name.clone(),
                children: sexp
                    .children
                    .iter()
                    .map(|c| map_sexp_to_term(c, funs, vars_map))
                    .collect(),
            };
        } else {
            let vlen = vars_map.len();
            return Term::Variable {
                symbol: vars_map
                    .entry(sexp.name.clone())
                    .or_insert(vlen as u32)
                    .clone(),
            };
        }
    }

    fn parse_cond(
        sexp: &SExpression,
        funs: &Vec<Fun<String>>,
        vars_map: &mut HashMap<String, u32>,
    ) -> Rule<String> {
        assert_eq!(sexp.name, "=");
        let lhs = map_sexp_to_term(&sexp.children[0], funs, vars_map);
        let rhs = map_sexp_to_term(&sexp.children[1], funs, vars_map);
        Rule { lhs, rhs }
    }

    for l in lines.map(|s| s.trim()).filter(|s| !s.starts_with(";") && !s.is_empty()) {
        let (sexp, _) = SExpression::parse_sexp(l)?;
        match sexp.name.as_str() {
            "fun" => {
                let sym = sexp.children[0].name.clone();
                let arity = sexp.children[1].name.parse().unwrap();
                fun.push(Fun {
                    symbol: sym,
                    arity,
                    condition_intermidiate_fun: false,
                });
            }
            "rule" => {
                let lhs = map_sexp_to_term(&sexp.children[0], &fun, &mut vars_map);
                let rhs = map_sexp_to_term(&sexp.children[1], &fun, &mut vars_map);
                let conds = sexp.children[2..]
                    .iter()
                    .map(|c| parse_cond(c, &fun, &mut vars_map))
                    .collect();
                rule.push(ConditionalRule { lhs, rhs, conds });
            }
            "infeasible?" => {
                query.cond = sexp
                    .children
                    .iter()
                    .map(|c| parse_cond(c, &fun, &mut vars_map))
                    .collect();
            }
            "format" => {
                // ignore
            }
            _ => {
                return Err("Invalid S-expression");
            }
        }
    }

    Ok(ICTRS {
        ctrs: CTRS {
            rules: rule,
            funs: fun,
        },
        query,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let s = "(format CTRS oriented :problem infeasibility)
(fun 0 0)
(fun cons 2)
(fun false 0)
(fun insert 2)
(fun lte 2)
(fun nil 0)
(fun ordered 1)
(fun s 1)
(fun true 0)
(rule (lte 0 n) true)
(rule (lte (s m) 0) false)
(rule (lte (s m) (s n)) (lte m n))
(rule (insert nil m) (cons m nil))
(rule (insert (cons n l) m) (cons m (cons n l)) (= (lte m n) true))
(rule (insert (cons n l) m) (cons n (insert l m)) (= (lte m n) false))
(rule (ordered nil) true)
(rule (ordered (cons m nil)) true)
(rule (ordered (cons m (cons n l))) (ordered (cons n l)) (= (lte m n) true))
(rule (ordered (cons m (cons n l))) false (= (lte m n) false))
(infeasible? (= (lte x3 x1) true) (= (lte x3 x1) false))";
        let ict = parse_lines(s.lines()).unwrap();
        assert_eq!(ict.ctrs.funs.len(), 9);
        assert_eq!(ict.ctrs.rules.len(), 10);
        assert_eq!(ict.query.cond.len(), 2);

        println!("{}", ict.ctrs);
        println!("{}", ict.ctrs.to_trs());
    }
}
