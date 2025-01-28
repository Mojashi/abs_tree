use std::io::Write;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SExpression {
    pub name: String,
    pub children: Vec<SExpression>,
}
impl SExpression {
    pub fn parse_sexp(s: &str) -> Result<(SExpression, &str), &'static str> {
        // assert!(s.starts_with("("));
        if !s.starts_with("(") {
            return Err("Invalid S-expression");
        }
        std::io::stdout().flush().unwrap();

        let mut s = s[1..].trim_start();
        let mut ret = Vec::<SExpression>::new();
        while !s.starts_with(")") {
            if s.starts_with("(") {
                let (child, rest) = Self::parse_sexp(s)?;
                ret.push(child);
                s = rest;
            } else {
                let word = s
                    .split_terminator(|c: char| c.is_whitespace() || c == ')')
                    .next()
                    .unwrap();
                s = &s[word.len()..];
                ret.push(SExpression {
                    name: word.to_string(),
                    children: Vec::new(),
                });
            }
            s = s.trim_start();
        }
        s = &s[1..];
        return Ok((
            SExpression {
                name: ret[0].name.clone(),
                children: ret[1..].to_vec(),
            },
            s,
        ));
    }
}