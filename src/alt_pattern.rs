#[derive(Debug)]
pub struct AltPattern {
    pat: Vec<PatternElem>,
}

#[derive(Debug)]
pub enum PatternElem {
    Static(Vec<char>),
    Alt(Vec<Vec<char>>),
}

impl PatternElem {
    pub fn matches(&self, inp: &str) -> bool {
        match self {
            PatternElem::Static(pat) => {
                for (&p,c) in pat.iter().zip(inp.chars()) {
                    if p != c && p != '?' {
                        return false;
                    }
                }
                true
            }

            PatternElem::Alt(pats) => {
                'outer: for pat in pats.iter() {
                    for (&p,c) in pat.iter().zip(inp.chars()) {
                        if p != c && p != '?' {
                            continue 'outer;
                        }
                    }
                    return true;
                }
                false
            }
        }
    }
}

impl AltPattern {
    pub fn new(inp: &str) -> Self {
        let mut pat = Vec::new();
        let mut save = Vec::new();
        let mut alt = Vec::new();
        let mut open_paren = false;
        for c in inp.chars() {
            match c {
                '*' => save.push('?'), // backwards compatibility
                '0' | '1' | '?' => save.push(c),
                '(' => {
                    assert!(!open_paren, "cannot have nested alternatives");
                    if !save.is_empty() {
                        pat.push(PatternElem::Static(save.clone()));
                        save.clear();
                    }
                    open_paren = true;
                }
                '|' => {
                    assert!(open_paren, "alternative outside of parentheses");
                    alt.push(save.clone());
                    save.clear();
                }
                ')' => {
                    assert!(open_paren, "unmatched close paren");
                    alt.push(save.clone());
                    pat.push(PatternElem::Alt(alt.clone()));
                    save.clear();
                    alt.clear();
                    open_paren = false;
                }
                _ => {
                    panic!("unknown input character '{}'", c);
                }
            }
        }
        assert!(!open_paren, "unclosed paren");
        if !save.is_empty() {
            pat.push(PatternElem::Static(save));
        }
        AltPattern { pat }
    }

    pub fn matches(&self, inp: &str) -> bool {
        for elem in self.pat.iter() {
            if !elem.matches(inp) {
                return false;
            }
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let p = AltPattern::new("01");
        assert!(p.matches("01"));
        assert!(!p.matches("00"));

        let p = AltPattern::new("(01|10)");
        assert!(p.matches("01"));
        assert!(p.matches("10"));
        assert!(!p.matches("00"));

        let p = AltPattern::new("(0**|111)");
        assert!(p.matches("000"));
        assert!(p.matches("001"));
        assert!(!p.matches("100"));
        assert!(!p.matches("101"));
        assert!(p.matches("111"));
    }
}
