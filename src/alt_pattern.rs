#[derive(Debug)]
pub struct AltPattern {
    pat: Vec<PatternElem>,
}

#[derive(Debug)]
pub enum PatternElem {
    Static(Vec<char>),
    Alt { pats: Vec<Vec<char>>, len: usize },
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

            PatternElem::Alt { pats, .. } => {
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

    pub fn matches_at(&self, index: usize, inp: char) -> bool {
        match self {
            PatternElem::Static(pat) => pat[index] == '?' || pat[index] == inp,

            PatternElem::Alt { pats, .. } => {
                for pat in pats.iter() {
                    if pat[index] == '?' || pat[index] == inp {
                        return true;
                    }
                }
                false
            }
        }
    }

    pub fn matches_both(&self, i: usize, xi: char, j: usize, xj: char) -> bool {
        match self {
            PatternElem::Static(pat) =>
                (pat[i] == '?' || pat[i] == xi) && (pat[j] == '?' || pat[j] == xj),

            PatternElem::Alt { pats, .. } => {
                for pat in pats.iter() {
                    if (pat[i] == '?' || pat[i] == xi) && (pat[j] == '?' || pat[j] == xj) {
                        return true;
                    }
                }
                false
            }
        }
    }

    pub fn len(&self) -> usize {
        match self {
            PatternElem::Static(pat) => pat.len(),
            PatternElem::Alt { len, .. } => *len,
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
                    if !alt.is_empty() {
                        assert_eq!(save.len(), alt[0].len(), "alternative with different length: {:?}", save);
                    }
                    alt.push(save.clone());
                    pat.push(PatternElem::Alt { pats: alt.clone(), len: save.len() });
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

    pub fn len(&self) -> usize {
        self.pat.iter().map(|e| e.len()).sum()
    }

    pub fn matches_at(&self, index: usize, inp: char) -> bool {
        let mut cur = index;
        for elem in self.pat.iter() {
            if cur >= elem.len() {
                cur -= elem.len();
                continue;
            }
            return elem.matches_at(cur, inp);
        }
        panic!("index ({}) >= pattern len ({})", index, self.len());
    }

    pub fn same_alternative(&self, index1: usize, index2: usize) -> Option<(&PatternElem, usize, usize)> {
        if index1 > index2 {
            match self.same_alternative(index2, index1) {
                Some((elem, jp, ip)) => return Some((elem, ip, jp)),
                None => return None,
            }
        }
        let mut i = index1;
        let mut j = index2;
        for elem in self.pat.iter() {
            if i >= elem.len() {
                i -= elem.len();
                j -= elem.len();
                continue;
            }
            if j < elem.len() {
                return Some((elem, i, j));
            } else {
                return None;
            }
        }
        None
    }

    pub fn matches_both(&self, i: usize, xi: char, j: usize, xj: char) -> bool {
        if let Some((elem, ip, jp)) = self.same_alternative(i, j) {
            elem.matches_both(ip, xi, jp, xj)
        } else {
            self.matches_at(i, xi) && self.matches_at(j, xj)
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_matches() {
        let p = AltPattern::new("01");
        assert!(p.matches("01"));
        assert!(!p.matches("00"));
        assert!(!p.matches_at(0, '1'));

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
        assert!(p.matches_at(1, '1'));

        let p = AltPattern::new("(00|11|01)");
        assert!(p.matches("00"));
        assert!(p.matches("11"));
        assert!(p.matches("01"));
        assert!(!p.matches("10"));
    }

    #[test]
    fn test_matches_both() {
        let p = AltPattern::new("01");
        assert!(p.matches_both(0,'0',1,'1'));
        assert!(!p.matches_both(0,'1',1,'0'));

        let p = AltPattern::new("(0**|111)");
        assert!(p.matches_both(0, '1', 2, '1'));
        assert!(!p.matches_both(0, '1', 2, '0'));

        let p = AltPattern::new("(00|11|01)");
        assert!(p.matches_both(0,'0',1,'0'));
        assert!(p.matches_both(0,'1',1,'1'));
        assert!(p.matches_both(0,'0',1,'1'));
        assert!(!p.matches_both(0,'1',1,'0'));
    }
}
