use std::collections::HashMap;

pub struct CodonsInfo<'a> {
    codons: HashMap<&'a str, &'a str>,
}

impl<'a> CodonsInfo<'a> {
    pub fn name_for(&self, codon: &str) -> Option<&'a str> {
        match self.codons.get(codon) {
            Some(name) => Some(name),
            None => None,
        }
    }

    pub fn of_rna(&self, rna: &str) -> Option<Vec<&'a str>> {
        let names: Vec<&'a str> = rna
            .chars()
            .collect::<Vec<char>>()
            .chunks(3)
            .map(|c| match self.name_for(&c.iter().collect::<String>()) {
                Some(name) => name,
                None => "stop codon",
            })
            .take_while(|codon| codon != &"stop codon")
            .collect();
        if names.len() == 0 {
            return None;
        }
        Some(names)
    }
}

pub fn parse<'a>(pairs: Vec<(&'a str, &'a str)>) -> CodonsInfo<'a> {
    let mut codons = HashMap::<&'a str, &'a str>::new();
    for (codon, name) in pairs {
        codons.insert(codon, name);
    }
    CodonsInfo { codons: codons }
}
