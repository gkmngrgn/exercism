use std::collections::HashMap;

const DNA_CODES: &str = "GCTA";
const RNA_CODES: &str = "CGAU";

#[derive(Debug, PartialEq)]
pub struct DNA(String);

#[derive(Debug, PartialEq)]
pub struct RNA(String);

impl DNA {
    pub fn new(dna: &str) -> Result<DNA, usize> {
        for (i, code) in dna.chars().enumerate() {
            if !DNA_CODES.contains(code) {
                return Err(i);
            }
        }
        Ok(Self(dna.to_owned()))
    }

    pub fn into_rna(self) -> RNA {
        let strands: HashMap<char, char> = DNA_CODES.chars().zip(RNA_CODES.chars()).collect();
        let strand: String = self.0.chars().map(|c| strands.get(&c).unwrap()).collect();
        RNA(strand)
    }
}

impl RNA {
    pub fn new(rna: &str) -> Result<RNA, usize> {
        for (i, code) in rna.chars().enumerate() {
            if !RNA_CODES.contains(code) {
                return Err(i);
            }
        }
        Ok(Self(rna.to_owned()))
    }
}
