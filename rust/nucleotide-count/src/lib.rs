use std::collections::HashMap;

const NUCLEOTIDES: &str = "GCTA";

fn check_dna_codes(dna: &str) -> Result<(), char> {
    for code in dna.chars() {
        if !NUCLEOTIDES.contains(code) {
            return Err(code);
        }
    }
    Ok(())
}

pub fn count(nucleotide: char, dna: &str) -> Result<usize, char> {
    let is_dna_valid = check_dna_codes(dna);
    if is_dna_valid.is_err() {
        return Err(is_dna_valid.err().unwrap());
    }
    if !NUCLEOTIDES.contains(nucleotide) {
        return Err(nucleotide);
    }
    Ok(dna.chars().filter(|&c| c == nucleotide).count())
}

pub fn nucleotide_counts(dna: &str) -> Result<HashMap<char, usize>, char> {
    let is_dna_valid = check_dna_codes(dna);
    if is_dna_valid.is_err() {
        return Err(is_dna_valid.err().unwrap());
    }
    let mut result = HashMap::new();
    for code in NUCLEOTIDES.chars() {
        result.insert(code, count(code, dna).unwrap());
    }
    Ok(result)
}
