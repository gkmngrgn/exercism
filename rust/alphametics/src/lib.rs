use itertools::Itertools;
use std::collections::{HashMap, HashSet};

struct Statement {
    first_letters: HashSet<char>,
    other_letters: HashSet<char>,
    values: Vec<String>,
    total: String,
    digits: HashMap<char, u8>,
}

impl Statement {
    fn new(input: &str) -> Self {
        let statement: Vec<&str> = input.split("==").map(|s| s.trim()).collect();
        let values: Vec<String> = statement[0]
            .split("+")
            .map(|s| s.trim().to_string())
            .collect();
        let total = statement[1].to_string();
        let mut first_letters: HashSet<char> =
            values.iter().map(|s| s.chars().next().unwrap()).collect();
        first_letters.insert(total.chars().next().unwrap());
        Self {
            first_letters: first_letters.clone(),
            other_letters: input
                .chars()
                .filter(|c| c.is_alphabetic() && !first_letters.contains(c))
                .collect(),
            values: values,
            total: total,
            digits: HashMap::new(),
        }
    }

    fn solve(&mut self) -> Option<HashMap<char, u8>> {
        for first_perm in (1..10).permutations(self.first_letters.len()) {
            self.update_digits(first_perm.clone(), self.first_letters.clone());
            let other_digits = (0..10).filter(|i| !first_perm.contains(i));
            for other_perm in other_digits.permutations(self.other_letters.len()) {
                self.update_digits(other_perm, self.other_letters.clone());
                let value_int: u64 = self.values.iter().map(|v| self.convert_to_int(v)).sum();
                if value_int == self.convert_to_int(&self.total) {
                    return Some(self.digits.clone());
                }
            }
        }
        None
    }

    fn convert_to_int(&self, value: &str) -> u64 {
        value
            .chars()
            .rev()
            .enumerate()
            .map(|(i, d)| *self.digits.get(&d).unwrap() as u64 * 10u64.pow(i as u32))
            .sum()
    }

    fn update_digits(&mut self, perm: Vec<u8>, letters: HashSet<char>) {
        for (k, v) in letters.iter().map(|&c| c).zip(perm) {
            self.digits.insert(k, v);
        }
    }
}

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    Statement::new(input).solve()
}
