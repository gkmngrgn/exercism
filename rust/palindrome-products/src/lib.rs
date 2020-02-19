#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Palindrome {
    a: u64,
    b: u64,
    factors: Vec<Vec<u64>>,
}

impl Palindrome {
    pub fn new(a: u64, b: u64) -> Self {
        Self {
            a: a,
            b: b,
            factors: vec![vec![a, b]],
        }
    }

    pub fn value(&self) -> u64 {
        self.a * self.b
    }

    pub fn insert(&mut self, a: u64, b: u64) {
        let factor = vec![a, b];
        if !self.factors.contains(&factor) {
            self.factors.push(factor);
        }
    }
}

pub fn palindrome_products(min: u64, max: u64) -> Option<(Palindrome, Palindrome)> {
    let p_min = find_smallest_palindome_product(min, max);
    if p_min.is_none() {
        return None;
    }

    let mut pp_min = p_min.unwrap();
    for a in min..=max {
        for b in a..=max {
            let product = a * b;
            if product > pp_min.value() {
                break;
            }
            if product != pp_min.value() {
                continue;
            }
            pp_min.insert(a, b);
        }
    }

    let mut pp_max = find_largest_palindrome_product(min, max).unwrap();
    for a in (min..=max).rev() {
        for b in (a..=max).rev() {
            let product = a * b;
            if product < pp_max.value() {
                break;
            }
            if product != pp_max.value() {
                continue;
            }
            pp_max.insert(a, b);
        }
    }
    return Some((pp_min, pp_max));
}

fn find_smallest_palindome_product(min: u64, max: u64) -> Option<Palindrome> {
    for product in min.pow(2)..=max.pow(2) {
        if !is_palindrome(product) {
            continue;
        }
        for a in min..=max {
            if product % a != 0 {
                continue;
            }
            let b = product / a;
            if b < a || b > max {
                continue;
            }
            return Some(Palindrome::new(a, b));
        }
    }
    None
}

fn find_largest_palindrome_product(min: u64, max: u64) -> Option<Palindrome> {
    for product in (min.pow(2)..=max.pow(2)).rev() {
        if !is_palindrome(product) {
            continue;
        }
        for b in (min..=max).rev() {
            if product % b != 0 {
                continue;
            }
            let a = product / b;
            if a > b || a < min {
                continue;
            }
            return Some(Palindrome::new(a, b));
        }
    }
    None
}

fn is_palindrome(original_value: u64) -> bool {
    let (mut value, mut reversed_value) = (original_value, 0);
    while value > 0 {
        reversed_value = reversed_value * 10 + value % 10;
        value /= 10;
    }
    original_value == reversed_value
}
