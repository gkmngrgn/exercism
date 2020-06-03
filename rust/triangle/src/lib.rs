pub struct Triangle(usize);

impl Triangle {
    pub fn build(sides: [u64; 3]) -> Option<Self> {
        let sum: u64 = sides.iter().sum();
        if sides.iter().filter(|&s| sum - s <= *s).count() > 0 {
            return None;
        }
        let mut sides = sides.to_vec();
        sides.sort();
        sides.dedup();
        Some(Self(sides.len()))
    }

    pub fn is_equilateral(&self) -> bool {
        &self.0 == &1
    }

    pub fn is_scalene(&self) -> bool {
        &self.0 == &3
    }

    pub fn is_isosceles(&self) -> bool {
        &self.0 == &2
    }
}
