use std::collections::BTreeSet;

pub struct Triangle(usize);

impl Triangle {
    pub fn build(sides: [u64; 3]) -> Option<Triangle> {
        let mut different_sides = BTreeSet::new();
        let sum: u64 = sides.iter().sum();
        for side in sides.iter() {
            if side == &0 || sum - side < *side {
                return None;
            }
            different_sides.insert(side);
        }
        Some(Triangle(different_sides.len()))
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
