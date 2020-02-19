#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Allergen {
    Eggs = 1,
    Peanuts = 2,
    Shellfish = 4,
    Strawberries = 8,
    Tomatoes = 16,
    Chocolate = 32,
    Pollen = 64,
    Cats = 128,
}

impl Allergen {
    fn values() -> Vec<Self> {
        vec![
            Allergen::Eggs,
            Allergen::Peanuts,
            Allergen::Shellfish,
            Allergen::Strawberries,
            Allergen::Tomatoes,
            Allergen::Chocolate,
            Allergen::Pollen,
            Allergen::Cats,
        ]
    }
}

pub struct Allergies {
    score: u32,
}

impl Allergies {
    pub fn new(score: u32) -> Self {
        Self { score: score % 256 }
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        *allergen as u32 & self.score > 0
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        Allergen::values()
            .into_iter()
            .filter(|a| self.is_allergic_to(a))
            .collect()
    }
}
