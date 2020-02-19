use std::fmt::{Display, Formatter, Result};

const ROMAN_NUMERALS: [(u32, &str); 13] = [
    (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I"),
];

pub struct Roman(u32);

impl Display for Roman {
    fn fmt(&self, _f: &mut Formatter<'_>) -> Result {
        let mut num = self.0;
        let mut roman = String::new();
        for &(min_num, roman_numeral) in ROMAN_NUMERALS.iter() {
            while num >= min_num {
                roman.push_str(roman_numeral);
                num -= min_num;
            }
        }
        write!(_f, "{}", roman)
    }
}

impl From<u32> for Roman {
    fn from(num: u32) -> Self {
        Self(num)
    }
}
