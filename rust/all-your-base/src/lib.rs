#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidInputBase,
    InvalidOutputBase,
    InvalidDigit(u32),
}

pub fn convert(number: &[u32], from_base: u32, to_base: u32) -> Result<Vec<u32>, Error> {
    if from_base < 2 {
        return Err(Error::InvalidInputBase);
    }

    if to_base < 2 {
        return Err(Error::InvalidOutputBase);
    }

    let mut number_from_base = 0;
    for (base, digit) in number.iter().rev().enumerate() {
        if digit >= &from_base {
            return Err(Error::InvalidDigit(*digit));
        }
        number_from_base += digit * from_base.pow(base as u32)
    }

    let mut number_to_base = vec![];
    while number_from_base > 0 {
        number_to_base.insert(0, number_from_base % to_base);
        number_from_base /= to_base;
    }
    Ok(number_to_base)
}
