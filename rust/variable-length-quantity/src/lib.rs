#[derive(Debug, PartialEq)]
pub enum Error {
    IncompleteNumber,
    Overflow,
}

pub fn to_bytes(values: &[u32]) -> Vec<u8> {
    values
        .iter()
        .flat_map(|&v| {
            let mut value = v;
            let mut bytes = Vec::with_capacity(5);
            let mut mask: u8 = 0;
            while mask == 0 || value != 0 {
                bytes.push(mask | (value & 127) as u8);
                value >>= 7;
                mask = 128;
            }
            bytes.into_iter().rev()
        })
        .collect()
}

pub fn from_bytes(bytes: &[u8]) -> Result<Vec<u32>, Error> {
    if let Some(x) = bytes.last() {
        if x & 128 > 0 {
            return Err(Error::IncompleteNumber);
        }
    }
    let mut value: u32 = 0;
    let mut values = vec![];
    for byte in bytes {
        if value.leading_zeros() < 7 {
            return Err(Error::Overflow);
        }
        value = (value << 7) | (byte & 127) as u32;
        if byte & 128 == 0 {
            values.push(value as u32);
            value = 0;
        }
    }
    Ok(values)
}
