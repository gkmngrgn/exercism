#[derive(Debug, PartialEq, Eq)]
pub enum Classification {
    Abundant,
    Perfect,
    Deficient,
}

pub fn classify(num: u64) -> Option<Classification> {
    if num == 0 {
        return None;
    }
    Some(match (1..num).filter(|n| num % n == 0).sum::<u64>() {
        n if n < num => Classification::Deficient,
        n if n > num => Classification::Abundant,
        _ => Classification::Perfect,
    })
}
