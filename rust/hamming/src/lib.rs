pub fn hamming_distance(s1: &str, s2: &str) -> Option<usize> {
    if s1.len() != s2.len() {
        return None;
    }
    let sum: usize = s1
        .chars()
        .zip(s2.chars())
        .map(|(a, b)| if a == b { 0 } else { 1 })
        .sum();
    Some(sum)
}
