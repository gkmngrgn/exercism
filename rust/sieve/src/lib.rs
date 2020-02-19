pub fn primes_up_to(upper_bound: u64) -> Vec<u64> {
    if upper_bound <= 1 {
        return Vec::new();
    }

    let mut prime_table = vec![true; upper_bound as usize + 1];
    for i in 2..(upper_bound as f64).sqrt() as usize + 1 {
        if prime_table[i] == true {
            for n in (i.pow(2)..upper_bound as usize + 1).step_by(i) {
                prime_table[n] = false;
            }
        }
    }

    prime_table
        .iter()
        .enumerate()
        .skip(2)
        .filter(|(_, &v)| v == true)
        .map(|(i, _)| i as u64)
        .collect()
}
