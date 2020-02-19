pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    let mut sum = 0;
    for number in 0..limit {
        for factor in factors {
            if *factor > 0 && number % *factor == 0 {
                sum += number;
                break;
            }
        }
    }
    sum
}
