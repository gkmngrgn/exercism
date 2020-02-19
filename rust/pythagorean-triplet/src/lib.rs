use std::collections::HashSet;

pub fn find(sum: u32) -> HashSet<[u32; 3]> {
    let mut triplets = HashSet::new();
    for a in 1..sum {
        let (mut b, mut c) = (a + 1, a + 2);
        while a + b + c <= sum {
            let cc = a * a + b * b;
            while c * c < cc {
                c += 1;
            }
            if c * c == cc && a + b + c == sum {
                triplets.insert([a, b, c]);
            }
            b += 1;
        }
    }
    triplets
}
