pub struct RailFence {
    rails: u32,
}

impl RailFence {
    pub fn new(rails: u32) -> Self {
        Self { rails: rails }
    }

    pub fn encode(&self, text: &str) -> String {
        let mut indexed_chars: Vec<(char, usize)> =
            text.chars().zip(self.cycle(text.len())).collect();
        indexed_chars.sort_by_key(|a| a.1);
        indexed_chars.iter().map(|(c, _)| c).collect()
    }

    pub fn decode(&self, cipher: &str) -> String {
        let cycle: Vec<(usize, usize)> = (0..cipher.len()).zip(self.cycle(cipher.len())).collect();
        let mut text: Vec<char> = cipher.chars().collect();
        let mut index = 0;
        for s in 0..self.rails as usize {
            for (a, _) in cycle.iter().filter(|(_, b)| b == &s) {
                text[*a] = cipher.chars().nth(index).unwrap();
                index += 1;
            }
        }
        text.iter().collect()
    }

    fn cycle(&self, len: usize) -> Vec<usize> {
        let step = self.rails as usize * 2 - 2;
        (0..len)
            .map(|i| {
                let s = i % step;
                if s <= step / 2 {
                    s
                } else {
                    step - s
                }
            })
            .collect()
    }
}
