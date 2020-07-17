use std::cmp::min;

#[derive(Debug)]
pub struct HighScores {
    scores: Vec<u32>,
    sorted_scores: Vec<u32>,
}

impl HighScores {
    pub fn new(scores: &[u32]) -> Self {
        let scores = scores.to_vec();
        let mut sorted_scores = scores.clone();
        sorted_scores.sort_by(|a, b| b.partial_cmp(a).unwrap());

        Self {
            scores,
            sorted_scores,
        }
    }

    fn length(&self) -> usize {
        self.scores.len()
    }

    pub fn scores(&self) -> &[u32] {
        &self.scores
    }

    pub fn latest(&self) -> Option<u32> {
        match self.length() {
            0 => None,
            len => Some(self.scores()[len - 1]),
        }
    }

    pub fn personal_best(&self) -> Option<u32> {
        match self.length() {
            0 => None,
            _ => Some(*self.sorted_scores.first().unwrap()),
        }
    }

    pub fn personal_top_three(&self) -> Vec<u32> {
        self.sorted_scores[..min(3, self.length())].to_vec()
    }
}
