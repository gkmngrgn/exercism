use std::cmp::{min, Reverse};

#[derive(Debug)]
pub struct HighScores {
    scores: Vec<u32>,
    sorted_scores: Vec<u32>,
}

impl HighScores {
    pub fn new(scores: &[u32]) -> Self {
        let mut sorted_scores = scores.to_vec();
        sorted_scores.sort_by_key(|&x| Reverse(x));

        Self {
            scores: scores.to_vec(),
            sorted_scores: sorted_scores,
        }
    }

    pub fn scores(&self) -> &[u32] {
        &self.scores
    }

    pub fn latest(&self) -> Option<u32> {
        match self.scores.len() > 0 {
            true => Some(*self.scores.last().unwrap()),
            false => None,
        }
    }

    pub fn personal_best(&self) -> Option<u32> {
        match self.sorted_scores.len() > 0 {
            true => Some(*self.sorted_scores.first().unwrap()),
            false => None,
        }
    }

    pub fn personal_top_three(&self) -> Vec<u32> {
        self.sorted_scores[..min(3, self.sorted_scores.len())].to_vec()
    }
}
