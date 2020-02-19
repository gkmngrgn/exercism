pub struct PascalsTriangle {
    row_count: u32,
}

impl PascalsTriangle {
    pub fn new(row_count: u32) -> Self {
        Self {
            row_count: row_count,
        }
    }

    pub fn rows(&self) -> Vec<Vec<u32>> {
        let mut rows = vec![];
        let mut row = vec![1];

        for _ in 0..self.row_count {
            rows.push(row.clone());
            row.push(0);
            row = row
                .iter()
                .zip(row.iter().rev())
                .map(|(a, b)| a + b)
                .collect();
        }
        rows
    }
}
