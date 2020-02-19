pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<(usize, usize)> {
    let mut saddle_points = vec![];
    for (row_index, row) in input.iter().enumerate() {
        if row.len() == 0 {
            break;
        }
        let max_point = row.iter().max().unwrap();
        for (column_index, point) in row.iter().enumerate() {
            if point == max_point && point == &input.iter().map(|r| r[column_index]).min().unwrap()
            {
                saddle_points.push((row_index, column_index));
            }
        }
    }
    saddle_points
}
