#[macro_use]
extern crate itertools;

pub fn count(lines: &[&str]) -> u32 {
    let mut counter = 0;
    let horizontal_lines: Vec<Vec<char>> = lines.iter().map(|l| l.chars().collect()).collect();
    let vertical_lines = transpose_lines(&horizontal_lines);
    let corners: Vec<(usize, usize)> = lines
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.chars()
                .enumerate()
                .filter_map(move |(x, col)| match col {
                    '+' => Some((x, y)),
                    _ => None,
                })
        })
        .collect();
    for ((left, top), (right, bottom)) in iproduct!(&corners, &corners) {
        let h = get_slice(&horizontal_lines, *top, *bottom, *left, *right);
        let v = get_slice(&vertical_lines, *left, *right, *top, *bottom);
        if !h.is_empty()
            && !v.is_empty()
            && !h.contains(' ')
            && !v.contains(' ')
            && !h.contains('|')
            && !v.contains('-')
        {
            counter += 1;
        }
    }
    counter
}

fn transpose_lines(lines: &[Vec<char>]) -> Vec<Vec<char>> {
    lines.iter().fold(vec![], |mut zipped, line| {
        line.iter().enumerate().for_each(|(i, &l)| {
            match zipped.get_mut(i) {
                Some(line) => line.push(l),
                None => zipped.push(vec![l]),
            };
        });
        zipped
    })
}

fn get_slice(lines: &[Vec<char>], t: usize, b: usize, l: usize, r: usize) -> String {
    if b <= t || r <= l {
        return "".to_string();
    }
    let p1 = lines.get(t).unwrap().iter().skip(l).take(r - l + 1);
    let p2 = lines.get(b).unwrap().iter().skip(l).take(r - l + 1);
    p1.chain(p2).collect()
}
