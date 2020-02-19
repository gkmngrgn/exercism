use std::cmp::min;

const ZERO: u8 = 48;
const MINE: char = '*';

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    minefield
        .iter()
        .enumerate()
        .map(|(y, row)| {
            row.chars()
                .enumerate()
                .map(|(x, c)| match c {
                    MINE => MINE,
                    _ => count_mines(x, y, minefield),
                })
                .collect::<String>()
        })
        .collect()
}

fn count_mines(x: usize, y: usize, field: &[&str]) -> char {
    let max_y = field.len() - 1;
    let max_x = field[y].len() - 1;
    let mut counter: u8 = 0;
    for y_ in y.checked_sub(1).unwrap_or(0)..=min(max_y, y + 1) {
        for x_ in x.checked_sub(1).unwrap_or(0)..=min(max_x, x + 1) {
            if field[y_].chars().nth(x_).unwrap() == MINE {
                counter += 1;
            }
        }
    }
    match counter {
        0 => ' ',
        _ => (counter + ZERO) as char,
    }
}
