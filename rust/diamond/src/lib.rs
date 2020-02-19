const A: u8 = 'A' as u8;

pub fn get_diamond(c: char) -> Vec<String> {
    let length = (c as u8 - A) as usize;
    let seq: Vec<String> = (0..=length).map(|i| get_line(length, i)).collect();
    seq.iter()
        .chain(seq.iter().rev().skip(1))
        .map(|l| l.to_string())
        .collect()
}

fn get_line(length: usize, index: usize) -> String {
    let line = (0..=length).map(|i| match i {
        n if n == length - index => (A + index as u8) as char,
        _ => ' ',
    });
    line.clone().chain(line.rev().skip(1)).collect()
}
