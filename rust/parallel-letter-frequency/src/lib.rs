use std::cmp::max;
use std::collections::HashMap;
use std::sync::Arc;
use std::thread;

pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
    let mut data = HashMap::new();
    input
        .chunks(max(input.len() / worker_count, 1))
        .map(|i| Arc::new(i.iter().map(|s| s.to_lowercase()).collect::<Vec<String>>()))
        .map(|input_part| {
            let input_part = Arc::clone(&input_part);
            thread::spawn(move || {
                let mut data = HashMap::new();
                input_part
                    .join("")
                    .chars()
                    .filter(|l| l.is_alphabetic())
                    .for_each(|letter| *data.entry(letter).or_insert(0) += 1);
                data
            })
        })
        .for_each(|thread| {
            thread
                .join()
                .unwrap()
                .iter()
                .for_each(|(&letter, count)| *data.entry(letter).or_insert(0) += count);
        });
    data
}
