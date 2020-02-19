use std::collections::BTreeMap;

pub fn transform(h: &BTreeMap<i32, Vec<char>>) -> BTreeMap<char, i32> {
    h.iter()
        .flat_map(|(number, letters)| {
            letters
                .iter()
                .map(move |letter| (letter.to_ascii_lowercase(), *number))
        })
        .collect()
}
