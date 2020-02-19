pub fn build_proverb(list: &[&str]) -> String {
    let mut result = String::new();
    let mut current: String;
    let last = match list.first() {
        Some(first) => format!("And all for the want of a {}.", first),
        None => String::new(),
    };

    if list.len() >= 1 {
        for index in 0..list.len() - 1 {
            current = format!(
                "For want of a {} the {} was lost.\n",
                list[index],
                list[index + 1]
            );
            result.push_str(&current);
        }
    }
    result.push_str(&last);

    return result;
}
