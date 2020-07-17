const BRACKETS_OPEN: [char; 3] = ['[', '(', '{'];
const BRACKETS_CLOSE: [char; 3] = [']', ')', '}'];

pub fn brackets_are_balanced(string: &str) -> bool {
    let mut open_brackets = String::new();

    for bracket in string.chars() {
        if BRACKETS_OPEN.contains(&bracket) {
            open_brackets.push(bracket);
        } else if !BRACKETS_CLOSE.contains(&bracket) {
            continue;
        } else if open_brackets.is_empty()
            || open_brackets.chars().rev().next().unwrap()
                != BRACKETS_OPEN[BRACKETS_CLOSE.iter().position(|&b| b == bracket).unwrap()]
        {
            return false;
        } else {
            open_brackets.pop();
        }
    }

    open_brackets.is_empty()
}
