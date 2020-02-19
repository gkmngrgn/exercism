pub fn brackets_are_balanced(string: &str) -> bool {
    let brackets_open = ['[', '(', '{'];
    let brackets_close = [']', ')', '}'];
    let mut open_brackets = String::new();

    for bracket in string.chars() {
        if brackets_open.contains(&bracket) {
            open_brackets.push(bracket);
            continue;
        }

        if !brackets_close.contains(&bracket) {
            continue;
        }

        if open_brackets.len() == 0
            || open_brackets.chars().rev().next().unwrap()
                != brackets_open[brackets_close.iter().position(|&b| b == bracket).unwrap()]
        {
            return false;
        }

        open_brackets.pop();
    }

    open_brackets.len() == 0
}
