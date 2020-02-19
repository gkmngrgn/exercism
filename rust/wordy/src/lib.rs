pub struct WordProblem;

pub fn answer(command: &str) -> Option<i32> {
    let mut commands: Vec<&str> = command
        .trim_start_matches("What is ")
        .trim_end_matches("?")
        .trim_end_matches("power")
        .split_whitespace()
        .rev()
        .collect();

    let mut result;
    if let Some(first_value) = parse_number(commands.pop()) {
        result = first_value;
    } else {
        return None;
    }

    let mut cmd = vec![];
    loop {
        let item = commands.pop();
        if let Some(number) = parse_number(item) {
            match cmd.as_slice() {
                ["plus"] => result += number,
                ["minus"] => result -= number,
                ["multiplied", "by"] => result *= number,
                ["divided", "by"] => result /= number,
                ["raised", "to", "the"] => result = result.pow(number as u32),
                _ => return None,
            }
            cmd.clear();
        } else if let Some(item) = item {
            cmd.push(item);
        } else {
            break;
        }
    }

    if cmd.is_empty() {
        Some(result)
    } else {
        None
    }
}

fn parse_number(input: Option<&str>) -> Option<i32> {
    if let Some(mut digits) = input {
        if digits.len() > 2 {
            let indicator = &digits[digits.len() - 2..];
            digits = match indicator {
                "st" | "nd" | "rd" | "th" => digits.trim_end_matches(indicator),
                _ => digits,
            };
        }
        if digits
            .chars()
            .filter(|&c| c.is_digit(10) || c == '-')
            .count()
            == digits.len()
        {
            let mut number = if digits.starts_with('-') { -1 } else { 1 };
            number *= digits
                .chars()
                .filter(|d| d.is_digit(10))
                .rev()
                .enumerate()
                .map(|(i, d)| (10u32.pow(i as u32) * d.to_digit(10).unwrap()) as i32)
                .sum::<i32>();
            return Some(number);
        }
    }
    None
}
