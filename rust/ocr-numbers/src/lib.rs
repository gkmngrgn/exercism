const ROW_SIZE: usize = 4;
const COLUMN_SIZE: usize = 3;

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidRowCount(usize),
    InvalidColumnCount(usize),
}

pub fn convert(input: &str) -> Result<String, Error> {
    Ok(parse_digits(input)?
        .iter()
        .map(|digits| digits.iter().map(|d| convert_digit(d)).collect::<String>())
        .collect::<Vec<String>>()
        .join(","))
}

fn parse_digits(input: &str) -> Result<Vec<Vec<String>>, Error> {
    let lines: Vec<&str> = input.split('\n').collect();
    if lines.len() % ROW_SIZE != 0 {
        return Err(Error::InvalidRowCount(lines.len()));
    }
    let mut all_digits = vec![];
    for (row_index, sub_lines) in lines.chunks(ROW_SIZE).enumerate() {
        let mut digits_count = 0;
        for line in sub_lines {
            if line.len() % COLUMN_SIZE != 0
                || (digits_count != 0 && digits_count != line.len() / COLUMN_SIZE)
            {
                return Err(Error::InvalidColumnCount(line.len()));
            }
            digits_count = line.len() / COLUMN_SIZE;
        }
        let mut digits = vec![];
        for col_index in 0..digits_count {
            let (row_start, row_end) = (row_index * ROW_SIZE, (row_index + 1) * ROW_SIZE);
            let (col_start, col_end) = (col_index * COLUMN_SIZE, (col_index + 1) * COLUMN_SIZE);
            let digit: Vec<String> = (row_start..row_end)
                .map(|r| lines[r][col_start..col_end].to_string())
                .collect();
            digits.push(digit.join("."));
        }
        all_digits.push(digits);
    }
    Ok(all_digits)
}

fn convert_digit(input: &str) -> String {
    let digit = match input.trim_end() {
        " _ .| |.|_|." => "0",
        "   .  |.  |." => "1",
        " _ . _|.|_ ." => "2",
        " _ . _|. _|." => "3",
        "   .|_|.  |." => "4",
        " _ .|_ . _|." => "5",
        " _ .|_ .|_|." => "6",
        " _ .  |.  |." => "7",
        " _ .|_|.|_|." => "8",
        " _ .|_|. _|." => "9",
        _ => "?",
    };
    digit.to_string()
}
