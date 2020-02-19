use failure::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
pub struct Flags {
    print_line_numbers: bool,
    print_file_names: bool,
    case_insensitive: bool,
    inverted: bool,
    match_exact: bool,
}

impl Flags {
    pub fn new(flags: &[&str]) -> Self {
        Self {
            print_line_numbers: flags.contains(&"-n"),
            print_file_names: flags.contains(&"-l"),
            case_insensitive: flags.contains(&"-i"),
            inverted: flags.contains(&"-v"),
            match_exact: flags.contains(&"-x"),
        }
    }
}

fn get_matches(
    pattern: &str,
    line: &str,
    filename: &str,
    index: usize,
    has_multiple_files: bool,
    flags: &Flags,
) -> Option<String> {
    let no_filter_rule_exists = [flags.match_exact, flags.case_insensitive]
        .iter()
        .all(|&f| f == false);
    if !(no_filter_rule_exists && line.contains(pattern))
        && !(flags.match_exact && line == pattern)
        && !(flags.case_insensitive && line.to_lowercase().contains(&pattern.to_lowercase()))
    {
        if !flags.inverted {
            return None;
        }
    } else if flags.inverted {
        return None;
    }

    let mut result;
    if flags.print_file_names {
        result = filename.to_string();
    } else {
        if flags.print_line_numbers {
            result = format!("{}:{}", index, &line);
        } else {
            result = line.to_string()
        }
        if has_multiple_files {
            result = format!("{}:{}", filename.to_string(), result)
        }
    }
    Some(result)
}

pub fn grep(pattern: &str, flags: &Flags, files: &[&str]) -> Result<Vec<String>, Error> {
    let mut matches = vec![];
    for file in files {
        let buffer = BufReader::new(File::open(file)?);
        for (i, l) in buffer.lines().enumerate() {
            if let Some(m) = get_matches(&pattern, &l?, file, i + 1, files.len() > 1, flags) {
                if !matches.contains(&m) {
                    &matches.push(m);
                }
            }
        }
    }
    Ok(matches)
}
