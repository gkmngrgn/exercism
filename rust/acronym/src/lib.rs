pub fn abbreviate(phrase: &str) -> String {
    let mut acronym = String::new();
    let mut prev = ' ';
    for p in phrase.chars() {
        if p.is_alphabetic() && [' ', '-'].iter().any(|x| x == &prev)
            || (p.is_uppercase() && prev.is_lowercase())
        {
            acronym.push(p);
        }
        prev = p;
    }
    acronym.to_uppercase()
}
