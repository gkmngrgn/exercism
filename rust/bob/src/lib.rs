pub fn reply(message: &str) -> &str {
    let msg: String = message
        .trim()
        .chars()
        .filter(|x| x.is_alphabetic() || *x == '?')
        .collect();
    let answer;

    if message.trim() == "" {
        answer = "Fine. Be that way!";
    } else if msg.len() > 1 && msg.ends_with("?") && msg.to_uppercase() == msg {
        answer = "Calm down, I know what I'm doing!";
    } else if msg.ends_with("?") {
        answer = "Sure.";
    } else if msg.len() > 0 && msg.to_uppercase() == msg {
        answer = "Whoa, chill out!";
    } else {
        answer = "Whatever.";
    }

    answer
}
