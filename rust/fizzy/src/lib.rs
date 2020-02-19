use std::fmt::Display;
use std::ops::Rem;

pub struct Matcher<T> {
    func: fn(T) -> bool,
    subs: String,
}

impl<T> Matcher<T> {
    pub fn new<S: Display>(matcher: fn(T) -> bool, subs: S) -> Self {
        Self {
            func: matcher,
            subs: subs.to_string(),
        }
    }
}

pub struct Fizzy<T> {
    matchers: Vec<Matcher<T>>,
}

impl<T: Display + Copy> Fizzy<T> {
    pub fn new() -> Self {
        Self { matchers: vec![] }
    }

    pub fn add_matcher(self, matcher: Matcher<T>) -> Self {
        let matchers = self.matchers.into_iter().chain(vec![matcher]).collect();
        Self { matchers: matchers }
    }

    pub fn apply<I: Iterator<Item = T>>(self, iter: I) -> impl Iterator<Item = String> {
        iter.map(move |i| {
            let text_list: Vec<String> = self
                .matchers
                .iter()
                .filter(|m| (m.func)(i))
                .map(|m| m.subs.clone())
                .collect();
            match text_list.len() {
                0 => i.to_string(),
                _ => text_list.join(""),
            }
        })
    }
}

pub fn fizz_buzz<T: Display + Copy>() -> Fizzy<T>
where
    T: Rem<Output = T> + PartialEq + From<u8>,
{
    Fizzy::new()
        .add_matcher(Matcher::new(|n: T| n % 3.into() == 0.into(), "fizz"))
        .add_matcher(Matcher::new(|n: T| n % 5.into() == 0.into(), "buzz"))
}
