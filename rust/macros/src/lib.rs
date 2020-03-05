#[macro_export]
macro_rules! hashmap {
    () => { std::collections::HashMap::new() };
    ($($key:expr => $val:expr),+ $(,)?) => {
        vec![$(($key, $val),)*].into_iter().collect::<HashMap<_, _>>()
    };
}
