pub fn map<T, R>(input: Vec<T>, func: impl FnMut(T) -> R) -> Vec<R> {
    input.into_iter().map(func).collect()
}
