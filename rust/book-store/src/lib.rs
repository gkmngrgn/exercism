pub fn lowest_price(books: &[u32]) -> u32 {
    let mut book_types = books.to_vec();
    book_types.sort();
    book_types.dedup();
    (1..=book_types.len())
        .map(|size| calculate_group_price(books, size))
        .min()
        .unwrap_or_default()
}

fn calculate_group_price(books: &[u32], size: usize) -> u32 {
    let mut sizes = vec![];
    let mut books = books.to_vec();
    while !books.is_empty() {
        let (mut grouped, mut remaining) = (vec![], vec![]);
        books.iter().for_each(|book| match book {
            b if grouped.len() == size || grouped.contains(&b) => remaining.push(*b),
            b => grouped.push(b),
        });
        sizes.push(grouped.len());
        books = remaining;
    }
    sizes.iter().map(|&s| calculate_price(s)).sum()
}

fn calculate_price(size: usize) -> u32 {
    let discount: u32 = match size {
        1 => 100,
        2 => 95,
        3 => 90,
        4 => 80,
        _ => 75,
    };
    size as u32 * 8 * discount
}
