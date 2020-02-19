pub fn lowest_price(books: &[u32]) -> u32 {
    let book_types = books.iter().fold(vec![], |mut list, i| {
        if !list.contains(i) {
            list.push(*i)
        }
        list
    });
    (1..=book_types.len())
        .map(|max_size| {
            get_sizes_of_grouped_books(books.to_vec(), max_size)
                .iter()
                .map(|&size| calculate_basket_price(size))
                .sum()
        })
        .min()
        .unwrap_or(0)
}

fn get_sizes_of_grouped_books(books: Vec<u32>, size: usize) -> Vec<usize> {
    let mut sizes = vec![];
    let mut books = books;
    while !books.is_empty() {
        let (mut grouped, mut remaining) = (vec![], vec![]);
        for book in &books {
            if grouped.len() == size || grouped.contains(&book) {
                remaining.push(*book);
            } else {
                grouped.push(book);
            }
        }
        sizes.push(grouped.len());
        books = remaining;
    }
    sizes
}

fn calculate_basket_price(size: usize) -> u32 {
    let discount = match size {
        1 => 100,
        2 => 95,
        3 => 90,
        4 => 80,
        _ => 75,
    };
    (size * 8 * discount) as u32
}
