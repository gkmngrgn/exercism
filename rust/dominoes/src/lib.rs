pub fn chain(input: &[(u8, u8)]) -> Option<Vec<(u8, u8)>> {
    match input.first() {
        Some(tile) => get_chain(vec![*tile], input[1..].to_vec()),
        _ => Some(vec![]),
    }
}

fn get_chain(mut chain: Vec<(u8, u8)>, tiles: Vec<(u8, u8)>) -> Option<Vec<(u8, u8)>> {
    let last_number = chain.last().unwrap().1;
    let tiles_len = tiles.len();
    let tiles: Vec<(u8, u8)> = tiles
        .iter()
        .filter(|(first, second)| first != &last_number || second != &last_number)
        .copied()
        .collect();
    for _ in 0..tiles_len - tiles.len() {
        chain.push((last_number, last_number));
    }
    for (index, tile) in tiles.iter().enumerate() {
        if tile.0 != last_number && tile.1 != last_number {
            continue;
        }
        let tile = match tile {
            t if tile.1 == last_number => (t.1, t.0),
            _ => *tile,
        };
        if let Some(chain) = get_chain(
            chain.iter().chain(vec![tile].iter()).copied().collect(),
            tiles
                .iter()
                .enumerate()
                .filter(|(i, _)| i != &index)
                .map(|(_, &t)| t)
                .collect(),
        ) {
            return Some(chain);
        }
    }
    if !tiles.is_empty() || chain.first().unwrap().0 != chain.last().unwrap().1 {
        return None;
    }
    Some(chain)
}
