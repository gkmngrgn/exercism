pub fn find(array: &[i32], key: i32) -> Option<usize> {
    if array.len() == 0 {
        return None;
    }

    let mut low = 0;
    let mut high = array.len() - 1;

    while low <= high {
        let index = (low + high) / 2;
        let current_key = array[index];

        if current_key == key {
            return Some(index);
        }

        if current_key > key && index > 0 {
            high = index - 1;
        } else {
            low = index + 1;
        }
    }

    None
}
