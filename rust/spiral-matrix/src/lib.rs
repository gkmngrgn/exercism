use std::collections::HashMap;

#[derive(Clone)]
enum Direction {
    Right,
    Down,
    Left,
    Up,
}

#[derive(Hash, Eq, PartialEq, Clone)]
struct Position {
    x: u32,
    y: u32,
}

#[derive(Clone)]
struct Matrix {
    positions: HashMap<Position, u32>,
    current_position: Position,
    current_direction: Direction,
    size: u32,
}

impl Matrix {
    fn new(size: u32) -> Self {
        Self {
            positions: HashMap::new(),
            current_position: Position { x: 0, y: 0 },
            current_direction: Direction::Right,
            size: size,
        }
    }

    fn fill_positions(&mut self) {
        let max_value = self.size.pow(2);
        for value in 1..max_value + 1 {
            self.positions.insert(self.current_position.clone(), value);
            if value < max_value {
                self.set_next_position();
            }
        }
    }

    fn set_next_position(&mut self) {
        let (x, y) = match self.current_direction {
            Direction::Right => (self.current_position.x + 1, self.current_position.y),
            Direction::Down => (self.current_position.x, self.current_position.y + 1),
            Direction::Left => (
                self.current_position.x.checked_sub(1).unwrap_or(0),
                self.current_position.y,
            ),
            Direction::Up => (
                self.current_position.x,
                self.current_position.y.checked_sub(1).unwrap_or(0),
            ),
        };
        let new_position = Position { x: x, y: y };

        if x == self.size || y == self.size || self.positions.contains_key(&new_position) {
            self.current_direction = match self.current_direction {
                Direction::Right => Direction::Down,
                Direction::Down => Direction::Left,
                Direction::Left => Direction::Up,
                Direction::Up => Direction::Right,
            };
            self.set_next_position();
        } else {
            self.current_position = new_position;
        }
    }

    fn to_vec(self) -> Vec<Vec<u32>> {
        (0..self.size)
            .map(|y| {
                (0..self.size)
                    .map(|x| {
                        self.positions
                            .get(&Position { x: x, y: y })
                            .unwrap()
                            .clone()
                    })
                    .collect()
            })
            .collect()
    }
}

pub fn spiral_matrix(size: u32) -> Vec<Vec<u32>> {
    let mut matrix = Matrix::new(size);
    matrix.fill_positions();
    matrix.to_vec()
}
