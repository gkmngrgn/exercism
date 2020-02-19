#[derive(PartialEq, Debug)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

pub struct Robot {
    x: i32,
    y: i32,
    d: Direction,
}

impl Robot {
    pub fn new(x: i32, y: i32, d: Direction) -> Self {
        Self { x: x, y: y, d: d }
    }

    pub fn turn_right(self) -> Self {
        let direction = match self.d {
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
        };
        Self {
            d: direction,
            ..self
        }
    }

    pub fn turn_left(self) -> Self {
        let direction = match self.d {
            Direction::North => Direction::West,
            Direction::West => Direction::South,
            Direction::South => Direction::East,
            Direction::East => Direction::North,
        };
        Self {
            d: direction,
            ..self
        }
    }

    pub fn advance(self) -> Self {
        let x = match self.d {
            Direction::East => self.x + 1,
            Direction::West => self.x - 1,
            _ => self.x,
        };
        let y = match self.d {
            Direction::North => self.y + 1,
            Direction::South => self.y - 1,
            _ => self.y,
        };
        Self { x: x, y: y, ..self }
    }

    pub fn instructions(self, instructions: &str) -> Self {
        instructions
            .chars()
            .fold(self, |robot, instruction| match instruction {
                'L' => robot.turn_left(),
                'R' => robot.turn_right(),
                'A' => robot.advance(),
                _ => robot,
            })
    }

    pub fn position(&self) -> (i32, i32) {
        (self.x, self.y)
    }

    pub fn direction(&self) -> &Direction {
        &self.d
    }
}
