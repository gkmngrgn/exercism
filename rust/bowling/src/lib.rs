const MAX_FRAMES: usize = 10;
const MAX_PINS: u16 = 10;

#[derive(Debug, PartialEq)]
pub enum Error {
    NotEnoughPinsLeft,
    GameComplete,
}

#[derive(Clone, Copy)]
struct Frame(u16, Option<u16>);

impl Frame {
    fn new(pins: u16) -> Self {
        Self(pins, None)
    }

    fn is_strike(self) -> bool {
        self.0 == MAX_PINS && self.1.is_none()
    }

    fn is_spare(self) -> bool {
        self.0 < MAX_PINS && self.sum() == MAX_PINS
    }

    fn is_normal(self) -> bool {
        self.1.is_some() && !self.is_spare()
    }

    fn is_finished(self) -> bool {
        self.is_normal() || self.is_spare() || self.is_strike()
    }

    fn sum(self) -> u16 {
        self.0 + self.1.unwrap_or(0)
    }

    pub fn len(self) -> usize {
        match self.1 {
            Some(_) => 2,
            None => 1,
        }
    }
}

#[derive(Default)]
pub struct BowlingGame {
    frames: Vec<Frame>,
}

impl BowlingGame {
    pub fn new() -> Self {
        Self { frames: vec![] }
    }

    fn is_game_completed(&self) -> bool {
        if self.frames.len() < MAX_FRAMES {
            return false;
        }

        let last_frame = self.frames[MAX_FRAMES - 1];
        if !last_frame.is_finished() {
            return false;
        }

        let bonus = match last_frame {
            f if f.is_spare() => 1,
            f if f.is_strike() => 2,
            _ => 0,
        };

        self.frames[MAX_FRAMES..]
            .iter()
            .map(|f| f.len())
            .sum::<usize>()
            >= bonus
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        if self.is_game_completed() {
            return Err(Error::GameComplete);
        }

        if pins > MAX_PINS {
            return Err(Error::NotEnoughPinsLeft);
        }

        match self.frames.last_mut() {
            Some(frame) => {
                if frame.is_finished() {
                    self.frames.push(Frame::new(pins))
                } else {
                    if frame.0 + pins > MAX_PINS {
                        return Err(Error::NotEnoughPinsLeft);
                    }
                    frame.1 = Some(pins)
                }
            }
            None => self.frames.push(Frame::new(pins)),
        }

        Ok(())
    }

    pub fn score(&self) -> Option<u16> {
        if !self.is_game_completed() {
            return None;
        }

        let sum = self.frames[..MAX_FRAMES]
            .iter()
            .enumerate()
            .fold(0, |score, (index, frame)| {
                let mut sum = frame.sum();
                if frame.is_spare() {
                    sum += self.frames[index + 1].0;
                } else if frame.is_strike() {
                    match self.frames[index + 1] {
                        f if f.0 == MAX_PINS => sum += MAX_PINS + self.frames[index + 2].0,
                        f => sum += f.sum(),
                    }
                }
                score + sum
            });

        Some(sum)
    }
}
