use std::cmp;
use std::fmt;

const HOURS_IN_A_DAY: i32 = 24;
const MINUTES_IN_AN_HOUR: i32 = 60;

#[derive(Debug)]
pub struct Clock {
    hours: i32,
    minutes: i32,
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Clock {
            hours: hours,
            minutes: minutes,
        }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Clock {
            hours: self.hours,
            minutes: self.minutes + minutes,
        }
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut hours = self.hours;
        let mut minutes = self.minutes;

        if minutes.abs() >= MINUTES_IN_AN_HOUR {
            hours += minutes  / MINUTES_IN_AN_HOUR;
            minutes %= MINUTES_IN_AN_HOUR;
        }

        if minutes < 0 {
            hours -= 1;
            minutes += MINUTES_IN_AN_HOUR;
        }

        if hours.abs() >= HOURS_IN_A_DAY {
            hours %= HOURS_IN_A_DAY;
        }

        if hours < 0 {
            hours += HOURS_IN_A_DAY;
        }

        write!(f, "{:02}:{:02}", hours, minutes)
    }
}

impl cmp::PartialEq for Clock {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}
