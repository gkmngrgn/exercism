use lazy_static::lazy_static;
use rand::Rng;
use std::sync::Mutex;

const A: u8 = 'A' as u8;
const Z: u8 = 'Z' as u8;

lazy_static! {
    static ref RESERVED: Mutex<Vec<String>> = Mutex::new(vec![]);
}

pub struct Robot {
    name: String,
}

impl Robot {
    pub fn new() -> Self {
        let name = generate_random_name();
        Self { name: name }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn reset_name(&mut self) {
        loop {
            let random_name = generate_random_name();
            if !RESERVED.lock().unwrap().contains(&random_name) {
                self.name = random_name.clone();
                RESERVED.lock().unwrap().push(random_name);
                break;
            }
        }
    }
}

fn generate_random_name() -> String {
    format!(
        "{}{}",
        (0..2)
            .map(|_| rand::thread_rng().gen_range(A, Z + 1) as char)
            .collect::<String>(),
        rand::thread_rng().gen_range(100, 1000)
    )
}
