use std::cell::RefCell;
use std::collections::HashSet;

use rand::distr::Alphanumeric;
use rand::rngs::ThreadRng;
use rand::Rng;

thread_local! {
    pub static GENERATOR: RefCell<Generator> = RefCell::new(Generator::new());
}

pub struct Generator {
    used_identifiers: HashSet<String>,
    rng: ThreadRng,
}

impl Generator {
    pub fn new() -> Self {
        Self {
            used_identifiers: HashSet::default(),
            rng: rand::rng(),
        }
    }

    pub fn generate(&mut self) -> String {
        loop {
            let length = self.rng.random_range(8..15);
            let random_part: String = (&mut self.rng)
                .sample_iter(&Alphanumeric)
                .take(length)
                .map(char::from)
                .collect();
            let name = format!("_{}", random_part);

            if !self.used_identifiers.contains(&name) {
                self.used_identifiers.insert(name.clone());
                return name;
            }
        }
    }
}
