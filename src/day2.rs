use std::{
    fs::File,
    io::{prelude::*, BufReader},
    path::Path,
};

struct InputPassword {
    policy: (u32, u32),
    letter: String,
    password: String,
}

impl InputPassword {
    fn from_string(input: &str) -> Self {
        use regex::Regex;
        let re = Regex::new(r"^(\d+)-(\d+) ([a-z]): (.*)$").unwrap();
        let caps = re
            .captures(input)
            .expect(&format!("Input ({}) does not match correct format", input));
        let fbound = caps
            .get(1)
            .expect("Could not find first bound")
            .as_str()
            .parse::<u32>()
            .expect("First bound not an integer");
        let sbound = caps
            .get(2)
            .expect("Could not find second bound")
            .as_str()
            .parse::<u32>()
            .expect("Second bound not an integer");
        let letter = caps.get(3).expect("Could not find letter").as_str();
        let password = caps.get(4).expect("Could not find password").as_str();
        Self {
            policy: (fbound, sbound),
            letter: String::from(letter),
            password: String::from(password),
        }
    }

    fn is_ok(&self) -> bool {
        let c = self.password.matches(&self.letter).count() as u32;
        return c >= self.policy.0 && c <= self.policy.1;
    }

    fn is_ok_new_policy(&self) -> bool {
        let (i, j) = self.policy;
        if self.password.len() < (i as usize) || self.password.len() < (j as usize) {
            return false;
        }
        let a = self.password.chars().nth((i - 1) as usize);
        let b = self.password.chars().nth((j - 1) as usize);
        let letter = self.letter.chars().nth(0);
        return a != b && (a == letter || b == letter);
    }
}

fn passwords_from_file(filename: impl AsRef<Path>) -> Vec<InputPassword> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .map(|l| InputPassword::from_string(&l))
        .collect()
}

pub fn day2() -> (u32, u32) {
    let passwords = passwords_from_file("data/day2.txt");
    return (
        passwords.iter().filter(|l| l.is_ok()).count() as u32,
        passwords.iter().filter(|l| l.is_ok_new_policy()).count() as u32,
    );
}

#[cfg(test)]
mod tests {
    #[test]
    fn parse_policy() {
        let password = super::InputPassword::from_string("1-3 a: absaibea");
        assert_eq!(password.policy, (1, 3));
        assert_eq!(password.letter, "a");
        assert_eq!(password.password, "absaibea");
    }

    #[test]
    fn passwords_from_file() {
        let passwords = super::passwords_from_file("data/day2.txt");
        assert_eq!(passwords[0].policy, (3, 4));
        assert_eq!(passwords.last().unwrap().password, "ccchc");
    }

    #[test]
    fn is_ok() {
        assert!(super::InputPassword::from_string("1-3 a: absaibea").is_ok());
        assert!(!super::InputPassword::from_string("1-2 a: absaibea").is_ok());
    }

    #[test]
    fn is_ok_new_policy() {
        assert!(super::InputPassword::from_string("1-3 a: absaibea").is_ok_new_policy());
        assert!(!super::InputPassword::from_string("1-4 a: absaibea").is_ok_new_policy());
    }
}
