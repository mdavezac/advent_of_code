use std::path::Path;

#[derive(Debug, PartialEq, Clone)]
enum Operation {
    Nop(i64),
    Jmp(i64),
    Acc(i64),
}

impl Operation {
    fn from_string(data: &str) -> Result<Operation, &'static str> {
        let (op, val) = {
            let x: Vec<&str> = data.split(' ').collect();
            (x[0], x[1].parse::<i64>())
        };
        if !val.is_ok() {
            return Err("Could not parse integer");
        }
        match op {
            "nop" => Ok(Operation::Nop(val.unwrap())),
            "jmp" => Ok(Operation::Jmp(val.unwrap())),
            "acc" => Ok(Operation::Acc(val.unwrap())),
            &_ => Err("Unknown string"),
        }
    }
}

struct Machine {
    tape: Vec<Operation>,
    offset: i64,
    accumulator: i64,
}

impl Machine {
    fn from_string(data: &str) -> Result<Machine, &'static str> {
        let mut ops: Vec<Result<_, _>> = data.lines().map(Operation::from_string).collect();
        if ops.iter().any(|x| !x.is_ok()) {
            return Err("Could not read input data");
        }
        Ok(Machine {
            tape: ops.drain(0..).map(|x| x.unwrap()).collect(),
            offset: 0i64,
            accumulator: 0i64,
        })
    }

    fn from_file(filename: impl AsRef<Path>) -> Result<Machine, &'static str> {
        use std::{
            fs::File,
            io::{prelude::*, BufReader},
        };
        let data: String = {
            let mut buffer = String::new();
            let file = File::open(filename);
            if !file.is_ok() {
                return Err("Could not open file");
            }
            let size = BufReader::new(file.unwrap()).read_to_string(&mut buffer);
            if !size.is_ok() {
                return Err("could not read file.");
            }
            buffer
        };
        Machine::from_string(&data)
    }

    fn next(&mut self) -> Option<(i64, i64)> {
        if self.offset >= self.tape.len() as i64 || self.offset < 0 {
            return None;
        }
        match self.tape[self.offset as usize] {
            Operation::Nop(_) => {
                self.offset += 1;
                Some((self.offset, self.accumulator))
            }
            Operation::Jmp(x) => {
                self.offset += x;
                Some((self.offset, self.accumulator))
            }
            Operation::Acc(x) => {
                self.offset += 1;
                self.accumulator += x;
                Some((self.offset, self.accumulator))
            }
        }
    }

    fn reset(&mut self) {
        self.offset = 0;
        self.accumulator = 0;
    }

    fn is_noend(&mut self) -> bool {
        let mut visited: Vec<bool> = self.tape.iter().enumerate().map(|(i, _)| i <= 0).collect();
        while let Some((i, _)) = self.next() {
            if i >= visited.len() as i64 {
                return false;
            }
            if visited[i as usize] {
                return true;
            };
            visited[i as usize] = true;
        }
        self.reset();
        false
    }
}

pub fn day8() -> (i64, i64) {
    let mut machine = Machine::from_file("data/day8.txt").unwrap();
    let first = {
        machine.is_noend();
        machine.accumulator
    };
    let second = {
        let mut result : i64 = 0;
        for (i, op) in machine
            .tape
            .iter()
            .enumerate()
            .filter_map(|(i, v)| match v {
                Operation::Nop(_) => Some((i, v)),
                Operation::Jmp(_) => Some((i, v)),
                _ => None,
            })
        {
            let mut tape: Vec<Operation> = machine.tape.clone();
            tape[i] = match op {
                Operation::Nop(x) => Operation::Jmp(*x),
                Operation::Jmp(x) => Operation::Nop(*x),
                Operation::Acc(x) => Operation::Acc(*x),
            };
            let mut modified = Machine {
                tape: tape,
                offset: 0i64,
                accumulator: 0i64,
            };
            if !modified.is_noend() {
                result = modified.accumulator;
                break;
            }
        }
        result
    };
    (first, second)
}

#[cfg(test)]
mod tests {
    #[test]
    fn operation_from_string() {
        assert_eq!(
            super::Operation::from_string("nop ssqwfas"),
            Ok(super::Operation::Nop)
        );
        assert_eq!(
            super::Operation::from_string("jmp 3"),
            Ok(super::Operation::Jmp(3))
        );
        assert_eq!(
            super::Operation::from_string("acc 3"),
            Ok(super::Operation::Acc(3))
        );
        assert!(!super::Operation::from_string("acc a3").is_ok());
        assert!(!super::Operation::from_string("wa a3").is_ok());
    }

    #[test]
    fn machine_from_string() {
        let definition = "nop 0\nacc 1\njmp -2";
        assert!(super::Machine::from_string(definition).is_ok());
        assert_eq!(
            super::Machine::from_string(definition).unwrap().tape.len(),
            3
        );
        assert_eq!(
            super::Machine::from_string(definition).unwrap().accumulator,
            0
        );
        assert_eq!(super::Machine::from_string(definition).unwrap().offset, 0);
    }

    #[test]
    fn machine_next_noend() {
        let mut machine = super::Machine::from_string("nop 0\nacc 1\njmp -2").unwrap();
        assert_eq!(machine.next(), Some((1, 0)));
        assert_eq!(machine.next(), Some((2, 1)));
        assert_eq!(machine.next(), Some((0, 1)));
    }

    #[test]
    fn machine_next_end() {
        let mut machine = super::Machine::from_string("nop 0\nacc 1").unwrap();
        assert_eq!(machine.next(), Some((1, 0)));
        assert_eq!(machine.next(), Some((2, 1)));
        assert_eq!(machine.next(), None);
    }
}
