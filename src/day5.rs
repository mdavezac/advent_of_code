use std::path::Path;

fn read_inputs(filename: impl AsRef<Path>) -> Vec<(String, String)> {
    use std::{
        fs::File,
        io::{prelude::*, BufReader},
    };
    let file = File::open(filename).expect("no such file");
    BufReader::new(file)
        .lines()
        .map(|l| l.expect("Could not parse line"))
        .filter(|l| l.len() == 10)
        .map(|l| (l[..7].to_owned(), l[7..].to_owned()))
        .collect()
}

fn to_number(data: &str) -> u32 {
    let mut start = 0u32;
    let mut end = 2u32.pow(data.len() as u32);
    for character in data.chars() {
        let mid = (end + start) / 2;
        if character == 'B' || character == 'R' {
            start = mid;
        } else {
            end = mid;
        }
    }
    start
}

pub fn day5() -> (u32, u32) {
    let inputs = read_inputs("data/day5.txt");
    let mut ids: Vec<u32> = inputs
        .iter()
        .map(|(row, col)| to_number(row) * 8 + to_number(col))
        .collect();
    ids.sort();
    let second = ids
        .iter()
        .zip(ids[1..].iter())
        .find(|tp| (tp.0 + 1) != *tp.1)
        .unwrap();
    (ids[ids.len() - 1].to_owned(), second.0 + 1)
}

#[cfg(test)]
mod tests {
    #[test]
    fn read_inputs() {
        let strings = super::read_inputs("data/day5.txt");
        assert_eq!(strings.len(), 884);
        assert!(strings.iter().all(|x| x.0.len() == 7));
        assert!(strings.iter().all(|x| x.1.len() == 3));
        assert!(strings[0].0 == "BFFFBFF");
        assert!(strings[0].1 == "RLR");
    }

    #[test]
    fn to_number_row() {
        assert_eq!(super::to_number("BBB"), 7);
        assert_eq!(super::to_number("BFB"), 5);
        assert_eq!(super::to_number("BFF"), 4);
        assert_eq!(super::to_number("F"), 0);
        assert_eq!(super::to_number("B"), 1);
        assert_eq!(super::to_number("L"), 0);
        assert_eq!(super::to_number("R"), 1);
        assert_eq!(super::to_number("BFFFBBF"), 70)
    }
}
