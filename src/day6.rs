use std::collections::HashSet;
use std::path::Path;

fn read_inputs(filename: impl AsRef<Path>) -> Vec<String> {
    use regex::Regex;
    use std::fs::read_to_string;

    let data = read_to_string(filename).expect("no such file");
    let splitter = Regex::new(r"(?m)^\s*$").unwrap();
    splitter
        .split(&data)
        .filter(|l| !l.is_empty())
        .map(|l| {
            l.replace("\n", " ")
                .trim_end_matches(' ')
                .trim_start_matches(' ')
                .to_owned()
        })
        .collect()
}

fn union(data: &str) -> HashSet<char> {
    let sets: Vec<HashSet<_>> = data
        .split(' ')
        .map(|l| l.chars().collect::<HashSet<_>>())
        .collect();
    match sets.len() {
        0 => HashSet::new(),
        1 => sets[0].clone(),
        _ => sets[1..].iter().fold(sets[0].clone(), |acc, s| {
            acc.union(&s).cloned().collect::<HashSet<char>>()
        }),
    }
}

fn intersection(data: &str) -> HashSet<char> {
    let sets: Vec<HashSet<_>> = data
        .split(' ')
        .map(|l| l.chars().collect::<HashSet<_>>())
        .collect();
    match sets.len() {
        0 => HashSet::new(),
        1 => sets[0].clone(),
        _ => sets[1..].iter().fold(sets[0].clone(), |acc, s| {
            acc.intersection(&s).cloned().collect::<HashSet<char>>()
        }),
    }
}

pub fn day6() -> (u32, u32) {
    let inputs = read_inputs("data/day6.txt");
    (
        inputs.iter().map(|l| union(l).len() as u32).sum(),
        inputs.iter().map(|l| intersection(l).len() as u32).sum(),
    )
}

#[cfg(test)]
mod tests {

    #[test]
    fn read_inputs() {
        let data = super::read_inputs("data/day6.txt");
        assert_eq!(data[0], "l l vqb");
        assert_eq!(
            data.last(),
            Some(&"wmfpvn whbzmvjplc vwpsmk sovwpm msvrpwdf".to_owned())
        );
    }

    #[test]
    fn union() {
        use std::collections::HashSet;
        assert_eq!(
            super::union("l l la"),
            vec!['l', 'a'].iter().cloned().collect::<HashSet<char>>()
        );
        assert_eq!(
            super::union("lbca lad alx"),
            vec!['a', 'b', 'c', 'd', 'l', 'x']
                .iter()
                .cloned()
                .collect::<HashSet<char>>()
        );
        assert_eq!(super::union(""), HashSet::<char>::new());
        assert_eq!(
            super::union("a"),
            vec!['a'].iter().cloned().collect::<HashSet<char>>()
        );
    }

    #[test]
    fn intersection() {
        use std::collections::HashSet;
        assert_eq!(
            super::intersection("l l la"),
            vec!['l'].iter().cloned().collect::<HashSet<char>>()
        );
        assert_eq!(
            super::intersection("lbca lad alx"),
            vec!['l', 'a'].iter().cloned().collect::<HashSet<char>>()
        );
        assert_eq!(super::intersection("a b c"), HashSet::<char>::new());
        assert_eq!(super::intersection(""), HashSet::<char>::new());
        assert_eq!(
            super::intersection("a"),
            vec!['a'].iter().cloned().collect::<HashSet<char>>()
        );
    }
}
