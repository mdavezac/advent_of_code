use std::{
    fs::File,
    io::{prelude::*, BufReader},
    path::Path,
    slice,
};

fn read_inputs(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect()
}

fn count_trees(strings: slice::Iter<String>, right: u64, down: u64) -> u64 {
    strings
        .step_by(down as usize)
        .enumerate()
        .map(|(i, l)| l.chars().nth(((right as usize) * i) % l.len()))
        .filter(|&x| x == Some('#'))
        .count() as u64
}

pub fn day3() -> (u64, u64) {
    let inputs = read_inputs("data/day3.txt");
    let first = count_trees(inputs.iter(), 3, 1);
    let second = vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        .iter()
        .map(|l| count_trees(inputs.iter(), l.0, l.1))
        .product();
    return (first, second);
}

#[cfg(test)]
mod tests {
    #[test]
    fn read_inputs() {
        let lines = super::read_inputs("data/day3.txt");
        assert_eq!(lines[0], "......#..##..#...#...#.###.....");
        assert_eq!(lines.last().unwrap(), ".....#....#..#..#...##...#...#.");
    }

    #[test]
    fn count_trees() {
        let notrees: Vec<String> = vec![
            String::from(".####"),
            String::from("###.#"),
            String::from("#.###"),
        ];
        assert_eq!(super::count_trees(notrees.iter(), 3, 1), 0);
        let alltrees: Vec<String> = vec![
            String::from("#...."),
            String::from("...#."),
            String::from(".#..."),
        ];
        assert_eq!(super::count_trees(alltrees.iter(), 3, 1), 3);
    }

    #[test]
    fn count_trees_with_steep_slope() {
        let notrees: Vec<String> = vec![
            String::from(".####"),
            String::from("#####"),
            String::from("#.###"),
            String::from("#####"),
        ];
        assert_eq!(super::count_trees(notrees.iter(), 1, 2), 0);
        assert_eq!(super::count_trees(notrees.iter(), 6, 2), 0);
        let alltrees: Vec<String> = vec![
            String::from("#...."),
            String::from("....."),
            String::from(".#..."),
            String::from("....."),
            String::from("..#.."),
        ];
        assert_eq!(super::count_trees(alltrees.iter(), 1, 2), 3);
        assert_eq!(super::count_trees(alltrees.iter(), 6, 2), 3);
        assert_eq!(super::count_trees(alltrees[0..4].iter(), 6, 2), 2);
    }

    #[test]
    fn count_trees_aoc_example() {
        let example: Vec<String> = vec![
            String::from("..##......."),
            String::from("#...#...#.."),
            String::from(".#....#..#."),
            String::from("..#.#...#.#"),
            String::from(".#...##..#."),
            String::from("..#.##....."),
            String::from(".#.#.#....#"),
            String::from(".#........#"),
            String::from("#.##...#..."),
            String::from("#...##....#"),
            String::from(".#..#...#.#"),
        ];
        assert_eq!(super::count_trees(example.iter(), 1, 1), 2);
        assert_eq!(super::count_trees(example.iter(), 3, 1), 7);
        assert_eq!(super::count_trees(example.iter(), 5, 1), 3);
        assert_eq!(super::count_trees(example.iter(), 7, 1), 4);
        assert_eq!(super::count_trees(example.iter(), 1, 2), 2);
        let second: u64 = vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
            .iter()
            .map(|l| super::count_trees(example.iter(), l.0, l.1))
            .product();
        assert_eq!(second, 336);
    }
}
