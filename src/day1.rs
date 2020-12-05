use std::{
    fs::File,
    io::{prelude::*, BufReader},
    path::Path,
};

fn ints_from_file(filename: impl AsRef<Path>) -> Vec<u32> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .filter_map(|l| l.parse::<u32>().ok())
        .collect()
}

fn find_sum(sum: u32, integers: &Vec<u32>) -> Option<(usize, usize)> {
    for i in 0..integers.len() {
        for j in (i + 1)..integers.len() {
            if integers[i] + integers[j] == sum {
                return Some((i, j));
            }
        }
    }
    return None;
}

fn find_sum3(sum: u32, integers: &Vec<u32>) -> Option<(usize, usize, usize)> {
    for i in 0..integers.len() {
        for j in (i + 1)..integers.len() {
            for k in (j + 1)..integers.len() {
                if integers[i] + integers[j] + integers[k] == sum {
                    return Some((i, j, k));
                }
            }
        }
    }
    return None;
}

#[allow(dead_code)]
pub fn day1() -> (u32, u32) {
    let integers = ints_from_file("data/day1.txt");
    let product2 = {
        let (i, j) = find_sum(2020u32, &integers).expect("value not found");
        integers[i] * integers[j]
    };
    let product3 = {
        let (i, j, k) = find_sum3(2020u32, &integers).expect("value not found");
        integers[i] * integers[j] * integers[k]
    };
    return (product2, product3);
}

#[cfg(test)]
mod tests {
    #[test]
    fn ints_from_file() {
        let integers = super::ints_from_file("data/day1.txt");
        assert_eq!(integers.len(), 200);
        assert_eq!(integers[0..2], [1046, 1565]);
        assert_eq!(integers.last(), Some(&1535u32));
    }

    #[test]
    fn find_sum() {
        assert_eq!(super::find_sum(10u32, &vec![5, 5, 3, 4]), Some((0, 1)));
        assert_eq!(super::find_sum(8u32, &vec![5, 5, 3, 4]), Some((0, 2)));
        assert_eq!(super::find_sum(7u32, &vec![5, 5, 3, 4]), Some((2, 3)));
    }

    #[test]
    fn find_sum3() {
        assert_eq!(super::find_sum3(13u32, &vec![5, 5, 3, 4]), Some((0, 1, 2)));
        assert_eq!(super::find_sum3(12u32, &vec![5, 6, 3, 4]), Some((0, 2, 3)));
    }

    #[test]
    fn day1() {
        let (first, second) = super::day1();
        assert_eq!(first, 888331u32);
        assert_eq!(second, 130933530u32);
    }
}
