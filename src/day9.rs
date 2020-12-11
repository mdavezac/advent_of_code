fn check_pair(data: &[i64]) -> Option<i64> {
    let x = data[data.len() - 1];
    for (i, val) in data[..data.len() - 1].iter().enumerate() {
        for v in data[(i + 1)..data.len() - 1].iter() {
            if v + val == x {
                return None;
            }
        }
    }
    Some(x)
}

pub fn day9() -> (i64, i64) {
    use std::fs;

    let data: Vec<i64> = fs::read_to_string("data/day9.txt")
        .expect("Something went wrong reading the file")
        .lines()
        .map(|l| l.parse::<i64>().unwrap())
        .collect();

    let first = {
        let mut result: i64 = 0;
        for i in 26..(data.len() - 1) {
            if let Some(x) = check_pair(&data[(i - 26)..i]) {
                result = x;
                break;
            }
        }
        result
    };
    let second = {
        let mut result = 0i64;
        for i in 0..(data.len() - 1) {
            for j in (i + 2)..(data.len() - 1) {
                if data[i..j].iter().sum::<i64>() == first {
                    result = data[i..j].iter().min().unwrap() + data[i..j].iter().max().unwrap();
                }
            }
        }
        result
    };
    (first, second)
}
