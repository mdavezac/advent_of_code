use std::collections::HashMap;
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

fn to_passport(data: &str) -> HashMap<String, String> {
    data.split(' ')
        .map(|l| {
            let xy: Vec<&str> = l.split(':').take(2).collect();
            (xy[0].to_owned(), xy[1].to_owned())
        })
        .collect::<HashMap<String, String>>()
}

fn has_required_keys(passport: &HashMap<String, String>) -> bool {
    for required in &["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] {
        if !passport.contains_key(*required) {
            return false;
        }
    }
    true
}

fn is_valid(passport: &HashMap<String, String>) -> bool {
    use regex::Regex;
    if !has_required_keys(passport) {
        return false;
    }
    if let Some(byr) = passport.get("byr") {
        if let Ok(year) = byr.parse::<u32>() {
            if byr.len() != 4 || year < 1920 || year > 2002 {
                return false;
            }
        }
    }
    if let Some(iyr) = passport.get("iyr") {
        if let Ok(year) = iyr.parse::<u32>() {
            if iyr.len() != 4 || year < 2010 || year > 2020 {
                return false;
            }
        }
    }
    if let Some(eyr) = passport.get("eyr") {
        if let Ok(year) = eyr.parse::<u32>() {
            if eyr.len() != 4 || year < 2020 || year > 2030 {
                return false;
            }
        }
    }
    let height: &str = passport.get("hgt").unwrap();
    if let Some(height_cm) = height.strip_suffix("cm") {
        if let Ok(cm) = height_cm.parse::<u32>() {
            if height_cm.len() != 3 || cm < 150 || cm > 193 {
                return false;
            }
        }
    } else if let Some(height_in) = height.strip_suffix("in") {
        if let Ok(hin) = height_in.parse::<u32>() {
            if height_in.len() != 2 || hin < 59 || hin > 76 {
                return false;
            }
        }
    } else {
        return false;
    }
    if let Some(eye_color) = passport.get("ecl") {
        let eye_colors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];
        if !eye_colors.iter().any(|x| x == eye_color) {
            return false;
        }
    }
    if let Some(hair_color) = passport.get("hcl") {
        let regex = Regex::new(r"^#((?:\d|[a-f]){6})$").unwrap();
        if !regex.is_match(hair_color) {
            return false;
        }
    }
    if let Some(pid) = passport.get("pid") {
        let regex = Regex::new(r"^\d{9}$").unwrap();
        if !regex.is_match(pid) {
            return false;
        }
    }
    true
}

pub fn day4() -> (usize, usize) {
    let passports: Vec<HashMap<String, String>> = read_inputs("data/day4.txt")
        .iter()
        .map(|l| to_passport(&l))
        .filter(has_required_keys)
        .collect();
    (
        passports.len(),
        passports.iter().filter(|x| is_valid(x)).count(),
    )
}

#[cfg(test)]
mod tests {

    #[test]
    fn read_inputs() {
        let data = super::read_inputs("data/day4.txt");
        assert_eq!(
            data[0],
            "byr:1971 iyr:2017 hgt:160cm eyr:2020 ecl:hzl pid:157096267"
        );
        assert_eq!(
            data.last().unwrap(),
            "hcl:#95f96b hgt:193cm iyr:2020 pid:719337690 byr:1971 ecl:brn eyr:2024"
        );
    }

    #[test]
    fn to_passport() {
        use std::collections::HashMap;
        assert_eq!(
            super::to_passport("z:1 b:2"),
            vec![("z", "1"), ("b", "2")]
                .iter()
                .map(|kv| (kv.0.to_owned(), kv.1.to_owned()))
                .collect::<HashMap<_, _>>()
        );
    }
}
