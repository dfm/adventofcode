pub fn parse(data: &str) -> Result<Vec<i64>, std::num::ParseIntError> {
    let result = data.lines().map(|x| x.parse());
    result.collect()
}

pub fn part1(data: &[i64]) -> i64 {
    0
}

pub fn part2(data: &[i64]) -> i64 {
    0
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let result = parse(r"0
10").unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(result, vec![0, 10]);
    }
}
