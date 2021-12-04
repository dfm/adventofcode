use anyhow::Result;
use aoc::solver::Solver;

pub struct Day03;

fn to_columns(data: &str) -> Result<Vec<Vec<bool>>> {
    let lines = data.lines();
    let mut result: Vec<Vec<bool>> = vec![];
    lines.for_each(|line| {
        result.resize(line.len(), Vec::new());
        result
            .iter_mut()
            .zip(line.chars())
            .for_each(|(r, c)| r.push(c == '1'))
    });
    Ok(result)
}

fn from_binary(mask: &[bool]) -> usize {
    mask.iter()
        .rev()
        .enumerate()
        .fold(0, |x, (i, &b)| x | ((b as usize) << i))
}

fn get_most_common(col: &[bool]) -> bool {
    let n = col.iter().filter(|&x| *x).count();
    n >= col.len() - n
}

fn solve_part2(columns: &[Vec<bool>], cmp: fn(bool, bool) -> bool) -> usize {
    let mut mask = columns[0].iter().map(|&_| true).collect::<Vec<bool>>();
    for col in columns.iter() {
        let common = get_most_common(
            &(col
                .iter()
                .zip(mask.iter())
                .filter(|(_, &m)| m)
                .map(|(&x, _)| x)
                .collect::<Vec<bool>>()),
        );
        mask = col
            .iter()
            .zip(mask.iter())
            .map(|(&x, &m)| cmp(x, common) & m)
            .collect();
        if mask.iter().filter(|&m| *m).count() == 1 {
            break;
        }
    }
    let x = columns
        .iter()
        .map(|col| *col.iter().zip(mask.iter()).find(|(_, &m)| m).unwrap().0)
        .collect::<Vec<bool>>();
    from_binary(&x)
}

impl Solver<&str> for Day03 {
    fn part1(data: &str) -> Result<String> {
        let columns = to_columns(data)?;
        let most_common = columns
            .iter()
            .map(|v| get_most_common(v))
            .collect::<Vec<bool>>();
        let eps = from_binary(&most_common);
        let gamma = from_binary(&(most_common.iter().map(|&x| !x).collect::<Vec<bool>>()));
        Ok(format!("{}", eps * gamma))
    }

    fn part2(data: &str) -> Result<String> {
        let columns = to_columns(data)?;
        let o2 = solve_part2(&columns, |a, b| a == b);
        let co2 = solve_part2(&columns, |a, b| a != b);
        Ok(format!("{}", co2 * o2))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let data = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
";
        assert_eq!(Day03::part1(&data).unwrap(), "198");
    }

    #[test]
    fn test_part2() {
        let data = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
";
        assert_eq!(Day03::part2(&data).unwrap(), "230");
    }
}
