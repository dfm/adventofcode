use aoc::solver::Solver;
use std::collections::HashSet;

pub struct Day20;

type Int = i32;
type Decoder = [bool; 512];
type Image = HashSet<(Int, Int)>;

fn new(data: &str) -> (Decoder, Image) {
    let mut lines = data.lines();
    let mut decoder = [false; 512];
    for (n, c) in lines.next().unwrap().chars().enumerate() {
        decoder[n] = c == '#';
    }
    lines.next().unwrap();

    let mut image = Image::new();
    for (y, line) in lines.enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                image.insert((x as Int, y as Int));
            }
        }
    }

    (decoder, image)
}

fn range(image: &Image) -> (Int, Int, Int, Int) {
    image.iter().fold(
        (Int::MAX, Int::MIN, Int::MAX, Int::MIN),
        |(xmin, xmax, ymin, ymax), (x, y)| {
            (
                std::cmp::min(xmin, *x),
                std::cmp::max(xmax, *x),
                std::cmp::min(ymin, *y),
                std::cmp::max(ymax, *y),
            )
        },
    )
}

fn enhance(background: bool, decoder: &Decoder, image: &Image) -> (bool, Image) {
    let new_background = if background { decoder[511] } else { decoder[0] };

    let mut new_image = Image::new();
    let (xmin, xmax, ymin, ymax) = range(image);
    for y in ymin - 1..=ymax + 1 {
        for x in xmin - 1..=xmax + 1 {
            let mut ind = 0;
            let mut value = 0;
            for dy in [-1, 0, 1] {
                for dx in [-1, 0, 1] {
                    let isin = image.contains(&(x + dx, y + dy));
                    if isin != background {
                        value |= 1 << (8 - ind);
                    }
                    ind += 1;
                }
            }

            let toset = decoder[value];
            if toset != new_background {
                new_image.insert((x, y));
            }
        }
    }
    (new_background, new_image)
}

fn solve(data: &str, iter: usize) -> usize {
    let mut background = false;
    let (decoder, mut image) = new(data);
    for _ in 0..iter {
        let result = enhance(background, &decoder, &image);
        background = result.0;
        image = result.1;
    }
    image.len()
}

impl Solver<&str> for Day20 {
    fn part1(data: &str) -> usize {
        solve(data, 2)
    }

    fn part2(data: &str) -> usize {
        solve(data, 50)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
";

    #[test]
    fn test_part1() {
        assert_eq!(Day20::part1(DATA), 35);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day20::part2(DATA), 3351);
    }
}
