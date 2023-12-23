use rayon::prelude::*;

pub type Coord = (i64, i64, i64);

pub fn parse(data: &str) -> Vec<(Coord, Coord)> {
  let mut blocks: Vec<(Coord, Coord)> = data
    .lines()
    .map(|line| {
      let (a, b) = line.split_once('~').unwrap();
      let mut a = a.split(',');
      let mut b = b.split(',');
      (
        (
          a.next().unwrap().parse().unwrap(),
          a.next().unwrap().parse().unwrap(),
          a.next().unwrap().parse().unwrap(),
        ),
        (
          b.next().unwrap().parse().unwrap(),
          b.next().unwrap().parse().unwrap(),
          b.next().unwrap().parse().unwrap(),
        ),
      )
    })
    .collect();

  blocks.sort_by_key(|&((_, _, z1), (_, _, z2))| z1.min(z2));
  for idx in 0..blocks.len() {
    let mut block = blocks[idx];
    loop {
      block.0 .2 -= 1;
      block.1 .2 -= 1;
      if block.0 .2 <= 0
        || block.1 .2 <= 0
        || blocks
          .iter()
          .take(idx)
          .any(|&other| intersect(block, other))
      {
        break;
      }
    }
    block.0 .2 += 1;
    block.1 .2 += 1;
    blocks[idx] = block;
  }

  blocks
}

pub fn part1(blocks: &[(Coord, Coord)]) -> usize {
  (0..blocks.len())
    .into_par_iter()
    .map(|idx| {
      for n in idx + 1..blocks.len() {
        let mut block = blocks[n];
        block.0 .2 -= 1;
        block.1 .2 -= 1;
        if block.0 .2 <= 0 || block.1 .2 <= 0 {
          continue;
        }
        if blocks
          .iter()
          .take(n)
          .enumerate()
          .any(|(m, &other)| m != idx && intersect(block, other))
        {
          continue;
        }
        return 0;
      }
      1
    })
    .sum()
}

pub fn part2(blocks: &[(Coord, Coord)]) -> usize {
  (0..blocks.len())
    .into_par_iter()
    .map(|idx| {
      let mut blocks = blocks.to_vec();
      let mut count = 0;
      for n in idx + 1..blocks.len() {
        let mut block = blocks[n];
        block.0 .2 -= 1;
        block.1 .2 -= 1;
        if block.0 .2 > 0
          && block.1 .2 > 0
          && blocks
            .iter()
            .enumerate()
            .take(n)
            .all(|(m, &other)| m == idx || !intersect(block, other))
        {
          count += 1;
          blocks[n] = block;
        }
      }
      count
    })
    .sum()
}

fn overlap(a1: i64, a2: i64, b1: i64, b2: i64) -> bool {
  b1 <= a2 && b2 >= a1
}

fn intersect(
  ((ax1, ay1, az1), (ax2, ay2, az2)): (Coord, Coord),
  ((bx1, by1, bz1), (bx2, by2, bz2)): (Coord, Coord),
) -> bool {
  overlap(ax1, ax2, bx1, bx2) && overlap(ay1, ay2, by1, by2) && overlap(az1, az2, bz1, bz2)
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 5);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 7);
  }
}
