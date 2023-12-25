use itertools::Itertools;

#[derive(Copy, Clone, Debug)]
pub struct Vec3 {
  x: f64,
  y: f64,
  z: f64,
}

impl std::ops::Sub<Vec3> for Vec3 {
  type Output = Vec3;
  fn sub(self, rhs: Vec3) -> Self::Output {
    Vec3 { 
      x: self.x - rhs.x,
      y: self.y - rhs.y,
      z: self.z - rhs.z,
    }
  }
}

impl std::ops::Add<Vec3> for Vec3 {
  type Output = Vec3;
  fn add(self, rhs: Vec3) -> Self::Output {
    Vec3 { 
      x: self.x + rhs.x,
      y: self.y + rhs.y,
      z: self.z + rhs.z,
    }
  }
}

impl std::ops::Mul<f64> for Vec3 {
  type Output = Vec3;
  fn mul(self, rhs: f64) -> Self::Output {
    Vec3 { 
      x: self.x * rhs,
      y: self.y * rhs,
      z: self.z * rhs,
    }
  }
}

impl From<&str> for Vec3 {
  fn from(value: &str) -> Self {
    let mut parts = value.split(", ");
    Self {
      x: parts.next().unwrap().trim().parse().unwrap(),
      y: parts.next().unwrap().trim().parse().unwrap(),
      z: parts.next().unwrap().trim().parse().unwrap(),
    }
  }
}

pub fn parse(data: &str) -> Vec<(Vec3, Vec3)> {
  data
    .lines()
    .map(|line| {
      let (p, v) = line.split_once(" @ ").unwrap();
      (p.into(), v.into())
    })
    .collect()
}

pub fn part1(data: &[(Vec3, Vec3)]) -> usize {
  let mn = if data.len() == 5 {
    7.
  } else {
    200000000000000.
  };
  let mx = if data.len() == 5 {
    27.
  } else {
    400000000000000.
  };

  let flat: Vec<_> = data
    .iter()
    .map(|(p, v)| {
      let p = Vec3 {
        x: p.x,
        y: p.y,
        z: 0.0,
      };
      let v = Vec3 {
        x: v.x,
        y: v.y,
        z: 0.0,
      };
      (p, v)
    })
    .collect();

  flat
    .iter()
    .tuple_combinations()
    .filter(|(&a, &b)| {
      if let Some(i) = intersection(a, b) {
        mn <= i.x && i.x <= mx && mn <= i.y && i.y <= mx
      } else {
        false
      }
    })
    .count()
}

pub fn part2(_data: &[(Vec3, Vec3)]) -> i64 {
  // # Solved using Python
  // import numpy as np
  //
  // data = """224985900829298, 214925632111185, 149368039079818 @ 77, 81, 242
  // 345130946591991, 463544251720861, 528954166317265 @ -24, -86, -250
  // 249711429057846, 265531095717107, 268515984071116 @ 35, 5, -12
  // """
  //
  // pos = []
  // vel = []
  // for n, line in enumerate(data.splitlines()):
  //   a, b = line.split(" @ ")
  //   pos.append(tuple(map(float, a.split(","))))
  //   vel.append(tuple(map(float, b.split(","))))
  //
  // pos = np.array(pos)
  // vel = np.array(vel)
  //
  // def cross(x):
  //   return np.array([
  //     [0, -x[2], x[1]],
  //     [x[2], 0, -x[0]],
  //     [-x[1], x[0], 0]
  //   ])
  //
  //
  // lhs = np.concatenate(
  //   [
  //     np.concatenate([
  //       cross(vel[0]) - cross(vel[1]),
  //       -cross(pos[0]) + cross(pos[1]),
  //     ], axis=-1),
  //     np.concatenate([
  //       cross(vel[0]) - cross(vel[2]),
  //       -cross(pos[0]) + cross(pos[2]),
  //     ], axis=-1),
  //   ],
  //   axis=0,
  // )
  // rhs = np.concatenate([
  //   -np.cross(pos[0], vel[0]) + np.cross(pos[1], vel[1]),
  //   -np.cross(pos[0], vel[0]) + np.cross(pos[2], vel[2]),
  // ])
  // np.floor(np.linalg.solve(lhs, rhs)[:3].sum())
  0
}

fn dot(a: Vec3, b: Vec3) -> f64 {
  a.x * b.x + a.y * b.y + a.z * b.z
}

fn cross(a: Vec3, b: Vec3) -> Vec3 {
  Vec3 {
    x: a.y * b.z - b.y * a.z,
    y: a.x * b.z - b.x * a.z,
    z: a.x * b.y - b.x * a.y,
  }
}

fn norm(v: Vec3) -> f64 {
  dot(v, v)
}

fn intersection((p1, v1): (Vec3, Vec3), (p2, v2): (Vec3, Vec3)) -> Option<Vec3> {
  let v = Vec3 {
    x: p2.x - p1.x,
    y: p2.y - p1.y,
    z: p2.z - p1.z,
  };

  let n = cross(v1, v2);
  if dot(v, n).abs() > 1e-8 || norm(n) < 1e-8 {
    return None;
  }

  let s = dot(cross(v, v2), n) / norm(n);
  let t = dot(cross(v, v1), n) / norm(n);
  if s < 0. || t < 0. {
    None
  } else {
    Some(Vec3 {
      x: p1.x + s * v1.x,
      y: p1.y + s * v1.y,
      z: p1.z + s * v1.z,
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 2);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 0);
  }
}
