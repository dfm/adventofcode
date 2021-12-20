use aoc::solver::Solver;
use std::collections::{HashMap, HashSet, VecDeque};

pub struct Day19;

enum Axis {
    X,
    Y,
    Z,
}
type TriangleHash = (i64, i64, i64);
type Rotation = (u8, u8, u8);
type Transform = (Point, Rotation);

#[derive(Debug)]
struct Scanner {
    points: HashSet<Point>,
    hashes: HashMap<TriangleHash, Triangle>,
}

#[derive(Debug, Eq, PartialEq, Hash, PartialOrd, Ord, Copy, Clone)]
struct Point(i64, i64, i64);

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
struct Triangle(Point, Point, Point);

impl Triangle {
    fn com(&self) -> Point {
        Point(
            self.0 .0 + self.1 .0 + self.2 .0,
            self.0 .1 + self.1 .1 + self.2 .1,
            self.0 .2 + self.1 .2 + self.2 .2,
        )
    }

    fn rotate(&self, axis: &Axis, angle: u8) -> Triangle {
        Triangle(
            self.0.rotate(axis, angle),
            self.1.rotate(axis, angle),
            self.2.rotate(axis, angle),
        )
    }
}

impl Point {
    fn rotate(&self, axis: &Axis, angle: u8) -> Self {
        let cos = match angle {
            0 => 1i64,
            2 => -1i64,
            _ => 0i64,
        };
        let sin = match angle {
            1 => 1i64,
            3 => -1i64,
            _ => 0i64,
        };
        match axis {
            Axis::X => Self(
                self.0,
                cos * self.1 + sin * self.2,
                cos * self.2 - sin * self.1,
            ),
            Axis::Y => Self(
                cos * self.0 + sin * self.2,
                self.1,
                cos * self.2 - sin * self.0,
            ),
            Axis::Z => Self(
                cos * self.0 + sin * self.1,
                cos * self.1 - sin * self.0,
                self.2,
            ),
        }
    }
}

impl std::ops::Sub for Point {
    type Output = Point;
    fn sub(self, other: Point) -> Self::Output {
        Point(self.0 - other.0, self.1 - other.1, self.2 - other.2)
    }
}

fn distance(p1: &Point, p2: &Point) -> i64 {
    let dx = p1.0 - p2.0;
    let dy = p1.1 - p2.1;
    let dz = p1.2 - p2.2;
    dx * dx + dy * dy + dz * dz
}

fn sort_triangle((a, b, c): TriangleHash) -> TriangleHash {
    fn sort2(x: i64, y: i64) -> (i64, i64) {
        (std::cmp::min(x, y), std::cmp::max(x, y))
    }
    let (a, b) = sort2(a, b);
    let (b, c) = sort2(b, c);
    let (a, b) = sort2(a, b);
    (a, b, c)
}

fn hash_points(points: &HashSet<Point>) -> Scanner {
    let mut hashes = HashMap::new();
    for reference in points.iter() {
        let mut dist = points
            .iter()
            .map(|target| (distance(reference, target), target))
            .collect::<Vec<_>>();
        dist.sort_unstable();
        let d1 = dist[1].0;
        let d2 = dist[2].0;
        let d3 = distance(dist[1].1, dist[2].1);
        hashes.insert(
            sort_triangle((d1, d2, d3)),
            Triangle(*reference, *dist[1].1, *dist[2].1),
        );
    }
    Scanner {
        points: points.clone(),
        hashes,
    }
}

fn parse_scanner(data: &str) -> Scanner {
    let points = data
        .lines()
        .skip(1)
        .map(|line| {
            let mut tokens = line.split(',');
            Point(
                tokens.next().unwrap().parse().unwrap(),
                tokens.next().unwrap().parse().unwrap(),
                tokens.next().unwrap().parse().unwrap(),
            )
        })
        .collect::<HashSet<_>>();
    hash_points(&points)
}

fn parse_scanners(data: &str) -> VecDeque<Scanner> {
    data.split("\n\n").map(parse_scanner).collect()
}

fn align_triangles(reference: &Triangle, target: &Triangle) -> HashSet<(Point, (u8, u8, u8))> {
    let mut aligns = HashSet::new();
    for rx in 0..4 {
        for ry in 0..4 {
            for rz in 0..4 {
                let dx = target
                    .rotate(&Axis::X, rx)
                    .rotate(&Axis::Y, ry)
                    .rotate(&Axis::Z, rz)
                    .com()
                    - reference.com();
                aligns.insert((dx, (rx, ry, rz)));
            }
        }
    }
    aligns
}

fn find_transform(s1: &Scanner, s2: &Scanner) -> Option<Transform> {
    let sets = s2
        .hashes
        .iter()
        .filter_map(|(k, tri)| {
            s1.hashes
                .get(k)
                .map(|reference| align_triangles(reference, tri))
        })
        .collect::<Vec<_>>();
    if sets.is_empty() {
        return None;
    }
    sets[0]
        .iter()
        .filter(|k| sets[1..].iter().all(|s| s.contains(k)))
        .map(|(p, rot)| (Point(p.0 / 3, p.1 / 3, p.2 / 3), *rot))
        .next()
}

fn apply_transform((dx, rot): Transform, s1: &Scanner, s2: &Scanner) -> Scanner {
    let mut points = s1.points.clone();
    for x in s2.points.iter() {
        points.insert(
            x.rotate(&Axis::X, rot.0)
                .rotate(&Axis::Y, rot.1)
                .rotate(&Axis::Z, rot.2)
                - dx,
        );
    }
    hash_points(&points)
}

fn get_aligned(data: &str) -> (Vec<Point>, Scanner) {
    let mut locations = Vec::new();
    let mut scanners = parse_scanners(data);
    let mut reference = scanners.pop_front().unwrap();
    while let Some(next) = scanners.pop_front() {
        if let Some(trans) = find_transform(&reference, &next) {
            reference = apply_transform(trans, &reference, &next);
            locations.push(trans.0);
        } else {
            scanners.push_back(next);
        }
    }
    (locations, reference)
}

impl Solver<&str> for Day19 {
    fn part1(data: &str) -> usize {
        get_aligned(data).1.points.len()
    }

    fn part2(data: &str) -> usize {
        let points = get_aligned(data).0;
        let mut max = 0;
        for &x in points.iter() {
            for &y in points.iter() {
                let dist = (x.0 - y.0).abs() + (x.1 - y.1).abs() + (x.2 - y.2).abs();
                max = std::cmp::max(max, dist as usize);
            }
        }
        max
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14
";

    #[test]
    fn test_part1() {
        assert_eq!(Day19::part1(DATA), 79);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day19::part2(DATA), 3621);
    }
}

// // Alternative implementation:
// use aoc::counter::Counter;
// use std::iter::FromIterator;

// type Coord = (i32, i32, i32);
// type Rotation = (u8, u8, u8);

// fn diff(x: &Coord, y: &Coord) -> Coord {
//     (x.0 - y.0, x.1 - y.1, x.2 - y.2)
// }

// fn rotate(coord: &Coord, rotation: &Rotation) -> Coord {
//     let x = coord.0;
//     let y = coord.1;
//     let z = coord.2;
//     let (y, z) = rotate_impl(y, z, rotation.0);
//     let (x, z) = rotate_impl(x, z, rotation.1);
//     let (x, y) = rotate_impl(x, y, rotation.2);
//     (x, y, z)
// }

// fn rotate_impl(x: i32, y: i32, angle: u8) -> (i32, i32) {
//     match angle % 4 {
//         0 => (x, y),
//         1 => (-y, x),
//         2 => (-x, -y),
//         3 => (y, -x),
//         _ => unreachable!(),
//     }
// }

// fn count_delta(
//     rotation: &Rotation,
//     reference: &[Coord],
//     target: &[Coord],
// ) -> HashMap<Coord, usize> {
//     let mut counter = HashMap::new();
//     for x in target.iter() {
//         let x = rotate(x, rotation);
//         for y in reference.iter() {
//             counter.increment(diff(&x, y), 1);
//         }
//     }
//     counter
// }

// fn align_data(reference: &[Coord], target: &[Coord]) -> Option<HashSet<Coord>> {
//     for rx in 0..4 {
//         for ry in 0..4 {
//             for rz in 0..4 {
//                 let counter = count_delta(&(rx, ry, rz), reference, target);
//                 if let Some((x, _)) = counter.iter().find(|(_, &n)| n >= 12) {
//                     let mut result = HashSet::from_iter(reference.iter().cloned());
//                     for y in target.iter() {
//                         result.insert(diff(&rotate(y, &(rx, ry, rz)), x));
//                     }
//                     return Some(result);
//                 }
//             }
//         }
//     }
//     None
// }
