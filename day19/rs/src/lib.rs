use std::collections::HashMap;
use std::collections::HashSet;
use std::ops;
use std::str::FromStr;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

type Transform = fn(Point) -> Point;

fn all_transformations() -> Vec<Vec<Transform>> {
    let mut res = vec![];
    for f_swap in [swap_xyz, swap_yxz, swap_xzy, swap_yzx, swap_zxy, swap_zyx] {
        for f_change_sign_x in [change_sign_none, change_sign_x] {
            for f_change_sign_y in [change_sign_none, change_sign_y] {
                for f_change_sign_z in [change_sign_none, change_sign_z] {
                    res.push(vec![
                        f_swap,
                        f_change_sign_x,
                        f_change_sign_y,
                        f_change_sign_z,
                    ]);
                }
            }
        }
    }

    res
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
struct Point([i32; 3]);

#[derive(Debug)]
struct Scanner {
    beacons: HashSet<Point>,
    probes: Vec<Point>,
}

fn invalid_number<E>(_: E) -> &'static str {
    "invalid number"
}

impl Point {
    fn new(x: i32, y: i32, z: i32) -> Self {
        Self([x, y, z])
    }

    fn manhattan(self) -> i32 {
        self.0.into_iter().map(i32::abs).sum()
    }

    fn transform(mut self, p_pre: Point, transform: &[fn(Point) -> Point], p_post: Point) -> Self {
        self -= p_pre;
        for f in transform {
            self = f(self);
        }

        self + p_post
    }
}

impl ops::Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self([
            self.0[0] + other.0[0],
            self.0[1] + other.0[1],
            self.0[2] + other.0[2],
        ])
    }
}

impl ops::Sub for Point {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        Self([
            self.0[0] - other.0[0],
            self.0[1] - other.0[1],
            self.0[2] - other.0[2],
        ])
    }
}

impl ops::Neg for Point {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self([-self.0[0], -self.0[1], -self.0[2]])
    }
}

impl ops::SubAssign for Point {
    fn sub_assign(&mut self, other: Self) {
        self.0[0] -= other.0[0];
        self.0[1] -= other.0[1];
        self.0[2] -= other.0[2];
    }
}

impl FromStr for Point {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut parts = input.split(',').map(i32::from_str);

        Ok(Point([
            parts.next().ok_or("x not found")?.map_err(invalid_number)?,
            parts.next().ok_or("y not found")?.map_err(invalid_number)?,
            parts.next().ok_or("z not found")?.map_err(invalid_number)?,
        ]))
    }
}

impl Scanner {
    fn new(beacons: HashSet<Point>, probes: Vec<Point>) -> Self {
        Self { beacons, probes }
    }

    fn transform(
        &self,
        p_pre: Point,
        transformation: &[fn(Point) -> Point],
        p_post: Point,
    ) -> HashSet<Point> {
        self.beacons
            .iter()
            .copied()
            .map(|p| p.transform(p_pre, transformation, p_post))
            .collect()
    }

    fn parse(input: &str) -> Result<(String, Scanner), &'static str> {
        let mut lines = input.lines();

        let id = lines
            .next()
            .ok_or("scanner not found")?
            .split(' ')
            .nth(2)
            .ok_or("scanner id not found")?
            .parse()
            .map_err(invalid_number)?;

        let beacons = lines
            .map(Point::from_str)
            .collect::<Result<HashSet<_>, _>>()?;

        let probes = vec![Point::new(0, 0, 0)];

        Ok((id, Self::new(beacons, probes)))
    }
}

fn parse_input(input: &str) -> Result<HashMap<String, Scanner>, &'static str> {
    input.split("\n\n").map(Scanner::parse).collect()
}

fn swap_xyz(p: Point) -> Point {
    p
}

fn swap_yxz(p: Point) -> Point {
    Point([p.0[1], p.0[0], p.0[2]])
}

fn swap_xzy(p: Point) -> Point {
    Point([p.0[0], p.0[2], p.0[1]])
}

fn swap_yzx(p: Point) -> Point {
    Point([p.0[1], p.0[2], p.0[0]])
}

fn swap_zxy(p: Point) -> Point {
    Point([p.0[2], p.0[0], p.0[1]])
}

fn swap_zyx(p: Point) -> Point {
    Point([p.0[2], p.0[1], p.0[0]])
}

fn change_sign_none(p: Point) -> Point {
    p
}

fn change_sign_x(mut p: Point) -> Point {
    p.0[0] = -p.0[0];
    p
}

fn change_sign_y(mut p: Point) -> Point {
    p.0[1] = -p.0[1];
    p
}

fn change_sign_z(mut p: Point) -> Point {
    p.0[2] = -p.0[2];
    p
}

fn find_merge(
    scanners: &HashMap<String, Scanner>,
) -> Option<(String, String, HashSet<Point>, Vec<Point>)> {
    for (id1, s1) in scanners.iter() {
        for (id2, s2) in scanners.iter().filter(|(id, _)| id != &id1) {
            for p1 in s1.beacons.iter().copied() {
                for p2 in s2.beacons.iter().copied() {
                    for transform in &all_transformations() {
                        let ts2 = s2.transform(p2, transform, p1);
                        if ts2.intersection(&s1.beacons).count() < 12 {
                            continue;
                        }

                        if ts2.difference(&s1.beacons).any(|p| {
                            s1.probes
                                .iter()
                                .copied()
                                .any(|o| (*p - o).manhattan() <= 1000)
                        }) {
                            continue;
                        }

                        let probes = s2
                            .probes
                            .iter()
                            .copied()
                            .map(|p| p.transform(p2, transform, p1))
                            .collect::<Vec<_>>();

                        if s1
                            .beacons
                            .difference(&ts2)
                            .any(|p| probes.iter().copied().any(|o| (*p - o).manhattan() <= 1000))
                        {
                            continue;
                        }

                        return Some((id1.to_owned(), id2.to_owned(), ts2, probes));
                    }
                }
            }
        }
    }

    None
}

fn solve(input: &str) -> Scanner {
    let mut scanners = parse_input(input).expect("invalid input");
    while let Some((id1, id2, beacons, mut probes)) = find_merge(&scanners) {
        let s1 = scanners.remove(&id1).unwrap();
        scanners.remove(&id2).unwrap();

        let merged_beacons = s1.beacons.union(&beacons).copied().collect();
        let mut merged_probes = s1.probes.to_owned();
        merged_probes.append(&mut probes);

        scanners.insert(
            format!("({}+{})", id1, id2),
            Scanner::new(merged_beacons, merged_probes),
        );
    }

    assert_eq!(scanners.len(), 1);

    let scanner = scanners.drain().map(|(_, scanner)| scanner).next().unwrap();
    scanner
}

fn solve_1(input: &str) -> usize {
    solve(input).beacons.len()
}

fn solve_2(input: &str) -> i32 {
    let probes = solve(input).probes;
    let mut max = 0;
    for (i, p1) in probes.iter().copied().enumerate().take(probes.len() - 1) {
        for p2 in probes.iter().skip(i + 1).copied() {
            max = max.max((p1 - p2).manhattan());
        }
    }

    max
}

pub fn part_1() -> usize {
    solve_1(&INPUT)
}

pub fn part_2() -> i32 {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = include_str!("../../inputex");
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 79);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 3621);
    }

    #[test]
    fn test_all_transformations() {
        assert_eq!(all_transformations().len(), 48);
    }
}
