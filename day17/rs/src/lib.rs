use std::ops::Range;

use regex::Regex;

use lazy_static::lazy_static;

lazy_static! {
    static ref RE: Regex = Regex::new(r"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)").unwrap();
    
    static ref INPUT: &'static str = include_str!("../../input");
}

fn parse_input(input: &str) -> (Range<i32>, Range<i32>) {
    let cap = RE.captures(input).expect("invalid input");

    (
        Range {
            start: cap[1].parse::<i32>().expect("invalid start x"),
            end: cap[2].parse::<i32>().expect("invalid end x") + 1,
        },
        Range {
            start: cap[3].parse::<i32>().expect("invalid start y"),
            end: cap[4].parse::<i32>().expect("invalid end y") + 1,
        },
    )
}

struct BallisticIterator {
    rx: Range<i32>,
    ry: Range<i32>,

    vx: i32,
    vy: i32,
}

impl BallisticIterator {
    fn new(input: &str) -> Self {
        let (rx, ry) = parse_input(input);

        Self {
            vx: 0,
            vy: -ry.start.abs(), 
            rx,
            ry,
        }
    }
}

impl Iterator for BallisticIterator {
    type Item = (i32, i32);

    fn next(&mut self) -> Option<Self::Item> {
        while self.vx < self.rx.end {
            if self.vx * (self.vx + 1) / 2 >= self.rx.start {
                while self.vy < self.ry.start.abs() {
                    let (mut dx, mut dy) = (self.vx, self.vy);
                    let (mut x, mut y) = (0, 0);

                    let r = loop {
                        if x >= self.rx.end || y < self.ry.start {
                            break None;
                        }
                        if x >= self.rx.start && y < self.ry.end {
                            break Some((self.vx, self.vy));
                        }

                        x += dx;
                        y += dy;

                        if dx > 0 {
                            dx -= 1;
                        }
                        dy -= 1;
                    };

                    self.vy += 1;

                    if r.is_some() {
                        return r;
                    }
                }
            }

            self.vx += 1;
            self.vy = -self.ry.start.abs();
        }

        None
    }
}

fn solve_1(input: &str) -> i32 {
    BallisticIterator::new(input)
        .map(|(_, vy)| vy * (vy + 1) / 2)
        .max()
        .unwrap()
}

fn solve_2(input: &str) -> usize {
    BallisticIterator::new(input).count()
}

pub fn part_1() -> i32 {
    solve_1(&INPUT)
}

pub fn part_2() -> usize {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"target area: x=20..30, y=-10..-5"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 45);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 112);
    }
}
