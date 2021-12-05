use std::collections::HashMap;
use std::str::FromStr;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

type Display<K, V> = HashMap<K, V>;

#[derive(PartialEq, Debug, Eq, Hash)]
struct Point(i32, i32);

#[derive(PartialEq, Debug)]
struct Line(Point, Point);

impl FromStr for Point {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut iter = input.split(',');

        let mut parse_number = || {
            let token = iter.next().ok_or("expecting number")?;
            token.parse().map_err(|_| "number format")
        };

        Ok(Self(parse_number()?, parse_number()?))
    }
}

impl FromStr for Line {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut iter = input.split(" -> ");

        let mut parse_point = || {
            let token = iter.next().ok_or("expecting point")?;
            token.parse()
        };

        Ok(Self(parse_point()?, parse_point()?))
    }
}

impl Line {
    fn is_vertical(&self) -> bool {
        self.0 .0 == self.1 .0
    }

    fn is_horizontal(&self) -> bool {
        self.0 .1 == self.1 .1
    }

    fn draw(&self, display: &mut Display<Point, usize>) {
        let dx = (self.1 .0 - self.0 .0).signum();
        let dy = (self.1 .1 - self.0 .1).signum();

        let (mut x, mut y) = (self.0 .0, self.0 .1);
        loop {
            let pixel = display.entry(Point(x, y)).or_insert(0);

            *pixel += 1;

            x += dx;
            y += dy;

            if x == self.1 .0 && y == self.1 .1 {
                break;
            }
        }

        let pixel = display.entry(Point(x, y)).or_insert(0);

        *pixel += 1;
    }
}

fn solve<F>(input: &str, mut f: F) -> usize
where
    F: FnMut(&Line) -> bool,
{
    input
        .lines()
        .map(|line| line.parse())
        .try_fold(Display::new(), |mut display, line| {
            line.map(|line| {
                if f(&line) {
                    line.draw(&mut display);
                }
                display
            })
        })
        .expect("invalid input")
        .values()
        .filter(|v| **v > 1)
        .count()
}

fn solve_1(input: &str) -> usize {
    solve(input, |line| line.is_vertical() || line.is_horizontal())
}

fn solve_2(input: &str) -> usize {
    solve(input, |_| true)
}

pub fn part_1() -> usize {
    solve_1(&INPUT)
}

pub fn part_2() -> usize {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"#;
    }

    #[test]
    fn test_is_vertical() {
        assert!(Line(Point(0, 0), Point(0, 10)).is_vertical());
    }

    #[test]
    fn test_is_horizontal() {
        assert!(Line(Point(0, 0), Point(10, 0)).is_horizontal());
    }

    #[test]
    fn test_draw() {
        let mut display = Display::new();

        Line(Point(0, 0), Point(1, 0)).draw(&mut display);

        assert_eq!(display[&Point(0, 0)], 1);
        assert_eq!(display[&Point(1, 0)], 1);
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 5);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 12);
    }
}
