use std::num::ParseIntError;

#[cfg(feature = "itools")]
use itertools::*;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: Vec<u32> = parse_input(include_str!("../../input")).unwrap();
}

fn parse_input(input: &str) -> Result<Vec<u32>, ParseIntError> {
    input.lines().map(|l| l.parse()).collect()
}

fn solve_1(input: &[u32]) -> usize {
    let mut count = 0;

    let mut current = input[0];

    for &v in &input[1..] {
        if v > current {
            count += 1;
        }
        current = v;
    }

    count
}

#[cfg(not(feature = "itools"))]
fn solve_2(input: &[u32]) -> usize {
    input
        .windows(3)
        .fold((0, None), |(count, current), w| {
            match (current, w.iter().sum::<u32>()) {
                (Some(current), w) if w > current => (count + 1, Some(w)),
                (_, w) => (count, Some(w)),
            }
        })
        .0
}

#[cfg(feature = "itools")]
fn solve_2(input: &[u32]) -> usize {
    input
        .iter()
        .tuple_windows()
        .map(|(v1, v2, v3)| v1 + v2 + v3)
        .tuple_windows()
        .filter(|(prev, next)| next > prev)
        .count()
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
        static ref INPUT_STR: &'static str = r#"199
200
208
210
200
207
240
269
260
263"#;
        static ref INPUT: Vec<u32> = parse_input(&INPUT_STR).unwrap();
    }

    #[test]
    fn check_parse_input() {
        assert_eq!(
            parse_input(&INPUT_STR).unwrap(),
            [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
        );
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 7);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 5);
    }
}
