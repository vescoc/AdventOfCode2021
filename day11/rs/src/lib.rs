#![no_std]

use core::cmp::Ordering;
use heapless::Vec;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

fn steps(octopuses: &mut [u32], steps: usize) -> usize {
    fn checked_add(v: usize, d: isize) -> Option<usize> {
        match d.cmp(&0) {
            Ordering::Equal => Some(v),
            Ordering::Less => v.checked_sub(1),
            Ordering::Greater => {
                let nv = v + 1;
                if nv > 9 {
                    None
                } else {
                    Some(nv)
                }
            }
        }
    }

    let mut count = 0;

    let mut flashing: Vec<_, 100> = Vec::new();
    let mut flashed: Vec<_, 100> = Vec::new();

    for _ in 0..steps {
        // step: increment energy
        for (i, v) in octopuses.iter_mut().enumerate() {
            *v += 1;
            if *v == 10 {
                flashing.push((i % 10, i / 10)).ok();
            }
        }

        // step: flashing
        while let Some((x, y)) = flashing.pop() {
            for (dx, dy) in [
                (-1, -1),
                (0, -1),
                (1, -1),
                (-1, 0),
                (1, 0),
                (-1, 1),
                (0, 1),
                (1, 1),
            ] {
                if let (Some(nx), Some(ny)) = (checked_add(x, dx), checked_add(y, dy)) {
                    let ni = ny * 10 + nx;
                    if octopuses[ni] < 10 {
                        octopuses[ni] += 1;
                        if octopuses[ni] == 10 {
                            flashing.push((nx, ny)).ok();
                        }
                    }
                }
            }

            flashed.push((x, y)).ok();
        }

        count += flashed.len();

        while let Some((x, y)) = flashed.pop() {
            octopuses[y * 10 + x] = 0;
        }
    }

    count
}

fn parse_input(input: &str) -> Vec<u32, 100> {
    input
        .lines()
        .flat_map(|line| line.chars().map(|c| c.to_digit(10).expect("invalid digit")))
        .collect::<Vec<_, 100>>()
}

fn solve_1(input: &str) -> usize {
    steps(&mut parse_input(input), 100)
}

fn solve_2(input: &str) -> usize {
    let mut octopuses = parse_input(input);

    let mut count = 1;
    while steps(&mut octopuses, 1) != 100 {
        count += 1;
    }

    count
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
        static ref INPUT: &'static str = r#"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"#;
    }

    #[test]
    fn step_1() {
        let mut octopuses = parse_input(&INPUT);

        assert_eq!(steps(&mut octopuses, 1), 0);

        assert_eq!(
            octopuses,
            parse_input(
                r"6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637"
            )
        );
    }

    #[test]
    fn step_2() {
        let mut octopuses = parse_input(&INPUT);

        assert_eq!(steps(&mut octopuses, 2), 35);

        assert_eq!(
            octopuses,
            parse_input(
                r"8807476555
5089087054
8597889608
8485769600
8700908800
6600088989
6800005943
0000007456
9000000876
8700006848"
            )
        );
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 1656);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 195);
    }
}
