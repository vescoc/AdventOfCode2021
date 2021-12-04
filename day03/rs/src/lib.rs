#![no_std]

use lazy_static::lazy_static;
use heapless::Vec;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

fn solve_1(input: &str) -> u32 {
    let l = input.lines().next().unwrap().len();
    let mut v = [0; 12];

    for line in input.lines() {
        for (i, b) in v.iter_mut().enumerate().take(l) {
            if char::from(line.as_bytes()[i]) == '0' {
                *b -= 1;
            } else {
                *b += 1;
            }
        }
    }

    let (mut gamma_rate, mut epsilon_rate) = (0, 0);
    for &b in v.iter().take(l) {
        let d = if b > 0 { 1 } else { 0 };

        gamma_rate = (gamma_rate << 1) | d;
        epsilon_rate = (epsilon_rate << 1) | (1 - d);
    }

    gamma_rate * epsilon_rate
}

fn reduce<const N: usize>(mut input: Vec<&str, N>, bit_criteria: bool, mut bit_index: usize) -> u32 {
    while input.len() > 1 {
        let mut v = 0;
        for number in &input {
            if u16::from(number.as_bytes()[bit_index]) == '0' as u16 {
                v -= 1;
            } else {
                v += 1;
            }
        }

        let check_value = if bit_criteria {
            if v >= 0 {
                '1'
            } else {
                '0'
            }
        } else if v >= 0 {
            '0'
        } else {
            '1'
        };

        for i in (0..input.len()).rev() {
            if char::from(input[i].as_bytes()[bit_index]) != check_value {
                input.swap_remove(i);
            }
        }

        bit_index += 1;
    }
    
    let mut r = 0;
    for d in input[0].chars() {
        r = (r << 1) | if d == '0' { 0 } else { 1 };
    }

    r
}

fn solve_2(input: &str) -> u32 {
    let input = input.lines().collect::<Vec<&str, 1000>>();

    let oxygen_generator_rating = reduce(input.clone(), true, 0);
    let co2_scrubber_rating = reduce(input, false, 0);

    oxygen_generator_rating * co2_scrubber_rating
}

pub fn part_1() -> u32 {
    solve_1(&INPUT)
}

pub fn part_2() -> u32 {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"00100
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
01010"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 198);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 230);
    }
}
