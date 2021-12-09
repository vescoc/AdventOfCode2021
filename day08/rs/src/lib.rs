use std::str::FromStr;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

struct LineValues {
    digits: [u8; 10],
    display_digits: [u8; 4],
}

impl FromStr for LineValues {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        fn parse_digit(input: &str) -> u8 {
            input
                .chars()
                .map(|c| ((c as u32) - ('a' as u32)) as u8)
                .fold(0, |mut acc, c| {
                    acc |= 1 << c;
                    acc
                })
        }

        let mut parts = input.split(" | ");

        let mut digits = [0; 10];
        for (i, part) in parts
            .next()
            .ok_or("invalid format")?
            .split_ascii_whitespace()
            .enumerate()
        {
            digits[i] = parse_digit(part);
        }

        let mut display_digits = [0; 4];
        for (i, part) in parts
            .next()
            .ok_or("invalid format")?
            .split_ascii_whitespace()
            .enumerate()
        {
            display_digits[i] = parse_digit(part);
        }

        Ok(LineValues {
            digits,
            display_digits,
        })
    }
}

impl LineValues {
    fn solve(&self) -> usize {
        fn find<'a, F>(
            target: usize,
            digits: &'a mut [u8; 10],
            unmapped_digits: &'a mut [u8; 10],
            mut f: F,
        ) -> Option<()>
        where
            F: FnMut(u8) -> bool,
        {
            let (index, digit) = unmapped_digits
                .iter()
                .copied()
                .enumerate()
                .find(|(_, digit)| digit & (1 << 7) == 0 && f(*digit))?;

            digits[target] = digit;
            unmapped_digits[index] |= 1 << 7;

            Some(())
        }

        let mut unmapped_digits = self.digits.to_owned();
        let mut digits = [0; 10];

        find(1, &mut digits, &mut unmapped_digits, |digit| {
            digit.count_ones() == 2
        })
            .expect("cannot find 1");
        
        find(4, &mut digits, &mut unmapped_digits, |digit| {
            digit.count_ones() == 4
        })
            .expect("cannot find 4");
        
        find(7, &mut digits, &mut unmapped_digits, |digit| {
            digit.count_ones() == 3
        })
            .expect("cannot find 7");
        
        find(8, &mut digits, &mut unmapped_digits, |digit| {
            digit.count_ones() == 7
        })
            .expect("cannot find 8");

        let a = digits[8] & digits[7] & !digits[1];
        let cf = digits[1];
        let bd = digits[8] & digits[4] & !digits[1];
        let eg = digits[8] & !(digits[7] | digits[4]);

        find(9, &mut digits, &mut unmapped_digits, |digit| {
            (bd | cf | a) & !digit == 0 && (digit & !(bd | cf | a)).count_ones() == 1
        })
            .expect("cannot find 9");

        let g = digits[9] & !(bd | cf | a);
        let e = eg & !g;

        find(0, &mut digits, &mut unmapped_digits, |digit| {
            (a | cf | eg) & !digit == 0 && (digit & !(a | cf | eg)).count_ones() == 1
        })
            .expect("cannot find 0");

        let b = digits[0] & !(a | cf | eg);
        let d = bd & !b;

        find(6, &mut digits, &mut unmapped_digits, |digit| {
            (a | b | e | g) & !digit == 0 && (digit & !(a | b | e | g)).count_ones() == 2
        })
            .expect("cannot find 6");

        let df = digits[6] & !(a | b | e | g);
        let c = cf & !df;
        let f = cf & df;

        digits[2] = a | c | d | e | g;
        digits[3] = a | c | d | f | g;
        digits[5] = a | b | d | f | g;

        self.display_digits.iter().fold(0, |acc, digit| {
            acc * 10
                + digits
                    .iter()
                    .enumerate()
                    .find_map(|(i, candidate)| if digit == candidate { Some(i) } else { None })
                    .unwrap()
        })
    }
}

fn solve_1(input: &str) -> usize {
    input
        .lines()
        .map(|l| {
            l.split(" | ")
                .skip(1)
                .map(|p| {
                    p.split_ascii_whitespace()
                        .map(|n| n.len())
                        .filter(|&n| n == 2 || n == 3 || n == 4 || n == 7)
                        .count()
                })
                .sum::<usize>()
        })
        .sum()
}

fn solve_2(input: &str) -> usize {
    input
        .lines()
        .map(|l| l.parse::<LineValues>().expect("invalid input"))
        .map(|l| l.solve())
        .sum()
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
        static ref INPUT: &'static str = r#"be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 26);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 61229);
    }
}
