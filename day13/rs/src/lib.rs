use std::collections::HashSet;
use std::str::FromStr;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(Debug)]
enum FoldInstruction {
    X(usize),
    Y(usize),
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
struct Dot(usize, usize);

fn invalid_number<E>(_: E) -> &'static str {
    "invalid number"
}

impl FromStr for FoldInstruction {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        use FoldInstruction::*;

        match input {
            _ if input.starts_with("fold along x=") => Ok(X(input["fold along x=".len()..]
                .parse()
                .map_err(invalid_number)?)),
            _ if input.starts_with("fold along y=") => Ok(Y(input["fold along y=".len()..]
                .parse()
                .map_err(invalid_number)?)),
            _ => Err("invalid line"),
        }
    }
}

impl FromStr for Dot {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut parts = input.split(',');

        let mut next_number = || {
            parts
                .next()
                .ok_or("no number")
                .and_then(|part| part.parse().map_err(invalid_number))
        };

        Ok(Dot(next_number()?, next_number()?))
    }
}

fn parse_input(input: &str) -> (Dot, HashSet<Dot>, Vec<FoldInstruction>) {
    let mut dim = Dot(0, 0);
    let mut paper = HashSet::new();
    let mut fold_instructions = vec![];
    for line in input.lines() {
        if let Ok(p) = line.parse::<Dot>() {
            dim = Dot(dim.0.max(p.0), dim.1.max(p.1));
            paper.insert(p);
        } else if let Ok(fold) = line.parse::<FoldInstruction>() {
            fold_instructions.push(fold);
        }
    }

    (dim, paper, fold_instructions)
}

fn solve(dim: &mut Dot, paper: &mut HashSet<Dot>, fold_instructions: &[FoldInstruction]) {
    use FoldInstruction::*;

    for fold_instruction in fold_instructions {
        match fold_instruction {
            X(line) => {
                let mut new_paper = HashSet::new();
                for d in paper.drain() {
                    match d {
                        Dot(x, _) if x < *line => {
                            new_paper.insert(d);
                        }
                        Dot(x, y) => {
                            new_paper.insert(Dot(2 * line - x, y));
                        }
                    }
                }

                *paper = new_paper;
                *dim = Dot(*line, dim.1)
            }
            Y(line) => {
                let mut new_paper = HashSet::new();
                for d in paper.drain() {
                    match d {
                        Dot(_, y) if y < *line => {
                            new_paper.insert(d);
                        }
                        Dot(x, y) => {
                            new_paper.insert(Dot(x, 2 * line - y));
                        }
                    }
                }

                *paper = new_paper;
                *dim = Dot(dim.0, *line)
            }
        }
    }
}

fn solve_1(input: &str) -> usize {
    let (mut dim, mut paper, fold_instructions) = parse_input(input);

    solve(&mut dim, &mut paper, &fold_instructions[0..1]);

    paper.len()
}

fn solve_2(input: &str) -> String {
    let (mut dim, mut paper, fold_instructions) = parse_input(input);

    solve(&mut dim, &mut paper, &fold_instructions);

    let mut s = String::with_capacity((dim.0 + 1) * dim.1);
    for y in 0..dim.1 {
        for x in 0..dim.0 {
            s.push(if paper.contains(&Dot(x, y)) { '#' } else { '.' });
        }
        s.push('\n')
    }

    s.pop();

    s
}

pub fn part_1() -> usize {
    solve_1(&INPUT)
}

pub fn part_2() -> String {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"#;
    }

    #[test]
    fn test_parse_input() {
        let (dim, paper, fold_instructions) = parse_input(&INPUT);

        assert_eq!(dim, Dot(10, 14));
        assert_eq!(paper.len(), 18);
        assert_eq!(fold_instructions.len(), 2);
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 17);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(
            solve_2(&INPUT),
            r#"#####
#...#
#...#
#...#
#####
.....
....."#
        );
    }
}
