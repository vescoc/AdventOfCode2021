use std::{
    str::FromStr,
};

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: Vec<Command> = parse_input(include_str!("../../input")).unwrap();
}

#[derive(PartialEq,Debug)]
enum Command {
    Forward(u32),
    Down(u32),
    Up(u32),
}

impl FromStr for Command {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        use Command::*;
        
        let mut tokens = input.split_whitespace();

        let invalid_number = |_| "invalid number";
        
        match (tokens.next(), tokens.next()) {
            (Some("forward"), Some(n)) => Ok(Forward(n.parse().map_err(invalid_number)?)),
            (Some("down"), Some(n)) => Ok(Down(n.parse().map_err(invalid_number)?)),
            (Some("up"), Some(n)) => Ok(Up(n.parse().map_err(invalid_number)?)),
            _ => Err("invalid input"),
        }
    }
}

trait Exec {
    fn exec(self, command: &Command) -> Self;

    fn result(self) -> u32;
}

#[derive(Default)]
struct Move(u32, u32);

impl Exec for Move {
    fn exec(mut self, command: &Command) -> Self {
        use Command::*;
        
        match command {
            Forward(units) => self.0 += units,
            Down(units) => self.1 += units,
            Up(units) => self.1 -= units,
        }

        self
    }

    fn result(self) -> u32 {
        self.0 * self.1             
    }
}

#[derive(Default)]
struct MoveWithAim(u32, u32, u32);

impl Exec for MoveWithAim {
    fn exec(mut self, command: &Command) -> Self {
        use Command::*;
        
        match command {
            Forward(units) => {
                self.0 += units;
                self.1 += self.2 * units;
            }
            Down(units) => self.2 += units,
            Up(units) => self.2 -= units,
        }

        self
    }

    fn result(self) -> u32 {
        self.0 * self.1             
    }
}

fn parse_input(input: &str) -> Result<Vec<Command>, &'static str> {
    input.lines().map(|l| l.parse()).collect()
}

fn solve<T>(input: &[Command]) -> u32
where T: Exec + Default
{
    input
        .iter()
        .fold(T::default(), |m, command| {
            m.exec(command)
        })
        .result()
}

fn solve_1(input: &[Command]) -> u32 {
    solve::<Move>(input)
}

fn solve_2(input: &[Command]) -> u32 {
    solve::<MoveWithAim>(input)
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
        static ref INPUT_STR: &'static str = r#"forward 5
down 5
forward 8
up 3
down 8
forward 2"#;
        static ref INPUT: Vec<Command> = parse_input(&INPUT_STR).unwrap();
    }

    #[test]
    fn check_parse_input() {
        use Command::*;
        
        assert_eq!(
            parse_input(&INPUT_STR).unwrap(),
            [Forward(5), Down(5), Forward(8), Up(3), Down(8), Forward(2)]
        );
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 150);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 900);
    }
}
