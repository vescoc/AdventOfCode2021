use std::fmt;
use std::iter::Peekable;
use std::iter::Sum;
use std::ops::Add;
use std::ops::AddAssign;
use std::str::FromStr;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(PartialEq, Clone)]
enum Number {
    Pair(Box<Number>, Box<Number>),
    Simple(u32),
}

#[derive(Debug)]
enum Reduce {
    Explode(u32, u32),
    ExplodeLeft(u32),
    ExplodeRight(u32),
    ExplodeDone,
}

impl Number {
    fn magnitude(&self) -> u32 {
        use Number::*;

        match self {
            Simple(value) => *value,
            Pair(left, right) => left.magnitude() * 3 + right.magnitude() * 2,
        }
    }

    #[cfg(debug_assertions)]
    fn debug(&self, f: &mut fmt::Formatter, level: usize) -> Result<(), fmt::Error> {
        use Number::*;

        match self {
            Pair(left, right) => {
                let (l, r) = if level >= 4 { ("<", ">") } else { ("[", "]") };

                write!(f, "{}", l)?;
                left.debug(f, level + 1)?;
                write!(f, ",")?;
                right.debug(f, level + 1)?;
                write!(f, "{}", r)
            }
            Simple(value) => {
                if *value >= 10 {
                    write!(f, "_{}_", value)
                } else {
                    write!(f, "{}", value)
                }
            }
        }
    }

    fn simple(&mut self) -> Option<u32> {
        use Number::*;

        match self {
            Simple(value) => Some(*value),
            _ => None,
        }
    }

    fn parse<I: Iterator<Item = char>>(iter: &mut Peekable<I>) -> Result<Self, &'static str> {
        use Number::*;

        match iter.peek() {
            None => Err("eof"),
            Some('[') => {
                iter.next();
                let left = Number::parse(iter)?;
                if let Some(',') = iter.next() {
                    let right = Number::parse(iter)?;
                    if let Some(']') = iter.next() {
                        Ok(Pair(Box::new(left), Box::new(right)))
                    } else {
                        Err("expecting ]")
                    }
                } else {
                    Err("expecting ,")
                }
            }
            Some(c) if *c >= '0' && *c <= '9' => {
                let mut value = iter.next().unwrap().to_digit(10).unwrap();
                while let Some(c) = iter.peek().and_then(|c| c.to_digit(10)) {
                    value = value * 10 + c;
                    iter.next();
                }
                Ok(Simple(value))
            }
            _ => Err("invalid digit"),
        }
    }

    fn reduce(mut self) -> Self {
        loop {
            if self.explode(0).is_some() {
                continue;
            }

            if !self.split() {
                break;
            }
        }

        self
    }

    fn split(&mut self) -> bool {
        use Number::*;

        match self {
            Simple(value) if *value >= 10 => {
                *self = {
                    let left = *value / 2;
                    let right = *value / 2 + *value % 2;
                    Pair(Box::new(Simple(left)), Box::new(Simple(right)))
                };
                true
            }
            Simple(_) => false,
            Pair(left, right) => left.split() || right.split(),
        }
    }

    fn explode(&mut self, level: usize) -> Option<Reduce> {
        use Number::*;
        use Reduce::*;

        match self {
            Simple(_) => None,
            Pair(left, right) if level >= 4 => {
                let left = left.simple().expect("simple");
                let right = right.simple().expect("simple");

                *self = Simple(0);

                Some(Explode(left, right))
            }
            Pair(left, right) => match left.explode(level + 1) {
                Some(Explode(l, r)) => {
                    right.add_left(r);
                    Some(ExplodeLeft(l))
                }
                Some(ExplodeRight(value)) => {
                    right.add_left(value);
                    Some(ExplodeDone)
                }
                Some(e) => Some(e),
                None => match right.explode(level + 1) {
                    Some(Explode(l, r)) => {
                        left.add_right(l);
                        Some(ExplodeRight(r))
                    }
                    Some(ExplodeLeft(value)) => {
                        left.add_right(value);
                        Some(ExplodeDone)
                    }
                    Some(e) => Some(e),
                    None => None,
                },
            },
        }
    }

    fn add_left(&mut self, value: u32) {
        use Number::*;

        match self {
            Simple(v) => *v += value,
            Pair(left, _) => left.add_left(value),
        }
    }

    fn add_right(&mut self, value: u32) {
        use Number::*;

        match self {
            Simple(v) => *v += value,
            Pair(_, right) => right.add_right(value),
        }
    }
}

impl fmt::Debug for Number {
    #[cfg(debug_assertions)]
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.debug(f, 0)
    }

    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use Number::*;

        match self {
            Pair(left, right) => write!(f, "[{:?},{:?}]", left, right),
            Simple(value) => write!(f, "{}", value),
        }
    }
}

impl FromStr for Number {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Number::parse(&mut input.chars().peekable())
    }
}

impl Add<Number> for Number {
    type Output = Number;

    fn add(self, rhs: Number) -> Self::Output {
        use Number::*;

        Pair(Box::new(self), Box::new(rhs)).reduce()
    }
}

impl AddAssign<Number> for Number {
    fn add_assign(&mut self, other: Number) {
        *self = self.to_owned() + other;
    }
}

impl Sum for Number {
    fn sum<I: Iterator<Item = Self>>(mut iter: I) -> Self {
        let mut r = iter.next().unwrap();
        for n in iter {
            r += n;
        }
        r
    }
}

fn solve_1(input: &str) -> u32 {
    input
        .lines()
        .map(|line| line.parse::<Number>().unwrap())
        .sum::<Number>()
        .magnitude()
}

fn solve_2(input: &str) -> u32 {
    let numbers = input
        .lines()
        .map(|line| line.parse::<Number>().unwrap())
        .collect::<Vec<_>>();

    let mut max = u32::MIN;
    for i in 0..numbers.len() - 1 {
        for j in i + 1..numbers.len() {
            max = max.max((numbers[i].to_owned() + numbers[j].to_owned()).magnitude());
            max = max.max((numbers[j].to_owned() + numbers[i].to_owned()).magnitude());
        }
    }

    max
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
        static ref INPUT: &'static str = r#"[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"#;
    }

    #[test]
    fn parse() {
        use Number::*;

        assert_eq!("1".parse(), Ok(Simple(1)));
        assert_eq!(
            "[1,2]".parse(),
            Ok(Pair(Box::new(Simple(1)), Box::new(Simple(2))))
        );
        assert_eq!(
            "[[3,4],5]".parse(),
            Ok(Pair(
                Box::new(Pair(Box::new(Simple(3)), Box::new(Simple(4)))),
                Box::new(Simple(5))
            ))
        );
    }

    #[test]
    fn add() {
        assert_eq!(
            "[1,2]".parse::<Number>().expect("[1,2]") + "[[3,4],5]".parse().expect("[[3,4],5]"),
            "[[1,2],[[3,4],5]]".parse().expect("[[1,2],[[3,4],5]]")
        );
    }

    #[test]
    fn explode() {
        assert_eq!(
            "[[[[[9,8],1],2],3],4]".parse::<Number>().unwrap().reduce(),
            "[[[[0,9],2],3],4]".parse().unwrap(),
            "cannot explode [[[[[9,8],1],2],3],4]",
        );

        assert_eq!(
            "[7,[6,[5,[4,[3,2]]]]]".parse::<Number>().unwrap().reduce(),
            "[7,[6,[5,[7,0]]]]".parse().unwrap(),
            "cannot explode [7,[6,[5,[4,[3,2]]]]]",
        );

        assert_eq!(
            "[[6,[5,[4,[3,2]]]],1]".parse::<Number>().unwrap().reduce(),
            "[[6,[5,[7,0]]],3]".parse().unwrap(),
            "cannot explode [[6,[5,[4,[3,2]]]],1]",
        );
    }

    #[test]
    fn explode_one() {
        {
            let mut number = "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
                .parse::<Number>()
                .unwrap();

            number.explode(0);

            assert_eq!(
                number,
                "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]".parse().unwrap(),
                "cannot explode [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]",
            );
        }

        {
            let mut number = "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
                .parse::<Number>()
                .unwrap();

            number.explode(0);

            assert_eq!(
                number,
                "[[3,[2,[8,0]]],[9,[5,[7,0]]]]".parse().unwrap(),
                "cannot explode [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
            );
        }
    }

    #[test]
    fn split() {
        assert_eq!(
            "10".parse::<Number>().unwrap().reduce(),
            "[5,5]".parse().unwrap()
        );
    }

    #[test]
    fn add_reduce() {
        assert_eq!(
            "[[[[4,3],4],4],[7,[[8,4],9]]]".parse::<Number>().unwrap() + "[1,1]".parse().unwrap(),
            "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]".parse().unwrap(),
        );
    }

    #[test]
    fn add_steps() {
        {
            let mut number = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
                .parse::<Number>()
                .unwrap();

            number.explode(0);

            assert_eq!(number, "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]".parse().unwrap(),)
        }

        {
            let mut number = "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"
                .parse::<Number>()
                .unwrap();

            number.explode(0);

            assert_eq!(number, "[[[[0,7],4],[15,[0,13]]],[1,1]]".parse().unwrap(),)
        }
    }

    #[test]
    fn simple_add_1() {
        assert_eq!(
            r"[1,1]
[2,2]
[3,3]
[4,4]"
                .lines()
                .map(|line| line.parse::<Number>().unwrap())
                .sum::<Number>(),
            "[[[[1,1],[2,2]],[3,3]],[4,4]]".parse().unwrap(),
        );
    }

    #[test]
    fn simple_add_2() {
        assert_eq!(
            r"[1,1]
[2,2]
[3,3]
[4,4]
[5,5]"
                .lines()
                .map(|line| line.parse::<Number>().unwrap())
                .sum::<Number>(),
            "[[[[3,0],[5,3]],[4,4]],[5,5]]".parse().unwrap(),
        );
    }

    #[test]
    fn simple_add_3() {
        assert_eq!(
            r"[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]"
                .lines()
                .map(|line| line.parse::<Number>().unwrap())
                .sum::<Number>(),
            "[[[[5,0],[7,4]],[5,5]],[6,6]]".parse().unwrap(),
        );
    }

    #[test]
    fn complex_add() {
        assert_eq!(
            r"[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]"
                .lines()
                .map(|line| line.parse::<Number>().unwrap())
                .sum::<Number>(),
            "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
                .parse()
                .unwrap(),
        )
    }

    #[test]
    fn complex_add_1() {
        assert_eq!(
            r"[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                .lines()
                .map(|line| line.parse::<Number>().unwrap())
                .sum::<Number>(),
            "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
                .parse()
                .unwrap(),
        )
    }

    #[test]
    fn magnitude() {
        assert_eq!(
            "[[1,2],[[3,4],5]]".parse::<Number>().unwrap().magnitude(),
            143,
        );
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 4140);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 3993);
    }
}
