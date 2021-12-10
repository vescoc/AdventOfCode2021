use lazy_static::lazy_static;

enum Error {
    InvalidChar(char),
    Incomplete(Vec<char>),
}

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

fn parse(input: &str) -> Result<(), Error> {
    let mut stack = vec![];
    for c in input.chars() {
        match (c, stack.pop()) {
            (')', Some('(')) => {}
            (')', _) => return Err(Error::InvalidChar(')')),
            (']', Some('[')) => {}
            (']', _) => return Err(Error::InvalidChar(']')),
            ('}', Some('{')) => {}
            ('}', _) => return Err(Error::InvalidChar('}')),
            ('>', Some('<')) => {}
            ('>', _) => return Err(Error::InvalidChar('>')),
            (c, None) => stack.push(c),
            (c, Some(t)) => {
                stack.push(t);
                stack.push(c);
            }
        }
    }

    if stack.is_empty() {
        Ok(())
    } else {
        Err(Error::Incomplete(stack))
    }
}

fn solve_1(input: &str) -> u32 {
    input
        .lines()
        .filter_map(|line| {
            if let Err(Error::InvalidChar(c)) = parse(line) {
                Some(c)
            } else {
                None
            }
        })
        .map(|c| match c {
            ')' => 3,
            ']' => 57,
            '}' => 1197,
            '>' => 25137,
            c => panic!("invalid char {}", c),
        })
        .sum()
}

fn solve_2(input: &str) -> u64 {
    let mut scores = input
        .lines()
        .filter_map(|line| {
            if let Err(Error::Incomplete(stack)) = parse(line) {
                Some(stack)
            } else {
                None
            }
        })
        .map(|mut stack| {
            let mut score = 0;
            while let Some(top) = stack.pop() {
                score = score * 5
                    + match top {
                        '(' => 1,
                        '[' => 2,
                        '{' => 3,
                        '<' => 4,
                        c => panic!("invalid char {}", c),
                    }
            }
            score
        })
        .collect::<Vec<_>>();

    scores.sort_unstable();

    scores[scores.len() / 2]
}

pub fn part_1() -> u32 {
    solve_1(&INPUT)
}

pub fn part_2() -> u64 {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 26397);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 288957);
    }
}
