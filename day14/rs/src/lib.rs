use std::collections::HashMap;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

fn parse_input(input: &str) -> (Vec<char>, HashMap<(char, char), char>) {
    let mut parts = input.lines();

    let polymer = parts
        .next()
        .expect("polymer not found")
        .chars()
        .collect::<Vec<_>>();

    parts.next();

    let rules = parts
        .map(|line| {
            let mut parts = line.split(" -> ");

            let p1 = parts.next().expect("pair not found");
            let p2 = parts.next().expect("target not found");

            let mut p1 = p1.chars();
            (
                (
                    p1.next().expect("cannot find first"),
                    p1.next().expect("cannot find second"),
                ),
                p2.chars().next().expect("cannot find target"),
            )
        })
        .collect::<HashMap<_, _>>();

    (polymer, rules)
}

fn solve(input: &str, steps: usize) -> u128 {
    let (polymer, rules) = parse_input(input);

    let mut pairs = polymer.windows(2).fold(
        HashMap::<(char, char), u128>::with_capacity(rules.len()),
        |mut acc, w| {
            let c1 = w[0];
            let c2 = w[1];

            *acc.entry((c1, c2)).or_default() += 1;
            acc
        },
    );

    for _i in 0..steps {
        for ((c1, c3), counts) in pairs.to_owned().drain() {
            let c2 = rules[&(c1, c3)];
            *pairs.entry((c1, c2)).or_default() += counts;
            *pairs.entry((c2, c3)).or_default() += counts;
            *pairs.entry((c1, c3)).or_default() -= counts;
        }
    }

    let mut components = pairs
        .into_iter()
        .fold(
            HashMap::<char, u128>::new(),
            |mut acc, ((c1, _c2), counts)| {
                *acc.entry(c1).or_default() += counts;
                acc
            },
        )
        .drain()
        .collect::<Vec<_>>();

    components.sort_by_key(|(_, count)| *count);

    components[components.len() - 1].1 - components[0].1 + 1
}

fn solve_1(input: &str) -> u128 {
    solve(input, 10)
}

fn solve_2(input: &str) -> u128 {
    solve(input, 40)
}

pub fn part_1() -> u128 {
    solve_1(&INPUT)
}

pub fn part_2() -> u128 {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"#;
    }

    #[test]
    fn test_parse_input() {
        let (polymer, rules) = parse_input(&INPUT);

        assert_eq!(polymer, vec!['N', 'N', 'C', 'B']);
        assert_eq!(rules[&('C', 'H')], 'B');
        assert_eq!(rules[&('C', 'N')], 'C');
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 1588);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 2188189693529);
    }
}
