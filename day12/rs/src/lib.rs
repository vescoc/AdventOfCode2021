use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct Value(u32);

impl FromStr for Value {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(Value(
            input.chars().fold(0, |acc, c| acc << 8 | u32::from(c)),
        ))
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Cave<T> {
    Start,
    End,
    Big(T),
    Small(T),
}

impl<T> Cave<T>
where
    T: FromStr,
    T::Err: Debug,
{
    fn new(input: &str) -> Self {
        use Cave::*;

        match input {
            "start" => Start,
            "end" => End,
            s => {
                let lowercase_s = s.to_lowercase();
                if lowercase_s == s {
                    Small(T::from_str(s).unwrap())
                } else {
                    Big(T::from_str(s).unwrap())
                }
            }
        }
    }
}

#[derive(Debug)]
struct Graph<T> {
    #[allow(dead_code)]
    nodes: HashSet<Cave<T>>,
    edges: HashMap<Cave<T>, HashSet<Cave<T>>>,
}

fn parse_input<T>(input: &str) -> Graph<T>
where
    T: Clone + FromStr + Eq + Hash,
    T::Err: Debug,
{
    let mut nodes: HashMap<String, Cave<T>> = HashMap::new();
    let mut edges = HashMap::new();
    for line in input.lines() {
        let mut parts = line.split('-');

        let start = parts.next().unwrap();
        let end = parts.next().unwrap();

        let start_node = Cave::new(start);
        let end_node = Cave::new(end);

        nodes
            .entry(start.to_string())
            .or_insert_with(|| start_node.to_owned());
        nodes
            .entry(end.to_string())
            .or_insert_with(|| end_node.to_owned());

        edges
            .entry(start_node.to_owned())
            .or_insert_with(HashSet::new)
            .insert(end_node.to_owned());
        edges
            .entry(end_node.to_owned())
            .or_insert_with(HashSet::new)
            .insert(start_node.to_owned());
    }

    let nodes = {
        let mut r: HashSet<Cave<T>> = HashSet::with_capacity(nodes.values().count());
        for node in nodes.into_values() {
            r.insert(node);
        }
        r
    };

    Graph { nodes, edges }
}

fn solve<T>(input: &str, max_visits: usize, max_visits_count: usize) -> usize
where
    T: Clone + FromStr + Eq + Hash,
    T::Err: Debug,
{
    use Cave::*;

    let graph: Graph<T> = parse_input(input);

    let mut count = 0;

    let mut paths = VecDeque::new();

    paths.push_front((Start, HashMap::new(), 0));

    while let Some((cave, mut visited, mut current_max_visits)) = paths.pop_front() {
        match cave {
            End => count += 1,
            Start => {
                for node in graph.edges[&cave].iter() {
                    paths.push_back((node.to_owned(), visited.to_owned(), current_max_visits));
                }
            }
            Big(_) => {
                for node in graph.edges[&cave].iter() {
                    match node {
                        Big(_) | End => paths.push_back((
                            node.to_owned(),
                            visited.to_owned(),
                            current_max_visits,
                        )),
                        Small(_) => {
                            let visits = visited.get(node).copied().unwrap_or(0);
                            if visits + 1 < max_visits
                                || visits + 1 == max_visits
                                    && current_max_visits + 1 < max_visits_count
                            {
                                paths.push_back((
                                    node.to_owned(),
                                    visited.to_owned(),
                                    current_max_visits,
                                ));
                            }
                        }
                        _ => {}
                    }
                }
            }
            Small(_) => {
                let c = visited.entry(cave.to_owned()).or_insert(0);
                *c += 1;
                if *c == max_visits {
                    current_max_visits += 1;
                }

                for node in graph.edges[&cave].iter() {
                    match node {
                        Big(_) | End => paths.push_back((
                            node.to_owned(),
                            visited.to_owned(),
                            current_max_visits,
                        )),
                        Small(_) => {
                            let visits = visited.get(node).copied().unwrap_or(0);
                            if visits + 1 < max_visits
                                || visits + 1 == max_visits
                                    && current_max_visits + 1 < max_visits_count
                            {
                                paths.push_back((
                                    node.to_owned(),
                                    visited.to_owned(),
                                    current_max_visits,
                                ));
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    count
}

fn solve_1(input: &str) -> usize {
    solve::<Value>(input, 1, usize::MAX)
}

fn solve_2(input: &str) -> usize {
    solve::<Value>(input, 2, 2)
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
        static ref EXAMPLE_1: &'static str = r#"start-A
start-b
A-c
A-b
b-d
A-end
b-end"#;
        static ref EXAMPLE_2: &'static str = r#"dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"#;
        static ref EXAMPLE_3: &'static str = r#"fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"#;
    }

    #[test]
    fn test_parse_input() {
        let graph: Graph<Value> = parse_input(&EXAMPLE_1);

        assert_eq!(graph.nodes.len(), 6);
    }

    #[test]
    fn same_results_1_1() {
        assert_eq!(solve_1(&EXAMPLE_1), 10);
    }

    #[test]
    fn same_results_1_2() {
        assert_eq!(solve_1(&EXAMPLE_2), 19);
    }

    #[test]
    fn same_results_1_3() {
        assert_eq!(solve_1(&EXAMPLE_3), 226);
    }

    #[test]
    fn same_results_2_1() {
        assert_eq!(solve_2(&EXAMPLE_1), 36);
    }

    #[test]
    fn same_results_2_2() {
        assert_eq!(solve_2(&EXAMPLE_2), 103);
    }

    #[test]
    fn same_results_2_3() {
        assert_eq!(solve_2(&EXAMPLE_3), 3509);
    }
}
