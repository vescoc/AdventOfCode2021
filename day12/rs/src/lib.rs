use std::{
    collections::{HashMap, HashSet, VecDeque},
    str::FromStr,
};

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Cave {
    Start,
    End,
    Big(String),
    Small(String),
}

impl FromStr for Cave {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        use Cave::*;

        Ok(match input {
            "start" => Start,
            "end" => End,
            s => {
                let lowercase_s = s.to_lowercase();
                let s = s.to_string();
                if lowercase_s == s {
                    Small(s)
                } else {
                    Big(s)
                }
            }
        })
    }
}

#[derive(Debug)]
struct Graph {
    #[allow(dead_code)]
    nodes: HashSet<Cave>,
    edges: HashMap<Cave, HashSet<Cave>>,
}

fn parse_input(input: &str) -> Graph {
    let mut nodes: HashMap<String, Cave> = HashMap::new();
    let mut edges = HashMap::new();
    for line in input.lines() {
        let mut parts = line.split('-');

        let start = parts.next().unwrap();
        let end = parts.next().unwrap();

        let start_node = start.parse::<Cave>().unwrap();
        let end_node = end.parse::<Cave>().unwrap();

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
        let mut r: HashSet<Cave> = HashSet::with_capacity(nodes.values().count());
        for node in nodes.into_values() {
            r.insert(node);
        }
        r
    };

    Graph { nodes, edges }
}

fn solve(input: &str, max_visits: usize, max_visits_count: usize) -> usize {
    use Cave::*;

    let graph = parse_input(input);

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
    solve(input, 1, usize::MAX)
}

fn solve_2(input: &str) -> usize {
    solve(input, 2, 2)
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
