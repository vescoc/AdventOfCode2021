use std::collections::{HashMap, VecDeque};

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

type Point = (usize, usize);

fn parse_input(input: &str) -> Vec<Vec<u32>> {
    input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn low_points<'a>(heightmap: &'a [Vec<u32>]) -> impl Iterator<Item = (&'a u32, Point)> {
    heightmap.iter().enumerate().flat_map(move |(y, ys)| {
        ys.iter().enumerate().filter_map(move |(x, v)| {
            let is_min = {
                let values: [u32; 4] = [
                    if x > 0 { heightmap[y][x - 1] } else { 10 },
                    if y > 0 { heightmap[y - 1][x] } else { 10 },
                    *heightmap[y].get(x + 1).unwrap_or(&10),
                    heightmap.get(y + 1).map(|r| r[x]).unwrap_or(10),
                ];

                values.iter().min().unwrap() > v
            };

            if is_min {
                Some((v, (x, y)))
            } else {
                None
            }
        })
    })
}

fn solve_1(input: &str) -> u32 {
    low_points(&parse_input(input)).fold(0, |acc, (v, _)| acc + v + 1)
}

fn solve_2(input: &str) -> usize {
    let heightmap = parse_input(input);

    let low_points = low_points(&heightmap).map(|(_, p)| p).collect::<Vec<_>>();

    let mut basins: Vec<Vec<Option<Point>>> =
        vec![vec![None; heightmap.first().unwrap().len()]; heightmap.len()];

    let (mut queue, mut dimensions) = {
        let mut queue = VecDeque::with_capacity(low_points.len());
        let mut dimensions = HashMap::with_capacity(low_points.len());

        for (x, y) in low_points.iter().copied() {
            basins[y][x] = Some((x, y));
            queue.push_back((x, y));
            dimensions.insert((x, y), 1);
        }
        (queue, dimensions)
    };

    while let Some((x, y)) = queue.pop_front() {
        let current = basins[y][x].unwrap();

        let mut push_if = |v: &mut Option<Point>, x: usize, y: usize| {
            if v.is_none() && heightmap[y][x] != 9 {
                v.replace(current);
                queue.push_back((x, y));
                *dimensions.get_mut(&current).unwrap() += 1;
            }
        };

        if x > 0 {
            if let Some(v) = basins[y].get_mut(x - 1) {
                push_if(v, x - 1, y);
            }
        }

        if y > 0 {
            if let Some(row) = basins.get_mut(y - 1) {
                if let Some(v) = row.get_mut(x) {
                    push_if(v, x, y - 1);
                }
            }
        }

        if let Some(v) = basins[y].get_mut(x + 1) {
            push_if(v, x + 1, y);
        }

        if let Some(row) = basins.get_mut(y + 1) {
            if let Some(v) = row.get_mut(x) {
                push_if(v, x, y + 1);
            }
        }
    }

    let mut dimensions = dimensions.values().collect::<Vec<_>>();
    dimensions.sort_by(|a, b| b.partial_cmp(a).unwrap());

    dimensions.into_iter().take(3).product()
}

pub fn part_1() -> u32 {
    solve_1(&INPUT)
}

pub fn part_2() -> usize {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"2199943210
3987894921
9856789892
8767896789
9899965678"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 15);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 1134);
    }
}
