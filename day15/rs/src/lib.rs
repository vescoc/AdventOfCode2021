use std::cmp::Ordering;
use std::collections::HashSet;
use std::ops::{Index, IndexMut};

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(Debug)]
struct Node {
    risk_level: u32,
    total_risk: u32,
    previous: Option<(usize, usize)>,
}

struct Chitons {
    chitons: Vec<Vec<Node>>,
    dim: (usize, usize),
}

impl Chitons {
    fn new(tile: &[Vec<u32>], repeat: usize) -> Self {
        assert!(repeat != 0);

        let tile_dim = (tile[0].len(), tile.len());
        let dim = (tile_dim.0 * repeat, tile_dim.1 * repeat);

        let chitons = (0..dim.1)
            .map(|y| {
                (0..dim.0)
                    .map(|x| {
                        let (dimx, dimy) = tile_dim;

                        let ((x, tx), (y, ty)) =
                            ((x % dimx, (x / dimx) as u32), (y % dimy, (y / dimy) as u32));

                        let risk_level = tile[y][x];

                        Node {
                            risk_level: (risk_level + tx + ty - 1) % 9 + 1,
                            total_risk: u32::MAX,
                            previous: None,
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        Self {
            dim: (tile[0].len() * repeat, tile.len() * repeat),
            chitons: chitons,
        }
    }

    fn dim(&self) -> (usize, usize) {
        self.dim
    }
}

impl Index<&(usize, usize)> for Chitons {
    type Output = Node;

    fn index(&self, (x, y): &(usize, usize)) -> &Self::Output {
        &self.chitons[*y][*x]
    }
}

impl IndexMut<&(usize, usize)> for Chitons {
    fn index_mut(&mut self, (x, y): &(usize, usize)) -> &mut Self::Output {
        &mut self.chitons[*y][*x]
    }
}

fn parse_input(input: &str) -> Result<Vec<Vec<u32>>, &'static str> {
    input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).ok_or("invalid digit"))
                .collect::<Result<Vec<_>, &'static str>>()
        })
        .collect::<Result<Vec<_>, _>>()
}

fn solve(input: &str, repeat: usize) -> u32 {
    let mut chitons = Chitons::new(&parse_input(input).unwrap(), repeat);

    let (dimx, dimy) = chitons.dim();

    let mut t = HashSet::new();
    {
        let zero_zero = &mut chitons[&(0, 0)];
        zero_zero.total_risk = 0;

        let zero_one = &mut chitons[&(1, 0)];
        zero_one.previous = Some((0, 0));
        zero_one.total_risk = zero_one.risk_level;

        let one_zero = &mut chitons[&(0, 1)];
        one_zero.previous = Some((0, 0));
        one_zero.total_risk = one_zero.risk_level;

        t.insert((0, 1));
        t.insert((1, 0));
    }

    while let Some((x, y)) = t
        .iter()
        .min_by_key(|(x, y)| chitons[&(*x, *y)].total_risk)
        .copied()
    {
        t.remove(&(x, y));

        if x == dimx - 1 && y == dimy - 1 {
            break;
        }

        let total_risk = chitons[&(x, y)].total_risk;

        for (dx, dy) in [(-1, 0), (0, 1), (0, -1), (1, 0)] {
            let p = match dx.cmp(&0) {
                Ordering::Equal => Some(x),
                Ordering::Less if x > 0 => Some(x - 1),
                Ordering::Greater if x < dimx - 1 => Some(x + 1),
                _ => None,
            }
            .and_then(|x| match dy.cmp(&0) {
                Ordering::Equal => Some((x, y)),
                Ordering::Less if y > 0 => Some((x, y - 1)),
                Ordering::Greater if y < dimy - 1 => Some((x, y + 1)),
                _ => None,
            });

            if let Some((nx, ny)) = p {
                let node = &mut chitons[&(nx, ny)];

                let n_total_risk = total_risk + node.risk_level;
                if node.total_risk == u32::MAX {
                    t.insert((nx, ny));

                    node.total_risk = n_total_risk;
                    node.previous = Some((x, y));
                } else if t.contains(&(nx, ny)) && node.total_risk > n_total_risk {
                    node.total_risk = n_total_risk;
                    node.previous = Some((x, y));
                }
            }
        }
    }

    chitons[&(dimx - 1, dimy - 1)].total_risk
}

fn solve_1(input: &str) -> u32 {
    solve(input, 1)
}

fn solve_2(input: &str) -> u32 {
    solve(input, 5)
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
        static ref INPUT: &'static str = r#"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"#;
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 40);
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 315);
    }

    #[test]
    fn test_index() {
        let chitons = Chitons::new(&parse_input(&INPUT).unwrap(), 5);

        let c1 = &chitons[&(0, 0)];
        let c2 = &chitons[&(0, 1)];

        assert_eq!(c1.total_risk, c2.total_risk);
    }
}
