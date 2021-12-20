use std::str::FromStr;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

struct Image {
    image_enhancement_data: Vec<u8>,
    image: Vec<Vec<u8>>,
    external_cell: u8,
}

impl Image {
    fn dim(&self) -> (isize, isize) {
        (self.image[0].len() as isize, self.image.len() as isize)
    }

    fn get(&self, (x, y): (isize, isize)) -> usize {
        let (dimx, dimy) = self.dim();

        let mut value = 0;
        for dy in [-1, 0, 1] {
            let ey = y + dy;
            for dx in [-1, 0, 1] {
                let ex = x + dx;
                if ex < 0 || ex >= dimx || ey < 0 || ey >= dimy {
                    value = value << 1 | self.external_cell as usize;
                } else {
                    value = value << 1 | self.image[ey as usize][ex as usize] as usize;
                }
            }
        }

        value
    }

    #[allow(clippy::unusual_byte_groupings)]
    fn enhances(&mut self) {
        let (dimx, dimy) = self.dim();

        let new_image = (-1..dimy + 1)
            .map(|y| {
                (-1..dimx + 1)
                    .map(|x| self.image_enhancement_data[self.get((x, y))])
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        self.external_cell = if self.external_cell == 1 {
            self.image_enhancement_data[0b111_111_111]
        } else {
            self.image_enhancement_data[0b00_000_000]
        };

        self.image = new_image;
    }

    fn count(&self) -> String {
        if self.external_cell == 1 {
            String::from("inf")
        } else {
            self.image
                .iter()
                .map(|row| row.iter().map(|v| *v as usize).sum::<usize>())
                .sum::<usize>()
                .to_string()
        }
    }
}

impl FromStr for Image {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let parse = |c| match c {
            '.' => Ok(0),
            '#' => Ok(1),
            _ => Err("invalid char"),
        };

        let mut parts = input.split("\n\n");

        let image_enhancement_data = parts
            .next()
            .ok_or("missing image enhancement data")?
            .chars()
            .map(parse)
            .collect::<Result<Vec<_>, _>>()?;

        if image_enhancement_data.len() != 512 {
            return Err("invalid image enhancement data");
        }

        let image = parts
            .next()
            .ok_or("missing image")?
            .lines()
            .map(|line| line.chars().map(parse).collect::<Result<_, _>>())
            .collect::<Result<_, _>>()?;

        Ok(Self {
            image_enhancement_data,
            image,
            external_cell: 0,
        })
    }
}

fn solve(input: &str, n: usize) -> String {
    let mut image = input.parse::<Image>().expect("invalid input");

    for _ in 0..n {
        image.enhances();
    }

    image.count()
}

fn solve_1(input: &str) -> String {
    solve(input, 2)
}

fn solve_2(input: &str) -> String {
    solve(input, 50)
}

pub fn part_1() -> String {
    solve_1(&INPUT)
}

pub fn part_2() -> String {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"#;
    }

    #[test]
    fn count() {
        assert_eq!(
            INPUT.parse::<Image>().expect("invalid input").count(),
            "10".to_string()
        );
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 35.to_string());
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 3351.to_string());
    }
}
