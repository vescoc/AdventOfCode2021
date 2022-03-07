use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

fn solve_1(input: &str) -> usize {
    let mut map = input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    '.' => None,
                    '>' | 'v' => Some((c, 0)),
                    _ => panic!("invalid sea cucumber: '{}'", c),
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut step = 1;
    loop {
        let mut moved = 0;

        // east sea cucumbers move
        for row in map.iter_mut() {
            for x in 0..row.len() {
                let mut sea_cucumber = row[x].take();
                match sea_cucumber {
                    Some(('>', s)) if s < step => {
                        let new_x = (x + 1) % row.len();
                        if match row[new_x] {
                            None => true,
                            Some((c, s)) if (c == 'G') || (c == 'g' && s < step) => true,
                            _ => false,
                        } {
                            moved += 1;
                            sea_cucumber.as_mut().unwrap().1 = step;
                            row[new_x] = sea_cucumber;
                            row[x] = Some(('g', step));
                        } else {
                            row[x] = sea_cucumber;
                        }
                    }
                    Some((c, s)) if (c == 'g' || c == 'G') && s < step => {}
                    _ => row[x] = sea_cucumber,
                }
            }
        }

        // south sea cucumbers move
        for y in 0..map.len() {
            for x in 0..map[y].len() {
                let mut sea_cucumber = map[y][x].take();
                match sea_cucumber {
                    Some(('v', s)) if s < step => {
                        let new_y = (y + 1) % map.len();
                        if match map[new_y][x] {
                            None => true,
                            Some((c, s)) if c == 'g' || (c == 'G' && s < step) => true,
                            _ => false,
                        } {
                            moved += 1;
                            sea_cucumber.as_mut().unwrap().1 = step;
                            map[new_y][x] = sea_cucumber;
                            map[y][x] = Some(('G', step));
                        } else {
                            map[y][x] = sea_cucumber;
                        }
                    }
                    Some((c, s)) if (c == 'g' || c == 'G') && s < step => {}
                    _ => map[y][x] = sea_cucumber,
                }
            }
        }

        if moved == 0 {
            break;
        }

        step += 1;
    }

    step
}

pub fn part_1() -> usize {
    solve_1(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 58);
    }
}
