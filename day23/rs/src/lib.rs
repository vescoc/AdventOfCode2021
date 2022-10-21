use lazy_static::lazy_static;

use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::fmt::{self, Write};
use std::hash::{Hash, Hasher};
use std::iter;
use std::ops::Index;
use std::str::FromStr;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum Ambhipod {
    A,
    B,
    C,
    D,
}

impl TryFrom<usize> for Ambhipod {
    type Error = &'static str;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Ambhipod::A),
            1 => Ok(Ambhipod::B),
            2 => Ok(Ambhipod::C),
            3 => Ok(Ambhipod::D),
            _ => Err("invalid value"),
        }
    }
}

impl Ambhipod {
    const fn cost(&self) -> usize {
        match self {
            Ambhipod::A => 1,
            Ambhipod::B => 10,
            Ambhipod::C => 100,
            Ambhipod::D => 1000,
        }
    }
}

struct AmbhipodsIter<'a, const N: usize> {
    ambhipods: &'a Ambhipods<N>,
    current: usize,
}

impl<'a, const N: usize> Iterator for AmbhipodsIter<'a, N> {
    type Item = (&'a Coord, Ambhipod);

    fn next(&mut self) -> Option<Self::Item> {
        let a = self.current / Ambhipods::<N>::room_size();
        let result = self
            .ambhipods
            .data
            .get(self.current)
            .map(|c| (c, Ambhipod::try_from(a).unwrap()));

        self.current = self.ambhipods.find(self.current + 1);

        result
    }
}

#[derive(Debug, Clone)]
struct Ambhipods<const N: usize> {
    data: [Coord; N],
    count: [usize; 4],
}

impl<const N: usize> Hash for Ambhipods<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for x in self {
            x.hash(state);
        }
    }
}

impl<const N: usize> PartialEq for Ambhipods<N> {
    fn eq(&self, other: &Self) -> bool {
        self.count
            .eq(&other.count)
            .then(|| self.iter().zip(other.iter()).all(|(a, b)| a == b))
            .unwrap_or(false)
    }
}

impl<const N: usize> Eq for Ambhipods<N> {}

impl<const N: usize> Index<Ambhipod> for Ambhipods<N> {
    type Output = usize;

    fn index(&self, idx: Ambhipod) -> &Self::Output {
        &self.count[idx as usize]
    }
}

impl<'a, const N: usize> IntoIterator for &'a Ambhipods<N> {
    type Item = <AmbhipodsIter<'a, N> as Iterator>::Item;
    type IntoIter = AmbhipodsIter<'a, N>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<const N: usize> Ambhipods<N> {
    fn new() -> Self {
        Ambhipods {
            data: [(0, 0); N],
            count: [0; 4],
        }
    }

    #[inline(always)]
    const fn room_size() -> usize {
        [(); N].len() / 4
    }

    fn insert(&mut self, ambhipod: Ambhipod, coord: Coord) -> Result<(), &'static str> {
        if self.count[ambhipod as usize] < Self::room_size() {
            self.data[ambhipod as usize * Self::room_size() + self.count[ambhipod as usize]] =
                coord;
            self.count[ambhipod as usize] += 1;
            Ok(())
        } else {
            Err("too many ambhipods")
        }
    }

    fn get(&self, coord: &Coord) -> Option<Ambhipod> {
        self.data.iter().enumerate().find_map(|(i, x)| {
            if x == coord {
                Some(Ambhipod::try_from(i / Self::room_size()).unwrap())
            } else {
                None
            }
        })
    }

    #[allow(dead_code)]
    fn len(&self) -> usize {
        self.count.iter().sum()
    }

    fn is_empty(&self) -> bool {
        self.count.iter().all(|&x| x == 0)
    }

    fn iter(&self) -> AmbhipodsIter<N> {
        AmbhipodsIter {
            ambhipods: self,
            current: self.find(0),
        }
    }

    fn find(&self, from: usize) -> usize {
        self.data
            .iter()
            .enumerate()
            .skip(from)
            .find_map(|(i, coord)| if *coord != (0, 0) { Some(i) } else { None })
            .unwrap_or(self.data.len())
    }

    fn target_cost(&self, room_lines: &HashMap<Ambhipod, Vec<Coord>>) -> usize {
        self.iter().fold(0, |acc, (coord, ambhipod)| {
            acc + if coord.1 == 1 {
                Self::hallway_to_room(*coord, *room_lines.get(&ambhipod).unwrap().first().unwrap())
                    .count()
            } else {
                Self::room_to_room(*coord, *room_lines.get(&ambhipod).unwrap().first().unwrap())
                    .count()
            }
        })
    }

    fn check_path<I: Iterator<Item = Coord>, F: FnOnce(Coord, Coord) -> I>(
        &self,
        f: F,
        start: Coord,
        end: Coord,
    ) -> Option<usize> {
        f(start, end).try_fold(0, |acc, coord| {
            if self.get(&coord).is_none() {
                Some(acc + 1)
            } else {
                None
            }
        })
    }

    #[must_use]
    fn remove(&self, coord: &Coord) -> Self {
        if let Some(index) = self.data.iter().position(|c| c == coord) {
            let mut data = self.data;
            for i in index..(index / Self::room_size() + 1) * Self::room_size() - 1 {
                data[i] = data[i + 1];
            }

            let mut count = self.count;
            count[index / Self::room_size()] -= 1;

            data[Ambhipod::try_from(index / Self::room_size()).unwrap() as usize
                * Self::room_size()
                + count[index / Self::room_size()]] = (0, 0);

            Ambhipods { data, count }
        } else {
            self.clone()
        }
    }

    #[must_use]
    fn move_to(&self, start: &Coord, end: &Coord) -> Self {
        let mut data = self.data;
        if let Some(value) = data.iter_mut().find(|coord| *coord == start) {
            *value = *end;
        }

        Ambhipods {
            data,
            count: self.count,
        }
    }

    fn room_to_hallway(mut start: Coord, end: Coord) -> impl Iterator<Item = Coord> {
        iter::from_fn(move || {
            if start == end {
                None
            } else {
                if start.1 > end.1 {
                    start.1 -= 1;
                } else if start.0 < end.0 {
                    start.0 += 1;
                } else {
                    start.0 -= 1;
                }
                Some(start)
            }
        })
    }

    fn hallway_to_room(mut start: Coord, end: Coord) -> impl Iterator<Item = Coord> {
        iter::from_fn(move || {
            if start == end {
                None
            } else {
                match start.0.cmp(&end.0) {
                    Ordering::Less => start.0 += 1,
                    Ordering::Equal => start.1 += 1,
                    Ordering::Greater => start.0 -= 1,
                }
                Some(start)
            }
        })
    }

    fn room_to_room(mut start: Coord, end: Coord) -> impl Iterator<Item = Coord> {
        iter::from_fn(move || match (start.0.cmp(&end.0), start.1.cmp(&end.1)) {
            (Ordering::Equal, Ordering::Equal) => None,
            (Ordering::Equal, _) => {
                start.1 += 1;
                Some(start)
            }
            (Ordering::Less, _) if start.1 == 1 => {
                start.0 += 1;
                Some(start)
            }
            (Ordering::Greater, _) if start.1 == 1 => {
                start.0 -= 1;
                Some(start)
            }
            (Ordering::Less, _) | (Ordering::Greater, _) => {
                start.1 -= 1;
                Some(start)
            }
        })
    }
}

#[derive(Copy, Clone)]
enum Cell {
    Undefined,
    Wall,
    Empty,
}

impl fmt::Debug for Cell {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Cell::Undefined => f.write_char(' '),
            Cell::Wall => f.write_char('#'),
            Cell::Empty => f.write_char('.'),
        }
    }
}

enum Either<L, R> {
    Left(L),
    Right(R),
}

impl TryFrom<char> for Either<Cell, Ambhipod> {
    type Error = &'static str;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            '#' => Ok(Either::Left(Cell::Wall)),
            ' ' => Ok(Either::Left(Cell::Undefined)),
            '.' => Ok(Either::Left(Cell::Empty)),
            'A' => Ok(Either::Right(Ambhipod::A)),
            'B' => Ok(Either::Right(Ambhipod::B)),
            'C' => Ok(Either::Right(Ambhipod::C)),
            'D' => Ok(Either::Right(Ambhipod::D)),
            _ => Err("invalid cell"),
        }
    }
}

type Coord = (usize, usize);

struct Map<const N: usize> {
    hallways: Vec<Coord>,
    room_lines: HashMap<Ambhipod, Vec<Coord>>,
    ambhipods: Ambhipods<N>,
}

impl<const N: usize> FromStr for Map<N> {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut hallways = Vec::new();
        let mut rooms = Vec::new();
        let mut ambhipods = Ambhipods::new();

        for (row, line) in input.lines().enumerate() {
            for (col, cell) in line.chars().enumerate() {
                match cell.try_into()? {
                    Either::Left(Cell::Empty) if row == 1 => {
                        hallways.push((col, row));
                    }
                    Either::Left(Cell::Empty) => {
                        rooms.push((col, row));
                    }
                    Either::Left(_) => {}
                    Either::Right(ambhipod) => {
                        ambhipods.insert(ambhipod, (col, row)).ok();
                        if row > 1 {
                            rooms.push((col, row));
                        } else {
                            hallways.push((col, row));
                        }
                    }
                }
            }
        }

        let hallways = hallways
            .into_iter()
            .filter(|(x, _)| !rooms.iter().any(|(x1, _)| x == x1))
            .collect();

        rooms.sort_by(|(x0, y0), (x1, y1)| x0.cmp(x1).then_with(|| y1.cmp(y0)));

        let rooms_tmp = rooms
            .chunks(rooms.len() / 4)
            .zip([Ambhipod::A, Ambhipod::B, Ambhipod::C, Ambhipod::D].iter())
            .map(|(rs, ambhipod)| (*ambhipod, rs.to_vec()))
            .collect::<Vec<(Ambhipod, Vec<_>)>>();

        let mut room_lines = HashMap::new();
        for (ambhipod_line, mut rs) in rooms_tmp {
            while let Some(room_coord) = rs.first_mut() {
                if let Some(ambhipod) = ambhipods.get(room_coord) {
                    if ambhipod == ambhipod_line {
                        ambhipods = ambhipods.remove(room_coord);
                        rs.remove(0);
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            if !rs.is_empty() {
                room_lines.insert(ambhipod_line, rs);
            }
        }

        Ok(Map {
            hallways,
            room_lines,
            ambhipods,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
struct State<const N: usize> {
    f_score: usize,
    ambhipods: Ambhipods<N>,
}

impl<const N: usize> PartialOrd for State<N> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<const N: usize> Ord for State<N> {
    fn cmp(&self, other: &Self) -> Ordering {
        other.f_score.cmp(&self.f_score)
    }
}

fn solve<const N: usize>(input: &str) -> usize {
    let Map::<N> {
        hallways,
        room_lines,
        ambhipods,
    } = input.parse().unwrap();

    let mut queue = BinaryHeap::new();
    let mut g_score = HashMap::from([(ambhipods.clone(), 0)]);

    queue.push(State {
        f_score: ambhipods.target_cost(&room_lines),
        ambhipods,
    });

    while let Some(State {
        f_score: _f_score,
        ambhipods,
    }) = queue.pop()
    {
        if ambhipods.is_empty() {
            return g_score[&ambhipods];
        }

        let current_cost = g_score[&ambhipods];

        for (coord, ambhipod) in &ambhipods {
            if coord.1 == 1 {
                let room = &room_lines[&ambhipod];
                let room_coord = room[room.len() - ambhipods[ambhipod]];
                if let Some(length) =
                    ambhipods.check_path(Ambhipods::<N>::hallway_to_room, *coord, room_coord)
                {
                    let tentative_g_score = current_cost + length * ambhipod.cost();

                    let ambhipods = ambhipods.remove(coord);
                    if tentative_g_score < *g_score.get(&ambhipods).unwrap_or(&usize::MAX) {
                        g_score.insert(ambhipods.clone(), tentative_g_score);

                        queue.push(State {
                            f_score: tentative_g_score + ambhipods.target_cost(&room_lines),
                            ambhipods,
                        });
                    }
                }
            } else {
                for hallway_coord in &hallways {
                    if let Some(length) = ambhipods.check_path(
                        Ambhipods::<N>::room_to_hallway,
                        *coord,
                        *hallway_coord,
                    ) {
                        let tentative_g_score = current_cost + length * ambhipod.cost();

                        let ambhipods = ambhipods.move_to(coord, hallway_coord);

                        if tentative_g_score < *g_score.get(&ambhipods).unwrap_or(&usize::MAX) {
                            g_score.insert(ambhipods.clone(), tentative_g_score);

                            queue.push(State {
                                f_score: tentative_g_score + ambhipods.target_cost(&room_lines),
                                ambhipods,
                            });
                        }
                    }
                }
            }
        }
    }

    panic!("not found")
}

fn solve_1(input: &str) -> usize {
    solve::<8>(input)
}

fn solve_2(input: &str) -> usize {
    let mut input = input.lines().collect::<Vec<_>>();
    input.splice(
        3..3,
        r#"  #D#C#B#A#
  #D#B#A#C#"#
            .lines(),
    );

    let input = input.join("\n");

    solve::<16>(&input)
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
        static ref INPUT: &'static str = r#"#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########"#;
    }

    #[test]
    fn test_parsing() {
        let Map::<8> {
            hallways: _hallways,
            ambhipods,
            room_lines,
        } = INPUT.parse().unwrap();

        assert_eq!(ambhipods.len(), 6);
        assert_eq!(room_lines[&Ambhipod::A], vec![(3, 2)]);
        assert_eq!(room_lines[&Ambhipod::C], vec![(7, 2)]);
    }

    #[test]
    fn test_room_to_hallway() {
        assert_eq!(
            Ambhipods::<8>::room_to_hallway((3, 3), (1, 1)).collect::<Vec<_>>(),
            vec![(3, 2), (3, 1), (2, 1), (1, 1)]
        );
        assert_eq!(
            Ambhipods::<8>::room_to_hallway((3, 3), (5, 1)).collect::<Vec<_>>(),
            vec![(3, 2), (3, 1), (4, 1), (5, 1)]
        );
    }

    #[test]
    fn test_hallway_to_room() {
        assert_eq!(
            Ambhipods::<8>::hallway_to_room((1, 1), (3, 3)).collect::<Vec<_>>(),
            vec![(2, 1), (3, 1), (3, 2), (3, 3)]
        );
        assert_eq!(
            Ambhipods::<8>::hallway_to_room((5, 1), (3, 3)).collect::<Vec<_>>(),
            vec![(4, 1), (3, 1), (3, 2), (3, 3)]
        );
    }

    #[test]
    fn test_room_to_room() {
        assert_eq!(
            Ambhipods::<8>::room_to_room((3, 3), (5, 3)).collect::<Vec<_>>(),
            vec![(3, 2), (3, 1), (4, 1), (5, 1), (5, 2), (5, 3)]
        );
        assert_eq!(
            Ambhipods::<8>::room_to_room((5, 3), (3, 3)).collect::<Vec<_>>(),
            vec![(5, 2), (5, 1), (4, 1), (3, 1), (3, 2), (3, 3)]
        );
    }

    #[test]
    fn simple1_solve_1() {
        assert_eq!(
            solve_1(
                r#"#############
#.........A.#
###.#B#C#D###
  #A#B#C#D#
  #########"#
            ),
            8
        );
    }

    #[test]
    fn simple2_iter() {
        let ambhipods = Ambhipods {
            data: [
                (10, 1),
                (0, 0),
                (0, 0),
                (0, 0),
                (0, 0),
                (0, 0),
                (6, 1),
                (8, 1),
            ],
            count: [1, 0, 0, 2],
        };

        assert_eq!(
            ambhipods.iter().collect::<Vec<_>>(),
            vec![
                (&(10, 1), Ambhipod::A),
                (&(6, 1), Ambhipod::D),
                (&(8, 1), Ambhipod::D)
            ]
        );
    }

    #[test]
    fn simple2_solve_1() {
        assert_eq!(
            solve_1(
                r#"#############
#.....D.D.A.#
###.#B#C#.###
  #A#B#C#.#
  #########"#
            ),
            7008
        );
    }

    #[test]
    fn simple3_solve_1() {
        assert_eq!(
            solve_1(
                r#"#############
#.....D.....#
###.#B#C#D###
  #A#B#C#A#
  #########"#
            ),
            9011
        );
    }

    #[test]
    fn simple4_solve_1() {
        assert_eq!(
            solve_1(
                r#"#############
#.....D.....#
###B#.#C#D###
  #A#B#C#A#
  #########"#
            ),
            9051
        );
    }

    #[test]
    fn simple5_solve_1() {
        assert_eq!(
            solve_1(
                r#"#############
#...B.......#
###B#.#C#D###
  #A#D#C#A#
  #########"#
            ),
            12081
        );
    }

    #[test]
    fn simple1_parsing() {
        let Map::<8> {
            hallways,
            ambhipods,
            room_lines,
        } = r#"#############
#.........A.#
###.#B#C#D###
  #A#B#C#D#
  #########"#
            .parse()
            .unwrap();

        assert_eq!(
            hallways,
            vec![(1, 1), (2, 1), (4, 1), (6, 1), (8, 1), (10, 1), (11, 1)]
        );
        assert_eq!(ambhipods.len(), 1);
        assert_eq!(room_lines[&Ambhipod::A], vec![(3, 2)]);
    }

    #[test]
    fn ambhipods_remove_1() {
        let mut ambhipods = Ambhipods::<8>::new();

        ambhipods.data[0] = (1, 1);
        ambhipods.count[0] = 1;

        let ambhipods = ambhipods.remove(&(1, 1));

        assert_eq!(ambhipods, Ambhipods::new());
    }

    #[test]
    fn ambhipods_remove_2() {
        let mut ambhipods = Ambhipods::new();

        ambhipods.data[2] = (1, 1);
        ambhipods.data[3] = (1, 2);
        ambhipods.data[4] = (1, 3);
        ambhipods.count[1] = 2;

        let ambhipods = ambhipods.remove(&(1, 1));

        assert_eq!(
            ambhipods,
            Ambhipods {
                data: [
                    (0, 0),
                    (0, 0),
                    (1, 2),
                    (0, 0),
                    (1, 3),
                    (0, 0),
                    (0, 0),
                    (0, 0)
                ],
                count: [0, 1, 0, 0],
            }
        );
    }

    #[test]
    fn ambhipods_remove_2_1() {
        let mut ambhipods = Ambhipods::new();

        ambhipods.data[2] = (1, 1);
        ambhipods.data[3] = (1, 2);
        ambhipods.data[4] = (1, 3);
        ambhipods.count[1] = 2;

        let ambhipods = ambhipods.remove(&(1, 2));

        assert_eq!(
            ambhipods,
            Ambhipods {
                data: [
                    (0, 0),
                    (0, 0),
                    (1, 1),
                    (0, 0),
                    (1, 3),
                    (0, 0),
                    (0, 0),
                    (0, 0)
                ],
                count: [0, 1, 0, 0],
            }
        );
    }

    #[test]
    fn ambhipods_remove_3() {
        let mut ambhipods = Ambhipods::new();

        ambhipods.data[2] = (1, 1);
        ambhipods.data[3] = (1, 2);
        ambhipods.data[4] = (1, 3);
        ambhipods.data[6] = (1, 6);
        ambhipods.data[7] = (1, 7);
        ambhipods.count[1] = 2;
        ambhipods.count[2] = 1;
        ambhipods.count[3] = 2;

        let ambhipods = ambhipods.remove(&(1, 7));

        assert_eq!(
            ambhipods,
            Ambhipods {
                data: [
                    (0, 0),
                    (0, 0),
                    (1, 1),
                    (1, 2),
                    (1, 3),
                    (0, 0),
                    (1, 6),
                    (0, 0)
                ],
                count: [0, 2, 1, 1],
            }
        );
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 12521);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 44169);
    }
}
