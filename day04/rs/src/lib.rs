use std::str::FromStr;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

struct Bingo<const T: usize> {
    draw_order: [usize; T],
    rounds: [usize; T],

    boards: Vec<Board>,
}

const BOARD_COLS: usize = 5;
const BOARD_ROWS: usize = 5;

struct Board([usize; BOARD_COLS * BOARD_ROWS]);

impl Board {
    fn winner_round(&self, draw_order: &[usize]) -> usize {
        let mut max_r = [usize::MIN; BOARD_ROWS];
        let mut max_c = [usize::MIN; BOARD_COLS];

        for (i, v) in self.0.iter().enumerate() {
            let (r, c) = (i / BOARD_COLS, i % BOARD_COLS);

            let o = draw_order[*v];

            max_r[r] = max_r[r].max(o);
            max_c[c] = max_c[c].max(o);
        }

        max_r.into_iter().chain(max_c.into_iter()).min().unwrap()
    }
}

impl<const T: usize> FromStr for Bingo<T> {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut iter = input.split("\n\n");

        let draw_order_values = iter.next().ok_or("invalid format")?;

        let mut draw_order = [0; T];
        let mut rounds = [0; T];
        for (i, v) in draw_order_values.split(',').enumerate() {
            let v = v.parse::<usize>().map_err(invalid_number)?;

            if v > T {
                return Err("invalid number");
            }
            if i > T {
                return Err("out of bounds");
            }

            draw_order[v] = i;
            rounds[i] = v;
        }

        Ok(Bingo {
            draw_order,
            rounds,
            boards: iter.map(|data| data.parse()).collect::<Result<_, _>>()?,
        })
    }
}

impl FromStr for Board {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut board = [0; BOARD_COLS * BOARD_ROWS];

        for (i, v) in input
            .lines()
            .flat_map(|l| l.split_ascii_whitespace())
            .enumerate()
        {
            if i > BOARD_COLS * BOARD_ROWS {
                return Err("invalid format");
            }

            let v = v.parse().map_err(invalid_number)?;
            if v > 100 {
                return Err("out of bounds");
            }

            board[i] = v;
        }

        Ok(Board(board))
    }
}

fn invalid_number<E>(_: E) -> &'static str {
    "invalid number"
}

macro_rules! solve {
    ($id:ident, $f:ident) => {
        fn $id(input: &str) -> usize {
            let bingo: Bingo<100> = input.parse().expect("invalid input");

            let (winner, winner_round) = bingo
                .boards
                .iter()
                .enumerate()
                .map(|(i, board)| (i, board.winner_round(&bingo.draw_order)))
                .$f(|(_, r)| *r)
                .unwrap();

            let sum: usize = bingo.boards[winner]
                .0
                .iter()
                .filter(|&v| bingo.draw_order[*v] > winner_round)
                .sum();

            sum * bingo.rounds[winner_round]
        }
    };
}

solve!(solve_1, min_by_key);
solve!(solve_2, max_by_key);

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
        static ref INPUT: &'static str = r#"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 4512);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 1924);
    }
}
