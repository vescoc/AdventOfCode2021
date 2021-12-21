use regex::Regex;

use lazy_static::lazy_static;

lazy_static! {
    static ref PLAYER_1_RE: Regex = Regex::new(r"Player 1 starting position: (\d+)").unwrap();
    static ref PLAYER_2_RE: Regex = Regex::new(r"Player 2 starting position: (\d+)").unwrap();
    static ref INPUT: &'static str = include_str!("../../input");
}

fn parse_input(input: &str) -> Result<(u64, u64), &'static str> {
    let invalid_number = |_| "invalid number";

    let mut lines = input.lines();

    let player1 = PLAYER_1_RE
        .captures(lines.next().ok_or("expecting player 1")?)
        .ok_or("invalid input player 1")?;
    let player2 = PLAYER_2_RE
        .captures(lines.next().ok_or("expecting player 2")?)
        .ok_or("invalid input player 2")?;

    Ok((
        player1[1].parse().map_err(invalid_number)?,
        player2[1].parse().map_err(invalid_number)?,
    ))
}

fn solve_1(input: &str) -> u64 {
    #[derive(Copy, Clone, Debug)]
    struct Game {
        player_1_turn: bool,
        player_1_position: u64,
        player_2_position: u64,
        player_1_score: u64,
        player_2_score: u64,
        dice: u64,
        dice_rolls: u64,
    }

    impl Iterator for Game {
        type Item = Game;

        fn next(&mut self) -> Option<Self::Item> {
            let (player_position, player_score) = if self.player_1_turn {
                (&mut self.player_1_position, &mut self.player_1_score)
            } else {
                (&mut self.player_2_position, &mut self.player_2_score)
            };
            *player_position = (*player_position + 3 * (self.dice + 1) - 1) % 10 + 1;
            *player_score += *player_position;

            self.dice = (self.dice + 3 - 1) % 100 + 1;
            self.dice_rolls += 3;
            self.player_1_turn = !self.player_1_turn;

            Some(*self)
        }
    }

    impl Game {
        fn new(player_1_position: u64, player_2_position: u64) -> Self {
            Self {
                player_1_turn: true,
                player_1_position,
                player_2_position,
                player_1_score: 0,
                player_2_score: 0,
                dice: 1,
                dice_rolls: 0,
            }
        }
    }

    let (p1, p2) = parse_input(input).expect("invalid input");

    Game::new(p1, p2)
        .find(
            |Game {
                 player_1_score,
                 player_2_score,
                 ..
             }| *player_1_score >= 1000 || *player_2_score >= 1000,
        )
        .map(
            |Game {
                 player_1_score,
                 player_2_score,
                 dice_rolls,
                 ..
             }| {
                dice_rolls
                    * if player_1_score >= 1000 {
                        player_2_score
                    } else {
                        player_1_score
                    }
            },
        )
        .unwrap()
}

fn solve_2(input: &str) -> u64 {
    use std::collections::HashMap;

    const CASES: [(u64, u64); 7] = {
        let mut cases = [(0, 0); 7];
        let mut i = 1;
        while i < 4 {
            let mut j = 1;
            while j < 4 {
                let mut k = 1;
                while k < 4 {
                    let s = i + j + k;
                    cases[(s - 3) as usize] = (s, cases[(s - 3) as usize].1 + 1);
                    k += 1;
                }
                j += 1;
            }
            i += 1;
        }
        cases
    };

    #[derive(PartialEq, Eq, Hash, Debug)]
    struct Game {
        player_1_turn: bool,
        player_1_position: u64,
        player_2_position: u64,
        player_1_score: u64,
        player_2_score: u64,
    }

    struct PlayGame(HashMap<Game, (u64, u64)>);

    impl Game {
        fn new(
            player_1_turn: bool,
            player_1_position: u64,
            player_2_position: u64,
            player_1_score: u64,
            player_2_score: u64,
        ) -> Self {
            Self {
                player_1_turn,
                player_1_position,
                player_2_position,
                player_1_score,
                player_2_score,
            }
        }
    }

    impl PlayGame {
        fn new() -> Self {
            Self(HashMap::new())
        }

        fn play(
            &mut self,
            player_1_turn: bool,
            player_1_position: u64,
            player_2_position: u64,
            player_1_score: u64,
            player_2_score: u64,
        ) -> (u64, u64) {
            let game = Game::new(
                player_1_turn,
                player_1_position,
                player_2_position,
                player_1_score,
                player_2_score,
            );
            if let Some(e) = self.0.get(&game) {
                return *e;
            }

            let (player_1_wins, player_2_wins) =
                CASES
                    .iter()
                    .fold((0, 0), |(player_1_wins, player_2_wins), (i, n)| {
                        let (i_player_1_wins, i_player_2_wins) = if player_1_turn {
                            let player_new_position = (player_1_position + i - 1) % 10 + 1;
                            let player_new_score = player_1_score + player_new_position;
                            if player_new_score >= 21 {
                                (*n, 0)
                            } else {
                                let (i_player_1_wins, i_player_2_wins) = self.play(
                                    !player_1_turn,
                                    player_new_position,
                                    player_2_position,
                                    player_new_score,
                                    player_2_score,
                                );

                                (i_player_1_wins * n, i_player_2_wins * n)
                            }
                        } else {
                            let player_new_position = (player_2_position + i - 1) % 10 + 1;
                            let player_new_score = player_2_score + player_new_position;
                            if player_new_score >= 21 {
                                (0, *n)
                            } else {
                                let (i_player_1_wins, i_player_2_wins) = self.play(
                                    !player_1_turn,
                                    player_1_position,
                                    player_new_position,
                                    player_1_score,
                                    player_new_score,
                                );

                                (i_player_1_wins * n, i_player_2_wins * n)
                            }
                        };

                        (
                            player_1_wins + i_player_1_wins,
                            player_2_wins + i_player_2_wins,
                        )
                    });

            self.0.insert(game, (player_1_wins, player_2_wins));

            (player_1_wins, player_2_wins)
        }
    }

    let (p1, p2) = parse_input(input).expect("invalid input");

    let mut game = PlayGame::new();
    let (player_1_score, player_2_score) = game.play(true, p1, p2, 0, 0);

    player_1_score.max(player_2_score)
}

pub fn part_1() -> u64 {
    solve_1(&INPUT)
}

pub fn part_2() -> u64 {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"Player 1 starting position: 4
Player 2 starting position: 8"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 739785);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 444356092776315);
    }
}
