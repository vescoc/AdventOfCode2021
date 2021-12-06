use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

fn solve(input: &str, mut count: usize) -> u128 {
    let mut lanternfish_lvl = [0; 9];

    for n in input
        .split(',')
        .map(|n| n.parse::<usize>().expect("invalid input"))
    {
        lanternfish_lvl[n as usize] += 1;
    }

    while count > 0 {
        let new_borns = lanternfish_lvl[0];
        for i in 1..lanternfish_lvl.len() {
            lanternfish_lvl[i - 1] = lanternfish_lvl[i];
        }
        lanternfish_lvl[6] += new_borns;
        lanternfish_lvl[8] = new_borns;

        count -= 1;
    }

    lanternfish_lvl.into_iter().sum()
}

fn solve_1(input: &str) -> u128 {
    solve(input, 80)
}

fn solve_2(input: &str) -> u128 {
    solve(input, 256)
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
        static ref INPUT: &'static str = r#"3,4,3,1,2"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 5934);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 26984457539);
    }
}
