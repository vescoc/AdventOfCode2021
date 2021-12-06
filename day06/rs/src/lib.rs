use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

fn solve(input: &str, mut count: usize) -> u128 {
    let mut lanterfish_lvl: [u128; 9] = input
        .split(',')
        .map(|n| n.parse::<usize>().expect("invalid input"))
        .fold([0; 9], |mut acc, n| {
            acc[n as usize] += 1;
            acc
        });

    while count > 0 {
        let new_borns = lanterfish_lvl[0];
        for i in 1..lanterfish_lvl.len() {
            lanterfish_lvl[i - 1] = lanterfish_lvl[i];
        }
        lanterfish_lvl[6] += new_borns;
        lanterfish_lvl[8] = new_borns;

        count -= 1;
    }

    lanterfish_lvl.into_iter().sum()
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
