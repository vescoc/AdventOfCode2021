use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

fn solve<F>(input: &str, mut f: F) -> i32
where
    F: FnMut(i32) -> i32,
{
    let crabs: Vec<i32> = input
        .split(',')
        .map(|n| n.parse().expect("invalid number"))
        .collect();

    let max = crabs.iter().max().copied().expect("no crabs");

    (0..=max)
        .map(|i| crabs.iter().map(|p| f((p - i).abs())).sum())
        .min()
        .unwrap()
}

fn solve_1(input: &str) -> i32 {
    solve(input, |n| n)
}

fn solve_2(input: &str) -> i32 {
    solve(input, |n| n * (n + 1) / 2)
}

pub fn part_1() -> i32 {
    solve_1(&INPUT)
}

pub fn part_2() -> i32 {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"16,1,2,0,4,2,7,1,2,14"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 37);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 168);
    }
}
