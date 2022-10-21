/// thanks https://www.reddit.com/user/LinAGKar/

use std::cmp::max;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

pub fn part_1() -> i64 {
    let mut lines = INPUT.lines();
    let mut pop = Vec::new();
    let mut add_table_0 = Vec::new();
    let mut add_table_1 = Vec::new();

    let mut get_num = |index| {
        lines
            .nth(index)
            .unwrap()
            .split_whitespace()
            .nth(2)
            .unwrap_or("0")
            .parse::<i64>()
            .unwrap_or(0)
    };

    for _ in 0..14 {
        pop.push(get_num(4) == 26);
        add_table_0.push(get_num(0));
        add_table_1.push(get_num(9));
        get_num(1);
    }

    let mut stack = Vec::new();
    let mut num = vec![0; 14];
    for i in 0..14 {
        if pop[i] {
            let (push_pos, push_add) = stack.pop().unwrap();
            let pop_sub = -add_table_0[i];
            let pushed_num = 9 + max(push_add, pop_sub);
            num[push_pos] = pushed_num - push_add;
            num[i] = pushed_num - pop_sub;
        } else {
            stack.push((i, add_table_1[i]));
        }
    }

    num.into_iter().fold(0, |acc, num| acc * 10 + num)
}

pub fn part_2() -> i64 {
    let mut lines = INPUT.lines();
    let mut pop = Vec::new();
    let mut add_table_0 = Vec::new();
    let mut add_table_1 = Vec::new();

    let mut get_num = |index| {
        lines
            .nth(index)
            .unwrap()
            .split_whitespace()
            .nth(2)
            .unwrap_or("0")
            .parse::<i64>()
            .unwrap_or(0)
    };

    for _ in 0..14 {
        pop.push(get_num(4) == 26);
        add_table_0.push(get_num(0));
        add_table_1.push(get_num(9));
        get_num(1);
    }

    let mut stack = Vec::new();
    let mut num = vec![0; 14];
    for i in 0..14 {
        if pop[i] {
            let (push_pos, push_add) = stack.pop().unwrap();
            let pop_sub = -add_table_0[i];
            let pushed_num = 1 + max(push_add, pop_sub);
            num[push_pos] = pushed_num - push_add;
            num[i] = pushed_num - pop_sub;
        } else {
            stack.push((i, add_table_1[i]));
        }
    }

    num.into_iter().fold(0, |acc, num| acc * 10 + num)
}

#[cfg(test)]
mod tests {
    use std::iter;

    #[test]
    fn test_example_1() {
        alu::alu!(
            Example1
            {
                inp x
                mul x -1
            }
        );

        let mut example = Example1::new();
        assert_eq!(example.run(iter::once(10)), Some(0));
        assert_eq!(example.x, -10);
    }

    #[test]
    fn test_example_2() {
        alu::alu!(
            Example2
            {
                inp z
                inp x
                mul z 3
                eql z x
            }
        );

        let mut example = Example2::new();
        assert_eq!(example.run(vec![1, 3].into_iter()), Some(1));
    }

    #[test]
    fn test_example_3() {
        alu::alu!(
            Example3
            {
                inp w
                add z w
                mod z 2
                div w 2
                add y w
                mod y 2
                div w 2
                add x w
                mod x 2
                div w 2
                mod w 2
            }
        );

        let mut example = Example3::new();
        example.run(iter::once(0b1010));
        assert_eq!(example.w, 1);
        assert_eq!(example.x, 0);
        assert_eq!(example.y, 1);
        assert_eq!(example.z, 0);
    }
}
