use std::iter;
use std::time::Instant;
//use rayon::prelude::*;

alu::alu!(
    Alu
    { include "../input" }
);

fn solve_1() -> String {
    let mut time = Instant::now();
    
    iter::successors(Some([9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9]),
                     |v| {
                         let mut v = *v;
                         let mut d = 13;
                         loop {
                             if v[d] - 1 == 0 {
                                 v[d] = 9;
                                 d -= 1;
                             } else {
                                 v[d] -= 1;
                                 break Some(v);
                             }
                         }
                     })
        .enumerate()
        .inspect(|(i, v)| {
            if i % 100_000_000 == 0 {
                println!("elapsed: {}s {:?}", time.elapsed().as_secs(), v);
                time = Instant::now();
            }
        })
        //.par_bridge()
        .find(|(_, v)| match Alu::new().run(v.iter().copied()) {
            Some(0) => true,
            _ => false,
        })
        .map(|(_, v)| v.iter().map(|d| char::from_digit(*d as u32, 10).unwrap()).collect())
        .unwrap()    
}

fn solve_2() -> String {
    todo!()
}

pub fn part_1() -> String {
    solve_1()
}

pub fn part_2() -> String {
    solve_2()
}

#[cfg(test)]
mod tests {
    use super::*;

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
