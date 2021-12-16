use std::fmt;
use std::str::FromStr;

use lazy_static::lazy_static;

type Value = u128;

#[derive(PartialEq, Debug)]
struct Packet(u32, Box<PacketType>);

#[derive(PartialEq, Debug)]
enum PacketType {
    Literal(Value),
    Sum(Vec<Packet>),
    Product(Vec<Packet>),
    Minimum(Vec<Packet>),
    Maximum(Vec<Packet>),
    GreaterThan(Packet, Packet),
    LessThan(Packet, Packet),
    EqualTo(Packet, Packet),
}

struct Tokenizer {
    input: Vec<u32>,
    position: usize,
}

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

impl Packet {
    fn new(version: u32, packet_type: PacketType) -> Self {
        Self(version, Box::new(packet_type))
    }

    fn checksum(&self) -> u32 {
        self.0 + self.1.checksum()
    }

    fn calculate(&self) -> Value {
        self.1.calculate()
    }

    fn parse(tokenizer: &mut Tokenizer) -> Result<Packet, &'static str> {
        use PacketType::*;

        let version = tokenizer.bits(3)?;
        match tokenizer.bits(3)? {
            4 => {
                let mut literal = 0;
                loop {
                    let chunk = tokenizer.bits(5)? as Value;
                    literal = literal << 4 | (chunk & 0b01111);
                    if chunk & 0b10000 == 0 {
                        break;
                    }
                }

                Ok(Packet::new(version, Literal(literal)))
            }

            type_id => {
                let packets = if tokenizer.bit()? {
                    let packets_count = tokenizer.bits(11)?;

                    let mut packets = vec![];
                    for _i in 0..packets_count {
                        packets.push(Packet::parse(tokenizer)?);
                    }

                    packets
                } else {
                    let total_length = tokenizer.bits(15)? as usize;
                    let current_position = tokenizer.position();

                    let mut packets = vec![];
                    while tokenizer.position() < current_position + total_length {
                        packets.push(Packet::parse(tokenizer)?);
                    }

                    packets
                };

                match type_id {
                    0 if packets.is_empty() => Err("zero packets"),
                    0 => Ok(Packet::new(version, Sum(packets))),
                    1 if packets.is_empty() => Err("zero packets"),
                    1 => Ok(Packet::new(version, Product(packets))),
                    2 if packets.is_empty() => Err("zero packets"),
                    2 => Ok(Packet::new(version, Minimum(packets))),
                    3 if packets.is_empty() => Err("zero packets"),
                    3 => Ok(Packet::new(version, Maximum(packets))),
                    5 if packets.len() > 2 => Err("too packets"),
                    5 => {
                        let mut parts = packets.into_iter();
                        Ok(Packet::new(
                            version,
                            GreaterThan(
                                parts.next().ok_or("left missing")?,
                                parts.next().ok_or("right missing")?,
                            ),
                        ))
                    }
                    6 if packets.len() > 2 => Err("too packets"),
                    6 => {
                        let mut parts = packets.into_iter();
                        Ok(Packet::new(
                            version,
                            LessThan(
                                parts.next().ok_or("left missing")?,
                                parts.next().ok_or("right missing")?,
                            ),
                        ))
                    }
                    7 if packets.len() > 2 => Err("too packets"),
                    7 => {
                        let mut parts = packets.into_iter();
                        Ok(Packet::new(
                            version,
                            EqualTo(
                                parts.next().ok_or("left missing")?,
                                parts.next().ok_or("right missing")?,
                            ),
                        ))
                    }
                    _ => Err("invalid packet type"),
                }
            }
        }
    }
}

impl FromStr for Packet {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Packet::parse(&mut Tokenizer::new(input)?)
    }
}

impl PacketType {
    fn checksum(&self) -> u32 {
        use PacketType::*;

        match self {
            Literal(_) => 0,
            Sum(packets) => packets.iter().map(|packet| packet.checksum()).sum(),
            Product(packets) => packets.iter().map(|packet| packet.checksum()).sum(),
            Minimum(packets) => packets.iter().map(|packet| packet.checksum()).sum(),
            Maximum(packets) => packets.iter().map(|packet| packet.checksum()).sum(),
            GreaterThan(left, right) => left.checksum() + right.checksum(),
            LessThan(left, right) => left.checksum() + right.checksum(),
            EqualTo(left, right) => left.checksum() + right.checksum(),
        }
    }

    fn calculate(&self) -> Value {
        use PacketType::*;

        match self {
            Literal(value) => *value,
            Sum(packets) => packets.iter().map(|packet| packet.calculate()).sum(),
            Product(packets) => packets.iter().map(|packet| packet.calculate()).product(),
            Minimum(packets) => packets
                .iter()
                .map(|packet| packet.calculate())
                .min()
                .unwrap(),
            Maximum(packets) => packets
                .iter()
                .map(|packet| packet.calculate())
                .max()
                .unwrap(),
            GreaterThan(left, right) => {
                if left.calculate() > right.calculate() {
                    1
                } else {
                    0
                }
            }
            LessThan(left, right) => {
                if left.calculate() < right.calculate() {
                    1
                } else {
                    0
                }
            }
            EqualTo(left, right) => {
                if left.calculate() == right.calculate() {
                    1
                } else {
                    0
                }
            }
        }
    }
}

impl fmt::Debug for Tokenizer {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "position: {}", self.position)?;
        for b in &self.input {
            write!(f, "{:04b}", b)?;
        }

        write!(f, "\n{:>width$}", '^', width = self.position + 1)
    }
}

impl Tokenizer {
    fn new(input: &str) -> Result<Self, &'static str> {
        Ok(Self {
            input: input
                .chars()
                .map(|c| c.to_digit(16).ok_or("invalid digit"))
                .collect::<Result<Vec<_>, _>>()?,
            position: 0,
        })
    }

    fn position(&self) -> usize {
        self.position
    }

    fn bits(&mut self, n: usize) -> Result<u32, &'static str> {
        assert!(n > 0 && n <= 32);

        let mut r = 0;
        for _i in 0..n {
            r = r << 1 | if self.bit()? { 1 } else { 0 };
        }

        Ok(r)
    }

    fn bit(&mut self) -> Result<bool, &'static str> {
        if self.position < self.input.len() * 4 {
            let (m, b) = (self.position % 4, self.position / 4);
            let r = self.input[b] & (0b1000 >> m) != 0;

            self.position += 1;

            Ok(r)
        } else {
            Err("eof")
        }
    }
}

fn solve_1(input: &str) -> u32 {
    input.parse::<Packet>().expect("invalid packet").checksum()
}

fn solve_2(input: &str) -> Value {
    input.parse::<Packet>().expect("invalid packet").calculate()
}

pub fn part_1() -> u32 {
    solve_1(&INPUT)
}

pub fn part_2() -> Value {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_literal_2021() {
        use PacketType::*;

        assert_eq!(
            "D2FE28".parse::<Packet>().unwrap(),
            Packet::new(6, Literal(2021))
        );
    }

    #[test]
    fn example_op_1() {
        use PacketType::*;

        assert_eq!(
            "38006F45291200".parse::<Packet>().unwrap(),
            Packet::new(
                1,
                LessThan(Packet::new(6, Literal(10)), Packet::new(2, Literal(20)))
            ),
        );
    }

    #[test]
    fn example_op_2() {
        use PacketType::*;

        assert_eq!(
            "EE00D40C823060".parse::<Packet>().unwrap(),
            Packet::new(
                7,
                Maximum(vec![
                    Packet::new(2, Literal(1)),
                    Packet::new(4, Literal(2)),
                    Packet::new(1, Literal(3)),
                ]),
            ),
        );
    }

    #[test]
    fn same_results_1_1() {
        assert_eq!(solve_1("8A004A801A8002F478"), 16);
    }

    #[test]
    fn same_results_1_2() {
        assert_eq!(solve_1("620080001611562C8802118E34"), 12);
    }

    #[test]
    fn same_results_1_3() {
        assert_eq!(solve_1("C0015000016115A2E0802F182340"), 23);
    }

    #[test]
    fn same_results_1_4() {
        assert_eq!(solve_1("A0016C880162017C3686B18A3D4780"), 31);
    }

    #[test]
    fn same_results_2_1() {
        assert_eq!(solve_2("C200B40A82"), 3);
    }

    #[test]
    fn same_results_2_2() {
        assert_eq!(solve_2("04005AC33890"), 54);
    }

    #[test]
    fn same_results_2_3() {
        assert_eq!(solve_2("880086C3E88112"), 7);
    }

    #[test]
    fn same_results_2_4() {
        assert_eq!(solve_2("CE00C43D881120"), 9);
    }

    #[test]
    fn same_results_2_5() {
        assert_eq!(solve_2("D8005AC2A8F0"), 1);
    }

    #[test]
    fn same_results_2_6() {
        assert_eq!(solve_2("F600BC2D8F"), 0);
    }

    #[test]
    fn same_results_2_7() {
        assert_eq!(solve_2("9C005AC2F8F0"), 0);
    }

    #[test]
    fn same_results_2_8() {
        assert_eq!(solve_2("9C0141080250320F1802104A08"), 1);
    }
}
