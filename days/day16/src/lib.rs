use aoc::solver::Solver;
use std::collections::VecDeque;

pub struct Day16;

#[derive(Debug)]
enum Packet {
    Literal {
        version: usize,
        value: usize,
    },
    Operator {
        version: usize,
        operator: usize,
        packets: Vec<Packet>,
    },
}

impl Packet {
    fn version(self) -> usize {
        match self {
            Packet::Literal { version, value: _ } => version,
            Packet::Operator {
                version,
                operator: _,
                packets,
            } => version + packets.into_iter().map(|p| p.version()).sum::<usize>(),
        }
    }

    fn eval(self) -> usize {
        match self {
            Packet::Literal { version: _, value } => value,
            Packet::Operator {
                version: _,
                operator,
                packets,
            } => {
                let mut values = packets.into_iter().map(|p| p.eval());
                match operator {
                    0 => values.sum(),
                    1 => values.product(),
                    2 => values.min().unwrap(),
                    3 => values.max().unwrap(),
                    5 | 6 | 7 => {
                        let a = values.next().unwrap();
                        let b = values.next().unwrap();
                        if operator == 5 {
                            (a > b) as usize
                        } else if operator == 6 {
                            (a < b) as usize
                        } else {
                            (a == b) as usize
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
}

struct Bits<'a> {
    cursor: usize,
    chars: std::str::Chars<'a>,
    bitstack: VecDeque<bool>,
}

impl<'a> Iterator for Bits<'a> {
    type Item = bool;
    fn next(&mut self) -> Option<bool> {
        self.cursor += 1;
        self.bitstack.pop_front().or_else(|| {
            if let Some(c) = self.chars.next() {
                let c = c.to_digit(16).unwrap();
                self.bitstack.push_back(c & (1 << 2) != 0);
                self.bitstack.push_back(c & (1 << 1) != 0);
                self.bitstack.push_back(c & 1 != 0);
                Some(c & (1 << 3) != 0)
            } else {
                None
            }
        })
    }
}

impl<'a> Bits<'a> {
    fn new(data: &'a str) -> Self {
        Self {
            cursor: 0,
            chars: data.chars(),
            bitstack: VecDeque::new(),
        }
    }

    fn read_var_width(&mut self, width: usize) -> usize {
        let mut value = 0;
        for (n, b) in self.take(width).enumerate() {
            if b {
                value |= 1 << (width - 1 - n);
            }
        }
        value
    }

    fn read_literal(&mut self) -> usize {
        let mut value = 0;
        loop {
            let flag = self.next().unwrap();
            let next_value = self.read_var_width(4);
            value += next_value;
            if !flag {
                break;
            }
            value <<= 4;
        }
        value
    }

    fn read_packet(&mut self) -> Packet {
        let version = self.read_var_width(3);
        let operator = self.read_var_width(3);
        match operator {
            4 => Packet::Literal {
                version,
                value: self.read_literal(),
            },
            _ => {
                let mut packets = Vec::new();
                if self.next().unwrap() {
                    let num_packets = self.read_var_width(11);
                    for _ in 0..num_packets {
                        packets.push(self.read_packet());
                    }
                } else {
                    let packet_size = self.read_var_width(15);
                    let current = self.cursor;
                    while self.cursor < current + packet_size {
                        packets.push(self.read_packet());
                    }
                }
                Packet::Operator {
                    version,
                    operator,
                    packets,
                }
            }
        }
    }
}

impl Solver<&str> for Day16 {
    fn part1(data: &str) -> usize {
        Bits::new(data).read_packet().version()
    }

    fn part2(data: &str) -> usize {
        Bits::new(data).read_packet().eval()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(Day16::part1("8A004A801A8002F478"), 16);
        assert_eq!(Day16::part1("620080001611562C8802118E34"), 12);
        assert_eq!(Day16::part1("C0015000016115A2E0802F182340"), 23);
        assert_eq!(Day16::part1("A0016C880162017C3686B18A3D4780"), 31);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day16::part2("C200B40A82"), 3);
        assert_eq!(Day16::part2("04005AC33890"), 54);
        assert_eq!(Day16::part2("880086C3E88112"), 7);
        assert_eq!(Day16::part2("CE00C43D881120"), 9);
        assert_eq!(Day16::part2("D8005AC2A8F0"), 1);
        assert_eq!(Day16::part2("F600BC2D8F"), 0);
        assert_eq!(Day16::part2("9C005AC2F8F0"), 0);
        assert_eq!(Day16::part2("9C0141080250320F1802104A08"), 1);
    }
}
