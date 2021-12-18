use aoc::solver::Solver;

pub struct Day18;

#[derive(Debug)]
enum Token {
    Literal(u8),
    Pair(Box<Pair>),
}
type Pair = (Token, Token);

impl Token {
    fn new_pair(a: Token, b: Token) -> Token {
        Token::Pair(Box::new((a, b)))
    }

    fn parse(data: &str) -> Self {
        let mut stack = Vec::new();
        for c in data.chars() {
            match c {
                ']' => {
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    stack.push(Token::new_pair(a, b));
                }
                '[' | ',' => {}
                _ => stack.push(Token::Literal(c as u8 - b'0')),
            }
        }
        stack.pop().unwrap()
    }

    fn apply<T>(self, literal_fn: fn(u8) -> T, pair_fn: fn(Token, Token) -> T) -> T {
        match self {
            Token::Literal(value) => literal_fn(value),
            Token::Pair(bx) => {
                let (a, b) = *bx;
                pair_fn(a, b)
            }
        }
    }

    fn is_literal(&self) -> bool {
        matches!(self, Token::Literal(_))
    }

    fn split(self) -> (bool, Token) {
        self.apply(
            |value| {
                if value >= 10 {
                    (
                        true,
                        Token::new_pair(Token::Literal(value / 2), Token::Literal((value + 1) / 2)),
                    )
                } else {
                    (false, Token::Literal(value))
                }
            },
            |a, b| {
                let (af, a) = a.split();
                if af {
                    (true, Token::new_pair(a, b))
                } else {
                    let (bf, b) = b.split();
                    (bf, Token::new_pair(a, b))
                }
            },
        )
    }

    fn explode(self, depth: usize) -> (bool, Option<Token>, Token, Option<Token>) {
        if let Token::Pair(bx) = self {
            let (a, b) = *bx;
            if depth == 4 {
                assert!(a.is_literal());
                assert!(b.is_literal());
                (true, Some(a), Token::Literal(0), Some(b))
            } else {
                let (af, al, ac, ar) = a.explode(depth + 1);
                if let Some(value) = ar {
                    (af, al, ac + b.ladd(value), None)
                } else if !af {
                    let (bf, bl, bc, br) = b.explode(depth + 1);
                    if let Some(value) = bl {
                        (af || bf, al, ac.radd(value) + bc, br)
                    } else {
                        (af || bf, al, ac + bc, br)
                    }
                } else {
                    (af, al, ac + b, None)
                }
            }
        } else {
            (false, None, self, None)
        }
    }

    fn ladd(self, other: Token) -> Token {
        if let Token::Literal(other_value) = other {
            match self {
                Token::Literal(value) => Token::Literal(value + other_value),
                Token::Pair(bx) => {
                    let (a, b) = *bx;
                    Token::new_pair(a.ladd(other), b)
                }
            }
        } else {
            unreachable!();
        }
    }

    fn radd(self, other: Token) -> Token {
        if let Token::Literal(other_value) = other {
            match self {
                Token::Literal(value) => Token::Literal(value + other_value),
                Token::Pair(bx) => {
                    let (a, b) = *bx;
                    Token::new_pair(a, b.radd(other))
                }
            }
        } else {
            unreachable!();
        }
    }

    fn reduce(self) -> Token {
        let mut token = self;
        loop {
            let (flag, _, next, _) = token.explode(0);
            if !flag {
                let (flag, next) = next.split();
                token = next;
                if !flag {
                    return token;
                }
            } else {
                token = next;
            }
        }
    }

    fn magnitude(self) -> usize {
        self.apply(
            |value| value as usize,
            |a, b| 3 * a.magnitude() + 2 * b.magnitude(),
        )
    }
}

impl std::ops::Add for Token {
    type Output = Token;
    fn add(self, rhs: Self) -> Token {
        Token::Pair(Box::new((self, rhs)))
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Token::Literal(value) => {
                if let Token::Literal(other_value) = other {
                    *value == *other_value
                } else {
                    false
                }
            }
            Token::Pair(bx) => {
                if let Token::Pair(other_bx) = other {
                    let (a1, b1) = &**bx;
                    let (a2, b2) = &**other_bx;
                    (*a1 == *a2) && (*b1 == *b2)
                } else {
                    false
                }
            }
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut result = String::new();
        match self {
            Token::Literal(value) => result.push_str(&format!("{}", value)),
            Token::Pair(bx) => {
                let (a, b) = &**bx;
                result.push_str(&format!("[{},{}]", a, b))
            }
        }
        write!(f, "{}", result)
    }
}

impl Solver<&str> for Day18 {
    fn part1(data: &str) -> usize {
        let mut lines = data.lines();
        let mut token = Token::parse(lines.next().unwrap());
        for line in lines {
            token = (token + Token::parse(line)).reduce();
        }
        token.magnitude()
    }

    fn part2(data: &str) -> usize {
        let mut max = 0;
        for (n, t1) in data.lines().enumerate().skip(1) {
            for t2 in data.lines().take(n) {
                max = std::cmp::max(
                    max,
                    (Token::parse(t1) + Token::parse(t2)).reduce().magnitude(),
                );
                max = std::cmp::max(
                    max,
                    (Token::parse(t2) + Token::parse(t1)).reduce().magnitude(),
                );
            }
        }
        max
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
";

    #[test]
    fn test_add1() {
        let a = Token::parse("[1,2]");
        let b = Token::parse("[[3,4],5]");
        let c = Token::parse("[[1,2],[[3,4],5]]");
        assert_eq!(a + b, c);
    }

    #[test]
    fn test_explode() {
        assert_eq!(
            Token::parse("[[[[[9,8],1],2],3],4]").explode(0).2,
            Token::parse("[[[[0,9],2],3],4]")
        );
        assert_eq!(
            Token::parse("[7,[6,[5,[4,[3,2]]]]]").explode(0).2,
            Token::parse("[7,[6,[5,[7,0]]]]")
        );
        assert_eq!(
            Token::parse("[[6,[5,[4,[3,2]]]],1]").explode(0).2,
            Token::parse("[[6,[5,[7,0]]],3]")
        );
        assert_eq!(
            Token::parse("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
                .explode(0)
                .2,
            Token::parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
        );
        assert_eq!(
            Token::parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
                .explode(0)
                .2,
            Token::parse("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
        );
    }

    #[test]
    fn test_split() {
        assert_eq!(Token::Literal(10).split().1, Token::parse("[5,5]"));
        assert_eq!(Token::Literal(11).split().1, Token::parse("[5,6]"));
        assert_eq!(Token::Literal(12).split().1, Token::parse("[6,6]"));
    }

    #[test]
    fn test_reduce() {
        let a = Token::parse("[[[[4,3],4],4],[7,[[8,4],9]]]");
        let b = Token::parse("[1,1]");
        let c = Token::parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]");
        assert_eq!((a + b).reduce(), c);
    }

    #[test]
    fn test_magnitude() {
        assert_eq!(
            Token::parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude(),
            3488
        );
    }

    #[test]
    fn test_part1() {
        assert_eq!(Day18::part1(DATA), 4140);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day18::part2(DATA), 3993);
    }
}
