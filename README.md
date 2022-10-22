My solutions for [Advent of Code](https://adventofcode.com/).

2022 in Rust. Previous years:

- [2019 in C++](https://github.com/dfm/adventofcode/tree/2019)
- [2020 in Haskell](https://github.com/dfm/adventofcode/tree/2020)
- [2021 in Rust](https://github.com/dfm/adventofcode/tree/2021)

It's not pretty, but sometimes it works(?):

[![Tests](https://github.com/dfm/adventofcode/workflows/Tests/badge.svg)](https://github.com/dfm/adventofcode/actions?query=workflow%3ATests)

## Usage notes

**Session key**: To automatically download the inputs for a specific day, you'll
need to have your AofC session key saved somewhere. To find it, log into [the
AoC website](https://adventofcode.com), and use the developer tools to look at
the cookies to find your session key. Then save that key to a file called `.aoc`
in the root of this repo, or as the `AOC_SESSION_KEY` environment variable.

**Running a day's solve**:

Once you have your session key set, you can run (for example) the problem for
Dec 1, 2022, using the following command:

```bash
cargo run -p aoc-2022-1
```

This will attempt to download any input dataset that hasn't been previously
accessed.

**Setting up a new day**:
To copy the template and boilerplate for a new day, run

```bash
cargo run DAY_NUMBER
```

**Testing the code**:
To run all the unit tests, execute:

```bash
cargo test --all
```

To run the tests for just a single day (e.g. Dec 1, 2022), run:

```bash
cargo test -p aoc-2022-1
```
