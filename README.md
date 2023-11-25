My solutions for [Advent of Code](https://adventofcode.com/).

2023 in Rust. Previous years:

- [2019 in C++11](https://github.com/dfm/adventofcode/tree/2019)
- [2020 in Haskell](https://github.com/dfm/adventofcode/tree/2020)
- [2021 in Rust](https://github.com/dfm/adventofcode/tree/2021)
- [2022 in C++20](https://github.com/dfm/adventofcode/tree/2022)

It's not pretty, but sometimes it works(?):

[![Tests](https://github.com/dfm/adventofcode/workflows/Tests/badge.svg)](https://github.com/dfm/adventofcode/actions?query=workflow%3ATests)

## Project Layout

The solutions for each day live in `aoc/src/y2023/dDD.rs` where `DD` is the
relevant day of the month. To implement a new solution, create a new file for
the appropriate day, and it will be automatically discovered by
[MAGIC](https://github.com/dfm/adventofcode/tree/9a3c55aeac3c67f6008f702b0893b80a36b60caa/aoc-derive).
This can be automated by running:

```bash
make new
```

or 

```bash
cargo run -- new
```

You can specify the day to create using:

```bash
cargo run -- new --day 12
```

## Usage Notes

**Session key:** To automatically download the inputs for a specific day, you'll
need to have your AofC session key saved somewhere. To find it, log into [the
AoC website](https://adventofcode.com), and use the developer tools to look at
the cookies to find your session key. Then save that key to a file called
`~/.config/aoc/key`. Then, the data files will be downloaded into
`~/.cache/aoc/2023` as text files.

To run today's solution, use:

```bash
make
```

which expands into:

```bash
cargo run -- --today
```

To run all the checks and solutions, use:

```bash
make all
```

## License

Copyright 2019-2023 Daniel Foreman-Mackey

Licensed under the [Apache License](/LICENSE).
