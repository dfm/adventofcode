My solutions for [Advent of Code](https://adventofcode.com/).

2022 in C++20. Previous years:

- [2019 in C++](https://github.com/dfm/adventofcode/tree/2019)
- [2020 in Haskell](https://github.com/dfm/adventofcode/tree/2020)
- [2021 in Rust](https://github.com/dfm/adventofcode/tree/2021)

It's not pretty, but sometimes it works(?):

[![Tests](https://github.com/dfm/adventofcode/workflows/Tests/badge.svg)](https://github.com/dfm/adventofcode/actions?query=workflow%3ATests)

## Project Layout

The infrastructure and solver scaffolding is implemented in `include/aoc` and
`src/aoc`, with unit tests in `tests`. The daily solvers are in `src/2022` with
tests implemented inline. A solver is implemented (following the code in `src/template.cpp.template`) as follows:

```cpp
AOC_IMPL(YEAR, DAY) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) { return 0; };
  static constexpr auto part2 = [](auto data) { return 0; };
};
```

where `parser` is a [lexy](https://github.com/foonathan/lexy) parser for the
input data, and `part1` and `part2` are functions accepting the parsed input.

Tests are implemented using the `AOC_TEST_CASE` macro, which expands into a
[doctest](https://github.com/doctest/doctest) `TEST_CASE`:

```cpp
AOC_TEST_CASE(EXPECTED_PART1_RESULT, EXPECTED_PART2_RESULT, R"(some
test
input
)")
```

## Usage Notes

**Session key:** To automatically download the inputs for a specific day, you'll
need to have your AofC session key saved somewhere. To find it, log into [the
AoC website](https://adventofcode.com), and use the developer tools to look at
the cookies to find your session key. Then save that key to a file called
`~/.config/aoc/key`.

**Downloading data:**
Once you have your session key set, download the inputs, run the tests, and run
with your inputs using

```bash
make
```

**Setting up a new day:**
To copy the template and boilerplate for a new day, run

```bash
make new
```

**Testing the code:**
To run the unit tests, execute

```bash
make test
```

**Running the code:**
To run the solver on your inputs, you can just run

```bash
make run
```

To run the solver on a specific day, use

```bash
build/src/aoc -y YEAR -d DAY
```

## Dependencies

Besides the C++20 standard library, I'm using the following external
dependencies (all included using CMake `FetchContent`):

- [lexy](https://github.com/foonathan/lexy) for parsing the input
- [doctest](https://github.com/doctest/doctest) for unit testing

To build the infrastructure, I'm also using the following dependencies:

- [CLI11](https://github.com/CLIUtils/CLI11) for building the command line
  interface
- [cpr](https://github.com/libcpr/cpr) for interacting with the AoC website
- [inja](https://github.com/pantor/inja) for formatting the daily template
- [fmt](https://github.com/fmtlib/fmt) for a backward-compatible `std::format`
  implementation, because my compiler doesn't support it

## License

Copyright 2019-2022 Daniel Foreman-Mackey

Licensed under the [Apache License](/LICENSE).
