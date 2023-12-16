default: today

.PHONY: all clean test run today format lint

all: format lint test run

clean:
	cargo clean -p aoc

new:
	cargo run -- new

test:
	cargo test --all

run:
	cargo run

today:
	cargo run -- --today

format:	
	@rustup component add rustfmt 2> /dev/null
	cargo fmt --all

lint:
	@rustup component add clippy 2> /dev/null
	cargo clippy -- -D warnings
