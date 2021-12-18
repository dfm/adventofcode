default: all

all: format-check lint test run

clean:
	@cargo clean

build:
	@cargo build

test:
	@cargo test --all

format:
	@rustup component add rustfmt 2> /dev/null
	@cargo fmt --all

format-check:
	@rustup component add rustfmt 2> /dev/null
	@cargo fmt --all -- --check

lint:
	@rustup component add clippy 2> /dev/null
	@cargo clippy -- -D warnings

bench:
	@cargo bench

run:
	@cargo run

new:
	@cargo run new
	@cargo run download

.PHONY: all clean build test format format-check lint bench run new
