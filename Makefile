default: run-all

build/Makefile:
	@cmake -B build .

build/src/aoc: build/Makefile src/*/day*.cpp
	@cmake --build build --parallel 2

compile: build/src/aoc

test: compile
	build/src/tests

unit-test: compile
	@ctest --output-on-failure --test-dir build

run: compile
	build/src/aoc

run-all: test
	build/src/aoc --year 2022

new: compile
	build/src/aoc --new

clean:
	@rm -rf build

.PHONY: compile test unit-test run run-all new clean
