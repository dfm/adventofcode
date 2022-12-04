default: run-all

build/Makefile:
	@cmake -B build .

build/aoc: build/Makefile src/*/day*.cpp
	@cmake --build build --parallel 4

compile: build/aoc

test: compile
	build/aoctest

run: test
	build/aoc

run-all: test
	build/aoc --year 2022

new: compile
	build/aoc --new

clean:
	@rm -rf build

.PHONY: compile test unit-test run run-all new clean
