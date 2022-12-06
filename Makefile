default: run-all

build/Makefile:
	@cmake -B build .

compile: build/Makefile
	@cmake --build build --parallel 4

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
