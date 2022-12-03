default: compile

build/Makefile:
	@cmake -B build .

build/src/aoc: build/Makefile src/*/day*.cpp
	@cmake --build build --parallel 2

compile: build/src/aoc

test: compile
	build/src/tests

run: compile
	build/src/aoc

run-all: compile
	build/src/aoc -y 2022

new: compile
	build/src/aoc -s

clean:
	@rm -rf build

.PHONY: compile test run run-all new clean
