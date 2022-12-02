default: compile

build/Makefile:
	@cmake -B build .

build/src/aoc: build/Makefile
	@cmake --build build --parallel 2

compile: build/src/aoc

test: compile
	build/src/tests

run: compile
	build/src/aoc

new: compile
	build/src/aoc -s

clean:
	@rm -rf build

.PHONY: compile test run new clean
