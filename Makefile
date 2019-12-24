
.PHONY: execs all

all: execs day1 day2a day2b day3a day3b day4a day4b day5a day5b

execs:
	mkdir -p build
	cd build;cmake ..;make -j4

day1: day1/* execs
	cat day1/day1.txt | build/bin/day1

day2a: day2/* execs
	cat day2/day2.txt | build/bin/day2a

day2b: day2/* execs
	cat day2/day2.txt | build/bin/day2b

day3a: day3/day3a.cpp day3/*.in execs
	echo "expect 6"
	cat day3/test1.in | build/bin/day3a
	echo "expect 159"
	cat day3/test2.in | build/bin/day3a
	echo "expect 135"
	cat day3/test3.in | build/bin/day3a
	cat day3/day3.txt | build/bin/day3a

day3b: day3/day3b.cpp day3/*.in execs
	echo "expect 30"
	cat day3/test1.in | build/bin/day3b
	echo "expect 610"
	cat day3/test2.in | build/bin/day3b
	echo "expect 410"
	cat day3/test3.in | build/bin/day3b
	cat day3/day3.txt | build/bin/day3b

day4a: day4/day4a.cpp execs
	build/bin/day4a

day4b: day4/day4b.cpp execs
	build/bin/day4b

day5a: day5/day5.cpp day5/*.hpp execs
	cat day5/day5a.txt | build/bin/day5

day5b: day5/day5.cpp day5/*.hpp execs
	cat day5/day5b.txt | build/bin/day5

day6a: day6/day6a.cpp execs
	cat day6/day6.txt | build/bin/day6a

day6b: day6/day6b.cpp execs
	cat day6/day6.txt | build/bin/day6b

day7a: day7/day7a.cpp execs
	cat day7/day7.txt | build/bin/day7a

day7b: day7/day7b.cpp execs
	cat day7/day7.txt | build/bin/day7b

day8a: day8/day8a.cpp execs
	build/bin/day8a day8/day8.txt

day8b: day8/day8b.cpp execs
	build/bin/day8b day8/day8.txt

day9a: day9/day9a.cpp execs
	build/bin/day9a day9/day9.txt

day9b: day9/day9b.cpp execs
	build/bin/day9b day9/day9.txt

day10a: day10/day10a.cpp execs
	build/bin/day10a day10/day10.txt

day10b: day10/day10b.cpp execs
	build/bin/day10b day10/day10.txt

day11: day11/day11.cpp day11/*.hpp execs
	build/bin/day11 day11/day11.txt

day12a: day12/day12a.cpp day12/*.hpp execs
	build/bin/day12a day12/day12.txt

day12b: day12/day12b.cpp day12/*.hpp execs
	build/bin/day12b day12/day12.txt

day13a: day13/day13a.cpp day13/*.hpp execs
	build/bin/day13a day13/day13.txt

day13b: day13/day13b.cpp day13/*.hpp execs
	build/bin/day13b day13/day13.txt

day14: day14/day14.cpp execs
	build/bin/day14 day14/day14.txt

day15: day15/day15.cpp day15/*.hpp execs
	build/bin/day15 day15/day15.txt

day16a: day16/day16a.cpp execs
	cat day16/day16.txt | build/bin/day16a

day16b: day16/day16b.cpp execs
	cat day16/day16.txt | build/bin/day16b

day17: day17/day17.cpp day17/*.hpp execs
	build/bin/day17 day17/day17.txt

day18a: day18/day18.cpp execs
	build/bin/day18 day18/day18.txt

day18b: day18/day18.cpp execs
	build/bin/day18 day18/day18b.txt

day19: day19/day19.cpp day19/*.hpp execs
	build/bin/day19 day19/day19.txt

day20a: day20/day20a.cpp execs
	build/bin/day20a day20/day20.txt

day20b: day20/day20b.cpp execs
	# build/bin/day20b 7 day20/test3.txt
	build/bin/day20b 27 day20/day20.txt
