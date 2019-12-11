
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
