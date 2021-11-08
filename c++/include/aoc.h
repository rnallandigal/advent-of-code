#ifndef AOC_H
#define AOC_H

#define AOC_SOLN_DECL(name)							\
	namespace name {								\
		std::string part1(std::string const & in);	\
		std::string part2(std::string const & in);	\
	}

AOC_SOLN_DECL(aoc2018::day01)
AOC_SOLN_DECL(aoc2018::day02)
AOC_SOLN_DECL(aoc2018::day03)
AOC_SOLN_DECL(aoc2018::day04)

AOC_SOLN_DECL(aoc2020::day01)
AOC_SOLN_DECL(aoc2020::day02)
AOC_SOLN_DECL(aoc2020::day03)
AOC_SOLN_DECL(aoc2020::day04)
AOC_SOLN_DECL(aoc2020::day05)
AOC_SOLN_DECL(aoc2020::day06)
AOC_SOLN_DECL(aoc2020::day07)
AOC_SOLN_DECL(aoc2020::day08)
AOC_SOLN_DECL(aoc2020::day09)
AOC_SOLN_DECL(aoc2020::day10)
AOC_SOLN_DECL(aoc2020::day11)
AOC_SOLN_DECL(aoc2020::day12)
AOC_SOLN_DECL(aoc2020::day13)
AOC_SOLN_DECL(aoc2020::day14)
AOC_SOLN_DECL(aoc2020::day15)
AOC_SOLN_DECL(aoc2020::day16)
AOC_SOLN_DECL(aoc2020::day17)
AOC_SOLN_DECL(aoc2020::day18)
AOC_SOLN_DECL(aoc2020::day19)
AOC_SOLN_DECL(aoc2020::day20)
AOC_SOLN_DECL(aoc2020::day21)
AOC_SOLN_DECL(aoc2020::day22)
AOC_SOLN_DECL(aoc2020::day23)
AOC_SOLN_DECL(aoc2020::day24)
namespace aoc2020::day25 {
	std::string part1(std::string const & in);
}

#endif	// AOC_H
