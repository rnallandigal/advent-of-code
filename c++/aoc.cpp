#include <string>
#include <map>
#include <stdexcept>
#include <tuple>

#include <fmt/format.h>
#include <docopt/docopt.h>

#include "aoc.h"
#include "utils.h"

const char COMMAND[] =
R"(Advent Of Code Runner

Usage:  aoc --year YEAR --day DAY --part PART
        aoc --all

Options:
    -y YEAR, --year YEAR    The year of the solution to run
    -d DAY, --day DAY       The day of the solution to run
    -p PART, --part PART    The part of the solution to run
    -a, --all               Run all solutions
    -h, --help              Show this screen
)";

std::map<std::string, std::function<std::string(std::string)>> solutions = {
	{ "2018.01.1", aoc2018::day01::part1 },
	{ "2018.01.2", aoc2018::day01::part2 },
	{ "2018.02.1", aoc2018::day02::part1 },
	{ "2018.02.2", aoc2018::day02::part2 },
	{ "2018.03.1", aoc2018::day03::part1 },
	{ "2018.03.2", aoc2018::day03::part2 },
	{ "2018.04.1", aoc2018::day04::part1 },
	{ "2018.04.2", aoc2018::day04::part2 },

	{ "2020.01.1", aoc2020::day01::part1 },
	{ "2020.01.2", aoc2020::day01::part2 },
	{ "2020.02.1", aoc2020::day02::part1 },
	{ "2020.02.2", aoc2020::day02::part2 },
	{ "2020.03.1", aoc2020::day03::part1 },
	{ "2020.03.2", aoc2020::day03::part2 },
	{ "2020.04.1", aoc2020::day04::part1 },
	{ "2020.04.2", aoc2020::day04::part2 },
	{ "2020.05.1", aoc2020::day05::part1 },
	{ "2020.05.2", aoc2020::day05::part2 },
	{ "2020.06.1", aoc2020::day06::part1 },
	{ "2020.06.2", aoc2020::day06::part2 },
	{ "2020.07.1", aoc2020::day07::part1 },
	{ "2020.07.2", aoc2020::day07::part2 },
	{ "2020.08.1", aoc2020::day08::part1 },
	{ "2020.08.2", aoc2020::day08::part2 },
	{ "2020.09.1", aoc2020::day09::part1 },
	{ "2020.09.2", aoc2020::day09::part2 },
	{ "2020.10.1", aoc2020::day10::part1 },
	{ "2020.10.2", aoc2020::day10::part2 },
	{ "2020.11.1", aoc2020::day11::part1 },
	{ "2020.11.2", aoc2020::day11::part2 },
	{ "2020.12.1", aoc2020::day12::part1 },
	{ "2020.12.2", aoc2020::day12::part2 },
	{ "2020.13.1", aoc2020::day13::part1 },
	{ "2020.13.2", aoc2020::day13::part2 },
	{ "2020.14.1", aoc2020::day14::part1 },
	{ "2020.14.2", aoc2020::day14::part2 },
	{ "2020.15.1", aoc2020::day15::part1 },
	{ "2020.15.2", aoc2020::day15::part2 },
	{ "2020.16.1", aoc2020::day16::part1 },
	{ "2020.16.2", aoc2020::day16::part2 },
	{ "2020.17.1", aoc2020::day17::part1 },
	{ "2020.17.2", aoc2020::day17::part2 },
	{ "2020.18.1", aoc2020::day18::part1 },
	{ "2020.18.2", aoc2020::day18::part2 },
	{ "2020.19.1", aoc2020::day19::part1 },
	{ "2020.19.2", aoc2020::day19::part2 },
	{ "2020.20.1", aoc2020::day20::part1 },
	{ "2020.20.2", aoc2020::day20::part2 },
	{ "2020.21.1", aoc2020::day21::part1 },
	{ "2020.21.2", aoc2020::day21::part2 },
	{ "2020.22.1", aoc2020::day22::part1 },
	{ "2020.22.2", aoc2020::day22::part2 },
	{ "2020.23.1", aoc2020::day23::part1 },
	{ "2020.23.2", aoc2020::day23::part2 },
	{ "2020.24.1", aoc2020::day24::part1 },
	{ "2020.24.2", aoc2020::day24::part2 },
	{ "2020.25.1", aoc2020::day25::part1 },

	{ "2021.01.1", aoc2021::day01::part1 },
	{ "2021.01.2", aoc2021::day01::part2 },
	{ "2021.02.1", aoc2021::day02::part1 },
	{ "2021.02.2", aoc2021::day02::part2 },
	{ "2021.03.1", aoc2021::day03::part1 },
	{ "2021.03.2", aoc2021::day03::part2 },
	{ "2021.04.1", aoc2021::day04::part1 },
	{ "2021.04.2", aoc2021::day04::part2 },
	{ "2021.05.1", aoc2021::day05::part1 },
	{ "2021.05.2", aoc2021::day05::part2 },
	{ "2021.06.1", aoc2021::day06::part1 },
	{ "2021.06.2", aoc2021::day06::part2 },
	{ "2021.07.1", aoc2021::day07::part1 },
	{ "2021.07.2", aoc2021::day07::part2 },
	{ "2021.08.1", aoc2021::day08::part1 },
	{ "2021.08.2", aoc2021::day08::part2 },
	{ "2021.09.1", aoc2021::day09::part1 },
	{ "2021.09.2", aoc2021::day09::part2 },
	{ "2021.10.1", aoc2021::day10::part1 },
	{ "2021.10.2", aoc2021::day10::part2 },
	{ "2021.11.1", aoc2021::day11::part1 },
	{ "2021.11.2", aoc2021::day11::part2 },
	{ "2021.12.1", aoc2021::day12::part1 },
	{ "2021.12.2", aoc2021::day12::part2 },
	{ "2021.13.1", aoc2021::day13::part1 },
	{ "2021.13.2", aoc2021::day13::part2 },
	{ "2021.14.1", aoc2021::day14::part1 },
	{ "2021.14.2", aoc2021::day14::part2 },
	{ "2021.15.1", aoc2021::day15::part1 },
	{ "2021.15.2", aoc2021::day15::part2 },
	{ "2021.16.1", aoc2021::day16::part1 },
	{ "2021.16.2", aoc2021::day16::part2 },
	{ "2021.17.1", aoc2021::day17::part1 },
	{ "2021.17.2", aoc2021::day17::part2 },
	{ "2021.18.1", aoc2021::day18::part1 },
	{ "2021.18.2", aoc2021::day18::part2 },
	{ "2021.19.1", aoc2021::day19::part1 },
	{ "2021.19.2", aoc2021::day19::part2 },
	{ "2021.20.1", aoc2021::day20::part1 },
	{ "2021.20.2", aoc2021::day20::part2 },
	{ "2021.21.1", aoc2021::day21::part1 },
	{ "2021.21.2", aoc2021::day21::part2 },
	{ "2021.22.1", aoc2021::day22::part1 },
	{ "2021.22.2", aoc2021::day22::part2 },
	{ "2021.23.1", aoc2021::day23::part1 },
	{ "2021.23.2", aoc2021::day23::part2 },
	{ "2021.24.1", aoc2021::day24::part1 },
	{ "2021.24.2", aoc2021::day24::part2 },
	{ "2021.25.1", aoc2021::day25::part1 },

	{ "2022.01.1", aoc2022::day01::part1 },
	{ "2022.01.2", aoc2022::day01::part2 },
	{ "2022.02.1", aoc2022::day02::part1 },
	{ "2022.02.2", aoc2022::day02::part2 },
	{ "2022.03.1", aoc2022::day03::part1 },
	{ "2022.03.2", aoc2022::day03::part2 },
	{ "2022.04.1", aoc2022::day04::part1 },
	{ "2022.04.2", aoc2022::day04::part2 },
	{ "2022.05.1", aoc2022::day05::part1 },
	{ "2022.05.2", aoc2022::day05::part2 },
	{ "2022.06.1", aoc2022::day06::part1 },
	{ "2022.06.2", aoc2022::day06::part2 },
	{ "2022.07.1", aoc2022::day07::part1 },
	{ "2022.07.2", aoc2022::day07::part2 },
	{ "2022.08.1", aoc2022::day08::part1 },
	{ "2022.08.2", aoc2022::day08::part2 },
	{ "2022.09.1", aoc2022::day09::part1 },
	{ "2022.09.2", aoc2022::day09::part2 },
	{ "2022.10.1", aoc2022::day10::part1 },
	{ "2022.10.2", aoc2022::day10::part2 },
	{ "2022.11.1", aoc2022::day11::part1 },
	{ "2022.11.2", aoc2022::day11::part2 },
	{ "2022.12.1", aoc2022::day12::part1 },
	{ "2022.12.2", aoc2022::day12::part2 },
	{ "2022.13.1", aoc2022::day13::part1 },
	{ "2022.13.2", aoc2022::day13::part2 },
	{ "2022.14.1", aoc2022::day14::part1 },
	{ "2022.14.2", aoc2022::day14::part2 },
	{ "2022.15.1", aoc2022::day15::part1 },
	{ "2022.15.2", aoc2022::day15::part2 },
	{ "2022.16.1", aoc2022::day16::part1 },
	{ "2022.16.2", aoc2022::day16::part2 },
	{ "2022.17.1", aoc2022::day17::part1 },
	{ "2022.17.2", aoc2022::day17::part2 },
	{ "2022.18.1", aoc2022::day18::part1 },
	{ "2022.18.2", aoc2022::day18::part2 },
	{ "2022.19.1", aoc2022::day19::part1 },
	{ "2022.19.2", aoc2022::day19::part2 },
	{ "2022.20.1", aoc2022::day20::part1 },
	{ "2022.20.2", aoc2022::day20::part2 },
	{ "2022.21.1", aoc2022::day21::part1 },
	{ "2022.21.2", aoc2022::day21::part2 },
	{ "2022.22.1", aoc2022::day22::part1 },
	{ "2022.22.2", aoc2022::day22::part2 },
	{ "2022.23.1", aoc2022::day23::part1 },
	{ "2022.23.2", aoc2022::day23::part2 },
	{ "2022.24.1", aoc2022::day24::part1 },
	{ "2022.24.2", aoc2022::day24::part2 },
	{ "2022.25.1", aoc2022::day25::part1 }
};

int main(int argc, char const ** argv) {
    auto args = docopt::docopt(COMMAND, { argv + 1, argv + argc }, true, "");

	if(args["--all"].asBool()) {
		for(auto [k, soln] : solutions) {
			std::string root = k.substr(0, k.find_last_of('.'));
			std::string input = aoc::read(fmt::format("in/{}.in", root));
			fmt::print("{}: {}\n", k, soln(input));
		}
	} else {
		long year = args["--year"].asLong();
		long day = args["--day"].asLong();
		std::string part = args["--part"].asString();

		std::string filename = fmt::format("in/{}.{:0>2}.in", year, day);
		std::string solnname = fmt::format("{}.{:0>2}.{}", year, day, part);

		if(auto it = solutions.find(solnname); it != solutions.end()) {
			auto [_, soln] = *it;
			fmt::print("{}\n", soln(aoc::read(filename)));
		} else {
			throw std::runtime_error(fmt::format(
				"Cannot find solution corresponding to name: {}",
				solnname
			));
		}
	}
    return 0;
}
