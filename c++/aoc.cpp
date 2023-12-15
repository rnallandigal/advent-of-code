#include <string>
#include <map>
#include <regex>

#include <fmt/format.h>
#include <docopt/docopt.h>

#include "aoc.h"
#include "utils.h"

const char COMMAND[] =
R"(Advent Of Code Runner

Usage:  aoc [FILTER]
        aoc --list

Options:
    FILTER          Select the problems to run using a regular expression, i.e. "2022.0[123]" [default: ""]
    -l, --list      List all available problems
    -h, --help      Show this screen
)";

std::map<std::string, std::function<std::string(std::string)>> problems = {
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
	{ "2022.25.1", aoc2022::day25::part1 },

	{ "2023.01.1", aoc2023::day01::part1 },
	{ "2023.01.2", aoc2023::day01::part2 },
	{ "2023.02.1", aoc2023::day02::part1 },
	{ "2023.02.2", aoc2023::day02::part2 },
	{ "2023.03.1", aoc2023::day03::part1 },
	{ "2023.03.2", aoc2023::day03::part2 },
	{ "2023.04.1", aoc2023::day04::part1 },
	{ "2023.04.2", aoc2023::day04::part2 },
	{ "2023.05.1", aoc2023::day05::part1 },
	{ "2023.05.2", aoc2023::day05::part2 },
	{ "2023.06.1", aoc2023::day06::part1 },
	{ "2023.06.2", aoc2023::day06::part2 },
	{ "2023.07.1", aoc2023::day07::part1 },
	{ "2023.07.2", aoc2023::day07::part2 },
	{ "2023.08.1", aoc2023::day08::part1 },
	{ "2023.08.2", aoc2023::day08::part2 },
	{ "2023.09.1", aoc2023::day09::part1 },
	{ "2023.09.2", aoc2023::day09::part2 },
	{ "2023.10.1", aoc2023::day10::part1 },
	{ "2023.10.2", aoc2023::day10::part2 },
	{ "2023.11.1", aoc2023::day11::part1 },
	{ "2023.11.2", aoc2023::day11::part2 },
	{ "2023.12.1", aoc2023::day12::part1 },
	{ "2023.12.2", aoc2023::day12::part2 },
	{ "2023.13.1", aoc2023::day13::part1 },
	{ "2023.13.2", aoc2023::day13::part2 },
	{ "2023.14.1", aoc2023::day14::part1 },
	{ "2023.14.2", aoc2023::day14::part2 },
	{ "2023.15.1", aoc2023::day15::part1 },
	{ "2023.15.2", aoc2023::day15::part2 },
	{ "2023.16.1", aoc2023::day16::part1 },
	{ "2023.16.2", aoc2023::day16::part2 },
	{ "2023.17.1", aoc2023::day17::part1 },
	{ "2023.17.2", aoc2023::day17::part2 },
	{ "2023.18.1", aoc2023::day18::part1 },
	{ "2023.18.2", aoc2023::day18::part2 },
	{ "2023.19.1", aoc2023::day19::part1 },
	{ "2023.19.2", aoc2023::day19::part2 },
	{ "2023.20.1", aoc2023::day20::part1 },
	{ "2023.20.2", aoc2023::day20::part2 },
	{ "2023.21.1", aoc2023::day21::part1 },
	{ "2023.21.2", aoc2023::day21::part2 },
	{ "2023.22.1", aoc2023::day22::part1 },
	{ "2023.22.2", aoc2023::day22::part2 },
	{ "2023.23.1", aoc2023::day23::part1 },
	{ "2023.23.2", aoc2023::day23::part2 },
	{ "2023.24.1", aoc2023::day24::part1 },
	{ "2023.24.2", aoc2023::day24::part2 },
	{ "2023.25.1", aoc2023::day25::part1 }
};

int main(int argc, char const ** argv) {
    auto args = docopt::docopt(COMMAND, { argv + 1, argv + argc }, true, "");

	if(args["--list"].asBool()) {
		for(auto [id, _] : problems) {
			fmt::print("{}\n", id);
		}
	} else {
		// docopt.cpp can't set a default value for positional arguments
		std::string filter_string
			= args["FILTER"].kind() == docopt::Kind::String
			? args["FILTER"].asString()
			: "";

		std::regex filter(filter_string);
		for(auto [id, impl] : problems) {
			if(!regex_search(id, filter)) continue;
			std::string inputfile = fmt::format("in/{}.in", id.substr(0, 7));
			fmt::print("{}: {}\n", id, impl(aoc::read(inputfile)));
		}
	}
    return 0;
}
