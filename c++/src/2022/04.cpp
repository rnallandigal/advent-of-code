#include <string>
#include <regex>
#include <functional>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day04 {

using contains_fn_t = std::function<bool(int, int, int, int)>;
int solve(std::string const & in, contains_fn_t const & contains) {
	static std::regex line_regex("(\\d+)-(\\d+),(\\d+)-(\\d+)");

	int count = 0;
	std::smatch matches;
	for(auto line : aoc::split(in, "\n")) {
		if(!std::regex_match(line, matches, line_regex)) continue;
		count += contains(
			std::stoi(matches[1].str()),
			std::stoi(matches[2].str()),
			std::stoi(matches[3].str()),
			std::stoi(matches[4].str())
		);
	};
	return count;
}

std::string part1(std::string const & in) {
	return fmt::format("{}", solve(in, [](int a, int b, int c, int d) {
		return (aoc::between(a, b, c) && aoc::between(a, b, d)) ||
			(aoc::between(c, d, a) && aoc::between(c, d, b));
	}));
}

std::string part2(std::string const & in) {
	return fmt::format("{}", solve(in, [](int a, int b, int c, int d) {
		return aoc::between(a, b, c) || aoc::between(c, d, a);
	}));
}

}	// namespace aoc2022::day04
