#include <vector>
#include <string>
#include <functional>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day02 {

int solve(std::string const & in, std::function<int(int, int)> score) {
	int total = 0;
	for(auto line : aoc::split(in, "\n")) {
		total += score(line[0] - 'A', line[2] - 'X');
	}
	return total;
}

std::string part1(std::string const & in) {
	return fmt::format("{}", solve(in, [](int p1, int p2) {
		return 1 + p2 + 3 * ((p2 - p1 + 4) % 3);
	}));
}

std::string part2(std::string const & in) {
	return fmt::format("{}", solve(in, [](int p1, int p2) {
		return 1 + ((p1 + p2 + 2) % 3) + 3 * p2;
	}));
}

}	// namespace aoc2022::day02
