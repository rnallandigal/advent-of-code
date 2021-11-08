#include <vector>
#include <string>
#include <algorithm>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day05 {

std::vector<int> input(std::string const & in) {
	std::vector<int> seats;
	for(auto s : aoc::split(in, "\n")) {
		int seat = 0;
		for(int i = 0, p = 512; i < (int)s.size(); i++, p /= 2) {
			seat += (p * (s[i] == 'B' || s[i] == 'R'));
		}
		seats.push_back(seat);
	}
	return seats;
}

std::string part1(std::string const & in) {
	auto seats = input(in);
	return fmt::format("{}", *std::max_element(seats.begin(), seats.end()));
}

std::string part2(std::string const & in) {
	auto seats = input(in);
	std::sort(seats.begin(), seats.end());

	for(int i = 1; i < (int)seats.size() - 1; i++) {
		if(seats[i - 1] == seats[i] - 2)
			return fmt::format("{}", seats[i] - 1);
	}
	return "";
}

}	// namespace aoc2020::day05
