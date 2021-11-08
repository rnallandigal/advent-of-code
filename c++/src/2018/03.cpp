#include <string>
#include <vector>
#include <tuple>
#include <regex>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2018::day03 {

using claim_t = std::tuple<int, int, int, int, int>;
std::vector<claim_t> input(std::string const & in) {
	std::vector<claim_t> claims;
	std::regex lineRegex("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)");
	std::smatch matches;

	for(auto line : aoc::split(in, "\n")) {
		if(regex_match(line, matches, lineRegex)) {
			claims.emplace_back(
				std::stoi(matches[1]),
				std::stoi(matches[2]),
				std::stoi(matches[3]),
				std::stoi(matches[4]),
				std::stoi(matches[5])
			);
		}
	}
	return claims;
}

std::vector<std::vector<int>> mark_claims(std::vector<claim_t> const & claims) {
	std::vector<std::vector<int>> area(2000, std::vector<int>(2000, 0));
	for(auto [_, x, y, w, h] : claims) {
		for(int i = y; i < y + h; i++) {
			for(int j = x; j < x + w; j++) {
				area[i][j]++;
			}
		}
	}
	return area;
}

std::string part1(std::string const & in) {
	auto claims = input(in);
	auto area = mark_claims(claims);

	int overlaps = 0;
	for(auto const & row : area) {
		for(int cell : row) {
			overlaps += (cell > 1);
		}
	}
	return fmt::format("{}", overlaps);
}

std::string part2(std::string const & in) {
	auto claims = input(in);
	auto area = mark_claims(claims);

	for(auto [id, x, y, w, h] : claims) {
		bool overlaps = false;
		for(int i = y; i < y + h && !overlaps; i++) {
			for(int j = x; j < x + w && !overlaps; j++) {
				overlaps |= (area[i][j] > 1);
			}
		}
		if(!overlaps) return fmt::format("{}", id);
	}
	return "";
}

}	// namespace aoc2018::day03
