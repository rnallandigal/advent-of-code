#include <vector>
#include <string>
#include <utility>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day03 {

std::vector<std::pair<int, int>> slopes = {
	{3, 1}, {1, 1}, {5, 1}, {7, 1}, {1, 2}
};

int countTrees(std::vector<std::string> const & area, std::pair<int, int> slope) {
	int h = (int)area.size(), w = (int)area[0].size(), num = 0;
	auto [dx, dy] = slope;
	for(int x = 0, y = 0; y < h; x += dx, y += dy) {
		num += (area[y][x % w] == '#');
	}
	return num;
}

std::string part1(std::string const & in) {
	auto area = aoc::split(in, "\n");
	return fmt::format("{}", countTrees(area, slopes[0]));
}

std::string part2(std::string const & in) {
	auto area = aoc::split(in, "\n");
	int ret = 1;
	for(int i = 0; i < (int)slopes.size(); i++) {
		ret *= countTrees(area, slopes[i]);
	}
	return fmt::format("{}", ret);
}

}	// namespace aoc2020::day03
