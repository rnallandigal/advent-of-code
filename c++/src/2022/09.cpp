#include <vector>
#include <string>
#include <unordered_set>
#include <utility>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day09 {

int solve(std::string const & in, int rope_length) {
	std::unordered_set<std::pair<int, int>> tail_positions;
	std::vector<int> py(rope_length), px(rope_length);
	for(auto line : aoc::split(in, "\n")) {
		char dir = line[0];
		for(int steps = std::stoi(line.substr(2)); steps; steps--) {
			if(dir == 'U')		py.front()--;
			else if(dir == 'D')	py.front()++;
			else if(dir == 'L')	px.front()--;
			else if(dir == 'R')	px.front()++;

			for(int i = 1; i < rope_length; i++) {
				int y_diff = py[i - 1] - py[i], x_diff = px[i - 1] - px[i];
				if(std::max(std::abs(y_diff), std::abs(x_diff)) > 1) {
					py[i] += y_diff ? y_diff / std::abs(y_diff) : 0;
					px[i] += x_diff ? x_diff / std::abs(x_diff) : 0;
				}
			}
			tail_positions.emplace(py.back(), px.back());
		}
	}
	return tail_positions.size();
}

std::string part1(std::string const & in) {
	return fmt::format("{}", solve(in, 2));
}

std::string part2(std::string const & in) {
	return fmt::format("{}", solve(in, 10));
}

}	// namespace aoc2022::day09
