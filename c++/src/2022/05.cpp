#include <vector>
#include <string>
#include <tuple>
#include <regex>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day05 {

using crates_t = std::vector<std::string>;
using insts_t = std::vector<std::tuple<int, int, int>>;
using input_t = std::pair<crates_t, insts_t>;

input_t input(std::string const & in) {
	auto parts = aoc::split(in, "\n\n");

	// crates
	auto cratelines = aoc::split(parts[0], "\n");
	crates_t crates((cratelines[0].size() + 2) / 4);
	for(int i = cratelines.size() - 2; i >= 0; i--) {
		for(int j = 1; j < (int)cratelines[i].size(); j += 4) {
			if(cratelines[i][j] != ' ')
				crates[(j - 1) / 4].push_back(cratelines[i][j]);
		}
	}

	// insts
	insts_t insts;
	std::regex inst_regex("move (\\d+) from (\\d+) to (\\d+)");
	std::smatch matches;
	for(auto line : aoc::split(parts[1], "\n")) {
		if(!regex_match(line, matches, inst_regex)) continue;
		insts.emplace_back(
			std::stoi(matches[1].str()),
			std::stoi(matches[2].str()) - 1,
			std::stoi(matches[3].str()) - 1
		);
	}
	return { crates, insts };
}

std::string solve(std::string const & in, bool reverse) {
	auto [ crates, insts ] = input(in);

	for(auto const & [n, i, j] : insts) {
		if(reverse) {
			crates[j].append(crates[i].rbegin(), crates[i].rbegin() + n);
		} else {
			crates[j].append(crates[i].end() - n, crates[i].end());
		}
		crates[i].resize(crates[i].size() - n);
	}

	std::string ans;
	for(auto const & stack : crates) {
		ans += stack.back();
	}
	return ans;
}

std::string part1(std::string const & in) {
	return fmt::format("{}", solve(in, true));
}

std::string part2(std::string const & in) {
	return fmt::format("{}", solve(in, false));
}

}	// namespace aoc2022::day05
