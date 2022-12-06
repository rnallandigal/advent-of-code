#include <vector>
#include <string>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day06 {

int solve(std::string const & in, int length) {
	std::vector<int> seen(26);
	int l = 0, r = 0;
	while(r < (int)in.size() && r - l < length) {
		if(seen[in[r] - 'a']) {
			while(in[l] != in[r]) seen[in[l++] - 'a'] = 0;
			seen[in[l++] - 'a'] = 0;
		}
		seen[in[r++] - 'a'] = 1;
	}
	return r;
}

std::string part1(std::string const & in) {
	return fmt::format("{}", solve(in, 4));
}

std::string part2(std::string const & in) {
	return fmt::format("{}", solve(in, 14));
}

}	// namespace aoc2022::day06
