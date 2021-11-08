#include <vector>
#include <string>
#include <tuple>
#include <regex>
#include <algorithm>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day02 {

using entry_t = std::tuple<int, int, char, std::string>;
std::vector<entry_t> input(std::string const & in) {
	std::vector<entry_t> entries;

	std::regex entryRegex("(\\d+)-(\\d+) (.): (.*)");
	std::smatch matches;
	for(auto line : aoc::split(in, "\n")) {
		if(std::regex_match(line, matches, entryRegex)) {
			entries.emplace_back(
				std::stoi(matches[1].str()),
				std::stoi(matches[2].str()),
				matches[3].str()[0],
				matches[4].str()
			);
		}
	}
	return entries;
}

std::string part1(std::string const & in) {
	int valid = 0;
	for(auto [low, high, ch, text] : input(in)) {
		int freq = std::count(text.begin(), text.end(), ch);
		valid += aoc::between(low, high, freq);
	}
	return fmt::format("{}", valid);
}

std::string part2(std::string const & in) {
	int valid = 0;
	for(auto [i, j, ch, text] : input(in)) {
		valid += ((text[i - 1] == ch) ^ (text[j - 1] == ch));
	}
	return fmt::format("{}", valid);
}

}	// namespace aoc2020::day02
