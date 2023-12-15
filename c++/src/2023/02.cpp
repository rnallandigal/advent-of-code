#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <tuple>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2023::day02 {

/**
 *Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
*/

using input_t = std::vector<std::vector<std::vector<std::pair<int, std::string>>>>;

input_t get_input(std::string const & in) {
	input_t input;
	for(auto const & line : aoc::split(in, "\n")) {
		input.emplace_back();
		auto parts = aoc::split(line, ": ");
		for(auto const & shows : aoc::split(parts[1], "; ")) {
			input.back().emplace_back();
			for(auto const & cubes : aoc::split(shows, ", ")) {
				auto parts2 = aoc::split(cubes, " ");
				input.back().back().emplace_back(std::stoi(parts2[0]), parts2[1]);
				// fmt::print("{}, {}\n", input.back().back().back().first, input.back().back().back().second);
			}
			// fmt::print("\n");
		}
		// fmt::print("\n");
	}
	return input;
}

std::string part1(std::string const & in) {
	auto input = get_input(in);
	int ans = 0;
	for(int i = 0; i < (int)input.size(); i++) {
		auto & game = input[i];
		bool possible = true;
		for(auto const & show : game) {
			for(auto const & [num, type] : show) {
				if((num > 12 && type == "red") || (num > 13 && type == "green") || (num > 14 && type == "blue")) {
					possible = false;
				}
			}
		}
		if(possible) ans += (i + 1);
	}
	return fmt::format("{}", ans);
}

std::string part2(std::string const & in) {
	auto input = get_input(in);
	int ans = 0;
	for(int i = 0; i < (int)input.size(); i++) {
		auto & game = input[i];
		int r = 0, g = 0, b = 0;
		for(auto const & show : game) {
			for(auto const & [num, type] : show) {
				if(type == "red") r = std::max(num, r);
				if(type == "green") g = std::max(num, g);
				if(type == "blue") b = std::max(num, b);
			}
		}
		ans += (r * g * b);
	}
	return fmt::format("{}", ans);
}

}	// namespace aoc2023::day02
