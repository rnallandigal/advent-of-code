#include <vector>
#include <string>
#include <utility>
#include <algorithm>
#include <functional>
#include <regex>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day04 {

using passport_t = std::unordered_map<std::string, std::string>;
std::vector<passport_t> input(std::string const & in) {
	std::vector<passport_t> passports;
	for(auto group : aoc::split(in, "\n\n")) {
		passport_t pass;
		for(auto lines : aoc::split(group, "\n")) {
			for(auto field : aoc::split(lines, " ")) {
				std::vector<std::string> kv = aoc::split(field, ":");
				pass.emplace(kv[0], kv[1]);
			}
		}
		passports.emplace_back(pass);
	}
	return passports;
}

bool between_str(int low, int high, std::string const & str) {
	int val = 0, i = 0, n = str.size();
	for(; i < n && std::isdigit(str[i]); i++) {
		val = 10 * val + (str[i] - '0');
	}
	return n > 0 && i == n && aoc::between(low, high, val);
}

bool validate_height(std::string const & s) {
	if(s.ends_with("cm"))
		return between_str(150, 193, s.substr(0, s.size() - 2));
	else if(s.ends_with("in"))
		return between_str(59, 76, s.substr(0, s.size() - 2));
	else return false;
}

std::vector<std::string> fields = {"byr","iyr","eyr","hgt","hcl","ecl","pid"};

std::string part1(std::string const & in) {
	int valid = 0;
	for(auto passport : input(in)) {
		valid += std::all_of(fields.begin(), fields.end(), [&](auto const & key) {
			return passport.find(key) != passport.end();
		});
	}
	return fmt::format("{}", valid);
}

std::string part2(std::string const & in) {
	int valid = 0;
	std::regex hcl("#[0-9a-f]{6}"), ecl("amb|blu|brn|gry|grn|hzl|oth"), pid("\\d{9}");
	std::smatch matches;
	for(auto passport : input(in)) {
		valid += std::all_of(fields.begin(), fields.end(), [&](auto const & key) {
			auto it = passport.find(key);
			if(it == passport.end()) return false;

			std::string & val = it->second;
			if(key == "byr") return between_str(1920, 2002, val);
			if(key == "iyr") return between_str(2010, 2020, val);
			if(key == "eyr") return between_str(2020, 2030, val);
			if(key == "hgt") return validate_height(val);
			if(key == "hcl") return std::regex_match(val, matches, hcl);
			if(key == "ecl") return std::regex_match(val, matches, ecl);
			if(key == "pid") return std::regex_match(val, matches, pid);
			return true;
		});
	}
	return fmt::format("{}", valid);
}

}	// namespace aoc2020::day04
