#include <vector>
#include <string>
#include <regex>
#include <set>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day16 {

using rule_t = std::tuple<std::string, int, int, int, int>;
using ticket_t = std::vector<int>;
using spec_t = std::pair<std::vector<rule_t>, std::vector<ticket_t>>;

spec_t input(std::string const & in) {
	std::vector<std::vector<std::string>> sections;
	for(auto section : aoc::split(in, "\n\n")) {
		sections.push_back(aoc::split(section, "\n"));
	}
	
	// parse rules
	std::vector<rule_t> rules;
	std::regex rule("^(.*): (\\d+)-(\\d+) or (\\d+)-(\\d+)$");
	std::smatch matches;
	for(auto const & ruletext : sections[0]) {
		if(!std::regex_match(ruletext, matches, rule)) continue;
		rules.emplace_back(
			matches[1].str(),
			std::stoi(matches[2].str()),
			std::stoi(matches[3].str()),
			std::stoi(matches[4].str()),
			std::stoi(matches[5].str())
		);
	}

	// parse tickets
	std::vector<ticket_t> tickets;
	auto parse_ticket = [](std::string const & line) -> ticket_t {
		ticket_t ticket;
		for(auto field : aoc::split(line, ",")) {
			ticket.push_back(std::stoi(field));
		}
		return ticket;
	};

	tickets.push_back(parse_ticket(sections[1][1]));
	for(int i = 1; i < (int)sections[2].size(); i++) {
		tickets.push_back(parse_ticket(sections[2][i]));
	}
	return { rules, tickets };
}

std::pair<std::vector<ticket_t>, int> pick_valid(spec_t const & spec) {
	auto const & [rules, tickets] = spec;

	std::vector<int> invalid_values(1000, 1);
	for(auto [_, a, b, c, d] : rules) {
		for(int i = a; i <= b; i++) invalid_values[i] = 0;
		for(int i = c; i <= d; i++) invalid_values[i] = 0;
	}

	std::vector<ticket_t> valid_tickets;
	int error_rate = 0;
	for(int i = 0; i < (int)tickets.size(); i++) {
		bool invalid = false;
		for(int val : tickets[i]) {
			if(!invalid_values[val]) continue;
			error_rate += val;
			invalid = true;
		}
		if(!invalid) valid_tickets.push_back(tickets[i]);
	}
	return { valid_tickets, error_rate };
}

std::string part1(std::string const & in) {
	auto [_, error_rate] = pick_valid(input(in));
	return fmt::format("{}", error_rate);
}

std::string part2(std::string const & in) {
	auto spec    = input(in);
	auto rules   = std::get<0>(spec);
	auto tickets = std::get<0>(pick_valid(spec));

	int n = (int)rules.size(), m = (int)tickets.size();

	std::vector<std::vector<int>> check(n, std::vector<int>(1000, 0));
	for(int i = 0; i < n; i++) {
		auto [_, a, b, c, d] = rules[i];
		for(int j = a; j <= b; j++) check[i][j] = 1;
		for(int j = c; j <= d; j++) check[i][j] = 1;
	}

	std::unordered_map<int, std::unordered_set<int>> relation;
	for(int i = 0; i < n; i++) {
		for(int j = 0; j < n; j++) {
			bool all_match = true;
			for(int k = 0; all_match && k < m; k++) {
				all_match &= check[i][tickets[k][j]];
			}
			if(all_match) relation[i].insert(j);
		}
	}
	auto matching = aoc::assign(relation, n, n);

	uint64_t soln = 1;
	for(int i = 0; i < n; i++) {
		if(!std::get<0>(rules[i]).starts_with("departure")) continue;
		soln *= tickets[0][matching[i]];
	}
	return fmt::format("{}", soln);
}

}	// namespace aoc2020::day16
