#include <vector>
#include <string>
#include <unordered_set>
#include <tuple>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day19 {

using and_t = std::vector<int>;
using productions_t = std::vector<and_t>;
using grammar_t = std::vector<productions_t>;
using input_t = std::pair<grammar_t, std::vector<std::string>>;

input_t input(std::string const & in) {
	auto input_parts = aoc::split(in, "\n\n");

	grammar_t grammar(200);
	for(auto ruletext : aoc::split(input_parts[0], "\n")) {
		auto ruletext_parts = aoc::split(ruletext, ": ");
		int nonterminal = std::stoi(ruletext_parts[0]);

		productions_t productions;
		for(auto productiontext : aoc::split(ruletext_parts[1], " | ")) {
			if(productiontext.starts_with('\"')) {
				// 'a' -> -1, 'b' -> -2, ...
				productions.emplace_back(1, (int)'a' - productiontext[1] - 1);
			} else {
				productions.emplace_back();
				for(auto nonterminaltext : aoc::split(productiontext, " ")) {
					productions.back().push_back(std::stoi(nonterminaltext));
				}
			}
		}
		grammar[nonterminal] = productions;
	}
	return { grammar, aoc::split(input_parts[1], "\n") };
}

// unordered_set that keeps track of insertion order
class state_set {
	std::unordered_set<std::tuple<int, int, int, int>> uniques;
public:
	std::vector<std::tuple<int, int, int, int>> states;
	void add(int rule, int production, int dot, int start) {
		auto [it, b] = uniques.emplace(rule, production, dot, start);
		if(b) states.emplace_back(rule, production, dot, start);
	}
};
bool earley(grammar_t const & grammar, std::string const & tokens) {
	int n = (int)tokens.size();
	std::vector<state_set> S(n + 2);

	for(int i = 0; i < (int)grammar[0].size(); i++) S[0].add(0, i, 0, 0);
	for(int k = 0; k <= n; k++) {
		for(int i = 0; i < (int)S[k].states.size(); i++) {
			auto [rule, production, dot, start] = S[k].states[i];
			if(dot < (int)grammar[rule][production].size()) {
				int expected = grammar[rule][production][dot];
				if(expected >= 0) {
					// prediction
					for(int j = 0; j < (int)grammar[expected].size(); j++) {
						S[k].add(expected, j, 0, k);
					}
				} else {
					// scan
					char terminal = 'a' - (char)expected - 1;
					if(terminal == tokens[k]) {
						S[k + 1].add(rule, production, dot + 1, start);
					}
				}
			} else {
				// completion
				for(int j = 0; j < (int)S[start].states.size(); j++) {
					auto [prule, pproduction, pdot, pstart] = S[start].states[j];
					if(pdot < (int)grammar[prule][pproduction].size() &&
						grammar[prule][pproduction][pdot] == rule
					) {
						S[k].add(prule, pproduction, pdot + 1, pstart);
					}
				}
			}
		}
	}

	for(auto [rule, production, dot, start] : S[n].states) {
		if(	start == 0 &&
			rule == 0 &&
			dot >= (int)grammar[rule][production].size()
		) return true;
	}
	return false;
}

std::string part1(std::string const & in) {
	auto [grammar, tests] = input(in);

	int matches = 0;
	for(auto test : tests)
		matches += earley(grammar, test);

	return fmt::format("{}", matches);
}

std::string part2(std::string const & in) {
	auto [grammar, tests] = input(in);

	grammar[8] = { {42}, {42, 8} };
	grammar[11] = { {42, 31}, {42, 11, 31} };

	int matches = 0;
	for(auto test : tests)
		matches += earley(grammar, test);

	return fmt::format("{}", matches);
}

}	// namespace aoc2020::day19
