#include <vector>
#include <string>
#include <regex>
#include <unordered_map>
#include <unordered_set>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day07 {

using bag_desc_t = std::tuple<int, std::string>;
using input_t = std::unordered_map<std::string, std::vector<bag_desc_t>>;
using graph_t = std::unordered_map<std::string, std::unordered_set<std::string>>;

input_t input(std::string const & in) {
	input_t neighbors;

	std::regex line_regex("(.*) bags contain (.*)");
	std::regex bag_regex("(\\d+) (.*) bags?");
	std::smatch matches;
	for(auto line : aoc::split(in, "\n")) {
		if(!std::regex_match(line, matches, line_regex)) continue;
		std::string bag_color = matches[1].str();
		std::string bags_text = matches[2].str();
		bags_text.erase(bags_text.size() - 1);

		if(bags_text == "no other bags") {
			neighbors.emplace(bag_color, std::vector<bag_desc_t>{});
		} else {
			std::vector<bag_desc_t> descs;
			for(auto bag_desc : aoc::split(bags_text, ", ")) {
				if(!std::regex_match(bag_desc, matches, bag_regex)) continue;
				descs.emplace_back(std::stoi(matches[1].str()), matches[2].str());
			}
			neighbors.emplace(bag_color, descs);
		}
	}
	return neighbors;
}

void dfs(graph_t const & graph, std::unordered_set<std::string> & visited, std::string const & bag) {
	if(visited.count(bag)) return;

	visited.emplace(bag);
	if(auto it = graph.find(bag); it != graph.end()) {
		for(auto parent : it->second) {
			dfs(graph, visited, parent);
		}
	}
}

int count_bags(input_t const & g, std::string const & bag) {
	int ret = 1;
	if(auto it = g.find(bag); it != g.end()) {
		for(auto const & [n, child] : it->second) {
			ret += n * count_bags(g, child);
		}
	}
	return ret;
}

std::string part1(std::string const & in) {
	input_t rules = input(in);

	// invert each edge in directed graph
	graph_t graph;
	for(auto [k, v] : rules) {
		for(auto [_, b] : v) {
			graph[b].emplace(k);
		}
	}
	
	std::unordered_set<std::string> visited;
	dfs(graph, visited, "shiny gold");

	return fmt::format("{}", visited.size() - 1);
}

std::string part2(std::string const & in) {
	input_t g = input(in);
	return fmt::format("{}", count_bags(g, "shiny gold") - 1);
}

}	// namespace aoc2020::day07
