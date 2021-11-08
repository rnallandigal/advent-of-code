#include <string>
#include <vector>
#include <algorithm>
#include <fstream>
#include <ios>
#include <regex>
#include <unordered_map>
#include <unordered_set>
#include <stack>
#include <queue>

#include <fmt/format.h>

#include "utils.h"

namespace std {

size_t hash<tuple<int, int, int>>::operator()(
	tuple<int, int, int> const & t
) const {
	size_t seed = 3;
	seed ^= (std::get<0>(t) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
	seed ^= (std::get<1>(t) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
	seed ^= (std::get<2>(t) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
	return seed;
}

size_t hash<tuple<int, int, int, int>>::operator()(
	tuple<int, int, int, int> const & t
) const {
	size_t seed = 4;
	seed ^= (std::get<0>(t) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
	seed ^= (std::get<1>(t) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
	seed ^= (std::get<2>(t) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
	seed ^= (std::get<3>(t) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
	return seed;
}

}	// namespace std

namespace fmt {

format_parse_context::iterator base_formatter::parse(format_parse_context & ctx) {
	return std::find(ctx.begin(), ctx.end(), '}');
}

}	// namespace fmt

namespace aoc {

std::string read(std::string const & filename) {
	std::string buf;
	std::ifstream ifs(filename);

	ifs.seekg(0, std::ios::end);
	buf.resize(ifs.tellg());
	ifs.seekg(0);
	ifs.read(buf.data(), buf.size());

	return buf;
}

std::vector<std::string> split(std::string const & s, std::string delim) {
	std::vector<std::string> ret;

	std::string::size_type i = 0, j;
	while((j = s.find(delim, i)) != std::string::npos) {
		ret.emplace_back(s.begin() + i, s.begin() + j);
		i = j + delim.size();
	}
	if(i < s.size()) ret.emplace_back(s.begin() + i, s.end());
	return ret;
}

bool between(int low, int high, int value) {
	return low <= value && value <= high;
}

bool rmatch(std::string const & text, std::string pattern) {
	std::regex exp(pattern);
	std::smatch matches;
	return std::regex_match(text, matches, exp);
}

int ipow(int x, int e) {
	if(e == 0) return 1;
	if(e == 1) return x;

	int a = ipow(x, e / 2);
	return a * a * (e % 2 ? x : 1);
}

// maximum bipartite matching - Edmonds-Karp
std::unordered_map<int, int> assign(
	std::unordered_map<int, std::unordered_set<int>> const & relation,
	int n,
	int m
) {
	// add source node connecting to domain, sink connecting to codomain
	std::vector<std::unordered_map<int, int>> graph(n + m + 2);
	for(auto const & [x, adj] : relation) {
		for(auto const & y : adj) {
			graph[x][n + y] = 1;
		}
	}
	for(int i = 0; i < n; i++) graph[n + m][i] = 1;
	for(int i = n; i < n + m; i++) graph[i][n + m + 1] = 1;

	while(true) {
		std::deque<int> q;
		std::vector<int> seen(n + m + 2, 0);
		std::vector<int> parent(n + m + 2, -1);

		// BFS to find an augmenting path from source to sink
		q.push_back(n + m); seen[n + m] = 1;
		while(!q.empty()) {
			int u = q.front(); q.pop_front();
			if(u == n + m + 1) break;

			for(auto [v, f] : graph[u]) {
				if(seen[v] || !f) continue;
				seen[v] = 1;
				q.push_back(v);
				parent[v] = u;
			}
		}

		int t = n + m + 1;
		if(parent[t] == -1) break;

		// construct residual graph by reversing flows along path
		do {
			int s = parent[t];
			graph[s][t] -= 1;
			graph[t][s] += 1;
			t = s;
		} while(t != n + m);
	}

	// convert residual graph to matching in original problem
	std::unordered_map<int, int> matching;
	for(int v = n; v < n + m; v++) {
		for(auto [u, f] : graph[v]) {
			if(!f) continue;
			matching[u] = v - n;
			break;
		}
	}
	return matching;
}

}	// namespace aoc
