#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <tuple>
#include <functional>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day08 {

using grid_t = std::vector<std::string>;
using order_t = std::vector<int>;
using iter_t = std::pair<int, int>;
using iterschemes_t = std::vector<std::pair<order_t, std::vector<iter_t>>>;
using boundscheck_t = std::function<bool(int const &, int const &)>;
using input_t = std::tuple<grid_t, iterschemes_t, int, int, boundscheck_t>;

input_t input(std::string const & in) {
	grid_t grid = aoc::split(in, "\n");
	int n = grid.size(), m = grid[0].size();
	iterschemes_t iterschemes = {
		{ { 0, 1 }, { {     0,  1 }, { 0    ,  1 } } },	// left to right
		{ { 0, 1 }, { {     0,  1 }, { m - 1, -1 } } },	// right to left
		{ { 1, 0 }, { {     0,  1 }, { 0    ,  1 } } },	// top to bottom
		{ { 1, 0 }, { { n - 1, -1 }, { 0    ,  1 } } }	// bottom to top
	};
	auto boundscheck = [=](int const & y, int const & x) {
		return aoc::between(0, n - 1, y) && aoc::between(0, m - 1, x);
	};
	return { grid, iterschemes, n, m, boundscheck};
}

std::string part1(std::string const & in) {
	auto [grid, iterschemes, n, m, boundscheck] = input(in);

	std::unordered_set<std::pair<int, int>> visible;
	for(auto const & [order, iter_vars] : iterschemes) {
		std::vector<int> iter(iter_vars.size());
		iter_t outer = iter_vars[order[0]], inner = iter_vars[order[1]];
		int & i = iter[order[0]], & j = iter[order[1]];
		int & y = iter[0], & x = iter[1];

		for(i = outer.first; boundscheck(y, x); i += outer.second) {
			char max_seen = '0' - 1;
			for(j = inner.first; boundscheck(y, x); j += inner.second) {
				if(grid[y][x] > max_seen) visible.emplace(y, x);
				max_seen = std::max(max_seen, grid[y][x]);
			}
			j = inner.first;
		}
	}
	return fmt::format("{}", visible.size());
}

std::string part2(std::string const & in) {
	auto [grid, iterschemes, n, m, boundscheck] = input(in);

	std::unordered_map<std::pair<int, int>, int> score;
	for(int y = 0; y < n; y++)
		for(int x = 0; x < m; x++)
			score.emplace(std::pair(y, x), 1);

	for(auto const & [order, iter_vars] : iterschemes) {
		std::vector<int> iter(iter_vars.size());
		iter_t outer = iter_vars[order[0]], inner = iter_vars[order[1]];
		int & i = iter[order[0]], & j = iter[order[1]];
		int & y = iter[0], & x = iter[1];

		for(i = outer.first; boundscheck(y, x); i += outer.second) {
			std::vector<int> last_seen(10, inner.first);
			for(j = inner.first; boundscheck(y, x); j += inner.second) {
				int dist = std::max(n, m) + 1;
				for(int k = (int)(grid[y][x] - '0'); k < 10 && dist; k++) {
					dist = std::min(dist, inner.second * (j - last_seen[k]));
				}
				score[std::pair(y, x)] *= dist;
				last_seen[grid[y][x] - '0'] = j;
			}
			j = inner.first;
		}
	}
	int max_score = 0;
	for(auto [_, v] : score) max_score = std::max(max_score, v);
	return fmt::format("{}", max_score);
}

}	// namespace aoc2022::day08
