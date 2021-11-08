#include <vector>
#include <string>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day23 {

using cups_t = std::vector<int>;

std::tuple<cups_t, int, int> input(std::string const & in) {
	cups_t order, cups;
	for(auto c : in)
		if(c >= '0' && c <= '9')
			order.push_back((int)(c - '0'));

	cups.resize(order.size() + 1);
	for(int i = 0; i < (int)order.size() - 1; i++) {
		cups[order[i]] = order[i + 1];
	}
	cups[order.back()] = order.front();
	return { cups, order.back(), order.front() };
}

void run(cups_t & cups, int first, int iters) {
	for(int curr = first; iters--; curr = cups[curr]) {
		int a = cups[curr], b = cups[a], c = cups[b];

		int dest = curr;
		do {
			dest = (dest - 1 == 0 ? (int)cups.size() : dest) - 1;
		} while(dest == a || dest == b || dest == c);

		cups[curr] = cups[c];
		cups[c] = cups[dest];
		cups[dest] = a;
	}
}

std::string part1(std::string const & in) {
	auto [cups, _1, first] = input(in);
	run(cups, first, 100);

	std::string soln; soln.reserve(9);
	for(int curr = cups[1]; curr != 1; curr = cups[curr]) {
		soln += curr + '0';
	}
	return fmt::format("{}", soln);
}

std::string part2(std::string const & in) {
	auto [cups, last, first] = input(in);
	cups[last] = (int)cups.size();
	for(int i = (int)cups.size() + 1; i <= 1'000'000; i++) {
		cups.push_back(i);
	}
	cups.push_back(first);

	run(cups, first, 10'000'000);
	return fmt::format("{}", (int64_t)cups[1] * cups[cups[1]]);
}

}	// namespace aoc2020::day23
