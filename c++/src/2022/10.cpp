#include <vector>
#include <string>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day10 {

void run(std::string const & in, std::function<void(int, int)> const & cb) {
	int cycle = 1, x = 1, dx = 0, latency = 0;
	auto insts = aoc::split(in, "\n");
	for(int i = 0; i < (int)insts.size() || latency; cycle++, latency--) {
		if(latency == 0) {
			x += dx;
			if(insts[i].substr(0, 4) == "noop") {
				latency = 1;
				dx = 0;
			} else if(insts[i].substr(0, 4) == "addx") {
				latency = 2;
				dx = std::stoi(insts[i].substr(5));
			}
			i++;
		}
		cb(cycle, x);
	}
}

std::string part1(std::string const & in) {
	int score = 0;
	run(in, [&score](int cycle, int x) {
		score += cycle % 40 == 20 ? cycle * x : 0;
	});
	return fmt::format("{}", score);
}

std::string part2(std::string const & in) {
	std::string crt = "\n";
	run(in, [&crt](int cycle, int x) {
		crt.push_back(aoc::between(x - 1, x + 1, (cycle - 1) % 40) ? '#' : '.');
		if(cycle % 40 == 0) crt.push_back('\n');
	});
	return crt;
}

}	// namespace aoc2022::day10
