#include <vector>
#include <string>
#include <variant>
#include <compare>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day13 {

struct packet_t : std::variant<std::vector<packet_t>, int> {};

packet_t parse_packet(std::string const & s) {
	int num = 0;
	std::vector<packet_t> stack;
	for(int i = 0; i < (int)s.size(); i++) {
		if(s[i] == '[') {
			num = 0;
			stack.emplace_back();
		} else if(s[i] == ']') {
			if('0' <= s[i - 1] && s[i - 1] <= '9') {
				std::get<0>(stack.back()).emplace_back(num);
				num = 0;
			}

			auto last = std::move(stack.back());
			stack.pop_back();
			if(stack.empty()) return last;
			else std::get<0>(stack.back()).emplace_back(last);

		} else if(s[i] == ',') {
			if('0' <= s[i - 1] && s[i - 1] <= '9') {
				std::get<0>(stack.back()).emplace_back(num);
				num = 0;
			}
		} else {
			num = 10 * num + (s[i] - '0');
		}
	}
	return packet_t();
}

std::vector<packet_t> input(std::string const & in) {
	std::vector<packet_t> packets;
	for(auto line : aoc::split(in, "\n")) {
		if(!line.empty()) packets.emplace_back(parse_packet(line));
	}
	return packets;
}

std::strong_ordering comp(packet_t const & a, packet_t const & b) {
	if(a.index() == 1 && b.index() == 1) {
		return std::get<1>(a) <=> std::get<1>(b);
	} else if(a.index() == 0 && b.index() == 0) {
		auto const & av = std::get<0>(a), & bv = std::get<0>(b);
		for(int i = 0; i < (int)av.size() && i < (int)bv.size(); i++) {
			std::strong_ordering cmp = comp(av[i], bv[i]);
			if(std::is_neq(cmp)) return cmp;
		}
		return av.size() <=> bv.size();
	} else if(a.index() == 0) {
		return comp(a, packet_t(std::vector<packet_t>{ b }));
	} else {
		return comp(packet_t(std::vector<packet_t>{ a }), b);
	}
}

std::string part1(std::string const & in) {
	int score = 0;
	auto packets = input(in);
	for(int i = 0; i < (int)packets.size(); i += 2) {
		if(comp(packets[i], packets[i + 1]) <= 0) {
			score += 1 + i / 2;
		}
	}
	return fmt::format("{}", score);
}

std::string part2(std::string const & in) {
	auto packets = input(in);

	packet_t divider1 = parse_packet("[[2]]"), divider2 = parse_packet("[[6]]");
	packets.push_back(divider1);
	packets.push_back(divider2);

	auto less = [](packet_t const & a, packet_t const & b) {
		return comp(a, b) < 0;
	};

	std::sort(packets.begin(), packets.end(), less);

	auto i = std::lower_bound(packets.begin(), packets.end(), divider1, less);
	auto j = std::lower_bound(packets.begin(), packets.end(), divider2, less);

	return fmt::format("{}",
		(i - packets.begin() + 1) * (j - packets.begin() + 1)
	);
}

}	// namespace aoc2022::day13
