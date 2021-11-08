#include <vector>
#include <string>
#include <utility>
#include <algorithm>
#include <unordered_set>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day22 {

using deck_t = std::vector<int>;
using game_t = std::pair<deck_t, deck_t>;

game_t input(std::string const & in) {
	std::vector<deck_t> decks;
	for(auto part : aoc::split(in, "\n\n")) {
		decks.emplace_back();
		auto cards = aoc::split(part, "\n");
		for(int i = 1; i < (int)cards.size(); i++) {
			decks.back().push_back(std::stoi(cards[i]));
		}
	}
	return { decks[0], decks[1] };
}

void exchange(deck_t & winner, deck_t & loser) {
	std::rotate(winner.begin(), winner.begin() + 1, winner.end());
	winner.push_back(loser.front());
	loser.erase(loser.begin());
}

bool winner(deck_t const &, deck_t const &);
bool rec_combat(game_t & game, std::unordered_set<game_t> & record) {
	auto & [p1, p2] = game;
	if(p1.empty()) return false;
	if(p2.empty()) return true;
	if(record.count(game)) return true;

	record.insert(game);
	winner(p1, p2) ? exchange(p1, p2) : exchange(p2, p1);
	return rec_combat(game, record);
}

bool winner(deck_t const & p1, deck_t const & p2) {
	if(p1.front() >= (int)p1.size() || p2.front() >= (int)p2.size()) {
		return p1.front() > p2.front();
	}
	game_t next = {
		deck_t(p1.begin() + 1, p1.begin() + 1 + p1.front()),
		deck_t(p2.begin() + 1, p2.begin() + 1 + p2.front())
	};
	std::unordered_set<game_t> record;
	return rec_combat(next, record);
}

int score(deck_t const & deck) {
	int ret = 0;
	for(int i = 0, j = (int)deck.size(); j > 0; i++, j--) {
		ret += deck[i] * j;
	}
	return ret;
}

std::string part1(std::string const & in) {
	auto [p1, p2] = input(in);
	while(!p1.empty() && !p2.empty()) {
		p1.front() > p2.front() ? exchange(p1, p2) : exchange(p2, p1);
	}
	return fmt::format("{}", score(p1.empty() ? p2 : p1));
}

std::string part2(std::string const & in) {
	auto game = input(in);
	auto & [p1, p2] = game;
	std::unordered_set<game_t> record;
	return fmt::format("{}", score(rec_combat(game, record) ? p1 : p2));
}

}	// namespace aoc2020::day22
