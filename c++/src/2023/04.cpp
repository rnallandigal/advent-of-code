#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2023::day04 {

using input_t = std::vector<std::pair<std::vector<int>, std::vector<int>>>;

input_t get_input(std::string const& in) {
    input_t input;
    for(auto line : aoc::split(in, "\n")) {
        auto parts = aoc::split(line.substr(line.find(":", 0) + 2), " | ");
        std::vector<int> winning, yours;
        for(int i = 0; 3 * i < (int)parts[0].size(); i++) {
            std::string part = parts[0].substr(3 * i, 2);
            fmt::print("{}\n", part);
            winning.push_back(std::stoi(part));
        }

        for(int i = 0; 3 * i < (int)parts[1].size(); i++) {
            yours.push_back(std::stoi(parts[1].substr(3 * i, 2)));
        }

        input.emplace_back(winning, yours);
    }
    return input;
}

std::string part1(std::string const& in) {
    input_t input = get_input(in);
    int ans = 0;
    for(auto const& [winning, yours] : input) {
        std::unordered_set<int> winning_set(winning.begin(), winning.end());

        int matching = 0;
        for(int your : yours) {
            if(winning_set.count(your)) {
                matching++;
            }
        }
        fmt::print(
            "{}; {}\n", fmt::join(winning, ", "), fmt::join(yours, ", ")
        );
        fmt::print("matching: {}\n", matching);
        int val = (matching > 0 ? aoc::ipow(2, matching - 1) : 0);
        fmt::print("Card: {}\n", val);
        ans += val;
    }
    return fmt::format("{}", ans);
}

std::string part2(std::string const& in) {
    input_t input = get_input(in);
    int ans = 0;
    std::vector<int> cards(input.size(), 1), matching(input.size(), 0);
    for(int i = 0; i < (int)cards.size(); i++) {
        auto const& [winning, yours] = input[i];
        std::unordered_set<int> winning_set(winning.begin(), winning.end());

        int matching = 0;
        for(int your : yours) {
            if(winning_set.count(your)) {
                matching++;
            }
        }

        while(cards[i]--) {
            ans++;
            for(int j = 0; j < matching; j++) {
                cards[i + j + 1]++;
            }
        }
    }
    return fmt::format("{}", ans);
}

} // namespace aoc2023::day04
