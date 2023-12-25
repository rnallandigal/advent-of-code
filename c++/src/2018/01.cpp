#include <string>
#include <unordered_set>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2018::day01 {

std::vector<int> input(std::string const& in) {
    std::vector<int> nums;
    for(auto s : aoc::split(in, "\n")) {
        nums.push_back(std::stoi(s));
    }
    return nums;
}

std::string part1(std::string const& in) {
    int ret = 0;
    for(int i : input(in))
        ret += i;
    return fmt::format("{}", ret);
}

std::string part2(std::string const& in) {
    auto nums = input(in);
    std::unordered_set<int> seen;
    for(int cum_freq = 0, i = 0;; i = (i + 1) % nums.size()) {
        cum_freq += nums[i];
        if(auto [_, b] = seen.emplace(cum_freq); b == false) {
            return fmt::format("{}", cum_freq);
        }
    }
    return "";
}

} // namespace aoc2018::day01
