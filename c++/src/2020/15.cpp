#include <string>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day15 {

std::vector<int> input(std::string const& in) {
    std::vector<int> nums;
    for(auto s : aoc::split(in, ",")) {
        nums.push_back(std::stoi(s));
    }
    return nums;
}

int play(std::vector<int> const& nums, int num_turns) {
    int n = (int)nums.size();
    std::vector<int> spoken(num_turns, 0);
    for(int i = 0; i < n - 1; i++)
        spoken[nums[i]] = i + 1;

    int most_recently_spoken = nums.back();
    for(int i = n; i < num_turns; i++) {
        int turns_since_spoken = i - spoken[most_recently_spoken];
        spoken[most_recently_spoken] = i;
        most_recently_spoken = turns_since_spoken < i ? turns_since_spoken : 0;
    }
    return most_recently_spoken;
}

std::string part1(std::string const& in) {
    return fmt::format("{}", play(input(in), 2020));
}

std::string part2(std::string const& in) {
    return fmt::format("{}", play(input(in), 30'000'000));
}

} // namespace aoc2020::day15
