#include <string>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day01 {

std::string part1(std::string const& in) {
    int max_calories = 0;
    for(auto elves : aoc::split(in, "\n\n")) {
        int sum_calories = 0;
        for(auto calories : aoc::split(elves, "\n")) {
            sum_calories += std::stoi(calories);
        }
        max_calories = std::max(max_calories, sum_calories);
    }
    return fmt::format("{}", max_calories);
}

std::string part2(std::string const& in) {
    std::vector<int> sums;
    for(auto elves : aoc::split(in, "\n\n")) {
        int sum_calories = 0;
        for(auto calories : aoc::split(elves, "\n")) {
            sum_calories += std::stoi(calories);
        }
        sums.push_back(sum_calories);
    }
    std::sort(sums.begin(), sums.end());
    int n = sums.size();
    return fmt::format("{}", sums[n - 3] + sums[n - 2] + sums[n - 1]);
}

} // namespace aoc2022::day01
