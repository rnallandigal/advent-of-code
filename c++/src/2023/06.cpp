#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2023::day06 {

std::vector<int64_t> times = {48, 93, 84, 66};

std::vector<int64_t> dists = {261, 1192, 1019, 1063};

int64_t ways(int64_t b, int64_t c) {
    double l = (b - std::sqrt(b * b - 4 * c)) / 2;
    double h = (b + std::sqrt(b * b - 4 * c)) / 2;
    return ((int64_t)h) - ((int64_t)(l + 1)) + 1;
}

std::string part1(std::string const& in) {
    int ans = 1;
    for(int i = 0; i < 4; i++) {
        ans *= ways(times[i], dists[i]);
    }
    return fmt::format("{}", ans);
}

std::string part2(std::string const& in) {
    return fmt::format("{}", ways(48'938'466, 261'119'210'191'063));
}

} // namespace aoc2023::day06
