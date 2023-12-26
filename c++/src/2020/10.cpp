#include <algorithm>
#include <string>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day10 {

std::vector<int64_t> input(std::string const& in) {
    std::vector<int64_t> nums{0};
    for(auto s : aoc::split(in, "\n")) {
        nums.push_back(std::stoi(s));
    }
    std::sort(nums.begin(), nums.end());
    nums.push_back(nums.back() + 3);
    return nums;
}

std::string part1(std::string const& in) {
    std::vector<int64_t> nums = input(in);
    int64_t n = nums.size();

    int64_t ones = 0, threes = 0;
    for(int64_t i = 1; i < n; i++) {
        if(nums[i] - nums[i - 1] == 1) ones++;
        else if(nums[i] - nums[i - 1] == 3) threes++;
    }
    return fmt::format("{}", ones * threes);
}

std::string part2(std::string const& in) {
    std::vector<int64_t> nums = input(in);
    int64_t n = nums.size();

    std::vector<int64_t> dp(n + 1, 0);
    dp[0] = 1;

    for(int64_t i = 1; i < n; i++) {
        dp[i] += (i - 3 >= 0 && nums[i] - nums[i - 3] <= 3 ? dp[i - 3] : 0);
        dp[i] += (i - 2 >= 0 && nums[i] - nums[i - 2] <= 3 ? dp[i - 2] : 0);
        dp[i] += (i - 1 >= 0 && nums[i] - nums[i - 1] <= 3 ? dp[i - 1] : 0);
    }
    return fmt::format("{}", dp[n - 1]);
}

} // namespace aoc2020::day10
