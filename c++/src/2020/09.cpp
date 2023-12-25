#include <algorithm>
#include <string>
#include <unordered_map>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day09 {

static int const PREAMBLE = 25;
std::vector<int64_t> input(std::string const& in) {
    std::vector<int64_t> nums;
    for(auto s : aoc::split(in, "\n")) {
        nums.push_back(std::stoll(s));
    }
    return nums;
}

int64_t first_not_two_sum(std::vector<int64_t> const& nums) {
    int64_t n = (int64_t)nums.size();

    std::unordered_map<int64_t, int64_t> cache;
    for(int64_t i = 0; i < PREAMBLE; i++)
        cache[nums[i]] = i;

    for(int64_t i = PREAMBLE; i < n; i++) {
        bool found = false;
        for(int64_t j = i - PREAMBLE; !found && j < i; j++) {
            auto it = cache.find(nums[i] - nums[j]);
            if(it != cache.end() && it->second != j) {
                found = true;
            }
        }
        if(!found) return nums[i];

        cache.erase(nums[i - PREAMBLE]);
        cache.emplace(nums[i], i);
    }
    return -1;
}

std::string part1(std::string const& in) {
    std::vector<int64_t> nums = input(in);
    return fmt::format("{}", first_not_two_sum(nums));
}

std::string part2(std::string const& in) {
    std::vector<int64_t> nums = input(in);
    int64_t n = (int64_t)nums.size(), target = first_not_two_sum(nums);

    std::vector<int64_t> prefixes(n + 1, 0);
    for(int i = 0; i < n; i++)
        prefixes[i + 1] = prefixes[i] + nums[i];

    for(int i = 0, j = 2; i <= n && j <= n;) {
        int64_t sum = prefixes[j] - prefixes[i];
        if(sum == target && i + 2 <= j) {
            auto [a, b] =
                std::minmax_element(nums.begin() + i, nums.begin() + j);
            return fmt::format("{}", *a + *b);
        } else if(sum <= target) {
            j++;
        } else {
            i++;
        }
    }
    return fmt::format("{}", "");
}

} // namespace aoc2020::day09
