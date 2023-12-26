#include <string>
#include <unordered_map>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day01 {

std::vector<int> input(std::string const& in) {
    std::vector<int> ints;
    for(auto s : aoc::split(in, "\n"))
        ints.push_back(std::stoi(s));

    return ints;
}

std::string part1(std::string const& in) {
    auto ints = input(in);
    int n = (int)ints.size();

    std::unordered_map<int, int> cache;
    for(int i = 0; i < n; i++)
        cache[ints[i]] = i;

    for(int i = 0; i < n; i++) {
        auto it = cache.find(2020 - ints[i]);
        if(it != cache.end() && it->second != i) {
            return fmt::format("{}", ints[i] * it->first);
        }
    }
    return "";
}

std::string part2(std::string const& in) {
    auto ints = input(in);
    int n = (int)ints.size();

    std::unordered_map<int, int> cache;
    for(int i = 0; i < n; i++)
        cache[ints[i]] = i;

    for(int i = 0; i < n; i++)
        for(int j = i + 1; j < n; j++) {
            auto it = cache.find(2020 - ints[i] - ints[j]);
            if(it != cache.end() && it->second != i && it->second != j) {
                return fmt::format("{}", ints[i] * ints[j] * it->first);
            }
        }
    return "";
}

} // namespace aoc2020::day01
