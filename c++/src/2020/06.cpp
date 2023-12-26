#include <string>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day06 {

std::vector<std::vector<std::string>> input(std::string const& in) {
    std::vector<std::vector<std::string>> answers;
    for(auto group : aoc::split(in, "\n\n")) {
        answers.emplace_back(aoc::split(group, "\n"));
    }
    return answers;
}

std::vector<int> frequency(std::vector<std::string> const& group) {
    std::vector<int> freq(26, 0);
    for(auto const& person : group)
        for(char c : person)
            freq[c - 'a']++;

    return freq;
}

std::string part1(std::string const& in) {
    int sum = 0;
    for(auto const& group : input(in))
        for(auto i : frequency(group))
            sum += (i > 0);

    return fmt::format("{}", sum);
}

std::string part2(std::string const& in) {
    int sum = 0;
    for(auto const& group : input(in))
        for(auto i : frequency(group))
            sum += (i == (int)group.size());

    return fmt::format("{}", sum);
}

} // namespace aoc2020::day06
