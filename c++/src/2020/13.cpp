#include <numeric>
#include <string>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day13 {

using departures_t = std::vector<std::pair<int64_t, int64_t>>;
std::pair<int64_t, departures_t> input(std::string const& in) {
    std::vector<std::string> lines = aoc::split(in, "\n");
    std::vector<std::string> buses = aoc::split(lines[1], ",");

    departures_t departures;
    for(int64_t i = 0; i < (int64_t)buses.size(); i++) {
        if(buses[i] != "x") departures.emplace_back(i, std::stoi(buses[i]));
    }
    return {std::stoi(lines[0]), departures};
}

std::string part1(std::string const& in) {
    auto [timestamp, departures] = input(in);

    int64_t min_time = std::numeric_limits<int64_t>::max(), min_bus = -1;
    for(auto [_, busID] : departures) {
        int64_t lag = (busID * ((timestamp + busID - 1) / busID)) - timestamp;
        if(lag < min_time) {
            min_time = lag;
            min_bus = busID;
        }
    }
    return fmt::format("{}", min_time * min_bus);
}

std::string part2(std::string const& in) {
    auto [_, departures] = input(in);

    auto [i, x] = departures[0];
    for(int64_t k = 1; k < (int64_t)departures.size(); k++) {
        auto [j, y] = departures[k];

        // offset = t s.t. x | (t + i) and y | (t + j)
        while((i + j) % y != 0)
            i += x;
        x = std::lcm(x, y);
    }
    return fmt::format("{}", i);
}

} // namespace aoc2020::day13
