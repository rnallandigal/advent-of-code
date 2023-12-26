#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day17 {

using rock_t = std::vector<std::pair<int, int>>;
using grid_t = std::unordered_set<std::pair<int, int>>;

std::vector<rock_t> rocks{
    {{2, 0}, {3, 0}, {4, 0}, {5, 0}},
    {{3, 2}, {2, 1}, {3, 1}, {4, 1}, {3, 0}},
    {{4, 2}, {4, 1}, {2, 0}, {3, 0}, {4, 0}},
    {{2, 3}, {2, 2}, {2, 1}, {2, 0}},
    {{2, 1}, {3, 1}, {2, 0}, {3, 0}}
};

bool move(grid_t const& grid, rock_t& rock, int dx, int dy) {
    for(auto [x, y] : rock) {
        if(!aoc::between(0, 6, x + dx)) return false;
        if(y + dy < 0) return false;
        if(grid.contains(std::pair(x + dx, y + dy))) return false;
    }
    for(auto& [x, y] : rock)
        x += dx, y += dy;
    return true;
}

std::tuple<int, int, std::vector<int>> find_cycle(std::string const& jet) {
    std::unordered_map<std::tuple<int, int, int>, int> seen;
    std::vector<int> heights;
    grid_t grid;

    int top = -1, n = jet.size() - 1, base = -1, period = 0;
    for(int i = 0, j = 0;; i++) {
        rock_t rock = rocks[i % rocks.size()];
        for(auto& [x, y] : rock)
            y += top + 4;

        do
            move(grid, rock, jet[j++ % n] == '<' ? -1 : 1, 0);
        while(move(grid, rock, 0, -1));

        for(auto const& p : rock)
            grid.insert(p);
        top = std::max(top, rock.front().second);
        heights.push_back(top);

        int top_row = 0;
        for(int x = 0; x <= 6; x++) {
            if(grid.contains(std::pair(x, top))) top_row |= (1 << x);
        }

        auto key = std::tuple(top_row, i % rocks.size(), j % n);
        if(auto it = seen.find(key); it != seen.end()) {
            int last_seen = it->second;
            if(base == -1 || i - last_seen != period) {
                base = last_seen;
                period = i - last_seen;
            } else if(i == base + 2 * period) {
                return {base, period, heights};
            }
        }
        seen[key] = i;
    }
    return {0, 0, {}};
}

int64_t get_height(std::string const& jet, int64_t round) {
    auto [base, period, heights] = find_cycle(jet);
    if(round <= (int64_t)heights.size()) return heights[round - 1];

    int64_t height_per_cycle = heights[base + period] - heights[base];
    auto [num_cycles, rem] = std::div(round - base - 1, (int64_t)period);
    return num_cycles * height_per_cycle + heights[base + rem];
}

std::string part1(std::string const& jet) {
    return fmt::format("{}", get_height(jet, 2022) + 1);
}

std::string part2(std::string const& jet) {
    return fmt::format("{}", get_height(jet, 1'000'000'000'000) + 1);
}

} // namespace aoc2022::day17
