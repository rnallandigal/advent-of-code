#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day14 {

using grid_t = std::unordered_set<std::pair<int, int>>;
std::pair<grid_t, int> input(std::string const& in) {
    grid_t grid;
    int max_y = 0;

    for(auto path : aoc::split(in, "\n")) {
        auto lines = aoc::split(path, " -> ");

        auto coords = aoc::split(lines[0], ",");
        int x = std::stoi(coords[0]), y = std::stoi(coords[1]);
        max_y = std::max(max_y, y);

        for(int i = 1; i < (int)lines.size(); i++) {
            coords = aoc::split(lines[i], ",");
            int X = std::stoi(coords[0]), Y = std::stoi(coords[1]);
            max_y = std::max(max_y, Y);

            int *k, K, dk;
            if(x == X) k = &y, K = Y, dk = (Y - y) / std::abs(Y - y);
            else k = &x, K = X, dk = (X - x) / std::abs(X - x);

            for(; *k != K; *k += dk)
                grid.emplace(x, y);
        }
        grid.emplace(x, y);
    }
    return {grid, max_y};
}

int drop_sand_while(grid_t& grid, std::function<bool(int, int)> cond) {
    int sand = 0;
    std::vector<std::pair<int, int>> ps{
        {500, 0}
    };
    while(!ps.empty()) {
        auto const& [x, y] = ps.back();
        if(!cond(x, y)) break;
        else if(!grid.contains(std::pair(x, y + 1))) {
            ps.emplace_back(x, y + 1);
        } else if(!grid.contains(std::pair(x - 1, y + 1))) {
            ps.emplace_back(x - 1, y + 1);
        } else if(!grid.contains(std::pair(x + 1, y + 1))) {
            ps.emplace_back(x + 1, y + 1);
        } else {
            grid.emplace(x, y);
            sand++;
            ps.pop_back();
        };
    }
    return sand;
}

std::string part1(std::string const& in) {
    auto [grid, abyss] = input(in);
    int sand = drop_sand_while(grid, [&](int, int y) { return y < abyss; });
    return fmt::format("{}", sand);
}

std::string part2(std::string const& in) {
    auto [grid, max_y] = input(in);
    for(int x = 497 - max_y; x < 503 + max_y; x++)
        grid.emplace(x, 2 + max_y);
    int sand = drop_sand_while(grid, [&](int, int y) { return true; });
    return fmt::format("{}", sand);
}

} // namespace aoc2022::day14
