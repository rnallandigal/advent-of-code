#include <algorithm>
#include <string>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day11 {

using grid_t = std::vector<std::vector<char>>;
grid_t input(std::string const& in) {
    grid_t grid;
    for(auto line : aoc::split(in, "\n")) {
        grid.emplace_back(line.begin(), line.end());
    }
    return grid;
}

std::vector<std::pair<int, int>> dir{
    {-1, -1},
    {-1,  0},
    {-1,  1},
    { 0, -1},
    { 0,  1},
    { 1, -1},
    { 1,  0},
    { 1,  1}
};

using stop_condition_t = std::function<bool(char)>;
grid_t run(grid_t const& grid, stop_condition_t const& stop, int threshold) {
    grid_t ret = grid;
    int n = grid.size(), m = grid[0].size();

    auto occupied = [&](int y, int x, int dy, int dx) {
        do {
            x += dx;
            y += dy;
        } while((0 <= y && y < n) && (0 <= x && x < m) && !stop(grid[y][x]));
        return (0 <= y && y < n) && (0 <= x && x < m) && grid[y][x] == '#';
    };

    for(int y = 0; y < n; y++) {
        for(int x = 0; x < m; x++) {
            if(grid[y][x] == '.') continue;

            int count = 0;
            for(auto [dx, dy] : dir) {
                count += occupied(y, x, dy, dx);
            }

            if(grid[y][x] == 'L' && count == 0) ret[y][x] = '#';
            if(grid[y][x] == '#' && count >= threshold) ret[y][x] = 'L';
        }
    }
    return ret;
}

int solve(std::string const& in, stop_condition_t const& stop, int threshold) {
    grid_t final_grid = aoc::fixed_point<grid_t>(
        [&](grid_t const& grid) { return run(grid, stop, threshold); },
        input(in)
    );

    int soln = 0;
    for(auto const& row : final_grid)
        soln += std::count(row.begin(), row.end(), '#');

    return soln;
}

std::string part1(std::string const& in) {
    stop_condition_t stop = [](char) { return true; };
    return fmt::format("{}", solve(in, stop, 4));
}

std::string part2(std::string const& in) {
    stop_condition_t stop = [](char c) { return c != '.'; };
    return fmt::format("{}", solve(in, stop, 5));
}

} // namespace aoc2020::day11
