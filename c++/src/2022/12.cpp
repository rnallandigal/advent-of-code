#include <functional>
#include <queue>
#include <string>
#include <tuple>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day12 {

std::vector<std::pair<int, int>> DIRS = {
    {-1,  0},
    { 1,  0},
    { 0, -1},
    { 0,  1}
};

int height(char c) {
    if(c == 'S') return 0;
    else if(c == 'E') return 25;
    else return c - 'a';
}

int bfs(std::vector<std::string> const& grid, std::function<bool(char)> end) {
    int h = grid.size() - 1, w = grid[0].size() - 1;
    std::unordered_set<std::pair<int, int>> seen;
    std::queue<std::tuple<int, int, int>> q;

    bool found = false;
    for(int y = 0; y <= h && !found; y++)
        for(int x = 0; x <= w && !found; x++)
            if(grid[y][x] == 'E') {
                seen.emplace(y, x);
                q.emplace(y, x, 0);
                found = true;
            }

    while(!q.empty()) {
        auto [y, x, d] = q.front();
        q.pop();
        if(end(grid[y][x])) return d;

        for(auto [dy, dx] : DIRS) {
            if(!aoc::between(0, h, y + dy)) continue;
            if(!aoc::between(0, w, x + dx)) continue;
            if(seen.contains(std::pair(y + dy, x + dx))) continue;
            if(height(grid[y][x]) > 1 + height(grid[y + dy][x + dx])) continue;

            seen.emplace(y + dy, x + dx);
            q.emplace(y + dy, x + dx, d + 1);
        }
    }
    return -1;
}

std::string part1(std::string const& in) {
    auto end = [](char c) { return c == 'S'; };
    return fmt::format("{}", bfs(aoc::split(in, "\n"), end));
}

std::string part2(std::string const& in) {
    auto end = [](char c) { return c == 'S' || c == 'a'; };
    return fmt::format("{}", bfs(aoc::split(in, "\n"), end));
}

} // namespace aoc2022::day12
