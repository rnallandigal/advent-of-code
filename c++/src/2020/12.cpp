#include <string>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day12 {

std::vector<std::pair<char, int>> input(std::string const& in) {
    std::vector<std::pair<char, int>> cmds;
    for(auto line : aoc::split(in, "\n")) {
        cmds.emplace_back(line[0], std::stoi(line.substr(1)));
    }
    return cmds;
}

std::pair<int, int> rotate(int x, int y, int arg) {
    if(arg == 0) return {x, y};
    if(arg == 90) return {-y, x};
    if(arg == 180) return {-x, -y};
    if(arg == 270) return {y, -x};
    if(arg == 360) return {x, y};
    return {0, 0};
}

std::string part1(std::string const& in) {
    int x = 0, y = 0, dx = 1, dy = 0;
    for(auto [action, arg] : input(in)) {
        if(action == 'N') y += arg;
        if(action == 'S') y -= arg;
        if(action == 'E') x += arg;
        if(action == 'W') x -= arg;
        if(action == 'L') std::tie(dx, dy) = rotate(dx, dy, arg);
        if(action == 'R') std::tie(dx, dy) = rotate(dx, dy, 360 - arg);
        if(action == 'F') x += (dx * arg), y += (dy * arg);
    }
    return fmt::format("{}", abs(x) + abs(y));
}

std::string part2(std::string const& in) {
    int x = 0, y = 0, dx = 10, dy = 1;
    for(auto [action, arg] : input(in)) {
        if(action == 'N') dy += arg;
        if(action == 'S') dy -= arg;
        if(action == 'E') dx += arg;
        if(action == 'W') dx -= arg;
        if(action == 'L') std::tie(dx, dy) = rotate(dx, dy, arg);
        if(action == 'R') std::tie(dx, dy) = rotate(dx, dy, 360 - arg);
        if(action == 'F') x += (dx * arg), y += (dy * arg);
    }
    return fmt::format("{}", abs(x) + abs(y));
}

} // namespace aoc2020::day12
