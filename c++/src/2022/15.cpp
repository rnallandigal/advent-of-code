#include <regex>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day15 {

using desc_t = std::tuple<int, int, int, int, int>;
using line_segment_t = std::tuple<int, int, int, int>;
using point_t = std::pair<int, int>;

std::vector<desc_t> input(std::string const& in) {
    std::regex line_regex("Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is "
                          "at x=(-?\\d+), y=(-?\\d+)");
    std::vector<desc_t> descs;
    std::smatch matches;
    for(auto line : aoc::split(in, "\n")) {
        if(std::regex_match(line, matches, line_regex)) {
            int sx = std::stoi(matches[1]);
            int sy = std::stoi(matches[2]);
            int bx = std::stoi(matches[3]);
            int by = std::stoi(matches[4]);
            int dist = std::abs(sx - bx) + std::abs(sy - by);
            descs.emplace_back(sx, sy, bx, by, dist);
        }
    }
    return descs;
}

std::vector<point_t> cover(std::vector<desc_t> const& descs, int y) {
    std::vector<point_t> intervals;
    for(auto [sx, sy, bx, by, d] : descs) {
        if(int width = d - std::abs(y - sy); width >= 0)
            intervals.emplace_back(sx - width, sx + width);
    }
    std::sort(intervals.begin(), intervals.end());

    std::vector<point_t> merged_intervals{intervals.front()};
    for(int i = 1; i < (int)intervals.size(); i++) {
        auto& [l, r] = merged_intervals.back();
        auto [L, R] = intervals[i];

        if(L <= r) r = std::max(r, R);
        else merged_intervals.emplace_back(L, R);
    }
    return merged_intervals;
}

bool is_within(std::vector<desc_t> const& descs, int x, int y) {
    for(auto [sx, sy, bx, by, d] : descs) {
        if(std::abs(x - sx) + std::abs(y - sy) <= d) return true;
    }
    return false;
}

// TODO: Is this solution always correct?
point_t find_beacon(std::vector<desc_t> const& descs, int dim) {
    std::vector<line_segment_t> segments;

    for(auto [sx, sy, bx, by, d] : descs) {
        d++;
        segments.emplace_back(-1, sy + sx - d, sx - d, sx); // top-left
        segments.emplace_back(1, sy - sx - d, sx, sx + d);  // top-right
        segments.emplace_back(1, sy - sx + d, sx - d, sx);  // bottom-left
        segments.emplace_back(-1, sy + sx + d, sx, sx + d); // bottom-right
    }

    std::unordered_map<point_t, std::vector<int>> intersections;
    for(int i = 0; i < (int)segments.size(); i++) {
        auto [m, b, l, r] = segments[i];
        for(int j = i + 1; j < (int)segments.size(); j++) {
            auto [M, B, L, R] = segments[j];
            if(m == M) continue;

            auto [x, rem_x] = std::div(B - b, m - M);
            auto [y, rem_y] = std::div(m * B - M * b, m - M);
            if(aoc::between(l, r, x) && rem_x == 0 && rem_y == 0) {
                intersections[std::pair(x, y)].push_back(i);
                intersections[std::pair(x, y)].push_back(j);
            }
        }
    }

    for(auto [p, segments] : intersections) {
        auto [x, y] = p;
        if(segments.size() < 4) continue;
        if(is_within(descs, x, y)) continue;
        if(!aoc::between(0, dim, x)) continue;
        if(!aoc::between(0, dim, y)) continue;

        return {x, y};
    }
    return {-1, -1};
}

std::string part1(std::string const& in) {
    int points_covered = 0;
    auto descs = input(in);
    for(auto [l, r] : cover(descs, 2'000'000)) {
        std::unordered_set<int> beacons;
        for(auto [sx, sy, bx, by, d] : descs) {
            if(by == 2'000'000 && aoc::between(l, r, bx)) {
                beacons.insert(bx);
            }
        }
        points_covered += r - l + 1 - beacons.size();
    }
    return fmt::format("{}", points_covered);
}

std::string part2(std::string const& in) {
    auto [x, y] = find_beacon(input(in), 4'000'000);
    return fmt::format("{}", 4'000'000ll * x + y);
}

} // namespace aoc2022::day15
