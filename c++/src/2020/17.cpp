#include <string>
#include <unordered_map>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day17 {

// insert-only set that supports fast element testing and iteration
struct grid_t {
    int sz;
    std::vector<int> test, iter;
    grid_t(int cap) : sz(0), test(cap, 0), iter(cap, 0) {}
    void insert(int i) {
        if(!test[i]) {
            test[i] = 1;
            iter[sz++] = i;
        }
    }
    void reset() {
        sz = 0;
        std::fill(test.begin(), test.end(), 0);
    }
};

void swap(grid_t& s1, grid_t& s2) {
    using std::swap;
    swap(s1.sz, s2.sz);
    swap(s1.test, s2.test);
    swap(s1.iter, s2.iter);
}

static int const EXTENT = 26;
using point_t = std::vector<int>;
int serialize(point_t const& point, int d) {
    int ret = 0;
    for(int i = 0; i < d; i++)
        ret = EXTENT * ret + (point[i] + EXTENT / 2);
    return ret;
}

grid_t input(std::string const& in, int d) {
    grid_t grid(aoc::ipow(EXTENT, d));
    point_t point(d, 0);
    auto lines = aoc::split(in, "\n");
    for(int y = 0; y < (int)lines.size(); y++) {
        for(int x = 0; x < (int)lines[0].size(); x++) {
            if(lines[y][x] == '.') continue;
            point[0] = x;
            point[1] = y;
            grid.insert(serialize(point, d));
        }
    }
    return grid;
}

std::vector<int> get_neighbors(int point, int d) {
    static point_t min(4), max(4), iter(4);

    std::vector<int> neighbors;
    neighbors.reserve(aoc::ipow(d, 3));

    // deserialize point into min and initialize max, iter
    for(int i = d - 1; i >= 0; i--) {
        int offset = point - EXTENT / 2;
        min[i] = (offset % EXTENT) - 1;
        max[i] = min[i] + 2;
        iter[i] = min[i];
        point = offset / EXTENT;
    }

    // populate neighbors
    while(iter[d - 1] <= max[d - 1]) {
        neighbors.push_back(serialize(iter, d));
        iter[0]++;
        for(int i = 1; iter[i - 1] > max[i - 1] && i < d; i++) {
            iter[i - 1] = min[i - 1];
            iter[i]++;
        }
    }
    return neighbors;
}

grid_t solve(grid_t& grid, int d, int iterations) {
    int len = aoc::ipow(EXTENT, d);
    grid_t candidates(len), next(len);
    std::unordered_map<int, std::vector<int>> neighbors_cache;

    for(int i = 0; i < iterations; i++) {
        candidates.reset();
        for(int j = 0; j < grid.sz; j++) {
            int point = grid.iter[j];
            if(neighbors_cache.find(point) == neighbors_cache.end()) {
                neighbors_cache[point] = get_neighbors(point, d);
            }
            for(int candidate : neighbors_cache[point]) {
                candidates.insert(candidate);
            }
        }

        next.reset();
        for(int j = 0; j < candidates.sz; j++) {
            int point = candidates.iter[j];
            if(neighbors_cache.find(point) == neighbors_cache.end()) {
                neighbors_cache[point] = get_neighbors(point, d);
            }

            int actives = 0;
            for(auto neighbor : neighbors_cache[point]) {
                actives += grid.test[neighbor];
            }
            actives -= grid.test[point];
            if(actives == 3 || (actives == 2 && grid.test[point]))
                next.insert(point);
        }
        using std::swap;
        swap(grid, next);
    }
    return grid;
}

std::string part1(std::string const& in) {
    auto grid = input(in, 3);
    return fmt::format("{}", solve(grid, 3, 6).sz);
}

std::string part2(std::string const& in) {
    auto grid = input(in, 4);
    return fmt::format("{}", solve(grid, 4, 6).sz);
}

} // namespace aoc2020::day17
