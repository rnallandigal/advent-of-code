#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day24 {

using coord_t = std::tuple<int, int, int>;
using path_t = std::vector<int>;
using input_t = std::vector<path_t>;

input_t input(std::string const& in) {
    input_t ret;
    for(auto line : aoc::split(in, "\n")) {
        ret.emplace_back();
        for(int i = 0, dir = 0; i < (int)line.size(); i++) {
            if(line[i] == 'e') dir = 0;
            else if(line[i] == 's') dir = line[++i] == 'e' ? 1 : 2;
            else if(line[i] == 'w') dir = 3;
            else if(line[i] == 'n') dir = line[++i] == 'w' ? 4 : 5;
            ret.back().push_back(dir);
        }
    }
    return ret;
}

coord_t coord(path_t const& path) {
    int x = 0, y = 0, z = 0;
    for(int i = 0; i < (int)path.size(); i++) {
        if(path[i] == 0) ++x, --y;
        else if(path[i] == 1) --y, ++z;
        else if(path[i] == 2) ++z, --x;
        else if(path[i] == 3) --x, ++y;
        else if(path[i] == 4) ++y, --z;
        else if(path[i] == 5) --z, ++x;
    }
    return {x, y, z};
}

std::string part1(std::string const& in) {
    std::unordered_map<coord_t, bool> tiles;
    for(auto path : input(in)) {
        auto loc = coord(path);
        tiles[loc] = !tiles[loc];
    }

    int soln = std::count_if(tiles.begin(), tiles.end(), [](auto const& p) {
        return p.second;
    });
    return fmt::format("{}", soln);
}

std::string part2(std::string const& in) {
    std::unordered_map<coord_t, bool> tiles;
    for(auto path : input(in)) {
        auto loc = coord(path);
        tiles[loc] = !tiles[loc];
    }

    auto get_color = [&](coord_t const& loc) {
        auto it = tiles.find(loc);
        return it != tiles.end() && it->second;
    };

    auto count_blacks = [&](coord_t const& loc) {
        auto [x, y, z] = loc;
        int count = 0;
        count += get_color({x + 1, y - 1, z});
        count += get_color({x, y - 1, z + 1});
        count += get_color({x - 1, y, z + 1});
        count += get_color({x - 1, y + 1, z});
        count += get_color({x, y + 1, z - 1});
        count += get_color({x + 1, y, z - 1});
        return count;
    };

    for(int i = 0; i < 100; i++) {
        std::unordered_map<coord_t, bool> new_tiles;
        std::unordered_set<coord_t> candidates;

        for(auto const& [loc, color] : tiles) {
            if(color == false) candidates.insert(loc);
            else {
                auto [x, y, z] = loc;
                candidates.insert(loc);
                candidates.insert({x + 1, y - 1, z});
                candidates.insert({x, y - 1, z + 1});
                candidates.insert({x - 1, y, z + 1});
                candidates.insert({x - 1, y + 1, z});
                candidates.insert({x, y + 1, z - 1});
                candidates.insert({x + 1, y, z - 1});
            }
        }

        for(auto const& loc : candidates) {
            bool color = get_color(loc);
            int blacks = count_blacks(loc);
            if(blacks == 2 || (color && blacks == 1))
                new_tiles.emplace(loc, true);
        }
        std::swap(tiles, new_tiles);
    }
    return fmt::format("{}", tiles.size());
}

} // namespace aoc2020::day24
