#include <queue>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>
#include <fmt/ranges.h>

#include "utils.h"

namespace aoc2022::day18 {

using cube_t = std::tuple<int, int, int>;
using cubes_t = std::unordered_set<cube_t>;

cubes_t input(std::string const& in) {
    cubes_t cubes;
    for(auto line : aoc::split(in, "\n")) {
        auto coords = aoc::split(line, ",");
        cubes.emplace(
            std::stoi(coords[0]), std::stoi(coords[1]), std::stoi(coords[2])
        );
    }
    return cubes;
}

std::vector<cube_t> neighbors(cube_t const& cube) {
    auto [x, y, z] = cube;
    return {
        {x + 1,     y,     z},
        {x - 1,     y,     z},
        {    x, y + 1,     z},
        {    x, y - 1,     z},
        {    x,     y, z + 1},
        {    x,     y, z - 1}
    };
}

int incident_area(cubes_t const& cubes, cubes_t const& boundary) {
    int area = 0;
    for(auto const& cube : cubes) {
        for(auto neighbor : neighbors(cube)) {
            if(boundary.contains(neighbor)) area++;
        }
    }
    return area;
}

std::string part1(std::string const& in) {
    auto cubes = input(in);
    return fmt::format("{}", 6 * cubes.size() - incident_area(cubes, cubes));
}

struct DisjointSet {
    std::unordered_map<cube_t, cube_t> link;
    std::unordered_map<cube_t, int> size;

    DisjointSet() {}

    void add(cube_t const& cube) {
        link[cube] = cube;
        size[cube] = 1;
    }

    void unite(cube_t a, cube_t b) {
        a = find(a), b = find(b);
        if(a == b) return;
        if(size[a] < size[b]) std::swap(a, b);
        link[b] = a;
        size[a] += size[b];
        size.erase(b);
    }

    cube_t find(cube_t cube) {
        auto root = cube;
        while(root != link[root])
            root = link[root];
        while(root != link[cube])
            cube = exchange(link[cube], root);
        return root;
    }
};

std::string part2(std::string const& in) {
    auto cubes = input(in);

    int level = 0, num_groups = 0;
    DisjointSet groups;
    cubes_t seen(cubes.begin(), cubes.end());

    std::queue<std::pair<cube_t, int>> q;
    for(auto const& cube : cubes)
        q.emplace(cube, level);

    while(!q.empty()) {
        auto [cube, current_level] = q.front();
        q.pop();
        if(current_level > level) {
            level = current_level;
            if(num_groups == (int)groups.size.size()) break;
            else num_groups = groups.size.size();
        }

        for(auto const& neighbor : neighbors(cube)) {
            if(!seen.contains(neighbor)) {
                seen.emplace(neighbor);
                q.emplace(neighbor, level + 1);
            }

            if(!cubes.contains(neighbor)) {
                if(level == 0 || !groups.link.contains(neighbor))
                    groups.add(neighbor);

                if(level > 0) groups.unite(cube, neighbor);
            }
        }
    }

    int largest_group = 1;
    for(auto const& [_, size] : groups.size)
        largest_group = std::max(largest_group, size);

    // assume only one lava droplet => largest group is exterior cubes
    cubes_t internal_cubes;
    for(auto const& cube : seen) {
        if(cubes.contains(cube)) continue;
        if(groups.size[groups.find(cube)] < largest_group)
            internal_cubes.insert(cube);
    }

    int total_surface_area = 6 * cubes.size() - incident_area(cubes, cubes);
    int interior_surface_area = incident_area(internal_cubes, cubes);
    return fmt::format("{}", total_surface_area - interior_surface_area);
}

} // namespace aoc2022::day18
