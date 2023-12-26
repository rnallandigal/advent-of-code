#include <queue>
#include <regex>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day16 {

using line_entry_t = std::tuple<std::string, int, std::vector<std::string>>;
using graph_t = std::vector<std::vector<int>>;
using graph_dist_t = std::vector<std::vector<std::pair<int, int>>>;
using input_t = std::tuple<graph_dist_t, std::vector<int>, int>;
using dp_cache_t = std::vector<std::vector<std::vector<int>>>;
using dp_cache_v_t = std::vector<std::vector<int>>;

graph_dist_t::value_type bfs(graph_t const& graph, int start, int n) {
    graph_dist_t::value_type distances;
    std::unordered_set<int> seen;
    std::queue<std::pair<int, int>> q;

    seen.insert(start);
    q.emplace(start, 0);
    while(!q.empty()) {
        auto [valve, dist] = q.front();
        q.pop();
        if(valve < n && valve != start) distances.emplace_back(valve, dist);

        for(auto const& tunnel : graph[valve]) {
            if(!seen.contains(tunnel)) {
                seen.insert(tunnel);
                q.emplace(tunnel, dist + 1);
            }
        }
    }
    return distances;
}

input_t preprocess_input(std::string const& in) {
    std::regex line_regex(
        "Valve (\\S*) has flow rate=(\\d+); tunnels? leads? to valves? (.*)"
    );
    std::smatch matches;

    std::vector<line_entry_t> line_entries;
    for(auto line : aoc::split(in, "\n")) {
        if(std::regex_match(line, matches, line_regex)) {
            line_entries.emplace_back(
                matches[1], std::stoi(matches[2]), aoc::split(matches[3], ", ")
            );
        }
    }

    std::sort(
        line_entries.begin(),
        line_entries.end(),
        [](auto const& a, auto const& b) {
            return std::get<1>(a) > std::get<1>(b);
        }
    );

    int AA = 0, n = 0;
    for(int i = 0; i < (int)line_entries.size(); i++) {
        auto const& [valve, flow, neighbors] = line_entries[i];
        if(valve == "AA") AA = i;
        if(flow > 0) n++;
    }
    std::swap(line_entries[n], line_entries[AA]);

    std::unordered_map<std::string, int> vmap;
    std::vector<int> flows;
    for(int i = 0; i < (int)line_entries.size(); i++) {
        auto const& [valve, flow, neighbors] = line_entries[i];
        vmap.emplace(valve, i);
        flows.emplace_back(flow);
    }

    graph_t graph;
    for(auto const& [valve, flow, neighbors] : line_entries) {
        graph.emplace_back();
        for(auto const& neighbor : neighbors) {
            graph.back().push_back(vmap[neighbor]);
        }
    }

    graph_dist_t processed_graph;
    for(int i = 0; i <= n; i++) {
        processed_graph.emplace_back(bfs(graph, i, n));
    }
    return {processed_graph, flows, n};
}

std::string part1(std::string const& in) {
    auto [graph, flows, n] = preprocess_input(in);
    dp_cache_t dp(31, dp_cache_v_t(n + 1, std::vector<int>(1 << n, -1)));

    // top-down dynamic programming
    std::function<int(int, int, int)> max_pressure =
        [&](int time, int valve, int opens) -> int {
        if(time <= 1) return 0;
        if(dp[time][valve][opens] >= 0) return dp[time][valve][opens];

        int pressure = 0;
        if(valve < n && (opens & (1 << valve)) == 0) {
            pressure = (time - 1) * flows[valve]
                       + max_pressure(time - 1, valve, opens | (1 << valve));
        }
        for(auto const& [tunnel, dist] : graph[valve]) {
            if(~(opens & (1 << tunnel)) && time - dist > 1) {
                pressure = std::max(
                    pressure, max_pressure(time - dist, tunnel, opens)
                );
            }
        }
        dp[time][valve][opens] = pressure;
        return pressure;
    };
    return fmt::format("{}", max_pressure(30, n, 0));
}

std::string part2(std::string const& in) {
    auto [graph, flows, n] = preprocess_input(in);
    dp_cache_t dp(27, dp_cache_v_t(n + 1, std::vector<int>(1 << n, -1)));

    // bottom up dynamic programming
    for(int time = 2; time <= 26; time++) {
        for(int valve = 0; valve <= n; valve++) {
            for(int opens = 0; opens < (1 << n); opens++) {
                int pressure = 0;
                if(valve < n && (opens & (1 << valve)) == 0) {
                    pressure = (time - 1) * flows[valve]
                               + dp[time - 1][valve][opens | (1 << valve)];
                }
                for(auto [tunnel, dist] : graph[valve]) {
                    if(time - dist > 1)
                        pressure =
                            std::max(pressure, dp[time - dist][tunnel][opens]);
                }
                dp[time][valve][opens] = pressure;
            }
        }
    }

    int max_pressure = 0;
    for(int opens = 0; opens < (1 << n); opens++) {
        max_pressure = std::max(
            max_pressure, dp[26][n][opens] + dp[26][n][opens ^ ((1 << n) - 1)]
        );
    }
    return fmt::format("{}", max_pressure);
}

} // namespace aoc2022::day16
