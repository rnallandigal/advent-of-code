#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2023::day03 {

using input_t = std::vector<std::vector<char>>;

input_t get_input(std::string const& in) {
    input_t input;
    for(auto const& line : aoc::split(in, "\n")) {
        input.emplace_back();
        for(auto const& c : line) {
            input.back().push_back(c);
        }
    }
    return input;
}
bool isDigit(char c) { return c >= '0' && c <= '9'; }
bool checkAdjacent(input_t const& vec, int y, int x) {
    for(int dy = -1; dy <= 1; dy++) {
        for(int dx = -1; dx <= 1; dx++) {
            if(aoc::between(0, (int)vec.size() - 1, y + dy)
               && aoc::between(0, (int)vec[y + dy].size() - 1, x + dx)) {
                if(vec[y + dy][x + dx] != '.'
                   && !isDigit(vec[y + dy][x + dx])) {
                    return true;
                }
            }
        }
    }
    return false;
}

std::string part1(std::string const& in) {
    auto input = get_input(in);
    int ans = 0;
    for(int i = 0; i < (int)input.size(); i++) {
        int num = 0;
        bool adj = false;
        for(int j = 0; j < (int)input[i].size(); j++) {
            if(num == 0) {
                if(isDigit(input[i][j])) {
                    num = input[i][j] - '0';
                    adj = adj || checkAdjacent(input, i, j);
                }
            } else {
                if(isDigit(input[i][j])) {
                    num = 10 * num + (input[i][j] - '0');
                    adj = adj || checkAdjacent(input, i, j);
                } else {
                    if(adj) {
                        ans += num;
                    }
                    num = 0;
                    adj = false;
                }
            }
        }

        if(adj) {
            ans += num;
        }
    }
    return fmt::format("{}", ans);
}

std::string part2(std::string const& in) {
    std::unordered_map<std::pair<int, int>, std::unordered_set<int>> cache;
    auto input = get_input(in);
    for(int i = 0; i < (int)input.size(); i++) {
        int num = 0;
        std::unordered_set<std::pair<int, int>> matches;

        for(int j = 0; j < (int)input[i].size(); j++) {
            if(isDigit(input[i][j])) {
                num = 10 * num + (input[i][j] - '0');
                for(int di = -1; di <= 1; di++) {
                    for(int dj = -1; dj <= 1; dj++) {
                        if(aoc::between(0, (int)input.size() - 1, i + di)
                           && aoc::between(
                               0, (int)input[i + di].size() - 1, j + dj
                           )) {
                            if(input[i + di][j + dj] == '*') {
                                matches.emplace(i + di, j + dj);
                            }
                        }
                    }
                }
            } else {
                if(num > 0) {
                    for(auto const& p : matches) {
                        cache[p].insert(num);
                    }
                }
                num = 0;
                matches = {};
            }
        }
        if(num > 0) {
            for(auto const& p : matches) {
                cache[p].insert(num);
            }
        }
    }

    int ans = 0;
    for(auto const& [p, nums] : cache) {
        if(nums.size() == 2) {
            int gear_ratio = 1;
            for(auto const& num : nums) {
                gear_ratio *= num;
            }
            ans += gear_ratio;
        }
    }
    return fmt::format("{}", ans);
}

} // namespace aoc2023::day03
