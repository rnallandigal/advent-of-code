#include <string>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day03 {

int tr(char c) {
    if(c >= 'a' && c <= 'z') return c - 'a';
    else return c - 'A' + 26;
}

std::string part1(std::string const& in) {
    int score = 0;
    for(auto line : aoc::split(in, "\n")) {
        std::vector<int> seen(52);
        for(int i = 0; i < (int)line.size() / 2; i++) {
            seen[tr(line[i])] = 1;
        }
        for(int i = line.size() / 2; i < (int)line.size(); i++) {
            if(seen[tr(line[i])]) {
                score += tr(line[i]) + 1;
                break;
            }
        }
    }
    return fmt::format("{}", score);
}

std::string part2(std::string const& in) {
    int score = 0, linenum = 0;
    std::vector<int> seen(52);
    for(auto line : aoc::split(in, "\n")) {
        if(linenum == 0) std::fill(seen.begin(), seen.end(), 0);
        for(char c : line) {
            if((seen[tr(c)] |= (1 << linenum)) == 0b111) {
                score += tr(c) + 1;
                break;
            }
        }
        linenum = (linenum + 1) % 3;
    }
    return fmt::format("{}", score);
}

} // namespace aoc2022::day03
