#include <string>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2018::day02 {

std::string part1(std::string const& in) {
    int num_twos = 0, num_threes = 0;
    for(auto line : aoc::split(in, "\n")) {
        std::vector<int> freqs(26, 0);
        for(auto c : line)
            freqs[c - 'a']++;

        bool twos = false, threes = false;
        for(int i = 0; i < (int)freqs.size() && !(twos && threes); i++) {
            twos |= (freqs[i] == 2);
            threes |= (freqs[i] == 3);
        }
        num_twos += twos;
        num_threes += threes;
    }
    return fmt::format("{}", num_twos * num_threes);
}

std::string part2(std::string const& in) {
    auto lines = aoc::split(in, "\n");
    for(int i = 0; i < (int)lines.size(); i++) {
        for(int j = i + 1; j < (int)lines.size(); j++) {
            int diff = 0;
            for(int k = 0; k < (int)lines[i].size(); k++) {
                diff += (lines[i][k] != lines[j][k]);
            }
            if(diff != 1) continue;

            std::string ret = "";
            for(int k = 0; k < (int)lines[i].size(); k++) {
                if(lines[i][k] == lines[j][k]) ret += lines[i][k];
            }
            return ret;
        }
    }
    return "";
}

} // namespace aoc2018::day02
