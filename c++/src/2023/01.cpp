#include <regex>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2023::day01 {

std::string part1(std::string const& in) {
    int sum = 0;
    for(auto line : aoc::split(in, "\n")) {
        std::vector<int> digits;
        for(char c : line) {
            if(c >= '0' && c <= '9') {
                digits.push_back(c - '0');
            }
        }
        int num = 10 * digits.front() + digits.back();
        sum += num;
    }
    return fmt::format("{}", sum);
}

std::string reverse(std::string str) {
    std::reverse(str.begin(), str.end());
    return str;
}

std::string part2(std::string const& in) {
    std::unordered_map<std::string, int> rules{
        {  "one", 1},
        {  "two", 2},
        {"three", 3},
        { "four", 4},
        { "five", 5},
        {  "six", 6},
        {"seven", 7},
        {"eight", 8},
        { "nine", 9}
    };

    int sum = 0;
    for(auto line : aoc::split(in, "\n")) {
        std::string line2, line3;
        std::vector<int> digits;

        for(char c : line) {
            line2.push_back(c);
            if(line2.size() >= 3
               && rules.contains(line2.substr(line2.size() - 3, 3))) {
                char d = (char)rules[line2.substr(line2.size() - 3, 3)] + '0';
                line2.pop_back();
                line2.pop_back();
                line2.pop_back();
                line2.push_back(d);
            } else if(line2.size() >= 4 && rules.contains(line2.substr(line2.size() - 4, 4))) {

                char d = (char)rules[line2.substr(line2.size() - 4, 4)] + '0';
                line2.pop_back();
                line2.pop_back();
                line2.pop_back();
                line2.pop_back();
                line2.push_back(d);
            } else if(line2.size() >= 5 && rules.contains(line2.substr(line2.size() - 5, 5))) {

                char d = (char)rules[line2.substr(line2.size() - 5, 5)] + '0';
                line2.pop_back();
                line2.pop_back();
                line2.pop_back();
                line2.pop_back();
                line2.pop_back();
                line2.push_back(d);
            }
        }

        std::string revline = line;
        std::reverse(revline.begin(), revline.end());

        for(char c : revline) {
            line3.push_back(c);
            if(line3.size() >= 3
               && rules.contains(reverse(line3.substr(line3.size() - 3, 3)))) {
                char d = (char)rules[reverse(line3.substr(line3.size() - 3, 3))]
                         + '0';
                line3.pop_back();
                line3.pop_back();
                line3.pop_back();
                line3.push_back(d);
            } else if(line3.size() >= 4 && rules.contains(reverse(line3.substr(line3.size() - 4, 4)))) {

                char d = (char)rules[reverse(line3.substr(line3.size() - 4, 4))]
                         + '0';
                line3.pop_back();
                line3.pop_back();
                line3.pop_back();
                line3.pop_back();
                line3.push_back(d);
            } else if(line3.size() >= 5 && rules.contains(reverse(line3.substr(line3.size() - 5, 5)))) {

                char d = (char)rules[reverse(line3.substr(line3.size() - 5, 5))]
                         + '0';
                line3.pop_back();
                line3.pop_back();
                line3.pop_back();
                line3.pop_back();
                line3.pop_back();
                line3.push_back(d);
            }
        }

        for(char c : line2) {
            if(c >= '0' && c <= '9') {
                digits.push_back(c - '0');
            }
        }

        std::vector<int> digits2;

        for(char c : line3) {
            if(c >= '0' && c <= '9') {
                digits2.push_back(c - '0');
            }
        }
        sum += 10 * digits.front() + digits2.front();
    }
    return fmt::format("{}", sum);
}

} // namespace aoc2023::day01
