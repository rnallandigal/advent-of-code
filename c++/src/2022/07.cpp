#include <regex>
#include <string>
#include <unordered_map>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day07 {

std::unordered_map<std::string, int> traverse(std::string const& in) {
    std::regex cd_regex("\\$ cd (.*)");
    std::regex ls_regex("\\$ ls");
    std::regex file_regex("(\\d+) (.*)");

    std::unordered_map<std::string, int> dirsize;
    std::smatch matches;
    std::string cwd("/");
    auto lines = aoc::split(in, "\n");
    for(int i = 1; i < (int)lines.size();) {
        if(std::regex_match(lines[i], matches, cd_regex)) {
            std::string segment = matches[1].str();
            if(segment == "..") {
                std::string cwd_old = cwd;
                cwd.resize(cwd.rfind('/', cwd.size() - 2) + 1);
                dirsize[cwd] += dirsize[cwd_old];
            } else {
                cwd.append(segment).append("/");
            }
            i++;
        } else if(std::regex_match(lines[i], matches, ls_regex)) {
            for(i++; i < (int)lines.size() && lines[i][0] != '$'; i++) {
                if(std::regex_match(lines[i], matches, file_regex)) {
                    dirsize[cwd] += std::stoi(matches[1].str());
                }
            }
        }
    }
    while(cwd != "/") {
        std::string cwd_old = cwd;
        cwd.resize(cwd.rfind('/', cwd.size() - 2) + 1);
        dirsize[cwd] += dirsize[cwd_old];
    }
    return dirsize;
}

std::string part1(std::string const& in) {
    int total = 0;
    for(auto [_, size] : traverse(in)) {
        if(size < 100'000) total += size;
    }
    return fmt::format("{}", total);
}

std::string part2(std::string const& in) {
    auto dirsizes = traverse(in);
    int need_to_delete = 70'000'000;
    for(auto [_, size] : dirsizes) {
        if(dirsizes["/"] - size < 40'000'000)
            need_to_delete = std::min(need_to_delete, size);
    }
    return fmt::format("{}", need_to_delete);
}

} // namespace aoc2022::day07
