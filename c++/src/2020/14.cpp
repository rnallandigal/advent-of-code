#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day14 {

using cmd_t = std::variant<std::string, std::pair<uint64_t, uint64_t>>;
std::vector<cmd_t> input(std::string const& in) {
    std::vector<cmd_t> program;
    for(auto line : aoc::split(in, "\n")) {
        if(line.substr(0, 4) == "mask") {
            program.emplace_back(std::in_place_index<0>, line.substr(7));
        } else {
            auto fields = aoc::split(line.substr(4), "] = ");
            program.emplace_back(
                std::in_place_index<1>,
                std::pair<uint64_t, uint64_t>{
                    std::stoull(fields[0]), std::stoull(fields[1])
                }
            );
        }
    }
    return program;
}

std::string part1(std::string const& in) {
    std::unordered_map<uint64_t, uint64_t> mem;
    std::string mask;
    for(auto cmd : input(in)) {
        if(cmd.index() == 0) {
            mask = std::get<0>(cmd);
        } else {
            auto [addr, val] = std::get<1>(cmd);
            for(int i = 0; i < 36; i++) {
                if(mask[35 - i] == '0') val &= ~(1ull << i);
                else if(mask[35 - i] == '1') val |= 1ull << i;
            }
            mem[addr] = val;
        }
    }

    uint64_t sum = 0;
    for(auto [_, v] : mem)
        sum += v;
    return fmt::format("{}", sum);
}

std::string part2(std::string const& in) {
    std::unordered_map<uint64_t, uint64_t> mem;
    std::string mask;
    for(auto cmd : input(in)) {
        if(cmd.index() == 0) {
            mask = std::get<0>(cmd);
        } else {
            auto [addr, val] = std::get<1>(cmd);
            std::vector<uint64_t> floating;
            for(int i = 0; i < 36; i++) {
                if(mask[35 - i] == '1') addr |= 1ull << i;
                else if(mask[35 - i] == 'X') floating.push_back(1ull << i);
            }

            uint64_t n = (uint64_t)floating.size();
            for(uint64_t i = 0; i < (1ull << n); i++) {
                for(uint64_t j = 0; j < n; j++) {
                    if(i & (1ull << j)) addr |= floating[j];
                    else addr &= ~floating[j];
                }
                mem[addr] = val;
            }
        }
    }

    uint64_t sum = 0;
    for(auto [_, v] : mem)
        sum += v;
    return fmt::format("{}", sum);
}

} // namespace aoc2020::day14
