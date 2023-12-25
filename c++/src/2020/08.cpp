#include <string>
#include <tuple>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day08 {

using program_t = std::vector<std::tuple<std::string, int>>;

program_t input(std::string const& in) {
    program_t program;
    for(auto line : aoc::split(in, "\n")) {
        program.emplace_back(line.substr(0, 3), std::stoi(line.substr(4)));
    }
    return program;
}

std::tuple<int, bool> run(program_t const& program) {
    std::vector<int> seen(program.size(), 0);
    int acc = 0, ip = 0;

    while(true) {
        if(ip >= (int)program.size()) return {acc, true};
        if(seen[ip]) return {acc, false};

        seen[ip] = 1;
        auto [inst, arg] = program[ip];

        if(inst == "acc") acc += arg, ip++;
        else if(inst == "jmp") ip += arg;
        else ip++;
    }
    return {acc, true}; // unreachable
}

std::string part1(std::string const& in) {
    auto program = input(in);
    return fmt::format("{}", std::get<0>(run(program)));
}

std::string part2(std::string const& in) {
    auto program = input(in);
    for(int i = 0; i < (int)program.size(); i++) {
        auto [inst, arg] = program[i];
        if(inst == "acc") continue;

        program[i] = {inst == "jmp" ? "nop" : "jmp", arg};

        auto [acc, halted] = run(program);
        if(halted) return fmt::format("{}", acc);

        program[i] = {inst, arg};
    }
    return fmt::format("bad input: cannot find a program that halts");
}

} // namespace aoc2020::day08
