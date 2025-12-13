#include <algorithm>
#include <functional>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2022::day11 {

using items_t = std::vector<int>;
using operation_t = std::function<int64_t(int64_t)>;
using mod_fn_t = std::function<int(operation_t const&, int)>;
using monkey_t = std::tuple<items_t, operation_t, int, int, int>;

std::pair<std::vector<monkey_t>, int> input(std::string const& in) {
    std::vector<monkey_t> monkeys;
    int mod = 1;
    for(auto monkey : aoc::split(in, "\n\n")) {
        auto desc = aoc::split(monkey, "\n");

        items_t items;
        for(auto item : aoc::split(desc[1].substr(18), ", "))
            items.push_back(std::stoll(item));

        operation_t op;
        if(desc[2][25] == 'o') op = [](int64_t x) { return x * x; };
        else {
            int64_t arg = std::stoll(desc[2].substr(25));
            if(desc[2][23] == '+') op = [arg](int64_t x) { return x + arg; };
            if(desc[2][23] == '*') op = [arg](int64_t x) { return x * arg; };
        }

        int divisor = std::stoi(desc[3].substr(21));
        int monkey_true = std::stoi(desc[4].substr(29));
        int monkey_false = std::stoi(desc[5].substr(30));

        monkeys.emplace_back(items, op, divisor, monkey_true, monkey_false);
        mod *= divisor;
    }
    return {monkeys, mod};
}

int64_t play(std::vector<monkey_t>& monkeys, int rounds, mod_fn_t mod_fn) {
    std::vector<int> inspections(monkeys.size());
    for(int i = 0; i < rounds; i++) {
        for(int j = 0; j < (int)monkeys.size(); j++) {
            auto& [items, op, divisor, monkey_true, monkey_false] = monkeys[j];
            for(int k : items) {
                inspections[j]++;
                int worry = mod_fn(op, k);
                if(worry % divisor == 0) {
                    std::get<0>(monkeys[monkey_true]).push_back(worry);
                } else {
                    std::get<0>(monkeys[monkey_false]).push_back(worry);
                }
            }
            items.clear();
        }
    }

    std::sort(inspections.begin(), inspections.end(), std::greater<int>{});
    return (int64_t)inspections[0] * inspections[1];
}

std::string part1(std::string const& in) {
    auto [monkeys, _] = input(in);
    return fmt::format("{}", play(monkeys, 20, [](auto const& op, int x) {
                           return op(x) / 3;
                       }));
}

std::string part2(std::string const& in) {
    auto [monkeys, mod] = input(in);
    return fmt::format("{}", play(monkeys, 10000, [=](auto const& op, int x) {
                           return op(x) % mod;
                       }));
}

} // namespace aoc2022::day11
