#include <stack>
#include <string>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day18 {

// https://en.wikipedia.org/wiki/Shunting-yard_algorithm
int64_t shunting_yard(std::string const& expr, int add, int mul) {
    std::stack<int64_t> output;
    std::vector<char> shunt(100, '\0');
    int depth = -1;
    auto prec = [add, mul](char c) { return c == '+' ? add : mul; };
    auto apply_op = [&](char op) {
        int64_t b = output.top();
        output.pop();
        int64_t a = output.top();
        output.pop();
        output.push(op == '+' ? a + b : a * b);
    };
    for(auto c : expr) {
        if(c == ' ') continue;
        else if(c == '(') shunt[++depth] = c;
        else if(c == ')') {
            while(shunt[depth] != '(')
                apply_op(shunt[depth--]);
            if(depth >= 0) depth--;
        } else if(c == '+' || c == '*') {
            while(depth >= 0 && shunt[depth] != '('
                  && prec(shunt[depth]) >= prec(c))
                apply_op(shunt[depth--]);
            shunt[++depth] = c;
        } else output.push((int64_t)(c - '0'));
    }
    while(depth >= 0)
        apply_op(shunt[depth--]);
    return output.top();
}

std::string part1(std::string const& in) {
    int64_t ret = 0;
    for(auto problem : aoc::split(in, "\n")) {
        ret += shunting_yard(problem, 1, 1);
    }
    return fmt::format("{}", ret);
}

std::string part2(std::string const& in) {
    int64_t ret = 0;
    for(auto problem : aoc::split(in, "\n")) {
        ret += shunting_yard(problem, 2, 1);
    }
    return fmt::format("{}", ret);
}

} // namespace aoc2020::day18
