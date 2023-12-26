#include <string>
#include <unordered_map>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day25 {

std::pair<int, int> input(std::string const& in) {
    auto parts = aoc::split(in, "\n");
    return {std::stoi(parts[0]), std::stoi(parts[1])};
}

// baby-step, giant-step: https://cp-algorithms.com/algebra/discrete-log.html
// O(log(m)) to solve for x in a^x = b (mod m) assuming a, m are coprime
int discrete_log(int a, int b, int m) {
    int64_t n = (int)sqrt(m) + 1;

    int an = 1;
    for(int i = 0; i < n; ++i)
        an = (an * 1ll * a) % m;

    std::unordered_map<int, int> vals;
    for(int q = 0, cur = b; q <= n; ++q) {
        vals[cur] = q;
        cur = (cur * 1ll * a) % m;
    }

    for(int p = 1, cur = 1; p <= n; ++p) {
        cur = (cur * 1ll * an) % m;
        if(vals.count(cur)) return n * p - vals[cur];
    }
    return -1;
}

int powmod(int a, int b, int m) {
    int res = 1;
    while(b > 0) {
        if(b & 1) res = (res * 1ll * a) % m;
        a = (a * 1ll * a) % m;
        b >>= 1;
    }
    return res;
}

std::string part1(std::string const& in) {
    auto [card_key, door_key] = input(in);
    int msg = 7, mod = 20'201'227;

    return fmt::format(
        "{}", powmod(door_key, discrete_log(msg, card_key, mod), mod)
    );
}

} // namespace aoc2020::day25
