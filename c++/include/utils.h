#ifndef AOC_UTILS_H
#define AOC_UTILS_H

#include <functional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <fmt/format.h>

template <typename T>
concept Hashable = requires(T a) {
    { std::hash<T>{}(a) } -> std::convertible_to<std::size_t>;
};

namespace std {

template <Hashable T>
struct hash<vector<T>> {
    size_t operator()(vector<T> const& v) const {
        auto H = hash<T>{};
        size_t seed = v.size();
        for(int i : v)
            seed ^= (H(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
        return seed;
    }
};

template <Hashable T, Hashable U>
struct hash<pair<T, U>> {
    size_t operator()(pair<T, U> const& p) const {
        auto& [t, u] = p;
        size_t seed = 2;
        seed ^= (hash<T>{}(t) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
        seed ^= (hash<U>{}(u) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
        return seed;
    }
};

template <>
struct hash<tuple<int, int, int>> {
    size_t operator()(tuple<int, int, int> const& t) const;
};

template <>
struct hash<tuple<int, int, int, int>> {
    size_t operator()(tuple<int, int, int, int> const& t) const;
};

} // namespace std

namespace fmt {

struct base_formatter {
    format_parse_context::iterator parse(format_parse_context& ctx);
};

template <class T>
struct formatter<std::vector<T>> : base_formatter {
    template <class FormatContext>
    auto format(std::vector<T> const& vec, FormatContext& ctx) {
        return format_to(ctx.out(), "{{{}}}", join(vec, ","));
    }
};

} // namespace fmt

namespace aoc {

std::string read(std::string const& filename);
std::vector<std::string> split(std::string const& s, std::string delim);
bool between(int low, int high, int value);
bool rmatch(std::string const& text, std::string pattern);
int ipow(int x, int e);

// maximum bipartite matching - Edmonds-Karp
std::unordered_map<int, int> assign(
    std::unordered_map<int, std::unordered_set<int>> const& relation,
    int n,
    int m
);

template <class T>
T fixed_point(std::function<T(T const&)> f, T const& x) {
    auto y = f(x), z = x;
    while(y != z) {
        std::swap(y, z);
        y = f(z);
    }
    return y;
}

} // namespace aoc
#endif // AOC_UTILS_H
