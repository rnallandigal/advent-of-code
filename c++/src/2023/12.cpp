#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2023::day12 {

bool check_valid(std::string const &spring, std::vector<int> const &groups) {
  // fmt::print("{}; {}\n", spring, fmt::join(groups, ","));
  int i = 0, group_size = 0, group_it = 0;
  for (; i < (int)spring.size() && group_it < (int)groups.size(); i++) {
    if (spring[i] == '.') {
      if (i > 0 && spring[i - 1] == '#') {
        if (group_size != groups[group_it]) {
          // fmt::print("false: {} {} {}\n", i, group_size, group_it);
          return false;
        } else
          group_it++;
      }
      group_size = 0;
    } else if (spring[i] == '#') {
      group_size++;
    }
  }
  // fmt::print("end: {} {} {}\n", i, group_size, group_it);

  if (i >= (int)spring.size() && group_it >= (int)groups.size()) {
    return true;
  } else if (i >= (int)spring.size()) {
    return group_size == groups[group_it] &&
           (group_it + 1) == (int)groups.size();
  } else if (group_it >= (int)groups.size()) {
    return spring.find("#", i) == std::string::npos;
  } else
    return false;
}

int get_combos(int i, std::string spring, std::vector<int> const &groups) {
  if (i == (int)spring.size()) {
    bool val = check_valid(spring, groups);
    // fmt::print("check_valid({:20}, {}) = {}\n", spring, fmt::join(groups,
    // ","), val);
    return val;
  }

  int ans = 0;
  for (; i < (int)spring.size() && spring[i] != '?'; i++)
    ;

  if (i < (int)spring.size()) {

    spring[i] = '#';
    ans += get_combos(i + 1, spring, groups);
    spring[i] = '.';
    ans += get_combos(i + 1, spring, groups);
    spring[i] = '?';
  } else {

    bool val = check_valid(spring, groups);
    // fmt::print("check_valid({:20}, {}) = {}\n", spring, fmt::join(groups,
    // ","), val);
    return val;
  }

  return ans;
}

std::string part1(std::string const &in) {
  // check_valid(".##.###.", {2, 3});
  // return "1";
  int ans = 0;
  for (auto line : aoc::split(in, "\n")) {
    auto parts = aoc::split(line, " ");
    std::vector<int> groups;
    for (auto group : aoc::split(parts[1], ",")) {
      groups.push_back(std::stoi(group));
    }
    int sum = get_combos(0, parts[0], groups);
    // fmt::print("{:20}: {}\n", parts[0], sum);
    ans += sum;
  }
  return fmt::format("{}", ans);
}

std::string part2(std::string const &in) { return fmt::format("{}", ""); }

} // namespace aoc2023::day12
