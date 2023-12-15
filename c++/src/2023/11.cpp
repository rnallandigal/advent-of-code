#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2023::day11 {

std::string part1(std::string const &in) {
  std::vector<std::string> grid;
  for (auto line : aoc::split(in, "\n")) {
    grid.push_back(line);
  }

  std::vector<std::pair<int, int>> points;

  std::vector<int> row_add(grid.size(), 0), col_add(grid[0].size(), 0);

  for (int i = 0; i < (int)grid.size(); i++) {
    row_add[i] =
        (i > 0 ? row_add[i - 1] : 0) + (grid[i].find("#") == std::string::npos);
  }
  // fmt::print("row: {}\n", fmt::join(row_add, ", "));

  for (int j = 0; j < (int)grid[0].size(); j++) {
    bool found = false;
    for (int i = 0; i < (int)grid.size(); i++) {
      if (grid[i][j] == '#') {
        found = true;
        break;
      }
    }
    col_add[j] = (j > 0 ? col_add[j - 1] : 0) + (!found);
  }
  // fmt::print("col: {}\n", fmt::join(col_add, ", "));

  for (int i = 0; i < (int)grid.size(); i++) {
    for (int j = 0; j < (int)grid[i].size(); j++) {
      if (grid[i][j] == '#') {
        points.emplace_back(i + row_add[i], j + col_add[j]);
      }
    }
  }

  int ans = 0;
  for (int i = 0; i < (int)points.size(); i++) {
    // fmt::print("{}, {}\n", points[i].first, points[i].second);
    for (int j = i + 1; j < (int)points.size(); j++) {
      ans += std::abs(points[i].first - points[j].first) +
             std::abs(points[i].second - points[j].second);
    }
  }

  return fmt::format("{}", ans);
}

std::string part2(std::string const &in) {
  std::vector<std::string> grid;
  for (auto line : aoc::split(in, "\n")) {
    grid.push_back(line);
  }

  std::vector<std::pair<int64_t, int64_t>> points;

  std::vector<int64_t> row_add(grid.size(), 0), col_add(grid[0].size(), 0);

  for (int64_t i = 0; i < (int64_t)grid.size(); i++) {
    bool found = grid[i].find("#") == std::string::npos;
    if (!found) {
      row_add[i] = (i > 0 ? row_add[i - 1] : 0);
    } else {
      row_add[i] = (i > 0 ? row_add[i - 1] : 0) + (1'000'000 - 1);
    }
  }
  fmt::print("row: {}\n", fmt::join(row_add, ", "));

  for (int64_t j = 0; j < (int64_t)grid[0].size(); j++) {
    bool found = false;
    for (int64_t i = 0; i < (int64_t)grid.size(); i++) {
      if (grid[i][j] == '#') {
        found = true;
        break;
      }
    }
    if (found) {
      col_add[j] = (j > 0 ? col_add[j - 1] : 0);
    } else {
      col_add[j] = (j > 0 ? col_add[j - 1] : 0) + (1'000'000 - 1);
    }
  }
  fmt::print("col: {}\n", fmt::join(col_add, ", "));

  for (int64_t i = 0; i < (int64_t)grid.size(); i++) {
    for (int64_t j = 0; j < (int64_t)grid[i].size(); j++) {
      if (grid[i][j] == '#') {
        points.emplace_back(i + row_add[i], j + col_add[j]);
      }
    }
  }

  int64_t ans = 0;
  for (int64_t i = 0; i < (int64_t)points.size(); i++) {
    fmt::print("{}, {}\n", points[i].first, points[i].second);
    for (int64_t j = i + 1; j < (int64_t)points.size(); j++) {
      ans += std::abs(points[i].first - points[j].first) +
             std::abs(points[i].second - points[j].second);
    }
  }

  return fmt::format("{}", ans);
}

} // namespace aoc2023::day11
