#include <algorithm>
#include <limits>
#include <queue>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2023::day05 {

using seeds_t = std::vector<int64_t>;
using range_t = std::vector<int64_t>;
using sections_t =
    std::unordered_map<std::string,
                       std::pair<std::string, std::vector<range_t>>>;
using input_t = std::pair<seeds_t, sections_t>;

input_t get_input(std::string const &in) {
  seeds_t seeds;
  sections_t sections_map;
  for (auto section : aoc::split(in, "\n\n")) {
    auto lines = aoc::split(section, "\n");
    auto header = lines[0].substr(0, lines[0].find(':'));
    if (header == "seeds") {
      for (auto seed :
           aoc::split(lines[0].substr(lines[0].find(':') + 2), " ")) {
        seeds.push_back(std::stoll(seed));
      }
      continue;
    }

    std::string source, destination;
    std::vector<range_t> v;

    int hyphen_idx = lines[0].find('-');
    source = lines[0].substr(0, hyphen_idx);
    destination =
        lines[0].substr(hyphen_idx + 4, lines[0].find(' ') - hyphen_idx - 4);

    for (int i = 1; i < (int)lines.size(); i++) {
      v.emplace_back();
      for (auto num : aoc::split(lines[i], " ")) {
        v.back().push_back(std::stoll(num));
      }
    }
    sections_map[source] = std::pair(destination, v);
  }
  return input_t{seeds, sections_map};
}

std::string part1(std::string const &in) {
  auto [seeds, mappings] = get_input(in);
  int64_t ans = std::numeric_limits<int64_t>::max();
  for (auto const &seed : seeds) {
    std::string source = "seed";
    int64_t value = seed;
    while (source != "location") {
      auto [destination, ranges] = mappings[source];
      for (auto const &range : ranges) {
        if (aoc::between(range[1], range[1] + range[2] - 1, value)) {
          value = range[0] + (value - range[1]);
          break;
        }
      }
      source = destination;
    }
    ans = std::min(ans, value);
  }

  return fmt::format("{}", ans);
}

/**
 *
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

0 14 39
15 51 -15
50 97 2
52 53 -15
98 99 -48

0 39
15 -39
15 -15
50 2
52 15
52 -15
54 15
98 -48
98 -2
100 48

0 39
15 -54
50 2
52 0
54 15
98 -50
100 48

0 14 39
15 49 -15
50 53 -13
54 97 2
98 99 -48
100, inf 0

*/

template <class T>
std::vector<std::pair<T, T>> pairs(std::vector<T> const &items) {
  std::vector<std::pair<T, T>> pairs_vec;
  for (int i = 0; i < (int)items.size(); i += 2) {
    pairs_vec.emplace_back(items[0], items[1]);
  }
  return pairs_vec;
}

using point_t = std::vector<int64_t>;
using points_t = std::vector<point_t>;

using interval_t = std::vector<int64_t>;
using intervals_t = std::vector<interval_t>;

std::vector<std::vector<int64_t>>
merge_events(std::vector<std::vector<int64_t>> const &mappings) {
  std::vector<std::vector<int64_t>> events;
  for (auto const &mapping : mappings) {
    events.push_back({mapping[1], mapping[0] - mapping[1]});
    events.push_back({mapping[1] + mapping[2], mapping[1] - mapping[0]});
  }

  std::sort(events.begin(), events.end(),
            [](auto const &a, auto const &b) { return a[0] < b[0]; });

  std::vector<std::vector<int64_t>> merged_events;
  for (int i = 0; i < (int)events.size(); i++) {
    if (merged_events.empty())
      merged_events.push_back(events[i]);
    else if (merged_events.back()[0] != events[i][0]) {
      if (merged_events.back()[1] == 0) {
        merged_events.pop_back();
      }
      merged_events.push_back(events[i]);
    } else {
      merged_events.back()[1] += events[i][1];
    }
  }

  fmt::print("merged_events: \n");
  for (auto const &event : merged_events) {
    fmt::print("{}, {}\n", event[0], event[1]);
  }
  fmt::print("\n");

  return merged_events;
}

std::vector<std::vector<int64_t>>
merge_ranges(std::vector<std::vector<int64_t>> &ranges) {
  std::sort(ranges.begin(), ranges.end(), [](auto const &a, auto const &b) {
    return a[0] < b[0] || (a[0] == b[0] && a[1] > b[1]);
  });

  std::vector<std::vector<int64_t>> merged_ranges{ranges[0]};

  for (int i = 1; i < (int)ranges.size(); i++) {
    int64_t l = merged_ranges.back()[0], n = merged_ranges.back()[1];
    if (ranges[i][0] <= (l + n)) {
      merged_ranges.back()[1] = std::max(n, ranges[i][1]);
    } else {
      merged_ranges.push_back(ranges[i]);
    }
  }

  return merged_ranges;
}

std::vector<std::vector<int64_t>>
iterate(std::vector<std::vector<int64_t>> input_ranges,
        std::vector<std::vector<int64_t>> mappings) {

  auto events = merge_events(mappings);

  std::vector<std::vector<int64_t>> output_ranges;
  for (auto const &range : input_ranges) {
    int64_t start = range[0], len = range[1], displacement = 0;

    int64_t i = 0, last_start = start;
    for (; i < (int)events.size() && events[i][0] < start; i++) {
      displacement += events[i][1];
    }

    for (; i < (int)events.size() && events[i][0] < (start + len); i++) {
      output_ranges.push_back(
          {last_start + displacement, events[i][0] - last_start});
      last_start = events[i][0];
      displacement += events[i][1];
    }

    output_ranges.push_back(
        {last_start + displacement, start + len - last_start});
  }
  return merge_ranges(output_ranges);
}

std::string part2(std::string const &in) {
  auto [seeds, mappings] = get_input(in);

  std::string input_category = "seed";
  std::vector<std::vector<int64_t>> input_ranges;
  for (int i = 0; i < (int)seeds.size(); i += 2) {
    input_ranges.push_back({seeds[i], seeds[i + 1]});
  }

  std::sort(input_ranges.begin(), input_ranges.end(),
            [](auto const &a, auto const &b) {
              return a[0] < b[0] || (a[0] == b[0] && a[1] < b[1]);
            });

  while (input_category != "location") {
    auto [output_category, mapping] = mappings[input_category];
    fmt::print("{} ranges:\n", input_category);
    for (auto const &range : input_ranges) {
      fmt::print("{}, {}\n", range[0], range[1], range[0] + range[1] - 1);
    }
    fmt::print("\n");

    fmt::print("{} to {} mapping:\n", input_category, output_category);
    for (auto const &rule : mapping) {
      fmt::print("{}\n", fmt::join(rule, ", "));
    }
    fmt::print("\n");

    input_ranges = iterate(input_ranges, mapping);
    input_category = output_category;

    std::sort(input_ranges.begin(), input_ranges.end(),
              [](auto const &a, auto const &b) {
                return a[0] < b[0] || (a[0] == b[0] && a[1] < b[1]);
              });
  }

  fmt::print("{} ranges: \n", input_category);
  for (auto const &range : input_ranges) {
    fmt::print("{}, {}\n", range[0], range[1], range[0] + range[1] - 1);
  }

  return fmt::format("{}", "");
}
} // namespace aoc2023::day05
