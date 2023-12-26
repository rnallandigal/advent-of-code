#include <algorithm>
#include <numeric>
#include <regex>
#include <string>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2018::day04 {

std::unordered_map<int, std::vector<int>> input(std::string const& in) {
    std::vector<std::string> lines = aoc::split(in, "\n");
    sort(lines.begin(), lines.end());

    std::regex begin_shift("\\[.*\\] Guard #(\\d+) begins shift");
    std::regex falls_asleep("\\[.*:(\\d+)\\] falls asleep");
    std::regex wakes_up("\\[.*:(\\d+)\\] wakes up");

    std::unordered_map<int, std::vector<int>> records;
    std::smatch matches;
    int guard = -1, asleep = -1;
    for(auto const& note : lines) {
        if(regex_match(note, matches, begin_shift)) {
            guard = stoi(matches[1].str());
        } else if(regex_match(note, matches, falls_asleep)) {
            asleep = stoi(matches[1].str());
        } else if(regex_match(note, matches, wakes_up)) {
            if(guard >= 0 && asleep >= 0) {
                int minute = stoi(matches[1].str());
                if(records.find(guard) == records.end())
                    records[guard] = std::vector<int>(60, 0);

                for(int i = asleep; i < minute; i++)
                    records[guard][i]++;
                asleep = -1;
            }
        }
    }
    return records;
}

std::string part1(std::string const& in) {
    auto records = input(in);
    int max_time = -1, max_guard;
    for(auto [guard, times] : records) {
        int time = std::accumulate(times.begin(), times.end(), 0);
        if(time > max_time) {
            max_time = time;
            max_guard = guard;
        }
    }

    int max_freq_minute = distance(
        records[max_guard].begin(),
        max_element(records[max_guard].begin(), records[max_guard].end())
    );
    return fmt::format("{}", max_guard * max_freq_minute);
}

std::string part2(std::string const& in) {
    auto records = input(in);
    int max_minutes = 0, max_guard = -1, max_freq_minute = -1;
    for(auto [guard, times] : records) {
        for(int m = 0; m < 60; m++) {
            if(times[m] > max_minutes) {
                max_minutes = times[m];
                max_guard = guard;
                max_freq_minute = m;
            }
        }
    }
    return fmt::format("{}", max_guard * max_freq_minute);
}

} // namespace aoc2018::day04
