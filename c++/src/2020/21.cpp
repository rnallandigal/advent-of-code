#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day21 {

using items_vec = std::vector<std::string>;
using clues_t = std::vector<std::pair<items_vec, items_vec>>;

clues_t input(std::string const& in) {
    clues_t clues;
    for(auto line : aoc::split(in, "\n")) {
        auto parts = aoc::split(line, " (contains ");
        clues.emplace_back(
            aoc::split(parts[0], " "),
            aoc::split(parts[1].substr(0, parts[1].size() - 1), ", ")
        );
        std::sort(clues.back().first.begin(), clues.back().first.end());
        std::sort(clues.back().second.begin(), clues.back().second.end());
    }
    return clues;
}

std::unordered_map<std::string, items_vec>
compute_candidates(clues_t const& clues) {
    std::unordered_map<std::string, items_vec> candidates;
    for(auto const& [ingredients, allergens] : clues) {
        for(auto allergen : allergens) {
            if(auto it = candidates.find(allergen); it == candidates.end()) {
                candidates.emplace(allergen, ingredients);
            } else {
                items_vec intersection;
                std::set_intersection(
                    it->second.begin(),
                    it->second.end(),
                    ingredients.begin(),
                    ingredients.end(),
                    std::back_inserter(intersection)
                );
                it->second = intersection;
            }
        }
    }
    return candidates;
}

std::string part1(std::string const& in) {
    auto clues = input(in);

    std::unordered_set<std::string> unsafes;
    for(auto [_, ingredients] : compute_candidates(clues)) {
        unsafes.insert(ingredients.begin(), ingredients.end());
    }

    int soln = 0;
    for(auto [ingredients, _] : clues) {
        for(auto ingredient : ingredients)
            soln += !unsafes.count(ingredient);
    }
    return fmt::format("{}", soln);
}

std::string part2(std::string const& in) {
    auto clues = input(in);
    auto candidates = compute_candidates(clues);

    auto remove_duplicates = [](items_vec& v) {
        std::sort(v.begin(), v.end());
        v.erase(std::unique(v.begin(), v.end()), v.end());
    };

    items_vec all_ingredients, all_allergens;
    for(auto [is, as] : clues) {
        all_ingredients.insert(all_ingredients.end(), is.begin(), is.end());
        all_allergens.insert(all_allergens.end(), as.begin(), as.end());
    }
    remove_duplicates(all_ingredients);
    remove_duplicates(all_allergens);

    auto serialize = [](items_vec const& items, std::string const& item) {
        return std::lower_bound(items.begin(), items.end(), item)
               - items.begin();
    };

    int n = all_allergens.size(), m = all_ingredients.size();
    std::unordered_map<int, std::unordered_set<int>> relation;
    for(auto [allergen, ingredients] : candidates) {
        int k = serialize(all_allergens, allergen);
        relation.emplace(k, std::unordered_set<int>{});
        for(auto ingredient : ingredients)
            relation[k].insert(serialize(all_ingredients, ingredient));
    }

    auto matching = aoc::assign(relation, n, m);

    items_vec soln;
    soln.reserve(n);
    for(int i = 0; i < n; i++)
        soln.push_back(all_ingredients[matching[i]]);

    return fmt::format("{}", fmt::join(soln, ","));
}

} // namespace aoc2020::day21
